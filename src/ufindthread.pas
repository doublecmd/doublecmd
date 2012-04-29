{
   Double Commander
   -------------------------------------------------------------------------
   Thread for search files (called from frmSearchDlg)

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2012  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit uFindThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, uFindFiles, uFindEx;

type

  { TFindThread }

  TFindThread = class(TThread)
  private
    FItems: TStrings;
    FStatus: TLabel;
    FFound: TLabel;
    FCurrent: TLabel;
    FCurrentDir:String;
    FFilesScanned:Integer;
    FFilesFound:Integer;
    FFoundFile:String;
    FCurrentDepth: Integer;
    FSearchTemplate: TSearchTemplateRec;
    FFileChecks: TFindFileChecks;
    FLinkTargets: TStringList;  // A list of encountered directories (for detecting cycles)

    function CheckFile(const Folder : String; const sr : TSearchRecEx) : Boolean;
    function CheckDirectory(const CurrentDir, FolderName : String) : Boolean;
    function FindInFile(const sFileName:UTF8String;
                        sData: String; bCase:Boolean): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFindOptions: TSearchTemplateRec);
    destructor Destroy; override;
    procedure AddFile;
    procedure WalkAdr(const sNewDir:String);
    procedure UpDateProgress;
    function IsAborting: Boolean;

    property Items:TStrings write FItems;
    property Status:TLabel read FStatus write FStatus;
    property Found:TLabel read FFound write FFound;
    property Current:TLabel read FCurrent write FCurrent; // label current file
  end;

implementation

uses
  LCLProc, StrUtils, LConvEncoding, DCStrUtils,
  uLng, DCClassesUtf8, uFindMmap, uGlobs, uShowMsg, DCOSUtils, uOSUtils,
  uLog;

{ TFindThread }

constructor TFindThread.Create(const AFindOptions: TSearchTemplateRec);
begin
  inherited Create(True);

  FLinkTargets := TStringList.Create;
  FSearchTemplate := AFindOptions;

  with FSearchTemplate do
  begin
    if SearchDepth < 0 then
      SearchDepth := MaxInt;

    // use case insensitive mask because
    // MatchesMaskList work incorrect with non ASCII characters
    // since it uses UpCase function
    FilesMasks := UTF8UpperCase(FilesMasks);
    ExcludeFiles := UTF8UpperCase(ExcludeFiles);
    ExcludeDirectories := UTF8UpperCase(ExcludeDirectories);

    FindText := ConvertEncoding(FindText, EncodingUTF8, TextEncoding);
    ReplaceText := ConvertEncoding(ReplaceText, EncodingUTF8, TextEncoding);
  end;

  SearchTemplateToFindFileChecks(FSearchTemplate, FFileChecks);
end;

destructor TFindThread.Destroy;
begin
  FreeThenNil(FLinkTargets);
  inherited Destroy;
end;

procedure TFindThread.Execute;
var
  sTemp, sPath: UTF8String;
begin
  FreeOnTerminate := True;

  try
    Assert(Assigned(FItems),'assert:FItems is empty');
    Synchronize(@UpDateProgress);
    FCurrentDepth:= -1;
    sTemp:= FSearchTemplate.StartPath;
    while sTemp <> EmptyStr do
      begin
        sPath:= Copy2SymbDel(sTemp, ';');
        sPath:= ExcludeBackPathDelimiter(sPath);
        WalkAdr(sPath);
      end;
    FCurrentDir:= rsOperFinished;
    Synchronize(@UpDateProgress);
  except
    on E:Exception do
      msgError(Self, E.Message);
  end;
end;

procedure TFindThread.AddFile;
begin
  FItems.Add(FFoundFile);
end;

function TFindThread.CheckDirectory(const CurrentDir, FolderName : String): Boolean;
begin
  if (FolderName = '.') or (FolderName = '..') then
    Result := False
  else
  begin
    with FSearchTemplate do
    begin
      Result := CheckDirectoryName(FFileChecks, UTF8UpperCase(FolderName)) and
                CheckDirectoryNameRelative(FFileChecks,
                  UTF8UpperCase(CurrentDir + PathDelim + FolderName),
                  UTF8UpperCase(FSearchTemplate.StartPath));
    end;
  end;
end;

procedure TFindThread.UpDateProgress;
begin
  FStatus.Caption:= Format(rsFindScanned, [FFilesScanned]);
  FFound.Caption := Format(rsFindFound, [FFilesFound]);
  FCurrent.Caption:= rsFindScanning + ': ' + FCurrentDir;
end;

function TFindThread.FindInFile(const sFileName:UTF8String;
                                sData: String; bCase:Boolean): Boolean;
var
  fs: TFileStreamEx;

  function FillBuffer(Buffer: PAnsiChar; BytesToRead: Longint): Longint;
  var
    DataRead: Longint;
  begin
    Result := 0;
    repeat
      DataRead := fs.Read(Buffer[Result], BytesToRead - Result);
      if DataRead = 0 then
        Break;
      Result := Result + DataRead;
    until Result >= BytesToRead;
  end;

var
  lastPos,
  sDataLength,
  DataRead: Longint;
  Buffer: PAnsiChar = nil;
  BufferSize: Integer;
begin
  Result := False;
  if sData = '' then Exit;

  if gUseMmapInSearch then
    begin
      // memory mapping should be slightly faster and use less memory
      case FindMmap(sFileName, sData, bCase, @IsAborting) of
        0 : Exit(False);
        1 : Exit(True);
        // else fall back to searching via stream reading
      end;
    end;

  BufferSize := gCopyBlockSize;
  sDataLength := Length(sData);

  if sDataLength > BufferSize then
    raise Exception.Create(rsMsgErrSmallBuf);

  fs := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
  try
    if sDataLength > fs.Size then // string longer than file, cannot search
      Exit;

    // Buffer is extended by sDataLength-1 and BufferSize + sDataLength - 1
    // bytes are read. Then strings of length sDataLength are compared with
    // sData starting from offset 0 to BufferSize-1. The remaining part of the
    // buffer [BufferSize, BufferSize+sDataLength-1] is moved to the beginning,
    // buffer is filled up with BufferSize bytes and the search continues.

    GetMem(Buffer, BufferSize + sDataLength - 1);
    if Assigned(Buffer) then
      try
        if FillBuffer(Buffer, sDataLength-1) = sDataLength-1 then
        begin
          while not Terminated do
          begin
            DataRead := FillBuffer(@Buffer[sDataLength-1], BufferSize);
            if DataRead = 0 then
              Break;

            for lastPos := 0 to DataRead - 1 do
            begin
              if PosMem(@Buffer[lastPos], sDataLength, 0, sData, bCase, False) <> Pointer(-1) then
                Exit(True); // found
            end;

            // Copy last 'sDataLength-1' bytes to the beginning of the buffer
            // (to search 'on the boundary' - where previous buffer ends,
            // and the next buffer starts).
            Move(Buffer[DataRead], Buffer^, sDataLength-1);
          end;
        end;
      except
      end;

  finally
    FreeAndNil(fs);
    if Assigned(Buffer) then
    begin
      FreeMem(Buffer);
      Buffer := nil;
    end;
  end;
end;


procedure FileReplaceString(const FileName, SearchString, ReplaceString: string; bCase:Boolean);
var
  fs: TFileStreamEx;
  S: string;
  Flags : TReplaceFlags = [];
begin
  Include(Flags, rfReplaceAll);
  if not bCase then
    Include(Flags, rfIgnoreCase);
    
  fs := TFileStreamEx.Create(FileName, fmOpenread or fmShareDenyNone);
  try
    SetLength(S, fs.Size);
    fs.ReadBuffer(S[1], fs.Size);
  finally
    fs.Free;
  end;
  S  := StringReplace(S, SearchString, replaceString, Flags);
  fs := TFileStreamEx.Create(FileName, fmCreate);
  try
    fs.WriteBuffer(S[1], Length(S));
  finally
    fs.Free;
  end;
end;

function TFindThread.CheckFile(const Folder : String; const sr : TSearchRecEx) : Boolean;
begin
  Result := True;

  with FSearchTemplate do
  begin
    if not CheckFileName(FFileChecks, UTF8UpperCase(sr.Name)) then
      Exit(False);

    if (IsDateFrom or IsDateTo or IsTimeFrom or IsTimeTo or IsNotOlderThan) then
        Result := CheckFileTime(FFileChecks, sr.Time);

    if (IsFileSizeFrom or IsFileSizeTo) and Result then
        Result := CheckFileSize(FFileChecks, sr.Size);

    if Result then
      Result := CheckFileAttributes(FFileChecks, sr.Attr);

    if (Result and IsFindText) then
       begin
         if FPS_ISDIR(sr.Attr) then
           Exit(False);

         try
           Result := FindInFile(Folder + PathDelim + sr.Name, FindText, CaseSensitive);

           if (Result and IsReplaceText) then
             FileReplaceString(Folder + PathDelim + sr.Name, FindText, ReplaceText, CaseSensitive);

           if NotContainingText then
             Result := not Result;

         except
           on e : EFOpenError do
             begin
               if (log_errors in gLogOptions) then
                 logWrite(Self, rsMsgLogError + rsMsgErrEOpen + ' ' +
                                Folder + PathDelim + sr.Name, lmtError);
               Result := False;
             end;
         end;
       end;
   end;
end;

procedure TFindThread.WalkAdr(const sNewDir:String);
var
  sr: TSearchRecEx;
  Path, SubPath: UTF8String;
  IsLink: Boolean;
begin
  if Terminated then
    Exit;

  Inc(FCurrentDepth);
  FCurrentDir := sNewDir;

  // Search all files to display statistics
  Path := sNewDir + PathDelim + '*';

  if FindFirstEx(Path, faAnyFile, sr) = 0 then
  repeat
    if not FPS_ISDIR(sr.Attr) then
    begin
      if CheckFile(sNewDir, sr) then
      begin
        FFoundFile := sNewDir + PathDelim + sr.Name;
        Synchronize(@AddFile);
        Inc(FFilesFound);
      end;

      Inc(FFilesScanned);
      Synchronize(@UpDateProgress);
    end;
  until (FindNextEx(sr)<>0) or Terminated;
  FindCloseEx(sr);
  Synchronize(@UpDateProgress);

  { Search in sub folders }
  if (not Terminated) and (FCurrentDepth < FSearchTemplate.SearchDepth) then
  begin
    if FindFirstEx(Path, faDirectory, sr) = 0 then
      repeat
        if CheckDirectory(sNewDir, sr.Name) then
        begin
          SubPath := sNewDir + PathDelim + sr.Name;
          IsLink := FPS_ISLNK(sr.Attr);
          if FSearchTemplate.FollowSymLinks then
          begin
            if IsLink then
              SubPath := mbReadAllLinks(SubPath);
            if FLinkTargets.IndexOf(SubPath) >= 0 then
              Continue; // Link already encountered - links form a cycle.
            // Add directory to already-searched list.
            FLinkTargets.Add(SubPath);
          end
          else if IsLink then
            Continue;

          WalkAdr(SubPath);
        end;
      until Terminated or (FindNextEx(sr) <> 0);
    FindCloseEx(sr);
  end;

  Dec(FCurrentDepth);
end;

function TFindThread.IsAborting: Boolean;
begin
  Result := Terminated;
end;

end.

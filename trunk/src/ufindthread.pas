{
   Double Commander
   -------------------------------------------------------------------------
   Thread for search files (called from frmSearchDlg)

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2017 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFindThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFindFiles, uFindEx, uFindByrMr, uMasks;

type

  { TFindThread }

  TFindThread = class(TThread)
  private
    FItems: TStrings;
    FCurrentDir:String;
    FFilesScanned:Integer;
    FFilesFound:Integer;
    FFoundFile:String;
    FCurrentDepth: Integer;
    FTextSearchType: TTextSearch;
    FSearchTemplate: TSearchTemplateRec;
    FSelectedFiles: TStringList;
    FFileChecks: TFindFileChecks;
    FLinkTargets: TStringList;  // A list of encountered directories (for detecting cycles)
    RecodeTable:TRecodeTable;
    FFilesMasks: TMaskList;
    FExcludeFiles: TMaskList;
    FExcludeDirectories: TMaskList;

    FTimeSearchStart:TTime;
    FTimeSearchEnd:TTime;
    FTimeOfScan:TTime;

    function GetTimeOfScan:TTime;
    procedure FindInArchive(const FileName: String);
    function CheckFileName(const FileName: String) : Boolean;
    function CheckDirectoryName(const DirectoryName: String) : Boolean;
    function CheckFile(const Folder : String; const sr : TSearchRecEx) : Boolean;
    function CheckDirectory(const CurrentDir, FolderName : String) : Boolean;
    function FindInFile(const sFileName: String;sData: String; bCase, bRegExp: Boolean): Boolean;

  protected
    procedure Execute; override;
  public
    constructor Create(const AFindOptions: TSearchTemplateRec; SelectedFiles: TStringList);
    destructor Destroy; override;
    procedure AddFile;
    procedure AddArchiveFile;
    procedure DoFile(const sNewDir: String; const sr : TSearchRecEx);
    procedure WalkAdr(const sNewDir: String);
    function IsAborting: Boolean;

    property FilesScanned: Integer read FFilesScanned;
    property FilesFound: Integer read FFilesFound;
    property CurrentDir: String read FCurrentDir;
    property TimeOfScan:TTime read GetTimeOfScan;

    property Items:TStrings write FItems;
  end;

implementation

uses
  LCLProc, StrUtils, LConvEncoding, RegExpr, DCStrUtils,
  uLng, DCClassesUtf8, uFindMmap, uGlobs, uShowMsg, DCOSUtils, uOSUtils,
  uLog, uWCXmodule, WcxPlugin, Math, uDCUtils, uConvEncoding;

{ TFindThread }

constructor TFindThread.Create(const AFindOptions: TSearchTemplateRec; SelectedFiles: TStringList);
begin
  inherited Create(True);

  FLinkTargets := TStringList.Create;
  FSearchTemplate := AFindOptions;
  FSelectedFiles := SelectedFiles;

  with FSearchTemplate do
  begin
    if SearchDepth < 0 then
      SearchDepth := MaxInt;

    if IsFindText then
    begin
      TextEncoding := NormalizeEncoding(TextEncoding);
      FindText := ConvertEncoding(FindText, EncodingUTF8, TextEncoding);
      ReplaceText := ConvertEncoding(ReplaceText, EncodingUTF8, TextEncoding);

      // Determine search type
      if SingleByteEncoding(TextEncoding) then
      begin
        FTextSearchType := tsAnsi;
        RecodeTable := InitRecodeTable(TextEncoding, CaseSensitive);
      end
      else if (CaseSensitive = False) then
      begin
        if ((TextEncoding = EncodingUTF8) or (TextEncoding = EncodingUTF8BOM)) then
          FTextSearchType:= tsUtf8
        else if (TextEncoding = EncodingUCS2LE) then
          FTextSearchType:= tsUtf16le
        else if (TextEncoding = EncodingUCS2BE) then
          FTextSearchType:= tsUtf16be
        else
          FTextSearchType:= tsOther;
      end
      else begin
        FTextSearchType:= tsOther;
      end;
    end
  end;

  SearchTemplateToFindFileChecks(FSearchTemplate, FFileChecks);

  with FFileChecks do
  begin
    FFilesMasks := TMaskList.Create(FilesMasks);
    FExcludeFiles := TMaskList.Create(ExcludeFiles);
    FExcludeDirectories := TMaskList.Create(ExcludeDirectories);
  end;

  FTimeSearchStart:=0;
  FTimeSearchEnd:=0;
  FTimeOfScan:=0;
end;

destructor TFindThread.Destroy;
begin
//  FItems.Add('End');
  FreeAndNil(FFilesMasks);
  FreeAndNil(FExcludeFiles);
  FreeThenNil(FLinkTargets);
  FreeAndNil(FExcludeDirectories);
  inherited Destroy;
end;

procedure TFindThread.Execute;
var
  I: Integer;
  sPath: String;
  sr: TSearchRecEx;
begin
  FTimeSearchStart:=Now;

  FreeOnTerminate := True;

  try
    Assert(Assigned(FItems), 'Assert: FItems is empty');
    FCurrentDepth:= -1;
    if not Assigned(FSelectedFiles) or (FSelectedFiles.Count = 0) then
    begin
      // Normal search (all directories).
      for sPath in SplitPath(FSearchTemplate.StartPath) do
      begin
        WalkAdr(ExcludeBackPathDelimiter(sPath));
      end;
    end
    else
    begin
      // Search only selected directories.
      for I := 0 to FSelectedFiles.Count - 1 do
      begin
        sPath:= FSelectedFiles[I];
        sPath:= ExcludeBackPathDelimiter(sPath);
        if FindFirstEx(sPath, faAnyFile, sr) = 0 then
        begin
          if FPS_ISDIR(sr.Attr) then
            WalkAdr(sPath)
          else
            DoFile(ExtractFileDir(sPath), sr);
        end;
        FindCloseEx(sr);
      end;
    end;
    FCurrentDir:= rsOperFinished;
  except
    on E:Exception do
      msgError(Self, E.Message);
  end;

  FTimeSearchEnd:=Now;
  FTimeOfScan:=FTimeSearchEnd-FTimeSearchStart;
end;

procedure TFindThread.AddFile;
begin
  FItems.Add(FFoundFile);
end;

procedure TFindThread.AddArchiveFile;
begin
  FItems.AddObject(FFoundFile, Self);
end;

function TFindThread.CheckDirectory(const CurrentDir, FolderName : String): Boolean;
begin
  with FSearchTemplate do
  begin
    Result := CheckDirectoryName(FolderName) and
              CheckDirectoryNameRelative(FFileChecks,
                CurrentDir + PathDelim + FolderName,
                FSearchTemplate.StartPath);
  end;
end;

function TFindThread.FindInFile(const sFileName: String; sData: String;
                                bCase, bRegExp: Boolean): Boolean;
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
  BufferSize: Integer;
  Buffer: PAnsiChar = nil;
  S: String;
begin
  Result := False;
  if sData = '' then Exit;

  // Simple regular expression search (don't work for very big files)
  if bRegExp then
  begin
    fs := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
    try
      if fs.Size = 0 then Exit;
      {$PUSH}{$R-}
      SetLength(S, fs.Size);
      {$POP}
      if Length(S) = 0 then
        raise EFOpenError.Create(EmptyStr);
      fs.ReadBuffer(S[1], fs.Size);
    finally
      fs.Free;
    end;
    Exit(ExecRegExpr(sData, S));
  end;

  if gUseMmapInSearch then
  begin
    // Memory mapping should be slightly faster and use less memory
    case FTextSearchType of
      tsAnsi:    lastPos:= FindMmapBM(sFileName, sData, RecodeTable, @IsAborting);
      tsUtf8:    lastPos:= FindMmapU(sFileName, sData);
      tsUtf16le: lastPos:= FindMmapW(sFileName, sData, True);
      tsUtf16be: lastPos:= FindMmapW(sFileName, sData, False);
      else       lastPos:= FindMmap(sFileName, sData, bCase, @IsAborting);
    end;
    case lastPos of
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
            DataRead := FillBuffer(@Buffer[sDataLength - 1], BufferSize);
            if DataRead = 0 then
              Break;

            case FTextSearchType of
              tsAnsi:
                begin
                  if PosMemBoyerMur(@Buffer[0], DataRead + sDataLength - 1, sData, RecodeTable) <> -1 then
                    Exit(True);
                end;
              tsUtf8:
                begin
                  if PosMemU(@Buffer[0], DataRead + sDataLength - 1, 0, sData, False) <> Pointer(-1) then
                    Exit(True);
                end;
              tsUtf16le,
              tsUtf16be:
                begin
                  if PosMemW(@Buffer[0], DataRead + sDataLength - 1, 0, sData, False, FTextSearchType = tsUtf16le) <> Pointer(-1) then
                    Exit(True);
                end;
              else
                begin
                  if PosMem(@Buffer[0], DataRead + sDataLength - 1, 0, sData, bCase, False) <> Pointer(-1) then
                    Exit(True);
                end;
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

procedure FileReplaceString(const FileName, SearchString, ReplaceString: string; bCase, bRegExp: Boolean);
var
  S: String;
  fs: TFileStreamEx;
  Flags : TReplaceFlags = [];
begin
  fs := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if fs.Size = 0 then Exit;
    {$PUSH}{$R-}
    SetLength(S, fs.Size);
    {$POP}
    if Length(S) = 0 then
      raise EFOpenError.Create(EmptyStr);
    fs.ReadBuffer(S[1], fs.Size);
  finally
    fs.Free;
  end;

  if bRegExp then
    S := ReplaceRegExpr(SearchString, S, replaceString, True)
  else
    begin
      Include(Flags, rfReplaceAll);
      if not bCase then Include(Flags, rfIgnoreCase);
      S := StringReplace(S, SearchString, replaceString, Flags);
    end;

  fs := TFileStreamEx.Create(FileName, fmCreate);
  try
    fs.WriteBuffer(S[1], Length(S));
  finally
    fs.Free;
  end;
end;

function TFindThread.GetTimeOfScan: TTime;
begin
  FTimeOfScan:=Now-FTimeSearchStart;
  Result:=FTimeOfScan;
end;

procedure TFindThread.FindInArchive(const FileName: String);
var
  Index: Integer;
  Header: TWcxHeader;

  function CheckHeader: Boolean;
  var
    NameLength: Integer;
    DirectoryName: String;
  begin
    with FSearchTemplate do
    begin
      if IsFindText then
      begin
        // Skip directories
        if (Header.FileAttr and faFolder) <> 0 then Exit(False);
        // Some plugins end directories with path delimiter.
        // And not set directory attribute. Process this case.
        NameLength := Length(Header.FileName);
        if (NameLength > 0) and (Header.FileName[NameLength] = PathDelim) then
          Exit(False);
      end;

      DirectoryName:= ExtractFileName(ExtractFileDir(Header.FileName));
      if not CheckDirectoryName(DirectoryName) then Exit(False);

      if not CheckFileName(ExtractFileName(Header.FileName)) then
        Exit(False);

      if (IsDateFrom or IsDateTo or IsTimeFrom or IsTimeTo or IsNotOlderThan) then
        Result := CheckFileDateTime(FFileChecks, WcxFileTimeToDateTime(Header));

      if (IsFileSizeFrom or IsFileSizeTo) and Result then
        Result := CheckFileSize(FFileChecks, Header.UnpSize);

      if Result then
        Result := CheckFileAttributes(FFileChecks, Header.FileAttr);
    end;
  end;

var
  Flags: Integer;
  Result: Boolean;
  Operation: Integer;
  TargetPath: String;
  ArcHandle: TArcHandle;
  TargetFileName: String;
  WcxModule: TWcxModule = nil;
begin
  TargetPath:= ExtractOnlyFileExt(FileName);

  for Index := 0 to gWCXPlugins.Count - 1 do
  begin
    if SameText(TargetPath, gWCXPlugins.Ext[Index]) and (gWCXPlugins.Enabled[Index]) then
    begin
      if FSearchTemplate.IsFindText and (gWCXPlugins.Flags[Index] and PK_CAPS_SEARCHTEXT = 0) then
        Continue;
      WcxModule:= gWCXPlugins.LoadModule(GetCmdDirFromEnvVar(gWCXPlugins.FileName[Index]));
      Break;
    end;
  end;

  if Assigned(WcxModule) then
  begin
    if FSearchTemplate.IsFindText then
    begin
      Flags:= PK_OM_EXTRACT;
      Operation:= PK_EXTRACT;
    end
    else begin
      Flags:= PK_OM_LIST;
      Operation:= PK_SKIP;
    end;

    ArcHandle := WcxModule.OpenArchiveHandle(FileName, Flags, Index);

    if ArcHandle <> 0 then
    try
      TargetPath:= GetTempName(GetTempFolder);
      if not mbCreateDir(TargetPath) then Exit;

      WcxModule.WcxSetChangeVolProc(ArcHandle, nil, nil);
      WcxModule.WcxSetProcessDataProc(ArcHandle, nil, nil);

      while (WcxModule.ReadWCXHeader(ArcHandle, Header) = E_SUCCESS) do
      begin
        Result:= CheckHeader;
        if Terminated then Break;
        Flags:= IfThen(Result, Operation, PK_SKIP);
        if Flags = PK_EXTRACT then TargetFileName:= TargetPath + PathDelim + ExtractFileName(Header.FileName);
        if WcxModule.WcxProcessFile(ArcHandle, Flags, EmptyStr, TargetFileName) = E_SUCCESS then
        begin
          with FSearchTemplate do
          begin
            if Result and IsFindText then
            begin
              Result:= FindInFile(TargetFileName, FindText, CaseSensitive, TextRegExp);
              if NotContainingText then Result:= not Result;
              mbDeleteFile(TargetFileName);
            end;
          end;
        end;
        if Result then
        begin
          FFoundFile := FileName + ReversePathDelim + Header.FileName;
          Synchronize(@AddArchiveFile);
          Inc(FFilesFound);
        end;
        FreeAndNil(Header);
      end;
      mbRemoveDir(TargetPath);
    finally
      WcxModule.CloseArchive(ArcHandle);
    end;
  end;
end;

function TFindThread.CheckFileName(const FileName: String): Boolean;
begin
  with FFileChecks do
  begin
    if RegExp then
    begin
      Result := ((FilesMasks = '') or ExecRegExpr(FilesMasks, FileName)) and
                ((ExcludeFiles = '') or not ExecRegExpr(ExcludeFiles, FileName));
    end
    else
    begin
      Result := FFilesMasks.Matches(FileName) and
                not FExcludeFiles.Matches(FileName);
    end;
  end;
end;

function TFindThread.CheckDirectoryName(const DirectoryName: String): Boolean;
begin
  with FFileChecks do
  begin
    Result := not FExcludeDirectories.Matches(DirectoryName);
  end;
end;

function TFindThread.CheckFile(const Folder : String; const sr : TSearchRecEx) : Boolean;
begin
  Result := True;

  with FSearchTemplate do
  begin
    if not CheckFileName(sr.Name) then
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
           Result := FindInFile(Folder + PathDelim + sr.Name, FindText, CaseSensitive, TextRegExp);

           if (Result and IsReplaceText) then
             FileReplaceString(Folder + PathDelim + sr.Name, FindText, ReplaceText, CaseSensitive, TextRegExp);

           if NotContainingText then
             Result := not Result;

         except
           on E : Exception do
           begin
             Result := False;
             if (log_errors in gLogOptions) then
             begin
               logWrite(Self, rsMsgLogError + E.Message + ' (' +
                        Folder + PathDelim + sr.Name + ')', lmtError);
             end;
           end;
         end;
       end;
    if Result and ContentPlugin then
    begin
      Result:= CheckPlugin(FSearchTemplate, Folder + PathDelim + sr.Name);
    end;
   end;
end;

procedure TFindThread.DoFile(const sNewDir: String; const sr : TSearchRecEx);
begin
  if FSearchTemplate.FindInArchives then
    FindInArchive(IncludeTrailingBackslash(sNewDir) + sr.Name);

  if CheckFile(sNewDir, sr) then
  begin
    FFoundFile := IncludeTrailingBackslash(sNewDir) + sr.Name;
    Synchronize(@AddFile);
    Inc(FFilesFound);
  end;

  Inc(FFilesScanned);
end;

procedure TFindThread.WalkAdr(const sNewDir:String);
var
  sr: TSearchRecEx;
  Path, SubPath: String;
  IsLink: Boolean;
begin
  if Terminated then
    Exit;

  Inc(FCurrentDepth);
  FCurrentDir := sNewDir;

  // Search all files to display statistics
  Path := IncludeTrailingBackslash(sNewDir) + '*';

  if FindFirstEx(Path, faAnyFile, sr) = 0 then
  repeat
    if not (FPS_ISDIR(sr.Attr) or FileIsLinkToDirectory(sNewDir + PathDelim + sr.Name, sr.Attr)) then
      DoFile(sNewDir, sr)
    else if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        DoFile(sNewDir, sr);
        // Search in sub folders
        if (FCurrentDepth < FSearchTemplate.SearchDepth) and CheckDirectory(sNewDir, sr.Name) then
        begin
          SubPath := IncludeTrailingBackslash(sNewDir) + sr.Name;
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
          FCurrentDir := sNewDir;
        end;
      end;
  until (FindNextEx(sr) <> 0) or Terminated;
  FindCloseEx(sr);

  Dec(FCurrentDepth);
end;

function TFindThread.IsAborting: Boolean;
begin
  Result := Terminated;
end;

end.

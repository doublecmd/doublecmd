{
   Double Commander
   -------------------------------------------------------------------------
   Thread for search files (called from frmSearchDlg)

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, StdCtrls, SysUtils, DsxPlugin;

type

{ TFindThread }

TFindThread = class(TThread)
  private
    { Private declarations }
    FPathStart:String;
    FItems: TStrings;
    FStatus: TLabel;
    FFound: TLabel;
    FCurrent: TLabel;
    FCurrentFile:String;
    FFilesScanned:Integer;
    FFilesFound:Integer;
    FFoundFile:String;
    FFileMask : String;
    FAttributes: Cardinal;
    FAttribStr : String;
    FCaseSens: Boolean;
    FRegExp: Boolean;
    FCurrentDepth,
    FSearchDepth: Integer;
    {Date search}
    FIsDateFrom,
    FIsDateTo : Boolean;
    FDateTimeFrom,
    FDateTimeTo : TDateTime;
    {Time search}
    FIsTimeFrom,
    FIsTimeTo : Boolean;
    (* File size search *)
    FIsFileSizeFrom,
    FIsFileSizeTo : Boolean;
    FFileSizeFrom,
    FFileSizeTo : Int64;
    (* Find text *)
    FIsNoThisText,
    FFindInFiles:Boolean;
    FFindData:String;
    (* Replace text *)
    FReplaceInFiles : Boolean;
    FReplaceData : String;

    procedure SetSearchDepth(const AValue: Integer);
    function CheckFileDate(DT : LongInt) : Boolean;
    function CheckFileSize(FileSize : Int64) : Boolean;
    function CheckFile(const Folder : String; const sr : TSearchRec) : Boolean;
    function FindInFile(const sFileName:UTF8String;
                        sData: String; bCase:Boolean): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile;
    procedure WalkAdr(const sNewDir:String);
    procedure UpDateProgress;
    procedure FillSearchRecord(var Srec:TSearchAttrRecord);
    function IsAborting: Boolean;

    property FilterMask:String read FFileMask write FFileMask;
    property PathStart:String read FPathStart write FPathStart;
    property Items:TStrings write FItems;
    property RegularExpressions: Boolean read FRegExp write FRegExp;
    property SearchDepth: Integer read FSearchDepth write SetSearchDepth;
    (* Find text *)
    property FindInFiles:Boolean write FFindInFiles;
    property IsNoThisText:Boolean write FIsNoThisText default False;
    property Status:TLabel read FStatus write FStatus;
    property Found:TLabel read FFound write FFound;
    property Current:TLabel read FCurrent write FCurrent; // label current file
    property CaseSensitive:boolean read FCaseSens write FCaseSens;
    property FindData:String read FFindData write FFindData;
    (* Replace text *)
    property ReplaceInFiles:Boolean write FReplaceInFiles;
    property ReplaceData:String read FReplaceData write FReplaceData;
    (* Date search *)
    property IsDateFrom:Boolean read FIsDateFrom write FIsDateFrom;
    property IsDateTo:Boolean read FIsDateTo write FIsDateTo;
    property DateTimeFrom:TDateTime read FDateTimeFrom write FDateTimeFrom;
    property DateTimeTo:TDateTime read FDateTimeTo write FDateTimeTo;
    (* Time search *)
    property IsTimeFrom:Boolean read FIsTimeFrom write FIsTimeFrom;
    property IsTimeTo:Boolean read FIsTimeTo write FIsTimeTo;

    (* File size search *)
    property IsFileSizeFrom : Boolean read FIsFileSizeFrom write FIsFileSizeFrom;
    property IsFileSizeTo : Boolean read FIsFileSizeTo write FIsFileSizeTo;
    property FileSizeFrom : Int64 read FFileSizeFrom write FFileSizeFrom;
    property FileSizeTo : Int64 read FFileSizeTo write FFileSizeTo;
    
    property Attributes: Cardinal read FAttributes write FAttributes;
    property  AttribStr : String read FAttribStr write FAttribStr;
  end;


implementation

uses
  LCLProc, Dialogs, DateUtils, Masks, SynRegExpr, uLng, uClassesEx, uFindMmap, uFindEx,
  uGlobs, uShowMsg, uOSUtils, uLog;

{ TFindThread }

constructor TFindThread.Create;
begin
  inherited Create(True);
  FCaseSens:=True;
  FFilesScanned:=0;
  FFilesFound := 0;
  FilterMask:='*';
  FPathStart:= mbGetCurrentDir;
  FItems:=nil;
  FIsDateFrom := False;
  FIsDateTo := False;
  FIsFileSizeFrom := False;
  FIsFileSizeTo := False;
  FAttributes := faAnyFile;
  FAttribStr := '?????????';
  FSearchDepth:= MaxInt;
end;

destructor TFindThread.Destroy;
begin
  inherited;
end;

procedure TFindThread.Execute;
var
  sCurrDir:String;
begin
  FreeOnTerminate := True;

  try
    assert(Assigned(FItems),'assert:FItems is empty');
    Synchronize(@UpDateProgress);
    if length(FPathStart)>1 then
    if FPathStart[length(FPathStart)] = PathDelim then
      Delete(FPathStart,length(FPathStart),1);
    FCurrentDepth:= -1;
    sCurrDir:= mbGetCurrentDir;
    try
      WalkAdr(FPathStart);
    finally
      mbSetCurrentDir(sCurrDir);
    end;  

  except
    on E:Exception do
      msgError(Self, E.Message);
  end;
end;

procedure TFindThread.SetSearchDepth(const AValue: Integer);
begin
  if AValue < 0 then
    FSearchDepth:= MaxInt
  else
    FSearchDepth:= AValue;
end;

procedure TFindThread.AddFile;
begin
  FItems.Add(FFoundFile);
end;

procedure TFindThread.UpDateProgress;
begin
  FStatus.Caption:= Format(rsFindScanned, [FFilesScanned]);
  FFound.Caption := Format(rsFindFound, [FFilesFound]);

  if FCurrentFile = '' then
    FCurrent.Caption := ''
  else
    FCurrent.Caption:=rsFindScanning + ': ' + FCurrentFile;
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

function TFindThread.CheckFileDate(DT : LongInt) : Boolean;
var
  DateTime: TDateTime;
begin
   Result := True;
   DateTime := FileDateToDateTime(DT);

   (* Check date from *)
   if FIsDateFrom then
      Result := (Int(DateTime) >= Int(FDateTimeFrom));

   (* Check time to *)
   if (FIsDateTo and Result) then
      Result := (Int(DateTime) <= Int(FDateTimeTo));

   (* Check time from *)
   if (FIsTimeFrom and Result) then
      Result := (CompareTime(DateTime, FDateTimeFrom) >= 0);
      
   //DebugLn('Time From = ', FloatToStr(FDateTimeFrom), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));

   (* Check time to *)
   if (FIsTimeTo and Result) then
      Result := (CompareTime(DateTime, FDateTimeTo) <= 0);

   //DebugLn('Time To = ', FloatToStr(FDateTimeTo), ' File time = ', FloatToStr(DateTime), ' Result = ', BoolToStr(Result));
end;

function TFindThread.CheckFileSize(FileSize: Int64): Boolean;
begin
   Result := True;
   if FIsFileSizeFrom then
      Result := (FileSize >= FFileSizeFrom);
   //DebugLn('After From', FileSize, '-',  FFileSizeFrom, BoolToStr(Result));
      
   if (FIsFileSizeTo and Result) then
      Result := (FileSize <= FFileSizeTo);
   //DebugLn('After To',  FileSize, '-',  FFileSizeTo, BoolToStr(Result));
end;

function TFindThread.CheckFile(const Folder : String; const sr : TSearchRec) : Boolean;
begin
  Result := True;

  // check regular expression
  if FRegExp and not ExecRegExpr(FFileMask, sr.Name) then
    Exit(False);

  //DebugLn('File = ', sr.Name);
  if (not FRegExp) and (not MatchesMaskList(sr.Name, FFileMask)) then
    Exit(False);

  if (FIsDateFrom or FIsDateTo or FIsTimeFrom or FIsTimeTo) then
      Result := CheckFileDate(sr.Time);

  if (FIsFileSizeFrom or FIsFileSizeTo) and Result then
      Result := CheckFileSize(sr.Size);
      
 // if Length(FAttribStr) <> 0 then
    begin
      Result := CheckAttrMask(FAttributes, FAttribStr, sr.Attr);
    end;

  if (FFindInFiles and Result) then
     begin
       if FPS_ISDIR(sr.Attr) then
         begin
           Result := False;
           Exit;
         end;

       try
         Result := FindInFile(Folder + PathDelim + sr.Name, FFindData, FCaseSens);

         if (FReplaceInFiles and Result) then
           FileReplaceString(Folder + PathDelim + sr.Name, FFindData, FReplaceData, FCaseSens);

         if FIsNoThisText then
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

procedure TFindThread.FillSearchRecord(var Srec:TSearchAttrRecord);

begin
  with Srec do
  begin
    rFileMask:= StrNew(PChar(FFileMask));
    rAttributes:=FAttributes;
    rAttribStr:= StrNew(PChar(FAttribStr));
    rCaseSens:=FCaseSens;
    {Date search}
    rIsDateFrom:=FIsDateFrom;
    rIsDateTo:=FIsDateTo;
    rDateTimeFrom:=FDateTimeFrom;
    rDateTimeTo:=FDateTimeTo;
    {Time search}
    rIsTimeFrom:=FIsTimeFrom;
    rIsTimeTo:=FIsTimeTo;
    (* File size search *)
    rIsFileSizeFrom:=FIsFileSizeFrom;
    rIsFileSizeTo:=FIsFileSizeTo;
    rFileSizeFrom:=FFileSizeFrom;
    rFileSizeTo:=FFileSizeTo;
    (* Find text *)
    rIsNoThisText:=FIsNoThisText;
    rFindInFiles:=FFindInFiles;
    rFindData:= StrNew(PChar(FFindData));
    (* Replace text *)
    rReplaceInFiles:=FReplaceInFiles;
    rReplaceData:= StrNew(PChar(FReplaceData));
  end;
end;

procedure TFindThread.WalkAdr(const sNewDir:String);
var
  sr: TSearchRec;
  Path : String;
begin
  DebugLn(sNewDir);

  if not mbSetCurrentDir(sNewDir) then Exit;

  Inc(FCurrentDepth);

  // if regular expression then search all files
  if FRegExp or (Pos(';', FFileMask) <> 0) then
    Path := sNewDir + PathDelim + '*'
  else
    Path := sNewDir + PathDelim + FFileMask;
  //DebugLn('Path = ', Path);

  DebugLn('FAttributes == ' + IntToStr(FAttributes));

  if FindFirstEx(Path, FAttributes, sr) = 0 then
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;

    FCurrentFile:=sNewDir + PathDelim + sr.Name;
    Synchronize(@UpDateProgress);

    if CheckFile(sNewDir, sr) then
    begin
      FFoundFile := FCurrentFile;
      Synchronize(@AddFile);
      FFilesFound := FFilesFound + 1;
    end;
      
    inc(FFilesScanned);
  until (FindNextEx(sr)<>0) or Terminated;
  FindCloseEx(sr);
  FCurrentFile := '';
  Synchronize(@UpDateProgress);

  { Search in sub folders }
  if (not Terminated) and (FCurrentDepth < FSearchDepth) then
  begin
    Path := sNewDir + PathDelim + '*';
    DebugLn('Search in sub folders = ', Path);
    if not Terminated and (FindFirstEx(Path, faDirectory, sr) = 0) then
      repeat
        if (sr.Name[1] <> '.') then
          WalkAdr(sNewDir + PathDelim + sr.Name);
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

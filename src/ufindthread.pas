{
   Double Commander
   -------------------------------------------------------------------------
   Thread for search files (called from frmSearchDlg)

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2023 Alexander Koblov (alexx2000@mail.ru)

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
{$include calling.inc}

interface

uses
  Classes, SysUtils, Contnrs, DCStringHashListUtf8, uFindFiles, uFindEx,
  uFindByrMr, uMasks, uRegExpr, uRegExprW, uWcxModule;

type

  { TDuplicate }

  TDuplicate = class
    Name: String;
    Hash: String;
    Size: Int64;
    Index: IntPtr;
    Count: Integer;
    function Clone: TDuplicate;
  end;

  { TEncoding }

  TEncoding = class
    FindText: String;
    ReplaceText: String;
    FRegExpr: TRegExprEx;
    RecodeTable: TRecodeTable;
    FTextSearchType: TTextSearch;
  public
    destructor Destroy; override;
  end;

  { TFindThread }

  TFindThread = class(TThread)
  private
    FItems: TStrings;
    FCurrentDir:String;
    FFilesScanned:Integer;
    FFilesFound:Integer;
    FFoundFile:String;
    FCurrentDepth: Integer;
    FSearchText: String;
    FSearchTemplate: TSearchTemplateRec;
    FSelectedFiles: TStringList;
    FFileChecks: TFindFileChecks;
    FLinkTargets: TStringList;  // A list of encountered directories (for detecting cycles)
    FFilesMasks: TMaskList;
    FExcludeFiles: TMaskList;
    FEncodings: TObjectList;
    FExcludeDirectories: TMaskList;
    FFilesMasksRegExp: TRegExprW;
    FExcludeFilesRegExp: TRegExprW;
    FArchive: TWcxModule;
    FHeader: TWcxHeader;

    FTimeSearchStart:TTime;
    FTimeSearchEnd:TTime;
    FTimeOfScan:TTime;

    FBuffer: TBytes;
    FFoundIndex: IntPtr;
    FDuplicateIndex: Integer;
    FDuplicates: TStringHashListUtf8;

    function GetTimeOfScan:TTime;
    procedure FindInArchive(const FileName: String);
    function CheckFileName(const FileName: String) : Boolean;
    function CheckDirectoryName(const DirectoryName: String) : Boolean;
    function CheckFile(const Folder : String; const sr : TSearchRecEx) : Boolean;
    function CheckDirectory(const CurrentDir, FolderName : String) : Boolean;
    function CheckDuplicate(const Folder : String; const sr : TSearchRecEx): Boolean;
    function FindInFile(const sFileName: String; bCase, bRegExp: Boolean): Boolean;
    procedure FileReplaceString(const FileName: String; bCase, bRegExp: Boolean);

  protected
    procedure Execute; override;
  public
    constructor Create(const AFindOptions: TSearchTemplateRec; SelectedFiles: TStringList);
    destructor Destroy; override;
    procedure AddFile;
    procedure AddArchiveFile;
    procedure AddDuplicateFile;
    procedure DoFile(const sNewDir: String; const sr : TSearchRecEx);
    procedure WalkAdr(const sNewDir: String);
    function IsAborting: Boolean;

    property FilesScanned: Integer read FFilesScanned;
    property FilesFound: Integer read FFilesFound;
    property CurrentDir: String read FCurrentDir;
    property TimeOfScan:TTime read GetTimeOfScan;
    property Archive: TWcxModule write FArchive;

    property Items:TStrings write FItems;
  end;

implementation

uses
  LCLProc, LazUtf8, StrUtils, LConvEncoding, DCStrUtils, DCConvertEncoding,
  uLng, DCClassesUtf8, uFindMmap, uGlobs, uShowMsg, DCOSUtils, uOSUtils, uHash,
  uLog, WcxPlugin, Math, uDCUtils, uConvEncoding, DCDateTimeUtils, uOfficeXML;

function ProcessDataProcAG(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  if TThread.CheckTerminated then
    Result:= 0
  else
    Result:= 1;
end;

function ProcessDataProcWG(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  if TThread.CheckTerminated then
    Result:= 0
  else
    Result:= 1;
end;

{ TDuplicate }

function TDuplicate.Clone: TDuplicate;
begin
  Result:= TDuplicate.Create;
  Result.Index:= Self.Index;
end;

{ TEncoding }

destructor TEncoding.Destroy;
begin
  FRegExpr.Free;
  inherited Destroy;
end;

{ TFindThread }

constructor TFindThread.Create(const AFindOptions: TSearchTemplateRec; SelectedFiles: TStringList);
var
  S: String;
  Index: Integer;
  AEncoding: TEncoding;
  ATextEncoding: String;
  AEncodings: TStringArray;
begin
  inherited Create(True);

  FEncodings:= TObjectList.Create(True);
  FLinkTargets := TStringList.Create;
  FSearchTemplate := AFindOptions;
  FSelectedFiles := SelectedFiles;

  FDuplicates:= TStringHashListUtf8.Create(True);

  with FSearchTemplate do
  begin
    if SearchDepth < 0 then
      SearchDepth := MaxInt;

    if IsFindText then
    begin
      FSearchText := FindText;

      AEncodings:= SplitString(TextEncoding, '|');

      for Index:= 0 to High(AEncodings) do
      begin
        AEncoding:= TEncoding.Create;
        ATextEncoding:= AEncodings[Index];

        if HexValue then
        begin
          ATextEncoding := EncodingAnsi;
          FindText := HexToBin(FindText);
        end
        else begin
          ATextEncoding := NormalizeEncoding(ATextEncoding);
          AEncoding.FindText := ConvertEncoding(FindText, EncodingUTF8, ATextEncoding);
          AEncoding.ReplaceText := ConvertEncoding(ReplaceText, EncodingUTF8, ATextEncoding);
          if TextRegExp then
          begin
            AEncoding.FRegExpr := TRegExprEx.Create(ATextEncoding, True);
            AEncoding.FRegExpr.Expression := FSearchText;
          end;
        end;

        // Determine search type
        if SingleByteEncoding(ATextEncoding) then
        begin
          AEncoding.FTextSearchType := tsAnsi;
          AEncoding.RecodeTable := InitRecodeTable(ATextEncoding, CaseSensitive);
        end
        else if (CaseSensitive = False) then
        begin
          if ATextEncoding = EncodingDefault then begin
            ATextEncoding := GetDefaultTextEncoding;
          end;
          if ((ATextEncoding = EncodingUTF8) or (ATextEncoding = EncodingUTF8BOM)) then
            AEncoding.FTextSearchType:= tsUtf8
          else if (ATextEncoding = EncodingUTF16LE) then
            AEncoding.FTextSearchType:= tsUtf16le
          else if (ATextEncoding = EncodingUTF16BE) then
            AEncoding.FTextSearchType:= tsUtf16be
          else
            AEncoding.FTextSearchType:= tsOther;
        end
        else begin
          AEncoding.FTextSearchType:= tsOther;
        end;
        FEncodings.Add(AEncoding);

        if HexValue then Break;
      end;
    end
  end;

  SearchTemplateToFindFileChecks(FSearchTemplate, FFileChecks);

  with FFileChecks do
  begin
    if RegExp then begin
      FFilesMasksRegExp := TRegExprW.Create(CeUtf8ToUtf16(FilesMasks));
      FExcludeFilesRegExp := TRegExprW.Create(CeUtf8ToUtf16(ExcludeFiles));
    end
    else begin
      FFilesMasks := TMaskList.Create(FilesMasks);
      FExcludeFiles := TMaskList.Create(ExcludeFiles);
    end;
    FExcludeDirectories := TMaskList.Create(ExcludeDirectories);
  end;

  if FSearchTemplate.Duplicates and FSearchTemplate.DuplicateHash then
    SetLength(FBuffer, gHashBlockSize);

  FTimeSearchStart:=0;
  FTimeSearchEnd:=0;
  FTimeOfScan:=0;
end;

destructor TFindThread.Destroy;
var
  Index: Integer;
begin
//  FItems.Add('End');
  FreeAndNil(FEncodings);
  FreeAndNil(FFilesMasks);
  FreeAndNil(FExcludeFiles);
  FreeThenNil(FLinkTargets);
  FreeAndNil(FFilesMasksRegExp);
  FreeAndNil(FExcludeFilesRegExp);
  FreeAndNil(FExcludeDirectories);
  for Index:= 0 to FDuplicates.Count - 1 do
    TObject(FDuplicates.List[Index]^.Data).Free;
  FreeAndNil(FDuplicates);
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
    if Assigned(FArchive) then
    begin
      FindInArchive(FSearchTemplate.StartPath);
    end
    else if not Assigned(FSelectedFiles) or (FSelectedFiles.Count = 0) then
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
        if FindFirstEx(sPath, 0, sr) = 0 then
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
  FItems.AddObject(FFoundFile, FHeader.Clone);
end;

procedure TFindThread.AddDuplicateFile;
var
  AData: TDuplicate;
begin
  AData:= TDuplicate(FDuplicates.List[FFoundIndex]^.Data);
  if AData.Count = 1 then
  begin
    Inc(FFilesFound);
    FItems.AddObject(AData.Name, AData.Clone);
  end;
  Inc(FFilesFound);
  FItems.AddObject(FFoundFile, AData.Clone);
end;

function TFindThread.CheckDirectory(const CurrentDir, FolderName : String): Boolean;
begin
  with FSearchTemplate do
  begin
    Result := CheckDirectoryName(FolderName) and
              CheckDirectoryNameEx(FFileChecks,
                                   CurrentDir + PathDelim + FolderName,
                                   FSearchTemplate.StartPath);
  end;
end;

function TFindThread.FindInFile(const sFileName: String;
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
  S: String;
  Index: Integer;
  MaxLen: Integer;
  lastPos: Pointer;
  DataRead: Integer;
  fmr : TFileMapRec;
  BufferSize: Integer;
  sDataLength: Integer;
  AEncoding: TEncoding;
  Buffer: PAnsiChar = nil;
begin
  Result := False;
  if FSearchText = '' then Exit;

  if FSearchTemplate.OfficeXML and OfficeMask.Matches(sFileName) then
  begin
    if LoadFromOffice(sFileName, S) then
    begin
      if bRegExp then
        Result:= uRegExprW.ExecRegExpr(UTF8ToUTF16(FSearchText), UTF8ToUTF16(S))
      else if FSearchTemplate.CaseSensitive then
        Result:= PosMem(Pointer(S), Length(S), 0, FSearchText, False, False) <> Pointer(-1)
      else begin
        Result:= PosMemU(Pointer(S), Length(S), 0, FSearchText, False) <> Pointer(-1);
      end;
    end;
    Exit;
  end;

  // Simple regular expression search (don't work for very big files)
  if bRegExp then
  begin
    fs := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone or fmOpenNoATime);
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
    for Index:= 0 to FEncodings.Count - 1 do
    begin
      AEncoding:= TEncoding(FEncodings[Index]);
      AEncoding.FRegExpr.SetInputString(Pointer(S), Length(S));
      if AEncoding.FRegExpr.Exec() then Exit(True);
    end;
    Exit;
  end;

  if gUseMmapInSearch then
  begin
    // Memory mapping should be slightly faster and use less memory
    if MapFile(sFileName, fmr) then
    try
      for Index:= 0 to FEncodings.Count - 1 do
      begin
        AEncoding:= TEncoding(FEncodings[Index]);
        with AEncoding do
        begin
          case FTextSearchType of
            tsAnsi:    lastPos:= Pointer(PosMemBoyerMur(fmr.MappedFile, fmr.FileSize, FindText, RecodeTable));
            tsUtf8:    lastPos:= PosMemU(fmr.MappedFile, fmr.FileSize, 0, FindText, False);
            tsUtf16le: lastPos:= PosMemW(fmr.MappedFile, fmr.FileSize, 0, FindText, False, True);
            tsUtf16be: lastPos:= PosMemW(fmr.MappedFile, fmr.FileSize, 0, FindText, False, False);
            else       lastPos:= PosMem(fmr.MappedFile, fmr.FileSize, 0, FindText, bCase, False);
          end;
        end;
        if (lastPos <> Pointer(-1)) then Exit(True);
      end;
      Exit;
    finally
      UnMapFile(fmr);
    end;
    // else fall back to searching via stream reading
  end;

  fs := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone or fmOpenNoATime);
  try
    MaxLen:= 0;
    BufferSize := gCopyBlockSize;

    for Index:= 0 to FEncodings.Count - 1 do
    begin
      AEncoding:= TEncoding(FEncodings[Index]);
      sDataLength := Length(AEncoding.FindText);
      if sDataLength > MaxLen then
        MaxLen:= sDataLength;
    end;

    // Buffer is extended by sDataLength-1 and BufferSize + sDataLength - 1
    // bytes are read. Then strings of length sDataLength are compared with
    // sData starting from offset 0 to BufferSize-1. The remaining part of the
    // buffer [BufferSize, BufferSize+sDataLength-1] is moved to the beginning,
    // buffer is filled up with BufferSize bytes and the search continues.

    GetMem(Buffer, BufferSize + MaxLen - 1);

    if Assigned(Buffer) then
    try
      for Index:= 0 to FEncodings.Count - 1 do
      begin
        fs.Seek(0, soFromBeginning);
        AEncoding:= TEncoding(FEncodings[Index]);
        sDataLength := Length(AEncoding.FindText);

        if sDataLength > BufferSize then
          raise Exception.Create(rsMsgErrSmallBuf);

        if sDataLength > fs.Size then // string longer than file, cannot search
          Continue;

        try
          if FillBuffer(Buffer, sDataLength-1) = sDataLength-1 then
          begin
            while not Terminated do
            begin
              DataRead := FillBuffer(@Buffer[sDataLength - 1], BufferSize);
              if DataRead = 0 then
                Break;

              case AEncoding.FTextSearchType of
                tsAnsi:
                  begin
                    if PosMemBoyerMur(@Buffer[0], DataRead + sDataLength - 1, AEncoding.FindText, AEncoding.RecodeTable) <> -1 then
                      Exit(True);
                  end;
                tsUtf8:
                  begin
                    if PosMemU(@Buffer[0], DataRead + sDataLength - 1, 0, AEncoding.FindText, False) <> Pointer(-1) then
                      Exit(True);
                  end;
                tsUtf16le,
                tsUtf16be:
                  begin
                    if PosMemW(@Buffer[0], DataRead + sDataLength - 1, 0, AEncoding.FindText, False, AEncoding.FTextSearchType = tsUtf16le) <> Pointer(-1) then
                      Exit(True);
                  end;
                else
                  begin
                    if PosMem(@Buffer[0], DataRead + sDataLength - 1, 0, AEncoding.FindText, bCase, False) <> Pointer(-1) then
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
      end;
    finally
      FreeMem(Buffer);
      Buffer := nil;
    end;

  finally
    FreeAndNil(fs);
  end;
end;

procedure TFindThread.FileReplaceString(const FileName: String; bCase, bRegExp: Boolean);
var
  S: String;
  fs: TFileStreamEx;
  AEncoding: TEncoding;
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

  AEncoding:= TEncoding(FEncodings[0]);

  if bRegExp then
    S := AEncoding.FRegExpr.ReplaceAll(AEncoding.FindText, S, AEncoding.ReplaceText)
  else
    begin
      Include(Flags, rfReplaceAll);
      if not bCase then Include(Flags, rfIgnoreCase);
      S := StringReplace(S, AEncoding.FindText, AEncoding.ReplaceText, Flags);
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

  function CheckHeader: Boolean;
  var
    NameLength: Integer;
    DirectoryName: String;
  begin
    with FSearchTemplate do
    begin
      Result:= True;

      if IsFindText then
      begin
        // Skip directories
        if (FHeader.FileAttr and faFolder) <> 0 then Exit(False);
        // Some plugins end directories with path delimiter.
        // And not set directory attribute. Process this case.
        NameLength := Length(FHeader.FileName);
        if (NameLength > 0) and (FHeader.FileName[NameLength] = PathDelim) then
          Exit(False);
      end;

      DirectoryName:= ExtractFileName(ExtractFileDir(FHeader.FileName));
      if not CheckDirectoryName(DirectoryName) then Exit(False);

      if not CheckFileName(ExtractFileName(FHeader.FileName)) then
        Exit(False);

      if (IsDateFrom or IsDateTo or IsTimeFrom or IsTimeTo or IsNotOlderThan) then
        Result := CheckFileDateTime(FFileChecks, WcxFileTimeToDateTime(FHeader.FileTime));

      if (IsFileSizeFrom or IsFileSizeTo) and Result then
        Result := CheckFileSize(FFileChecks, FHeader.UnpSize);

      if Result then
        Result := CheckFileAttributes(FFileChecks, FHeader.FileAttr);
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
  if Assigned(FArchive) then
    WcxModule:= FArchive
  else begin
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
      if Operation = PK_EXTRACT then
      begin
        TargetPath:= GetTempName(GetTempFolder);
        if not mbCreateDir(TargetPath) then Exit;
      end;

      WcxModule.WcxSetChangeVolProc(ArcHandle);
      WcxModule.WcxSetProcessDataProc(ArcHandle, @ProcessDataProcAG, @ProcessDataProcWG);

      while (WcxModule.ReadWCXHeader(ArcHandle, FHeader) = E_SUCCESS) do
      begin
        Result:= CheckHeader;
        if Terminated then Break;
        Flags:= IfThen(Result, Operation, PK_SKIP);
        if Flags = PK_EXTRACT then TargetFileName:= TargetPath + PathDelim + ExtractFileName(FHeader.FileName);
        if WcxModule.WcxProcessFile(ArcHandle, Flags, EmptyStr, TargetFileName) = E_SUCCESS then
        begin
          with FSearchTemplate do
          begin
            if Result and IsFindText then
            begin
              Result:= FindInFile(TargetFileName, CaseSensitive, TextRegExp);
              if NotContainingText then Result:= not Result;
              mbDeleteFile(TargetFileName);
            end;
          end;
        end;
        if Result then
        begin
          FFoundFile := FileName + ReversePathDelim + FHeader.FileName;
          Synchronize(@AddArchiveFile);
          Inc(FFilesFound);
        end;
        FreeAndNil(FHeader);
      end;
      if Operation = PK_EXTRACT then mbRemoveDir(TargetPath);
    finally
      WcxModule.CloseArchive(ArcHandle);
    end;
  end;
end;

function TFindThread.CheckFileName(const FileName: String): Boolean;
var
  AFileName: UnicodeString;
begin
  with FFileChecks do
  begin
    if RegExp then
    begin
      AFileName := CeUtf8ToUtf16(FileName);
      Result := ((FilesMasks = '') or FFilesMasksRegExp.Exec(AFileName)) and
                ((ExcludeFiles = '') or not FExcludeFilesRegExp.Exec(AFileName));
    end
    else
    begin
      Result := FFilesMasks.Matches(FileName) and
                not FExcludeFiles.Matches(FileName);
    end;
  end;
end;

function TFindThread.CheckDuplicate(const Folder: String; const sr: TSearchRecEx): Boolean;
var
  AKey: String;
  AHash: String;
  Index: IntPtr;
  AData: TDuplicate;
  AFileName: String;
  AValue: String = '';
  AStart, AFinish: Integer;

  function FileHash(const AName: String; Size: Int64; out Hash: String): Boolean;
  var
    Handle: THandle;
    BytesRead: Integer;
    BytesToRead: Integer;
    Context: THashContext;
  begin
    Handle:= mbFileOpen(AName, fmOpenRead or fmShareDenyWrite);
    Result:= (Handle <> feInvalidHandle);
    if Result then
    begin
      HashInit(Context, HASH_BEST);
      BytesToRead:= Length(FBuffer);
      while (Size > 0) and (not Terminated) do
      begin
        if (Size < BytesToRead) then BytesToRead:= Size;
        BytesRead := FileRead(Handle, FBuffer[0], BytesToRead);
        if (BytesRead < 0) then Break;
        HashUpdate(Context, FBuffer[0], BytesRead);
        Dec(Size, BytesRead);
      end;
      FileClose(Handle);
      Result:= (Size = 0);
      HashFinal(Context, Hash);
    end;
  end;

  function CompareFiles(fn1, fn2: String; len: Int64): Boolean;
  const
    BUFLEN = 1024 * 32;
  var
    i, j: Int64;
    fs1, fs2: TFileStreamEx;
    buf1, buf2: array [1..BUFLEN] of Byte;
  begin
    try
      fs1 := TFileStreamEx.Create(fn1, fmOpenRead or fmShareDenyWrite);
      try
        fs2 := TFileStreamEx.Create(fn2, fmOpenRead or fmShareDenyWrite);
        try
          i := 0;
          repeat
            if len - i <= BUFLEN then
              j := len - i
            else begin
              j := BUFLEN;
            end;
            fs1.ReadBuffer(buf1, j);
            fs2.ReadBuffer(buf2, j);
            i := i + j;
            Result := CompareMem(@buf1, @buf2, j);
          until Terminated or not Result or (i >= len);
        finally
          fs2.Free;
        end;
      finally
        fs1.Free;
      end;
    except
      Result:= False;
    end;
  end;

begin
  AFileName:= IncludeTrailingBackslash(Folder) + sr.Name;

  if (FPS_ISDIR(sr.Attr) or FileIsLinkToDirectory(AFileName, sr.Attr)) then
    Exit(False);

  if FSearchTemplate.DuplicateName then
  begin
    if FileNameCaseSensitive then
      AValue:= sr.Name
    else
      AValue:= UTF8LowerCase(sr.Name);
  end;

  if FSearchTemplate.DuplicateSize then
    AValue+= IntToStr(sr.Size);

  if FSearchTemplate.DuplicateHash then
    AHash:= EmptyStr;

  Index:= FDuplicates.Find(AValue);
  Result:= (Index >= 0);
  if Result then
  begin
    FDuplicates.FindBoundaries(Index, AStart, AFinish);

    for Index:= AStart to AFinish do
    begin
      AKey:= FDuplicates.List[Index]^.Key;

      if (Length(AKey) = Length(AValue)) and (CompareByte(AKey[1], AValue[1], Length(AKey)) = 0) then
      begin
        AData:= TDuplicate(FDuplicates.List[Index]^.Data);

        if FSearchTemplate.DuplicateHash then
        begin
          // Group file hash
          if Length(AData.Hash) = 0 then
          begin
            if not FileHash(AData.Name, AData.Size, AData.Hash) then
            begin
              AData.Name:= AFileName;
              AData.Size:= sr.Size;
              if (Index < AFinish) then
              begin
                Result:= False;
                Continue;
              end;
              Exit(False);
            end;
          end;
          // Current file hash
          if (Length(AHash) = 0) then
          begin
            if not FileHash(AFileName, sr.Size, AHash) then
              Exit;
          end;
          Result:= SameStr(AHash, AData.Hash);
        end
        else if FSearchTemplate.DuplicateContent then
          Result:= CompareFiles(AData.Name, AFileName, sr.Size)
        else begin
          Result:= True;
        end;

        if Result then
        begin
          Inc(AData.Count);
          FFoundIndex:= Index;
          // First match
          if (AData.Count = 1) then
          begin
            Inc(FDuplicateIndex);
            AData.Index:= FDuplicateIndex;
          end;
          Exit;
        end;
      end;
    end;
  end;
  if not Result then
  begin
    AData:= TDuplicate.Create;
    AData.Name:= AFileName;
    AData.Hash:= AHash;
    AData.Size:= sr.Size;
    FDuplicates.Add(AValue, AData);
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
      if FPS_ISDIR(sr.Attr) or (sr.Size = 0) then
        Exit(False);

      try
        Result := FindInFile(IncludeTrailingBackslash(Folder) + sr.Name, CaseSensitive, TextRegExp);

        if (Result and IsReplaceText) then
          FileReplaceString(IncludeTrailingBackslash(Folder) + sr.Name, CaseSensitive, TextRegExp);

        if NotContainingText then
          Result := not Result;

      except
        on E : Exception do
        begin
          Result := False;
          if (log_errors in gLogOptions) then
          begin
            logWrite(Self, rsMsgLogError + E.Message + ' (' +
                     IncludeTrailingBackslash(Folder) + sr.Name + ')', lmtError);
          end;
        end;
      end;
    end;

    if Result and ContentPlugin then
    begin
      Result:= CheckPlugin(FSearchTemplate, sr, Folder);
    end;
  end;
end;

procedure TFindThread.DoFile(const sNewDir: String; const sr : TSearchRecEx);
begin
  if FSearchTemplate.FindInArchives then
    FindInArchive(IncludeTrailingBackslash(sNewDir) + sr.Name);

  if CheckFile(sNewDir, sr) then
  begin
    if FSearchTemplate.Duplicates then
    begin
      if CheckDuplicate(sNewDir, sr) then
      begin
        FFoundFile := IncludeTrailingBackslash(sNewDir) + sr.Name;
        Synchronize(@AddDuplicateFile);
      end;
    end
    else begin
      FFoundFile := IncludeTrailingBackslash(sNewDir) + sr.Name;
      Synchronize(@AddFile);
      Inc(FFilesFound);
    end;
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

  if FindFirstEx(Path, 0, sr) = 0 then
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

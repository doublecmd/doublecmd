{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin

  Copyright (C) 2014-2024 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library. If not, see <http://www.gnu.org/licenses/>.
}

unit SevenZipFunc;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  WcxPlugin, Extension;

{ Mandatory }
function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle; winapi;
function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer; winapi;
function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer; winapi;
function CloseArchive (hArcData : TArcHandle) : Integer; winapi;
procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW); winapi;
procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW); winapi;
{ Optional }
function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar; SrcPath: PWideChar; AddList: PWideChar; Flags: Integer): Integer; winapi;
function DeleteFilesW(PackedFile, DeleteList: PWideChar): Integer; winapi;
function CanYouHandleThisFileW(FileName: PWideChar): Boolean; winapi;
procedure ConfigurePacker(Parent: HWND; DllInstance: THandle); winapi;
{ Extension }
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); winapi;

implementation

uses
  Windows, SysUtils, Classes, JclCompression, SevenZip, SevenZipAdv, fpTimer, DCOSUtils,
  SevenZipDlg, SevenZipLng, SevenZipOpt, LazFileUtils, SyncObjs, LazUTF8, SevenZipCodecs,
  DCFileAttributes, DCConvertEncoding, SevenZipHlp;

type

   { ESevenZipAbort }

    ESevenZipAbort = class(EJclCompressionError)

    end;

  { TSevenZipUpdate }

  TSevenZipUpdate = class(TThread)
    FPercent: Int64;
    FFileName: WideString;
    FPause: TEventObject;
    FProgress: TEventObject;
    FArchive: TJclCompressionArchive;
  public
    constructor Create; overload;
    constructor Create(Archive: TJclCompressionArchive); overload;
    destructor Destroy; override;
  public
    procedure Execute; override;
    function Update: Integer; virtual;
    procedure JclCompressionPassword(Sender: TObject; var Password: WideString);
    procedure JclCompressionProgress(Sender: TObject; const Value, MaxValue: Int64); virtual;
  end;

  { TSevenZipHandle }

  TSevenZipHandle = class(TSevenZipUpdate)
    Index,
    Count: LongWord;
    OpenMode,
    OperationMode: Integer;
    ProcessIndex: Cardinal;
    ArchiveName: String;
    ProcessArray: TCardinalArray;
    FileName: array of String;
    ProcessDataProc: TProcessDataProcW;
  public
    procedure Execute; override;
    function Update: Integer; override;
    procedure SetArchive(AValue: TJclDecompressArchive);
    function JclCompressionExtract(Sender: TObject; AIndex: Integer;
      var AFileName: TFileName; var Stream: TStream; var AOwnsStream: Boolean): Boolean;
  end;

  { TPasswordCache }

  TPasswordCache = class
  private
    FTimer: TFPTimer;
    FArchiveSize: Int64;
    FArchiveName: String;
    FArchiveTime: Integer;
    FMutex: TCriticalSection;
    FArchivePassword: WideString;
    const FInterval: Cardinal = 120000;
  private
    procedure ResetTimer;
    procedure ZeroPassword;
    procedure TimerEvent(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function GetPassword(const Archive: String): WideString;
    procedure SetPassword(const Archive: String; const Password: WideString);
  end;

var
  PasswordCache: TPasswordCache = nil;

threadvar
  ProcessDataProcT: TProcessDataProcW;

function GetArchiveError(const E: Exception): Integer;
begin
  if E is EFOpenError then
    Result:= E_EOPEN
  else if E is EFCreateError then
    Result:= E_ECREATE
  else if E is EReadError then
    Result:= E_EREAD
  else if E is EWriteError then
    Result:= E_EWRITE
  else if E is ESevenZipAbort then
    Result:= E_EABORTED
  else if Pos(HexStr(E_OUTOFMEMORY, 8), E.Message) > 0 then
    Result:= E_NO_MEMORY
  else begin
    Result:= E_UNKNOWN_FORMAT;
  end;
end;

function Verify: Boolean;
begin
  Result:= Is7ZipLoaded;
  if not Result then
  begin
    MessageBox(Format(rsSevenZipLoadError, [SevenZipDefaultLibraryName]),
               'SevenZip', MB_OK or MB_ICONERROR);
  end;
end;

function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle; winapi;
var
  I: Integer;
  ResultHandle: TSevenZipHandle;
  Archive: TJclDecompressArchive;
  AFormats: TJclDecompressArchiveClassArray;
begin
  if not Verify then
  begin
    ArchiveData.OpenResult:= E_HANDLED;
    Exit(0);
  end;
  ResultHandle:= TSevenZipHandle.Create;
  with ResultHandle do
  begin
    Index:= 0;
    ProcessIndex:= 0;
    OpenMode:= ArchiveData.OpenMode;
    ArchiveName := Utf16ToUtf8(WideString(ArchiveData.ArcName));
    AFormats := FindDecompressFormats(ArchiveName);
    for I := Low(AFormats) to High(AFormats) do
    begin
      Archive := AFormats[I].Create(ArchiveName, 0, False);
      try
        SetArchive(Archive);

        Archive.Password:= PasswordCache.GetPassword(ArchiveName);

        Archive.ListFiles;

        PasswordCache.SetPassword(ArchiveName, Archive.Password);

        Count:= Archive.ItemCount;

        if OpenMode = PK_OM_EXTRACT then
        begin
          SetLength(FileName, Count);
          SetLength(ProcessArray, Count);
        end;

        ArchiveData.OpenResult:= E_SUCCESS;

        Exit(TArcHandle(ResultHandle));
      except
        on E: Exception do
        begin
          ArchiveData.OpenResult:= GetArchiveError(E);
          FreeAndNil(Archive);
          Continue;
        end;
      end;
    end;
    if (Archive = nil) then Free;
  end;
  Result:= 0;
end;

function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer; winapi;
var
  FileNameW: UnicodeString;
  Item: TJclCompressionItem;
  Handle: TSevenZipHandle absolute hArcData;
begin
  with Handle do
  begin
    if Index >= Count then Exit(E_END_ARCHIVE);
    Item:= FArchive.Items[Index];
    FileNameW:= Item.PackedName;
    HeaderData.UnpSize:= Int64Rec(Item.FileSize).Lo;
    HeaderData.UnpSizeHigh:= Int64Rec(Item.FileSize).Hi;
    HeaderData.PackSize:= Int64Rec(Item.PackedSize).Lo;
    HeaderData.PackSizeHigh:= Int64Rec(Item.PackedSize).Hi;
    if ipAttributes in Item.ValidProperties then
      HeaderData.FileAttr:= SevenZipToWcxAttr(Item.Attributes)
    else begin
      HeaderData.FileAttr:= GENERIC_ATTRIBUTE_FILE;
    end;
    HeaderData.MfileTime:= UInt64(Item.LastWriteTime);
    if Item.Encrypted then begin
      HeaderData.Flags:= RHDF_ENCRYPTED;
    end;
    // Special case for absolute file name
    if (Length(FileNameW) > 0) and (FileNameW[1] = PathDelim) then
      HeaderData.FileName:= Copy(FileNameW, 2, Length(FileNameW) - 1)
    else begin
      HeaderData.FileName:= FileNameW;
    end;
    // Special case for BZip2, GZip and Xz archives
    if (HeaderData.FileName[0] = #0) then
    begin
      HeaderData.FileName:= GetNestedArchiveName(ArchiveName, Item);
    end;
  end;
  Result:= E_SUCCESS;
end;

function ProcessFileW(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PWideChar): Integer; winapi;
var
  Handle: TSevenZipHandle absolute hArcData;
begin
  try
    with Handle do
    case Operation of
      PK_TEST,
      PK_EXTRACT:
        begin
          if Operation = PK_EXTRACT then
          begin
            if Assigned(DestPath) then
            begin
              FileName[Index]:= IncludeTrailingPathDelimiter(Utf16ToUtf8(WideString(DestPath))) +
                                Utf16ToUtf8(WideString(DestName));
            end
            else begin
              FileName[Index]:= Utf16ToUtf8(WideString(DestName));
            end;
          end;
          Result:= E_SUCCESS;
          OperationMode:= Operation;
          ProcessArray[ProcessIndex]:= Index;
          Inc(ProcessIndex);
        end;
      else
        Result:= E_SUCCESS;
    end;
  finally
    Inc(Handle.Index);
  end;
end;

function CloseArchive(hArcData: TArcHandle): Integer; winapi;
var
  Handle: TSevenZipHandle absolute hArcData;
begin
  Result:= E_SUCCESS;
  if (hArcData <> wcxInvalidHandle) then
  with Handle do
  begin
    if OpenMode = PK_OM_EXTRACT then
    begin
      Start;
      Update;
    end;
    FArchive.Free;
    Free;
  end;
end;

procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW); winapi;
begin

end;

procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW); winapi;
var
  Handle: TSevenZipHandle absolute hArcData;
begin
  if (hArcData = wcxInvalidHandle) then
    ProcessDataProcT:= pProcessDataProc
  else begin
    Handle.ProcessDataProc:= pProcessDataProc;
  end;
end;

function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar;
  SrcPath: PWideChar; AddList: PWideChar; Flags: Integer): Integer; winapi;
var
  I, J: Integer;
  Encrypt: Boolean;
  AMessage: String;
  Password: WideString;
  FilePath: WideString;
  FileName: WideString;
  SfxModule: String = '';
  FileNameUTF8: String;
  AItem: TJclCompressionItem;
  AProgress: TSevenZipUpdate;
  Archive: TJclCompressArchive;
  AFormats: TJclCompressArchiveClassArray;
begin
  if not Verify then Exit(E_HANDLED);

  FileNameUTF8 := Utf16ToUtf8(WideString(PackedFile));

  // If update existing archive
  if (mbFileGetAttr(FileNameUTF8) <> faInvalidAttributes) then
    AFormats := TJclCompressArchiveClassArray(FindUpdateFormats(FileNameUTF8))
  else begin
    if not SameText(ExtractFileExt(FileNameUTF8), SevenZipSfxExt) then
      AFormats := FindCompressFormats(FileNameUTF8)
    else begin
      // Only 7-Zip supports self-extract
      SfxModule := ExtractFilePath(SevenzipLibraryName) + SevenZipSfxName;
      if FileExistsUTF8(SfxModule) then
      begin
        SetLength(AFormats, 1);
        AFormats[0] := TJcl7zCompressArchive;
      end
      else begin
        AMessage := SysErrorMessage(GetLastOSError) + LineEnding;
        AMessage += rsSevenZipSfxNotFound + LineEnding + SfxModule;
        MessageBox(AMessage, nil, MB_OK or MB_ICONERROR);
        Exit(E_NO_FILES);
      end;
    end;
  end;

  for I := Low(AFormats) to High(AFormats) do
  begin
    Archive := AFormats[I].Create(FileNameUTF8, 0, False);
    try
      AProgress:= TSevenZipUpdate.Create(Archive);

      if (Flags and PK_PACK_ENCRYPT) <> 0 then
      begin
        Encrypt:= Archive is IJclArchiveEncryptHeader;
        if not ShowPasswordQuery(Encrypt, Password) then
          Exit(E_EABORTED)
        else begin
          Archive.Password:= Password;
          if Archive is TJcl7zUpdateArchive then TJcl7zUpdateArchive(Archive).SetEncryptHeader(Encrypt);
          if Archive is TJcl7zCompressArchive then TJcl7zCompressArchive(Archive).SetEncryptHeader(Encrypt);
          if Archive is TJclZipUpdateArchive then TJclZipUpdateArchive(Archive).SetEncryptionMethod(emAES256);
          if Archive is TJclZipCompressArchive then TJclZipCompressArchive(Archive).SetEncryptionMethod(emAES256);
        end;
      end;

      if (Archive is TJclUpdateArchive) then
      try
        TJclUpdateArchive(Archive).ListFiles;
      except
        Continue;
      end;

      SetArchiveOptions(Archive);

      if Archive is TJcl7zCompressArchive then
      begin
        TJcl7zCompressArchive(Archive).SfxModule := SfxModule;
      end;

      if Assigned(SubPath) then
      begin
        FilePath:= WideString(SubPath);
        if FilePath[Length(FilePath)] <> PathDelim then
          FilePath := FilePath + PathDelim;
      end;

      while True do
      begin
        FileName := WideString(AddList);
        FileNameUTF8:= Utf16ToUtf8(WideString(SrcPath + FileName));
        if FileName[Length(FileName)] = PathDelim then
          Archive.AddDirectory(FilePath + Copy(FileName, 1, Length(FileName) - 1), FileNameUTF8)
        else
          Archive.AddFile(FilePath + FileName, FileNameUTF8);
        if (AddList + Length(FileName) + 1)^ = #0 then
          Break;
        Inc(AddList, Length(FileName) + 1);
      end;

      AProgress.Start;
      Result:= AProgress.Update;

      // If move files requested
      if (Result = E_SUCCESS) and (Flags and PK_PACK_MOVE_FILES <> 0) then
      begin
        // First remove files
        for J:= 0 to Archive.ItemCount - 1 do
        begin
          AItem:= Archive.Items[J];
          if AItem.OperationSuccess = osOK then
          begin
            if not AItem.Directory then DeleteFileUtf8(AItem.FileName);
          end;
        end;
        // Second remove directories
        for J:= Archive.ItemCount - 1 downto 0 do
        begin
          AItem:= Archive.Items[J];
          if AItem.Directory then RemoveDirUtf8(AItem.FileName);
        end;
      end;

      Exit;
    finally
      Archive.Free;
      AProgress.Free;
    end;
  end;
  Result:= E_NOT_SUPPORTED;
end;

function DeleteFilesW(PackedFile, DeleteList: PWideChar): Integer; winapi;
var
  I: Integer;
  PathEnd : WideChar;
  FileList : PWideChar;
  FileName : WideString;
  FileNameUTF8 : String;
  Archive: TJclUpdateArchive;
  AProgress: TSevenZipUpdate;
  AFormats: TJclUpdateArchiveClassArray;
begin
  FileNameUTF8 := Utf16ToUtf8(WideString(PackedFile));
  AFormats := FindUpdateFormats(FileNameUTF8);
  for I := Low(AFormats) to High(AFormats) do
  begin
    Archive := AFormats[I].Create(FileNameUTF8, 0, False);
    try
      AProgress:= TSevenZipUpdate.Create(Archive);

      try
        Archive.ListFiles;
      except
        Continue;
      end;

      // Parse file list.
      FileList := DeleteList;
      while FileList^ <> #0 do
      begin
        FileName := FileList;  // Convert PWideChar to WideString (up to first #0)
        PathEnd := (FileList + Length(FileName) - 1)^;
        // If ends with '.../*.*' or '.../' then delete directory.
        if (PathEnd = '*') or (PathEnd = PathDelim) then
          TJclSevenzipUpdateArchive(Archive).RemoveDirectory(WideExtractFilePath(FileName))
        else
          TJclSevenzipUpdateArchive(Archive).RemoveItem(FileName);

        FileList := FileList + Length(FileName) + 1; // move after filename and ending #0
        if FileList^ = #0 then
          Break;  // end of list
      end;

      AProgress.Start;
      Exit(AProgress.Update);
    finally
      Archive.Free;
      AProgress.Free;
    end;
  end;
  Result:= E_NOT_SUPPORTED;
end;

function CanYouHandleThisFileW(FileName: PWideChar): Boolean; winapi;
begin
  if not Is7ZipLoaded then Exit(False);
  Result:= FindDecompressFormats(Utf16ToUtf8(WideString(FileName))) <> nil;
end;

procedure ConfigurePacker(Parent: WcxPlugin.HWND; DllInstance: THandle); winapi;
begin
  ShowConfigurationDialog(Parent);
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); winapi;
var
  ModulePath: String;
begin
  DialogInitialize(StartupInfo);
  TranslateResourceStrings(StartupInfo);
  // Save configuration file name
  ConfigFile:= StartupInfo^.PluginConfDir + DefaultIniName;
  // Get plugin path
  ModulePath:= StartupInfo^.PluginDir;
  // Use configuration from plugin path
  if mbFileExists(ModulePath + DefaultIniName) then
  begin
    ConfigFile:= ModulePath + DefaultIniName;
  end;
  // Load plugin configuration
  LoadConfiguration;
  // Try to find library path
  if mbFileExists(LibraryPath) then
    SevenzipLibraryName:= LibraryPath
  else if Length(ModulePath) > 0 then
  begin
    if mbFileExists(ModulePath + TargetCPU + PathDelim + SevenzipDefaultLibraryName) then
      SevenzipLibraryName:= ModulePath + TargetCPU + PathDelim + SevenzipDefaultLibraryName
    else if mbFileExists(ModulePath + SevenzipDefaultLibraryName) then begin
      SevenzipLibraryName:= ModulePath + SevenzipDefaultLibraryName;
    end;
  end;
  if (SevenzipLibraryName = SevenzipDefaultLibraryName) then
  begin
    ModulePath:= mbGetEnvironmentVariable('COMMANDER_PATH') + PathDelim;
    if mbFileExists(ModulePath + SevenzipDefaultLibraryName) then
      SevenZipLibraryName:= ModulePath + SevenzipDefaultLibraryName
    else begin
      ModulePath:= mbExpandEnvironmentStrings(SevenZipDefaultLibraryPath);
      if mbFileExists(ModulePath + SevenZipDefaultLibraryName) then
        SevenZipLibraryName:= ModulePath + SevenZipDefaultLibraryName;
    end;
  end;
  // Replace TJclXzCompressArchive by TJclXzCompressArchiveEx
  GetArchiveFormats.UnregisterFormat(TJclXzCompressArchive);
  GetArchiveFormats.RegisterFormat(TJclXzCompressArchiveEx);
  // Don't process PE files as archives
  GetArchiveFormats.UnregisterFormat(TJclPeDecompressArchive);
  // Try to load 7z.dll
  if (Is7ZipLoaded or Load7Zip) then
    LoadLibraries
  else begin
    MessageBox(Format(rsSevenZipLoadError, [SevenZipDefaultLibraryName]) +
               LineEnding + GetLoadErrorStr, 'SevenZip', MB_OK or MB_ICONERROR);
  end;
  // Create password cache object
  PasswordCache:= TPasswordCache.Create;
end;

{ TSevenZipUpdate }

constructor TSevenZipUpdate.Create;
begin
  inherited Create(True);
  FPause:= TEventObject.Create(nil, False, False, '');
  FProgress:= TEventObject.Create(nil, False, False, '');
end;

constructor TSevenZipUpdate.Create(Archive: TJclCompressionArchive);
begin
  Create;
  FArchive:= Archive;
  Archive.OnPassword:= JclCompressionPassword;
end;

destructor TSevenZipUpdate.Destroy;
begin
  FPause.Free;
  FProgress.Free;
  inherited Destroy;
end;

procedure TSevenZipUpdate.Execute;
begin
  try
    (FArchive as TJclCompressArchive).Compress;
    ReturnValue:= E_SUCCESS;
  except
    on E: Exception do
      ReturnValue:= GetArchiveError(E);
  end;
  Terminate;
  FProgress.SetEvent;
end;

function TSevenZipUpdate.Update: Integer;
begin
  FArchive.OnProgress:= JclCompressionProgress;
  while not Terminated do
  begin
    // Wait progress event
    FProgress.WaitFor(INFINITE);
    // If the user has clicked on Cancel, the function returns zero
    FArchive.CancelCurrentOperation:= (ProcessDataProcT(PWideChar(FFileName), -FPercent) = 0);
    // Drop pause
    FPause.SetEvent;
  end;
  Result:= ReturnValue;
end;

procedure TSevenZipUpdate.JclCompressionPassword(Sender: TObject;
  var Password: WideString);
var
  Encrypt: Boolean = False;
begin
  if not ShowPasswordQuery(Encrypt, Password) then
    raise ESevenZipAbort.Create(EmptyStr);
end;

procedure TSevenZipUpdate.JclCompressionProgress(Sender: TObject; const Value, MaxValue: Int64);
begin
  if MaxValue > 0 then begin
    FPercent:= (Value * 100) div MaxValue;
  end;
  if FArchive.ItemCount > 0 then begin
    FFileName:= FArchive.Items[FArchive.CurrentItemIndex].PackedName;
  end;
  // Fire progress event
  FProgress.SetEvent;
  // Check pause progress
  FPause.WaitFor(INFINITE);
end;

{ TSevenZipHandle }

procedure TSevenZipHandle.Execute;
begin
  try
    SetLength(ProcessArray, ProcessIndex);
    TJclSevenzipDecompressArchive(FArchive).ProcessSelected(ProcessArray, OperationMode = PK_TEST);
    ReturnValue:= E_SUCCESS;
  except
    on E: Exception do
    begin
      ReturnValue:= GetArchiveError(E);
      MessageBox(E.Message, nil, MB_OK or MB_ICONERROR);
    end;
  end;
  Terminate;
  FProgress.SetEvent;
end;

function TSevenZipHandle.Update: Integer;
begin
  FArchive.OnProgress:= JclCompressionProgress;
  while not Terminated do
  begin
    // Wait progress event
    FProgress.WaitFor(INFINITE);
    if Assigned(ProcessDataProc) then
    begin
      // If the user has clicked on Cancel, the function returns zero
      FArchive.CancelCurrentOperation:= ProcessDataProc(PWideChar(FFileName), -FPercent) = 0;
    end;
    // Drop pause
    FPause.SetEvent;
  end;
  Result:= ReturnValue;
end;

procedure TSevenZipHandle.SetArchive(AValue: TJclDecompressArchive);
begin
  FArchive:= AValue;
  AValue.OnPassword := JclCompressionPassword;
  AValue.OnExtract  := JclCompressionExtract;
end;

function TSevenZipHandle.JclCompressionExtract(Sender: TObject; AIndex: Integer;
  var AFileName: TFileName; var Stream: TStream; var AOwnsStream: Boolean): Boolean;
begin
  Result:= True;
  AFileName:= FileName[AIndex];
end;

{ TPasswordCache }

procedure TPasswordCache.ResetTimer;
begin
  if FTimer.Interval > FInterval then
    FTimer.Interval:= FTimer.Interval - 1
  else
    FTimer.Interval:= FTimer.Interval + 1;
end;

procedure TPasswordCache.ZeroPassword;
begin
  if (Length(FArchivePassword) > 0) then
  begin
    FillWord(FArchivePassword[1], Length(FArchivePassword), 0);
    SetLength(FArchivePassword, 0);
  end;
end;

procedure TPasswordCache.TimerEvent(Sender: TObject);
begin
  FMutex.Acquire;
  try
    ZeroPassword;
    FTimer.Enabled:= False;
  finally
    FMutex.Release;
  end;
end;

function TPasswordCache.GetPassword(const Archive: String): WideString;
begin
  FMutex.Acquire;
  try
    if (SameText(FArchiveName, Archive)) and
       (FArchiveSize = FileSizeUtf8(Archive)) and
       (FArchiveTime = FileAgeUtf8(Archive)) then
    begin
      ResetTimer;
      Result:= FArchivePassword
    end
    else begin
      FTimer.Enabled:= False;
      Result:= EmptyWideStr;
      ZeroPassword;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TPasswordCache.SetPassword(const Archive: String; const Password: WideString);
begin
  FMutex.Acquire;
  try
    if (Length(Password) = 0) then
      FArchiveName:= EmptyStr
    else begin
      FArchiveName:= Archive;
      FArchivePassword:= Password;
      FArchiveTime:= FileAgeUtf8(Archive);
      FArchiveSize:= FileSizeUtf8(Archive);
      FTimer.Enabled:= True;
      ResetTimer;
    end;
  finally
    FMutex.Release;
  end;
end;

constructor TPasswordCache.Create;
begin
  FTimer:= TFPTimer.Create(nil);
  FTimer.UseTimerThread:= True;
  FTimer.OnTimer:= TimerEvent;
  FTimer.Interval:= FInterval;
  FMutex:= TCriticalSection.Create;
end;

destructor TPasswordCache.Destroy;
begin
  FTimer.Free;
  FMutex.Free;
  inherited Destroy;
end;

finalization
  PasswordCache.Free;

end.


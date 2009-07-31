unit uWcxArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Dialogs, DialogAPI,
  uWCXprototypes, uWCXhead, dynlibs, uClassesEx,
  StringHashList, uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uArchiveFileSource, uFileProperty, uFileSource, uFileSourceOperation;

type

  { Handles THeaderData and THeaderDataEx }
  TWCXHeader = class
  private
    function PCharLToUTF8(CharString: PChar; MaxSize: Integer): UTF8String;

  public
    ArcName: UTF8String;
    FileName: UTF8String;
    Flags,
    HostOS,
    FileCRC,
    FileTime,
    UnpVer,
    Method,
    FileAttr: Longint;
    PackSize,
    UnpSize: Int64;
    Cmt: UTF8String;
    CmtState: Longint;

    constructor Create(const Data: PHeaderData); overload;
    constructor Create(const Data: PHeaderDataEx); overload;
    constructor Create; overload; // allows creating empty record
  end;

  TWcxArchiveFileSource = class(TArchiveFileSource)
  private
    FModuleFileName: String;
    FModuleHandle: TLibHandle;  // Handle to .DLL or .so
    FPluginFlags: PtrInt;
    FArcFileList : TObjectList;

    // module's functions
  //**mandatory:
    OpenArchive : TOpenArchive;
    ReadHeader : TReadHeader;
    ProcessFile : TProcessFile;
    CloseArchive : TCloseArchive;
  //**optional:
    ReadHeaderEx : TReadHeaderEx;
    PackFiles : TPackFiles;
    DeleteFiles : TDeleteFiles;
    GetPackerCaps : TGetPackerCaps;
    ConfigurePacker : TConfigurePacker;
    SetChangeVolProc : TSetChangeVolProc;
    SetProcessDataProc : TSetProcessDataProc;
    StartMemPack : TStartMemPack;
    PackToMem : TPackToMem;
    DoneMemPack : TDoneMemPack;
    CanYouHandleThisFile : TCanYouHandleThisFile;
    PackSetDefaultParams : TPackSetDefaultParams;
    // Dialog API
    SetDlgProc: TSetDlgProc;

    function LoadModule: Boolean;
    procedure UnloadModule;

    function ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

    { Reads WCX header using ReadHeaderEx if available or ReadHeader. }
    function ReadWCXHeader(hArcData: TArcHandle;
                           out HeaderData: TWCXHeader): Integer;


  protected
    class function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    constructor Create(anArchiveFileName: String;
                       aWcxPluginFileName: String;
                       aWcxPluginFlags: PtrInt); reintroduce;
    destructor Destroy; override;

    function Clone: TWcxArchiveFileSource; override;
    procedure CloneTo(FileSource: TFileSource); override;

    // Retrieve operations permitted on the source.  = capabilities?
    class function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Returns a list of property types supported by this source for each file.
    class function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;

    // Retrieve some properties of the file source.
    class function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    // Each parameter will be owned by the operation (will be freed).
    function CreateListOperation: TFileSourceOperation; override;
{    function CreateCopyInOperation(var SourceFileSource: TFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String;
                                   RenameMask: String): TFileSourceOperation; virtual abstract;
    function CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String;
                                    RenameMask: String): TFileSourceOperation; virtual abstract;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; virtual abstract;
}

    class function CreateByArchiveName(anArchiveFileName: String): TWcxArchiveFileSource;

    property ArchiveFileList: TObjectList read FArcFileList;
  end;


implementation

uses Forms, Masks, uGlobs, uLog, uOSUtils, LCLProc,
     uDCUtils, uLng, Controls, fPackInfoDlg, fDialogBox, uGlobsPaths, FileUtil,
     uFileProcs, uFileSystemFile, uWcxArchiveListOperation;

const
  WcxIniFileName = 'wcx.ini';

{var
  WCXModule : TWCXModule = nil;  // used in ProcessDataProc}

class function TWcxArchiveFileSource.CreateByArchiveName(anArchiveFileName: String): TWcxArchiveFileSource;
var
  i: Integer;
  ModuleFileName: String;
  sExtension: String;
begin
  Result := nil;

  // Check if there is a registered plugin for the extension of the archive file name.
  for i := 0 to gWCXPlugins.Count - 1 do
  begin
    sExtension := ExtractFileExt(anArchiveFileName);
    if sExtension <> '' then   // delete '.' at the front
      Delete(sExtension, 1, 1);

    if sExtension = gWCXPlugins.Ext[i] then
    begin
      ModuleFileName := GetCmdDirFromEnvVar(gWCXPlugins.FileName[I]);

      Result := TWcxArchiveFileSource.Create(anArchiveFileName,
                                             ModuleFileName,
                                             gWCXPlugins.Flags[I]);

      debugln('Registered plugin ' + ModuleFileName + ' for archive');
      break;
    end;
  end;
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveFileSource.Create(anArchiveFileName: String;
                                         aWcxPluginFileName: String;
                                         aWcxPluginFlags: PtrInt);
begin
  inherited Create(anArchiveFileName);

  FModuleFileName := aWcxPluginFileName;
  FPluginFlags := aWcxPluginFlags;
  FArcFileList := TObjectList.Create(True);
  FModuleHandle := 0;

  LoadModule;
  ReadArchive;
end;

destructor TWcxArchiveFileSource.Destroy;
begin
  if Assigned(FArcFileList) then
    FreeAndNil(FArcFileList);
  UnloadModule;
end;

function TWcxArchiveFileSource.Clone: TWcxArchiveFileSource;
begin
  Result := TWcxArchiveFileSource.Create(FCurrentAddress, FModuleFileName, FPluginFlags);
  CloneTo(Result);
end;

procedure TWcxArchiveFileSource.CloneTo(FileSource: TFileSource);
begin
  if Assigned(FileSource) then
  begin
    inherited CloneTo(FileSource);

    // Clone FArcFileList : TList;
    // probably don't copy module handle and function addresses?
  end;
end;

class function TWcxArchiveFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList];
end;

class function TWcxArchiveFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

class function TWcxArchiveFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [];
end;

class function TWcxArchiveFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TWcxArchiveFileSource.LoadModule: Boolean;
var
  PackDefaultParamStruct : TPackDefaultParamStruct;
  SetDlgProcInfo: TSetDlgProcInfo;
  sPluginDir: WideString;
  sPluginConfDir: WideString;
begin
  FModuleHandle := mbLoadLibrary(FModuleFileName);
  debugln('loaded ' + FModuleFileName + ' at ' + hexStr(Pointer(FModuleHandle)));
  if FModuleHandle = 0 then
    Exit;

  // mandatory functions
  OpenArchive:= TOpenArchive(GetProcAddress(FModuleHandle,'OpenArchive'));
  ReadHeader:= TReadHeader(GetProcAddress(FModuleHandle,'ReadHeader'));
  ReadHeaderEx:= TReadHeaderEx(GetProcAddress(FModuleHandle,'ReadHeaderEx'));
  ProcessFile:= TProcessFile(GetProcAddress(FModuleHandle,'ProcessFile'));
  CloseArchive:= TCloseArchive(GetProcAddress(FModuleHandle,'CloseArchive'));
  if (OpenArchive = nil) or (ReadHeader = nil) or
     (ProcessFile = nil) or (CloseArchive = nil) then
    begin
      OpenArchive := nil;
      ReadHeader:= nil;
      ProcessFile := nil;
      CloseArchive := nil;
      Result := False;
      Exit;
    end;
  // optional functions
  PackFiles:= TPackFiles(GetProcAddress(FModuleHandle,'PackFiles'));
  DeleteFiles:= TDeleteFiles(GetProcAddress(FModuleHandle,'DeleteFiles'));
  GetPackerCaps:= TGetPackerCaps(GetProcAddress(FModuleHandle,'GetPackerCaps'));
  ConfigurePacker:= TConfigurePacker(GetProcAddress(FModuleHandle,'ConfigurePacker'));
  SetChangeVolProc:= TSetChangeVolProc(GetProcAddress(FModuleHandle,'SetChangeVolProc'));
  SetProcessDataProc:= TSetProcessDataProc(GetProcAddress(FModuleHandle,'SetProcessDataProc'));
  StartMemPack:= TStartMemPack(GetProcAddress(FModuleHandle,'StartMemPack'));
  PackToMem:= TPackToMem(GetProcAddress(FModuleHandle,'PackToMem'));
  DoneMemPack:= TDoneMemPack(GetProcAddress(FModuleHandle,'DoneMemPack'));
  CanYouHandleThisFile:= TCanYouHandleThisFile(GetProcAddress(FModuleHandle,'CanYouHandleThisFile'));
  PackSetDefaultParams:= TPackSetDefaultParams(GetProcAddress(FModuleHandle,'PackSetDefaultParams'));
  // Dialog API function
  SetDlgProc:= TSetDlgProc(GetProcAddress(FModuleHandle,'SetDlgProc'));

  if Assigned(PackSetDefaultParams) then
    begin
      with PackDefaultParamStruct do
        begin
          Size := SizeOf(PackDefaultParamStruct);
          PluginInterfaceVersionLow := 10;
          PluginInterfaceVersionHi := 2;
          DefaultIniName := gpIniDir + WcxIniFileName;
        end;
      PackSetDefaultParams(@PackDefaultParamStruct);
    end;

  // Dialog API
  if Assigned(SetDlgProc) then
    begin
      sPluginDir := UTF8Decode(ExtractFilePath(FModuleFileName));
      sPluginConfDir := UTF8Decode(gpIniDir);

      with SetDlgProcInfo do
      begin
        PluginDir:= PWideChar(sPluginDir);
        PluginConfDir:= PWideChar(sPluginConfDir);
        InputBox:= @fDialogBox.InputBox;
        MessageBox:= @fDialogBox.MessageBox;
        DialogBox:= @fDialogBox.DialogBox;
        DialogBoxEx:= @fDialogBox.DialogBoxEx;
        SendDlgMsg:= @fDialogBox.SendDlgMsg;
      end;
      SetDlgProc(SetDlgProcInfo);
    end;

  Result := True;
end;

procedure TWcxArchiveFileSource.UnloadModule;
begin
  if FModuleHandle <> 0 then
  begin
    FreeLibrary(FModuleHandle);
    FModuleHandle := 0;
  end;

  OpenArchive:= nil;
  ReadHeader:= nil;
  ReadHeaderEx:= nil;
  ProcessFile:= nil;
  CloseArchive:= nil;
  PackFiles:= nil;
  DeleteFiles:= nil;
  GetPackerCaps:= nil;
  ConfigurePacker:= nil;
  SetChangeVolProc:= nil;
  SetProcessDataProc:= nil;
  StartMemPack:= nil;
  PackToMem:= nil;
  DoneMemPack:= nil;
  CanYouHandleThisFile:= nil;
  PackSetDefaultParams:= nil;
end;

function TWcxArchiveFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TWcxArchiveFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWcxArchiveListOperation.Create(TargetFileSource);
end;

function TWcxArchiveFileSource.ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

  procedure CollectDirs(Path: PAnsiChar; var DirsList: TStringHashList);
  var
    I : Integer;
    Dir : AnsiString;
  begin
    // Scan from the second char from the end, to the second char from the beginning.
    for I := strlen(Path) - 2 downto 1 do
    begin
      if Path[I] = PathDelim then
      begin
        SetString(Dir, Path, I);
        if DirsList.Find(Dir) = -1 then
          // Add directory and continue scanning for parent directories.
          DirsList.Add(Dir)
        else
          // This directory is already in the list and we assume
          // that all parent directories are too.
          Exit;
      end
    end;
  end;

var
  ArcHandle : TArcHandle;
  ArcFile : tOpenArchiveData;
  Header: TWCXHeader;
  AllDirsList, ExistsDirList : TStringHashList;
  I : Integer;
  NameLength: Integer;
  iResult : Integer;
begin
  if not mbFileAccess(ArchiveFileName, fmOpenRead) then
    begin
      Result := False;
      Exit;
    end;

  if bCanYouHandleThisFile and Assigned(CanYouHandleThisFile) then
    begin
      Result := CanYouHandleThisFile(PChar(UTF8ToSys(ArchiveFileName)));
      if not Result then Exit;
    end;

  DebugLN('Open Archive');

  (*Open Archive*)
  FillChar(ArcFile, SizeOf(ArcFile), #0);
  ArcFile.ArcName := PChar(UTF8ToSys(ArchiveFileName));
  ArcFile.OpenMode := PK_OM_LIST;

  try
    ArcHandle := OpenArchive(ArcFile);
  except
    ArcHandle := 0;
  end;

  if ArcHandle = 0 then
    begin
      {if not bCanYouHandleThisFile then
        ShowErrorMsg(ArcFile.OpenResult);}
      Result := False;
      Exit;
    end;

//  WCXModule := Self;  // set WCXModule variable to current module
{  SetChangeVolProc(ArcHandle, ChangeVolProc);
  SetProcessDataProc(ArcHandle, ProcessDataProc);}

  DebugLN('Get File List');
  (*Get File List*)
  FArcFileList.Clear;
  ExistsDirList := TStringHashList.Create(True);
  AllDirsList := TStringHashList.Create(True);

  try
    while (ReadWCXHeader(ArcHandle, Header) = E_SUCCESS) do
      begin
        // Some plugins end directories with path delimiter. Delete it if present.
        if FPS_ISDIR(Header.FileAttr) then
        begin
          NameLength := Length(Header.FileName);
          if (Header.FileName[NameLength] = PathDelim) then
            Delete(Header.FileName, NameLength, 1);

        //****************************
        (* Workaround for plugins that don't give a list of folders
           or the list does not include all of the folders. *)

          // Collect directories that the plugin supplies.
          if (ExistsDirList.Find(Header.FileName) < 0) then
            ExistsDirList.Add(Header.FileName);
        end;

        // Collect all directories.
        CollectDirs(PAnsiChar(Header.FileName), AllDirsList);

        //****************************

        FArcFileList.Add(Header);

        // get next file
        iResult := ProcessFile(ArcHandle, PK_SKIP, nil, nil);

        //Check for errors
        {if iResult <> E_SUCCESS then
          ShowErrorMessage;}
      end; // while

      (* if plugin does not give a list of folders *)
      for I := 0 to AllDirsList.Count - 1 do
      begin
        // Add only those directories that were not supplied by the plugin.
        if ExistsDirList.Find(AllDirsList.List[I]^.Key) < 0 then
        begin
          Header := TWCXHeader.Create;
          try
            Header.FileName := AllDirsList.List[I]^.Key;
            Header.ArcName  := ArchiveFileName;
            Header.FileAttr := faFolder;
            Header.FileTime := mbFileAge(ArchiveFileName);
            FArcFileList.Add(Header);
          except
            FreeAndNil(Header);
          end;
        end;
      end;

  finally
    AllDirsList.Free;
    ExistsDirList.Free;
    CloseArchive(ArcHandle);
  end;

  Result := True;
end;

function TWcxArchiveFileSource.ReadWCXHeader(hArcData: TArcHandle;
                                             out HeaderData: TWCXHeader): Integer;
var
  ArcHeader : THeaderData;
  ArcHeaderEx : THeaderDataEx;
begin
  HeaderData := nil;

  if Assigned(ReadHeaderEx) then
  begin
    FillChar(ArcHeaderEx, SizeOf(ArcHeaderEx), #0);
    Result := ReadHeaderEx(hArcData, ArcHeaderEx);
    if Result = E_SUCCESS then
    begin
      HeaderData := TWCXHeader.Create(PHeaderDataEx(@ArcHeaderEx));
    end;
  end
  else if Assigned(ReadHeader) then
  begin
    FillChar(ArcHeader, SizeOf(ArcHeader), #0);
    Result := ReadHeader(hArcData, ArcHeader);
    if Result = E_SUCCESS then
    begin
      HeaderData := TWCXHeader.Create(PHeaderData(@ArcHeader));
    end;
  end
  else
  begin
    Result := E_NOT_SUPPORTED;
  end;
end;

{ TWCXHeader }

constructor TWCXHeader.Create(const Data: PHeaderData);
begin
  ArcName  := PCharLToUTF8(Data^.ArcName, SizeOf(Data^.ArcName));
  FileName := PCharLToUTF8(Data^.FileName, SizeOf(Data^.FileName));
  Flags    := Data^.Flags;
  HostOS   := Data^.HostOS;
  FileCRC  := Data^.FileCRC;
  FileTime := Data^.FileTime;
  UnpVer   := Data^.UnpVer;
  Method   := Data^.Method;
  FileAttr := Data^.FileAttr;
  PackSize := Data^.PackSize;
  UnpSize  := Data^.UnpSize;
  if Assigned(Data^.CmtBuf) then
    Cmt := PCharLToUTF8(Data^.CmtBuf, Data^.CmtSize);
  CmtState := Data^.CmtState;
end;

constructor TWCXHeader.Create(const Data: PHeaderDataEx);

  function Combine64(High, Low: Longint): Int64;
  begin
    Result := Int64(High) shl (SizeOf(Int64) shl 2);
    Result := Result + Int64(Low);
  end;

begin
  ArcName  := PCharLToUTF8(Data^.ArcName, SizeOf(Data^.ArcName));
  FileName := PCharLToUTF8(Data^.FileName, SizeOf(Data^.FileName));
  Flags    := Data^.Flags;
  HostOS   := Data^.HostOS;
  FileCRC  := Data^.FileCRC;
  FileTime := Data^.FileTime;
  UnpVer   := Data^.UnpVer;
  Method   := Data^.Method;
  FileAttr := Data^.FileAttr;
  PackSize := Combine64(Data^.PackSizeHigh, Data^.PackSize);
  UnpSize  := Combine64(Data^.UnpSizeHigh, Data^.UnpSize);
  if Assigned(Data^.CmtBuf) then
    Cmt := PCharLToUTF8(Data^.CmtBuf, Data^.CmtSize);
  CmtState := Data^.CmtState;
end;

constructor TWCXHeader.Create;
begin
end;

function TWCXHeader.PCharLToUTF8(CharString: PChar; MaxSize: Integer): UTF8String;
var
  NameLength: Integer;
  TempString: AnsiString;
begin
  NameLength := strlen(CharString);
  if NameLength > MaxSize then
    NameLength := MaxSize;

  SetString(TempString, CharString, NameLength);
  Result := SysToUTF8(TempString);
end;

end.


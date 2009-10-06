unit uWfxPluginFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWFXModule,
  uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uFileProperty, uFileSource, uFileSourceOperation;

const
  WFX_SUCCESS      =  0;
  WFX_NOTSUPPORTED = -1;
  WFX_ERROR        = -2;

type

  { TWfxPluginFileSource }

  TWfxPluginFileSource = class(TFileSource)
  private
    FModuleFileName: UTF8String;
    FWFXModule: TWFXModule;

  protected
    class function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetCurrentAddress: String; override;
  public
    procedure FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
    procedure WfxStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
    function WfxMkDir(const sDirName: UTF8String): LongInt;
    function WfxRemoveDir(const sDirName: UTF8String): Boolean;
    function WfxDeleteFile(const sFileName: UTF8String): Boolean;
  public
    constructor Create(aModuleFileName: UTF8String); reintroduce;
    destructor Destroy; override;

    function Clone: TWfxPluginFileSource; override;
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
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(DirectoryPath: String): TFileSourceOperation; override;

    class function CreateByRootName(aRootName: String): TWfxPluginFileSource;

    property WfxModule: TWfxModule read FWfxModule;

  end;


implementation

uses
  LCLProc, FileUtil, uGlobs, uDCUtils,
  uWfxPluginListOperation, uWfxPluginCreateDirectoryOperation, uWfxPluginDeleteOperation,
  uWfxPluginFile, ufsplugin;

constructor TWfxPluginFileSource.Create(aModuleFileName: UTF8String);
begin
  inherited Create;
  CurrentPath:= PathDelim;
  FModuleFileName:= aModuleFileName;
  FWFXModule:= TWFXModule.Create;
  FWFXModule.LoadModule(FModuleFileName);
end;

destructor TWfxPluginFileSource.Destroy;
begin
  inherited Destroy;
end;

function TWfxPluginFileSource.Clone: TWfxPluginFileSource;
begin
  Result := TWfxPluginFileSource.Create(FModuleFileName);
  CloneTo(Result);
end;

procedure TWfxPluginFileSource.CloneTo(FileSource: TFileSource);
begin
  if Assigned(FileSource) then
  begin
    inherited CloneTo(FileSource);
  end;
end;

class function TWfxPluginFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoDelete, fsoCreateDirectory];
end;

class function TWfxPluginFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

class function TWfxPluginFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [];
end;

class function TWfxPluginFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TWfxPluginFileSource.GetCurrentAddress: String;
begin
  Result:= 'wfx://' + FModuleFileName;
end;

procedure TWfxPluginFileSource.FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    FindData: TWin32FindData;
    Handle: THandle;
    aFile: TWfxPluginFile;
  begin
    with FWfxModule do
    begin
      Handle := FsFindFirst(PChar(UTF8ToSys(srcPath)), FindData);
      if Handle = feInvalidHandle then Exit;

      repeat
        if (FindData.cFileName = '.') or (FindData.cFileName = '..') then Continue;
        aFile:= TWfxPluginFile.Create(FindData);
        aFile.Path:= srcPath;
        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            FillAndCountRec(srcPath + SysToUTF8(FindData.cFileName) + PathDelim);
          end
        else
          begin
            Inc(FilesSize, aFile.Size);
            Inc(FilesCount);
          end;
      until not FsFindNext(Handle, FindData);

      FsFindClose(Handle);
    end;
  end;

var
  I: Integer;
  aFile: TWfxPluginFile;
begin
  NewFiles := TFiles.Create;
  FilesCount:= 0;
  FilesSize:= 0;
  for I := 0 to Files.Count - 1 do
  begin
    aFile := Files[I] as TWfxPluginFile;

    NewFiles.Add(aFile.Clone);

    if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
      begin
        FillAndCountRec(aFile.Path + aFile.Name + DirectorySeparator);  // recursive browse child dir
      end
    else
      begin
        Inc(FilesCount);
        Inc(FilesSize, aFile.Size); // in first level we know file size -> use it
      end;
  end;
end;

procedure TWfxPluginFileSource.WfxStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
begin
  with FWfxModule do
  begin
    if Assigned(FsStatusInfo) then
      FsStatusInfo(PChar(UTF8ToSys(RemoteDir)), InfoStartEnd, InfoOperation);
  end;
end;

function TWfxPluginFileSource.WfxMkDir(const sDirName: UTF8String): LongInt;
begin
  with FWfxModule do
  begin
    Result:= WFX_NOTSUPPORTED;
    if Assigned(FsMkDir) then
      begin
        WfxStatusInfo(CurrentPath, FS_STATUS_START, FS_STATUS_OP_MKDIR);
        if FsMkDir(PChar(UTF8ToSys(sDirName))) then
          Result:= WFX_SUCCESS
        else
          Result:= WFX_ERROR;
        WfxStatusInfo(CurrentPath, FS_STATUS_END, FS_STATUS_OP_MKDIR);
      end;
  end;
end;

function TWfxPluginFileSource.WfxRemoveDir(const sDirName: UTF8String): Boolean;
begin
  with FWfxModule do
  begin
    Result:= False;
    if Assigned(FsRemoveDir) then
      begin
        Result:= FsRemoveDir(PChar(UTF8ToSys(sDirName)));
      end;
  end;
end;

function TWfxPluginFileSource.WfxDeleteFile(const sFileName: UTF8String): Boolean;
begin
  with FWfxModule do
  begin
    Result:= False;
    if Assigned(FsDeleteFile) then
      begin
        Result:= FsDeleteFile(PChar(UTF8ToSys(sFileName)));
      end;
  end;
end;

function TWfxPluginFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginListOperation.Create(TargetFileSource);
end;

function TWfxPluginFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TWfxPluginFileSource.CreateCreateDirectoryOperation(DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginCreateDirectoryOperation.Create(TargetFileSource, DirectoryPath);
end;

class function TWfxPluginFileSource.CreateByRootName(aRootName: String): TWfxPluginFileSource;
var
  I: Integer;
  sModuleFileName: UTF8String;
begin
  Result:= nil;

  if gWFXPlugins.Count = 0 then Exit;
  // Check if there is a registered plugin for the name of the file system plugin.
  sModuleFileName:= gWFXPlugins.Values[aRootName];
  if sModuleFileName <> EmptyStr then
    begin
      sModuleFileName:= GetCmdDirFromEnvVar(sModuleFileName);
      Result:= TWfxPluginFileSource.Create(sModuleFileName);

      DebugLn('Registered plugin ' + sModuleFileName + ' for file system ' + aRootName);
    end;
end;

end.


unit uWcxArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Dialogs, StringHashList, uOSUtils,
  uWCXhead, uWCXmodule, uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uArchiveFileSource, uFileProperty, uFileSource, uFileSourceOperation;

type
  TWcxArchiveFileSource = class(TArchiveFileSource)
  private
    FModuleFileName: String;
    FPluginFlags: PtrInt;
    FArcFileList : TObjectList;
    FWcxModule: TWCXModule;

    function LoadModule: Boolean;
    procedure UnloadModule;

    function ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

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
}
    function CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
{
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; virtual abstract;
}

    class function CreateByArchiveName(anArchiveFileName: String): TWcxArchiveFileSource;

    property ArchiveFileList: TObjectList read FArcFileList;
    property PluginFlags: PtrInt read FPluginFlags;
    property WcxModule: TWCXModule read FWcxModule;
  end;

implementation

uses Forms, Controls, uGlobs, LCLProc, uDCUtils,
     uGlobsPaths, FileUtil, uWcxArchiveFile, uWcxArchiveListOperation,
     uWcxArchiveCopyOutOperation;

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

    if (sExtension = gWCXPlugins.Ext[i]) and (gWCXPlugins.Enabled[i]) then
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
  FWcxModule := TWCXModule.Create;

  LoadModule;
  ReadArchive;
end;

destructor TWcxArchiveFileSource.Destroy;
begin
  UnloadModule;

  inherited;

  if Assigned(FArcFileList) then
    FreeAndNil(FArcFileList);
  if Assigned(FWcxModule) then
    FreeAndNil(FWcxModule);
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
  Result := [fsoList, fsoCopyOut];
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
  Result := TWcxArchiveFile.GetSupportedProperties;
end;

function TWcxArchiveFileSource.LoadModule: Boolean;
begin
  WcxModule.VFSInit(FPluginFlags);
  Result := WcxModule.LoadModule(FModuleFileName);
end;

procedure TWcxArchiveFileSource.UnloadModule;
begin
  WcxModule.UnloadModule;
end;

function TWcxArchiveFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWcxArchiveListOperation.Create(TargetFileSource);
end;

function TWcxArchiveFileSource.CreateCopyOutOperation(
            var TargetFileSource: TFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: TFileSource;
begin
  SourceFileSource := Self.Clone;
  Result := TWcxArchiveCopyOutOperation.Create(SourceFileSource,
                                               TargetFileSource,
                                               SourceFiles, TargetPath);
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
  Header: TWCXHeader;
  AllDirsList, ExistsDirList : TStringHashList;
  I : Integer;
  NameLength: Integer;
  iResult : Integer;
  lOpenResult : Longint;
begin
  if not mbFileAccess(ArchiveFileName, fmOpenRead) then
    begin
      Result := False;
      Exit;
    end;

  if bCanYouHandleThisFile and Assigned(WcxModule.CanYouHandleThisFile) then
    begin
      Result := WcxModule.CanYouHandleThisFile(PChar(UTF8ToSys(ArchiveFileName)));
      if not Result then Exit;
    end;

  DebugLN('Open Archive');

  (*Open Archive*)
  ArcHandle := WcxModule.OpenArchiveHandle(ArchiveFileName, PK_OM_LIST, lOpenResult);
  if ArcHandle = 0 then
    begin
      {if not bCanYouHandleThisFile then
        ShowErrorMsg(lOpenResult);}
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
    while (WcxModule.ReadWCXHeader(ArcHandle, Header) = E_SUCCESS) do
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
        iResult := WcxModule.ProcessFile(ArcHandle, PK_SKIP, nil, nil);

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
    WcxModule.CloseArchive(ArcHandle);
  end;

  Result := True;
end;

end.


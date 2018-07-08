unit uGioFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, URIParser, SyncObjs,
  uFileSourceProperty, uFileSourceOperationTypes,
  uRealFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile, uGLib2, uGObject2, uGio2;

type
    IGioFileSource = interface(IRealFileSource)
      ['{6DC5BCCA-BDD5-43DA-A0D6-7BAA26D93B92}']
      function MountPath(AFile: PGFile; out AError: PGError): Boolean;
    end;

    { TGioFileSource }

    TGioFileSource = class(TRealFileSource, IGioFileSource)
    private
      MountTry: Integer;
      MountLoop: TSimpleEvent;
      MountError: PGError;

    protected
      function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    public
      function MountPath(AFile: PGFile; out AError: PGError): Boolean;

    public
      constructor Create(const URI: TURI); override;

      class function IsSupportedPath(const Path: String): Boolean; override;

      function CreateDirectory(const Path: String): Boolean; override;
      function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

      class function CreateFile(const APath: String): TFile; override;
      class function CreateFile(const APath: String; AFolder: PGFile; AFileInfo: PGFileInfo): TFile;

      procedure Reload(const PathsToReload: TPathsArray); override;
      function GetParentDir(sPath : String): String; override;
      function IsPathAtRoot(Path: String): Boolean; override;
      function GetRootDir(sPath: String): String; override; overload;
      function GetRootDir: String; override; overload;

      // Retrieve some properties of the file source.
      function GetProperties: TFileSourceProperties; override;
      function GetOperationsTypes: TFileSourceOperationTypes; override;

      // These functions create an operation object specific to the file source.
      function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
      function CreateCopyOperation(var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
      function CreateCopyInOperation(SourceFileSource: IFileSource;
                                     var SourceFiles: TFiles;
                                     TargetPath: String): TFileSourceOperation; override;
      function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                      var SourceFiles: TFiles;
                                      TargetPath: String): TFileSourceOperation; override;
      function CreateMoveOperation(var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
      function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
      function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
      function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
      function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                              var theNewProperties: TFileProperties): TFileSourceOperation; override;
    end;

implementation

uses
  DCFileAttributes, DCDateTimeUtils, uGioListOperation, uGioCopyOperation,
  uGioDeleteOperation, uGioExecuteOperation, uGioCreateDirectoryOperation,
  uGioMoveOperation, uGioSetFilePropertyOperation, uDebug, fGioAuthDlg,
  DCBasicTypes, DCStrUtils, uShowMsg;

{ TGioFileSource }

function TGioFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result:= [fsoList, fsoCopy, fsoCopyIn, fsoCopyOut, fsoDelete, fsoExecute, fsoCreateDirectory, fsoMove, fsoSetFileProperty];
end;

class function TGioFileSource.CreateFile(const APath: String): TFile;
begin
  Result:=inherited CreateFile(APath);

  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    CreationTimeProperty := TFileCreationDateTimeProperty.Create;
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create;
    LinkProperty := TFileLinkProperty.Create;
    OwnerProperty := TFileOwnerProperty.Create;
    TypeProperty := TFileTypeProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;

class function TGioFileSource.CreateFile(const APath: String; AFolder: PGFile;
  AFileInfo: PGFileInfo): TFile;
var
  AFile: PGFile;
  ATarget: Pgchar;
  AFileType: TGFileType;
  ASymlinkInfo: PGFileInfo;
begin
  Result:= CreateFile(APath);
  Result.Name:= g_file_info_get_name(AFileInfo);
  Result.Size:= g_file_info_get_size (AFileInfo);
  Result.Attributes:= g_file_info_get_attribute_uint32 (AFileInfo, FILE_ATTRIBUTE_UNIX_MODE);
  Result.ModificationTime:= UnixFileTimeToDateTime(g_file_info_get_attribute_uint64 (AFileInfo, FILE_ATTRIBUTE_TIME_MODIFIED));
  Result.LinkProperty := TFileLinkProperty.Create;

  // Get a file's type (whether it is a regular file, symlink, etc).
  AFileType:= g_file_info_get_file_type (AFileInfo);

  if AFileType = G_FILE_TYPE_DIRECTORY then
    begin
      Result.Attributes:= Result.Attributes or S_IFDIR;
    end
  else if AFileType =  G_FILE_TYPE_SYMBOLIC_LINK then
    begin
      ATarget:= g_file_info_get_symlink_target(AFileInfo);
      AFile:= g_file_get_child(AFolder, ATarget);

      ASymlinkInfo := g_file_query_info (AFile, FILE_ATTRIBUTE_STANDARD_TYPE,
                                         G_FILE_QUERY_INFO_NONE, nil, nil);

      Result.LinkProperty.LinkTo := ATarget;
      Result.LinkProperty.IsValid := Assigned(ASymlinkInfo);

      if (Result.LinkProperty.IsValid) then
      begin
        AFileType:= g_file_info_get_file_type(ASymlinkInfo);
        Result.LinkProperty.IsLinkToDirectory := (AFileType = G_FILE_TYPE_DIRECTORY);
        g_object_unref(ASymlinkInfo);
      end;
      g_object_unref(PGObject(AFile));
    end
  else if AFileType in [G_FILE_TYPE_SHORTCUT, G_FILE_TYPE_MOUNTABLE] then
  begin
    Result.Attributes:= Result.Attributes or S_IFLNK or S_IFDIR;
    ATarget:= g_file_info_get_attribute_string(AFileInfo, FILE_ATTRIBUTE_STANDARD_TARGET_URI);
    Result.LinkProperty.IsValid := Length(ATarget) > 0;
    Result.LinkProperty.LinkTo := ATarget;
  end;
end;

procedure TGioFileSource.Reload(const PathsToReload: TPathsArray);
var
  Index: Integer;
  PathList: TPathsArray;
begin
  SetLength(PathList, Length(PathsToReload));
  for Index:= Low(PathsToReload) to High(PathsToReload) do
  begin
    PathList[Index]:= StringReplace(PathsToReload[Index], FCurrentAddress, '', []);
  end;
  inherited Reload(PathList);
end;

function TGioFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result:= TRue; //inherited SetCurrentWorkingDirectory(NewDir);
end;

procedure ask_password_cb (op: PGMountOperation;
                               const message: Pgchar;
                               const default_user: Pgchar;
                               const default_domain: Pgchar;
                               flags: TGAskPasswordFlags;
                               user_data: gpointer); cdecl;
var
  UserName,
  Password,
  Domain: String;
  password_save: TGPasswordSave;
  mount_handled: gboolean = FALSE;
  FileSource: TGioFileSource absolute user_data;
begin
  Inc(FileSource.MountTry);

  //*  First pass, look if we have a password to supply  */
  if (FileSource.MountTry = 1) then
  begin
    if ((flags and G_ASK_PASSWORD_NEED_USERNAME <> 0) and (Length(FileSource.FURI.Username) > 0)) then
    begin
      g_printf ('(WW) ask_password_cb: mount_try = %d, trying login with saved username...\n', [FileSource.MountTry]);
      g_mount_operation_set_username (op, Pgchar(FileSource.FURI.Username));
      mount_handled := TRUE;
    end;
    if ((flags and G_ASK_PASSWORD_NEED_PASSWORD <> 0) and (Length(FileSource.FURI.Password) > 0)) then
    begin
      g_printf ('(WW) ask_password_cb: mount_try = %d, trying login with saved password...\n', [FileSource.MountTry]);
      g_mount_operation_set_password (op, Pgchar(FileSource.FURI.Password));
      mount_handled := TRUE;
    end;
    if (mount_handled) then
    begin
      g_mount_operation_reply (op, G_MOUNT_OPERATION_HANDLED);
      Exit;
    end;
  end;

  //*  Handle abort message from certain backends properly  */
  //*   - e.g. SMB backends use this to mask multiple auth callbacks from smbclient  */
  if ((default_user <> nil) and (strcomp(default_user, 'ABORT') = 0)) then
  begin
    g_print ('(WW) default_user == "ABORT", aborting\n', []);
    g_mount_operation_reply (op, G_MOUNT_OPERATION_ABORTED);
    Exit;
  end;

  password_save := G_PASSWORD_SAVE_NEVER;

  Username:= default_user;
  Domain:= default_domain;

  if not ShowAuthDlg(message, flags, UserName, Domain, Password) then
    g_mount_operation_reply (op, G_MOUNT_OPERATION_ABORTED)
  else begin
    if (flags and G_ASK_PASSWORD_NEED_USERNAME <> 0) then
      g_mount_operation_set_username (op, Pgchar(Username));
    if (flags and G_ASK_PASSWORD_NEED_DOMAIN <> 0) then
      g_mount_operation_set_domain (op, Pgchar(Domain));
    if (flags and G_ASK_PASSWORD_NEED_PASSWORD <> 0) then
      g_mount_operation_set_password (op, Pgchar(password));
    if (flags and G_ASK_PASSWORD_ANONYMOUS_SUPPORTED <> 0) then
      g_mount_operation_set_anonymous (op, True);
  end;

  if (flags and G_ASK_PASSWORD_SAVING_SUPPORTED <> 0) then
    g_mount_operation_set_password_save (op, password_save);

  g_mount_operation_reply (op, G_MOUNT_OPERATION_HANDLED);
end;

procedure ask_question_cb(op: PGMountOperation;
                          const message: Pgchar;
                          const choices: PPgchar;
                          user_data: gpointer);  cdecl;
var
  len: Integer = 0;
  choice: Integer = -1;
  buttons: TDynamicStringArray;
begin
  g_print('(WW) ask_question_cb: message = "%s"\n', [message]);

  while (choices[len] <> nil) do
  begin
    AddString(buttons, StrPas(choices[len]));
    g_print('(WW) ask_question_cb: choice[%d] = "%s"\n', [len, choices[len]]);
    Inc(len);
  end;

  DCDebug('  (II) Spawning callback_ask_question...');
  // At this moment, only SFTP uses ask_question and the second button is cancellation
  choice:= MsgChoiceBox(nil, message, buttons);
  g_print('    (II) Received choice = %d\n', [choice]);

  if (choice < 0) then
    g_mount_operation_reply(op, G_MOUNT_OPERATION_ABORTED)
  else begin
    g_mount_operation_set_choice(op, choice);
    g_mount_operation_reply(op, G_MOUNT_OPERATION_HANDLED);
  end;
end;

procedure mount_done_cb (object_: PGObject;
                           res: PGAsyncResult;
                           user_data: gpointer); cdecl;
var
  Result: gboolean;
  FileSource: TGioFileSource absolute user_data;
begin

  Result := g_file_mount_enclosing_volume_finish (PGFile(object_), res, @FileSource.MountError);

  if Result then
  begin
    DCDebug('(II) Mount successful.');
    FileSource.MountError:= nil;
  end
  else begin
    g_print ('(EE) Error mounting location: %s\n', [FileSource.MountError^.message]);
  end;

  FileSource.MountLoop.SetEvent;
end;

function TGioFileSource.MountPath(AFile: PGFile; out AError: PGError): Boolean;
var
  Operation: PGMountOperation;
begin
  Operation:= g_mount_operation_new();
  g_signal_connect_data(Operation, 'ask-password', TGCallback(@ask_password_cb), Self, nil, 0);
  g_signal_connect_data(Operation, 'ask-question', TGCallback(@ask_question_cb), Self, nil, 0);
  MountTry:= 0;
  MountError:= nil;
  MountLoop:= TSimpleEvent.Create;
  g_file_mount_enclosing_volume (AFile, G_MOUNT_MOUNT_NONE, Operation, nil, @mount_done_cb, Self);

  repeat
    if g_main_context_pending(g_main_context_default) then
      g_main_context_iteration(g_main_context_default, False);
  until MountLoop.WaitFor(1) <> wrTimeout;

  MountLoop.Free;
  g_object_unref (Operation);
  Result:= MountError = nil;
  AError:= MountError;
end;

constructor TGioFileSource.Create(const URI: TURI);
begin
  inherited Create(URI);
  FOperationsClasses[fsoMove] := TGioMoveOperation.GetOperationClass;
  FOperationsClasses[fsoCopy] := TGioCopyOperation.GetOperationClass;
  FOperationsClasses[fsoCopyIn] := TGioCopyInOperation.GetOperationClass;
  FOperationsClasses[fsoCopyOut] := TGioCopyOutOperation.GetOperationClass;
end;

class function TGioFileSource.IsSupportedPath(const Path: String): Boolean;
var
  GVfs: PGVfs;
  Schemes: PPgchar;
begin
  GVfs := g_vfs_get_default ();
  Schemes := g_vfs_get_supported_uri_schemes (GVfs);
  while Schemes^ <> nil do
  begin
    Result := (Pos(Schemes^ + '://', Path) = 1);
    if Result then Exit;
    Inc(Schemes);
  end;
end;

function TGioFileSource.CreateDirectory(const Path: String): Boolean;
var
  AGFile: PGFile;
begin
  AGFile:= g_file_new_for_commandline_arg(Pgchar(Path));
  Result:= g_file_make_directory_with_parents(AGFile, nil, nil);
  g_object_unref(PGObject(AGFile));
end;

function TGioFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize: Int64): Boolean;
var
  AFile: PGFile;
  AInfo: PGFileInfo;
begin
  AFile := g_file_new_for_commandline_arg(Pgchar(Path));
  AInfo := g_file_query_filesystem_info (AFile, FILE_ATTRIBUTE_FILESYSTEM_FREE + ',' + FILE_ATTRIBUTE_FILESYSTEM_SIZE, nil, nil);
  Result := Assigned(AInfo);
  if Result then
  begin
    FreeSize := g_file_info_get_attribute_uint64(AInfo, FILE_ATTRIBUTE_FILESYSTEM_FREE);
    TotalSize := g_file_info_get_attribute_uint64(AInfo, FILE_ATTRIBUTE_FILESYSTEM_SIZE);
    g_object_unref(AInfo);
  end;
  g_object_unref(PGObject(AFile));
end;

function TGioFileSource.GetParentDir(sPath: String): String;
begin
  Result:=inherited GetParentDir(sPath);
end;

function TGioFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result:=inherited IsPathAtRoot(Path);
end;

function TGioFileSource.GetRootDir(sPath: String): String;
begin
  Result:=inherited GetRootDir(sPath);
end;

function TGioFileSource.GetRootDir: String;
begin
  Result:=inherited GetRootDir;
end;

function TGioFileSource.GetProperties: TFileSourceProperties;
begin
  Result:=inherited GetProperties;
end;

function TGioFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TGioListOperation.Create(TargetFileSource, FCurrentAddress + TargetPath);
end;

function TGioFileSource.CreateCopyOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  SourceFiles.Path:= FCurrentAddress + SourceFiles.Path;
  Result:= TGioCopyOperation.Create(SourceFileSource, SourceFileSource, SourceFiles, FCurrentAddress + TargetPath);
end;

function TGioFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  if not StrBegins(TargetPath, FCurrentAddress) then TargetPath:= FCurrentAddress + TargetPath;
  Result:= TGioCopyInOperation.Create(SourceFileSource, TargetFileSource, SourceFiles, TargetPath);
end;

function TGioFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  if not StrBegins(SourceFiles.Path, FCurrentAddress) then SourceFiles.Path:= FCurrentAddress + SourceFiles.Path;
  Result := TGioCopyOutOperation.Create(SourceFileSource, TargetFileSource, SourceFiles, TargetPath);
end;

function TGioFileSource.CreateMoveOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  SourceFiles.Path:= FCurrentAddress + SourceFiles.Path;
  Result := TGioMoveOperation.Create(TargetFileSource, SourceFiles, FCurrentAddress + TargetPath);
end;

function TGioFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  FilesToDelete.Path:= FCurrentAddress + FilesToDelete.Path;
  Result := TGioDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TGioFileSource.CreateCreateDirectoryOperation(BasePath: String;
  DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TGioCreateDirectoryOperation.Create(TargetFileSource, FCurrentAddress + BasePath, DirectoryPath);
end;

function TGioFileSource.CreateExecuteOperation(var ExecutableFile: TFile;
  BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TGioExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TGioFileSource.CreateSetFilePropertyOperation(
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties
  ): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  theTargetFiles.Path:= FCurrentAddress + theTargetFiles.Path;
  Result := TGioSetFilePropertyOperation.Create(
                TargetFileSource,
                theTargetFiles,
                theNewProperties);
end;

end.


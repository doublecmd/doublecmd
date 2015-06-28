unit uGioFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, URIParser,
  uFileSourceProperty, uFileSourceOperationTypes,
  uRealFileSource, uFileSystemFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile, uGLib2, uGObject2, uGio2;

type
    IGioFileSource = interface(IRealFileSource)
      ['{6DC5BCCA-BDD5-43DA-A0D6-7BAA26D93B92}']
      function MountPath(AFile: PGFile): Boolean;
    end;

    { TGioFileSource }

    TGioFileSource = class(TRealFileSource, IGioFileSource)
    private
      MountTry: Integer;
      MountLoop: PGMainLoop;
      MountError: PGError;
    private

            function GetOperationsTypes: TFileSourceOperationTypes; override;

    protected
      function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    public
      function MountPath(AFile: PGFile): Boolean;

    public
      constructor Create(const URI: TURI); override;

      class function IsSupportedPath(const Path: String): Boolean; override;

      function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

                                         class function CreateFile(const APath: String): TFile; override;
                                         class function CreateFile(const APath: String; AFileInfo: PGFileInfo): TFile;

      function GetParentDir(sPath : String): String; override;
      function IsPathAtRoot(Path: String): Boolean; override;
      function GetRootDir(sPath: String): String; override; overload;
      function GetRootDir: String; override; overload;

      // Retrieve some properties of the file source.
      function GetProperties: TFileSourceProperties; override;

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
        uses uGioListOperation, uGioCopyOperation,  uDebug, DCDateTimeUtils, uShowMsg,
          uGioDeleteOperation, uGioExecuteOperation, uGioCreateDirectoryOperation,
          uGioMoveOperation, uGioSetFilePropertyOperation;

const
  MessageTitle = 'Double Commander';

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

class function TGioFileSource.CreateFile(const APath: String;
  AFileInfo: PGFileInfo): TFile;
begin
  Result:= CreateFile(APath);
  Result.Name:= g_file_info_get_name(AFileInfo);
  Result.Size:= g_file_info_get_size (AFileInfo);
  Result.Attributes:= g_file_info_get_attribute_uint32 (AFileInfo, FILE_ATTRIBUTE_UNIX_MODE);
  Result.ModificationTime:= UnixFileTimeToDateTime(g_file_info_get_attribute_uint64 (AFileInfo, FILE_ATTRIBUTE_TIME_MODIFIED));
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
  Domain: UTF8String;
  anonymous: Boolean;
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
      if (flags and G_ASK_PASSWORD_ANONYMOUS_SUPPORTED <> 0) then
      begin
        g_printf ('(WW) ask_password_cb: mount_try = %d, trying FTP anonymous login...\n', [FileSource.MountTry]);
        g_mount_operation_set_anonymous (op, TRUE);
        g_mount_operation_reply (op, G_MOUNT_OPERATION_HANDLED);
        Exit;
      end;
      if (mount_handled) then
      begin
        g_mount_operation_reply (op, G_MOUNT_OPERATION_HANDLED);
        Exit;
      end;
    end;

    //*  Ask user for password  */
   // g_print ('(WW) ask_password_cb: mount_try = %d, message = "%s"\n', [globs^.mount_try, message]);
    //end;

    //*  Handle abort message from certain backends properly  */
    //*   - e.g. SMB backends use this to mask multiple auth callbacks from smbclient  */
    if ((default_user <> nil) and (strcomp(default_user, 'ABORT') = 0)) then
    begin
      g_print ('(WW) default_user == "ABORT", aborting\n', []);
      g_mount_operation_reply (op, G_MOUNT_OPERATION_ABORTED);
      Exit;
    end;

    anonymous := FALSE;
    password_save := G_PASSWORD_SAVE_NEVER;

      //g_fprintf (stderr, '  (II) Spawning callback_ask_password (%p)...\n', [1]);

      Username:= default_user;
      Domain:= default_domain;

      if (flags and G_ASK_PASSWORD_NEED_USERNAME <> 0) then
      begin
        if ShowInputQuery(MessageTitle, message, False, Username) then
          g_mount_operation_set_username (op, Pgchar(Username))
        else begin
          g_mount_operation_reply (op, G_MOUNT_OPERATION_ABORTED);
          Exit;
        end;
      end;
      if (flags and G_ASK_PASSWORD_NEED_DOMAIN <> 0) then
      begin
        if ShowInputQuery(MessageTitle, message, False, Domain) then
          g_mount_operation_set_domain (op, Pgchar(Domain))
        else begin
          g_mount_operation_reply (op, G_MOUNT_OPERATION_ABORTED);
          Exit;
        end;
      end;
      if (flags and G_ASK_PASSWORD_NEED_PASSWORD <> 0) then
      begin
        if ShowInputQuery(MessageTitle, message, True, Password) then
          g_mount_operation_set_password (op, Pgchar(password))
        else begin
          g_mount_operation_reply (op, G_MOUNT_OPERATION_ABORTED);
          Exit;
        end;
      end;
      if (flags and G_ASK_PASSWORD_ANONYMOUS_SUPPORTED <> 0) then
        g_mount_operation_set_anonymous (op, anonymous);
      if (flags and G_ASK_PASSWORD_SAVING_SUPPORTED <> 0) then
        g_mount_operation_set_password_save (op, password_save);

      g_mount_operation_reply (op, G_MOUNT_OPERATION_HANDLED);
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

  g_main_loop_quit (FileSource.MountLoop);

end;

function TGioFileSource.MountPath(AFile: PGFile): Boolean;
var
  Operation: PGMountOperation;
begin
  Operation:= g_mount_operation_new();
  g_signal_connect_data (Operation, 'ask-password', TGCallback(@ask_password_cb), Self, nil, 0);
  //g_signal_connect (op, "ask-question", (GCallback)ask_question_cb, globs);
  MountTry:= 0;
  MountLoop := g_main_loop_new(nil, False);
  g_file_mount_enclosing_volume (AFile, G_MOUNT_MOUNT_NONE, Operation, nil, @mount_done_cb, Self);
  g_main_loop_run (MountLoop);
  g_main_loop_unref (MountLoop);
  g_object_unref (Operation);
  Result:= MountError = nil;
  if not Result then g_error_free(MountError);
end;

constructor TGioFileSource.Create(const URI: TURI);
begin
  inherited Create(URI);
  FOperationsClasses[fsoMove] := TGioMoveOperation.GetOperationClass;
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
  Result:= TGioCopyInOperation.Create(SourceFileSource, TargetFileSource, SourceFiles, FCurrentAddress + TargetPath);
end;

function TGioFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  SourceFiles.Path:= FCurrentAddress + SourceFiles.Path;
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


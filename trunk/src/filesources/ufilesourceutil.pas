unit uFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileView, uFile, uFileSourceOperationTypes;

{en
   Decides what should be done when user chooses a file in a file view.
   This function may add/remove a file source from the view,
   change path, execute a file or a command, etc.
}
procedure ChooseFile(aFileView: TFileView; aFile: TFile);

{en
   Checks if choosing the given file will change to another file source,
   and adds this new file source to the view if it does.
   @returns @true if the file matched any rules and a new file source was created,
            @false otherwise, which means no action was taken.
}
function ChooseFileSource(aFileView: TFileView; aFile: TFile): Boolean; overload;

function ChooseFileSource(aFileView: TFileView; const aPath: UTF8String): Boolean; overload;

function ChooseArchive(aFileView: TFileView; aFile: TFile; bForce: Boolean = False): Boolean;

procedure ChooseSymbolicLink(aFileView: TFileView; aFile: TFile);

procedure SetFileSystemPath(aFileView: TFileView; aPath: UTF8String);

function RenameFile(aFileSource: IFileSource; const aFile: TFile;
                    const NewFileName: UTF8String; Interactive: Boolean): Boolean;

function GetCopyOperationType(SourceFileSource, TargetFileSource: IFileSource;
                              out OperationType: TFileSourceOperationType): Boolean;

implementation

uses
  LCLProc, fFileExecuteYourSelf, uGlobs, uShellExecute, uFindEx, uDebug,
  uOSUtils, uShowMsg, uLng, uVfsModule, DCOSUtils, DCStrUtils,
  uFileSourceOperation,
  uFileSourceSetFilePropertyOperation,
  uFileSourceExecuteOperation,
  uVfsFileSource,
  uFileSourceProperty,
  uFileSystemFileSource,
  uWfxPluginFileSource,
  uArchiveFileSourceUtil,
  uFileSourceOperationMessageBoxesUI,
  uFileProperty, URIParser;

procedure ChooseFile(aFileView: TFileView; aFile: TFile);
var
  sOpenCmd: String;
  Operation: TFileSourceExecuteOperation = nil;
  aFileCopy: TFile = nil;
begin
  // First test for file sources.
  if ChooseFileSource(aFileView, aFile) then
    Exit;

  // For now work only for local files.
  if aFileView.FileSource.Properties * [fspDirectAccess, fspLinksToLocalFiles] <> [] then
  begin
    if fspLinksToLocalFiles in aFileView.FileSource.Properties then
      aFileView.FileSource.GetLocalName(aFile);
    // Now test if exists Open command in doublecmd.ext :)
    sOpenCmd := gExts.GetExtActionCmd(aFile, 'open');
    if (sOpenCmd <> '') then
    begin
      sOpenCmd := PrepareParameter(sOpenCmd, aFile);
      if ProcessExtCommand(sOpenCmd, aFileView.CurrentPath) then
        Exit;
    end;
  end;

  if (fsoExecute in aFileView.FileSource.GetOperationsTypes) then
    try
      aFileCopy := aFile.Clone;
      Operation := aFileView.FileSource.CreateExecuteOperation(
                        aFileCopy,
                        aFileView.CurrentPath,
                        'open') as TFileSourceExecuteOperation;

      if Assigned(Operation) then
        begin
          Operation.Execute;
          case Operation.ExecuteOperationResult of
          fseorError:
            begin
              // Show error message
              if Length(Operation.ResultString) = 0 then
                msgError(rsMsgErrEOpen)
              else
                msgError(Operation.ResultString);
            end;
          fseorYourSelf:
            begin
              // Copy out file to temp file system and execute
              if not ShowFileExecuteYourSelf(aFileView, aFile, False) then
                DCDebug('Execution error!');
            end;
          fseorWithAll:
            begin
              // Copy out all files to temp file system and execute chosen
              if not ShowFileExecuteYourSelf(aFileView, aFile, True) then
                DCDebug('Execution error!');
            end;
          fseorSymLink:
            begin
              // change directory to new path (returned in Operation.ResultString)
              DCDebug('Change directory to ', Operation.ResultString);
              with aFileView do
              begin
                if (FileSource.IsClass(TFileSystemFileSource)) or
                   (mbSetCurrentDir(ExcludeTrailingPathDelimiter(Operation.ResultString)) = False) then
                  begin
                    // Simply change path
                    CurrentPath:= Operation.ResultString;
                  end
                else
                  begin
                    // Get a new filesystem file source
                    AddFileSource(TFileSystemFileSource.GetFileSource, Operation.ResultString);
                  end;
              end;
            end;
          end; // case
        end; // assigned
    finally
      FreeAndNil(aFileCopy);
      FreeAndNil(Operation);
    end;
end;

function ChooseFileSource(aFileView: TFileView; aFile: TFile): Boolean;
var
  FileSource: IFileSource;
  VfsModule: TVfsModule;
begin
  Result := False;

  if ChooseArchive(aFileView, aFile) then
    Exit(True);

  // Work only for TVfsFileSource.
  if aFileView.FileSource.IsClass(TVfsFileSource) then
  begin
    // Check if there is a registered WFX plugin by file system root name.
    FileSource := FileSourceManager.Find(TWfxPluginFileSource, 'wfx://' + aFile.Name);
    if not Assigned(FileSource) then
      FileSource := TWfxPluginFileSource.CreateByRootName(aFile.Name);

    if not Assigned(FileSource) then
    begin
      // Check if there is a registered Vfs module by file system root name.
      VfsModule:= gVfsModuleList.VfsModule[aFile.Name];
      if Assigned(VfsModule) then
      begin
        FileSource := FileSourceManager.Find(VfsModule.FileSourceClass, aFile.Name);
        if not Assigned(FileSource) then
          FileSource := VfsModule.FileSourceClass.Create;
      end;
    end;

    if Assigned(FileSource) then
    begin
      aFileView.AddFileSource(FileSource, FileSource.GetRootDir);
      Exit(True);
    end;
  end;
end;

function ChooseFileSource(aFileView: TFileView; const aPath: UTF8String): Boolean;
var
  URI: TURI;
  RemotePath: UTF8String;
  aFileSourceClass: TFileSourceClass;
begin
  Result:= True;
  aFileSourceClass:= gVfsModuleList.GetFileSource(aPath);
  // If found special FileSource for path
  if Assigned(aFileSourceClass) then
    begin
      // If path is URI
      if Pos('://', aPath) > 0 then
        begin
          URI:= ParseURI(aPath);
          RemotePath:= NormalizePathDelimiters(URI.Path + URI.Document);
          RemotePath:= IncludeTrailingPathDelimiter(RemotePath);
          // Create new FileSource with given URI
          aFileView.AddFileSource(aFileSourceClass.Create(URI), RemotePath);
        end
      // If found FileSource is same as current then simply change path
      else if aFileSourceClass.ClassNameIs(aFileView.FileSource.ClassName) then
        aFileView.CurrentPath := aPath
      // Else create new FileSource with given path
      else
        aFileView.AddFileSource(aFileSourceClass.Create, aPath);
    end
  // If current FileSource has address
  else if Length(aFileView.CurrentAddress) > 0 then
     aFileView.CurrentPath := aPath
  // Else use FileSystemFileSource
  else
    begin
      SetFileSystemPath(aFileView, aPath);
      Result:= mbSetCurrentDir(aPath);
    end;
end;

function ChooseArchive(aFileView: TFileView; aFile: TFile; bForce: Boolean): Boolean;
var
  FileSource: IFileSource;
begin
  // Check if there is a ArchiveFileSource for possible archive.
  FileSource := GetArchiveFileSource(aFileView.FileSource, aFile, EmptyStr, bForce);

  if Assigned(FileSource) then
  begin
    if not mbCompareFileNames(aFileView.CurrentPath, aFile.Path) then
    begin
      SetFileSystemPath(aFileView, aFile.Path);
    end;
    aFileView.AddFileSource(FileSource, FileSource.GetRootDir);
    Exit(True);
  end;

  Result := False;
end;

procedure ChooseSymbolicLink(aFileView: TFileView; aFile: TFile);
var
  SearchRec: TSearchRecEx;
  sPath: UTF8String;
begin
  if not aFileView.FileSource.IsClass(TFileSystemFileSource) then
  begin
    aFileView.ChangePathToChild(aFile);
    Exit;
  end;

  sPath:= aFileView.CurrentPath + IncludeTrailingPathDelimiter(aFile.Name);
  try
    if FindFirstEx(sPath + AllFilesMask, faAnyFile, SearchRec) = 0 then
      begin
        with aFileView do
        CurrentPath := CurrentPath + IncludeTrailingPathDelimiter(aFile.Name);
      end
    else
      begin
        sPath:= ReadSymLink(aFile.FullPath);
        if sPath <> EmptyStr then
          aFileView.CurrentPath := IncludeTrailingPathDelimiter(GetAbsoluteFileName(aFileView.CurrentPath, sPath))
        else
          msgError(Format(rsMsgChDirFailed, [aFile.FullPath]));
      end;
  finally
    FindCloseEx(SearchRec);
  end;
end;

procedure SetFileSystemPath(aFileView: TFileView; aPath: UTF8String);
var
  i: Integer;
begin
  // Search for filesystem file source in this view, and remove others.
  with aFileView do
  begin
    for i := FileSourcesCount - 1 downto 0 do
    begin
      // Search FileSource with same class name, we can not use "is"
      // operator because it also works for descendant classes
      if TFileSystemFileSource.ClassNameIs(FileSources[i].ClassName) then
      begin
        CurrentPath := aPath;
        Break;
      end
      else
        RemoveCurrentFileSource;
    end;

    if FileSourcesCount = 0 then
    begin
      // If not found, get a new filesystem file source.
      AddFileSource(TFileSystemFileSource.GetFileSource, aPath);
    end;
  end;
end;

function RenameFile(aFileSource: IFileSource; const aFile: TFile;
                    const NewFileName: UTF8String; Interactive: Boolean): Boolean;
var
  aFiles: TFiles = nil;
  Operation: TFileSourceSetFilePropertyOperation = nil;
  NewProperties: TFileProperties;
  UserInterface: TFileSourceOperationMessageBoxesUI = nil;
begin
  Result:= False;

  if fsoSetFileProperty in aFileSource.GetOperationsTypes then
  begin
    FillByte(NewProperties, SizeOf(NewProperties), 0);
    NewProperties[fpName] := TFileNameProperty.Create(NewFileName);
    try
      aFiles := TFiles.Create(aFile.Path);
      aFiles.Add(aFile.Clone);

      Operation := aFileSource.CreateSetFilePropertyOperation(
                     aFiles,
                     NewProperties) as TFileSourceSetFilePropertyOperation;

      if Assigned(Operation) then
      begin
        // Only if the operation can change file name.
        if fpName in Operation.SupportedProperties then
        begin
          Operation.SkipErrors := not Interactive;

          if Interactive then
          begin
            UserInterface := TFileSourceOperationMessageBoxesUI.Create;
            Operation.AddUserInterface(UserInterface);
          end;

          Operation.Execute;
          Result := (Operation.Result = fsorFinished);
        end;
      end;

    finally
      FreeThenNil(NewProperties[fpName]);
      FreeThenNil(Operation);
      FreeThenNil(UserInterface);
      FreeThenNil(aFiles);
    end;
  end;
end;

function GetCopyOperationType(SourceFileSource, TargetFileSource: IFileSource;
  out OperationType: TFileSourceOperationType): Boolean;
begin
  // If same file source and address
  if (fsoCopy in SourceFileSource.GetOperationsTypes) and
     (fsoCopy in TargetFileSource.GetOperationsTypes) and
     SourceFileSource.Equals(TargetFileSource) and
     SameText(SourceFileSource.GetCurrentAddress, TargetFileSource.GetCurrentAddress) then
  begin
    Result:= True;
    OperationType := fsoCopy;
  end
  else if TargetFileSource.IsClass(TFileSystemFileSource) and
          (fsoCopyOut in SourceFileSource.GetOperationsTypes) then
  begin
    Result:= True;
    OperationType := fsoCopyOut;
  end
  else if SourceFileSource.IsClass(TFileSystemFileSource) and
          (fsoCopyIn in TargetFileSource.GetOperationsTypes) then
  begin
    Result:= True;
    OperationType := fsoCopyIn;
  end
  else
  begin
    Result:= False;
  end;
end;

end.

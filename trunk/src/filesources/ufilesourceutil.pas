unit uFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileView, uFile, uFileSourceOperationTypes,
  uFileSourceSetFilePropertyOperation;

{en
   Decides what should be done when user chooses a file in a file view.
   This function may add/remove a file source from the view,
   change path, execute a file or a command, etc.
}
procedure ChooseFile(aFileView: TFileView; aFileSource: IFileSource; aFile: TFile);

{en
   Checks if choosing the given file will change to another file source,
   and adds this new file source to the view if it does.
   @returns @true if the file matched any rules and a new file source was created,
            @false otherwise, which means no action was taken.
}
function ChooseFileSource(aFileView: TFileView; aFileSource: IFileSource; aFile: TFile): Boolean; overload;

function ParseFileSource(var aPath: String; const CurrentFileSource: IFileSource = nil): IFileSource;

function ChooseFileSource(aFileView: TFileView; const aPath: String; bLocal: Boolean = False): Boolean; overload;

function ChooseArchive(aFileView: TFileView; aFileSource: IFileSource; aFile: TFile; bForce: Boolean = False): Boolean;

procedure ChooseSymbolicLink(aFileView: TFileView; aFile: TFile);

procedure SetFileSystemPath(aFileView: TFileView; aPath: String);

function RenameFile(aFileSource: IFileSource; const aFile: TFile;
                    const NewFileName: String; Interactive: Boolean): TSetFilePropertyResult;

function GetCopyOperationType(SourceFileSource, TargetFileSource: IFileSource;
                              out OperationType: TFileSourceOperationType): Boolean;

implementation

uses
  LCLProc, fFileExecuteYourSelf, uGlobs, uShellExecute, uFindEx, uDebug,
  uOSUtils, uShowMsg, uLng, uVfsModule, DCOSUtils, DCStrUtils,
  uFileSourceOperation,
  uFileSourceExecuteOperation,
  uVfsFileSource,
  uFileSourceProperty,
  uFileSystemFileSource,
  uWfxPluginFileSource,
  uArchiveFileSourceUtil,
  uFileSourceOperationMessageBoxesUI,
  uFileProperty, URIParser;

procedure ChooseFile(aFileView: TFileView; aFileSource: IFileSource;
  aFile: TFile);
var
  sCmd, sParams, sStartPath: String;
  Operation: TFileSourceExecuteOperation = nil;
  aFileCopy: TFile = nil;
begin
  // First test for file sources.
  if ChooseFileSource(aFileView, aFileSource, aFile) then
    Exit;

  // For now work only for local files.
  if aFileView.FileSource.Properties * [fspDirectAccess, fspLinksToLocalFiles] <> [] then
  begin
    // Now test if exists Open command in "extassoc.xml" :)
    if gExts.GetExtActionCmd(aFile, 'open', sCmd, sParams, sStartPath) then
    begin
      try
        // Resolve filename here since ProcessExtCommandFork doesn't do it (as of 2017)
        // The limitation is that only one file will be opened on a FileSource of links
        if fspLinksToLocalFiles in aFileView.FileSource.Properties then
        begin
          aFileCopy := aFile.Clone;
          aFileView.FileSource.GetLocalName(aFileCopy);
        end;

        if ProcessExtCommandFork(sCmd,sParams,sStartPath,aFileCopy) then
          Exit;
      finally
        FreeAndNil(aFileCopy);
      end;
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
                // If path is URI
                if Pos('://', Operation.ResultString) > 0 then
                  ChooseFileSource(aFileView, Operation.ResultString)
                else if (FileSource.IsClass(TFileSystemFileSource)) or
                        (mbSetCurrentDir(ExcludeTrailingPathDelimiter(Operation.ResultString)) = False) then
                begin
                  // Simply change path
                  CurrentPath:= Operation.ResultString;
                end
                else begin
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

function ChooseFileSource(aFileView: TFileView; aFileSource: IFileSource;
  aFile: TFile): Boolean;
var
  FileSource: IFileSource;
  VfsModule: TVfsModule;
begin
  Result := False;

  if ChooseArchive(aFileView, aFileSource, aFile) then
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

function ParseFileSource(var aPath: String; const CurrentFileSource: IFileSource = nil): IFileSource;
var
  URI: TURI;
  aFileSourceClass: TFileSourceClass;
begin
  aFileSourceClass:= gVfsModuleList.GetFileSource(aPath);
  // If found special FileSource for path
  if Assigned(aFileSourceClass) then
    begin
      // If path is URI
      if Pos('://', aPath) > 0 then
        begin
          URI:= ParseURI(aPath);
          aPath:= NormalizePathDelimiters(URI.Path + URI.Document);
          aPath:= IncludeTrailingPathDelimiter(aPath);
          Result:= FileSourceManager.Find(aFileSourceClass,
                                              URI.Protocol + '://' + URI.Host,
                                              not SameText(URI.Protocol, 'smb')
                                              );
          if not Assigned(Result) then
          try
            // Create new FileSource with given URI
            Result := aFileSourceClass.Create(URI);
          except
            Result := nil;
          end;
        end
      // If found FileSource is same as current then simply change path
      else if aFileSourceClass.ClassNameIs(CurrentFileSource.ClassName) then
        Result := CurrentFileSource
      // Else create new FileSource with given path
      else
        Result := aFileSourceClass.Create;
    end
  else
    Result:= nil;
end;

function ChooseFileSource(aFileView: TFileView; const aPath: String;
  bLocal: Boolean): Boolean;
var
  RemotePath: String;
  FileSource: IFileSource;
begin
  Result:= True;
  RemotePath:= aPath;
  FileSource:= ParseFileSource(RemotePath, aFileView.FileSource);
  // If found special FileSource for path
  if Assigned(FileSource) then
    begin
      // If path is URI
      if RemotePath <> aPath then
        aFileView.AddFileSource(FileSource, RemotePath)
      // If found FileSource is same as current then simply change path
      else if aFileView.FileSource.Equals(FileSource) then
        aFileView.CurrentPath := aPath
      // Else create new FileSource with given path
      else
        aFileView.AddFileSource(FileSource, aPath);
    end
  // If current FileSource has address
  else if bLocal and (Length(aFileView.CurrentAddress) > 0) then
     aFileView.CurrentPath := aPath
  // Else use FileSystemFileSource
  else
    begin
      SetFileSystemPath(aFileView, aPath);
      Result:= mbSetCurrentDir(aPath);
    end;
end;

function ChooseArchive(aFileView: TFileView; aFileSource: IFileSource;
  aFile: TFile; bForce: Boolean): Boolean;
var
  FileSource: IFileSource;
begin
  try
    // Check if there is a ArchiveFileSource for possible archive.
    FileSource := GetArchiveFileSource(aFileSource, aFile, EmptyStr, bForce, False);
  except
    on E: Exception do
    begin
      if not bForce then
      begin
        msgError(E.Message + LineEnding + aFile.FullPath);
        Exit(True);
      end;
    end;
  end;

  if Assigned(FileSource) then
  begin
    if not mbCompareFileNames(aFileView.CurrentPath, aFile.Path) then
    begin
      if aFileSource.Properties * [fspDirectAccess, fspLinksToLocalFiles] <> [] then
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
  sPath: String;
begin
  if not aFileView.FileSource.IsClass(TFileSystemFileSource) then
  begin
    aFileView.ChangePathToChild(aFile);
    Exit;
  end;

  sPath:= aFileView.CurrentPath + IncludeTrailingPathDelimiter(aFile.Name);
  try
    if FindFirstEx(sPath + AllFilesMask, 0, SearchRec) = 0 then
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

procedure SetFileSystemPath(aFileView: TFileView; aPath: String);
begin
  with aFileView do
  begin
    if TFileSystemFileSource.ClassNameIs(FileSource.ClassName) then
      CurrentPath := aPath
    else
      AddFileSource(TFileSystemFileSource.GetFileSource, aPath);
  end;
end;

function RenameFile(aFileSource: IFileSource; const aFile: TFile;
  const NewFileName: String; Interactive: Boolean): TSetFilePropertyResult;
var
  aFiles: TFiles = nil;
  Operation: TFileSourceSetFilePropertyOperation = nil;
  NewProperties: TFileProperties;
  UserInterface: TFileSourceOperationMessageBoxesUI = nil;
begin
  Result:= sfprError;

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
          case Operation.Result of
            fsorFinished: Result:= sfprSuccess;
            fsorAborted: Result:= sfprSkipped;
          end;
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

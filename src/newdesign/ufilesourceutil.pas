unit uFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileView, uFile;

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
function ChooseFileSource(aFileView: TFileView; aFile: TFile): Boolean;

function ChooseArchive(aFileView: TFileView; aFile: TFile; bForce: Boolean = False): Boolean;

procedure ChooseSymbolicLink(aFileView: TFileView; aFile: TFile);

function RenameFile(aFileSource: IFileSource; const aFile: TFile;
                    const NewFileName: UTF8String; Interactive: Boolean): Boolean;

implementation

uses
  LCLProc, fFileExecuteYourSelf, uGlobs, uShellExecute, uFindEx, uDebug,
  uOSUtils, uShowMsg, uTypes, uLng, uDCUtils, uVfsModule,
  uFileSourceOperation,
  uFileSourceSetFilePropertyOperation,
  uFileSourceExecuteOperation,
  uVfsFileSource,
  uFileSystemFileSource,
  uWfxPluginFileSource,
  uArchiveFileSourceUtil,
  uFileSourceOperationTypes,
  uFileSourceOperationMessageBoxesUI,
  uFileProperty;

procedure ChooseFile(aFileView: TFileView; aFile: TFile);
var
  sOpenCmd: String;
  Operation: TFileSourceExecuteOperation = nil;
  aFileCopy: TFile = nil;
begin
  // First test for file sources.
  if ChooseFileSource(aFileView, aFile) then
    Exit;

  // For now work only for FileSystem until temporary file system is done.
  if aFileView.FileSource.IsClass(TFileSystemFileSource) then
  begin
    //now test if exists Open command in doublecmd.ext :)
    sOpenCmd:= gExts.GetExtActionCmd(aFile, 'open');
    if (sOpenCmd<>'') then
    begin
(*
      if Pos('{!VFS}',sOpenCmd)>0 then
      begin
        if fVFS.FindModule(sName) then
        begin
          LoadPanelVFS(pfri);
          Exit;
        end;
      end;
*)
      ReplaceExtCommand(sOpenCmd, aFile, aFileView.CurrentPath);
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
            // Show error message
            msgError(Operation.ResultString);
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
          aFileView.Reload;
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

function ChooseArchive(aFileView: TFileView; aFile: TFile; bForce: Boolean): Boolean;
var
  FileSource: IFileSource;
begin
  // Check if there is a ArchiveFileSource for possible archive.
  FileSource := GetArchiveFileSource(aFileView.FileSource, aFile, EmptyStr, bForce);

  if Assigned(FileSource) then
  begin
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

end.


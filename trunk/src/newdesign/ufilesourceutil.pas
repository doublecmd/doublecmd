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

function RenameFile(aFileSource: IFileSource; const aFile: TFile; const NewFileName: UTF8String): Boolean;

implementation

uses
  LCLProc, uGlobs, uShellExecute,
  uFileSourceOperation,
  uFileSourceSetFilePropertyOperation,
  uFileSourceExecuteOperation,
  uVfsFileSource,
  uFileSystemFileSource,
  uWcxArchiveFileSource,
  uWfxPluginFileSource,
  uFileSourceOperationTypes,
  uFileProperty;

procedure ChooseFile(aFileView: TFileView; aFile: TFile);
var
  sOpenCmd: String;
  Operation: TFileSourceExecuteOperation;
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
{
      if Pos('{!VFS}',sOpenCmd)>0 then
      begin
        if fVFS.FindModule(sName) then
        begin
          LoadPanelVFS(pfri);
          Exit;
        end;
      end;
}
      ReplaceExtCommand(sOpenCmd, aFile, aFileView.CurrentPath);
      if ProcessExtCommand(sOpenCmd, aFileView.CurrentPath) then
        Exit;
    end;
  end;

  if (fsoExecute in aFileView.FileSource.GetOperationsTypes) then
    begin
      Operation := aFileView.FileSource.CreateExecuteOperation(
                       aFileView.CurrentPath,
                       aFile.Name, 'open') as TFileSourceExecuteOperation;

      if Assigned(Operation) then
        try
          Operation.Execute;
          case Operation.ExecuteOperationResult of
          fseorError:
            // Show error message
            DebugLn('Execution error!');
          fseorYourSelf:
            begin
              // CopyOut file to temp file system and execute
            end;
          fseorSymLink:
            begin
              // change directory to new path (returned in Operation.ExecutablePath)
              DebugLn('Change directory to ', Operation.ExecutablePath);
              aFileView.CurrentPath:= Operation.ExecutablePath;
            end;
          end;
        finally
          FreeAndNil(Operation);
          aFileView.Reload;
        end;
    end;
end;

function ChooseFileSource(aFileView: TFileView; aFile: TFile): Boolean;
var
  FileSource: IFileSource;
begin
  Result := False;

  // Opening archives directly only from FileSystem.
  if aFileView.FileSource.IsClass(TFileSystemFileSource) then
  begin
    // Check if there is a registered WCX plugin for possible archive.
    FileSource := FileSourceManager.Find(TWcxArchiveFileSource, aFile.Path + aFile.Name);
    if not Assigned(FileSource) then
      FileSource := TWcxArchiveFileSource.CreateByArchiveName(aFile.Path + aFile.Name);

    if Assigned(FileSource) then
    begin
      aFileView.AddFileSource(FileSource, FileSource.GetRootDir);
      Exit(True);
    end;
  end;

  // Work only for TVfsFileSource.
  if aFileView.FileSource.IsClass(TVfsFileSource) then
  begin
    // Check if there is a registered WFX plugin by file system root name.
    FileSource := FileSourceManager.Find(TWfxPluginFileSource, aFile.Path + aFile.Name);
    if not Assigned(FileSource) then
      FileSource := TWfxPluginFileSource.CreateByRootName(aFile.Name);

    if Assigned(FileSource) then
    begin
      aFileView.AddFileSource(FileSource, FileSource.GetRootDir);
      Exit(True);
    end;
  end;
end;

function RenameFile(aFileSource: IFileSource; const aFile: TFile; const NewFileName: UTF8String): Boolean;
var
  aFiles: TFiles;
  Operation: TFileSourceSetFilePropertyOperation = nil;
  NewProperties: TFileProperties;
begin
  Result:= False;
  aFiles := aFileSource.CreateFiles;
  aFiles.Add(aFile.Clone);

  if fsoSetFileProperty in aFileSource.GetOperationsTypes then
  begin
    FillByte(NewProperties, SizeOf(NewProperties), 0);
    NewProperties[fpName] := TFileNameProperty.Create(NewFileName);
    try
      Operation := aFileSource.CreateSetFilePropertyOperation(
                     aFiles,
                     NewProperties) as TFileSourceSetFilePropertyOperation;

      if Assigned(Operation) then
      begin
        // Only if the operation can change file name.
        if fpName in Operation.SupportedProperties then
        begin
          Operation.Execute;
          Result := (Operation.Result = fsorFinished);
        end;
      end;

    finally
      FreeThenNil(NewProperties[fpName]);
      FreeThenNil(Operation);
    end;
  end;

  FreeThenNil(aFiles);
end;

end.


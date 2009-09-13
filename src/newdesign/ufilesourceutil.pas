unit uFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileView, uFile;

procedure ChooseFile(aFileView: TFileView; aFile: TFile);
function RenameFile(aFileSource: TFileSource; aFile: TFile; NewFileName: UTF8String): Boolean;

implementation

uses
  uFileSystemFileSource, uGlobs, uShellExecute, uOSUtils,
  uFileSourceOperation, uFileSourceMoveOperation, uWcxArchiveFileSource, LCLProc;

procedure ChooseFile(aFileView: TFileView; aFile: TFile);
var
  sOpenCmd: String;
  FileSource: TFileSource;
begin
  // For now work only for FileSystem until temporary file system is done.
  if aFileView.FileSource is TFileSystemFileSource then
  begin
    // Check if it is registered plugin (for archives).
    FileSource := TWcxArchiveFileSource.CreateByArchiveName(aFile.Path + aFile.Name);
    if Assigned(FileSource) then
    begin
      aFileView.AddFileSource(FileSource);
      Exit;
    end;

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
      ReplaceExtCommand(sOpenCmd, aFile, aFileView.FileSource.CurrentPath);
      if ProcessExtCommand(sOpenCmd, aFileView.FileSource.CurrentPath) then
        Exit;
    end;

    // and at the end try to open by system
    mbSetCurrentDir(aFileView.FileSource.CurrentPath);
    ShellExecute(aFile.Name);
    aFileView.Reload;
  end;
end;

function RenameFile(aFileSource: TFileSource; aFile: TFile; NewFileName: UTF8String): Boolean;
var
  aFiles: TFiles;
  sDestPath: UTF8String;
  Operation: TFileSourceMoveOperation;
begin
  Result:= False;
  with aFileSource.GetFiles do
  begin
    aFiles:= CreateObjectOfSameType;
    Free;
  end;
  aFiles.Add(aFile);
  sDestPath:= ExtractFilePath(NewFileName);
  Operation := aFileSource.CreateMoveOperation(
                         aFiles, sDestPath) as TFileSourceMoveOperation;
  if Assigned(Operation) then
    try
      Operation.RenameMask := ExtractFileName(NewFileName);
      Operation.Execute;
      Result:= True;
    finally
      FreeAndNil(Operation);
    end;
  FreeThenNil(aFiles);
end;

end.


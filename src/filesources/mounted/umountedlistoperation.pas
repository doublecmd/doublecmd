unit uMountedListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource, uMountedFileSource, uFileSystemListOperation,
  uDCUtils, DCStrUtils;

type

  TMountedListOperation = class(TFileSourceListOperation)
  private
    FFileSource: IMountedFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uFile;

constructor TMountedListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  inherited Create(aFileSource, aPath);
  FFiles := TFiles.Create(aPath);
  FFileSource := aFileSource as IMountedFileSource;
  FNeedsConnection := False;
end;

{
  self.Path
    ///iCloud Drivers/Pages Documents
  logicPath
    /Pages Documents
  realPath
    ~/Library/Mobile Documents/com~apple~Pages/Documents
}
procedure TMountedListOperation.MainExecute;
var
  mountedFS: TMountedFileSource;
  logicPath: String;
  realPath: String;

  function getFilesByFileSystemListOperation: TFiles;
  var
    listOperation: TFileSourceListOperation;
  begin
    listOperation:= TFileSystemListOperation.Create( mountedFS, realPath );
    try
      listOperation.Execute;
      Result:= listOperation.ReleaseFiles;
    finally
      FreeAndNil( listOperation );
    end;
  end;

  procedure addMountedPaths;
  var
    mountPoint: TMountPoint;
    mountPointParentPath: String;
    mountPointName: String;
    mountedPath: String;
    mountedTFile: TFile;
  begin
    for mountPoint in mountedFS.mountPoints do begin
      mountPointParentPath:= GetParentDir( mountPoint.point );
      mountedPath:= mountPoint.path;
      if logicPath <> mountPointParentPath then
        continue;

      mountedTFile:= mountedFS.CreateFileFromFile( mountedPath );
      mountPointName:= mountedFS.getDefaultPointForPath( mountedPath );
      if NOT mountPointName.IsEmpty then
        mountedTFile.Name:= mountPointName;
      FFiles.Add( mountedTFile );
    end;
  end;

  procedure addRegularFiles;
  begin
    FFiles.List.AddList( getFilesByFileSystemListOperation.List );
  end;

begin
  mountedFS:= FFileSource as TMountedFileSource;
  logicPath:= self.Path.Substring( FFileSource.GetRootDir.Length - 1 );
  realPath:= mountedFS.getRealPath( self.Path );

  FFiles.Clear;
  addMountedPaths;
  addRegularFiles;
end;

end.

unit uMountedListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource, uMountedFileSource, uFileSystemFileSource,
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
    ///iCloud云盘/Pages Documents
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

  function getRealPath: String;
  var
    mountPoint: TMountPoint;
  begin
    for mountPoint in mountedFS.mountPoints do begin
      if logicPath.StartsWith(mountPoint.point) then begin
        Result:= mountPoint.path + logicPath.Substring(mountPoint.point.Length);
        Exit;
      end;
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

      mountedTFile:= TFileSystemFileSource.CreateFileFromFile( mountedPath );
      mountPointName:= mountedFS.getDefaultPointForPath( mountedPath );
      if NOT mountPointName.IsEmpty then
        mountedTFile.Name:= mountPointName;
      FFiles.Add( mountedTFile );
    end;
  end;

  procedure addRegularFiles;
  var
    realFS: IFileSource;
  begin
    realFS:= TFileSystemFileSource.Create;
    FFiles.List.AddList( realFS.GetFiles(realPath).List );
  end;

begin
  FFiles.Clear;
  mountedFS:= FFileSource as TMountedFileSource;
  logicPath:= self.Path.Substring( FileSource.GetRootDir.Length - 1 );
  realPath:= getRealPath;

  addMountedPaths;
  addRegularFiles;
end;

end.

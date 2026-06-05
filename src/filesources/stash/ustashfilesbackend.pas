unit uStashFilesBackend;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uFileSystemFileSource;

type

  { TStashFilesBackend }

  TStashFilesBackend = class
  private
    _paths: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure addPath( const path: String );
    procedure removePath( const path: String );
    procedure clear;
    function count: Integer;
    procedure addPaths( const files: TFiles );
    procedure removePaths( const files: TFiles );
    function toFiles: TFiles;
  end;

var
  stashFilesBackend: TStashFilesBackend;

implementation

{ TStashFilesBackend }

constructor TStashFilesBackend.Create;
begin
  _paths:= TStringList.Create;
  _paths.SortStyle:= sslAuto;
  _paths.Duplicates:= dupIgnore;
end;

destructor TStashFilesBackend.Destroy;
begin
  FreeAndNIl( _paths );
end;

procedure TStashFilesBackend.addPath(const path: String);
begin
  _paths.Add( path );
end;

procedure TStashFilesBackend.removePath(const path: String);
var
  i: Integer;
begin
  _paths.Find( path, i );
  if i >= 0 then
    _paths.Delete( i );
end;

procedure TStashFilesBackend.clear;
begin
  _paths.Clear;
end;

function TStashFilesBackend.count: Integer;
begin
  Result:= _paths.Count;
end;

procedure TStashFilesBackend.addPaths(const files: TFiles);
var
  i: Integer;
begin
  for i:= 0 to files.Count-1 do
    self.addPath( files[i].FullPath );
end;

procedure TStashFilesBackend.removePaths(const files: TFiles);
var
  i: Integer;
begin
  for i:= 0 to files.Count-1 do
    self.removePath( files[i].FullPath );
end;

function TStashFilesBackend.toFiles: TFiles;
var
  files: TFiles;
  path: String;
  f: TFile;
  i: Integer;
begin
  files:= TFiles.Create( EmptyStr );
  i:= 0;
  while i < _paths.Count do begin
    path:= _paths[i];
    try
      f:= TFileSystemFileSource.CreateFileFromFile( path );
      files.Add( f );
      inc( i );
    except
      _paths.Delete( i );
    end;
  end;
  Result:= files;
end;

initialization
  stashFilesBackend:= TStashFilesBackend.Create;

finalization
  FreeAndNil( stashFilesBackend );

end.


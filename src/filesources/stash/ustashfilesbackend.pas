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
    function toFiles: TFiles;
  end;

var
  stashFilesBackend: TStashFilesBackend;

implementation

{ TStashFilesBackend }

constructor TStashFilesBackend.Create;
begin
  _paths:= TStringList.Create;
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
  i:= _paths.IndexOf( path );
  if i >= 0 then
    _paths.Delete( i );
end;

procedure TStashFilesBackend.clear;
begin
  _paths.Clear;
end;

function TStashFilesBackend.toFiles: TFiles;
var
  files: TFiles;
  path: String;
  f: TFile;
begin
  files:= TFiles.Create( EmptyStr );
  for path in _paths do begin
    f:= TFileSystemFileSource.CreateFileFromFile( path );
    files.Add( f );
  end;
  Result:= files;
end;

initialization
  stashFilesBackend:= TStashFilesBackend.Create;

finalization
  FreeAndNil( stashFilesBackend );

end.


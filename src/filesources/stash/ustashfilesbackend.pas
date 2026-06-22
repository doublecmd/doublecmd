unit uStashFilesBackend;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFile, uFileSystemFileSource, DCOSUtils;

type

  { TStashFilesBackend }

  TStashFilesBackend = class
  private
    _lockObject: TCriticalSection;
    _paths: TStringList;
  private
    procedure addPath( const path: String ); inline;
    procedure removePath( const path: String ); inline;
    procedure doAddFromString(const pathsStr: String);
    procedure doAddFromStringArray(const pathsArray: TStringArray);
  public
    constructor Create;
    destructor Destroy; override;

    procedure setListener( const listener: TNotifyEvent );

    procedure clear;
    function count: Integer;
    procedure addPaths( const files: TFiles );
    procedure removePaths( const files: TFiles );

    function toFiles: TFiles;
    function toStringArray: TStringArray;
    function toString: String; override;

    procedure addFromString( const pathsStr: String );
    procedure setFromString( const pathsStr: String );
    procedure addFromStringArray(const pathsArray: TStringArray);
    procedure setFromStringArray(const pathsArray: TStringArray);
  end;

var
  stashFilesBackend: TStashFilesBackend;

implementation

{ TStashFilesBackend }

constructor TStashFilesBackend.Create;
begin
  _lockObject:= TCriticalSection.Create;;
  _paths:= TStringList.Create;
  _paths.SortStyle:= sslAuto;
  _paths.Duplicates:= dupIgnore;
end;

destructor TStashFilesBackend.Destroy;
begin
  FreeAndNIl( _paths );
  FreeAndNil( _lockObject );
end;

procedure TStashFilesBackend.setListener(const listener: TNotifyEvent);
begin
  _paths.OnChange:= listener;
end;

procedure TStashFilesBackend.addPath(const path: String);
begin
  _paths.Add( ExcludeTrailingPathDelimiter(path) );
end;

procedure TStashFilesBackend.removePath(const path: String);
var
  i: Integer;
begin
  _paths.Find( ExcludeTrailingPathDelimiter(path), i );
  if i >= 0 then
    _paths.Delete( i );
end;

procedure TStashFilesBackend.clear;
begin
  _lockObject.Acquire;
  try
    _paths.Clear;
  finally
    _lockObject.Release;
  end;
end;

function TStashFilesBackend.count: Integer;
begin
  _lockObject.Acquire;
  try
    Result:= _paths.Count;
  finally
    _lockObject.Release;
  end;
end;

procedure TStashFilesBackend.addPaths(const files: TFiles);
var
  i: Integer;
begin
  _lockObject.Acquire;
  try
    for i:= 0 to files.Count-1 do
      self.addPath( files[i].FullPath );
  finally
    _lockObject.Release;
  end;
end;

procedure TStashFilesBackend.removePaths(const files: TFiles);
var
  i: Integer;
begin
  _lockObject.Acquire;
  try
    for i:= 0 to files.Count-1 do
      self.removePath( files[i].FullPath );
  finally
    _lockObject.Release;
  end;
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

  _lockObject.Acquire;
  try
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
  finally
    _lockObject.Release;
  end;

  Result:= files;
end;

function TStashFilesBackend.toStringArray: TStringArray;
begin
  _lockObject.Acquire;
  try
    Result:= _paths.ToStringArray;
  finally
    _lockObject.Release;
  end;
end;

function TStashFilesBackend.toString: String;
var
  stringBuilder: TAnsiStringBuilder;
  path: String;
begin
  stringBuilder:= TAnsiStringBuilder.Create;

  _lockObject.Acquire;
  try
    for path in _paths do begin
      stringBuilder.Append( path );
      stringBuilder.Append( #10 );
    end;
  finally
    _lockObject.Release;
  end;

  Result:= stringBuilder.ToString;
end;

procedure TStashFilesBackend.doAddFromString(const pathsStr: String);
var
  pathsArray: TStringArray;
  path: String;
begin
  pathsArray:= pathsStr.Split( #10 );
  for path in pathsArray do begin
    if path.IsEmpty then
      continue;
    if mbFileSystemEntryExists(path) then
      self.addPath( path );
  end;
end;

procedure TStashFilesBackend.doAddFromStringArray(const pathsArray: TStringArray);
var
  path: String;
begin
  for path in pathsArray do begin
    if path.IsEmpty then
      continue;
    if mbFileSystemEntryExists(path) then
      self.addPath( path );
  end;
end;

procedure TStashFilesBackend.addFromString(const pathsStr: String);
begin
  _lockObject.Acquire;
  try
    doAddFromString( pathsStr );
  finally
    _lockObject.Release;
  end;
end;

procedure TStashFilesBackend.addFromStringArray(const pathsArray: TStringArray);
begin
  _lockObject.Acquire;
  try
    doAddFromStringArray( pathsArray );
  finally
    _lockObject.Release;
  end;
end;

procedure TStashFilesBackend.setFromString(const pathsStr: String);
begin
  _lockObject.Acquire;
  try
    _paths.Clear;
    doAddFromString( pathsStr );
  finally
    _lockObject.Release;
  end;
end;

procedure TStashFilesBackend.setFromStringArray(const pathsArray: TStringArray);
begin
  _lockObject.Acquire;
  try
    _paths.Clear;
    doAddFromStringArray( pathsArray );
  finally
    _lockObject.Release;
  end;
end;

initialization
  stashFilesBackend:= TStashFilesBackend.Create;

finalization
  FreeAndNil( stashFilesBackend );

end.


unit uCloudRootDriver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uMacCloudCore, uMacCloudUtil, uMacCloudConfig, uMacCloudOptions,
  uMiniUtil;

type

  { TCloudRootDriver }

  TCloudRootDriver = class( TCloudDriverBase )
  private
    _list: TFPList;
  public
    procedure listFolderBegin(const path: String); override;
    function listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  public
    procedure createFolder(const path: String); override;
    procedure delete(const path: String); override;
    procedure copyOrMove(const fromPath: String; const toPath: String; const needToMove: Boolean); override;
  end;

  TCloudRootHelper = class
  public
    class procedure saveConfig( const path: String = '' );
    class function getDriver( const parser: TCloudPathParser ): TCloudDriverBase;
  end;

const
  CONST_ADD_NEW_CONNECTION = '<Add New Connection>';

implementation

{ TCloudRootHelper }

class procedure TCloudRootHelper.saveConfig( const path: String = '' );
var
  configPath: String;
begin
  if path = EmptyStr then
    configPath:= macCloudPlugin.configPath
  else
    configPath:= path;
  macCloudDriverConfigManager.saveToCommon( configPath );
  macCloudDriverConfigManager.saveToSecurity;
end;

class function TCloudRootHelper.getDriver( const parser: TCloudPathParser ): TCloudDriverBase;
begin
  if parser.driverPath = EmptyStr then
    Result:= TCloudRootDriver.Create
  else
    Result:= parser.driver;
end;

{ TCloudRootDriver }

procedure TCloudRootDriver.listFolderBegin(const path: String);
  procedure addNewCommand;
  var
    cloudFile: TCloudFile;
  begin
    cloudFile:= TCloudFile.Create;
    cloudFile.name:= CONST_ADD_NEW_CONNECTION;
    cloudFile.creationTime:= 4.5753942770578702E+004;
    cloudFile.modificationTime:= cloudFile.creationTime;
    _list.Add( cloudFile );
  end;

  procedure addConnections;
  var
    cloudFile: TCloudFile;
    connection: TCloudConnection;
    i: Integer;
  begin
    for i:= 0 to cloudConnectionManager.connections.Count - 1 do begin;
      connection:= TCloudConnection( cloudConnectionManager.connections[i] );
      cloudFile:= TCloudFile.Create;
      cloudFile.name:= connection.name;
      cloudFile.creationTime:= connection.creationTime;
      cloudFile.modificationTime:= connection.modificationTime;
      _list.Add( cloudFile );
    end;
  end;

begin
  _list:= TFPList.Create;
  addNewCommand;
  addConnections;
end;

function TCloudRootDriver.listFolderGetNextFile: TCloudFile;
begin
  if _list.Count > 0 then begin
    Result:= TCloudFile( _list.First );
    _list.Delete( 0 );
  end else begin
    Result:= nil;
  end;
end;

procedure TCloudRootDriver.listFolderEnd;
begin
  FreeAndNil( _list );
  self.Free;
end;

procedure TCloudRootDriver.createFolder(const path: String);
begin
  TCloudOptionsUtil.addAndShow( path );
  TCloudRootHelper.saveConfig;
  self.Free;
end;

procedure TCloudRootDriver.delete(const path: String);
var
  connectionName: String absolute path;
begin
  TLogUtil.logInformation( 'Connection Deleted: ' + connectionName );
  cloudConnectionManager.delete( connectionName );
  TCloudRootHelper.saveConfig;
  self.Free;
end;

procedure TCloudRootDriver.copyOrMove(const fromPath: String;
  const toPath: String; const needToMove: Boolean);
var
  connectionOldName: String absolute fromPath;
  connectionNewName: String absolute toPath;
  connection: TCloudConnection;
begin
  if NOT needToMove then
    raise ENotSupportedException.Create( 'Connection only support renaming' );
  TLogUtil.logInformation( 'Connection Rename: ' + connectionOldName + ' --> ' + connectionNewName );
  connection:= cloudConnectionManager.get( connectionOldName );
  connection.name:= connectionNewName;
  TCloudRootHelper.saveConfig;
  self.Free;
end;

end.


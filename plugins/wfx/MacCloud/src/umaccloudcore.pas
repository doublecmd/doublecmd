unit uMacCloudCore;

{$mode ObjFPC}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Contnrs,
  WfxPlugin,
  uCloudDriver,
  uMiniUtil;

type

  { TMacCloudPlugin }

  TMacCloudPlugin = class
  strict private
    _pluginNumber: Integer;
    _progressProc: TProgressProcW;
    _logProc: TLogProcW;
    _pluginPath: String;
    _configPath: String;
    procedure pluginLogProc( const MsgType: Integer; const message: String );
  public
    constructor Create( const pluginNumber: Integer; const progressProc: TProgressProcW; const logProc: TLogProcW );
    function progress( const sourceName: pwidechar; const targetName: pwidechar; const percentDone: Integer ): Integer;
  public
    property pluginNumber: Integer read _pluginNumber;
    property pluginPath: String read _pluginPath write _pluginPath;
    property configPath: String read _configPath write _configPath;
  end;

  { TCloudConnection }

  TCloudConnection = class
  private
    _name: String;
    _driver: TCloudDriver;
    _creationTime: TDateTime;
    _modificationTime: TDateTime;
  public
    constructor Create( const name: String; const driver: TCloudDriver;
      const creationTime: TDateTime; const modificationTime: TDateTime );
    destructor Destroy; override;
    property name: String read _name write _name;
    property driver: TCloudDriver read _driver;
    property creationTime: TDateTime read _creationTime;
    property modificationTime: TDateTime read _modificationTime;
  end;

  TCloudConnections = TFPObjectList;

  { TCloudConnectionManager }

  TCloudConnectionManager = class
  private
    _connections: TCloudConnections;
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure setConnections( const connections: TCloudConnections );
  public
    procedure add( const connection: TCloudConnection );
    function get( const name: String ): TCloudConnection;
    procedure delete( const name: String );
    property connections: TCloudConnections read _connections write setConnections;
  end;

var
  macCloudPlugin: TMacCloudPlugin;
  cloudConnectionManager: TCloudConnectionManager;

implementation

{ TMacCloudPlugin }

constructor TMacCloudPlugin.Create( const pluginNumber: Integer; const progressProc: TProgressProcW; const logProc: TLogProcW );
begin
  _pluginNumber:= pluginNumber;
  _progressProc:= progressProc;
  _logProc:= logProc;
  TLogUtil.setLogProc( @self.pluginLogProc );
end;

function TMacCloudPlugin.progress(const sourceName: pwidechar;
  const targetName: pwidechar; const percentDone: Integer): Integer;
begin
  Result:= _progressProc( _pluginNumber, sourceName, targetName, percentDone );
end;

procedure TMacCloudPlugin.pluginLogProc(const MsgType: Integer; const message: String);
var
  buffer: Array [0..1024*10-1] of widechar;
begin
  TStringUtil.stringToWidechars( buffer, 'MacCloud: ' + message, sizeof(buffer) );
  _logProc( _pluginNumber, MsgType, buffer );
end;

{ TCloudConnection }

constructor TCloudConnection.Create(
  const name: String;
  const driver: TCloudDriver;
  const creationTime: TDateTime;
  const modificationTime: TDateTime );
begin
  _name:= name;
  _driver:= driver;
  _creationTime:= creationTime;
  _modificationTime:= modificationTime;
end;

destructor TCloudConnection.Destroy;
begin
  FreeAndNil( _driver );
end;

{ TCloudConnectionManager }

constructor TCloudConnectionManager.Create;
begin
  _connections:= TCloudConnections.Create( True );
end;

destructor TCloudConnectionManager.Destroy;
begin
  FreeAndNil( _connections );
end;

procedure TCloudConnectionManager.setConnections(
  const connections: TCloudConnections);
var
  oldConnections: TCloudConnections;
begin
  oldConnections:= _connections;
  _connections:= connections;
  FreeAndNil( oldConnections );
end;

procedure TCloudConnectionManager.add(const connection: TCloudConnection);
begin
  _connections.Add( connection );
end;

function TCloudConnectionManager.get(const name: String): TCloudConnection;
var
  i: Integer;
  connection: TCloudConnection;
begin
  for i:= 0 to _connections.Count - 1 do begin
    connection:= TCloudConnection( _connections[i] );
    if connection.name <> name then
      continue;
    Exit( connection );
  end;

  raise EArgumentException.Create( 'Connection not found in TCloudConnectionManager.get(): ' + name );
end;

procedure TCloudConnectionManager.delete(const name: String);
var
  connection: TCloudConnection;
begin
  connection:= self.get( name );
  _connections.Remove( connection );
end;

initialization
  cloudDriverManager:= TCloudDriverManager.Create;
  cloudConnectionManager:= TCloudConnectionManager.Create;

finalization
  FreeAndNil( cloudConnectionManager );
  FreeAndNil( cloudDriverManager );

end.


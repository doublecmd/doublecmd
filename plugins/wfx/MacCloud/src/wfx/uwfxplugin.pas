unit uWFXPlugin;

{$mode ObjFPC}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Contnrs,
  WfxPlugin, Extension,
  uCloudDriver,
  uMiniUtil;

type

  { TWFXMacCloudPlugin }

  TWFXMacCloudPlugin = class
  strict private
    _pluginNumber: Integer;
    _progressProc: TProgressProcW;
    _logProc: TLogProcW;
    _pluginPath: String;
    _configPath: String;
    _languageID: String;
    procedure pluginLogProc( const MsgType: Integer; const message: String );
  public
    constructor Create( const pluginNumber: Integer; const progressProc: TProgressProcW; const logProc: TLogProcW );
    function progress( const sourceName: pwidechar; const targetName: pwidechar; const percentDone: Integer ): Integer;
    procedure translateResourceStrings(StartupInfo: PExtensionStartupInfo);
  public
    property pluginNumber: Integer read _pluginNumber;
    property pluginPath: String read _pluginPath write _pluginPath;
    property configPath: String read _configPath write _configPath;
    property languageID: String read _languageID write _languageID;
  end;

  { TWFXConnection }

  TWFXConnection = class
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

  TWFXConnections = TFPObjectList;

  { TWFXConnectionManager }

  TWFXConnectionManager = class
  private
    _connections: TWFXConnections;
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure setConnections( const connections: TWFXConnections );
  public
    procedure add( const connection: TWFXConnection );
    function get( const name: String ): TWFXConnection;
    procedure delete( const name: String );
    property connections: TWFXConnections read _connections write setConnections;
  end;

  { TWFXCloudDriverMenuItem }

  TWFXCloudDriverMenuItem = class
  private
    _name: String;
    _displayName: String;
  public
    constructor Create( const name: String; const displayName: String );
    property name: String read _name;
    property displayName: String read _displayName;
  end;

  { TWFXCloudDriverMenuItems }

  TWFXCloudDriverMenuItems = class
  private
    _items: TFPObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure add( const item: TWFXCloudDriverMenuItem ); overload;
    procedure add( const name: String; const displayName: String ); overload;
    procedure addSeparator;
    function count: Integer;
    function getItem( index: Integer ): TWFXCloudDriverMenuItem;
    property Items[Index: Integer]: TWFXCloudDriverMenuItem read getItem; default;
  end;

var
  WFXMacCloudPlugin: TWFXMacCloudPlugin;
  WFXConnectionManager: TWFXConnectionManager;
  WFXCloudDriverMenuItems: TWFXCloudDriverMenuItems;

implementation

{ TWFXMacCloudPlugin }

constructor TWFXMacCloudPlugin.Create( const pluginNumber: Integer; const progressProc: TProgressProcW; const logProc: TLogProcW );
begin
  _pluginNumber:= pluginNumber;
  _progressProc:= progressProc;
  _logProc:= logProc;
  TLogUtil.setLogProc( @self.pluginLogProc );
end;

function TWFXMacCloudPlugin.progress(const sourceName: pwidechar;
  const targetName: pwidechar; const percentDone: Integer): Integer;
begin
  Result:= _progressProc( _pluginNumber, sourceName, targetName, percentDone );
end;

function Translate(Name, Value: AnsiString; Hash: LongInt; Arg: Pointer): AnsiString;
var
  ALen: Integer;
  StartupInfo: PExtensionStartupInfo absolute Arg;
begin
  with StartupInfo^ do
  begin
    SetLength(Result, MaxSmallint);
    ALen:= TranslateString(Translation, PAnsiChar(Name), PAnsiChar(Value), PAnsiChar(Result), MaxSmallint);
    SetLength(Result, ALen);
  end;
end;

procedure TWFXMacCloudPlugin.translateResourceStrings(
  StartupInfo: PExtensionStartupInfo);
begin
  if Assigned(StartupInfo^.Translation) then
  begin
    SetResourceStrings(@Translate, StartupInfo);
  end;
end;

procedure TWFXMacCloudPlugin.pluginLogProc(const MsgType: Integer; const message: String);
var
  buffer: Array [0..1024*10-1] of widechar;
begin
  TStringUtil.stringToWidechars( buffer, 'MacCloud: ' + message, sizeof(buffer) );
  _logProc( _pluginNumber, MsgType, buffer );
end;

{ TWFXConnection }

constructor TWFXConnection.Create(
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

destructor TWFXConnection.Destroy;
begin
  FreeAndNil( _driver );
end;

{ TWFXConnectionManager }

constructor TWFXConnectionManager.Create;
begin
  _connections:= TWFXConnections.Create( True );
end;

destructor TWFXConnectionManager.Destroy;
begin
  FreeAndNil( _connections );
end;

procedure TWFXConnectionManager.setConnections(
  const connections: TWFXConnections);
var
  oldConnections: TWFXConnections;
begin
  oldConnections:= _connections;
  _connections:= connections;
  FreeAndNil( oldConnections );
end;

procedure TWFXConnectionManager.add(const connection: TWFXConnection);
begin
  _connections.Add( connection );
end;

function TWFXConnectionManager.get(const name: String): TWFXConnection;
var
  i: Integer;
  connection: TWFXConnection;
begin
  for i:= 0 to _connections.Count - 1 do begin
    connection:= TWFXConnection( _connections[i] );
    if connection.name <> name then
      continue;
    Exit( connection );
  end;

  raise EArgumentException.Create( 'Connection not found in TCloudConnectionManager.get(): ' + name );
end;

procedure TWFXConnectionManager.delete(const name: String);
var
  connection: TWFXConnection;
begin
  connection:= self.get( name );
  _connections.Remove( connection );
end;

{ TWFXCloudDriverMenuItem }

constructor TWFXCloudDriverMenuItem.Create(const name: String;
  const displayName: String);
begin
  _name:= name;
  _displayName:= displayName;
end;

{ TWFXCloudDriverMenuItems }

constructor TWFXCloudDriverMenuItems.Create;
begin
  _items:= TFPObjectList.Create;
end;

destructor TWFXCloudDriverMenuItems.Destroy;
begin
  FreeAndNil( _items );
end;

procedure TWFXCloudDriverMenuItems.add(const item: TWFXCloudDriverMenuItem);
begin
  _items.Add( item );
end;

procedure TWFXCloudDriverMenuItems.add(const name: String;
  const displayName: String);
begin
  self.add( TWFXCloudDriverMenuItem.Create(name, displayName ) );
end;

procedure TWFXCloudDriverMenuItems.addSeparator;
begin
  self.add( EmptyStr, EmptyStr );
end;

function TWFXCloudDriverMenuItems.count: Integer;
begin
  Result:= _items.Count;
end;

function TWFXCloudDriverMenuItems.getItem(index: Integer
  ): TWFXCloudDriverMenuItem;
begin
  Result:= TWFXCloudDriverMenuItem( _items[index] );
end;

initialization
  cloudDriverManager:= TCloudDriverManager.Create;
  WFXConnectionManager:= TWFXConnectionManager.Create;
  WFXCloudDriverMenuItems:= TWFXCloudDriverMenuItems.Create;

finalization
  FreeAndNil( WFXConnectionManager );
  FreeAndNil( cloudDriverManager );
  FreeAndNil( WFXCloudDriverMenuItems );

end.


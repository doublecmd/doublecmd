unit uMacCloudConfig;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Contnrs,
  CocoaAll, uMiniCocoa,
  uMacCloudCore,
  uDropBoxClient,
  uMiniUtil;

type

  { TCloudDriverConfig }

  TCloudDriverConfig = class
    class procedure loadCommon( const params: NSDictionary ); virtual; abstract;
    class procedure saveCommon( const params: NSMutableDictionary ); virtual; abstract;
    class procedure loadSecurity( const driver: TCloudDriver; const params: NSDictionary ); virtual; abstract;
    class procedure saveSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); virtual; abstract;
  end;

  { TDropBoxCloudDriverConfig }

  TDropBoxCloudDriverConfig = class( TCloudDriverConfig )
    class procedure loadCommon( const params: NSDictionary ); override;
    class procedure saveCommon( const params: NSMutableDictionary ); override;
    class procedure loadSecurity( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
  end;

  TCloudDriverConfigClass = class of TCloudDriverConfig;

  TCloudDriverConfigItems = TFPDataHashTable;

  { TCloudConfigManager }

  TCloudConfigManager = class( ICloudDriverObserver )
  private
    _configItems: TCloudDriverConfigItems;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure driverUpdated( const driver: TCloudDriver );
  public
    procedure register( const name: String; const config: TCloudDriverConfigClass );
    procedure loadFromCommon( const path: String );
    procedure saveToCommon( const path: String );
    procedure loadFromSecurity;
    procedure saveToSecurity;
  end;

var
  macCloudDriverConfigManager: TCloudConfigManager;

implementation

{ TDropBoxCloudDriverConfig }

class procedure TDropBoxCloudDriverConfig.loadCommon(const params: NSDictionary);
var
  clientID: String;
  listenURI: String;
  oldDropBoxConfig: TDropBoxConfig;
begin
  clientID:= TJsonUtil.getString( params, 'clientID' );
  listenURI:= TJsonUtil.getString( params, 'listenURI' );
  oldDropBoxConfig:= dropBoxConfig;
  dropBoxConfig:= TDropBoxConfig.Create( clientID, listenURI );
  if Assigned(oldDropBoxConfig) then
    oldDropBoxConfig.Free;
  cloudDriverManager.register( TDropBoxClient );
end;

class procedure TDropBoxCloudDriverConfig.saveCommon(const params: NSMutableDictionary);
begin
  TJsonUtil.setString( params, 'name', 'DropBox' );
  TJsonUtil.setString( params, 'clientID', dropBoxConfig.clientID );
  TJsonUtil.setString( params, 'listenURI', dropBoxConfig.listenURI );
end;

class procedure TDropBoxCloudDriverConfig.loadSecurity(
  const driver: TCloudDriver; const params: NSDictionary);
var
  dropBoxClient: TDropBoxClient absolute driver;
  token: TDropBoxToken;
  jsonToken: NSDictionary;
begin
  jsonToken:= TJsonUtil.getDictionary( params, 'token' );
  token:= TDropBoxToken.Create(
    TJsonUtil.getString( jsonToken, 'access' ),
    TJsonUtil.getString( jsonToken, 'refresh' ),
    TJsonUtil.getDateTime( jsonToken, 'accessExpirationTime' ) );
  dropBoxClient.setToken( token );
end;

class procedure TDropBoxCloudDriverConfig.saveSecurity(
  const driver: TCloudDriver; const params: NSMutableDictionary);
var
  dropBoxClient: TDropBoxClient absolute driver;
  token: TDropBoxToken;
  jsonToken: NSMutableDictionary;
begin
  token:= dropBoxClient.getToken;
  jsonToken:= NSMutableDictionary.new;
  TJsonUtil.setString( jsonToken, 'access', token.access );
  TJsonUtil.setString( jsonToken, 'refresh', token.refresh );
  TJsonUtil.setDateTime( jsonToken, 'accessExpirationTime', token.accessExpirationTime );
  TJsonUtil.setDictionary( params, 'token', jsonToken );
  jsonToken.release;
end;

{ TCloudConfigManager }

constructor TCloudConfigManager.Create;
begin
  _configItems:= TCloudDriverConfigItems.Create;
  cloudDriverManager.observer:= self;
end;

destructor TCloudConfigManager.Destroy;
begin
  cloudDriverManager.observer:= nil;
  _configItems.Free;
end;

procedure TCloudConfigManager.driverUpdated(const driver: TCloudDriver);
begin
  saveToSecurity;
end;

procedure TCloudConfigManager.register(const name: String;
  const config: TCloudDriverConfigClass);
begin
  _configItems.Add( name, config );
end;

procedure TCloudConfigManager.loadFromSecurity;
  procedure loadConnectionsSecurity( const jsonConnections: NSArray );
  var
    jsonConnection: NSMutableDictionary;
    connectionName: String;
    connection: TCloudConnection;
    driverName: String;
    config: TCloudDriverConfigClass;
  begin
    for jsonConnection in jsonConnections do begin
      connectionName:= TJsonUtil.getString( jsonConnection, 'name' );
      connection:= cloudConnectionManager.get( connectionName );
      driverName:= connection.driver.driverName;
      config:= TCloudDriverConfigClass( _configItems[driverName] );
      if config = nil then
        raise Exception.Create( 'driver not fount in loadFromSecurity.TCloudConfigManager(): ' + driverName );
      config.loadSecurity( connection.driver, jsonConnection );
    end;
  end;

var
  jsonString: String;
  json: NSDictionary;
  jsonConnections: NSArray;
begin
  jsonString:= TSecUtil.getValue( 'MacCloud.wfx', 'connections' );
  if jsonString = EmptyStr then
    Exit;
  json:= TJsonUtil.parse( jsonString );
  jsonConnections:= TJsonUtil.getArray( json, 'connections' );
  loadConnectionsSecurity( jsonConnections );
end;

procedure TCloudConfigManager.saveToSecurity;
  function saveConnectionsSecurity: NSArray;
  var
    json: NSMutableArray;
    jsonConnection: NSMutableDictionary;
    i: Integer;
    connections: TCloudConnections;
    connection: TCloudConnection;
    driverName: String;
    config: TCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    connections:= cloudConnectionManager.connections;
    for i:=0 to connections.Count-1 do begin
      jsonConnection:= NSMutableDictionary.new;
      connection:= TCloudConnection( connections[i] );
      driverName:= connection.driver.driverName;
      TJsonUtil.setString( jsonConnection, 'name', connection.name );
      config:= TCloudDriverConfigClass( _configItems[driverName] );
      config.saveSecurity( connection.driver, jsonConnection );
      json.addObject( jsonConnection );
      jsonConnection.release;
    end;
    Result:= json;
  end;

var
  jsonString: String;
  jsonConnectionsSecurity: NSArray;
begin
  jsonConnectionsSecurity:= saveConnectionsSecurity;
  jsonString:= TJsonUtil.dumps(
    [ 'connections', jsonConnectionsSecurity ] );
  TSecUtil.saveValue( 'MacCloud.wfx', 'connections', jsonString );
end;

procedure TCloudConfigManager.loadFromCommon( const path: String );
  procedure loadDrivers( const jsonDrivers: NSArray );
  var
    jsonDriver: NSDictionary;
    driverName: String;
    config: TCloudDriverConfigClass;
  begin
    for jsonDriver in jsonDrivers do begin
      driverName:= TJsonUtil.getString( jsonDriver, 'name' );
      config:= TCloudDriverConfigClass( _configItems[driverName] );
      config.loadCommon( jsonDriver );
    end;
  end;

  procedure loadConnections( const jsonConnections: NSArray );
  var
    jsonConnection: NSDictionary;
    connections: TCloudConnections;
    connection: TCloudConnection;
    driverName: String;
  begin
    connections:= TCloudConnections.Create( True );
    for jsonConnection in jsonConnections do begin
      driverName:= TJsonUtil.getString( jsonConnection, 'driver' );
      connection:= TCloudConnection.Create(
        TJsonUtil.getString( jsonConnection, 'name' ),
        cloudDriverManager.createInstance( driverName ),
        TJsonUtil.getDateTime( jsonConnection, 'creationTime' ),
        TJsonUtil.getDateTime( jsonConnection, 'modificationTime' ) );
      connections.Add( connection );
    end;
    cloudConnectionManager.connections:= connections;
  end;
var
  jsonString: NSString;
  json: NSDictionary;
  error: NSError;
begin
  error:= nil;
  jsonString:= NSString.stringWithContentsOfFile_encoding_error(
    StringToNSString(path), NSUTF8StringEncoding, @error );
  if error <> nil then
    raise Exception.Create( error.localizedDescription.UTF8String );
  json:= TJsonUtil.parse( jsonString );
  loadDrivers( TJsonUtil.getArray(json, 'drivers') );
  loadConnections( TJsonUtil.getArray(json, 'connections') );
end;

procedure TCloudConfigManager.saveToCommon(const path: String);
  function saveDrivers: NSArray;
  var
    json: NSMutableArray;
    jsonDriver: NSMutableDictionary;
    i: Integer;
    driverClasses: TCloudDriverClasses;
    driverName: String;
    config: TCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    driverClasses:= cloudDriverManager.driverClasses;
    for i:=0 to driverClasses.Count-1 do begin
      jsonDriver:= NSMutableDictionary.new;
      driverName:= TCloudDriverClass(driverClasses[i]).driverName;
      config:= TCloudDriverConfigClass( _configItems[driverName] );
      config.saveCommon( jsonDriver );
      json.addObject( jsonDriver );
      jsonDriver.release;
    end;
    Result:= json;
  end;

  function saveConnectionsCommon: NSArray;
  var
    json: NSMutableArray;
    jsonConnection: NSMutableDictionary;
    i: Integer;
    connections: TCloudConnections;
    connection: TCloudConnection;
  begin
    json:= NSMutableArray.new.autorelease;
    connections:= cloudConnectionManager.connections;
    for i:=0 to connections.Count-1 do begin
      jsonConnection:= NSMutableDictionary.new;
      connection:= TCloudConnection( connections[i] );
      TJsonUtil.setString( jsonConnection, 'name', connection.name );
      TJsonUtil.setString( jsonConnection, 'driver', connection.driver.driverName );
      TJsonUtil.setDateTime( jsonConnection, 'creationTime', connection.creationTime );
      TJsonUtil.setDateTime( jsonConnection, 'modificationTime', connection.modificationTime );
      json.addObject( jsonConnection );
      jsonConnection.release;
    end;
    Result:= json;
  end;

var
  jsonString: String;
  jsonDrivers: NSArray;
  jsonConnectionsCommon: NSArray;
begin
  jsonDrivers:= saveDrivers;
  jsonConnectionsCommon:= saveConnectionsCommon;

  jsonString:= TJsonUtil.dumps(
    [ 'drivers',jsonDrivers, 'connections',jsonConnectionsCommon ],
    False,
    NSJSONWritingWithoutEscapingSlashes or NSJSONWritingPrettyPrinted );

  StringToNSString(jsonString).writeToFile_atomically_encoding_error(
    StringToNSString(path),
    False,
    NSUTF8StringEncoding,
    nil );
end;

procedure initMacCloudDriverManager;
begin
  macCloudDriverConfigManager:= TCloudConfigManager.Create;
  macCloudDriverConfigManager.register( 'DropBox', TDropBoxCloudDriverConfig );

  // the following are internal default configurations,
  // which would be overridden by MacCloud.json,
  // used when there is no MacCloud.json
  dropBoxConfig:= TDropBoxConfig.Create( 'ahj0s9xia6i61gh', 'dc2ea085a05ac273a://dropbox/auth' );
  cloudDriverManager.register( TDropBoxClient );
end;

initialization
  initMacCloudDriverManager;

finalization
  FreeAndNil( macCloudDriverConfigManager );

end.


unit uMacCloudConfig;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Contnrs,
  CocoaAll, uMiniCocoa,
  uMacCloudCore,
  uCloudDriver, uDropBoxClient, uYandexClient,
  uMiniUtil;

type

  { TMacCloudDriverConfig }

  TMacCloudDriverConfig = class
    class procedure loadCommon( const params: NSDictionary ); virtual; abstract;
    class procedure saveCommon( const params: NSMutableDictionary ); virtual; abstract;
    class procedure loadSecurity( const driver: TCloudDriver; const params: NSDictionary ); virtual; abstract;
    class procedure saveSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); virtual; abstract;
  end;

  TMacCloudDriverConfigClass = class of TMacCloudDriverConfig;

  TMacCloudDriverConfigItems = TFPDataHashTable;

  { TMacCloudConfigManager }

  TMacCloudConfigManager = class( ICloudDriverObserver )
  private
    _configItems: TMacCloudDriverConfigItems;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure driverUpdated( const driver: TCloudDriver );
  public
    procedure register( const name: String; const config: TMacCloudDriverConfigClass );
    procedure loadFromCommon( const path: String );
    procedure saveToCommon( const path: String );
    procedure loadFromSecurity;
    procedure saveToSecurity;
  end;

var
  macCloudDriverConfigManager: TMacCloudConfigManager;

implementation

type

  { TDropBoxCloudDriverConfig }

  TDropBoxCloudDriverConfig = class( TMacCloudDriverConfig )
    class procedure loadCommon( const params: NSDictionary ); override;
    class procedure saveCommon( const params: NSMutableDictionary ); override;
    class procedure loadSecurity( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
  end;

  { TYandexCloudDriverConfig }

  TYandexCloudDriverConfig = class( TMacCloudDriverConfig )
    class procedure loadCommon( const params: NSDictionary ); override;
    class procedure saveCommon( const params: NSMutableDictionary ); override;
    class procedure loadSecurity( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
  end;

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

{ TYandexCloudDriverConfig }

class procedure TYandexCloudDriverConfig.loadCommon(const params: NSDictionary);
var
  clientID: String;
  listenURI: String;
  oldYandexConfig: TCloudDriverConfig;
begin
  clientID:= TJsonUtil.getString( params, 'clientID' );
  listenURI:= TJsonUtil.getString( params, 'listenURI' );
  oldYandexConfig:= yandexConfig;
  yandexConfig:= TCloudDriverConfig.Create( clientID, listenURI );
  if Assigned(oldYandexConfig) then
    oldYandexConfig.Free;
  cloudDriverManager.register( TYandexClient );
end;

class procedure TYandexCloudDriverConfig.saveCommon(const params: NSMutableDictionary);
begin
  TJsonUtil.setString( params, 'clientID', yandexConfig.clientID );
  TJsonUtil.setString( params, 'listenURI', yandexConfig.listenURI );
end;

class procedure TYandexCloudDriverConfig.loadSecurity(
  const driver: TCloudDriver; const params: NSDictionary);
var
  yandexClient: TYandexClient absolute driver;
  token: TCloudDriverToken;
  jsonToken: NSDictionary;
begin
  jsonToken:= TJsonUtil.getDictionary( params, 'token' );
  token:= TCloudDriverToken.Create(
    TJsonUtil.getString( jsonToken, 'access' ),
    TJsonUtil.getString( jsonToken, 'refresh' ),
    TJsonUtil.getDateTime( jsonToken, 'accessExpirationTime' ) );
  yandexClient.setToken( token );
end;

class procedure TYandexCloudDriverConfig.saveSecurity(
  const driver: TCloudDriver; const params: NSMutableDictionary);
var
  yandexClient: TYandexClient absolute driver;
  token: TCloudDriverToken;
  jsonToken: NSMutableDictionary;
begin
  token:= yandexClient.getToken;
  jsonToken:= NSMutableDictionary.new;
  TJsonUtil.setString( jsonToken, 'access', token.access );
  TJsonUtil.setString( jsonToken, 'refresh', token.refresh );
  TJsonUtil.setDateTime( jsonToken, 'accessExpirationTime', token.accessExpirationTime );
  TJsonUtil.setDictionary( params, 'token', jsonToken );
  jsonToken.release;
end;

{ TMacCloudConfigManager }

constructor TMacCloudConfigManager.Create;
begin
  _configItems:= TMacCloudDriverConfigItems.Create;
  cloudDriverManager.observer:= self;
end;

destructor TMacCloudConfigManager.Destroy;
begin
  cloudDriverManager.observer:= nil;
  _configItems.Free;
end;

procedure TMacCloudConfigManager.driverUpdated(const driver: TCloudDriver);
begin
  saveToSecurity;
end;

procedure TMacCloudConfigManager.register(const name: String;
  const config: TMacCloudDriverConfigClass);
begin
  _configItems.Add( name, config );
end;

procedure TMacCloudConfigManager.loadFromSecurity;
  procedure loadConnectionsSecurity( const jsonConnections: NSArray );
  var
    jsonConnection: NSMutableDictionary;
    connectionName: String;
    connection: TCloudConnection;
    driverName: String;
    config: TMacCloudDriverConfigClass;
  begin
    for jsonConnection in jsonConnections do begin
      connectionName:= TJsonUtil.getString( jsonConnection, 'name' );
      connection:= cloudConnectionManager.get( connectionName );
      driverName:= connection.driver.driverName;
      config:= TMacCloudDriverConfigClass( _configItems[driverName] );
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

procedure TMacCloudConfigManager.saveToSecurity;
  function saveConnectionsSecurity: NSArray;
  var
    json: NSMutableArray;
    jsonConnection: NSMutableDictionary;
    i: Integer;
    connections: TCloudConnections;
    connection: TCloudConnection;
    driverName: String;
    config: TMacCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    connections:= cloudConnectionManager.connections;
    for i:=0 to connections.Count-1 do begin
      jsonConnection:= NSMutableDictionary.new;
      connection:= TCloudConnection( connections[i] );
      driverName:= connection.driver.driverName;
      TJsonUtil.setString( jsonConnection, 'name', connection.name );
      config:= TMacCloudDriverConfigClass( _configItems[driverName] );
      config.saveSecurity( connection.driver, jsonConnection );
      json.addObject( jsonConnection );
      jsonConnection.release;
    end;
    Result:= json;
  end;

var
  jsonString: NSString;
  jsonConnectionsSecurity: NSArray;
begin
  jsonConnectionsSecurity:= saveConnectionsSecurity;
  jsonString:= TJsonUtil.dumps(
    [ 'connections', jsonConnectionsSecurity ] );
  TSecUtil.saveValue( 'MacCloud.wfx', 'connections', jsonString );
end;

procedure TMacCloudConfigManager.loadFromCommon( const path: String );
  procedure loadDrivers( const jsonDrivers: NSArray );
  var
    jsonDriver: NSDictionary;
    driverName: String;
    config: TMacCloudDriverConfigClass;
  begin
    for jsonDriver in jsonDrivers do begin
      driverName:= TJsonUtil.getString( jsonDriver, 'name' );
      config:= TMacCloudDriverConfigClass( _configItems[driverName] );
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

procedure TMacCloudConfigManager.saveToCommon(const path: String);
  function saveDrivers: NSArray;
  var
    json: NSMutableArray;
    jsonDriver: NSMutableDictionary;
    i: Integer;
    driverClasses: TCloudDriverClasses;
    driverName: String;
    config: TMacCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    driverClasses:= cloudDriverManager.driverClasses;
    for i:=0 to driverClasses.Count-1 do begin
      jsonDriver:= NSMutableDictionary.new;
      driverName:= TCloudDriverClass(driverClasses[i]).driverName;
      config:= TMacCloudDriverConfigClass( _configItems[driverName] );
      TJsonUtil.setString( jsonDriver, 'name', driverName );
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
  jsonString: NSString;
  jsonDrivers: NSArray;
  jsonConnectionsCommon: NSArray;
begin
  jsonDrivers:= saveDrivers;
  jsonConnectionsCommon:= saveConnectionsCommon;

  jsonString:= TJsonUtil.dumps(
    [ 'drivers',jsonDrivers, 'connections',jsonConnectionsCommon ],
    False,
    NSJSONWritingWithoutEscapingSlashes or NSJSONWritingPrettyPrinted );

  jsonString.writeToFile_atomically_encoding_error(
    StringToNSString(path),
    False,
    NSUTF8StringEncoding,
    nil );
end;

procedure initMacCloudDriverManager;
begin
  // the following are internal default configurations,
  // which would be overridden by MacCloud.json,
  // used when there is no MacCloud.json
  macCloudDriverConfigManager:= TMacCloudConfigManager.Create;
  macCloudDriverConfigManager.register( TDropBoxClient.driverName, TDropBoxCloudDriverConfig );
  macCloudDriverConfigManager.register( TYandexClient.driverName, TYandexCloudDriverConfig );

  dropBoxConfig:= TDropBoxConfig.Create( 'ahj0s9xia6i61gh', 'dc2ea085a05ac273a://dropbox/auth' );
  cloudDriverManager.register( TDropBoxClient );

  yandexConfig:= TCloudDriverConfig.Create( 'eaf0c133568a46a0bd986bffb48c62b6', 'dc2ea085a05ac273a://yandex/auth' );
  cloudDriverManager.register( TYandexClient );
end;

initialization
  initMacCloudDriverManager;

finalization
  FreeAndNil( macCloudDriverConfigManager );

end.


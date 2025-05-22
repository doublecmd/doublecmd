unit uWFXConfig;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Contnrs,
  CocoaAll, uMiniCocoa,
  uWFXPlugin, uCloudDriver,
  uOAuth2Core, uOAuth2Client, uDropBoxClient, uYandexClient, uOneDriveClient, uBoxClient,
  uAWSCore, uS3Client,
  uAmazonS3Client, uBackBlazeB2Client, uS3CompatibleClient,
  uAliyunOSSClient, uTencentCOSClient, uHuaweiOBSClient, uQiniuKODOClient, uUpyunUSSClient,
  uMiniUtil;

type

  { TWFXCloudDriverConfig }

  TWFXCloudDriverConfig = class
    class procedure loadDriverCommon( const params: NSDictionary ); virtual; abstract;
    class procedure saveDriverCommon( const params: NSMutableDictionary ); virtual; abstract;
    class procedure loadConnectionCommon( const driver: TCloudDriver; const params: NSDictionary ); virtual; abstract;
    class procedure saveConnectionCommon( const driver: TCloudDriver; const params: NSMutableDictionary ); virtual; abstract;
    class procedure loadConnectionSecurity( const driver: TCloudDriver; const params: NSDictionary ); virtual; abstract;
    class procedure saveConnectionSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); virtual; abstract;
    class function getNotes: String; virtual; abstract;
  end;

  TWFXCloudDriverConfigClass = class of TWFXCloudDriverConfig;

  TWFXCloudDriverConfigItems = TFPDataHashTable;

  { TWFXCloudDriverConfigManager }

  TWFXCloudDriverConfigManager = class( ICloudDriverObserver )
  private
    _configItems: TWFXCloudDriverConfigItems;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure driverUpdated( const driver: TCloudDriver );
  public
    class procedure initMacCloudDriverManager;
    procedure register( const name: String; const config: TWFXCloudDriverConfigClass );
    function get( const name: String ): TWFXCloudDriverConfigClass;
    procedure loadFromCommon( const path: String );
    procedure saveToCommon( const path: String );
    procedure loadFromSecurity;
    procedure saveToSecurity;
  end;

var
  WFXCloudDriverConfigManager: TWFXCloudDriverConfigManager;

implementation

resourcestring
  rsDropBoxDisplayName = 'DropBox';
  rsYandexDisplayName = 'Yandex';
  rsOneDriveDisplayName = 'OneDrive';
  rsBoxDisplayName = 'Box';
  rsAmazonS3DisplayName = 'Amazon S3';
  rsBackBlazeB2DisplayName = 'BackBlaze B2';
  rsS3CompatibleDisplayName = 'S3 Compatible';
  rsAliyunOSSDisplayName = 'Alibaba Cloud OSS';
  rsTencentCOSDisplayName = 'Tencent Cloud COS';
  rsHuaweiOBSDisplayName = 'Huawei Cloud OBS';
  rsQiniuKODODisplayName = 'Qiniu Cloud KODO';
  rsUpyunUSSDisplayName = 'Upyun USS';

  rsOAuth2AuthNotes =
    '1. Before successfully enabling the connection, Double Commander needs to obtain authorization from {driverName}'#13#13 +
    '2. Click the connect button to be redirected to the {driverName} official website in the Safari browser'#13#13 +
    '3. Please login your {driverName} account in Safari and authorize Double Commander to access'#13#13 +
    '4. The authorization is completed on the {driverName} official website, Double Commander will not get your password';
  rsS3AutoConfigNotes =
    '1. AccessKeyID and SerectAccessKey will be saved in the macOS KeyChains to obtain system-level security. The confidential data can only be read by your own macOS permissions.'#13#13 +
    '2. Access Key ID and Secret Access Key are required, and the others are optional. Double Commander can usually automatically obtain others such as Buckets. Therefore, Region / EndPoint / Bucket are only required if Access Key permissions are insufficient.';
  rsS3ManualConfigNotes =
    '1. AccessKeyID and SerectAccessKey will be saved in the macOS KeyChains to obtain system-level security. The confidential data can only be read by your own macOS permissions.'#13#13 +
    '2. Access Key ID / Secret Access Key / Region / EndPoint / Bucket are required for {driverName}';

type

  { TWFXTokenCloudDriverConfig }

  TWFXTokenCloudDriverConfig = class( TWFXCloudDriverConfig )
    class procedure loadDriverCommon( const params: NSDictionary ); override;
    class procedure saveDriverCommon( const params: NSMutableDictionary ); override;
    class procedure loadConnectionCommon( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveConnectionCommon( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
    class procedure loadConnectionSecurity( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveConnectionSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
    class function getNotes: String; override;
    class function cloudDriverConfigPtr: TTokenCloudDriverConfigPtr; virtual; abstract;
    class function cloudDriverClass: TCloudDriverClass; virtual; abstract;
  end;

  { TWFXDropBoxConfig }

  TWFXDropBoxConfig = class( TWFXTokenCloudDriverConfig )
    class function cloudDriverConfigPtr: TTokenCloudDriverConfigPtr; override;
    class function cloudDriverClass: TCloudDriverClass; override;
  end;

  { TWFXYandexConfig }

  TWFXYandexConfig = class( TWFXTokenCloudDriverConfig )
    class function cloudDriverConfigPtr: TTokenCloudDriverConfigPtr; override;
    class function cloudDriverClass: TCloudDriverClass; override;
  end;

  { TWFXOneDriveConfig }

  TWFXOneDriveConfig = class( TWFXTokenCloudDriverConfig )
    class function cloudDriverConfigPtr: TTokenCloudDriverConfigPtr; override;
    class function cloudDriverClass: TCloudDriverClass; override;
  end;

  { TWFXBoxConfig }

  TWFXBoxConfig = class( TWFXTokenCloudDriverConfig )
    class function cloudDriverConfigPtr: TTokenCloudDriverConfigPtr; override;
    class function cloudDriverClass: TCloudDriverClass; override;
  end;

  { TWFXS3Config }

  TWFXS3Config = class( TWFXCloudDriverConfig )
    class procedure loadDriverCommon( const params: NSDictionary ); override;
    class procedure saveDriverCommon( const params: NSMutableDictionary ); override;
    class procedure loadConnectionCommon( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveConnectionCommon( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
    class procedure loadConnectionSecurity( const driver: TCloudDriver; const params: NSDictionary ); override;
    class procedure saveConnectionSecurity( const driver: TCloudDriver; const params: NSMutableDictionary ); override;
  end;

  { TWFXS3AutoConfig }

  TWFXS3AutoConfig = class( TWFXS3Config )
    class function getNotes: String; override;
  end;

  { TWFXS3ManualConfig }

  TWFXS3ManualConfig = class( TWFXS3Config )
    class function getNotes: String; override;
  end;

{ TWFXTokenCloudDriverConfig }

class procedure TWFXTokenCloudDriverConfig.loadDriverCommon(const params: NSDictionary);
var
  clientID: String;
  listenURI: String;
  oldCloudDriverConfig: TTokenCloudDriverConfig;
begin
  clientID:= TJsonUtil.getString( params, 'clientID' );
  listenURI:= TJsonUtil.getString( params, 'listenURI' );
  oldCloudDriverConfig:= self.cloudDriverConfigPtr^;
  self.cloudDriverConfigPtr^:= TTokenCloudDriverConfig.Create( clientID, listenURI );
  if Assigned(oldCloudDriverConfig) then
    oldCloudDriverConfig.Free;
  cloudDriverManager.register( self.cloudDriverClass );
end;

class procedure TWFXTokenCloudDriverConfig.saveDriverCommon(const params: NSMutableDictionary);
var
  cloudDriverConfig: TTokenCloudDriverConfig;
begin
  cloudDriverConfig:= self.cloudDriverConfigPtr^;
  TJsonUtil.setString( params, 'clientID', cloudDriverConfig.clientID );
  TJsonUtil.setString( params, 'listenURI', cloudDriverConfig.listenURI );
end;

class procedure TWFXTokenCloudDriverConfig.loadConnectionCommon( const driver: TCloudDriver; const params: NSDictionary );
begin
end;

class procedure TWFXTokenCloudDriverConfig.saveConnectionCommon( const driver: TCloudDriver; const params: NSMutableDictionary );
begin
end;

class procedure TWFXTokenCloudDriverConfig.loadConnectionSecurity(
  const driver: TCloudDriver; const params: NSDictionary);
var
  token: TCloudDriverToken;
  jsonToken: NSDictionary;
begin
  jsonToken:= TJsonUtil.getDictionary( params, 'token' );
  token:= TCloudDriverToken.Create(
    TJsonUtil.getString( jsonToken, 'access' ),
    TJsonUtil.getString( jsonToken, 'refresh' ),
    TJsonUtil.getDateTime( jsonToken, 'accessExpirationTime' ) );
  TTokenCloudDriver(driver).setToken( token );
end;

class procedure TWFXTokenCloudDriverConfig.saveConnectionSecurity(
  const driver: TCloudDriver; const params: NSMutableDictionary);
var
  client: TOAuth2SessionCloudDriver absolute driver;
  token: TCloudDriverToken;
  jsonToken: NSMutableDictionary;
begin
  token:= client.getToken;
  jsonToken:= NSMutableDictionary.new;
  TJsonUtil.setString( jsonToken, 'access', token.access );
  TJsonUtil.setString( jsonToken, 'refresh', token.refresh );
  TJsonUtil.setDateTime( jsonToken, 'accessExpirationTime', token.accessExpirationTime );
  TJsonUtil.setDictionary( params, 'token', jsonToken );
  jsonToken.release;
end;

class function TWFXTokenCloudDriverConfig.getNotes: String;
begin
  Result:= rsOAuth2AuthNotes;
end;

{ TWFXDropBoxConfig }

class function TWFXDropBoxConfig.cloudDriverConfigPtr: TTokenCloudDriverConfigPtr;
begin
  Result:= @dropBoxConfig;
end;

class function TWFXDropBoxConfig.cloudDriverClass: TCloudDriverClass;
begin
  Result:= TDropBoxClient;
end;

{ TWFXYandexConfig }

class function TWFXYandexConfig.cloudDriverConfigPtr: TTokenCloudDriverConfigPtr;
begin
  Result:= @yandexConfig;
end;

class function TWFXYandexConfig.cloudDriverClass: TCloudDriverClass;
begin
  Result:= TYandexClient;
end;

{ TWFXOneDriveConfig }

class function TWFXOneDriveConfig.cloudDriverConfigPtr: TTokenCloudDriverConfigPtr;
begin
  Result:= @oneDriveConfig;
end;

class function TWFXOneDriveConfig.cloudDriverClass: TCloudDriverClass;
begin
  Result:= TOneDriveClient;
end;

{ TWFXBoxConfig }

class function TWFXBoxConfig.cloudDriverConfigPtr: TTokenCloudDriverConfigPtr;
begin
  Result:= @boxConfig;
end;

class function TWFXBoxConfig.cloudDriverClass: TCloudDriverClass;
begin
  Result:= TBoxClient;
end;

{ TWFXS3Config }

class procedure TWFXS3Config.loadDriverCommon( const params: NSDictionary );
begin
end;

class procedure TWFXS3Config.saveDriverCommon( const params: NSMutableDictionary );
begin
end;

class procedure TWFXS3Config.loadConnectionCommon( const driver: TCloudDriver; const params: NSDictionary );
var
  client: TAWSCloudDriver absolute driver;
  connectionData: TAWSConnectionData;
  jsonConnectionData: NSDictionary;
begin
  jsonConnectionData:= TJsonUtil.getDictionary( params, 'connectionData' );
  connectionData:= Default( TAWSConnectionData );
  connectionData.region:= TJsonUtil.getString( jsonConnectionData, 'region' );
  connectionData.endPoint:= TJsonUtil.getString( jsonConnectionData, 'endPoint' );
  connectionData.bucketName:= TJsonUtil.getString( jsonConnectionData, 'bucket' );
  client.setDefaultConnectionData( connectionData );
end;

class procedure TWFXS3Config.saveConnectionCommon( const driver: TCloudDriver; const params: NSMutableDictionary );
var
  client: TAWSCloudDriver absolute driver;
  connectionData: TAWSConnectionData;
  jsonConnectionData: NSMutableDictionary;
begin
  jsonConnectionData:= NSMutableDictionary.new;
  connectionData:= client.getDefaultConnectionData;
  TJsonUtil.setString( jsonConnectionData, 'region', connectionData.region );
  TJsonUtil.setString( jsonConnectionData, 'endPoint', connectionData.endPoint );
  TJsonUtil.setString( jsonConnectionData, 'bucket', connectionData.bucketName );
  TJsonUtil.setDictionary( params, 'connectionData', jsonConnectionData );
  jsonConnectionData.release;
end;

class procedure TWFXS3Config.loadConnectionSecurity(
  const driver: TCloudDriver;
  const params: NSDictionary);
var
  accessKey: TS3AccessKey;
  jsonAccessKey: NSDictionary;
begin
  jsonAccessKey:= TJsonUtil.getDictionary( params, 'accessKey' );
  accessKey:= TS3AccessKey.Create(
    TJsonUtil.getString( jsonAccessKey, 'id' ),
    TJsonUtil.getString( jsonAccessKey, 'secret' ),
    TJsonUtil.getString( jsonAccessKey, 'token' )
  );
  TS3Client(driver).setAccessKey( accessKey );
end;

class procedure TWFXS3Config.saveConnectionSecurity(
  const driver: TCloudDriver;
  const params: NSMutableDictionary );
var
  client: TS3Client absolute driver;
  accessKey: TAWSAccessKey;
  jsonAccessKey: NSMutableDictionary;
begin
  accessKey:= client.getAccessKey;
  jsonAccessKey:= NSMutableDictionary.new;
  TJsonUtil.setString( jsonAccessKey, 'id', accessKey.id );
  TJsonUtil.setString( jsonAccessKey, 'secret', accessKey.secret );
  TJsonUtil.setString( jsonAccessKey, 'token', accessKey.token );
  TJsonUtil.setDictionary( params, 'accessKey', jsonAccessKey );
  jsonAccessKey.release;
end;

{ TWFXS3AutoConfig }

class function TWFXS3AutoConfig.getNotes: String;
begin
  Result:= rsS3AutoConfigNotes;
end;

{ TWFXS3ManualConfig }

class function TWFXS3ManualConfig.getNotes: String;
begin
  Result:= rsS3ManualConfigNotes;
end;

{ TWFXCloudDriverConfigManager }

constructor TWFXCloudDriverConfigManager.Create;
begin
  _configItems:= TWFXCloudDriverConfigItems.Create;
  cloudDriverManager.observer:= self;
end;

destructor TWFXCloudDriverConfigManager.Destroy;
begin
  cloudDriverManager.observer:= nil;
  _configItems.Free;
end;

procedure TWFXCloudDriverConfigManager.driverUpdated(const driver: TCloudDriver);
begin
  saveToSecurity;
end;

class procedure TWFXCloudDriverConfigManager.initMacCloudDriverManager;
begin
  // the following are internal default configurations,
  // which would be overridden by MacCloud.json,
  // used when there is no MacCloud.json
  if Assigned(WFXCloudDriverConfigManager) then
    Exit;

  WFXCloudDriverConfigManager:= TWFXCloudDriverConfigManager.Create;

  WFXCloudDriverConfigManager.register( TDropBoxClient.driverName, TWFXDropBoxConfig );
  dropBoxConfig:= TTokenCloudDriverConfig.Create( 'ahj0s9xia6i61gh', 'dc2ea085a05ac273a://dropbox/auth' );
  cloudDriverManager.register( TDropBoxClient );
  WFXCloudDriverMenuItems.add( TDropBoxClient.driverName, rsDropBoxDisplayName );

  WFXCloudDriverConfigManager.register( TYandexClient.driverName, TWFXYandexConfig );
  yandexConfig:= TTokenCloudDriverConfig.Create( 'eaf0c133568a46a0bd986bffb48c62b6', 'dc2ea085a05ac273a://yandex/auth' );
  cloudDriverManager.register( TYandexClient );
  WFXCloudDriverMenuItems.add( TYandexClient.driverName, rsYandexDisplayName );

  WFXCloudDriverConfigManager.register( TOneDriveClient.driverName, TWFXOneDriveConfig );
  oneDriveConfig:= TTokenCloudDriverConfig.Create( 'ceb7b2ed-b0cf-43e9-ade5-2e8ed9cfa3c0', 'dc2ea085a05ac273a://onedrive/auth' );
  cloudDriverManager.register( TOneDriveClient );
  WFXCloudDriverMenuItems.add( TOneDriveClient.driverName, rsOneDriveDisplayName );

  WFXCloudDriverConfigManager.register( TBoxClient.driverName, TWFXBoxConfig );
  boxConfig:= TTokenCloudDriverConfig.Create( 'rtm1apih0scrk1we9dnzej7fezd9t2xb', 'dc2ea085a05ac273a://box/auth' );
  cloudDriverManager.register( TBoxClient );
  WFXCloudDriverMenuItems.add( TBoxClient.driverName, rsBoxDisplayName );

  WFXCloudDriverMenuItems.addSeparator;

  WFXCloudDriverConfigManager.register( TAmazonS3Client.driverName, TWFXS3AutoConfig );
  cloudDriverManager.register( TAmazonS3Client );
  WFXCloudDriverMenuItems.add( TAmazonS3Client.driverName, rsAmazonS3DisplayName );

  WFXCloudDriverConfigManager.register( TBackBlazeB2Client.driverName, TWFXS3ManualConfig );
  cloudDriverManager.register( TBackBlazeB2Client );
  WFXCloudDriverMenuItems.add( TBackBlazeB2Client.driverName, rsBackBlazeB2DisplayName );

  WFXCloudDriverConfigManager.register( TS3CompatibleClient.driverName, TWFXS3ManualConfig );
  cloudDriverManager.register( TS3CompatibleClient );
  WFXCloudDriverMenuItems.add( TS3CompatibleClient.driverName, rsS3CompatibleDisplayName );

  WFXCloudDriverMenuItems.addSeparator;

  WFXCloudDriverConfigManager.register( TAliyunOSSClient.driverName, TWFXS3AutoConfig );
  cloudDriverManager.register( TAliyunOSSClient );
  WFXCloudDriverMenuItems.add( TAliyunOSSClient.driverName, rsAliyunOSSDisplayName );

  WFXCloudDriverConfigManager.register( TTencentCOSClient.driverName, TWFXS3AutoConfig );
  cloudDriverManager.register( TTencentCOSClient );
  WFXCloudDriverMenuItems.add( TTencentCOSClient.driverName, rsTencentCOSDisplayName );

  WFXCloudDriverConfigManager.register( THuaweiOBSClient.driverName, TWFXS3AutoConfig );
  cloudDriverManager.register( THuaweiOBSClient );
  WFXCloudDriverMenuItems.add( THuaweiOBSClient.driverName, rsHuaweiOBSDisplayName );

  WFXCloudDriverMenuItems.addSeparator;

  WFXCloudDriverConfigManager.register( TQiniuKODOClient.driverName, TWFXS3AutoConfig );
  cloudDriverManager.register( TQiniuKODOClient );
  WFXCloudDriverMenuItems.add( TQiniuKODOClient.driverName, rsQiniuKODODisplayName );

  WFXCloudDriverConfigManager.register( TUpyunUSSClient.driverName, TWFXS3AutoConfig );
  cloudDriverManager.register( TUpyunUSSClient );
  WFXCloudDriverMenuItems.add( TUpyunUSSClient.driverName, rsUpyunUSSDisplayName );
end;

procedure TWFXCloudDriverConfigManager.register(const name: String;
  const config: TWFXCloudDriverConfigClass);
begin
  _configItems.Add( name, config );
end;

function TWFXCloudDriverConfigManager.get( const name: String ): TWFXCloudDriverConfigClass;
begin
  Result:= TWFXCloudDriverConfigClass( _configItems[name] );
end;

procedure TWFXCloudDriverConfigManager.loadFromSecurity;
  procedure loadConnectionsSecurity( const jsonConnections: NSArray );
  var
    jsonConnection: NSMutableDictionary;
    connectionName: String;
    connection: TWFXConnection;
    driverName: String;
    config: TWFXCloudDriverConfigClass;
  begin
    for jsonConnection in jsonConnections do begin
      connectionName:= TJsonUtil.getString( jsonConnection, 'name' );
      connection:= WFXConnectionManager.get( connectionName );
      driverName:= connection.driver.driverName;
      config:= TWFXCloudDriverConfigClass( _configItems[driverName] );
      if config = nil then
        raise Exception.Create( 'driver not found in loadFromSecurity.TCloudConfigManager(): ' + driverName );
      config.loadConnectionSecurity( connection.driver, jsonConnection );
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

procedure TWFXCloudDriverConfigManager.saveToSecurity;
  function saveConnectionsSecurity: NSArray;
  var
    json: NSMutableArray;
    jsonConnection: NSMutableDictionary;
    i: Integer;
    connections: TWFXConnections;
    connection: TWFXConnection;
    driverName: String;
    config: TWFXCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    connections:= WFXConnectionManager.connections;
    for i:=0 to connections.Count-1 do begin
      jsonConnection:= NSMutableDictionary.new;
      connection:= TWFXConnection( connections[i] );
      driverName:= connection.driver.driverName;
      TJsonUtil.setString( jsonConnection, 'name', connection.name );
      config:= TWFXCloudDriverConfigClass( _configItems[driverName] );
      if config = nil then begin
        TLogUtil.logError( 'Config Class for Driver Not Found: ' + driverName );
      end else begin
        config.saveConnectionSecurity( connection.driver, jsonConnection );
        json.addObject( jsonConnection );
      end;
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

procedure TWFXCloudDriverConfigManager.loadFromCommon( const path: String );
  procedure loadDrivers( const jsonDrivers: NSArray );
  var
    jsonDriver: NSDictionary;
    driverName: String;
    config: TWFXCloudDriverConfigClass;
  begin
    for jsonDriver in jsonDrivers do begin
      driverName:= TJsonUtil.getString( jsonDriver, 'name' );
      config:= TWFXCloudDriverConfigClass( _configItems[driverName] );
      config.loadDriverCommon( jsonDriver );
    end;
  end;

  procedure loadConnections( const jsonConnections: NSArray );
  var
    jsonConnection: NSDictionary;
    connections: TWFXConnections;
    connection: TWFXConnection;
    driverName: String;
    config: TWFXCloudDriverConfigClass;
  begin
    connections:= TWFXConnections.Create( True );
    for jsonConnection in jsonConnections do begin
      driverName:= TJsonUtil.getString( jsonConnection, 'driver' );
      connection:= TWFXConnection.Create(
        TJsonUtil.getString( jsonConnection, 'name' ),
        cloudDriverManager.createInstance( driverName ),
        TJsonUtil.getDateTime( jsonConnection, 'creationTime' ),
        TJsonUtil.getDateTime( jsonConnection, 'modificationTime' ) );
      config:= TWFXCloudDriverConfigClass( _configItems[driverName] );
      config.loadConnectionCommon( connection.driver, jsonConnection );
      connections.Add( connection );
    end;
    WFXConnectionManager.connections:= connections;
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

procedure TWFXCloudDriverConfigManager.saveToCommon(const path: String);
  function saveDrivers: NSArray;
  var
    json: NSMutableArray;
    jsonDriver: NSMutableDictionary;
    i: Integer;
    driverClasses: TCloudDriverClasses;
    driverName: String;
    config: TWFXCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    driverClasses:= cloudDriverManager.driverClasses;
    for i:=0 to driverClasses.Count-1 do begin
      jsonDriver:= NSMutableDictionary.new;
      driverName:= TCloudDriverClass(driverClasses[i]).driverName;
      TJsonUtil.setString( jsonDriver, 'name', driverName );
      config:= TWFXCloudDriverConfigClass( _configItems[driverName] );
      config.saveDriverCommon( jsonDriver );
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
    connections: TWFXConnections;
    connection: TWFXConnection;
    config: TWFXCloudDriverConfigClass;
  begin
    json:= NSMutableArray.new.autorelease;
    connections:= WFXConnectionManager.connections;
    for i:=0 to connections.Count-1 do begin
      jsonConnection:= NSMutableDictionary.new;
      connection:= TWFXConnection( connections[i] );
      TJsonUtil.setString( jsonConnection, 'name', connection.name );
      TJsonUtil.setString( jsonConnection, 'driver', connection.driver.driverName );
      TJsonUtil.setDateTime( jsonConnection, 'creationTime', connection.creationTime );
      TJsonUtil.setDateTime( jsonConnection, 'modificationTime', connection.modificationTime );
      config:= TWFXCloudDriverConfigClass( _configItems[connection.driver.driverName] );
      config.saveConnectionCommon( connection.driver, jsonConnection );
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

finalization
  FreeAndNil( WFXCloudDriverConfigManager );

end.


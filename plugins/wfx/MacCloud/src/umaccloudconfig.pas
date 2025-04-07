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
    class procedure load( const params: NSDictionary ); virtual; abstract;
    class procedure save(const params: NSMutableDictionary); virtual; abstract;
  end;

  { TDropBoxCloudDriverConfig }

  TDropBoxCloudDriverConfig = class( TCloudDriverConfig )
    class procedure load(const params: NSDictionary); override;
    class procedure save(const params: NSMutableDictionary); override;
  end;

  TCloudDriverConfigClass = class of TCloudDriverConfig;

  TCloudDriverConfigItems = TFPDataHashTable;

  { TCloudConfigManager }

  TCloudConfigManager = class
  private
    _configItems: TCloudDriverConfigItems;
  private
    procedure loadDriverConfig( const name: String; const params: NSDictionary );
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure register( const name: String; const config: TCloudDriverConfigClass );
    procedure loadDriversConfigFromConfigFile( const path: String );
    procedure saveDriversConfigToConfigFile( const path: String );
  end;

var
  macCloudDriverConfigManager: TCloudConfigManager;

implementation

{ TDropBoxCloudDriverConfig }

class procedure TDropBoxCloudDriverConfig.load(const params: NSDictionary);
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

class procedure TDropBoxCloudDriverConfig.save(const params: NSMutableDictionary);
begin
  TJsonUtil.setString( params, 'name', 'DropBox' );
  TJsonUtil.setString( params, 'clientID', dropBoxConfig.clientID );
  TJsonUtil.setString( params, 'listenURI', dropBoxConfig.listenURI );
end;

{ TCloudConfigManager }

constructor TCloudConfigManager.Create;
begin
  _configItems:= TCloudDriverConfigItems.Create;
end;

destructor TCloudConfigManager.Destroy;
begin
  _configItems.Free;
end;

procedure TCloudConfigManager.register(const name: String;
  const config: TCloudDriverConfigClass);
begin
  _configItems.Add( name, config );
end;

procedure TCloudConfigManager.loadDriverConfig(const name: String;
  const params: NSDictionary);
var
  config: TCloudDriverConfigClass;
begin
  config:= TCloudDriverConfigClass( _configItems[name] );
  config.load( params );
end;

procedure TCloudConfigManager.loadDriversConfigFromConfigFile( const path: String );
var
  jsonString: NSString;
  json: NSDictionary;
  jsonDrivers: NSArray;
  jsonDriver: NSDictionary;
  driverName: String;
begin
  jsonString:= NSString.stringWithContentsOfFile_encoding_error(
    StringToNSString(path), NSUTF8StringEncoding, nil );
  json:= TJsonUtil.parse( jsonString );
  jsonDrivers:= TJsonUtil.getArray( json, 'drivers' );
  for jsonDriver in jsonDrivers do begin
    driverName:= TJsonUtil.getString( jsonDriver, 'name' );
    self.loadDriverConfig( driverName, jsonDriver );
  end;
end;

procedure TCloudConfigManager.saveDriversConfigToConfigFile(const path: String);
var
  jsonString: String;
  jsonDrivers: NSMutableArray;
  jsonDriver: NSMutableDictionary;
  i: Integer;
  driverClasses: TCloudDriverClasses;
  driverName: String;
  config: TCloudDriverConfigClass;
begin
  jsonDrivers:= NSMutableArray.new;
  driverClasses:= cloudDriverManager.driverClasses;
  for i:=0 to driverClasses.Count-1 do begin
    jsonDriver:= NSMutableDictionary.new;
    driverName:= TCloudDriverClass(driverClasses[i]).driverName;
    config:= TCloudDriverConfigClass( _configItems[driverName] );
    config.save( jsonDriver );
    jsonDrivers.addObject( jsonDriver );
    jsonDriver.release;
  end;
  jsonString:= TJsonUtil.dumps(
    ['drivers',jsonDrivers],
    False,
    NSJSONWritingWithoutEscapingSlashes or NSJSONWritingPrettyPrinted );
  jsonDrivers.release;

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


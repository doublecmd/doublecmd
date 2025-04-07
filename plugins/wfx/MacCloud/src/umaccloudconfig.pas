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
  end;

  TDropBoxCloudDriverConfig = class( TCloudDriverConfig )
    class procedure load(const params: NSDictionary); override;
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


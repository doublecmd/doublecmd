unit uiCloudDriverConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Dialogs,
  uGlobsPaths, uLng, DCJsonConfig, DCOSUtils;


type
  TiCloudDriverConfigPath = record
    base: String;
    driver: String;
    container: String;
  end;

  TiCloudDriverConfigIcon = record
    main: String;
    download: String;
  end;

  TiCloudDriverConfigAppItem = record
    name: String;
    app: String;
  end;

  TiCloudDriverConfig = record
    scheme: String;
    path: TiCloudDriverConfigPath;
    icon: TiCloudDriverConfigIcon;
    apps: Array of TiCloudDriverConfigAppItem;
  end;

  { iCloudDriverConfigUtil }

  iCloudDriverConfigUtil = class
  public
    class procedure load;
    class procedure save;
  end;


var
  iCloudDriverConfig: TiCloudDriverConfig;

implementation

const
  defaultiCloudDriverConfig: TiCloudDriverConfig = (
    scheme: 'iCloud://';
    path: (
      base: '~/Library/Mobile Documents';
      driver: '~/Library/Mobile Documents/com~apple~CloudDocs';
      container: '~/Library/Application Support/CloudDocs/session/containers'
    );
    icon: (
      main: '$COMMANDER_PATH/pixmaps/macOS/cloud.fill.png';
      download: '$COMMANDER_PATH/pixmaps/macOS/icloud.and.arrow.down.png'
    );
    apps: (
      ( name: 'Pages'; app: 'com~apple~Pages' ),
      ( name: 'Numbers'; app: 'com~apple~Numbers' ),
      ( name: 'Keynote'; app: 'com~apple~Keynote' ),
      ( name: 'ScriptEditor'; app: 'com~apple~ScriptEditor2' ),
      ( name: 'workflows'; app: 'iCloud~is~workflow~my~workflows' ),
      ( name: 'Playgrounds'; app: 'iCloud~com~apple~Playgrounds' ),
      ( name: 'iThoughts'; app: 'iCloud~com~toketaware~ios~ithoughts' ),
      ( name: 'xmind'; app: 'iCloud~net~xmind~brownieapp' )
    );
  );

const
  JSON_FILENAME = 'iCloud.json';

function getJsonPath: String;
begin
  Result:= gpCfgDir + JSON_FILENAME;
end;

procedure loadiCloudConfigFromJson( json: TJSONObject );
var
  jsonPath: TJSONObject;
  jsonIcon: TJSONObject;
  jsonApps: TJSONArray;
  jsonApp: TJSONObject;
  i: Integer;
begin
  iCloudDriverConfig.scheme:= json.Get( 'scheme', iCloudDriverConfig.scheme );

  jsonPath:= json.Get( 'path', TJSONObject(nil) );
  if jsonPath <> nil then begin
    iCloudDriverConfig.path.base:= jsonPath.Get( 'base', iCloudDriverConfig.path.base );
    iCloudDriverConfig.path.driver:= jsonPath.Get( 'driver', iCloudDriverConfig.path.driver );
    iCloudDriverConfig.path.container:= jsonPath.Get( 'container', iCloudDriverConfig.path.container );
  end;

  jsonIcon:= json.Get( 'icon', TJSONObject(nil) );
  if jsonIcon <> nil then begin
    iCloudDriverConfig.icon.main:= jsonIcon.Get( 'main', iCloudDriverConfig.icon.main );
    iCloudDriverConfig.icon.download:= jsonIcon.Get( 'download', iCloudDriverConfig.icon.download );
  end;

  jsonApps:= json.Get( 'apps', TJSONArray(nil) );
  if jsonApps <> nil then begin
    SetLength( iCloudDriverConfig.apps, jsonApps.Count );
    for i:=0 to jsonApps.Count -1 do begin
      jsonApp:= TJSONObject( jsonApps[i] );
      iCloudDriverConfig.apps[i].name:= jsonApp.Get( 'name', '' );
      iCloudDriverConfig.apps[i].app:= jsonApp.Get( 'app', '' );
    end;
  end;
end;

procedure saveiCloudConfigToJson( json: TJSONObject );
var
  jsonPath: TJSONObject;
  jsonIcon: TJSONObject;
  jsonApps: TJSONArray;
  jsonApp: TJSONObject;
  i: Integer;
begin
  json.Add( 'scheme', iCloudDriverConfig.scheme );

  jsonPath:= TJSONObject.Create;
  jsonPath.Add( 'base', iCloudDriverConfig.path.base );
  jsonPath.Add( 'driver', iCloudDriverConfig.path.driver );
  jsonPath.Add( 'container', iCloudDriverConfig.path.container );
  json.Add( 'path', jsonPath );

  jsonIcon:= TJSONObject.Create;
  jsonIcon.Add( 'main', iCloudDriverConfig.icon.main );
  jsonIcon.Add( 'download', iCloudDriverConfig.icon.download );
  json.Add( 'icon', jsonIcon );

  jsonApps:= TJSONArray.Create;
  for i:=0 to Length(iCloudDriverConfig.apps) - 1 do begin
    jsonApp:= TJSONObject.Create;
    jsonApp.Add( 'name', iCloudDriverConfig.apps[i].name );
    jsonApp.Add( 'app', iCloudDriverConfig.apps[i].app );
    jsonApps.Add( jsonApp );
  end;
  json.Add( 'apps', jsonApps );
end;

procedure loadiCloudConfig;
var
  config: TJsonConfig;
begin
  config:= TJsonConfig.Create;
  try
    try
      config.LoadFromFile( getJsonPath );
      if config.Root = nil then
        Exit;

      loadiCloudConfigFromJson( config.Root );
    except
    end;
  finally
    FreeAndNil( config );
  end;
end;

{ iCloudDriverConfigUtil }

class procedure iCloudDriverConfigUtil.load;
begin
  iCloudDriverConfig:= defaultiCloudDriverConfig;
  if mbFileExists(getJsonPath) then
    loadiCloudConfig
end;

class procedure iCloudDriverConfigUtil.save;
var
  config: TJsonConfig;
begin
  config:= TJsonConfig.Create;
  try
    saveiCloudConfigToJson( config.Root );
    config.SaveToFile( getJsonPath );
  finally
    FreeAndNil( config );
  end;
end;

end.


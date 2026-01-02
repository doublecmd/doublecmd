unit uiCloudDriveConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Dialogs,
  uGlobsPaths, DCJsonConfig, DCOSUtils;

type

  TiCloudDriveConfigPath = record
    base: String;
    drive: String;
    container: String;
  end;

  TiCloudDriveConfigIcon = record
    main: String;
    download: String;
  end;

  TiCloudDriveConfigAppItem = record
    name: String;
    app: String;
  end;

  TiCloudDriveConfig = record
    scheme: String;
    path: TiCloudDriveConfigPath;
    icon: TiCloudDriveConfigIcon;
    apps: Array of TiCloudDriveConfigAppItem;
  end;

  { iCloudDriveConfigUtil }

  iCloudDriveConfigUtil = class
  public
    class procedure load;
    class procedure save;
  end;


var
  iCloudDriveConfig: TiCloudDriveConfig;

implementation

const
  defaultiCloudDriveConfig: TiCloudDriveConfig = (
    scheme: 'iCloud://';
    path: (
      base: '~/Library/Mobile Documents';
      drive: '~/Library/Mobile Documents/com~apple~CloudDocs';
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
  iCloudDriveConfig.scheme:= json.Get( 'scheme', iCloudDriveConfig.scheme );

  jsonPath:= json.Get( 'path', TJSONObject(nil) );
  if jsonPath <> nil then begin
    iCloudDriveConfig.path.base:= jsonPath.Get( 'base', iCloudDriveConfig.path.base );
    iCloudDriveConfig.path.drive:= jsonPath.Get( 'drive', iCloudDriveConfig.path.drive );
    iCloudDriveConfig.path.container:= jsonPath.Get( 'container', iCloudDriveConfig.path.container );
  end;

  jsonIcon:= json.Get( 'icon', TJSONObject(nil) );
  if jsonIcon <> nil then begin
    iCloudDriveConfig.icon.main:= jsonIcon.Get( 'main', iCloudDriveConfig.icon.main );
    iCloudDriveConfig.icon.download:= jsonIcon.Get( 'download', iCloudDriveConfig.icon.download );
  end;

  jsonApps:= json.Get( 'apps', TJSONArray(nil) );
  if jsonApps <> nil then begin
    SetLength( iCloudDriveConfig.apps, jsonApps.Count );
    for i:=0 to jsonApps.Count -1 do begin
      jsonApp:= TJSONObject( jsonApps[i] );
      iCloudDriveConfig.apps[i].name:= jsonApp.Get( 'name', '' );
      iCloudDriveConfig.apps[i].app:= jsonApp.Get( 'app', '' );
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
  json.Add( 'scheme', iCloudDriveConfig.scheme );

  jsonPath:= TJSONObject.Create;
  jsonPath.Add( 'base', iCloudDriveConfig.path.base );
  jsonPath.Add( 'drive', iCloudDriveConfig.path.drive );
  jsonPath.Add( 'container', iCloudDriveConfig.path.container );
  json.Add( 'path', jsonPath );

  jsonIcon:= TJSONObject.Create;
  jsonIcon.Add( 'main', iCloudDriveConfig.icon.main );
  jsonIcon.Add( 'download', iCloudDriveConfig.icon.download );
  json.Add( 'icon', jsonIcon );

  jsonApps:= TJSONArray.Create;
  for i:=0 to Length(iCloudDriveConfig.apps) - 1 do begin
    jsonApp:= TJSONObject.Create;
    jsonApp.Add( 'name', iCloudDriveConfig.apps[i].name );
    jsonApp.Add( 'app', iCloudDriveConfig.apps[i].app );
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

{ iCloudDriveConfigUtil }

class procedure iCloudDriveConfigUtil.load;
begin
  iCloudDriveConfig:= defaultiCloudDriveConfig;
  if mbFileExists(getJsonPath) then
    loadiCloudConfig
end;

class procedure iCloudDriveConfigUtil.save;
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


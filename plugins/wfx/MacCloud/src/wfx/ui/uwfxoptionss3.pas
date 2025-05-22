unit uWFXOptionsS3;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uAWSCore, uS3Client,
  uWFXPlugin, uWFXUtil,
  uWFXOptionsCore, uWFXOptionsCommonRS,
  uMiniUtil;

type

  { TWFXS3RegionConfigItem }

  TWFXS3RegionConfigItem = objcclass( NSObject )
  private
    _regionName: NSString;
    _displayName: NSString;
    _endPoint: NSString;
  public
    function initWithData(
      const aRegionName: NSString;
      const aDisplayName: NSString;
      const aEndPoint: NSString ): id; message 'TWFXS3RegionConfigItem_initWithData:::';
    procedure dealloc; override;
    function regionName: NSString; message 'TWFXS3RegionConfigItem_regionName';
    function displayName: NSString; message 'TWFXS3RegionConfigItem_displayName';
    function endPoint: NSString; message 'TWFXS3RegionConfigItem_endPoint';
  end;

  { TWFXS3RegionConfigItems }

  TWFXS3RegionConfigItems = class
  private
    _items: NSMutableArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure addItem( const item: TWFXS3RegionConfigItem );
    function getItem( const index: Integer ): TWFXS3RegionConfigItem;
    function count: Integer;
    function indexOfRegion( const region: NSString ): Integer;
  end;

  { TWFXS3PropertyView }

  TWFXS3PropertyView = objcclass( TWFXPropertyView )
  protected
    _regionItems: TWFXS3RegionConfigItems;
    _regionDropDown: NSPopUpButton;
    _regionTextField: NSTextField;
    _endPointTextField: NSTextField;
    _accessKeyIDTextField: NSTextField;
    _accessKeySecretTextField: NSTextField;
    _accessKeyTokenTextField: NSTextField;
    _accessKeySecretPlainTextField: NSTextField;
    _bucketTextField: NSTextField;
    _secretButton: NSButton;
  private
    procedure saveConnection( sender: NSObject ); message 'TWFXS3PropertyView_saveConnection:';
    procedure initPropertyView; message 'TWFXS3PropertyView_initPropertyView';
    procedure togglePassword( sender: NSObject ); message 'TWFXS3PropertyView_togglePassword:';
    procedure loadRegionItems( const driver: TAWSCloudDriver ); message 'TWFXS3PropertyView_loadRegionItems:';
    procedure regionDropDownChanged( sender: NSObject ); message 'TWFXS3PropertyView_regionDropDownChanged:';
  public
    procedure loadConnectionProperties( const index: Integer ); override;
    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
  end;

implementation

resourcestring
  rsAccessKeyIDLabel = 'Access Key ID:';
  rsSecretAccessKeyLabel = 'Secret Access Key:';
  rsTemporaryTokenLabel = 'Temporary Token:';
  rsRegionListLabel = 'Region List:';
  rsRegionLabel = 'Region:';
  rsBucketLabel = 'Bucket:';
  rsEndpointLabel = 'Endpoint:';
  rsRegionUserCustom = '(User Custom)';
  rsParamsAlertTitle = 'Incomplete Parameters';
  rsParamsAlertText = 'Access Key ID and Secret Access Key are required, please make sure they are correct. If permissions are insufficient or you are setting "S3 Compatible", Region / Endpoint / Bucket is also required.';

{ TWFXS3RegionConfigItem }

function TWFXS3RegionConfigItem.initWithData(
  const aRegionName: NSString;
  const aDisplayName: NSString;
  const aEndPoint: NSString): id;
begin
  Result:= Inherited init;
  _regionName:= aRegionName.retain;
  _displayName:= aDisplayName.retain;
  _endPoint:= aEndPoint.retain;
end;

procedure TWFXS3RegionConfigItem.dealloc;
begin
  _regionName.release;
  _displayName.release;
  _endPoint.release;
end;

function TWFXS3RegionConfigItem.regionName: NSString;
begin
  Result:= _regionName;
end;

function TWFXS3RegionConfigItem.displayName: NSString;
begin
  Result:= _displayName;
end;

function TWFXS3RegionConfigItem.endPoint: NSString;
begin
  Result:= _endPoint;
end;

{ TWFXS3RegionConfigItems }

constructor TWFXS3RegionConfigItems.Create;
begin
  _items:= NSMutableArray.new;
end;

destructor TWFXS3RegionConfigItems.Destroy;
begin
  _items.release;
end;

procedure TWFXS3RegionConfigItems.addItem( const item: TWFXS3RegionConfigItem );
begin
  _items.addObject( item );
end;

function TWFXS3RegionConfigItems.getItem( const index: Integer ): TWFXS3RegionConfigItem;
begin
  Result:= TWFXS3RegionConfigItem( _items.objectAtIndex(index) );
end;

function TWFXS3RegionConfigItems.count: Integer;
begin
  Result:= _items.count;
end;

function TWFXS3RegionConfigItems.indexOfRegion( const region: NSString ): Integer;
var
  item: TWFXS3RegionConfigItem;
  i: Integer;
begin
  for i:=0 to _items.Count-1 do begin
    item:= TWFXS3RegionConfigItem( _items.objectAtIndex(i) );
    if item.regionName.isEqualToString(region) then
      Exit( i );
  end;
  Result:= -1;     // (User Custom)
end;

{ TWFXS3PropertyView }

procedure TWFXS3PropertyView.loadRegionItems( const driver: TAWSCloudDriver );
var
  region: TWFXS3RegionConfigItem;
  path: String;
  jsonString: NSString;
  json: NSDictionary;
  jsonRegions: NSArray;
  jsonRegion: NSDictionary;
begin
  path:= TWFXPluginUtil.driverDataPath(driver) + 'regions.json';
  path:= TFileUtil.pathWithLanguageID( path, WFXMacCloudPlugin.languageID );
  jsonString:= TFileUtil.contentAsUTF8String( path );
  if jsonString.length = 0 then
    Exit;

  json:= TJsonUtil.parse( jsonString );
  jsonRegions:= TJsonUtil.getArray( json, 'regions' );
  for jsonRegion in jsonRegions do begin
    region:= TWFXS3RegionConfigItem.alloc.initWithData(
      TJsonUtil.getNSString(jsonRegion, 'name'),
      TJsonUtil.getNSString(jsonRegion, 'displayName'),
      TJsonUtil.getNSString(jsonRegion, 'endPoint') );
    _regionItems.addItem( region );
    _regionDropDown.addItemWithTitle( region.displayName );
    region.release;
  end;
end;

procedure TWFXS3PropertyView.regionDropDownChanged(sender: NSObject);
var
  index: Integer;
  configItem: TWFXS3RegionConfigItem;
begin
  index:= _regionDropDown.indexOfSelectedItem - 1;
  if (index<0) OR (index>=_regionItems.Count) then
    Exit;

  configItem:= _regionItems.getItem( index );
  _regionTextField.setStringValue( configItem.regionName );
  _endPointTextField.setStringValue( configItem.endPoint );
end;

procedure TWFXS3PropertyView.loadConnectionProperties( const index: Integer );
var
  configItem: TWFXConnectionConfigItem;
  client: TAWSCloudDriver;
  connectionData: TAWSConnectionData;
  accessKey: TAWSAccessKey;
  regionIndex: Integer;
begin
  configItem:= _controller.currentConfigItem;
  if configItem = nil then
    Exit;
  client:= TAWSCloudDriver( configItem.driver );

  _regionDropDown.addItemWithTitle( StringToNSString(rsRegionUserCustom) );
  self.loadRegionItems( client );
  if _regionDropDown.itemArray.count = 1 then
    _regionDropDown.setEnabled( False );

  connectionData:= client.getDefaultConnectionData;
  accessKey:= client.getAccessKey;
  _logoImageView.setImage( TWFXPluginUtil.driverMainIcon(configItem.driver) );
  _nameTextField.setStringValue( configItem.name );
  _regionTextField.setStringValue( StringToNSString(connectionData.region) );
  _endPointTextField.setStringValue( StringToNSString(connectionData.endPoint) );
  _accessKeyIDTextField.setStringValue( StringToNSString(accessKey.id) );
  _accessKeySecretTextField.setStringValue( StringToNSString(accessKey.secret) );
  _accessKeyTokenTextField.setStringValue( StringToNSString(accessKey.token) );
  _bucketTextField.setStringValue( StringToNSString(connectionData.bucketName) );
  regionIndex:= _regionItems.indexOfRegion( _regionTextField.stringValue );
  if regionIndex >= 0 then
    _regionDropDown.selectItemAtIndex( regionIndex + 1 );
  _noteTextView.setString( configItem.getNotes );
end;

procedure TWFXS3PropertyView.saveConnection(sender: NSObject);
var
  configItem: TWFXConnectionConfigItem;
  client: TS3Client;
  data: TAWSConnectionData;
  accessKey: TAWSAccessKey;

  procedure checkConnectionParams;
  var
    count: Integer;
    alert: NSAlert;
  begin
    try
      count:= client.getAllBuckets.Count;
      if count > 0 then
        Exit;
    except
      on e: Exception do
        TLogUtil.logError( 'in TWFXS3PropertyView.saveConnection: ' + e.Message );
    end;

    alert:= NSAlert.new;
    alert.setMessageText( StringToNSString(rsParamsAlertTitle) );
    alert.setInformativeText( StringToNSString(rsParamsAlertText) );
    alert.addButtonWithTitle( StringToNSString(rsOkButtonTitle) );
    alert.runModal;
    alert.release;
  end;

begin
  configItem:= _controller.currentConfigItem;
  if configItem = nil then
    Exit;

  if _secretButton.state = NSOnState then
    _accessKeySecretTextField.setStringValue( _accessKeySecretPlainTextField.stringValue );

  client:= TS3Client( configItem.driver );

  data.region:= TStringUtil.removeSpace( _regionTextField.stringValue );
  data.endPoint:= TStringUtil.removeSpace( _endPointTextField.stringValue );
  data.bucketName:= TStringUtil.removeSpace( _bucketTextField.stringValue );
  client.setDefaultConnectionData( data );

  accessKey:= TAWSAccessKey.Create(
    TStringUtil.removeSpace( _accessKeyIDTextField.stringValue ),
    TStringUtil.removeSpace( _accessKeySecretTextField.stringValue ),
    TStringUtil.removeSpace( _accessKeyTokenTextField.stringValue ) );
  client.setAccessKey( accessKey );

  _controller.saveConnection( _nameTextField.stringValue );
  checkConnectionParams;
end;

procedure TWFXS3PropertyView.initPropertyView;
begin
  _logoImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(224,560,32,32) );
  self.addSubview( _logoImageView );
  _logoImageView.release;

  addLabel( StringToNSString(rsNameLabel), NSMakeRect(20,510,120,20) );
  _nameTextField:= addTextField( NSMakeRect(146,510,290,22) );

  addLabel( StringToNSString(rsAccessKeyIDLabel), NSMakeRect(20,470,120,20) );
  _accessKeyIDTextField:= addTextField( NSMakeRect(146,470,290,22) );

  addLabel( StringToNSString(rsSecretAccessKeyLabel), NSMakeRect(20,430,120,20) );
  _accessKeySecretTextField:= NSSecureTextField.alloc.initWithFrame( NSMakeRect(146,430,290,22) );
  _accessKeySecretTextField.cell.setScrollable( True );
  _accessKeySecretTextField.cell.setWraps( False );
  self.addSubview( _accessKeySecretTextField );
  _accessKeySecretTextField.release;
  _accessKeySecretPlainTextField:= addTextField( NSMakeRect(146,430,290,22) );
  _accessKeySecretPlainTextField.setHidden( True );

  _secretButton:= NSButton.alloc.initWithFrame( NSMakeRect(445,433,16,16) );
  _secretButton.setButtonType( NSToggleButton );
  _secretButton.setImage( NSImage.imageNamed( NSImageNameQuickLookTemplate ));
  _secretButton.setBordered( False );
  _secretButton.setTarget( self );
  _secretButton.setAction( ObjCSelector('TWFXS3PropertyView_togglePassword:') );
  self.addSubview( _secretButton );
  _secretButton.release;

  addLabel( StringToNSString(rsTemporaryTokenLabel), NSMakeRect(20,390,120,20) );
  _accessKeyTokenTextField:= addTextField( NSMakeRect(146,390,290,22) );

  addLabel( StringToNSString(rsRegionListLabel), NSMakeRect(20,350,120,20) );
  _regionDropDown:= NSPopUpButton.alloc.initWithFrame( NSMakeRect(146,350,290,22) );
  _regionDropDown.setTarget( self );
  _regionDropDown.setAction( ObjCSelector('TWFXS3PropertyView_regionDropDownChanged:') );
  self.addSubview( _regionDropDown );
  _regionDropDown.release;

  addLabel( StringToNSString(rsRegionLabel), NSMakeRect(20,310,120,20) );
  _regionTextField:= addTextField( NSMakeRect(146,310,290,22) );

  addLabel( StringToNSString(rsEndpointLabel), NSMakeRect(20,270,120,20) );
  _endPointTextField:= addTextField( NSMakeRect(146,270,290,22) );

  addLabel( StringToNSString(rsBucketLabel), NSMakeRect(20,230,120,20) );
  _bucketTextField:= addTextField( NSMakeRect(146,230,290,22) );

  _saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(200,190,100,22) );
  _saveButton.setBezelStyle( NSRoundedBezelStyle );
  _saveButton.setTitle( StringToNSString(rsSaveButtonTitle) );
  _saveButton.setTarget( self );
  _saveButton.setAction( ObjCSelector('TWFXS3PropertyView_saveConnection:') );
  _saveButton.setKeyEquivalent( NSSTR(#13) );
  self.addSubView( _saveButton );
  _saveButton.release;

  _noteTextView:= NSTextView.alloc.initWithFrame( NSMakeRect(20,50,440,100) );
  _noteTextView.setFont( NSFont.systemFontOfSize(11));
  _noteTextView.setEditable( False );
  _noteTextView.setDrawsBackground( False );
  self.addSubView( _noteTextView );
  _noteTextView.release;
end;

procedure TWFXS3PropertyView.togglePassword(sender: NSObject);
begin
  if _secretButton.state = NSOnState then begin
    _accessKeySecretTextField.setHidden( True );
    _accessKeySecretPlainTextField.setHidden( False );
    _accessKeySecretPlainTextField.setStringValue( _accessKeySecretTextField.stringValue );
  end else begin
    _accessKeySecretTextField.setHidden( False );
    _accessKeySecretPlainTextField.setHidden( True );
    _accessKeySecretTextField.setStringValue( _accessKeySecretPlainTextField.stringValue );
  end;
end;

function TWFXS3PropertyView.initWithFrame(frameRect: NSRect): id;
begin
  Result:= inherited;
  _regionItems:= TWFXS3RegionConfigItems.Create;
  self.initPropertyView;
end;

procedure TWFXS3PropertyView.dealloc;
begin
  FreeAndNil( _regionItems );
end;

end.

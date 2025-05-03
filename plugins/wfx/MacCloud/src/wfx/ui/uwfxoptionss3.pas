unit uWFXOptionsS3;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uAWSCore,
  uWFXPlugin, uWFXUtil, uWFXOptionsCore,
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

const
  CONST_AUTH_NOTES =
    '1. AccessKeyID and SerectAccessKey will be saved in the macOS KeyChains to obtain system-level security.'#13#13 +
    '2. The confidential information can only be read by your own macOS permissions.';

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
  Result:= _items.Count;     // (User Custom)
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
  index:= _regionDropDown.indexOfSelectedItem;
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

  self.loadRegionItems( client );
  if _regionDropDown.itemArray.count = 0 then
    _regionDropDown.setEnabled( False );
  _regionDropDown.addItemWithTitle( StringToNSString('(User Custom)') );

  connectionData:= client.getConnectionData;
  accessKey:= client.getAccessKey;
  _logoImageView.setImage( TWFXPluginUtil.driverMainIcon(configItem.driver) );
  _nameTextField.setStringValue( configItem.name );
  _regionTextField.setStringValue( StringToNSString(connectionData.region) );
  _endPointTextField.setStringValue( StringToNSString(connectionData.endPoint) );
  _accessKeyIDTextField.setStringValue( StringToNSString(accessKey.id) );
  _accessKeySecretTextField.setStringValue( StringToNSString(accessKey.secret) );
  _bucketTextField.setStringValue( StringToNSString(connectionData.defaultBucket) );
  regionIndex:= _regionItems.indexOfRegion( _regionTextField.stringValue );
  if regionIndex >= 0 then
    _regionDropDown.selectItemAtIndex( regionIndex );
end;

procedure TWFXS3PropertyView.saveConnection(sender: NSObject);
var
  configItem: TWFXConnectionConfigItem;
  client: TAWSCloudDriver;
  data: TAWSConnectionData;
  accessKey: TAWSAccessKey;
begin
  configItem:= _controller.currentConfigItem;
  if configItem = nil then
    Exit;

  if _secretButton.state = NSOnState then
    _accessKeySecretTextField.setStringValue( _accessKeySecretPlainTextField.stringValue );

  client:= TAWSCloudDriver( configItem.driver );

  data.region:= _regionTextField.stringValue.UTF8String;
  data.endPoint:= _endPointTextField.stringValue.UTF8String;
  data.defaultBucket:= _bucketTextField.stringValue.UTF8String;;
  client.setConnectionData( data );

  accessKey:= TAWSAccessKey.Create(
    _accessKeyIDTextField.stringValue.UTF8String,
    _accessKeySecretTextField.stringValue.UTF8String );
  client.setAccessKey( accessKey );

  _controller.saveConnection( _nameTextField.stringValue );
end;

procedure TWFXS3PropertyView.initPropertyView;

  procedure addLabel( const title: String; const rect: NSRect );
  var
    nsLabel: NSTextField;
  begin
    nsLabel:= NSTextField.alloc.initWithFrame( rect );
    nsLabel.setEditable( False );
    nsLabel.setDrawsBackground( False );
    nsLabel.setBordered( False );
    nsLabel.setStringValue( StringToNSString(title) );
    nsLabel.setAlignment( 2 );
    self.addSubview( nsLabel );
    nsLabel.release;
  end;

begin
  _logoImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(200,530,32,32) );
  self.addSubview( _logoImageView );
  _logoImageView.release;

  addLabel( 'Name:', NSMakeRect(20,480,120,20) );
  _nameTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,480,250,22) );
  self.addSubview( _nameTextField );
  _nameTextField.release;

  addLabel( 'Region List:', NSMakeRect(20,440,120,20) );
  _regionDropDown:= NSPopUpButton.alloc.initWithFrame( NSMakeRect(146,440,250,22) );
  _regionDropDown.setTarget( self );
  _regionDropDown.setAction( ObjCSelector('TWFXS3PropertyView_regionDropDownChanged:') );
  self.addSubview( _regionDropDown );
  _regionDropDown.release;

  addLabel( 'Region:', NSMakeRect(20,400,120,20) );
  _regionTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,400,250,22) );
  self.addSubview( _regionTextField );
  _regionTextField.release;

  addLabel( 'Endpoint:', NSMakeRect(20,360,120,20) );
  _endPointTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,360,250,22) );
  self.addSubview( _endPointTextField );
  _endPointTextField.release;

  addLabel( 'Access Key ID:', NSMakeRect(20,320,120,20) );
  _accessKeyIDTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,320,250,22) );
  self.addSubview( _accessKeyIDTextField );
  _accessKeyIDTextField.release;

  addLabel( 'Serect Access Key:', NSMakeRect(20,280,120,20) );
  _accessKeySecretTextField:= NSSecureTextField.alloc.initWithFrame( NSMakeRect(146,280,250,22) );
  self.addSubview( _accessKeySecretTextField );
  _accessKeySecretTextField.release;
  _accessKeySecretPlainTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,280,250,22) );
  _accessKeySecretPlainTextField.setHidden( True );
  self.addSubview( _accessKeySecretPlainTextField );
  _accessKeySecretPlainTextField.release;

  _secretButton:= NSButton.alloc.initWithFrame( NSMakeRect(405,283,16,16) );
  _secretButton.setButtonType( NSToggleButton );
  _secretButton.setImage( NSImage.imageNamed( NSImageNameQuickLookTemplate ));
  _secretButton.setBordered( False );
  _secretButton.setTarget( self );
  _secretButton.setAction( ObjCSelector('TWFXS3PropertyView_togglePassword:') );
  self.addSubview( _secretButton );
  _secretButton.release;

  addLabel( 'Bucket:', NSMakeRect(20,240,120,20) );
  _bucketTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,240,250,22) );
  self.addSubview( _bucketTextField );
  _bucketTextField.release;

  _saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(200,200,100,22) );
  _saveButton.setBezelStyle( NSRoundedBezelStyle );
  _saveButton.setTitle( NSSTR('Save') );
  _saveButton.setTarget( self );
  _saveButton.setAction( ObjCSelector('TWFXS3PropertyView_saveConnection:') );
  self.addSubView( _saveButton );
  _saveButton.release;

  _noteTextView:= NSTextView.alloc.initWithFrame( NSMakeRect(20,50,400,50) );
  _noteTextView.setFont( NSFont.systemFontOfSize(11));
  _noteTextView.setEditable( False );
  _noteTextView.setDrawsBackground( False );
  _noteTextView.setString( StringToNSString(CONST_AUTH_NOTES) );
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

unit uWFXOptions;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uWFXPlugin, uWFXUtil,
  uMiniUtil;

type

  { TWFXOptionsUtil }

  TWFXOptionsUtil = class
  private
    class function createWindow: NSWindow;
  public
    class procedure show( const connectionName: String );
    class procedure addAndShow( const connectionName: String = '' );
  end;

implementation

const
  CONST_AUTH_NOTES =
    '1. Before successfully enabling the link, Double Command needs to obtain authorization from {driverName}'#13#13 +
    '2. Click the connect button to be redirected to the {driverName} official website in the Safari browser'#13#13 +
    '3. Please login your {driverName} account in Safari and authorize Double Commander to access'#13#13 +
    '4. The authorization is completed on the {driverName} official website, Double Command will not get your password';

type

  { TWFXConnectionConfigItem }

  TWFXConnectionConfigItem = objcclass( NSObject )
  public
    _name: NSString;
    _creating: Boolean;
    _driver: TCloudDriver;
    _creationTime: TDateTime;
    _modificationTime: TDateTime;
  public
    procedure dealloc; override;
    procedure setName( name: NSString );
      message 'TConnectionConfigItem_setName:';
    procedure setCreating( creating: Boolean );
      message 'TConnectionConfigItem_setCreating:';
    procedure setDriver( driver: TCloudDriver );
      message 'TConnectionConfigItem_setDriver:';
    procedure setCreationTime( creationTime: TDateTime );
      message 'TConnectionConfigItem_setCreationTime:';
    procedure setModificationTime( modificationTime: TDateTime );
      message 'TConnectionConfigItem_setModificationTime:';
    function name: NSString;
      message 'TConnectionConfigItem_Name';
    function creating: Boolean;
      message 'TConnectionConfigItem_creating';
    function driver: TCloudDriver;
      message 'TConnectionConfigItem_driver';
    function creationTime: TDateTime;
      message 'TConnectionConfigItem_creationTime';
    function modificationTime: TDateTime;
      message 'TConnectionConfigItem_modificationTime';
  end;

  { TWFXConnectionConfigItems }

  TWFXConnectionConfigItems = class
  private
    _items: NSMutableArray;
  public
    constructor Create;
    destructor Destroy; override;
    function indexOf( const item: TWFXConnectionConfigItem ): Integer; overload;
    function indexOf( const name: NSString ): Integer; overload;
    function addItem( const item: TWFXConnectionConfigItem ): Integer;
    function getItem( const index: Integer ): TWFXConnectionConfigItem;
    procedure removeItemAtIndex( const index: Integer );
    function Count: Integer;
  end;

  { TWFXConfigItemsController }

  TWFXConfigItemsController = objcprotocol
    function getConfigItems: TWFXConnectionConfigItems; message 'TCloudConfigItemsController_getConfigItems';
    procedure newConnection( sender: NSObject ); message 'TCloudConfigItemsController_newConnection:';
    procedure removeConnection( sender: NSObject ); message 'TCloudConfigItemsController_removeConnection:';
    procedure saveConnection( sender: NSObject ); message 'TCloudConfigItemsController_saveConnection:';
    procedure connectOrDisconnect( sender: NSObject ); message 'TCloudConfigItemsController_connectOrDisconnect:';
    function currentConfigItem: TWFXConnectionConfigItem; message 'TCloudConfigItemsController_currentConfigItem';
    procedure onSelectedConnectionChanged( const selectedIndex: Integer );
      message 'TCloudConfigItemsController_onSelectedConnectionChanged:';
  end;

  { TWFXConnectionListView }

  TWFXConnectionListView = objcclass(
    NSTableView,
    NSTableViewDataSourceProtocol,
    NSTableViewDelegateProtocol )
  private
    controller: TWFXConfigItemsController;
  public
    function numberOfRowsInTableView (tableView: NSTableView): NSInteger;
    function tableView_viewForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
    procedure tableViewSelectionDidChange (notification: NSNotification);
  end;

  { TWFXPropertyView }

  TWFXPropertyView = objcclass( NSView )
  private
    controller: TWFXConfigItemsController;
    logoImageView: NSImageView;
    nameTextField: NSTextField;
    connectButton: NSButton;
    statusImageview: NSImageView;
    noteTextView: NSTextView;
  public
    procedure loadConnectionProperties( const index: Integer ); message 'TPropertyView_loadConnectionProperties:';
    procedure updateConnectStatus; message 'TPropertyView_updateConnectStatus';
  end;

  { TWFXOptionsWindow }

  TWFXOptionsWindow = objcclass(
    NSWindow,
    NSWindowDelegateProtocol,
    TWFXConfigItemsController )
  private
    configItems: TWFXConnectionConfigItems;
    connectionListView: TWFXConnectionListView;
    propertyView: TWFXPropertyView;
  public
    procedure dealloc; override;
    function getConfigItems: TWFXConnectionConfigItems;
    procedure addConnection( connectionName: NSString ); message 'TCloudOptionsWindow_addConnection:';
    procedure loadConnections; message 'TCloudOptionsWindow_loadConnections';
    procedure saveConnections; message 'TCloudOptionsWindow_saveConnections';
    procedure selectConnection( name: NSString ); message 'TCloudOptionsWindow_selectConnection:';
    procedure newConnection( sender: NSObject );
    procedure removeConnection( sender: NSObject );
    procedure saveConnection( sender: NSObject );
    procedure connectOrDisconnect( sender: NSObject );
    function currentConfigItem: TWFXConnectionConfigItem;
    procedure onSelectedConnectionChanged( const selectedIndex: Integer );
  public
    procedure windowWillClose (notification: NSNotification);
    procedure cancelOperation(sender: id); override;
    function performKeyEquivalent(theEvent: NSEvent): ObjCBOOL; override;
  end;

  { TWFXSelectDriverWindow }

  TWFXSelectDriverWindow = objcclass(
    NSWindow,
    NSWindowDelegateProtocol )
  public
    driversDropDown: NSPopUpButton;
    selectedIndex: Integer;
  public
    procedure windowWillClose (notification: NSNotification);
    procedure cancelOperation(sender: id); override;
    function performKeyEquivalent(theEvent: NSEvent): ObjCBOOL; override;
  public
    class function showModal: Integer; message 'TSelectDriverWindow_showModal';
  end;

{ TWFXSelectDriverWindow }

procedure TWFXSelectDriverWindow.windowWillClose(notification: NSNotification);
begin
  if self.selectedIndex = 0 then
    self.selectedIndex:= self.driversDropDown.indexOfSelectedItem;
  NSApplication(NSAPP).stopModal;
end;

procedure TWFXSelectDriverWindow.cancelOperation(sender: id);
begin
  self.selectedIndex:= -1;
  self.close;
end;

function TWFXSelectDriverWindow.performKeyEquivalent(theEvent: NSEvent): ObjCBOOL;
begin
  if theEvent.charactersIgnoringModifiers.isEqualToString(NSSTR('w')) and
     ((theEvent.modifierFlags and NSCommandKeyMask) <> 0 ) then begin
    self.close;
    Result:= True;
  end else
    Result:= inherited;
end;

class function TWFXSelectDriverWindow.showModal: Integer;
var
  win: TWFXSelectDriverWindow;
  winContentView: NSView;
  cancelButton: NSButton;
  okButton: NSButton;
  frameRect: NSRect;

  function createDriverMenu: NSPopUpButton;
  var
    driver: TCloudDriverClass;
    dropDown: NSPopUpButton;
    icon: NSImage;
    i: Integer;
  begin
    frameRect:= NSMakeRect( 30, 72, 250, 30 );
    dropDown:= NSPopUpButton.alloc.initWithFrame( frameRect );
    for i:=0 to cloudDriverManager.driverClasses.Count-1 do begin
      driver:= TCloudDriverClass( cloudDriverManager.driverClasses[i] );
      dropDown.addItemWithTitle( StringToNSString(driver.driverName) );
      icon:= TWFXPluginUtil.driverMainIcon( driver );
      icon.setSize( NSMakeSize(16,16) );
      dropDown.lastItem.setImage( icon );
      icon.release;
    end;
    Result:= dropDown;
  end;

begin
  frameRect:= NSMakeRect( 0, 0, 305, 140 );
  win:= TWFXSelectDriverWindow.alloc.initWithContentRect_styleMask_backing_defer(
    frameRect,
    NSTitledWindowMask or NSFullSizeContentViewWindowMask,
    NSBackingStoreBuffered,
    True );
  winContentView:= NSView.alloc.initWithFrame( frameRect );
  win.setContentView( winContentView );
  winContentView.release;
  win.setTitlebarAppearsTransparent( True );
  win.setDelegate( win );

  win.driversDropDown:= createDriverMenu;
  winContentView.addSubview( win.driversDropDown );

  frameRect:= NSMakeRect( 105, 25, 80, 22 );
  cancelButton:= NSButton.alloc.initWithFrame( frameRect );
  cancelButton.setTitle( StringToNSString('Cancel') );
  cancelButton.setButtonType( NSMomentaryPushInButton );
  cancelButton.setBezelStyle( NSRoundedBezelStyle );
  cancelButton.setKeyEquivalent( NSSTR(#27) );
  cancelButton.setTarget( win );
  cancelButton.setAction( ObjcSelector('cancelOperation:') );
  winContentView.addSubview( cancelButton );
  cancelButton.release;

  frameRect:= NSMakeRect( 200, 25, 80, 22 );
  okButton:= NSButton.alloc.initWithFrame( frameRect );
  okButton.setTitle( StringToNSString('OK') );
  okButton.setButtonType( NSMomentaryPushInButton );
  okButton.setBezelStyle( NSRoundedBezelStyle );
  okButton.setKeyEquivalent( NSSTR(#13) );
  okButton.setTarget( win );
  okButton.setAction( ObjcSelector('close') );
  winContentView.addSubview( okButton );
  okButton.release;

  NSApplication(NSApp).runModalForWindow( win );
  Result:= win.selectedIndex;
end;

{ TWFXConnectionConfigItems }

constructor TWFXConnectionConfigItems.Create;
begin
  _items:= NSMutableArray.new;
end;

destructor TWFXConnectionConfigItems.Destroy;
begin
  _items.release;
end;

function TWFXConnectionConfigItems.indexOf( const item: TWFXConnectionConfigItem ): Integer;
begin
  Result:= self.indexOf( item.name );
end;

function TWFXConnectionConfigItems.indexOf(const name: NSString): Integer;
var
  i: Integer;
  configItem: TWFXConnectionConfigItem;
begin
  Result:= -1;
  for i:= 0 to _items.Count-1 do begin
    configItem:= TWFXConnectionConfigItem( _items.objectAtIndex(i) );
    if configItem.name.isEqualToString(name) then
      Exit( i );
  end;
end;

function TWFXConnectionConfigItems.addItem( const item: TWFXConnectionConfigItem ): Integer;
begin
  Result:= self.indexOf(item);
  if Result >= 0 then
    Exit;
  _items.addObject( item );
  Result:= _items.count - 1;
end;

function TWFXConnectionConfigItems.getItem(const index: Integer
  ): TWFXConnectionConfigItem;
begin
  Result:= TWFXConnectionConfigItem( _items.objectAtIndex(index) );
end;

procedure TWFXConnectionConfigItems.removeItemAtIndex( const index: Integer );
begin
  _items.removeObjectAtIndex( index );
end;

function TWFXConnectionConfigItems.Count: Integer;
begin
  Result:= _items.count;
end;

{ TWFXPropertyView }

procedure TWFXPropertyView.loadConnectionProperties( const index: Integer );
var
  configItem: TWFXConnectionConfigItem;
begin
  configItem:= controller.currentConfigItem;
  if configItem = nil then
    Exit;
  self.logoImageView.setImage( TWFXPluginUtil.driverMainIcon(configItem.driver) );
  self.nameTextField.setStringValue( configItem.name );
  self.updateConnectStatus;
end;

procedure TWFXPropertyView.updateConnectStatus;
var
  configItem: TWFXConnectionConfigItem;
  connectButtonText: String;
  statusImageName: NSString;
  notes: String;
begin
  configItem:= controller.currentConfigItem;
  if configItem.driver.authorized then begin
    statusImageName:= NSImageNameStatusAvailable;
    connectButtonText:= 'Disconnect';
  end else begin
    statusImageName:= NSImageNameStatusUnavailable;
    connectButtonText:= 'Connect';
  end;
  self.statusImageView.setImage( NSImage.imageNamed(statusImageName) );
  self.connectButton.setTitle( StringToNSString(connectButtonText) );
  notes:= CONST_AUTH_NOTES.Replace( '{driverName}', configItem.driver.driverName );
  self.noteTextView.setString( StringToNSString(notes) );
end;

{ TWFXConnectionConfigItem }

procedure TWFXConnectionConfigItem.dealloc;
begin
  _name.release;
  FreeAndNil( _driver );
end;

procedure TWFXConnectionConfigItem.setName(name: NSString);
begin
  if Assigned(_name) then
    _name.release;
  _name:= name;
  _name.retain;
end;

procedure TWFXConnectionConfigItem.setDriver(driver: TCloudDriver);
begin
  _driver:= driver;
end;

procedure TWFXConnectionConfigItem.setCreationTime(creationTime: TDateTime);
begin
  _creationTime:= creationTime;
end;

procedure TWFXConnectionConfigItem.setModificationTime(modificationTime: TDateTime
  );
begin
  _modificationTime:= modificationTime;
end;

procedure TWFXConnectionConfigItem.setCreating(creating: Boolean);
begin
  _creating:= creating;
end;

function TWFXConnectionConfigItem.name: NSString;
begin
  Result:= _name;
end;

function TWFXConnectionConfigItem.creating: Boolean;
begin
  Result:= _creating;
end;

function TWFXConnectionConfigItem.driver: TCloudDriver;
begin
  Result:= _driver;
end;

function TWFXConnectionConfigItem.creationTime: TDateTime;
begin
  Result:= _creationTime;
end;

function TWFXConnectionConfigItem.modificationTime: TDateTime;
begin
  Result:= _modificationTime;
end;

{ TWFXOptionsWindow }

procedure TWFXOptionsWindow.dealloc;
begin
  FreeAndNil( self.configItems );
end;

function TWFXOptionsWindow.getConfigItems: TWFXConnectionConfigItems;
begin
  Result:= self.configItems;
end;

procedure TWFXOptionsWindow.loadConnections;
var
  i: Integer;
  configItem: TWFXConnectionConfigItem;
  connection: TWFXConnection;
  connections: TWFXConnections;
begin
  self.configItems:= TWFXConnectionConfigItems.Create;
  connections:= WFXConnectionManager.connections;
  for i:=0 to connections.Count-1 do begin
    connection:= TWFXConnection( connections[i] );
    configItem:= TWFXConnectionConfigItem.new;
    configItem.setName( StringToNSString(connection.name));
    configItem.setDriver( connection.driver.clone );
    configItem.setCreationTime( connection.creationTime );
    configItem.setModificationTime( connection.modificationTime );
    self.configItems.addItem( configItem );
    configItem.release;
  end;
end;

procedure TWFXOptionsWindow.saveConnections;
var
  i: Integer;
  configItem: TWFXConnectionConfigItem;
  connection: TWFXConnection;
  connections: TWFXConnections;
begin
  connections:= TWFXConnections.Create( True );
  for i:=0 to self.configItems.count-1 do begin
    configItem:= TWFXConnectionConfigItem( self.configItems.getItem(i) );
    connection:= TWFXConnection.Create(
      configItem.name.UTF8String,
      configItem.driver.clone,
      configItem.creationTime,
      configItem.modificationTime );
    connections.Add( connection );
  end;
  WFXConnectionManager.connections:= connections;
end;

procedure TWFXOptionsWindow.selectConnection(name: NSString);
var
  configItem: TWFXConnectionConfigItem;
  i: Integer;
begin
  for i:=0 to configItems.count-1 do begin
    configItem:= TWFXConnectionConfigItem( self.configItems.getItem(i) );
    if configItem.name.isEqualToString(name) then begin
      self.connectionListView.selectRow_byExtendingSelection( i, False );
    end;
  end;
end;

procedure TWFXOptionsWindow.addConnection( connectionName: NSString );
var
  driverIndex: Integer;
  driver: TCloudDriver;
  configItem: TWFXConnectionConfigItem;
  index: Integer;
begin
  driverIndex:= TWFXSelectDriverWindow.showModal;
  if driverIndex < 0 then
    Exit;

  driver:= cloudDriverManager.createInstance( driverIndex );
  if connectionName.length = 0 then
    connectionName:= StringToNSString( 'New (' + driver.driverName + ')' );
  configItem:= TWFXConnectionConfigItem.new;
  configItem.setCreating( True );
  configItem.setName( connectionName );
  configItem.setDriver( driver );
  configItem.setCreationTime( LocalTimeToUniversal(now) );
  configItem.setModificationTime( configItem.creationTime );
  index:= self.configItems.addItem( configItem );
  configItem.release;
  self.connectionListView.noteNumberOfRowsChanged;
  self.connectionListView.selectRow_byExtendingSelection( index, False );
end;

procedure TWFXOptionsWindow.newConnection(sender: NSObject);
begin
  self.addConnection( nil );
end;

procedure TWFXOptionsWindow.removeConnection(sender: NSObject);
var
  currentIndex: Integer;
begin
  currentIndex:= self.connectionListView.selectedRow;
  if (currentIndex<0) or (currentIndex>=self.configItems.count) then
    Exit;
  self.configItems.removeItemAtIndex( currentIndex );
  self.connectionListView.reloadData;
  if currentIndex >= self.configItems.Count then
    currentIndex:= self.configItems.Count - 1;
  self.connectionListView.selectRow_byExtendingSelection( currentIndex, False );
end;

procedure TWFXOptionsWindow.saveConnection(sender: NSObject);
var
  configItem: TWFXConnectionConfigItem;
  currentIndex: Integer;
  connectionName: NSString;

  procedure alertDuplicateName;
  var
    alert: NSAlert;
  begin
    alert:= NSAlert.new;
    alert.setMessageText( StringToNSString('Duplicate Name') );
    alert.setInformativeText( StringToNSString('Please rename the Connection before saving.') );
    alert.addButtonWithTitle( NSSTR('OK') );
    alert.runModal;
    alert.release;
  end;

begin
  connectionName:= propertyView.nameTextField.stringValue;
  if connectionName.length = 0 then
    Exit;
  if self.configItems.indexOf(connectionName) >= 0 then begin
    alertDuplicateName;
    Exit;
  end;

  configItem:= self.currentConfigItem;
  configItem.setName( connectionName );
  configItem.setModificationTime( LocalTimeToUniversal(now) );
  currentIndex:= self.connectionListView.selectedRow;
  self.connectionListView.reloadData;
  self.connectionListView.selectRow_byExtendingSelection( currentIndex, False );
end;

procedure TWFXOptionsWindow.connectOrDisconnect(sender: NSObject);
var
  configItem: TWFXConnectionConfigItem;
  driver: TCloudDriver;
begin
  try
    configItem:= self.currentConfigItem;
    driver:= configItem.driver;
    if driver.authorized then
      driver.unauthorize
    else
      driver.authorize;
  except
    on e: Exception do begin
      TLogUtil.logError( 'in TCloudOptionsWindow: ' + e.Message );
    end;
  end;

  try
    self.propertyView.updateConnectStatus;
  except
    on e: Exception do begin
      TLogUtil.logError( 'in TCloudOptionsWindow: ' + e.Message );
    end;
  end;
end;

function TWFXOptionsWindow.currentConfigItem: TWFXConnectionConfigItem;
var
  currentIndex: Integer;
begin
  currentIndex:= connectionListView.selectedRow;
  if (currentIndex<0) or (currentIndex>=self.configItems.count) then
    Exit( nil );
  Result:= TWFXConnectionConfigItem( self.configItems.getItem(currentIndex) );
end;

procedure TWFXOptionsWindow.onSelectedConnectionChanged( const selectedIndex: Integer );
begin
  self.propertyView.loadConnectionProperties( selectedIndex );
end;

procedure TWFXOptionsWindow.windowWillClose(notification: NSNotification);
begin
  self.saveConnections;
  NSApplication(NSAPP).stopModal;
end;

procedure TWFXOptionsWindow.cancelOperation(sender: id);
begin
  self.close;
end;

function TWFXOptionsWindow.performKeyEquivalent(theEvent: NSEvent): ObjCBOOL;
begin
  if theEvent.charactersIgnoringModifiers.isEqualToString(NSSTR('w')) and
     ((theEvent.modifierFlags and NSCommandKeyMask) <> 0 ) then begin
    self.close;
    Result:= True;
  end else
    Result:= inherited;
end;

{ TConnectionListView }

function TWFXConnectionListView.numberOfRowsInTableView(tableView: NSTableView
  ): NSInteger;
begin
  Result:= self.controller.getConfigItems.count;
end;

function TWFXConnectionListView.tableView_viewForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
var
  frameRect: NSRect;
  cellView: NSTableCellView;
  textField: NSTextField;
  imageView: NSImageView;
  configItem: TWFXConnectionConfigItem;
begin
  configItem:= TWFXConnectionConfigItem( self.controller.getConfigItems.getItem(row) );
  cellView:= NSTableCellView.alloc.initWithFrame( NSZeroRect );
  cellView.autorelease;

  frameRect:= NSMakeRect( 8, 4, 16, 16 );
  imageView:= NSImageView.alloc.initWithFrame( frameRect);
  imageView.setImage( TWFXPluginUtil.driverMainIcon(configItem.driver) );
  cellView.setImageView( imageView );
  cellView.addSubview( imageView );
  imageView.release;

  frameRect:= NSMakeRect( 32, 0, 100, 20 );
  textField:= NSTextField.alloc.initWithFrame( frameRect );
  textField.setEditable( False );
  textField.setStringValue( configItem.name );
  textField.sizeToFit;
  textField.setBordered( False );
  textField.setBezeled( False );
  textField.setBackgroundColor( NSColor.clearColor );
  cellView.setTextField( textField );
  cellView.addSubview( textField );
  textField.release;

  Result:= cellView;
end;

procedure TWFXConnectionListView.tableViewSelectionDidChange(
  notification: NSNotification);
begin
  controller.onSelectedConnectionChanged( self.selectedRow );
end;

{ TWFXOptionsUtil }

class function TWFXOptionsUtil.createWindow: NSWindow;
var
  win: TWFXOptionsWindow;
  contentView: NSView;

  splitView: NSSplitView;
  leftView: NSVisualEffectView;
  rightView: TWFXPropertyView;

  frameRect: NSRect;
  leftRect: NSRect;
  rightRect: NSRect;

  connectionListView: TWFXConnectionListView;

  procedure createLeftView;
  var
    connectionScrollView: NSScrollView;
    connectionColumn: NSTableColumn;

    addButton: NSButton;
    removeButton: NSButton;
  begin
    leftView:= NSVisualEffectView.alloc.initWithFrame( leftRect );
    leftView.setBlendingMode( NSVisualEffectBlendingModeBehindWindow );
    leftView.setMaterial( NSVisualEffectMaterialPopover );

    connectionListView:= TWFXConnectionListView.alloc.initWithFrame( NSZeroRect );
    if NSAppKitVersionNumber >= NSAppKitVersionNumber11_0 then
      connectionListView.setStyle( NSTableViewStyleSourceList );
    connectionListView.setWantsLayer( True );
    connectionListView.layer.setBackgroundColor( NSColor.clearColor.CGColor );
    connectionListView.controller:= win;
    connectionListView.setDataSource( connectionListView );
    connectionListView.setDelegate( connectionListView );
    connectionListView.setAllowsEmptySelection( False );
    connectionListView.setHeaderView( nil );
    connectionListView.setFocusRingType( NSFocusRingTypeNone );
    connectionListView.setIntercellSpacing( NSMakeSize(10,8) );
    connectionColumn:= NSTableColumn.alloc.initWithIdentifier( NSSTR('Column1') );
    connectionColumn.setWidth( leftRect.size.width-20-20 );
    connectionListView.addTableColumn( connectionColumn );
    connectionScrollView:= NSScrollView.alloc.initWithFrame(
      NSMakeRect(10, 60, connectionColumn.width+10+10, leftRect.size.height-60-40) );
    connectionScrollView.setDocumentView( connectionListView );
    connectionScrollView.setFocusRingType( NSFocusRingTypeNone );

    leftView.addSubview( connectionScrollView );
    connectionColumn.release;
    connectionScrollView.release;

    addButton:= NSButton.alloc.initWithFrame( NSMakeRect(26,22,22,22) );
    addButton.setButtonType( NSMomentaryPushInButton );
    addButton.setBezelStyle( NSSmallSquareBezelStyle );
    addButton.setImage( NSImage.imageNamed(NSImageNameAddTemplate) );
    addButton.setTarget( win );
    addButton.setAction( ObjCSelector('TCloudConfigItemsController_newConnection:') );
    leftView.addSubview( addButton );
    addButton.release;

    removeButton:= NSButton.alloc.initWithFrame( NSMakeRect(47,22,22,22) );
    removeButton.setButtonType( NSMomentaryPushInButton );
    removeButton.setBezelStyle( NSSmallSquareBezelStyle );
    removeButton.setImage( NSImage.imageNamed(NSImageNameRemoveTemplate) );
    removeButton.setTarget( win );
    removeButton.setAction( ObjCSelector('TCloudConfigItemsController_removeConnection:') );
    leftView.addSubview( removeButton );
    removeButton.release;
  end;

  procedure createRightView;
  var
    logoImageView: NSImageView;
    nameLabel: NSTextField;
    nameTextField: NSTextField;
    connectButton: NSButton;
    statusImageView: NSImageView;
    saveButton: NSButton;
    noteTextView: NSTextView;
  begin
    rightView:= TWFXPropertyView.alloc.initWithFrame( rightRect ) ;
    rightView.controller:= win;;

    logoImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(200,530,32,32) );
    rightView.logoImageView:= logoImageView;
    rightView.addSubview( logoImageView );
    logoImageView.release;

    nameLabel:= NSTextField.alloc.initWithFrame( NSMakeRect(20,480,50,20) );
    nameLabel.setEditable( False );
    nameLabel.setDrawsBackground( False );
    nameLabel.setBordered( False );
    nameLabel.setStringValue( NSSTR('Name:') );
    rightView.addSubview( nameLabel );
    nameLabel.release;

    nameTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(80,480,250,22) );
    rightView.nameTextField:= nameTextField;
    rightView.addSubview( nameTextField );
    nameTextField.release;

    statusImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(350,483,16,16) );
    rightView.statusImageview:= statusImageView;
    rightView.addSubview( statusImageView );
    statusImageView.release;

    connectButton:= NSButton.alloc.initWithFrame( NSMakeRect(80,430,100,22) );
    connectButton.setBezelStyle( NSRoundedBezelStyle );
    connectButton.setTarget( win );
    connectButton.setAction( ObjCSelector('TCloudConfigItemsController_connectOrDisconnect:') );
    rightView.connectButton:= connectButton;
    rightView.addSubView( connectButton );
    connectButton.release;

    saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(200,430,100,22) );
    saveButton.setBezelStyle( NSRoundedBezelStyle );
    saveButton.setTitle( NSSTR('Save') );
    saveButton.setTarget( win );
    saveButton.setAction( ObjCSelector('TCloudConfigItemsController_saveConnection:') );
    rightView.addSubView( saveButton );
    saveButton.release;

    noteTextView:= NSTextView.alloc.initWithFrame( NSMakeRect(20,100,400,100) );
    noteTextView.setFont( NSFont.systemFontOfSize(11));
    noteTextView.setEditable( False );
    noteTextView.setDrawsBackground( False );
    rightView.noteTextView:= noteTextView;
    rightView.addSubView( noteTextView );
    noteTextView.release;
  end;

begin
  frameRect:= NSMakeRect(0,0,680,600);
  leftRect:= NSMakeRect(0,0,240,600);
  rightRect:= NSMakeRect(0,0,440,600);

  contentView:= NSView.alloc.initWithFrame( frameRect );
  win:= TWFXOptionsWindow.alloc.initWithContentRect_styleMask_backing_defer(
    frameRect,
    NSFullSizeContentViewWindowMask or NSTitledWindowMask or NSClosableWindowMask,
    NSBackingStoreBuffered,
    True );
  win.loadConnections;
  win.setDelegate( win );
  win.setTitlebarAppearsTransparent( True );
  win.setContentView( contentView );
  contentView.release;

  splitView:= NSSplitView.alloc.initWithFrame( frameRect );
  splitView.setVertical( True );
  splitView.setDividerStyle( NSSplitViewDividerStyleThin );
  contentView.addSubview( splitView );
  splitView.release;

  createRightView;
  win.propertyView:= rightView;
  createLeftView;
  win.connectionListView:= connectionListView;
  splitView.addSubview( leftView );
  splitView.addSubview( rightView );
  leftView.release;
  rightView.release;

  win.makeFirstResponder( connectionListView );
  connectionListView.selectRow_byExtendingSelection( 0, False );
  Result:= win;
end;

class procedure TWFXOptionsUtil.show( const connectionName: String );
var
  win: NSWindow;
begin
  win:= self.createWindow;
  TWFXOptionsWindow(win).selectConnection( StringToNSString(connectionName) );
  NSApplication(NSApp).runModalForWindow( win );
end;

class procedure TWFXOptionsUtil.addAndShow( const connectionName: String = '' );
var
  win: NSWindow;
begin
  win:= self.createWindow;
  TWFXOptionsWindow(win).addConnection( StringToNSString(connectionName) );
  NSApplication(NSApp).runModalForWindow( win );
end;

end.


unit uMacCloudOptions;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uMacCloudCore;

type

  { TCloudOptionsUtil }

  TCloudOptionsUtil = class
  private
    class function createWindow: NSWindow;
  public
    class procedure show( const connectionName: String);
    class procedure addAndShow;
  end;

implementation

const
  CONST_AUTH_NOTES =
    '1. Before successfully enabling the link, DC needs to obtain authorization from DropBox'#13#13 +
    '2. Click the connect button to be redirected to the DropBox official website in the Safari browser'#13#13 +
    '3. Please login your DropBox account in Safari and authorize Double Commander to access'#13#13 +
    '4. The authorization is completed on the DropBox official website, Double Command will not get your password';

type

  { TConnectionConfigItem }

  TConnectionConfigItem = objcclass( NSObject )
  public
    _name: NSString;
    _creating: Boolean;
    _driver: TCloudDriver;
  public
    procedure dealloc; override;
    procedure setName( name: NSString );
      message 'TConnectionConfigItem_setName:';
    procedure setDriver( driver: TCloudDriver );
      message 'TConnectionConfigItem_setDriver:';
    procedure setCreating( creating: Boolean );
      message 'TConnectionConfigItem_setCreating:';
    function name: NSString;
      message 'TConnectionConfigItem_Name';
    function creating: Boolean;
      message 'TConnectionConfigItem_creating';
    function driver: TCloudDriver;
      message 'TConnectionConfigItem_driver';
  end;

  { TCloudConfigItemsController }

  TCloudConfigItemsController = objcprotocol
    function getConfigItems: NSArray; message 'TCloudConfigItemsController_getConfigItems';
    procedure addConnection( sender: NSObject ); message 'TCloudConfigItemsController_addConnection:';
    procedure removeConnection( sender: NSObject ); message 'TCloudConfigItemsController_removeConnection:';
    procedure saveConnection( sender: NSObject ); message 'TCloudConfigItemsController_saveConnection:';
    procedure connectOrDisconnect( sender: NSObject ); message 'TCloudConfigItemsController_connectOrDisconnect:';
    function currentConfigItem: TConnectionConfigItem; message 'TCloudConfigItemsController_currentConfigItem';
    procedure onSelectedConnectionChanged( const selectedIndex: Integer );
      message 'TCloudConfigItemsController_onSelectedConnectionChanged:';
  end;

  { TConnectionListView }

  TConnectionListView = objcclass(
    NSTableView,
    NSTableViewDataSourceProtocol,
    NSTableViewDelegateProtocol )
  private
    controller: TCloudConfigItemsController;
  public
    function numberOfRowsInTableView (tableView: NSTableView): NSInteger;
    function tableView_viewForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
    procedure tableViewSelectionDidChange (notification: NSNotification);
  end;

  { TPropertyView }

  TPropertyView = objcclass( NSView )
  private
    controller: TCloudConfigItemsController;
    nameTextField: NSTextField;
    connectButton: NSButton;
    statusImageview: NSImageView;
  public
    procedure loadConnectionProperties( const index: Integer ); message 'TPropertyView_loadConnectionProperties:';
    procedure updateConnectStatus; message 'TPropertyView_updateConnectStatus';
  end;

  { TCloudOptionsWindow }

  TCloudOptionsWindow = objcclass(
    NSWindow,
    NSWindowDelegateProtocol,
    TCloudConfigItemsController )
  private
    configItems: NSMutableArray;
    connectionListView: TConnectionListView;
    propertyView: TPropertyView;
  public
    procedure dealloc; override;
    function getConfigItems: NSArray;
    procedure loadConnections; message 'TCloudOptionsWindow_loadConnections';
    procedure saveConnections; message 'TCloudOptionsWindow_saveConnections';
    procedure selectConnection( name: NSString ); message 'TCloudOptionsWindow_selectConnection:';
    procedure addConnection( sender: NSObject );
    procedure removeConnection( sender: NSObject );
    procedure saveConnection( sender: NSObject );
    procedure connectOrDisconnect( sender: NSObject );
    function currentConfigItem: TConnectionConfigItem;
    procedure onSelectedConnectionChanged( const selectedIndex: Integer );
  public
    procedure windowWillClose (notification: NSNotification);
  end;

{ TPropertyView }

procedure TPropertyView.loadConnectionProperties( const index: Integer );
var
  configItem: TConnectionConfigItem;
begin
  configItem:= controller.currentConfigItem;
  if configItem = nil then
    Exit;
  self.nameTextField.setStringValue( configItem.name );
  self.updateConnectStatus;
end;

procedure TPropertyView.updateConnectStatus;
var
  configItem: TConnectionConfigItem;
  connectButtonText: String;
  statusImageName: NSString;
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
end;

{ TConnectionConfigItem }

procedure TConnectionConfigItem.dealloc;
begin
  _name.release;
  FreeAndNil( _driver );
end;

procedure TConnectionConfigItem.setName(name: NSString);
begin
  if Assigned(_name) then
    _name.release;
  _name:= name;
  _name.retain;
end;

procedure TConnectionConfigItem.setDriver(driver: TCloudDriver);
begin
  _driver:= driver;
end;

procedure TConnectionConfigItem.setCreating(creating: Boolean);
begin
  _creating:= creating;
end;

function TConnectionConfigItem.name: NSString;
begin
  Result:= _name;
end;

function TConnectionConfigItem.creating: Boolean;
begin
  Result:= _creating;
end;

function TConnectionConfigItem.driver: TCloudDriver;
begin
  Result:= _driver;
end;

{ TCloudOptionsWindow }

procedure TCloudOptionsWindow.dealloc;
begin
  self.configItems.release;
end;

function TCloudOptionsWindow.getConfigItems: NSArray;
begin
  Result:= self.configItems;
end;

procedure TCloudOptionsWindow.loadConnections;
var
  i: Integer;
  configItem: TConnectionConfigItem;
  connection: TCloudConnection;
  connections: TCloudConnections;
begin
  self.configItems:= NSMutableArray.new;
  connections:= cloudConnectionManager.connections;
  for i:=0 to connections.Count-1 do begin
    connection:= TCloudConnection( connections[i] );
    configItem:= TConnectionConfigItem.new;
    configItem.setName( StringToNSString(connection.name));
    configItem.setDriver( connection.driver.clone );
    self.configItems.addObject( configItem );
    configItem.release;
  end;
end;

procedure TCloudOptionsWindow.saveConnections;
var
  i: Integer;
  configItem: TConnectionConfigItem;
  connection: TCloudConnection;
  connections: TCloudConnections;
begin
  connections:= TCloudConnections.Create( True );
  for i:=0 to self.configItems.count-1 do begin
    configItem:= TConnectionConfigItem( self.configItems.objectAtIndex(i) );
    connection:= TCloudConnection.Create( configItem.name.UTF8String, configItem.driver.clone  );
    connections.Add( connection );
  end;
  cloudConnectionManager.connections:= connections;
end;

procedure TCloudOptionsWindow.selectConnection(name: NSString);
var
  configItem: TConnectionConfigItem;
  i: Integer;
begin
  for i:=0 to configItems.count-1 do begin
    configItem:= TConnectionConfigItem( self.configItems.objectAtIndex(i) );
    if configItem.name.isEqualToString(name) then begin
      self.connectionListView.selectRow_byExtendingSelection( i, False );
    end;
  end;
end;

procedure TCloudOptionsWindow.addConnection(sender: NSObject);
var
  configItem: TConnectionConfigItem;
begin
  configItem:= TConnectionConfigItem.new;
  configItem.setCreating( True );
  configItem.setName( StringToNSString( 'New Connection *' ) );
  configItem.setDriver( cloudDriverManager.createInstance('DropBox') );
  self.configItems.addObject( configItem );
  configItem.release;
  self.connectionListView.noteNumberOfRowsChanged;
  self.connectionListView.selectRow_byExtendingSelection( self.configItems.count-1, False );
end;

procedure TCloudOptionsWindow.removeConnection(sender: NSObject);
var
  currentIndex: Integer;
begin
  currentIndex:= self.connectionListView.selectedRow;
  if (currentIndex<0) or (currentIndex>=self.configItems.count) then
    Exit;
  self.configItems.removeObjectAtIndex( currentIndex );
  self.connectionListView.reloadData;
  if currentIndex >= self.configItems.Count then
    currentIndex:= self.configItems.Count - 1;
  self.connectionListView.selectRow_byExtendingSelection( currentIndex, False );
end;

procedure TCloudOptionsWindow.saveConnection(sender: NSObject);
var
  configItem: TConnectionConfigItem;
  currentIndex: Integer;
begin
  configItem:= self.currentConfigItem;
  configItem.setName( propertyView.nameTextField.stringValue );
  currentIndex:= self.connectionListView.selectedRow;
  self.connectionListView.reloadData;
  self.connectionListView.selectRow_byExtendingSelection( currentIndex, False );
end;

procedure TCloudOptionsWindow.connectOrDisconnect(sender: NSObject);
var
  configItem: TConnectionConfigItem;
  driver: TCloudDriver;
begin
  configItem:= self.currentConfigItem;
  driver:= configItem.driver;
  if driver.authorized then
    driver.unauthorize
  else
    driver.authorize;
  self.propertyView.updateConnectStatus;
end;

function TCloudOptionsWindow.currentConfigItem: TConnectionConfigItem;
var
  currentIndex: Integer;
begin
  currentIndex:= connectionListView.selectedRow;
  if (currentIndex<0) or (currentIndex>=self.configItems.count) then
    Exit( nil );
  Result:= TConnectionConfigItem( self.configItems.objectAtIndex(currentIndex) );
end;

procedure TCloudOptionsWindow.onSelectedConnectionChanged( const selectedIndex: Integer );
begin
  self.propertyView.loadConnectionProperties( selectedIndex );
end;

procedure TCloudOptionsWindow.windowWillClose(notification: NSNotification);
begin
  self.saveConnections;
  NSApplication(NSAPP).stopModal;
end;

{ TConnectionListView }

function TConnectionListView.numberOfRowsInTableView(tableView: NSTableView
  ): NSInteger;
begin
  Result:= self.controller.getConfigItems.count;
end;

function TConnectionListView.tableView_viewForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
var
  frameRect: NSRect;
  cellView: NSTableCellView;
  textField: NSTextField;
  configItem: TConnectionConfigItem;
begin
  configItem:= TConnectionConfigItem( self.controller.getConfigItems.objectAtIndex(row) );
  frameRect:= NSMakeRect( 0, 0, 100, 20 );
  cellView:= NSTableCellView.alloc.initWithFrame( NSZeroRect );
  cellView.autorelease;
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

procedure TConnectionListView.tableViewSelectionDidChange(
  notification: NSNotification);
begin
  controller.onSelectedConnectionChanged( self.selectedRow );
end;

{ TCloudOptionsUtil }

class function TCloudOptionsUtil.createWindow: NSWindow;
var
  win: TCloudOptionsWindow;
  contentView: NSView;

  splitView: NSSplitView;
  leftView: NSView;
  rightView: TPropertyView;

  frameRect: NSRect;
  leftRect: NSRect;
  rightRect: NSRect;

  connectionListView: TConnectionListView;

  procedure createLeftView;
  var
    connectionScrollView: NSScrollView;
    connectionColumn: NSTableColumn;

    addButton: NSButton;
    removeButton: NSButton;
  begin
    leftView:= NSView.alloc.initWithFrame( leftRect );
    leftView.setWantsLayer( True );
    leftView.layer.setBackgroundColor( NSColor.windowBackgroundColor.CGColor );

    connectionListView:= TConnectionListView.alloc.initWithFrame( NSZeroRect );
    connectionListView.controller:= win;
    connectionListView.setDataSource( connectionListView );
    connectionListView.setDelegate( connectionListView );
    connectionListView.setAllowsEmptySelection( False );
    connectionListView.setHeaderView( nil );
    connectionListView.setFocusRingType( NSFocusRingTypeNone );
    connectionListView.setBackgroundColor( NSColor.windowBackgroundColor.colorWithAlphaComponent(1) );
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
    addButton.setAction( ObjCSelector('TCloudConfigItemsController_addConnection:') );
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
    nameLabel: NSTextField;
    nameTextField: NSTextField;
    connectButton: NSButton;
    statusImageView: NSImageView;
    saveButton: NSButton;
    noteTextView: NSTextView;
  begin
    rightView:= TPropertyView.alloc.initWithFrame( rightRect ) ;
    rightView.controller:= win;;
    nameLabel:= NSTextField.alloc.initWithFrame( NSMakeRect(20,500,50,20) );
    nameLabel.setEditable( False );
    nameLabel.setDrawsBackground( False );
    nameLabel.setBordered( False );
    nameLabel.setStringValue( NSSTR('Name:') );
    rightView.addSubview( nameLabel );
    nameLabel.release;

    nameTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(80,500,250,22) );
    rightView.nameTextField:= nameTextField;
    rightView.addSubview( nameTextField );
    nameTextField.release;

    statusImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(350,503,16,16) );
    rightView.statusImageview:= statusImageView;
    rightView.addSubview( statusImageView );
    statusImageView.release;

    connectButton:= NSButton.alloc.initWithFrame( NSMakeRect(80,450,100,22) );
    connectButton.setBezelStyle( NSRoundedBezelStyle );
    connectButton.setTarget( win );
    connectButton.setAction( ObjCSelector('TCloudConfigItemsController_connectOrDisconnect:') );
    rightView.connectButton:= connectButton;
    rightView.addSubView( connectButton );
    connectButton.release;

    saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(200,450,100,22) );
    saveButton.setBezelStyle( NSRoundedBezelStyle );
    saveButton.setTitle( NSSTR('Save') );
    saveButton.setTarget( win );
    saveButton.setAction( ObjCSelector('TCloudConfigItemsController_saveConnection:') );
    rightView.addSubView( saveButton );
    saveButton.release;

    noteTextView:= NSTextView.alloc.initWithFrame( NSMakeRect(20,100,400,100) );
    noteTextView.setFont( NSFont.systemFontOfSize(11));
    noteTextView.setEditable( False );
    noteTextView.setString( StringToNSString(CONST_AUTH_NOTES) );
    noteTextView.setDrawsBackground( False );
    rightView.addSubView( noteTextView );
    noteTextView.release;
  end;

begin
  frameRect:= NSMakeRect(0,0,680,600);
  leftRect:= NSMakeRect(0,0,240,600);
  rightRect:= NSMakeRect(0,0,440,600);

  contentView:= NSView.alloc.initWithFrame( frameRect );
  win:= TCloudOptionsWindow.alloc.initWithContentRect_styleMask_backing_defer(
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

class procedure TCloudOptionsUtil.show( const connectionName: String );
var
  win: NSWindow;
begin
  win:= self.createWindow;
  TCloudOptionsWindow(win).selectConnection( StringToNSString(connectionName) );
  NSApplication(NSApp).runModalForWindow( win );
end;

class procedure TCloudOptionsUtil.addAndShow;
var
  win: NSWindow;
begin
  win:= self.createWindow;
  TCloudOptionsWindow(win).addConnection( nil );
  NSApplication(NSApp).runModalForWindow( win );
end;

end.


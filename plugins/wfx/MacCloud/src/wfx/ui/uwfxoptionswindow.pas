unit uWFXOptionsWindow;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uOAuth2Client,
  uWFXPlugin, uWFXUtil,
  uWFXOptionsCore, uWFXOptionsCommonRS, uWFXOptionsOAuth2, uWFXOptionsS3,
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

type

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
    procedure setFrameSize(newSize: NSSize); override;
  end;

  { TWFXOptionsWindow }

  TWFXOptionsWindow = objcclass(
    NSWindow,
    NSWindowDelegateProtocol,
    TWFXConfigItemsController )
  private
    configItems: TWFXConnectionConfigItems;
    splitView: NSSplitView;
    connectionListView: TWFXConnectionListView;
    propertyView: TWFXPropertyView;
  public
    procedure dealloc; override;
    function getConfigItems: TWFXConnectionConfigItems;
    procedure addConnection( connectionName: NSString ); message 'TCloudOptionsWindow_addConnection:';
    procedure loadConnections; message 'TCloudOptionsWindow_loadConnections';
    procedure saveConnections; message 'TCloudOptionsWindow_saveConnections';
    procedure selectConnection( name: NSString ); message 'TCloudOptionsWindow_selectConnection:';
    procedure selectConnectionIndex( index: Integer ); message 'TCloudOptionsWindow_selectConnectionIndex:';
    procedure newConnection( sender: NSObject );
    procedure removeConnection( sender: NSObject );
    procedure saveConnection( name: NSString );
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
    menuItem: TWFXCloudDriverMenuItem;
    driver: TCloudDriverClass;
    dropDown: NSPopUpButton;
    icon: NSImage;
    i: Integer;
  begin
    frameRect:= NSMakeRect( 30, 72, 250, 30 );
    dropDown:= NSPopUpButton.alloc.initWithFrame( frameRect );
    for i:=0 to WFXCloudDriverMenuItems.Count-1 do begin
      menuItem:= WFXCloudDriverMenuItems[i];
      if menuItem.name <> EmptyStr then begin
        dropDown.addItemWithTitle( StringToNSString(menuItem.displayName) );
        driver:= cloudDriverManager.find( menuItem.name );
        icon:= TWFXPluginUtil.driverMainIcon( driver );
        icon.setSize( NSMakeSize(16,16) );
        dropDown.lastItem.setImage( icon );
        icon.release;
      end else begin
        dropDown.menu.addItem( NSMenuItem.separatorItem );
      end;
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
  cancelButton.setTitle( StringToNSString(rsCancelButtonTitle) );
  cancelButton.setButtonType( NSMomentaryPushInButton );
  cancelButton.setBezelStyle( NSRoundedBezelStyle );
  cancelButton.setKeyEquivalent( NSSTR(#27) );
  cancelButton.setTarget( win );
  cancelButton.setAction( ObjcSelector('cancelOperation:') );
  winContentView.addSubview( cancelButton );
  cancelButton.release;

  frameRect:= NSMakeRect( 200, 25, 80, 22 );
  okButton:= NSButton.alloc.initWithFrame( frameRect );
  okButton.setTitle( StringToNSString(rsOkButtonTitle) );
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
      self.selectConnectionIndex( i );
      Exit;
    end;
  end;
  self.onSelectedConnectionChanged( i );
end;

procedure TWFXOptionsWindow.selectConnectionIndex( index: Integer );
var
  indexSet: NSIndexSet;
begin
  indexSet:= NSIndexSet.indexSetWithIndex( index );
  self.connectionListView.selectRowIndexes_byExtendingSelection( indexSet , False );
end;

procedure TWFXOptionsWindow.addConnection( connectionName: NSString );
var
  driverIndex: Integer;
  driver: TCloudDriver;
  configItem: TWFXConnectionConfigItem;
  menuItem: TWFXCloudDriverMenuItem;
  index: Integer;
begin
  driverIndex:= TWFXSelectDriverWindow.showModal;
  if driverIndex < 0 then begin
    self.onSelectedConnectionChanged( driverIndex );
    Exit;
  end;

  menuItem:= WFXCloudDriverMenuItems[driverIndex];
  driver:= cloudDriverManager.createInstance( menuItem.name );
  if connectionName.length = 0 then
    connectionName:= StringToNSString( menuItem.displayName + ' (New)' );
  configItem:= TWFXConnectionConfigItem.new;
  configItem.setCreating( True );
  configItem.setName( connectionName );
  configItem.setDriver( driver );
  configItem.setCreationTime( LocalTimeToUniversal(now) );
  configItem.setModificationTime( configItem.creationTime );
  index:= self.configItems.addItem( configItem );
  configItem.release;
  self.connectionListView.noteNumberOfRowsChanged;
  self.selectConnectionIndex( index );
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
  if currentIndex >= 0 then begin
    self.selectConnectionIndex( currentIndex );
  end else begin
    self.onSelectedConnectionChanged( currentIndex );
  end;
end;

procedure TWFXOptionsWindow.saveConnection( name: NSString );
var
  configItem: TWFXConnectionConfigItem;
  currentIndex: Integer;
  nameIndex: Integer;

  procedure alertDuplicateName;
  var
    alert: NSAlert;
  begin
    alert:= NSAlert.new;
    alert.setMessageText( StringToNSString('Duplicate Name') );
    alert.setInformativeText( StringToNSString('Please rename the Connection before saving.') );
    alert.addButtonWithTitle( StringToNSString(rsOkButtonTitle) );
    alert.runModal;
    alert.release;
  end;

begin
  if name.length = 0 then
    Exit;

  currentIndex:= self.connectionListView.selectedRow;
  nameIndex:= self.configItems.indexOf( name );

  if (nameIndex>=0) and (nameIndex<>currentIndex) then begin
    alertDuplicateName;
    Exit;
  end;

  configItem:= self.currentConfigItem;
  configItem.setName( name );
  configItem.setModificationTime( LocalTimeToUniversal(now) );
  self.connectionListView.reloadData;
  self.selectConnectionIndex( currentIndex );
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
var
  configItem: TWFXConnectionConfigItem;
  newView: TWFXPropertyView;
  rightRect: NSRect;
begin
  rightRect:= NSMakeRect(0,0,480,640);
  configItem:= self.currentConfigItem;
  if configItem = nil then
    newView:= TWFXPropertyView.alloc.initWithFrame( rightRect )
  else if configItem.driver is TOAuth2SessionCloudDriver then
    newView:= TWFXOAuth2PropertyView.alloc.initWithFrame( rightRect )
  else
    newView:= TWFXS3PropertyView.alloc.initWithFrame( rightRect ) ;
  newView.setController( self );
  if Assigned(self.propertyView) then
    self.propertyView.removeFromSuperview;
  self.propertyView:= newView;
  self.splitView.addSubview( newView );
  self.splitView.setPosition_ofDividerAtIndex( 240, 0 );
  newView.setFrame( rightRect );
  newView.loadConnectionProperties( selectedIndex );
  newView.release;
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

procedure TWFXConnectionListView.setFrameSize(newSize: NSSize);
begin
  inherited setFrameSize(newSize);
  self.sizeLastColumnToFit;
end;

{ TWFXOptionsUtil }

class function TWFXOptionsUtil.createWindow: NSWindow;
var
  win: TWFXOptionsWindow;
  contentView: NSView;

  leftView: NSVisualEffectView;

  frameRect: NSRect;
  leftRect: NSRect;

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
    connectionScrollView.setAutoresizingMask( NSViewWidthSizable);

    leftView.addSubview( connectionScrollView );
    connectionColumn.release;
    connectionScrollView.release;

    addButton:= NSButton.alloc.initWithFrame( NSMakeRect(26,22,22,22) );
    addButton.setButtonType( NSMomentaryPushInButton );
    addButton.setBezelStyle( NSSmallSquareBezelStyle );
    addButton.setImage( NSImage.imageNamed(NSImageNameAddTemplate) );
    addButton.setTarget( win );
    addButton.setAction( ObjCSelector('TWFXConfigItemsController_newConnection:') );
    leftView.addSubview( addButton );
    addButton.release;

    removeButton:= NSButton.alloc.initWithFrame( NSMakeRect(47,22,22,22) );
    removeButton.setButtonType( NSMomentaryPushInButton );
    removeButton.setBezelStyle( NSSmallSquareBezelStyle );
    removeButton.setImage( NSImage.imageNamed(NSImageNameRemoveTemplate) );
    removeButton.setTarget( win );
    removeButton.setAction( ObjCSelector('TWFXConfigItemsController_removeConnection:') );
    leftView.addSubview( removeButton );
    removeButton.release;
  end;

begin
  frameRect:= NSMakeRect(0,0,720,640);
  leftRect:= NSMakeRect(0,0,240,640);

  contentView:= NSView.alloc.initWithFrame( frameRect );
  win:= TWFXOptionsWindow.alloc.initWithContentRect_styleMask_backing_defer(
    frameRect,
    NSFullSizeContentViewWindowMask or NSTitledWindowMask or NSClosableWindowMask,
    NSBackingStoreBuffered,
    True );
  win.setAutorecalculatesKeyViewLoop( True );
  win.loadConnections;
  win.setDelegate( win );
  win.setTitlebarAppearsTransparent( True );
  win.setContentView( contentView );
  contentView.release;

  win.splitView:= NSSplitView.alloc.initWithFrame( frameRect );
  win.splitView.setVertical( True );
  win.splitView.setDividerStyle( NSSplitViewDividerStyleThin );
  contentView.addSubview( win.splitView );
  win.splitView.release;

  createLeftView;
  win.connectionListView:= connectionListView;
  win.splitView.insertArrangedSubview( leftView, 0 );
  leftView.release;

  win.makeFirstResponder( connectionListView );
  win.selectConnectionIndex( 0 );
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


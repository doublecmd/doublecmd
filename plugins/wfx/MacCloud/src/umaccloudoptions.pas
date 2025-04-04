unit uMacCloudOptions;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll;

type

  { TCloudOptionsUtil }

  TCloudOptionsUtil = class
  private
    class function createWindow: NSWindow;
  public
    class procedure show;
  end;

implementation

type

  { TConnectionListView }

  TConnectionListView = objcclass(
    NSTableView,
    NSTableViewDataSourceProtocol,
    NSTableViewDelegateProtocol )
  public
    function numberOfRowsInTableView (tableView: NSTableView): NSInteger;
    function tableView_viewForTableColumn_row (tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
  end;

  { TCloudOptionsWindow }

  TCloudOptionsWindow = objcclass( NSWindow, NSWindowDelegateProtocol )
    procedure windowWillClose (notification: NSNotification);
  end;

{ TCloudOptionsWindow }

procedure TCloudOptionsWindow.windowWillClose(notification: NSNotification);
begin
  NSApplication(NSAPP).stopModal;
end;

{ TConnectionListView }

function TConnectionListView.numberOfRowsInTableView(tableView: NSTableView
  ): NSInteger;
begin
  Result:= 100;
end;

function TConnectionListView.tableView_viewForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
begin
  Result:= nil;
end;

{ TCloudOptionsUtil }

class function TCloudOptionsUtil.createWindow: NSWindow;
var
  win: TCloudOptionsWindow;
  contentView: NSView;

  splitView: NSSplitView;
  leftView: NSView;
  rightView: NSView;

  frameRect: NSRect;
  leftRect: NSRect;
  rightRect: NSRect;

  procedure createLeftView;
  var
    connectionScrollView: NSScrollView;
    connectionListView: TConnectionListView;
    connectionColumn: NSTableColumn;

    addButton: NSButton;
    removeButton: NSButton;
  begin
    leftView:= NSView.alloc.initWithFrame( leftRect );
    leftView.setWantsLayer( True );
    leftView.layer.setBackgroundColor( NSColor.windowBackgroundColor.CGColor );

    connectionListView:= TConnectionListView.alloc.initWithFrame( NSZeroRect );
    connectionListView.setDataSource( connectionListView );
    connectionListView.setDelegate( connectionListView );
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
    leftView.addSubview( addButton );
    addButton.release;

    removeButton:= NSButton.alloc.initWithFrame( NSMakeRect(47,22,22,22) );
    removeButton.setButtonType( NSMomentaryPushInButton );
    removeButton.setBezelStyle( NSSmallSquareBezelStyle );
    removeButton.setImage( NSImage.imageNamed(NSImageNameRemoveTemplate) );
    leftView.addSubview( removeButton );
    removeButton.release;
  end;

  procedure createRightView;
  var
    nameLabel: NSTextField;
    nameTextField: NSTextField;
    saveButton: NSButton;
  begin
    rightView:= NSView.alloc.initWithFrame( rightRect ) ;
    nameLabel:= NSTextField.alloc.initWithFrame( NSMakeRect(20,500,50,20) );
    nameLabel.setEditable( False );
    nameLabel.setDrawsBackground( False );
    nameLabel.setBordered( False );
    nameLabel.setStringValue( NSSTR('Name:') );
    rightView.addSubview( nameLabel );
    nameLabel.release;

    nameTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(80,500,250,22) );
    rightView.addSubview( nameTextField );
    nameTextField.release;

    saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(70,450,80,22) );
    saveButton.setBezelStyle( NSRoundedBezelStyle );
    saveButton.setTitle( NSSTR('Save') );
    rightView.addSubView( saveButton );
    saveButton.release;
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
  win.setDelegate( win );
  win.setTitlebarAppearsTransparent( True );
  win.setContentView( contentView );
  contentView.release;

  splitView:= NSSplitView.alloc.initWithFrame( frameRect );
  splitView.setVertical( True );
  splitView.setDividerStyle( NSSplitViewDividerStyleThin );
  contentView.addSubview( splitView );
  splitView.release;

  createLeftView;
  splitView.addSubview( leftView );
  leftView.release;

  createRightView;
  splitView.addSubview( rightView );
  rightView.release;

  Result:= win;
end;


class procedure TCloudOptionsUtil.show;
var
  win: NSWindow;
begin
  win:= self.createWindow;
  NSApplication(NSApp).runModalForWindow( win );
end;

end.


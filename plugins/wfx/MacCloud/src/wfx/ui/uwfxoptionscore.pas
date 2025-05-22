unit uWFXOptionsCore;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uWFXConfig;

type

  { TWFXConnectionConfigItem }

  TWFXConnectionConfigItem = objcclass( NSObject )
  private
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
    function getNotes: NSString;
      message 'TConnectionConfigItem_getNotes';
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
    procedure newConnection( sender: NSObject ); message 'TWFXConfigItemsController_newConnection:';
    procedure removeConnection( sender: NSObject ); message 'TWFXConfigItemsController_removeConnection:';
    procedure saveConnection( name: NSString ); message 'TWFXConfigItemsController_saveConnection:';
    function currentConfigItem: TWFXConnectionConfigItem; message 'TWFXConfigItemsController_currentConfigItem';
    procedure onSelectedConnectionChanged( const selectedIndex: Integer );
      message 'TWFXConfigItemsController_onSelectedConnectionChanged:';
  end;

  { TWFXPropertyView }

  TWFXPropertyView = objcclass( NSView )
  protected
    _controller: TWFXConfigItemsController;
    _logoImageView: NSImageView;
    _nameTextField: NSTextField;
    _saveButton: NSButton;
    _noteTextView: NSTextView;
  protected
    procedure addLabel( const title: NSString; const rect: NSRect ); message 'TWFXPropertyView_addLabel::';
    function addTextField( const rect: NSRect ): NSTextField; message 'TWFXPropertyView_addTextField:';
  public
    procedure setController( const controller: TWFXConfigItemsController ); message 'TWFXPropertyView_setController:';
    procedure loadConnectionProperties( const index: Integer ); message 'TWFXPropertyView_loadConnectionProperties:';
  end;

implementation

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

function TWFXConnectionConfigItem.getNotes: NSString;
var
  configClass: TWFXCloudDriverConfigClass;
  notes: String;
begin
  configClass:= WFXCloudDriverConfigManager.get( _driver.driverName );
  notes:= configClass.getNotes.Replace( '{driverName}', _driver.driverName );
  Result:= StringToNSString( notes );
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

procedure TWFXPropertyView.addLabel( const title: NSString; const rect: NSRect );
var
  nsLabel: NSTextField;
begin
  nsLabel:= NSTextField.alloc.initWithFrame( rect );
  nsLabel.setEditable( False );
  nsLabel.setDrawsBackground( False );
  nsLabel.setBordered( False );
  nsLabel.setStringValue( title );
  nsLabel.setAlignment( 2 );
  self.addSubview( nsLabel );
  nsLabel.release;
end;

function TWFXPropertyView.addTextField( const rect: NSRect ): NSTextField;
begin
  Result:= NSTextField.alloc.initWithFrame( rect );
  Result.cell.setScrollable( True );
  Result.cell.setWraps( False );
  self.addSubview( Result );
  Result.release;
end;

procedure TWFXPropertyView.setController(const controller: TWFXConfigItemsController);
begin
  _controller:= controller;
end;

procedure TWFXPropertyView.loadConnectionProperties(const index: Integer);
begin
end;

end.


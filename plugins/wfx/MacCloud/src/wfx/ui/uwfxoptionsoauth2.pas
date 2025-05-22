unit uWFXOptionsOAuth2;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uWFXPlugin, uWFXUtil,
  uWFXOptionsCore, uWFXOptionsCommonRS,
  uMiniUtil;

type

  { TWFXOAuth2PropertyView }

  TWFXOAuth2PropertyView = objcclass( TWFXPropertyView )
  protected
    _statusImageview: NSImageView;
    _connectButton: NSButton;
  private
    procedure connectOrDisconnect( sender: NSObject ); message 'TWFXOAuth2PropertyView_connectOrDisconnect:';
    procedure saveConnection( sender: NSObject ); message 'TWFXOAuth2PropertyView_saveConnection:';
    procedure initPropertyView; message 'TWFXOAuth2PropertyView_initPropertyView';
    procedure updateConnectStatus; message 'TWFXOAuth2PropertyView_updateConnectStatus';
  public
    procedure loadConnectionProperties( const index: Integer ); override;
    function initWithFrame(frameRect: NSRect): id; override;
  end;

implementation

{ TWFXOAuth2PropertyView }

procedure TWFXOAuth2PropertyView.loadConnectionProperties( const index: Integer );
var
  configItem: TWFXConnectionConfigItem;
begin
  configItem:= _controller.currentConfigItem;
  if configItem = nil then
    Exit;
  _logoImageView.setImage( TWFXPluginUtil.driverMainIcon(configItem.driver) );
  _nameTextField.setStringValue( configItem.name );
  _noteTextView.setString( configItem.getNotes );
  self.updateConnectStatus;
end;

procedure TWFXOAuth2PropertyView.updateConnectStatus;
var
  configItem: TWFXConnectionConfigItem;
  connectButtonText: String;
  statusImageName: NSString;
begin
  configItem:= _controller.currentConfigItem;
  if configItem.driver.authorized then begin
    statusImageName:= NSImageNameStatusAvailable;
    connectButtonText:= rsDisconnectButtonTitle;
  end else begin
    statusImageName:= NSImageNameStatusUnavailable;
    connectButtonText:= rsConnectButtonTitle;
  end;
  _statusImageview.setImage( NSImage.imageNamed(statusImageName) );
  _connectButton.setTitle( StringToNSString(connectButtonText) );
end;

procedure TWFXOAuth2PropertyView.connectOrDisconnect(sender: NSObject);
var
  configItem: TWFXConnectionConfigItem;
  driver: TCloudDriver;
begin
  try
    configItem:= _controller.currentConfigItem;
    driver:= configItem.driver;
    if driver.authorized then
      driver.unauthorize
    else
      driver.authorize;
  except
    on e: Exception do begin
      TLogUtil.logError( 'in TWFXOptionsWindow: ' + e.Message );
    end;
  end;

  try
    self.updateConnectStatus;
  except
    on e: Exception do begin
      TLogUtil.logError( 'in TCloudOptionsWindow: ' + e.Message );
    end;
  end;
end;

procedure TWFXOAuth2PropertyView.saveConnection(sender: NSObject);
var
  connectionName: NSString;
begin
  connectionName:= _nameTextField.stringValue;
  _controller.saveConnection( connectionName );
end;

procedure TWFXOAuth2PropertyView.initPropertyView;
begin
  _logoImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(224,560,32,32) );
  self.addSubview( _logoImageView );
  _logoImageView.release;

  addLabel( StringToNSString(rsNameLabel), NSMakeRect(20,510,80,20) );
  _nameTextField:= addTextField( NSMakeRect(106,510,290,22) );

  _statusImageview:= NSImageView.alloc.initWithFrame( NSMakeRect(406,513,16,16) );
  self.addSubview( _statusImageview );
  _statusImageview.release;

  _connectButton:= NSButton.alloc.initWithFrame( NSMakeRect(120,440,120,22) );
  _connectButton.setBezelStyle( NSRoundedBezelStyle );
  _connectButton.setTarget( self );
  _connectButton.setAction( ObjCSelector('TWFXOAuth2PropertyView_connectOrDisconnect:') );
  self.addSubView( _connectButton );
  _connectButton.release;

  _saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(260,440,120,22) );
  _saveButton.setBezelStyle( NSRoundedBezelStyle );
  _saveButton.setTitle( StringToNSString(rsSaveButtonTitle) );
  _saveButton.setTarget( self );
  _saveButton.setAction( ObjCSelector('TWFXOAuth2PropertyView_saveConnection:') );
  _saveButton.setKeyEquivalent( NSSTR(#13) );
  self.addSubView( _saveButton );
  _saveButton.release;

  _noteTextView:= NSTextView.alloc.initWithFrame( NSMakeRect(20,100,440,100) );
  _noteTextView.setFont( NSFont.systemFontOfSize(11));
  _noteTextView.setEditable( False );
  _noteTextView.setDrawsBackground( False );
  self.addSubView( _noteTextView );
  _noteTextView.release;
end;

function TWFXOAuth2PropertyView.initWithFrame(frameRect: NSRect): id;
begin
  Result:= inherited;
  self.initPropertyView;
end;

end.


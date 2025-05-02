unit uWFXOptionsOAuth2;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uWFXPlugin, uWFXUtil,
  uWFXOptionsCore,
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

const
  CONST_AUTH_NOTES =
    '1. Before successfully enabling the link, Double Command needs to obtain authorization from {driverName}'#13#13 +
    '2. Click the connect button to be redirected to the {driverName} official website in the Safari browser'#13#13 +
    '3. Please login your {driverName} account in Safari and authorize Double Commander to access'#13#13 +
    '4. The authorization is completed on the {driverName} official website, Double Command will not get your password';

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
  self.updateConnectStatus;
end;

procedure TWFXOAuth2PropertyView.updateConnectStatus;
var
  configItem: TWFXConnectionConfigItem;
  connectButtonText: String;
  statusImageName: NSString;
  notes: String;
begin
  configItem:= _controller.currentConfigItem;
  if configItem.driver.authorized then begin
    statusImageName:= NSImageNameStatusAvailable;
    connectButtonText:= 'Disconnect';
  end else begin
    statusImageName:= NSImageNameStatusUnavailable;
    connectButtonText:= 'Connect';
  end;
  _statusImageview.setImage( NSImage.imageNamed(statusImageName) );
  _connectButton.setTitle( StringToNSString(connectButtonText) );
  notes:= CONST_AUTH_NOTES.Replace( '{driverName}', configItem.driver.driverName );
  _noteTextView.setString( StringToNSString(notes) );
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
var
  nameLabel: NSTextField;
begin
  _logoImageView:= NSImageView.alloc.initWithFrame( NSMakeRect(200,530,32,32) );
  self.addSubview( _logoImageView );
  _logoImageView.release;

  nameLabel:= NSTextField.alloc.initWithFrame( NSMakeRect(20,480,50,20) );
  nameLabel.setEditable( False );
  nameLabel.setDrawsBackground( False );
  nameLabel.setBordered( False );
  nameLabel.setStringValue( NSSTR('Name:') );
  self.addSubview( nameLabel );
  nameLabel.release;

  _nameTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(80,480,250,22) );
  self.addSubview( _nameTextField );
  _nameTextField.release;

  _statusImageview:= NSImageView.alloc.initWithFrame( NSMakeRect(350,483,16,16) );
  self.addSubview( _statusImageview );
  _statusImageview.release;

  _connectButton:= NSButton.alloc.initWithFrame( NSMakeRect(80,430,100,22) );
  _connectButton.setBezelStyle( NSRoundedBezelStyle );
  _connectButton.setTarget( self );
  _connectButton.setAction( ObjCSelector('TWFXOAuth2PropertyView_connectOrDisconnect:') );
  self.addSubView( _connectButton );
  _connectButton.release;

  _saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(200,430,100,22) );
  _saveButton.setBezelStyle( NSRoundedBezelStyle );
  _saveButton.setTitle( NSSTR('Save') );
  _saveButton.setTarget( self );
  _saveButton.setAction( ObjCSelector('TWFXOAuth2PropertyView_saveConnection:') );
  self.addSubView( _saveButton );
  _saveButton.release;

  _noteTextView:= NSTextView.alloc.initWithFrame( NSMakeRect(20,100,400,100) );
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


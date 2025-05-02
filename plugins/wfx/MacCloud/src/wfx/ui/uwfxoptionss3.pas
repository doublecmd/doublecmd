unit uWFXOptionsS3;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uAWSCore,
  uWFXPlugin, uWFXUtil, uWFXOptionsCore,
  uMiniUtil;

type

  { TWFXS3PropertyView }

  TWFXS3PropertyView = objcclass( TWFXPropertyView )
  protected
    _regionTextField: NSTextField;
    _accessKeyIDTextField: NSTextField;
    _accessKeySecretTextField: NSTextField;
    _accessKeySecretPlainTextField: NSTextField;
    _bucketTextField: NSTextField;
    _secretButton: NSButton;
  private
    procedure saveConnection( sender: NSObject ); message 'TWFXS3PropertyView_saveConnection:';
    procedure initPropertyView; message 'TWFXS3PropertyView_initPropertyView';
    procedure togglePassword( sender: NSObject ); message 'TWFXS3PropertyView_togglePassword:';
  public
    procedure loadConnectionProperties( const index: Integer ); override;
    function initWithFrame(frameRect: NSRect): id; override;
  end;

implementation

const
  CONST_AUTH_NOTES =
    '1. AccessKeyID and SerectAccessKey will be saved in the macOS KeyChains to obtain system-level security.'#13#13 +
    '2. The confidential information can only be read by your own macOS permissions.';

{ TWFXS3PropertyView }

procedure TWFXS3PropertyView.loadConnectionProperties( const index: Integer );
var
  configItem: TWFXConnectionConfigItem;
  client: TAWSCloudDriver;
  data: TAWSConnectionData;
  accessKey: TAWSAccessKey;
begin
  configItem:= _controller.currentConfigItem;
  if configItem = nil then
    Exit;
  client:= TAWSCloudDriver( configItem.driver );
  data:= client.getConnectionData;
  accessKey:= client.getAccessKey;
  _logoImageView.setImage( TWFXPluginUtil.driverMainIcon(configItem.driver) );
  _nameTextField.setStringValue( configItem.name );
  _regionTextField.setStringValue( StringToNSString(data.region) );
  _accessKeyIDTextField.setStringValue( StringToNSString(accessKey.id) );
  _accessKeySecretTextField.setStringValue( StringToNSString(accessKey.secret) );
  _bucketTextField.setStringValue( StringToNSString(data.defaultBucket) );
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
  data.endPoint:= client.getEndPointByRegion( data.region );
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

  addLabel( 'Region:', NSMakeRect(20,440,120,20) );
  _regionTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,440,250,22) );
  self.addSubview( _regionTextField );
  _regionTextField.release;

  addLabel( 'Access Key ID:', NSMakeRect(20,400,120,20) );
  _accessKeyIDTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,400,250,22) );
  self.addSubview( _accessKeyIDTextField );
  _accessKeyIDTextField.release;

  addLabel( 'Serect Access Key:', NSMakeRect(20,360,120,20) );
  _accessKeySecretTextField:= NSSecureTextField.alloc.initWithFrame( NSMakeRect(146,360,250,22) );
  self.addSubview( _accessKeySecretTextField );
  _accessKeySecretTextField.release;
  _accessKeySecretPlainTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,360,250,22) );
  _accessKeySecretPlainTextField.setHidden( True );
  self.addSubview( _accessKeySecretPlainTextField );
  _accessKeySecretPlainTextField.release;

  _secretButton:= NSButton.alloc.initWithFrame( NSMakeRect(405,363,16,16) );
  _secretButton.setButtonType( NSToggleButton );
  _secretButton.setImage( NSImage.imageNamed( NSImageNameQuickLookTemplate ));
  _secretButton.setBordered( False );
  _secretButton.setTarget( self );
  _secretButton.setAction( ObjCSelector('TWFXS3PropertyView_togglePassword:') );
  self.addSubview( _secretButton );
  _secretButton.release;

  addLabel( 'Bucket:', NSMakeRect(20,320,120,20) );
  _bucketTextField:= NSTextField.alloc.initWithFrame( NSMakeRect(146,320,250,22) );
  self.addSubview( _bucketTextField );
  _bucketTextField.release;

  _saveButton:= NSButton.alloc.initWithFrame( NSMakeRect(200,260,100,22) );
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
  self.initPropertyView;
end;

end.

unit uOAuth2Core;

{$mode ObjFPC}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver,
  uMiniHttpClient, uMiniUtil;

type
  { TTokenCloudDriverConfig }

  TTokenCloudDriverConfig = class
  private
    _clientID: String;
    _listenURI: String;
  public
    constructor Create( const aClientID: String; const aListenURI: String );
    property clientID: String read _clientID;
    property listenURI: String read _listenURI;
  end;

  TTokenCloudDriverConfigPtr = ^TTokenCloudDriverConfig;

  { TCloudDriverToken }

  TCloudDriverToken = class
  private
    _access: String;
    _refresh: String;
    _accessExpirationTime: NSTimeInterval;
  private
  public
    constructor Create;
    constructor Create( const access: String; const refresh: String; const accessExpirationTime: NSTimeInterval );
    function clone: TCloudDriverToken;
  public
    procedure setExpiration( const seconds: Integer );
    procedure invalid;
    property access: String read _access write _access;
    property refresh: String read _refresh write _refresh;
    property accessExpirationTime: NSTimeInterval read _accessExpirationTime write _accessExpirationTime;
    function isValidAccessToken: Boolean;
    function isValidRefreshToken: Boolean;
  end;

  { TTokenCloudDriver }

  TTokenCloudDriver = class( TCloudDriver )
  public
    function getToken: TCloudDriverToken; virtual; abstract;
    procedure setToken( const token: TCloudDriverToken ); virtual; abstract;
  end;

implementation

{ TTokenCloudDriverConfig }

constructor TTokenCloudDriverConfig.Create(
  const aClientID: String;
  const aListenURI: String );
begin
  _clientID:= aClientID;
  _listenURI:= aListenURI;
end;

{ TCloudDriverToken }

function TCloudDriverToken.isValidAccessToken: Boolean;
var
  now: NSDate;
begin
  Result:= False;
  if _access = EmptyStr then
    Exit;
  now:= NSDate.new;
  if now.timeIntervalSince1970 < _accessExpirationTime then
    Result:= True;
  now.release;
end;

function TCloudDriverToken.isValidRefreshToken: Boolean;
begin
  Result:= _refresh <> EmptyStr;
end;

constructor TCloudDriverToken.Create;
begin
end;

constructor TCloudDriverToken.Create(const access: String; const refresh: String;
  const accessExpirationTime: NSTimeInterval);
begin
  _access:= access;
  _refresh:= refresh;
  _accessExpirationTime:= accessExpirationTime;
end;

function TCloudDriverToken.clone: TCloudDriverToken;
begin
  Result:= TCloudDriverToken.Create( _access, _refresh, _accessExpirationTime );
end;

procedure TCloudDriverToken.setExpiration(const seconds: Integer);
var
  now: NSDate;
  expirationDate: NSDate;
begin
  now:= NSDate.new;
  expirationDate:= now.dateByAddingTimeInterval( seconds - 300 );
  _accessExpirationTime:= expirationDate.timeIntervalSince1970;
  now.release;
end;

procedure TCloudDriverToken.invalid;
begin
  _access:= EmptyStr;
  _refresh:= EmptyStr;
  _accessExpirationTime:= 0;
end;

end.


unit uCloudDriver;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uMiniHttpClient;

type

  { TCloudDriverResult }

  TCloudDriverResult = class
  public
    httpResult: TMiniHttpResult;
    resultMessage: String;
  end;

  { ECloudDriverException }

  ECloudDriverException = class( Exception );
  ECloudDriverTokenException = class( ECloudDriverException );
  ECloudDriverConflictException = class( ECloudDriverException );
  ECloudDriverPermissionException = class( ECloudDriverException );
  ECloudDriverRateLimitException = class( ECloudDriverException );

  { TCloudDriverConfig }

  TCloudDriverConfig = class
  private
    _clientID: String;
    _listenURI: String;
  public
    constructor Create( const clientID: String; const listenURI: String );
    property clientID: String read _clientID;
    property listenURI: String read _listenURI;
  end;

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
    function isValidFreshToken: Boolean;
  end;

implementation

{ TCloudDriverConfig }

constructor TCloudDriverConfig.Create(
  const clientID: String;
  const listenURI: String );
begin
  _clientID:= clientID;
  _listenURI:= listenURI;
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

function TCloudDriverToken.isValidFreshToken: Boolean;
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
end;

end.


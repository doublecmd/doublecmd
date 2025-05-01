unit uOAuth2Client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uOAuth2Core, uOAuth2Auth,
  uMiniHttpClient, uMiniUtil;

type

  { TOAuth2SessionCloudDriver }

  TOAuth2SessionCloudDriver = class( TTokenCloudDriver )
  protected
    _config: TTokenCloudDriverConfig;
    _authSession: TCloudDriverOAuth2Session;
  public
    constructor Create( const config: TTokenCloudDriverConfig );
    destructor Destroy; override;
  public
    function authorize: Boolean; override;
    procedure unauthorize; override;
    function authorized: Boolean; override;
    function getToken: TCloudDriverToken; override;
    procedure setToken( const token: TCloudDriverToken ); override;
  end;

implementation

{ TOAuth2SessionCloudDriver }

constructor TOAuth2SessionCloudDriver.Create(
  const config: TTokenCloudDriverConfig );
begin
  _config:= config;
end;

destructor TOAuth2SessionCloudDriver.Destroy;
begin
  FreeAndNil( _authSession );
end;

function TOAuth2SessionCloudDriver.authorize: Boolean;
begin
  Result:= _authSession.authorize;
end;

procedure TOAuth2SessionCloudDriver.unauthorize;
begin
  _authSession.unauthorize;
end;

function TOAuth2SessionCloudDriver.authorized: Boolean;
begin
  Result:= _authSession.authorized;
end;

function TOAuth2SessionCloudDriver.getToken: TCloudDriverToken;
begin
  Result:= _authSession.getToken;
end;

procedure TOAuth2SessionCloudDriver.setToken(const token: TCloudDriverToken);
begin
  _authSession.setToken( token );
end;

end.


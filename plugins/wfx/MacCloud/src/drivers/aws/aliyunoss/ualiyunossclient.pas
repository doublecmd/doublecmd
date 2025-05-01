unit uAliyunOSSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver, uAWSAuth, uS3Client;

type
  
  { TAliyunOSSClient }

  TAliyunOSSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    constructor Create( const config: TS3Config );
    function clone: TCloudDriver; override;
  end;

var
  aliyunOSSConfig: TS3Config;

implementation

{ TAliyunOSSClient }

class function TAliyunOSSClient.driverName: String;
begin
  Result:= 'AliyunOSS';
end;

class function TAliyunOSSClient.createInstance: TCloudDriver;
begin
  Result:= TAliyunOSSClient.Create( aliyunOSSConfig );
end;

constructor TAliyunOSSClient.Create( const config: TS3Config );
var
  params: TAWSAuthSessionParams;
begin
  Inherited Create( config );
  params:= Default( TAWSAuthSessionParams );
  params.config:= config;
  _authSession:= TAWSAuthSession.Create( params );
end;

function TAliyunOSSClient.clone: TCloudDriver;
var
  newClient: TAliyunOSSClient;
begin
  newClient:= TAliyunOSSClient.Create( _config );
  newClient._authSession:= TAWSAuthSession( _authSession.clone(newClient) );
  Result:= newClient;
end;

end.


unit uTencentCOSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver, uAWSAuth, uS3Client;

type

  { TTencentCOSClient }

  TTencentCOSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function clone: TCloudDriver; override;
  end;

implementation

{ TTencentCOSClient }

class function TTencentCOSClient.driverName: String;
begin
  Result:= 'TencentCOS';
end;

class function TTencentCOSClient.createInstance: TCloudDriver;
begin
  Result:= TTencentCOSClient.Create;
end;

function TTencentCOSClient.clone: TCloudDriver;
var
  newClient: TTencentCOSClient;
begin
  newClient:= TTencentCOSClient.Create;
  newClient._authSession.Free;
  newClient._authSession:= TAWSAuthSession( _authSession.clone(newClient) );
  Result:= newClient;
end;

end.


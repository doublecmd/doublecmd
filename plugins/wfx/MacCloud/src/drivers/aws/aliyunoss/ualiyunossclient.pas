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
    function clone: TCloudDriver; override;
  end;

implementation

{ TAliyunOSSClient }

class function TAliyunOSSClient.driverName: String;
begin
  Result:= 'AliyunOSS';
end;

class function TAliyunOSSClient.createInstance: TCloudDriver;
begin
  Result:= TAliyunOSSClient.Create;
end;

function TAliyunOSSClient.clone: TCloudDriver;
var
  newClient: TAliyunOSSClient;
begin
  newClient:= TAliyunOSSClient.Create;
  newClient._authSession.Free;
  newClient._authSession:= TAWSAuthSession( _authSession.clone(newClient) );
  Result:= newClient;
end;

end.


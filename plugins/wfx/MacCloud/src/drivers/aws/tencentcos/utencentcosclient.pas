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
    function getConcreteClass: TCloudDriverClass; override;
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

function TTencentCOSClient.getConcreteClass: TCloudDriverClass;
begin
  Result:= TTencentCOSClient;
end;

end.


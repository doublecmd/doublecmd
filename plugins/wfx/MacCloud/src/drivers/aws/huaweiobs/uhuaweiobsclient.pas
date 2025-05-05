unit uHuaweiOBSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver, uAWSAuth, uS3Client;

type

  { THuaweiOBSClient }

  THuaweiOBSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function getConcreteClass: TCloudDriverClass; override;
  end;

implementation

{ THuaweiOBSClient }

class function THuaweiOBSClient.driverName: String;
begin
  Result:= 'HuaweiOBS';
end;

class function THuaweiOBSClient.createInstance: TCloudDriver;
begin
  Result:= THuaweiOBSClient.Create;
end;

function THuaweiOBSClient.getConcreteClass: TCloudDriverClass;
begin
  Result:= THuaweiOBSClient;
end;

end.


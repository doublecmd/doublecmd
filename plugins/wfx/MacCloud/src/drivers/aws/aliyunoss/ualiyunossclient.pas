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
    function getConcreteClass: TCloudDriverClass; override;
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

function TAliyunOSSClient.getConcreteClass: TCloudDriverClass;
begin
  Result:= TAliyunOSSClient;
end;

end.


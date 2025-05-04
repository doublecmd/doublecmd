unit uAmazonS3Client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver, uAWSAuth, uS3Client;

type
  
  { TAmazonS3Client }

  TAmazonS3Client = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function getConcreteClass: TCloudDriverClass; override;
  end;

implementation

{ TAmazonS3Client }

class function TAmazonS3Client.driverName: String;
begin
  Result:= 'AmazonS3';
end;

class function TAmazonS3Client.createInstance: TCloudDriver;
begin
  Result:= TAmazonS3Client.Create;
end;

function TAmazonS3Client.getConcreteClass: TCloudDriverClass;
begin
  Result:= TAmazonS3Client;
end;

end.


unit uS3CompatibleClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver, uS3Client;

type
  
  { TS3CompatibleClient }

  TS3CompatibleClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function getConcreteClass: TCloudDriverClass; override;
  end;

implementation

{ TS3CompatibleClient }

class function TS3CompatibleClient.driverName: String;
begin
  Result:= 'S3Compatible';
end;

class function TS3CompatibleClient.createInstance: TCloudDriver;
begin
  Result:= TS3CompatibleClient.Create;
end;

function TS3CompatibleClient.getConcreteClass: TCloudDriverClass;
begin
  Result:= TS3CompatibleClient;
end;

end.


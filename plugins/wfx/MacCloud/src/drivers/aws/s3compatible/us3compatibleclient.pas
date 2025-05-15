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
  protected
    function autoBuildBuckets: TS3Buckets; override;
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

function TS3CompatibleClient.autoBuildBuckets: TS3Buckets;
var
  bucket: TS3Bucket;
begin
  bucket:= TS3Bucket.Create;
  bucket.connectionData:= self.getDefaultConnectionData;
  Result:= TS3Buckets.Create;
  Result.add( bucket );
end;

end.


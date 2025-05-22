unit uBackBlazeB2Client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type

  { TBackBlazeB2Client }

  TBackBlazeB2Client = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ TBackBlazeB2Client }

class function TBackBlazeB2Client.driverName: String;
begin
  Result:= 'BackBlazeB2';
end;

class function TBackBlazeB2Client.createInstance: TCloudDriver;
begin
  Result:= TBackBlazeB2Client.Create;
end;

function TBackBlazeB2Client.autoBuildBuckets: TS3Buckets;
var
  bucket: TS3Bucket;
begin
  bucket:= TS3Bucket.Create;
  bucket.connectionData:= self.getDefaultConnectionData;
  Result:= TS3Buckets.Create;
  Result.add( bucket );
end;

end.


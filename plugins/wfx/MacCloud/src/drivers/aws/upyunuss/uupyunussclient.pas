unit uUpyunUSSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type

  { TUpyunUSSGetAllBucketsSession }

  TUpyunUSSGetAllBucketsSession = class( TS3GetAllBucketsSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
  end;

  { TUpyunUSSClient }

  TUpyunUSSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  protected
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ TUpyunUSSGetAllBucketsSession }

procedure TUpyunUSSGetAllBucketsSession.constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement );
begin
  bucket.connectionData.region:= 'global';
  bucket.connectionData.endPoint:= self.getEndPointOfRegion( bucket.connectionData.region );
end;

function TUpyunUSSGetAllBucketsSession.getConnectionDataOfService: TAWSConnectionData;
begin
  Result.region:= '';
  Result.endPoint:= 's3.api.upyun.com';
  Result.bucketName:= '';
end;

function TUpyunUSSGetAllBucketsSession.getEndPointOfRegion( const region: String ): String;
begin
  Result:= 's3.api.upyun.com'
end;

{ TUpyunUSSClient }

class function TUpyunUSSClient.driverName: String;
begin
  Result:= 'UpyunUSS';
end;

class function TUpyunUSSClient.createInstance: TCloudDriver;
begin
  Result:= TUpyunUSSClient.Create;
end;

function TUpyunUSSClient.autoBuildBuckets: TS3Buckets;
var
  session: TS3GetAllBucketsSession = nil;
begin
  try
    session:= TUpyunUSSGetAllBucketsSession.Create( _authSession );
    Result:= session.listBuckets;
  finally
    FreeAndNil( session );
  end;
end;

end.


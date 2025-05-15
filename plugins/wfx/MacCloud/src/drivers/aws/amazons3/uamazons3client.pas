unit uAmazonS3Client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type
  
  { TAmazonS3GetAllBucketsSession }

  TAmazonS3GetAllBucketsSession = class( TS3GetAllBucketsWithRegionFunctionSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
  end;

  { TAmazonS3Client }

  TAmazonS3Client = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  protected
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ TAmazonS3GetAllBucketsSession }

procedure TAmazonS3GetAllBucketsSession.constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement );
begin
  bucket.connectionData.region:= self.getRegionOfBucket( bucket.connectionData.bucketName );
  bucket.connectionData.endPoint:= self.getEndPointOfRegion( bucket.connectionData.region );
end;

function TAmazonS3GetAllBucketsSession.getConnectionDataOfService: TAWSConnectionData;
begin
  Result.region:= 'us-east-1';
  Result.endPoint:= 's3.amazonaws.com';
  Result.bucketName:= '';
end;

function TAmazonS3GetAllBucketsSession.getEndPointOfRegion( const region: String ): String;
begin
  Result:= 's3.' + region + '.amazonaws.com';
end;

{ TAmazonS3Client }

class function TAmazonS3Client.driverName: String;
begin
  Result:= 'AmazonS3';
end;

class function TAmazonS3Client.createInstance: TCloudDriver;
begin
  Result:= TAmazonS3Client.Create;
end;

function TAmazonS3Client.autoBuildBuckets: TS3Buckets;
var
  session: TS3GetAllBucketsSession = nil;
begin
  try
    session:= TAmazonS3GetAllBucketsSession.Create( _authSession );
    Result:= session.listBuckets;
  finally
    FreeAndNil( session );
  end;
end;

end.


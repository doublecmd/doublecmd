unit uAmazonS3Client;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniHttpClient, uMiniUtil;

type
  
  { TAmazonS3GetAllBucketsSession }

  TAmazonS3GetAllBucketsSession = class( TS3GetAllBucketsSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
    function getRegionOfBucket(const name: String): String;
  end;

  { TAmazonS3Client }

  TAmazonS3Client = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  protected
    function getConcreteClass: TCloudDriverClass; override;
    function getAllBuckets: TS3Buckets; override;
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

function TAmazonS3GetAllBucketsSession.getRegionOfBucket( const name: String ): String;
var
  connectionData: TAWSConnectionData;
  endPoint: String;
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
begin
  try
    connectionData:= self.getConnectionDataOfService;
    endPoint:= self.getEndPointOfRegion( connectionData.region );
    urlString:= 'https://' + name + '.' + endPoint + '/';
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.HEAD );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;
    Result:= httpResult.getHeader( AWSConst.HEADER.BUCKET_REGION );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
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

function TAmazonS3Client.getConcreteClass: TCloudDriverClass;
begin
  Result:= TAmazonS3Client;
end;

function TAmazonS3Client.getAllBuckets: TS3Buckets;
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


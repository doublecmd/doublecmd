unit uAliyunOSSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type
  
  { TAliyunOSSGetAllBucketsSession }

  TAliyunOSSGetAllBucketsSession = class( TS3GetAllBucketsSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
  end;

  { TAliyunOSSClient }

  TAliyunOSSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ TAliyunOSSGetAllBucketsSession }

procedure TAliyunOSSGetAllBucketsSession.constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement );
begin
  bucket.connectionData.region:= TXmlUtil.getString( xmlBucket, 'Region' );
  bucket.connectionData.endPoint:= TXmlUtil.getString( xmlBucket, 'ExtranetEndpoint' );
end;

function TAliyunOSSGetAllBucketsSession.getConnectionDataOfService: TAWSConnectionData;
begin
  Result.region:= '';
  Result.endPoint:= 'oss.aliyuncs.com';
  Result.bucketName:= '';
end;

function TAliyunOSSGetAllBucketsSession.getEndPointOfRegion( const region: String ): String;
begin
  Result:= 'oss-' + region + '.aliyuncs.com';
end;

{ TAliyunOSSClient }

class function TAliyunOSSClient.driverName: String;
begin
  Result:= 'AliyunOSS';
end;

class function TAliyunOSSClient.createInstance: TCloudDriver;
begin
  Result:= TAliyunOSSClient.Create;
end;

function TAliyunOSSClient.autoBuildBuckets: TS3Buckets;
var
  session: TS3GetAllBucketsSession = nil;
begin
  try
    session:= TAliyunOSSGetAllBucketsSession.Create( _authSession );
    Result:= session.listBuckets;
  finally
    FreeAndNil( session );
  end;
end;

end.


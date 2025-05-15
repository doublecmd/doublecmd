unit uHuaweiOBSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type

  { THuaweiOBSGetAllBucketsSession }

  THuaweiOBSGetAllBucketsSession = class( TS3GetAllBucketsWithRegionFunctionSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
  end;

  { THuaweiOBSClient }

  THuaweiOBSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ THuaweiOBSGetAllBucketsSession }

procedure THuaweiOBSGetAllBucketsSession.constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement );
begin
  bucket.connectionData.region:= TXmlUtil.getString( xmlBucket, 'Location' );
  if bucket.connectionData.region = EmptyStr then
    bucket.connectionData.region:= self.getRegionOfBucket( bucket.connectionData.bucketName );
  bucket.connectionData.endPoint:= self.getEndPointOfRegion( bucket.connectionData.region );
end;

function THuaweiOBSGetAllBucketsSession.getConnectionDataOfService: TAWSConnectionData;
begin
  Result.region:= '';
  Result.endPoint:= 'obs.myhuaweicloud.com';
  Result.bucketName:= '';
end;

function THuaweiOBSGetAllBucketsSession.getEndPointOfRegion( const region: String ): String;
begin
  if region = EmptyStr then
    Result:= 'obs.myhuaweicloud.com'
  else
    Result:= 'obs.' + region + '.myhuaweicloud.com';
end;

{ THuaweiOBSClient }

class function THuaweiOBSClient.driverName: String;
begin
  Result:= 'HuaweiOBS';
end;

class function THuaweiOBSClient.createInstance: TCloudDriver;
begin
  Result:= THuaweiOBSClient.Create;
end;

function THuaweiOBSClient.autoBuildBuckets: TS3Buckets;
var
  session: TS3GetAllBucketsSession = nil;
begin
  try
    session:= THuaweiOBSGetAllBucketsSession.Create( _authSession );
    Result:= session.listBuckets;
  finally
    FreeAndNil( session );
  end;
end;

end.


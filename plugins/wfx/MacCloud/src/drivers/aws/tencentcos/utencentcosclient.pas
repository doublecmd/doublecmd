unit uTencentCOSClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type

  { TTencentCOSGetAllBucketsSession }

  TTencentCOSGetAllBucketsSession = class( TS3GetAllBucketsSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
  end;

  { TTencentCOSClient }

  TTencentCOSClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ TTencentCOSGetAllBucketsSession }

procedure TTencentCOSGetAllBucketsSession.constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement );
begin
  bucket.connectionData.region:= TXmlUtil.getString( xmlBucket, 'Location' );
  bucket.connectionData.endPoint:= self.getEndPointOfRegion( bucket.connectionData.region );
end;

function TTencentCOSGetAllBucketsSession.getConnectionDataOfService: TAWSConnectionData;
begin
  Result.region:= '*';
  Result.endPoint:= 'service.cos.myqcloud.com';
  Result.bucketName:= '';
end;

function TTencentCOSGetAllBucketsSession.getEndPointOfRegion( const region: String ): String;
begin
  Result:= 'cos.' + region + '.myqcloud.com';
end;

{ TTencentCOSClient }

class function TTencentCOSClient.driverName: String;
begin
  Result:= 'TencentCOS';
end;

class function TTencentCOSClient.createInstance: TCloudDriver;
begin
  Result:= TTencentCOSClient.Create;
end;

function TTencentCOSClient.autoBuildBuckets: TS3Buckets;
var
  session: TS3GetAllBucketsSession = nil;
begin
  try
    session:= TTencentCOSGetAllBucketsSession.Create( _authSession );
    Result:= session.listBuckets;
  finally
    FreeAndNil( session );
  end;
end;

end.


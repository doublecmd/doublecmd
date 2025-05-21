unit uQiniuKODOClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uS3Client,
  uMiniUtil;

type

  { TQiniuKODOGetAllBucketsSession }

  TQiniuKODOGetAllBucketsSession = class( TS3GetAllBucketsSession )
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); override;
    function getConnectionDataOfService: TAWSConnectionData; override;
    function getEndPointOfRegion(const region: String): String; override;
  end;

  { TQiniuKODOClient }

  TQiniuKODOClient = class( TS3Client )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  protected
    function autoBuildBuckets: TS3Buckets; override;
  end;

implementation

{ TQiniuKODOGetAllBucketsSession }

procedure TQiniuKODOGetAllBucketsSession.constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement );
begin
  bucket.connectionData.region:= TXmlUtil.getString( xmlBucket, 'LocationConstraint' );
  bucket.connectionData.endPoint:= self.getEndPointOfRegion( bucket.connectionData.region );
end;

function TQiniuKODOGetAllBucketsSession.getConnectionDataOfService: TAWSConnectionData;
begin
  Result.region:= 'cn-east-1';
  Result.endPoint:= 's3.cn-east-1.qiniucs.com';
  Result.bucketName:= '';
end;

function TQiniuKODOGetAllBucketsSession.getEndPointOfRegion( const region: String ): String;
begin
  Result:= 's3.' + region + '.qiniucs.com';
end;

{ TQiniuKODOClient }

class function TQiniuKODOClient.driverName: String;
begin
  Result:= 'QiniuKODO';
end;

class function TQiniuKODOClient.createInstance: TCloudDriver;
begin
  Result:= TQiniuKODOClient.Create;
end;

function TQiniuKODOClient.autoBuildBuckets: TS3Buckets;
var
  session: TS3GetAllBucketsSession = nil;
begin
  try
    session:= TQiniuKODOGetAllBucketsSession.Create( _authSession );
    Result:= session.listBuckets;
  finally
    FreeAndNil( session );
  end;
end;

end.


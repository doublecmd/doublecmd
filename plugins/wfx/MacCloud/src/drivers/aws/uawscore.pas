unit uAWSCore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver;

type

  { TAWSCredentialConfig }

  TAWSCredentialConfig = class
  private
    _versionAlgorithm: String;
    _prefix: String;
    _service: String;
    _request: String;
  public
    constructor Create(
      const versionAlgorithm: String;
      const prefix: String;
      const service: String;
      const request: String );
    property versionAlgorithm: String read _versionAlgorithm;
    property prefix: String read _prefix;
    property service: String read _service;
    property request: String read _request;
  end;

  { TAWSAccessKey }

  TAWSAccessKey = class
  private
    _id: String;
    _secret: String;
    _token: String;
  public
    constructor Create( const id: String; const secret: String; const token: String );
    function clone: TAWSAccessKey;
    property id: String read _id;
    property secret: String read _secret;
    property token: String read _token;
  end;

  { TAWSConnectionData }

  TAWSConnectionData = record
    region: String;
    endPoint: String;
    bucketName: String;
  end;

  { TAWSCloudDriver }

  TAWSCloudDriver = class( TCloudDriver )
  public
    function getDefaultConnectionData: TAWSConnectionData; virtual; abstract;
    procedure setDefaultConnectionData( const connectionData: TAWSConnectionData ); virtual; abstract;
    function getAccessKey: TAWSAccessKey; virtual; abstract;
    procedure setAccessKey( const accessKey: TAWSAccessKey ); virtual; abstract;
  end;

  { TAWSConstHeader }

  TAWSConstHeader = record
    DATE: String;
    BUCKET_REGION: String;
    SECURITY_TOKEN: String;
    COPY_SOURCE: String;
    CONTENT_SHA256: String;
    CONTENT_SHA256_DEFAULT_VALUE: String;
  end;

  { TAWSConst }

  TAWSConst = record
    HEADER: TAWSConstHeader;
  end;

const
  AWSConst: TAWSConst = (
    HEADER: (
      DATE: 'x-amz-date';
      BUCKET_REGION: 'x-amz-bucket-region';
      SECURITY_TOKEN: 'x-amz-security-token';
      COPY_SOURCE: 'x-amz-copy-source';
      CONTENT_SHA256: 'x-amz-content-sha256';
      CONTENT_SHA256_DEFAULT_VALUE: 'UNSIGNED-PAYLOAD';
    );
  );

implementation

{ TAWSCredentialConfig }

constructor TAWSCredentialConfig.Create(
  const versionAlgorithm: String;
  const prefix: String;
  const service: String;
  const request: String);
begin
  _versionAlgorithm:= versionAlgorithm;
  _prefix:= prefix;
  _service:= service;
  _request:= request;
end;

{ TAWSAccessKey }

constructor TAWSAccessKey.Create( const id: String; const secret: String; const token: String );
begin
  _id:= id;
  _secret:= secret;
  _token:= token;
end;

function TAWSAccessKey.clone: TAWSAccessKey;
begin
  Result:= TAWSAccessKey.Create( _id, _secret, _token );
end;

end.


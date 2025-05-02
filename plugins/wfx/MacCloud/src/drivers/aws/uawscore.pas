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
  public
    constructor Create( const id: String; const secret: String );
    function clone: TAWSAccessKey;
    property id: String read _id;
    property secret: String read _secret;
  end;

  { TAWSConnectionData }

  TAWSConnectionData = record
    region: String;
    endPoint: String;
    defaultBucket: String;
  end;

  { TAWSCloudDriver }

  TAWSCloudDriver = class( TCloudDriver )
  public
    function getConnectionData: TAWSConnectionData; virtual; abstract;
    procedure setConnectionData( const connectionData: TAWSConnectionData ); virtual; abstract;
    function getAccessKey: TAWSAccessKey; virtual; abstract;
    procedure setAccessKey( const accessKey: TAWSAccessKey ); virtual; abstract;
    function getEndPointByRegion( const region: String ): String; virtual; abstract;
  end;

  { TAWSConstHeader }

  TAWSConstHeader = record
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

constructor TAWSAccessKey.Create(  const id: String; const secret: String );
begin
  _id:= id;
  _secret:= secret;
end;

function TAWSAccessKey.clone: TAWSAccessKey;
begin
  Result:= TAWSAccessKey.Create( _id, _secret );
end;

end.


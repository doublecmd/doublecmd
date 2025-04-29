unit uAWSCore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCloudDriver;

type

  { TAWSConfig }

  TAWSConfig = class
  public
    credentialVersionAlgorithm: String;
    credentialPrefix: String;
    credentialService: String;
    credentialRequest: String;
  end;

  { TAWSAccessKey }

  TAWSAccessKey = class
  private
    _id: String;
    _secret: String;
  public
    constructor Create( const id: String; const secret: String );
    property id: String read _id;
    property secret: String read _secret;
  end;

  { TAWSCloudDriver }

  TAWSCloudDriver = class( TCloudDriver )
  public
    function getAccessKey: TAWSAccessKey; virtual; abstract;
    procedure setAccessKey( const accessKey: TAWSAccessKey ); virtual; abstract;
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

{ TAWSAccessKey }

constructor TAWSAccessKey.Create(  const id: String; const secret: String );
begin
  _id:= id;
  _secret:= secret;
end;

end.


unit uAWSCore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAWSConfig }

  TAWSConfig = class
  public
    accessKeyID: String;
    accessKeySecret: String;
    endPoint: String;
    credentialVersionAlgorithm: String;
    credentialPrefix: String;
    credentialRegion: String;
    credentialService: String;
    credentialRequest: String;
    credentialPrefixAndSecret: String;
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

end.


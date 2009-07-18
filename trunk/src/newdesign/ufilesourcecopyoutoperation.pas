unit uFileSourceCopyOutOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation;

type

  {en
     Operation that copies files into another file source from a file source of specific type
     (from file system for TFileSystemCopyOutOperation,
      from network for TNetworkCopyOutOperation, etc.).

     Source file source should match the class type.
     Target file source must be a file system file source.
     (Or is it enough if it's a file source with directly accessible files ? (DirectAccess flag))

     Example meaning of this operation:
     - archive: unpack
     - network: download
  }
  TFileSourceCopyOutOperation = class(TFileSourceOperation)

  public
    constructor Create; override;
    destructor Destroy; override;

  end;

implementation

constructor TFileSourceCopyOutOperation.Create;
begin
  inherited;
end;

destructor TFileSourceCopyOutOperation.Destroy;
begin
  inherited;
end;

end.


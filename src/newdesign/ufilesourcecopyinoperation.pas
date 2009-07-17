unit uFileSourceCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation;

type

  {en
     Operation that copies files from another file source into a file source of specific type
     (to file system for TFileSystemCopyInOperation,
      to network for TNetworkCopyInOperation, etc.).

     Source file source must be a file system file source.
     (Or is it enough if it's a file source with directly accessible files ? (DirectAccess flag))
     Target file source should match the class type.

     Example meaning of this operation:
     - archive: pack
     - network: upload
  }
  TFileSourceCopyInOperation = class(TFileSourceOperation)

  public
    constructor Create; virtual;
    destructor Destroy; override;

  end;

implementation

constructor TFileSourceCopyInOperation.Create;
begin
end;

destructor TFileSourceCopyInOperation.Destroy;
begin
end;

end.


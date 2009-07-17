unit uFileSourceDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation;

type

  {en
     Operation that deletes files from an arbitrary file source.
     File source should match the class type.
  }
  TFileSourceDeleteOperation = class(TFileSourceOperation)

  public
    constructor Create; virtual;
    destructor Destroy; override;

  end;

implementation

constructor TFileSourceDeleteOperation.Create;
begin
end;

destructor TFileSourceDeleteOperation.Destroy;
begin
end;

end.


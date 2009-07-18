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
    constructor Create; override;
    destructor Destroy; override;

  end;

implementation

constructor TFileSourceDeleteOperation.Create;
begin
  inherited;
end;

destructor TFileSourceDeleteOperation.Destroy;
begin
  inherited;
end;

end.


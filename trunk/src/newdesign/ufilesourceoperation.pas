unit uFileSourceOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{,
  uFileSource};

type

  TFileSourceOperation = class
//  private
//    FFileSource: TFileSource;

  public
//    constructor Create(FileSource: TFileSource); virtual; reintroduce;
    procedure Execute; virtual abstract;
  end;

implementation

{constructor TFileSourceOperation.Create(FileSource: TFileSource);
begin
  FFileSource := FileSource;
  inherited Create;
end;}

end.


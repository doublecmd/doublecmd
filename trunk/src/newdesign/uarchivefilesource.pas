unit uArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLocalFileSource;

type

  IArchiveFileSource = interface(ILocalFileSource)
    ['{13A8637C-FFDF-46B0-B5B4-E7C6851C157A}']

    function GetCurrentAddress: String;

    property ArchiveFileName: String read GetCurrentAddress;
  end;

  TArchiveFileSource = class(TLocalFileSource, IArchiveFileSource)
  private

  protected
    function GetCurrentAddress: String;

  public
    constructor Create(anArchiveFileName: String); virtual reintroduce overload;
    constructor Create(anArchiveFileName: String; aPath: String); virtual reintroduce overload;

    property ArchiveFileName: String read FCurrentAddress;
  end;

implementation

constructor TArchiveFileSource.Create(anArchiveFileName: String);
begin
  Create(anArchiveFileName, PathDelim);
end;

constructor TArchiveFileSource.Create(anArchiveFileName: String; aPath: String);
begin
  FCurrentAddress := anArchiveFileName;
  inherited Create;
end;

function TArchiveFileSource.GetCurrentAddress: String;
begin
  Result := FCurrentAddress;
end;

end.


unit uNetworkFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uGioFileSource;

type

  INetworkFileSource = interface(IGioFileSource)
    ['{C7128E35-76FC-4635-842D-4091AB4AC520}']
  end;

  { TNetworkFileSource }

  TNetworkFileSource = class(TGioFileSource, INetworkFileSource)
  public
    constructor Create; override;
    class function GetMainIcon(out Path: String): Boolean; override;
    class function IsSupportedPath(const Path: String): Boolean; override;
  end;

implementation

uses
  DCStrUtils;

{ TNetworkFileSource }

constructor TNetworkFileSource.Create;
begin
  inherited Create;
  FCurrentAddress:= 'network://';
end;

class function TNetworkFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Result:= True;
  Path:= 'network-workgroup';
end;

class function TNetworkFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= StrBegins(Path, 'network://');
end;

end.


unit uNetworkFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uFile, uFileSourceOperationTypes, uGioFileSource, uGio2;

type

  INetworkFileSource = interface(IGioFileSource)
    ['{C7128E35-76FC-4635-842D-4091AB4AC520}']
  end;

  { TNetworkFileSource }

  TNetworkFileSource = class(TGioFileSource, INetworkFileSource)
  public
    constructor Create; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    class function GetMainIcon(out Path: String): Boolean; override;
    class function IsSupportedPath(const Path: String): Boolean; override;
    class function CreateFile(const APath: String; AFolder: PGFile; AFileInfo: PGFileInfo): TFile; override;
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

function TNetworkFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result:= [fsoList, fsoExecute];
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

class function TNetworkFileSource.CreateFile(const APath: String;
  AFolder: PGFile; AFileInfo: PGFileInfo): TFile;
var
  ADisplayName: String;
begin
  Result:= inherited CreateFile(APath, AFolder, AFileInfo);
  ADisplayName:= g_file_info_get_display_name(AFileInfo);
  if Length(ADisplayName) > 0 then Result.Name:= ADisplayName;
end;

end.


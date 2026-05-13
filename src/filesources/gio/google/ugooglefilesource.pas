unit uGoogleFileSource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uFile, uGioFileSource, uGio2;

type

  IGoogleFileSource = interface(IGioFileSource)
    ['{02577ABB-81F1-40B2-AEDE-7E28AAFEA8B5}']
  end;

  { TGoogleFileSource }

  TGoogleFileSource = class(TGioFileSource, IGoogleFileSource)
  public
    class function IsSupportedPath(const Path: String): Boolean; override;
    class function CreateFile(const APath: String; AFolder, AFile: PGFile; AFileInfo: PGFileInfo): TFile; override;
  end;

implementation

uses
  DCStrUtils;

{ TGoogleFileSource }

class function TGoogleFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= StrBegins(Path, 'google-drive://');
end;

class function TGoogleFileSource.CreateFile(const APath: String; AFolder,
  AFile: PGFile; AFileInfo: PGFileInfo): TFile;
var
  ADisplayName: String;
begin
  Result:= inherited CreateFile(APath, AFolder, AFile, AFileInfo);
  ADisplayName:= g_file_info_get_display_name(AFileInfo);
  if Length(ADisplayName) > 0 then Result.Name:= ADisplayName;
end;

end.


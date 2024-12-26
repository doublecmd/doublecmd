unit uiCloudDriver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uMountedFileSource,
  uDCUtils, uMyDarwin;

type
  { TiCloudDriverFileSource }

  TiCloudDriverFileSource = class(TMountedFileSource)
  public
    function getDefaultPointForPath(const path: String): String; override;
    function GetRootDir(sPath : String): String; override;
    function IsSystemFile(aFile: TFile): Boolean; override;
    function IsPathAtRoot(Path: String): Boolean; override;
  end;

implementation

const
  iCLOUD_DRIVER_PATH = '~/Library/Mobile Documents/com~apple~CloudDocs';

{ TiCloudDriverFileSource }

function TiCloudDriverFileSource.getDefaultPointForPath(const path: String): String;
begin
  Result:= getMacOSDisplayNameFromPath( path );
end;

function TiCloudDriverFileSource.GetRootDir(sPath: String): String;
var
  path: String;
  displayName: String;
begin
  path:= uDCUtils.ReplaceTilde( iCLOUD_DRIVER_PATH );
  displayName:= getMacOSDisplayNameFromPath( path );
  Result:= PathDelim + PathDelim + PathDelim + displayName + PathDelim;
end;

function TiCloudDriverFileSource.IsSystemFile(aFile: TFile): Boolean;
begin
  Result:= inherited;
  if Result then
    Result:= NOT aFile.Name.EndsWith( '.icloud', True );
end;

function TiCloudDriverFileSource.IsPathAtRoot(Path: String): Boolean;
var
  iCloudPath: String;
  testPath: String;
begin
  Result:= inherited;
  if NOT Result then begin
    iCloudPath:= uDCUtils.ReplaceTilde( iCLOUD_DRIVER_PATH );
    testPath:= ExcludeTrailingPathDelimiter( Path );
    Result:= ( testPath=iCloudPath );
  end;
end;

end.


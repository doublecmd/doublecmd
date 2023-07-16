unit uMoveConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DCOSUtils, uGlobsPaths;

implementation

procedure Initialize;
var
  Index: Integer;
  AFileName: String;
  AList: TStringList;
begin
  // Double Commander Portable
  // Move settings from executable directory to 'settings' subdirectory
  if mbFileExists(gpExePath + 'doublecmd.inf') then
  begin
    AFileName:= ExcludeTrailingBackslash(gpGlobalCfgDir);
    if mbDirectoryExists(AFileName) or mbCreateDir(AFileName) then
    begin
      AList:= FindAllFiles(gpExePath, '*.cache;*.cfg;*.err;*.json;*.inf;*.ini;*.scf;*.txt;*.xml');
      for Index:= 0 to AList.Count - 1 do
      begin
        AFileName:= ExtractFileName(AList[Index]);
        if (AFileName <> 'dcupdater.ini') and (AFileName <> 'doublecmd.visualelementsmanifest.xml') then
        begin
          mbRenameFile(gpExePath + AFileName, gpGlobalCfgDir + AFileName);
        end;
      end;
      AList.Free;
    end;
  end;
end;

initialization
  Initialize;

end.


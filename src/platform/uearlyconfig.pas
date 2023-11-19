unit uEarlyConfig;

{$mode objfpc}{$H+}

{$IF DEFINED(darwin)}
  {$DEFINE DARKWIN}
{$ENDIF}

interface

uses
  Classes, SysUtils;

var
{$IFDEF DARKWIN}
  gAppMode: Integer = 1;
{$ENDIF}
  gSplashForm: Boolean = True;

procedure SaveEarlyConfig;

implementation

uses
  DCOSUtils, DCStrUtils, DCClassesUtf8, uSysFolders, uGlobsPaths;

var
  AConfig: String;

function GetEarlyConfig: String;
var
  Index: Integer;
begin
  for Index:= 1 to ParamCount do
  begin
    if StrBegins(ParamStr(Index), '--config-dir=') then
    begin
      Result:= Copy(ParamStr(Index), 14, MaxInt);
      Result:= IncludeTrailingBackslash(Result) + ApplicationName + ConfigExtension;
      Exit;
    end;
  end;
  if mbFileExists(gpGlobalCfgDir + ApplicationName + '.inf') then
    Result:= gpGlobalCfgDir + ApplicationName + ConfigExtension
  else begin
    Result:= IncludeTrailingBackslash(GetAppConfigDir) + ApplicationName + ConfigExtension;
  end;
end;

procedure Initialize;
begin
  AConfig:= GetEarlyConfig;
  if mbFileExists(AConfig) then
  try
    with TStringListEx.Create do
    try
      LoadFromFile(AConfig);
      gSplashForm:= StrToBoolDef(Values['SplashForm'], gSplashForm);
{$IFDEF DARKWIN}
      gAppMode:= StrToIntDef(Values['DarkMode'], gAppMode);
{$ENDIF}
    finally
      Free;
    end;
  except
    // Skip
  end;
end;

procedure SaveEarlyConfig;
begin
  AConfig:= GetEarlyConfig;
  ForceDirectories(ExtractFileDir(AConfig));
  with TStringListEx.Create do
  try
    Add('SplashForm' + NameValueSeparator + BoolToStr(gSplashForm));
{$IFDEF DARKWIN}
    AddPair('DarkMode', IntToStr(gAppMode));
{$ENDIF}
    SaveToFile(AConfig);
  finally
    Free;
  end;
end;

initialization
  Initialize;

end.

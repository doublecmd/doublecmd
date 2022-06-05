unit uEarlyConfig;

{$mode objfpc}{$H+}

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

var
  AConfig: String;

function GetEarlyConfig: String;
begin
  Result:= ExtractFilePath(ParamStr(0));
  if FileExists(Result + ApplicationName + '.inf') then
    Result:= Result + ApplicationName + ConfigExtension
  else begin
    Result:= GetAppConfigFile(False, False);
  end;
end;

procedure Initialize;
begin
  AConfig:= GetEarlyConfig;
  if FileExists(AConfig) then
  try
    with TStringList.Create do
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
  with TStringList.Create do
  try
    AddPair('SplashForm', BoolToStr(gSplashForm));
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

unit uAppleMagnifiedModeFix;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

implementation

uses
  BaseUnix, CocoaAll;

const
  SecondStart = 'SecondStart';
  AppleMagnifiedMode = 'AppleMagnifiedMode';

var
  UserDefaults: NSUserDefaults;

function setenv(const name, value: pchar; overwrite: longint): longint; cdecl; external 'c' name 'setenv';

procedure ExportLanguage;
var
  CurrentLocale: NSLocale;
  Language, Country: String;
begin
  if fpGetEnv(PAnsiChar('LANG')) = '' then
  begin
    CurrentLocale:= NSLocale.currentLocale();
    Country:= NSString(CurrentLocale.objectForKey(NSLocaleCountryCode)).UTF8String;
    Language:= NSString(CurrentLocale.objectForKey(NSLocaleLanguageCode)).UTF8String;
    if (Length(Language) > 0) and (Length(Country) > 0) then
    begin
      Language:= Language + '_' + Country + '.UTF-8';
      setenv('LANG', PAnsiChar(Language), 1);
      WriteLn('Export LANG=' + Language);
    end;
  end;
end;

initialization
  {$IFDEF LCLQT} ExportLanguage; {$ENDIF}
  UserDefaults:= NSUserDefaults.standardUserDefaults;
  if not UserDefaults.boolForKey(NSSTR(SecondStart)) then
  begin
    UserDefaults.setBool_forKey(True, NSSTR(SecondStart));
    UserDefaults.setBool_forKey(False, NSSTR(AppleMagnifiedMode));
    UserDefaults.synchronize;
  end;
end.


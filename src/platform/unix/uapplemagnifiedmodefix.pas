unit uAppleMagnifiedModeFix;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

implementation

uses
  CocoaAll;

const
  SecondStart = 'SecondStart';
  AppleMagnifiedMode = 'AppleMagnifiedMode';

var
  UserDefaults: NSUserDefaults;

initialization
  UserDefaults:= NSUserDefaults.standardUserDefaults;
  if not UserDefaults.boolForKey(NSSTR(SecondStart)) then
  begin
    UserDefaults.setBool_forKey(True, NSSTR(SecondStart));
    UserDefaults.setBool_forKey(False, NSSTR(AppleMagnifiedMode));
    UserDefaults.synchronize;
  end;
end.


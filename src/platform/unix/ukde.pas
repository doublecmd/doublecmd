unit uKde;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMyUnix;

function ShowOpenWithDialog(const FileList: TStringList): Boolean;

var
  UseKde: Boolean = False;

implementation

uses
  uDCUtils, uGlobs, uGlobsPaths, uOSUtils;

var
  PythonScript: UTF8String = 'scripts/doublecmd-kde.py';

function ShowOpenWithDialog(const FileList: TStringList): Boolean;
var
  I: Integer;
  Args: UTF8String;
begin
  Args := ' openwith';
  for I := 0 to FileList.Count - 1 do
    Args+= ' ' + QuoteStr(FileList[I]);
  Result:= ExecCmdFork('python ' + PythonScript + Args);
end;

procedure Initialize;
begin
  UseKde:= (DesktopEnv = DE_KDE);
  if UseKde then
  begin
    PythonScript:= gpExePath + PythonScript;
    UseKde:= (fpSystemStatus('python ' + PythonScript + ' > /dev/null 2>&1') = 0);
  end;
end;

initialization
  RegisterInitialization(@Initialize);

end.


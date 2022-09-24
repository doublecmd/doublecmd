unit uQt5Workaround;

{$mode objfpc}{$H+}

interface

implementation

uses
  InitC, BaseUnix, LCLVersion;

procedure _exit(status: cint); cdecl; external clib;
function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;

{$IF LCL_FULLVERSION < 2020000}
initialization
  if (LowerCase(fpGetEnv(PAnsiChar('XDG_SESSION_TYPE'))) = 'wayland') then
    setenv('QT_QPA_PLATFORM', 'xcb', 1);
{$ENDIF}

finalization
  // Workaround: https://doublecmd.sourceforge.io/mantisbt/view.php?id=2079
  if (UpCase(fpGetEnv(PAnsiChar('XDG_CURRENT_DESKTOP'))) = 'KDE') then
  begin
    WriteLn('Warning: Skip libKF5IconThemes exit handler');
    _exit(ExitCode);
  end;

end.


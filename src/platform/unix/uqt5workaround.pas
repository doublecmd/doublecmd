unit uQt5Workaround;

{$mode objfpc}{$H+}

interface

implementation

uses
  InitC, BaseUnix;

var
  __dso_handle: pointer = nil; cvar; public;

procedure _exit(status: cint); cdecl; external clib;
function atexit(func: pointer): cint; cdecl; external clib;
function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;

procedure DoExit; cdecl;
begin
  _exit(ExitCode);
end;

initialization
  if (LowerCase(fpGetEnv(PAnsiChar('XDG_SESSION_TYPE'))) = 'wayland') then
    setenv('QT_QPA_PLATFORM', 'xcb', 1);

finalization
  // Workaround: https://doublecmd.sourceforge.io/mantisbt/view.php?id=2079
  if (UpCase(fpGetEnv(PAnsiChar('XDG_CURRENT_DESKTOP'))) = 'KDE') then
  begin
    atexit(@DoExit);
    WriteLn('Warning: Skip libKF5IconThemes exit handler');
  end;

end.


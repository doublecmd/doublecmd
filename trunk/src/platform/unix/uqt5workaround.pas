unit uQt5Workaround;

{$mode objfpc}{$H+}

interface

implementation

uses
  InitC, BaseUnix;

procedure _exit(status: cint); cdecl; external clib;
function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;

initialization
  if (LowerCase(fpGetEnv(PAnsiChar('XDG_SESSION_TYPE'))) = 'wayland') then
    setenv('QT_QPA_PLATFORM', 'xcb', 1);

finalization
  // Workaround: https://doublecmd.sourceforge.io/mantisbt/view.php?id=2079
  if (UpCase(fpGetEnv(PAnsiChar('XDG_CURRENT_DESKTOP'))) = 'KDE') then
  begin
    WriteLn('Warning: Skip libKF5IconThemes exit handler');
    _exit(ExitCode);
  end;

end.


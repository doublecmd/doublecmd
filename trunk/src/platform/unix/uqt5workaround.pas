unit uQt5Workaround;

{$mode objfpc}{$H+}

interface

implementation

uses
  InitC, CTypes, BaseUnix;

procedure _exit(status: cint); cdecl; external clib;
function atexit(func: pointer): cint; cdecl; external clib;

procedure DoExit; cdecl;
begin
  _exit(ExitCode);
end;

finalization
  // Workaround: https://doublecmd.sourceforge.io/mantisbt/view.php?id=2079
  if (UpCase(fpGetEnv(PAnsiChar('XDG_CURRENT_DESKTOP'))) = 'KDE') then
  begin
    atexit(@DoExit);
    WriteLn('Warning: Skip libKF5IconThemes exit handler');
  end;

end.


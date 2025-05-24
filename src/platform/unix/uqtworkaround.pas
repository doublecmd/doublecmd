unit uQtWorkaround;

{$mode objfpc}{$H+}

interface

implementation

uses
  InitC, BaseUnix, LCLVersion;

{$IF DEFINED(LCLQT5)}
procedure _exit(status: cint); cdecl; external clib;
{$ELSEIF DEFINED(LCLQT6)}
function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;
{$ENDIF}

{$IF DEFINED(LCLQT6)}
initialization
  // Workaround: https://github.com/doublecmd/doublecmd/issues/2290
  if (LowerCase(fpGetEnv(PAnsiChar('XDG_SESSION_TYPE'))) = 'wayland') then
    setenv('QT_QPA_PLATFORM', 'xcb', 1);
{$ENDIF}

{$IF DEFINED(LCLQT5)}
finalization
  // Workaround: https://doublecmd.sourceforge.io/mantisbt/view.php?id=2079
  if (UpCase(fpGetEnv(PAnsiChar('XDG_CURRENT_DESKTOP'))) = 'KDE') then
  begin
    WriteLn('Warning: Skip libKF5IconThemes exit handler');
    _exit(ExitCode);
  end;
{$ENDIF}

end.


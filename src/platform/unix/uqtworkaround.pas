unit uQtWorkaround;

{$mode objfpc}{$H+}

interface

implementation

uses
  InitC, BaseUnix, LCLVersion
{$IF DEFINED(LCLQT6)}
  , SysUtils, qt6
{$ENDIF}
  ;

{$IF DEFINED(LCLQT5)}
procedure _exit(status: cint); cdecl; external clib;
{$ELSEIF DEFINED(LCLQT6)}
function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;
{$ENDIF}

{$IF DEFINED(LCLQT6)}
var
  Minor, Micro: Integer;
  AVersion: TStringArray;
initialization
  // Workaround: https://github.com/doublecmd/doublecmd/issues/2290
  if (LowerCase(fpGetEnv(PAnsiChar('XDG_SESSION_TYPE'))) = 'wayland') then
  begin
    AVersion:= StrPas(QtVersion).Split(['.']);
    if (Length(AVersion) > 1) then
    begin
      Minor:= StrToIntDef(AVersion[1], 0);
      if (Length(AVersion) > 2) then
        Micro:= StrToIntDef(AVersion[2], 0)
      else begin
        Micro:= 0;
      end;
    end;
    if (Minor < 9) or ((Minor = 9) and (Micro = 0)) then
    begin
      setenv('QT_QPA_PLATFORM', 'xcb', 1);
    end;
  end;
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


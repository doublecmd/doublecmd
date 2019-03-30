unit uOverlayScrollBarFix; 

{$mode objfpc}{$H+}

interface

implementation

uses
  BaseUnix, XLib;

function setenv(const name, value: pchar; overwrite: longint): longint; cdecl; external 'c' name 'setenv';

initialization
  setenv('LIBOVERLAY_SCROLLBAR', '0', 1);
  if (fpGetEnv(PAnsiChar('GTK_IM_MODULE')) = 'xim') then
  begin
    setenv('GTK_IM_MODULE', '', 1);
    WriteLn('Warning: Unsupported input method (xim)');
  end;
  WriteLn('XInitThreads: ', XInitThreads);

end.

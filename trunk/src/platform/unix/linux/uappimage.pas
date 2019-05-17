unit uAppImage;

{$mode objfpc}{$H+}

interface

implementation

uses
  BaseUnix;

function unsetenv(const name: pansichar): cint; cdecl; external 'c';

initialization
  if (fpGetEnv(PAnsiChar('APPIMAGE')) <> nil) then
    unsetenv('PYTHONHOME');

end.


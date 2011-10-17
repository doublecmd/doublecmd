unit uOverlayScrollBarFix; 

{$mode objfpc}{$H+}

interface

implementation

  function setenv(const name, value: pchar; overwrite: longint): longint; cdecl; external 'c' name 'setenv';

initialization
  setenv('LIBOVERLAY_SCROLLBAR', '0', 1);

end.
{$MACRO ON}
{$IFDEF MSWINDOWS}
{$DEFINE dcpcall:=stdcall}
{$ELSE}
{$DEFINE dcpcall:=cdecl}
{$ENDIF}

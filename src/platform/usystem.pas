unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Math
{$IF DEFINED(MSWINDOWS)}
  , Windows
{$ENDIF}
  ;

procedure Initialize;

implementation

procedure Initialize;
begin
  // Disable invalid floating point operation exception
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exZeroDivide]);
{$IF DEFINED(MSWINDOWS)}
  SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
{$ENDIF}
end;

initialization
  Initialize;

end.


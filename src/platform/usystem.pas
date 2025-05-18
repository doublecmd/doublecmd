unit uSystem;

{$mode objfpc}{$H+}

interface

uses
  Math
{$IF DEFINED(MSWINDOWS)}
  , Windows
{$ENDIF}
{$IF DEFINED(LCLQT6)}
  , Qt6
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

{$IF DEFINED(LCLQT6)}
procedure InitializeOnce;
begin
  QCoreApplication_setAttribute(QtAA_ShareOpenGLContexts, True);
end;
{$ENDIF}

initialization
  Initialize;
{$IF DEFINED(LCLQT6)}
  InitializeOnce;
{$ENDIF}

end.


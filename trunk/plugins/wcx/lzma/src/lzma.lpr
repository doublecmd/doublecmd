library lzma;

{$mode objfpc}{$H+}

uses
  lzmafunc;

{$IFDEF WINDOWS}{$R lzma.rc}{$ENDIF}

{$E wcx}

exports
  { Mandatory }
  OpenArchive,
  ReadHeader,
  ProcessFile,
  CloseArchive,
  SetChangeVolProc,
  SetProcessDataProc,
  { Optional }
  CanYouHandleThisFile;

begin
end.


library lzma;

{$mode objfpc}{$H+}

uses
  lzmafunc;

exports
  { Mandatory }
  OpenArchive,
  ReadHeader,
  ProcessFile,
  CloseArchive,
  SetChangeVolProc,
  SetProcessDataProc,
  { Optional }
  PackFiles,
  GetPackerCaps;

begin
end.


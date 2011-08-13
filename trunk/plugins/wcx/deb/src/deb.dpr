library deb;

uses
  deb_io in 'deb_io.pas',
  deb_archive in 'deb_archive.pas',
  deb_def in 'deb_def.pas';

{$E wcx}

exports
  CloseArchive,
  GetPackerCaps,
  OpenArchive,
  ProcessFile,
  ReadHeader,
  SetChangeVolProc,
  SetProcessDataProc;
  
begin
end.

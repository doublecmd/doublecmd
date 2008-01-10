library deb;

uses
  deb_io in 'deb_io.pas',
  deb_archive in 'deb_archive.pas',
  deb_def in 'deb_def.pas';

{$E wcx}
{$R *.res}

exports
  CloseArchive       name 'CloseArchive',
  GetPackerCaps      name 'GetPackerCaps',
  OpenArchive        name 'OpenArchive',
  ProcessFile        name 'ProcessFile',
  ReadHeader         name 'ReadHeader',
  SetChangeVolProc   name 'SetChangeVolProc',
  SetProcessDataProc name 'SetProcessDataProc';
  
begin
end.

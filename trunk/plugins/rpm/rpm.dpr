//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************

//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************

library rpm;

uses
  SysUtils,
  Classes,
  wcx in 'wcx.pas',
  rpm_io in 'rpm_io.pas',
  rpm_def in 'rpm_def.pas',
  rpm_archive in 'rpm_archive.pas';

{$E wcx}

{$R *.RES}

exports
  CloseArchive       name 'CloseArchive',
  GetPackerCaps      name 'GetPackerCaps',
  OpenArchive        name 'OpenArchive',
  ProcessFile        name 'ProcessFile',
  ReadHeader         name 'ReadHeader',
  SetChangeVolProc   name 'SetChangeVolProc',
  SetProcessDataProc name 'SetProcessDataProc';
  
begin
{$IFNDEF WIN32}
WriteLN('Rpm plugin is loaded');
{$ENDIF}
end.

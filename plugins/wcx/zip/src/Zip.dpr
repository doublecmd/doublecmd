library Zip;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  FPCAdds,
  SysUtils,
  Classes,
  ZipFunc, ZipOpt;

exports
  { Mandatory }
  OpenArchive,
  OpenArchiveW,
  ReadHeader,
  ReadHeaderExW,
  ProcessFile,
  ProcessFileW,
  CloseArchive,
  SetChangeVolProc,
  SetChangeVolProcW,
  SetProcessDataProc,
  SetProcessDataProcW,
  { Optional }
  PackFilesW,
  DeleteFilesW,
  GetPackerCaps,
  ConfigurePacker,
  GetBackgroundFlags,
  CanYouHandleThisFileW,
  { Extension API }
  ExtensionInitialize;

{$R *.res}

begin
{$IFDEF UNIX}
  WriteLn('Zip plugin is loaded');
{$ENDIF}
end.

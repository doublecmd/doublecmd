library Zip;



uses
  SysUtils,
  Classes,
  ZipFunc in 'ZipFunc.pas';

exports
  { Mandatory }
  OpenArchive,
  OpenArchiveW,
  ReadHeader,
  ReadHeaderEx,
  ReadHeaderExW,
  ProcessFile,
  ProcessFileW,
  CloseArchive,
  SetChangeVolProc,
  SetChangeVolProcW,
  SetProcessDataProc,
  SetProcessDataProcW,
  { Optional }
  PackFiles,
  PackFilesW,
  DeleteFiles,
  DeleteFilesW,
  GetPackerCaps,
  ConfigurePacker,
  CanYouHandleThisFile,
  CanYouHandleThisFileW,
  { Extension API }
  ExtensionInitialize;

begin
{$IFDEF UNIX}
WriteLN('Zip plugin is loaded');
{$ENDIF}
end.

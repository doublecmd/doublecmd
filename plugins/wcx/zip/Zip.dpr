library Zip;



uses
  SysUtils,
  Classes,
  ZipFunc in 'ZipFunc.pas';

{$E wcx}

{$IFDEF MSWINDOWS}{$R *.res}{$ENDIF}
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
  DeleteFiles,
  GetPackerCaps,
  ConfigurePacker,
  { DialogAPI }
  SetDlgProc;

begin
{$IFDEF UNIX}
WriteLN('Zip plugin is loaded');
{$ENDIF}
end.

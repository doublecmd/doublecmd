library Base64Wcx;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  FPCAdds,
  SysUtils,
  Classes,
  Base64Func;

exports
  { Mandatory }
  OpenArchiveW,
  ReadHeaderExW,
  ProcessFileW,
  CloseArchive,
  SetChangeVolProcW,
  SetProcessDataProcW,
  { Optional }
  PackFilesW,
  GetPackerCaps;

{$R *.res}

begin
end.


library Zip;



uses
  SysUtils,
  Classes,
  ZipFunc in 'ZipFunc.pas';

{$E wcx}

{$R *.res}
exports
{mandatory}
OpenArchive,
ReadHeader,
ProcessFile,
CloseArchive,
SetChangeVolProc,
SetProcessDataProc,
{optional}
PackFiles,
DeleteFiles,
GetPackerCaps;

begin
{$IFDEF UNIX}
WriteLN('Zip plugin is loaded');
{$ENDIF}
end.

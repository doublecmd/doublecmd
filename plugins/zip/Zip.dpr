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
DeleteFiles,
GetPackerCaps;

begin
{$IFNDEF WIN32}
WriteLN('Zip plugin is loaded');
{$ENDIF}
end.

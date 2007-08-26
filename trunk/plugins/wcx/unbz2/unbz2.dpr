library unbz2;



uses
  bz2func in 'bz2func.pas';

{$E wcx}

{$R *.res}
exports
{mandatory}
OpenArchive,
ReadHeader,
ProcessFile,
CloseArchive,
SetChangeVolProc,
SetProcessDataProc;
{optional}


begin
{$IFNDEF WIN32}
WriteLN('unbz2 plugin is loaded');
{$ENDIF}
end.

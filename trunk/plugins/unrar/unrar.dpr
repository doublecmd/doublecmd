library unrar;

uses
  unrardll;

{$E wcx}

exports
{mandatory}
OpenArchive,
ReadHeader,
ProcessFile,
CloseArchive,
SetChangeVolProc,
SetProcessDataProc;
{optional}
//ReadHeaderEx;

begin
end.

unit Dump;

interface

uses
	Classes
  ;

  procedure AddDump(Dump: string);
	procedure SetDumpOutput(DumpOutput: TStrings);

implementation

var
  _DumpOutput: TStrings = nil;

procedure SetDumpOutput(DumpOutput: TStrings);
begin
  _DumpOutput := DumpOutput;
end;

procedure AddDump(Dump: string);
begin
	if (_DumpOutput <> nil) then
	  _DumpOutput.Add(Dump);
end;

end.

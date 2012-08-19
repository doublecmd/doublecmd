unit Dump;

interface
const
  nl = #13#10;

var
  DumpStr: string;
  procedure AddDump(Dump: string);
  procedure ShowDump;

implementation
uses
//  Windows;
UNIT1;


procedure AddDump(Dump: string);
begin
  UNIT1.Form1.Memo1.Lines.Add(Dump);
//  DumpStr := DumpStr + Dump + nl;
end;

procedure ShowDump;
begin
//  OutputDebugString(pChar(DumpStr));
//  DumpStr := '';
end;

end.

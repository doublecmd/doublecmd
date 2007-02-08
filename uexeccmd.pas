unit uExecCmd;
{
  Execute external commands (and get there output) .
  Part of Commander, realised under GNU GPL 2.  (C)opyright 2003

Authors:
  Radek Cervinka, radek.cervinka@centrum.cz

contributors:

  Alexander Koblov (Alexx2000@mail.ru)

}

{$H+}
interface
uses
  Classes;

function ExecCmdInput(const sCmd:String; var lsListing:TStringlist):Boolean;
function ExecCmdFork(const sCmd:String):Integer;

implementation
uses
  SysUtils, Process;

function ExecCmdFork(const sCmd:String):Integer;
var
  AProcess: TProcess;
Begin
  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := sCmd;
  AProcess.Execute;
  AProcess.Free;
  Result:=0;
end;

function ExecCmdInput(const sCmd:String; var lsListing:TStringlist):Boolean;
{var
  OutPut:PIOFile;
  rb:Integer;
  sDummy:String;
  c:Char;}
//  i:Integer;
begin
  assert(assigned(lsListing),'ExecCmdInput: lsListing=nil');
  Result:=False;
//  writeln(sCmd);

// replace with frepascal popen
  Exit;
{  lsListing.Clear;
  OutPut:=popen(PChar(sCmd),'r');
  if not assigned(output) then Exit;
  sDummy:='';
  rb:=1;
  while (FEOF(OutPut)=0) and (rb=1) do
    begin
      rb:=fread(@c, 1, 1, output);
      if (c=#$0A) or (rb=0) then
      begin
        if sDummy<>'' then
          lsListing.Add(sDummy);
        sDummy:='';
      end
      else
        sDummy:=sDummy+c;
    end;
  pclose(output);
  Result:=True;}
  
{  for i:=0 to lsListing.Count-1 do
    writeln(lsListing.Strings[i]);}
end;
end.

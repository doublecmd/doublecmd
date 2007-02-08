{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

storing commands (by file extensions)

contributors:

}

unit uExts;

interface
uses
  Classes, Contnrs;
type
  TExtCommand=Class
    sExtName:String;
    ExtCommands:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TExts=Class
  protected
    FExtList:TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const sName:String);
    function GetExtCommand(iIndex:Integer):TExtCommand;
    function GetCommandText(sExt:String; const sCmd:String):String;
    Function GetExtCommands(sExt:String; var slCommands:TStringList):Boolean;
  end;


implementation
uses
  SysUtils, uLog;

constructor TExtCommand.Create;
begin
  ExtCommands:=TStringList.Create;
end;

destructor TExtCommand.Destroy;
begin
  if assigned(ExtCommands) then
    FreeAndNil(ExtCommands);
  inherited
end;


procedure TExts.LoadFromFile(const sName:String);
var
  extfile:TextFile;
  sLine:String;
  extcmd:TExtCommand;
  iIndex:Integer;
begin
  assign(extfile, sName);
  reset(extfile);
  extcmd:=nil;
  while not eof(extfile) do
  begin
    readln(extfile,sLine);
    sLine:=Trim(sLine);
    if (sLine='') or (sLine[1]='#') then Continue;
//    writeln(sLine);
    if sLine[1]='[' then
    begin
      if assigned(extCmd) then
      begin
        // check if any commands
        if extcmd.ExtCommands.Count>0 then
          FExtList.Add(extcmd) // add and NOT Free
        else // no commands, free command list
          FreeAndNil(FExtList);
      end;
      extCmd:=TExtCommand.Create;
      Delete(sLine,1,1); // delete [
      iIndex:=pos(']', sLine);
      if iIndex>0 then
        sLine:=Copy(sLine,1,iIndex-1)
      else
        logWrite('] not found in line '+sLine);
{      add | for easy searching in two and more extensions
       now I can search for example |pas| or |z|
       (now in second case i can't get correct result
       for bzip, zip and so
}
      extCmd.sExtName:='|'+LowerCase(sLine)+'|';
    end // end if.. '['
    else
    begin // this must be a command
      if not assigned(extCmd) then
      begin
        logWrite('Command '+sLine+' have not defined extension - ignored.');
        Continue;
      end;
      // now set command to lowercase
      for iIndex:=1 to length(sLine) do
        begin
          if sLine[iIndex]='=' then Break;
          sLine[iIndex]:=UpCase(sLine[iIndex]);
        end;
      extCmd.ExtCommands.Add(sLine);
    end;
  end;
  closefile(extfile);
end;

Function TExts.GetExtCommands(sExt:String; var slCommands:TStringList):Boolean;
var
  i:Integer;
begin
  Result:=False;
  if sExt='' then Exit;
  if sExt[1]='.' then
    Delete(sExt,1,1);
  for i:=0 to FExtList.Count-1 do
    with GetExtCommand(i) do
    begin
      if Pos('|'+sExt+'|',sExtName)>0 then
      begin
        slCommands.Assign(ExtCommands);
        Result:=True;
        Break;
      end;
    end;
end;

constructor TExts.Create;
begin
  FExtList:=TObjectList.Create;
end;

destructor TExts.Destroy;
begin
  if assigned(FExtList) then
    FreeAndNil(FExtList);
  inherited
end;

function TExts.GetExtCommand(iIndex:Integer):TExtCommand;
begin
  Result:=TExtCommand(FExtList.Items[iIndex]);
end;

function TExts.GetCommandText(sExt:String; const sCmd:String):String;
var
  i:Integer;
begin
  Result:='';
  if sExt='' then Exit;
  if sExt[1]='.' then
    Delete(sExt,1,1);
  for i:=0 to FExtList.Count-1 do
    with GetExtCommand(i) do
    begin
      if Pos('|'+sExt+'|',sExtName)>0 then
      begin
        Result:=ExtCommands.Values[UpperCase(sCmd)];
        Break;
      end;
    end;
end;

end.

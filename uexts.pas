{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   storing commands (by file extensions)

   contributors:

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)
}

unit uExts;

interface
uses
  Classes, Contnrs;
type
  TExtAction = class
    SectionName,   //en> Section name, for example "[htm|html|mht]"
    Name,          //en> File type name, for example "Hyper text documents"
    Icon : String; //en> Path to icon
    Extensions,    //en> List of extensions
    Actions : TStringList; //en> List of actions, for example "Open=opera '%f'"
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TExts = class
  protected
    FExtList:TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const sName:String);
    function GetExtCommand(iIndex:Integer):TExtAction;
    function GetCommandText(sExt:String; const sCmd:String):String;
    function GetExtCommands(sExt:String; var slCommands:TStringList):Boolean;
    property ExtList : TObjectList read FExtList write FExtList;
  end;


implementation
uses
  LCLProc, SysUtils, uLog;

constructor TExtAction.Create;
begin
  Extensions := TStringList.Create;
  Actions := TStringList.Create;
end;

destructor TExtAction.Destroy;
begin
  if Assigned(Extensions) then
    FreeAndNil(Extensions);
  if Assigned(Actions) then
    FreeAndNil(Actions);
  inherited
end;


procedure TExts.LoadFromFile(const sName:String);
var
  extfile : TextFile;
  sLine, s, sExt :String;
  extcmd : TExtAction;
  iIndex : Integer;
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
        if extcmd.Actions.Count>0 then
          FExtList.Add(extcmd) // add and NOT Free
        else // no commands, free command list
          FreeAndNil(FExtList);
      end;
      extCmd:=TExtAction.Create;
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
      extCmd.SectionName:='|'+LowerCase(sLine)+'|';

      // fill extensions list
      s := LowerCase(sLine)+'|';
      while Pos('|', s) <> 0 do
        begin
          iIndex := Pos('|',s);
          sExt := Copy(s,1,iIndex-1);
          Delete(s, 1, iIndex);
          extCmd.Extensions.Add(sExt);
        end;
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
      DebugLn(sLine);
      if Pos('NAME', sLine) = 1 then // File type name
        extCmd.Name := Copy(sLine, iIndex + 1, Length(sLine))
      else if Pos('ICON', sLine) = 1 then // File type icon
        extCmd.Icon := Copy(sLine, iIndex + 1, Length(sLine))
      else // action
        extCmd.Actions.Add(sLine);
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
      if Pos('|'+sExt+'|',SectionName)>0 then
      begin
        slCommands.Assign(Actions);
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

function TExts.GetExtCommand(iIndex:Integer):TExtAction;
begin
  Result:=TExtAction(FExtList.Items[iIndex]);
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
      if Pos('|'+sExt+'|',SectionName)>0 then
      begin
        Result:=Actions.Values[UpperCase(sCmd)];
        Break;
      end;
    end;
end;

end.

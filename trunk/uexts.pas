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
    IconIndex : Integer;
    Extensions,    //en> List of extensions
    Actions : TStringList; //en> List of actions, for example "Open=opera '%f'"
    IsChanged : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TExts }

  TExts = class
    function GetCount: Integer;
  private
    function GetItems(Index: Integer): TExtAction;
  protected
    FExtList:TObjectList;
    function GetNewSectionName(Index: Integer): String;
    procedure EraseSection(extFile : TStringList; SectionIndex: Integer; SkipComments : Boolean = False);
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(AExtAction: TExtAction): Integer;
    procedure DeleteItem(Index: Integer);
    procedure LoadFromFile(const sName:String);
    procedure SaveToFile(const sName:String);
    function GetExtActionCmd(sExt:String; const sActionName:String):String;
    function GetExtActions(sExt:String; var slActions:TStringList):Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TExtAction read GetItems;
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

function TExts.GetNewSectionName(Index: Integer): String;
var
  I, iCount: Integer;
begin
  with GetItems(Index) do
  begin
    iCount := Extensions.Count - 1;
    Result := Extensions[0];
    for I:= 1 to iCount do
      Result := '|' + Extensions[I];
  end;
end;

procedure TExts.EraseSection(extFile : TStringList; SectionIndex: Integer; SkipComments : Boolean = False);
var
  sLine : String;
begin
  repeat
    if SkipComments and (Pos('#', Trim(extFile.Strings[SectionIndex]))=1) then
	  Continue;
	extFile.Delete(SectionIndex);
    sLine := extFile.Strings[SectionIndex];
  until ((Pos('[', sLine)<>0) and (Pos(']', sLine)<>0)) or
        ((Pos('#', sLine)<>0) and (Pos('[', extFile.Strings[SectionIndex+1])<>0) and
        (Pos(']', extFile.Strings[SectionIndex+1])<>0));
end;

procedure TExts.SaveToFile(const sName: String);
var
  I, J, iIndex,
  iCount,
  iBegin, iEnd : Integer;
  extFile : TStringList;
  sLine,
  sNewName,
  sSectionName: String;
  bExists : Boolean;
begin
  extFile:= TStringList.Create;

  if FileExists(sName) then
    begin
      extFile.LoadFromFile(sName);
      // first rename sections if needed
      iCount := Count - 1;
      for I := 0 to iCount do
        with GetItems(I) do
        begin
          sNewName := GetNewSectionName(I);
          if SectionName <> sNewName then
            begin
              iIndex:= extFile.IndexOf(SectionName);
              if iIndex >=0 then
                extFile.Strings[iIndex] := sNewName;
            end;
        end;
      // second delete old sections
      iCount := extFile.Count - 1;
      while I <= iCount do
        with GetItems(I) do
        begin
          sLine := extFile.Strings[I];
          iBegin:= Pos('[', sLine);
          iEnd:=   Pos(']', sLine);
          if (iBegin <> 0) and (iEnd <> 0) then
            begin
              sSectionName := Copy(extFile.Strings[I],iBegin + 1, iEnd - iBegin);
              bExists:= False;
              for J:= 0 to Count - 1 do
                begin
                  if sSectionName = SectionName then
                    begin
                      bExists := True;
                      Break;
                    end;
                end;
              if not bExists then // delete section
                EraseSection(extFile, I);
            end;
        Inc(I);
        end; // while

        // third rewrite changed sections
	iCount := Count - 1;
        for I := 0 to iCount do
        with GetItems(I) do
        begin
          if IsChanged then
	    begin
	      sNewName := GetNewSectionName(I);
              iIndex:= extFile.IndexOf(sNewName);
              if iIndex >= 0 then // if section exists then insert actions
	        begin
                  EraseSection(extFile, iIndex+1, True);
                  for J:= 0 to Actions.Count - 1 do
                    extFile.Insert(iIndex+1, Actions.Strings[J]);
                end
              else // else add new section
                begin
                  extFile.Add(sNewName);
                  for J:= 0 to Actions.Count - 1 do
                    extFile.Add(Actions.Strings[J]);
                end;
            end;
	end;

    end // FileExists
  else
    begin
      iCount := Count - 1;
      for I := 0 to iCount do
      with GetItems(I) do
        begin
          extFile.Add(sNewName);
          for J:= 0 to Actions.Count - 1 do
            extFile.Add(Actions.Strings[J]);
	end;
    end;
  extFile.SaveToFile(sName);
  extFile.Free;
end;

function TExts.GetExtActions(sExt:String; var slActions:TStringList):Boolean;
var
  i:Integer;
begin
  Result:=False;
  if sExt='' then Exit;
  if sExt[1]='.' then
    Delete(sExt,1,1);
  for i:=0 to FExtList.Count-1 do
    with GetItems(i) do
    begin
      if Pos('|'+sExt+'|',SectionName)>0 then
      begin
        slActions.Assign(Actions);
        Result:=True;
        Break;
      end;
    end;
end;

function TExts.GetCount: Integer;
begin
  Result := FExtList.Count;
end;

function TExts.GetItems(Index: Integer): TExtAction;
begin
  Result := TExtAction(FExtList.Items[Index]);
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

function TExts.AddItem(AExtAction: TExtAction): Integer;
begin
  Result := FExtList.Add(AExtAction);
end;

procedure TExts.DeleteItem(Index: Integer);
begin
  FExtList.Delete(Index);
end;

function TExts.GetExtActionCmd(sExt:String; const sActionName:String):String;
var
  i:Integer;
begin
  Result:='';
  if sExt='' then Exit;
  if sExt[1]='.' then
    Delete(sExt,1,1);
  for i:=0 to FExtList.Count-1 do
    with GetItems(i) do
    begin
      if Pos('|'+sExt+'|',SectionName)>0 then
      begin
        Result:=Actions.Values[UpperCase(sActionName)];
        Break;
      end;
    end;
end;

end.

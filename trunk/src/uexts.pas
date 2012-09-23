{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   storing commands (by file extensions)

   contributors:

   Copyright (C) 2008-2011  Koblov Alexander (Alexx2000@mail.ru)
}

unit uExts;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, uFile;

type
  {en
     Class for storage actions by file extensions
  }
  TExtAction = class
    SectionName: String;     //en< Section name, for example "[htm|html|mht]"
    Name: String;            //en< File type name, for example "Hyper text documents"
    Icon: String;            //en< Path to icon
    IconIndex: Integer;      //en< Icon index (used in configuration dialog for paint icons)
    Extensions: TStringList; //en< List of extensions
    Actions: TStringList;    //en< List of actions, for example "Open=opera '%f'"
    IsChanged: Boolean;      //en< True if item was changed
  public
    {en
       Constructs an object and initializes its data before the object is first used.
    }
    constructor Create;
    {en
       Destroys an object and frees its memory.
    }
    destructor Destroy; override;
  end;

  {en
     Main class for storage actions list by file extensions
  }
  TExts = class
  private
    {en
       Return the number of items
       @returns(The number of items)
    }
    function GetCount: Integer;
    {en
       Get item by index
       @param(Index Item index)
       @returns(TExtAction item)
    }
    function GetItems(Index: Integer): TExtAction;
  protected
    {en
       Internal ObjectList for storage items.
    }
    FExtList:TObjectList;
    {en
       Return new section name for item by index
       @param(Index Item index)
       @returns(New section name)
    }
    function GetNewSectionName(Index: Integer): String;
    {en
       Erase section from file by section line index
       @param(extFile StringList with loaded extension file)
       @param(SectionIndex Section line index)
       @param(SkipComments If @true then don't delete comments)
    }
    procedure EraseSection(extFile : TStringList; var SectionIndex: Integer; SkipComments : Boolean = False);
  public
    {en
       Constructs an object and initializes its data before the object is first used.
    }
    constructor Create;
    {en
       Destroys an object and frees its memory.
    }
    destructor Destroy; override;
    {en
       Deletes all items.
    }
    procedure Clear;
    {en
       Inserts a new item at the end of the list
       @param(AExtAction TExtAction item)
       @returns(The index of the new item)
    }
    function AddItem(AExtAction: TExtAction): Integer;
    {en
       Removes the item at the position given by the Index parameter
       @param(Index Item index)
    }
    procedure DeleteItem(Index: Integer);
    {en
       Fills the actions list from file
       @param(sName File name)
    }
    procedure LoadFromFile(const sName:String);
    {en
       Save the actions list to file
       @param(sName File name)
    }
    procedure SaveToFile(const sName:String);
    {en
       Return action command by file and action name
       @param(aFile File for which action is sought)
       @param(sActionName Action name)
       @returns(Action command)
    }
    function GetExtActionCmd(aFile: TFile; const sActionName:String):String;
    {en
       Return list of actions by extension
       @param(File File which actions to retrieve)
       @param(slActions Actions list)
       @returns(The function returns @true if successful, @false otherwise)
    }
    function GetExtActions(aFile: TFile; var slActions:TStringList):Boolean;
    {en
       Indicates the number of items
    }
    property Count: Integer read GetCount;
    {en
       Give access to items by index
    }
    property Items[Index: Integer]: TExtAction read GetItems;
  end;

const
  cMaskDefault = 'default';
  cMaskFolder = 'folder';
  cMaskFile = 'file';

implementation

uses
  SysUtils, uLog, DCClassesUtf8, DCOSUtils, uDebug;

constructor TExtAction.Create;
begin
  Extensions := TStringList.Create;
  Actions := TStringList.Create;
  Actions.CaseSensitive:= False;
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
  extFile: TStringListEx;
  sLine, s, sExt: String;
  extcmd: TExtAction;
  I, iIndex: Integer;
begin
  extFile:= TStringListEx.Create;
  extFile.LoadFromFile(sName);
  extcmd:=nil;
  for I:= 0 to extFile.Count - 1 do
  begin
    sLine:= extFile.Strings[I];
    sLine:= Trim(sLine);
    if (sLine='') or (sLine[1]='#') then Continue;
//    writeln(sLine);
    if sLine[1]='[' then
    begin
      extCmd:= TExtAction.Create;
      FExtList.Add(extcmd);

      iIndex:=pos(']', sLine);
      if iIndex>0 then
        sLine:=Copy(sLine,1,iIndex)
      else
        logWrite('] not found in line '+sLine);
{      add | for easy searching in two and more extensions
       now I can search for example |pas| or |z|
}
      extCmd.SectionName:=LowerCase(sLine);

      // fill extensions list
      s := LowerCase(sLine);
      Delete(s, 1, 1); // Delete '['
      Delete(s, Length(s), 1); // Delete ']'
      s := s + '|';
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
      s := sLine;
      for iIndex:=1 to Length(s) do
        begin
          if s[iIndex]='=' then Break;
          s[iIndex]:= LowerCase(s[iIndex]);
        end;

      // DCDebug(sLine);
      if Pos('name', s) = 1 then // File type name
        extCmd.Name := Copy(sLine, iIndex + 1, Length(sLine))
      else if Pos('icon', s) = 1 then // File type icon
        extCmd.Icon := Copy(sLine, iIndex + 1, Length(sLine))
      else // action
        extCmd.Actions.Add(sLine);
    end;
  end;
  extFile.Free;
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
      Result := Result + '|' + Extensions[I];
  end;
  Result := '[' + Result + ']';
end;

procedure TExts.EraseSection(extFile : TStringList; var SectionIndex: Integer; SkipComments : Boolean = False);
var
  sLine : String;
begin
  repeat
    if SkipComments and (Pos('#', Trim(extFile.Strings[SectionIndex]))=1) then
      Inc(SectionIndex)
    else
      extFile.Delete(SectionIndex);

    if SectionIndex >= extFile.Count then Exit;

    sLine := extFile.Strings[SectionIndex];
    //DCDebug('sLine = ', sLine);
  until ((Pos('[', sLine)<>0) and (Pos(']', sLine)<>0)) or
        ((Pos('#', sLine)<>0) and (Pos('[', extFile.Strings[SectionIndex+1])<>0) and
        (Pos(']', extFile.Strings[SectionIndex+1])<>0));
end;

procedure TExts.SaveToFile(const sName: String);
var
  I, J, iIndex,
  iCount,
  iBegin, iEnd : Integer;
  extFile : TStringListEx;
  sLine,
  sNewName,
  sSectionName: String;
  bExists: Boolean;
begin
  extFile := TStringListEx.Create;

  if mbFileExists(sName) then
  begin
    extFile.LoadFromFile(sName);

    // first rename sections if needed
    iCount := Count - 1;
    for I := 0 to iCount do
      with GetItems(I) do
      begin
        sNewName := GetNewSectionName(I);
        if (SectionName <> sNewName) and
           // SectionName might be empty for new items
           (SectionName <> '') then
        begin
          iIndex := extFile.IndexOf(SectionName);
          if iIndex >= 0 then
          begin
            extFile.Strings[iIndex] := sNewName;
            // Update section name so it doesn't get deleted below.
            SectionName := sNewName;
          end;
        end;
      end;

    // second delete old sections
    I      := 0;
    iCount := extFile.Count - 1;
    while I <= iCount do
    begin
      sLine  := Trim(extFile.Strings[I]);
      iBegin := Pos('[', sLine);
      iEnd   := Pos(']', sLine);
      if (iBegin = 1) and (iEnd <> 0) then
      begin
        sSectionName := LowerCase(Copy(extFile.Strings[I], iBegin, iEnd));
        bExists      := False;
        for J := 0 to Count - 1 do
        begin
          //DCDebug('sSectionName = ', sSectionName);
          //DCDebug('GetItems(J).SectionName = ', GetItems(J).SectionName);

          if sSectionName = GetItems(J).SectionName then
          begin
            bExists := True;
            Break;
          end;
        end; // for
        if not bExists then // delete section
        begin
          EraseSection(extFile, I);
          iCount := extFile.Count - 1;
        end;
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
          iIndex   := extFile.IndexOf(sNewName);
          if iIndex >= 0 then // if section exists then insert actions
          begin
            Inc(iIndex); // skip section name
            EraseSection(extFile, iIndex, True);
            if Name <> '' then
            begin
              extFile.Insert(iIndex, 'Name=' + Name);
              Inc(iIndex);
            end;
            if Icon <> '' then
            begin
              extFile.Insert(iIndex, 'Icon=' + Icon);
              Inc(iIndex);
            end;
            for J := 0 to Actions.Count - 1 do
            begin
              extFile.Insert(iIndex, Actions.Strings[J]);
              Inc(iIndex);
            end;
            extFile.Insert(iIndex, ''); // add empty line
          end
          else // else add new section
          begin
            extFile.Add(sNewName); // section
            if Name <> '' then
              extFile.Add('Name=' + Name); // file type name
            if Icon <> '' then
              extFile.Add('Icon=' + Icon); // icon path
            for J := 0 to Actions.Count - 1 do
              extFile.Add(Actions.Strings[J]);
            extFile.Add(''); // add empty line
          end;
        end;
      end;

  end // mbFileExists
  else
  begin
    iCount := Count - 1;
    for I := 0 to iCount do
      with GetItems(I) do
      begin
        extFile.Add(GetNewSectionName(I));
        if Name <> '' then
          extFile.Add('Name=' + Name); // file type name
        if Icon <> '' then
          extFile.Add('Icon=' + Icon); // icon path
        for J := 0 to Actions.Count - 1 do
          extFile.Add(Actions.Strings[J]);
        extFile.Add(''); // add empty line
      end;
  end;
  try
    extFile.SaveToFile(sName);
  except
    on e: EStreamError do
      DCDebug(Format('Error saving file associations to "%s": %s', [sName, e.Message]));
  end;
  extFile.Free;
end;

function TExts.GetExtActions(aFile: TFile; var slActions:TStringList):Boolean;
var
  I: Integer;
  sMask: String;
begin
  Result:= False;

  if aFile.IsDirectory or aFile.IsLinkToDirectory then
    sMask:= cMaskFolder
  else
    sMask:= LowerCase(aFile.Extension);

  if Length(sMask) <> 0 then
  for I:= 0 to FExtList.Count - 1 do
    with GetItems(i) do
    begin
      if Extensions.IndexOf(sMask) >= 0 then
      begin
        slActions.Assign(Actions);
        Result:= True;
        Break;
      end;
    end;

  if sMask = cMaskFolder then Exit;

  for I:= 0 to FExtList.Count - 1 do
    with GetItems(i) do
    begin
      if Extensions.IndexOf(cMaskFile) >= 0 then
      begin
        slActions.AddStrings(Actions);
        Result:= True;
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

procedure TExts.Clear;
begin
  FExtList.Clear;
end;

function TExts.AddItem(AExtAction: TExtAction): Integer;
begin
  Result := FExtList.Add(AExtAction);
end;

procedure TExts.DeleteItem(Index: Integer);
begin
  FExtList.Delete(Index);
end;

function TExts.GetExtActionCmd(aFile: TFile; const sActionName:String):String;
var
  I: Integer;
  sMask: String;
begin
  Result:= EmptyStr;

  if aFile.IsDirectory or aFile.IsLinkToDirectory then
    sMask:= cMaskFolder
  else
    sMask:= LowerCase(aFile.Extension);

  if Length(sMask) <> 0 then
  for I:= 0 to FExtList.Count - 1 do
    with GetItems(I) do
    begin
      if Extensions.IndexOf(sMask) >= 0 then
      begin
        Result:= Actions.Values[sActionName];
        Exit;
      end;
    end;

  // if command not found then try to find default command
  for I:= 0 to FExtList.Count - 1 do
    with GetItems(I) do
    begin
      if Extensions.IndexOf(cMaskDefault) >= 0 then
      begin
        Result:=Actions.Values[sActionName];
        Break;
      end;
    end;
end;

end.

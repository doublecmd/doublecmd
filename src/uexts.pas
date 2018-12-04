{
   Double commander
   -------------------------------------------------------------------------
   Manager for commands associated to file extension.

   Copyright (C) 2008-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.


   Original comment:
   ----------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   storing commands (by file extensions)
}

unit uExts;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, Contnrs,

  uFile;

type

  { What constitutes our basic info for the external actual action }
  TExtActionCommand = class
    FActionName: string;
    FCommandName: string;
    FParams: string;
    FStartPath: string;
    FIconIndex: integer;
    FIconBitmap: Graphics.TBitmap;
  public
    constructor Create(ParamActionName, ParamCommandName, ParamParams, ParamStartPath: string);
    destructor Destroy; override;
    function CloneExtAction: TExtActionCommand;
    property ActionName: string read FActionName write FActionName;
    property CommandName: string read FCommandName write FCommandName;
    property Params: string read FParams write FParams;
    property StartPath: string read FStartPath write FStartPath;
    property IconIndex: integer read FIconIndex write FIconIndex;
    property IconBitmap: Graphics.TBitmap read FIconBitmap write FIconBitmap;
  end;

  { Each file type may have more than one possible associated action ("TExtActionCommand").
    This class is to hold a collection of this }
  TExtActionList = class(TList)
  private
    function GetExtActionCommand(Index: integer): TExtActionCommand;
  public
    constructor Create;
    procedure Clear; override;
    function Add(ExtActionCommand: TExtActionCommand): integer;
    procedure Insert(Index: integer; ExtActionCommand: TExtActionCommand);
    procedure DeleteExtActionCommand(Index: integer);
    property ExtActionCommand[Index: integer]: TExtActionCommand read GetExtActionCommand;
  end;

  { Class for storage actions by file extensions }
  TExtAction = class
    Name: string;            //en< File type name, for example "Hyper text documents"
    Icon: string;            //en< Path to icon
    IconIndex: integer;
    Extensions: TStringList; //en< List of extensions
    ActionList: TExtActionList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetIconListForStorage: string;
    procedure SetIconListFromStorage(sStorage: string);
  end;

  { Main class for storage actions list by file extensions }
  TExts = class
  private
    function GetCount: integer;
    function GetItems(Index: integer): TExtAction;
    procedure LegacyLoadFromFile(const sName: string);
  protected
    FExtList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddItem(AExtAction: TExtAction): integer;
    procedure DeleteItem(Index: integer);
    procedure MoveItem(SrcIndex, DestIndex: integer);
    function Load: boolean;
    function LoadXMLFile: boolean;
    procedure SaveXMLFile;
    function GetExtActionCmd(aFile: TFile; const sActionName: string; var sCmd: string; var sParams: string; var sStartPath: string): boolean;
    function GetExtActions(aFile: TFile; paramActionList: TExtActionList; pIndexOfFirstPossibleFileType: PInteger = nil; bWantedAllActions: boolean = False): boolean;
    function ComputeSignature(Seed:dword=$00000000): dword;
    property Count: integer read GetCount;
    property Items[Index: integer]: TExtAction read GetItems;
    property FileType[Index: integer]: TExtAction read GetItems;
  end;

const
  cMaskDefault = 'default';
  cMaskFolder = 'folder';
  cMaskFile = 'file';

implementation

uses
  DCXmlConfig, uDCVersion, uGlobsPaths, uDCUtils, crc, uLng, SysUtils, uLog,
  DCClassesUtf8, DCOSUtils, strUtils;

{ TExtActionCommand.Create }
constructor TExtActionCommand.Create(ParamActionName, ParamCommandName, ParamParams, ParamStartPath: string);
begin
  inherited Create;
  FActionName := ParamActionName;
  FCommandName := ParamCommandName;
  FParams := ParamParams;
  FStartPath := ParamStartPath;
  FIconIndex := -1; // <--IconIndex is used only in "uShellContextMenu" to show an icon next to the command AND is filled correctly when doing "TExts.GetExtActions"
  FIconBitmap := nil; // <--IconBitmap is used only in "uShellContextMenu" to show an icon next to the command AND is filled correctly when doing "CreateActionSubMenu" from "uShellContextMenu"
end;

{ TExtActionCommand.Destroy }
destructor TExtActionCommand.Destroy;
begin
  if Assigned(FIconBitmap) then
    FreeAndNil(FIconBitmap);
  inherited;
end;

{ TExtActionCommand.CloneExtAction }
function TExtActionCommand.CloneExtAction: TExtActionCommand;
begin
  Result := TExtActionCommand.Create(self.FActionName, self.FCommandName, self.FParams, self.FStartPath);
end;

{ TExtActionList.Create }
constructor TExtActionList.Create;
begin
  inherited Create;
end;

{ TExtActionList.Clear }
procedure TExtActionList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    ExtActionCommand[i].Free;
  inherited Clear;
end;

{ TExtActionList.Add }
function TExtActionList.Add(ExtActionCommand: TExtActionCommand): integer;
begin
  Result := inherited Add(ExtActionCommand);
end;

{ TExtActionList.Insert }
procedure TExtActionList.Insert(Index: integer; ExtActionCommand: TExtActionCommand);
begin
  inherited Insert(Index, ExtActionCommand);
end;

{ TExtActionList.DeleteExtActionCommand }
procedure TExtActionList.DeleteExtActionCommand(Index: integer);
begin
  ExtActionCommand[Index].Free;
  Delete(Index);
end;

{ TExtActionList.GetExtActionCommand }
function TExtActionList.GetExtActionCommand(Index: integer): TExtActionCommand;
begin
  Result := TExtActionCommand(Items[Index]);
end;

constructor TExtAction.Create;
begin
  inherited Create;
  Extensions := TStringList.Create;
  Extensions.CaseSensitive := False;
  ActionList := TExtActionList.Create;
end;

destructor TExtAction.Destroy;
begin
  if Assigned(Extensions) then
    FreeAndNil(Extensions);
  if Assigned(ActionList) then
    FreeAndNil(ActionList);
  inherited;
end;

{ TExtAction.GetIconListForStorage }
function TExtAction.GetIconListForStorage: string;
var
  iExtension: integer;
begin
  Result := '';
  if Extensions.Count = 0 then
    Result := rsMsgUserDidNotSetExtension
  else
    for iExtension := 0 to pred(Extensions.Count) do
      if Result = '' then
        Result := Extensions[iExtension]
      else
        Result := Result + '|' + Extensions[iExtension];
end;

{ TExtAction.SetIconListFromStorage }
procedure TExtAction.SetIconListFromStorage(sStorage: string);
var
  PosPipe, LastPosPipe: integer;
begin
  LastPosPipe := 0;

  repeat
    PosPipe := posEx('|', sStorage, LastPosPipe + 1);
    if PosPipe <> 0 then
    begin
      Extensions.add(copy(sStorage, LastPosPipe + 1, ((PosPipe - LastPosPipe) - 1)));
      LastPosPipe := PosPipe;
    end;
  until PosPipe = 0;

  if length(sStorage) > LastPosPipe then
    Extensions.add(copy(sStorage, LastPosPipe + 1, (length(sStorage) - LastPosPipe)));

  if Extensions.Count = 0 then
    Extensions.Add(rsMsgUserDidNotSetExtension);
end;

{ TExts.LegacyLoadFromFile }
//We need to keep this routine to be able to load "old legacy format" of the
//file associated action based on file extension that was using DC originally.
procedure TExts.LegacyLoadFromFile(const sName: string);
var
  extFile: TStringListEx;
  sLine, s, sExt: string;
  extCurrentFileType: TExtAction;
  I, iIndex: integer;
  sCommandName, sEndingPart, sCommandCmd, sParams: string;
begin
  extFile := TStringListEx.Create;
  try
    extFile.LoadFromFile(sName);
    extCurrentFileType := nil;

    for I := 0 to extFile.Count - 1 do
    begin
      sLine := extFile.Strings[I];
      sLine := Trim(sLine);
      if (sLine = '') or (sLine[1] = '#') then
        Continue;
      if sLine[1] = '[' then
      begin
        extCurrentFileType := TExtAction.Create;
        FExtList.Add(extCurrentFileType);

        iIndex := pos(']', sLine);
        if iIndex > 0 then
          sLine := Copy(sLine, 1, iIndex)
        else
          logWrite(Format(rsExtsClosedBracketNoFound, [sLine]));

        extCurrentFileType.Name:=sLine; // Just in case we don't have a name later on, let's named the file type based on the extension defined.

        // fill extensions list
        s := sLine;
        Delete(s, 1, 1); // Delete '['
        Delete(s, Length(s), 1); // Delete ']'
        s := s + '|';
        while Pos('|', s) <> 0 do
        begin
          iIndex := Pos('|', s);
          sExt := Copy(s, 1, iIndex - 1);
          Delete(s, 1, iIndex);
          extCurrentFileType.Extensions.Add(sExt);
        end;
      end // end if.. '['
      else
      begin // this must be a command
        if not assigned(extCurrentFileType) then
        begin
          logWrite(Format(rsExtsCommandWithNoExt, [sLine]));
          Continue;
        end;

        // now set command to lowercase
        s := sLine;
        for iIndex := 1 to Length(s) do
        begin
          if s[iIndex] = '=' then
            Break;
          s[iIndex] := LowerCase(s[iIndex]);
        end;

        if Pos('name', s) = 1 then // File type name
          extCurrentFileType.Name := Copy(sLine, iIndex + 1, Length(sLine))
        else if Pos('icon', s) = 1 then // File type icon
          extCurrentFileType.Icon := Copy(sLine, iIndex + 1, Length(sLine))
        else // action
        begin
          sCommandName := Copy(sLine, 1, iIndex - 1);
          sEndingPart := Copy(sLine, iIndex + 1, Length(sLine));
          try
            SplitCmdLineToCmdParams(sEndingPart, sCommandCmd, sParams);
          except
            sCommandCmd := '<ERROR IN CONVERSION> '+sEndingPart; //Just in case the user has something wrong in his settings, LIKE a missing ending quote...
            sParams := '';
          end;
          sCommandCmd := Trim(sCommandCmd);
          sParams := Trim(sParams);
          extCurrentFileType.ActionList.Add(TExtActionCommand.Create(sCommandName, sCommandCmd, sParams, ''));
        end;
      end;
    end;
  finally
    extFile.Free;
  end;
end;

function TExts.GetExtActions(aFile: TFile; paramActionList: TExtActionList; pIndexOfFirstPossibleFileType: PInteger = nil; bWantedAllActions: boolean = False): boolean;
var
  I, iActionNo: integer;
  sMask: string;
  ExtActionCommand: TExtActionCommand;
begin
  if pIndexOfFirstPossibleFileType <> nil then
    pIndexOfFirstPossibleFileType^ := -1;
  Result := False;

  if aFile.IsDirectory or aFile.IsLinkToDirectory then
    sMask := cMaskFolder
  else
    sMask := LowerCase(aFile.Extension);

  if Length(sMask) <> 0 then
    for I := 0 to FExtList.Count - 1 do
      with GetItems(i) do
      begin
        if Extensions.IndexOf(sMask) >= 0 then
        begin
          if paramActionList.Count > 0 then
            paramActionList.Add(TExtActionCommand.Create('-', '', '', ''));
          for iActionNo := 0 to pred(ActionList.Count) do
          begin
            ExtActionCommand := ActionList.ExtActionCommand[iActionNo].CloneExtAction;
            ExtActionCommand.IconIndex := IconIndex;
            paramActionList.Add(ExtActionCommand);
          end;
          if pIndexOfFirstPossibleFileType <> nil then
            if pIndexOfFirstPossibleFileType^ = -1 then
              pIndexOfFirstPossibleFileType^ := I;
          Result := True;
          if not bWantedAllActions then
            Break;
        end;
      end;

  if sMask = cMaskFolder then
    Exit;

  for I := 0 to FExtList.Count - 1 do
    with GetItems(i) do
    begin
      if Extensions.IndexOf(cMaskFile) >= 0 then
      begin
        if paramActionList.Count > 0 then
          paramActionList.Add(TExtActionCommand.Create('-', '', '', ''));
        for iActionNo := 0 to pred(ActionList.Count) do
        begin
          ExtActionCommand := ActionList.ExtActionCommand[iActionNo].CloneExtAction;
          ExtActionCommand.IconIndex := IconIndex;
          paramActionList.Add(ExtActionCommand);
        end;
        if pIndexOfFirstPossibleFileType <> nil then
          if pIndexOfFirstPossibleFileType^ = -1 then
            pIndexOfFirstPossibleFileType^ := I;
        Result := True;
        if not bWantedAllActions then
          Break;
      end;
    end;
end;

function TExts.GetCount: integer;
begin
  Result := FExtList.Count;
end;

function TExts.GetItems(Index: integer): TExtAction;
begin
  Result := TExtAction(FExtList.Items[Index]);
end;

constructor TExts.Create;
begin
  inherited Create;
  FExtList := TObjectList.Create;
end;

destructor TExts.Destroy;
begin
  if assigned(FExtList) then
    FreeAndNil(FExtList);
  inherited;
end;

procedure TExts.Clear;
begin
  FExtList.Clear;
end;

function TExts.AddItem(AExtAction: TExtAction): integer;
begin
  Result := FExtList.Add(AExtAction);
end;

procedure TExts.DeleteItem(Index: integer);
begin
  FExtList.Delete(Index);
end;

procedure TExts.MoveItem(SrcIndex, DestIndex: integer);
begin
  FExtList.Move(SrcIndex, DestIndex);
end;

function TExts.GetExtActionCmd(aFile: TFile; const sActionName: string; var sCmd: string; var sParams: string; var sStartPath: string): boolean;
var
  I: integer;
  sMask: string;
  iAction: integer;
begin
  Result := False;
  sCmd := '';
  sParams := '';
  sStartPath := '';

  if aFile.IsDirectory or aFile.IsLinkToDirectory then
    sMask := cMaskFolder
  else
    sMask := LowerCase(aFile.Extension);

  if Length(sMask) <> 0 then
  begin
    for I := 0 to FExtList.Count - 1 do
      with GetItems(I) do
      begin
        if Extensions.IndexOf(sMask) >= 0 then
        begin
          iAction := 0;
          while (iAction < ActionList.Count) and (not Result) do
          begin
            if UpperCase(ActionList.ExtActionCommand[iAction].ActionName) = UpperCase(sActionName) then
            begin
              sCmd := ActionList.ExtActionCommand[iAction].CommandName;
              sParams := ActionList.ExtActionCommand[iAction].Params;
              sStartPath := ActionList.ExtActionCommand[iAction].StartPath;
              Result := True;
              Exit;
            end
            else
            begin
              Inc(iAction);
            end;
          end;
        end;
      end;
  end;

  // if command not found then try to find default command
  for I := 0 to FExtList.Count - 1 do
    with GetItems(I) do
    begin
      if Extensions.IndexOf(cMaskDefault) >= 0 then
      begin
        iAction := 0;
        while (iAction < ActionList.Count) and (not Result) do
        begin
          if UpperCase(ActionList.ExtActionCommand[iAction].ActionName) = UpperCase(sActionName) then
          begin
            sCmd := ActionList.ExtActionCommand[iAction].CommandName;
            sParams := ActionList.ExtActionCommand[iAction].Params;
            sStartPath := ActionList.ExtActionCommand[iAction].StartPath;
            Result := True;
            Exit;
          end
          else
          begin
            Inc(iAction);
          end;
        end;
      end;
    end;
end;

{ TExts.ComputeSignature }
function TExts.ComputeSignature(Seed:dword): dword;
var
  iExtType, iExtension, iAction: integer;
begin
  Result := Seed;

  for iExtType := 0 to pred(Count) do
  begin
    Result := crc32(Result, @Items[iExtType].Name[1], length(Items[iExtType].Name));
    if length(Items[iExtType].Icon) > 0 then
      Result := crc32(Result, @Items[iExtType].Icon[1], length(Items[iExtType].Icon));
    for iExtension := 0 to pred(Items[iExtType].Extensions.Count) do
      if length(Items[iExtType].Extensions.Strings[iExtension]) > 0 then
        Result := crc32(Result, @Items[iExtType].Extensions.Strings[iExtension][1], length(Items[iExtType].Extensions.Strings[iExtension]));
    for iAction := 0 to pred(Items[iExtType].ActionList.Count) do
    begin
      if length(Items[iExtType].ActionList.ExtActionCommand[iAction].FActionName) > 0 then
        Result := crc32(Result, @Items[iExtType].ActionList.ExtActionCommand[iAction].FActionName[1], length(Items[iExtType].ActionList.ExtActionCommand[iAction].FActionName));
      if length(Items[iExtType].ActionList.ExtActionCommand[iAction].FCommandName) > 0 then
        Result := crc32(Result, @Items[iExtType].ActionList.ExtActionCommand[iAction].FCommandName[1], length(Items[iExtType].ActionList.ExtActionCommand[iAction].FCommandName));
      if length(Items[iExtType].ActionList.ExtActionCommand[iAction].FParams) > 0 then
        Result := crc32(Result, @Items[iExtType].ActionList.ExtActionCommand[iAction].FParams[1], length(Items[iExtType].ActionList.ExtActionCommand[iAction].FParams));
      if length(Items[iExtType].ActionList.ExtActionCommand[iAction].FStartPath) > 0 then
        Result := crc32(Result, @Items[iExtType].ActionList.ExtActionCommand[iAction].FStartPath[1], length(Items[iExtType].ActionList.ExtActionCommand[iAction].FStartPath));
    end;
  end;
end;

{ TExts.SaveXMLFile }
procedure TExts.SaveXMLFile;
var
  ActionList: TExtActionList;
  iFileType, iAction: Integer;
  ExtXMLSettings: TXmlConfig = nil;
  Root, Node, SubNode, SubSubNode: TXmlNode;
begin
  ExtXMLSettings := TXmlConfig.Create(gpCfgDir + gcfExtensionAssociation);
  try
    with ExtXMLSettings do
    begin
      Root := ExtXMLSettings.RootNode;
      SetAttr(Root, 'DCVersion', dcVersion);

      Node := FindNode(Root, 'ExtensionAssociation', True);
      ClearNode(Node);
      { Each file type has its own extensions }
      for iFileType := 0 to Pred(Count) do
      begin
        SubNode := AddNode(Node, 'FileType');
        SetValue(SubNode, 'Name', FileType[iFileType].Name);
        SetValue(SubNode, 'IconFile', FileType[iFileType].Icon);
        SetValue(SubNode, 'ExtensionList', FileType[iFileType].GetIconListForStorage);

        SubNode := AddNode(SubNode, 'Actions');
        ActionList := FileType[iFileType].ActionList;
        for iAction := 0 to Pred(ActionList.Count) do
        begin
          SubSubNode := AddNode(SubNode, 'Action');
          SetValue(SubSubNode, 'Name', ActionList.ExtActionCommand[iAction].ActionName);
          if ActionList.ExtActionCommand[iAction].CommandName <> '' then
            SetValue(SubSubNode, 'Command', ActionList.ExtActionCommand[iAction].CommandName);
          if ActionList.ExtActionCommand[iAction].Params <> '' then
            SetValue(SubSubNode, 'Params', ActionList.ExtActionCommand[iAction].Params);
          if ActionList.ExtActionCommand[iAction].StartPath <> '' then
            SetValue(SubSubNode, 'StartPath', ActionList.ExtActionCommand[iAction].StartPath);
        end;
      end;
    end;
    ExtXMLSettings.Save;
  finally
    ExtXMLSettings.Free;
  end;
end;

{ TExts.Load}
function TExts.Load: boolean;
begin
  Result := False;

  try
    if (mbFileExists(gpCfgDir + 'doublecmd.ext')) AND (not mbFileExists(gpCfgDir + gcfExtensionAssociation)) then
    begin
      LegacyLoadFromFile(gpCfgDir + 'doublecmd.ext');
      SaveXmlFile;
      mbRenameFile(gpCfgDir + 'doublecmd.ext', gpCfgDir + 'doublecmd.ext.obsolete');
      Result := True;
    end
    else
    begin
      Result := LoadXMLFile;
    end;
  except
    Result := False;
  end;
end;

{ TExts.LoadXMLFile }
function TExts.LoadXMLFile: boolean;
var
  extCurrentFileType: TExtAction;
  ExtXMLSettings: TXmlConfig = nil;
  Node, SubNode, SubSubNode: TXmlNode;
  sName, sIconFilename, sExtensionList,
  sActionName, sCommandName, sParams, sStartPath: string;
begin
  Result := False;

  try
    ExtXMLSettings := TXmlConfig.Create(gpCfgDir + gcfExtensionAssociation);
    try
      ExtXMLSettings.Load;

      with ExtXMLSettings do
      begin
        Node := FindNode(ExtXMLSettings.RootNode, 'ExtensionAssociation');
        if Assigned(Node) then
        begin
          SubNode := Node.FirstChild;
          while Assigned(SubNode) do
          begin
            if SubNode.CompareName('FileType') = 0 then
            begin
              sName := ExtXMLSettings.GetValue(SubNode, 'Name', rsMsgUserDidNotSetName);
              if sName <> rsMsgUserDidNotSetName then
              begin
                sIconFilename := ExtXMLSettings.GetValue(SubNode, 'IconFile', '');
                sExtensionList := ExtXMLSettings.GetValue(SubNode, 'ExtensionList', rsMsgUserDidNotSetExtension);

                extCurrentFileType := TExtAction.Create;
                extCurrentFileType.Name := sName;
                extCurrentFileType.Icon := sIconFilename;
                extCurrentFileType.SetIconListFromStorage(sExtensionList);

                SubSubNode := FindNode(SubNode, 'Actions');
                if Assigned(SubSubNode) then
                begin
                  SubSubNode := SubSubNode.FirstChild;
                  while Assigned(SubSubNode) do
                  begin
                    if SubSubNode.CompareName('Action') = 0 then
                    begin
                      sActionName := ExtXMLSettings.GetValue(SubSubNode, 'Name', rsMsgUserDidNotSetName);
                      sCommandName := ExtXMLSettings.GetValue(SubSubNode, 'Command', '');
                      sParams := ExtXMLSettings.GetValue(SubSubNode, 'Params', '');
                      sStartPath := ExtXMLSettings.GetValue(SubSubNode, 'StartPath', '');
                      extCurrentFileType.ActionList.Add(TExtActionCommand.Create(sActionName, sCommandName, sParams, sStartPath));
                    end;
                    SubSubNode := SubSubNode.NextSibling;
                  end;
                end;

                AddItem(extCurrentFileType);
              end;
            end;
            SubNode := SubNode.NextSibling;
          end;
        end;
      end;

    finally
      ExtXMLSettings.Free;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

end.
//Cleaner les >Action@//Utiliser ExtFileType comme nom au lieu de Action car pas tout de suite une action//Remplacer le Savefile by SaveXMLFile

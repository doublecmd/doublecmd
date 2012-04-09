{
    Double Commander
    -------------------------------------------------------------------------
    Extended tool items types for KASToolBar

    Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uKASToolItemsExtended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, KASToolItems, KASToolBar, IniFiles, DCXmlConfig,
  uDrive;

type
  { TKASCommandItem }

  TKASCommandItem = class(TKASNormalItem)
    Command: String;
    Params:  TDynamicStringArray;
    procedure Assign(OtherItem: TKASToolItem); override;
    function Clone: TKASToolItem; override;
    function ConfigNodeName: String; override;
    procedure Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader); override;
    procedure SaveContents(Config: TXmlConfig; Node: TXmlNode); override;
  end;

  { TKASProgramItem }

  TKASProgramItem = class(TKASNormalItem)
    Command:   String;
    Params:    String;
    StartPath: String;
    procedure Assign(OtherItem: TKASToolItem); override;
    function Clone: TKASToolItem; override;
    function ConfigNodeName: String; override;
    procedure Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader); override;
    procedure SaveContents(Config: TXmlConfig; Node: TXmlNode); override;
  end;

  { TKASDriveItem }

  TKASDriveItem = class(TKASNormalItem)
    Drive: PDrive;
    procedure Assign(OtherItem: TKASToolItem); override;
    function Clone: TKASToolItem; override;
    function ConfigNodeName: String; override;
  end;

  TKASToolBarIniLoader = class;
  TOnLoadIniItem = procedure (Loader: TKASToolBarIniLoader; var Item: TKASToolItem) of object;
  TOnOpenIniFile = function (FileName: String): TIniFile of object;

  { TKASToolBarIniLoader }

  TKASToolBarIniLoader = class
  private
    FDepthLevel: Integer; // In case .bar files reference each other
  public
    procedure Load(IniFileName: String; ToolBar: TKASToolBar; ToolItemMenu: TKASMenuItem; OnLoadIniItem: TOnLoadIniItem);
  end;

  { TKASToolBarExtendedLoader }

  TKASToolBarExtendedLoader = class(TKASToolBarLoader)
  protected
    function CreateItem(Node: TXmlNode): TKASToolItem; override;
  end;

implementation

uses
  DCClassesUtf8;

const
  CommandItemConfigNode = 'Command';
  ProgramItemConfigNode = 'Program';
  DriveItemConfigNode   = 'Drive';

procedure AddString(var anArray: TDynamicStringArray; const sToAdd: String);
var
  Len: Integer;
begin
  Len := Length(anArray);
  SetLength(anArray, Len + 1);
  anArray[Len] := sToAdd;
end;

procedure SaveIfNotEmpty(Config: TXmlConfig; Node: TXmlNode; Name, Value: String);
begin
  if Value <> EmptyStr then
    Config.AddValue(Node, Name, Value);
end;

{ TKASDriveItem }

procedure TKASDriveItem.Assign(OtherItem: TKASToolItem);
var
  DriveItem: TKASDriveItem;
begin
  inherited Assign(OtherItem);
  if OtherItem is TKASDriveItem then
  begin
    DriveItem := TKASDriveItem(OtherItem);
    Drive     := DriveItem.Drive;
  end;
end;

function TKASDriveItem.Clone: TKASToolItem;
begin
  Result := TKASDriveItem.Create;
  Result.Assign(Self);
end;

function TKASDriveItem.ConfigNodeName: String;
begin
  Result := DriveItemConfigNode;
end;

{ TKASCommandItem }

procedure TKASCommandItem.Assign(OtherItem: TKASToolItem);
var
  CommandItem: TKASCommandItem;
begin
  inherited Assign(OtherItem);
  if OtherItem is TKASCommandItem then
  begin
    CommandItem := TKASCommandItem(OtherItem);
    Command     := CommandItem.Command;
    Params      := Copy(CommandItem.Params);
  end;
end;

function TKASCommandItem.Clone: TKASToolItem;
begin
  Result := TKASCommandItem.Create;
  Result.Assign(Self);
end;

function TKASCommandItem.ConfigNodeName: String;
begin
  Result := CommandItemConfigNode;
end;

procedure TKASCommandItem.Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader);
begin
  inherited Load(Config, Node, Loader);
  Params := nil;
  Node   := Node.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('Command') = 0 then
      Command := Config.GetContent(Node)
    else if Node.CompareName('Param') = 0 then
      AddString(Params, Config.GetContent(Node));
    Node := Node.NextSibling;
  end;
end;

procedure TKASCommandItem.SaveContents(Config: TXmlConfig; Node: TXmlNode);
var
  AParam: String;
begin
  inherited SaveContents(Config, Node);
  Config.AddValue(Node, 'Command', Command);
  for AParam in Params do
    SaveIfNotEmpty(Config, Node, 'Param', AParam);
end;

{ TKASProgramItem }

procedure TKASProgramItem.Assign(OtherItem: TKASToolItem);
var
  ProgramItem: TKASProgramItem;
begin
  inherited Assign(OtherItem);
  if OtherItem is TKASProgramItem then
  begin
    ProgramItem := TKASProgramItem(OtherItem);
    Command     := ProgramItem.Command;
    Params      := ProgramItem.Params;
    StartPath   := ProgramItem.StartPath;
  end;
end;

function TKASProgramItem.Clone: TKASToolItem;
begin
  Result := TKASProgramItem.Create;
  Result.Assign(Self);
end;

function TKASProgramItem.ConfigNodeName: String;
begin
  Result := ProgramItemConfigNode;
end;

procedure TKASProgramItem.Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader);
begin
  inherited Load(Config, Node, Loader);
  Node   := Node.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('Command') = 0 then
      Command   := Config.GetContent(Node)
    else if Node.CompareName('Params') = 0 then
      Params    := Config.GetContent(Node)
    else if Node.CompareName('StartPath') = 0 then
      StartPath := Config.GetContent(Node);
    Node := Node.NextSibling;
  end;
end;

procedure TKASProgramItem.SaveContents(Config: TXmlConfig; Node: TXmlNode);
begin
  inherited SaveContents(Config, Node);
  SaveIfNotEmpty(Config, Node, 'Command', Command);
  SaveIfNotEmpty(Config, Node, 'Params', Params);
  SaveIfNotEmpty(Config, Node, 'StartPath', StartPath);
end;

{ TKASToolBarExtendedLoader }

function TKASToolBarExtendedLoader.CreateItem(Node: TXmlNode): TKASToolItem;
begin
  Result := inherited CreateItem(Node);
  if not Assigned(Result) then
  begin
    if Node.CompareName(CommandItemConfigNode) = 0 then
      Result := TKASCommandItem.Create
    else if Node.CompareName(ProgramItemConfigNode) = 0 then
      Result := TKASProgramItem.Create
    else if Node.CompareName(DriveItemConfigNode) = 0 then
      Result := TKASDriveItem.Create;
  end;
end;

{ TKASToolBarIniLoader }

procedure TKASToolBarIniLoader.Load(IniFileName: String; ToolBar: TKASToolBar; ToolItemMenu: TKASMenuItem; OnLoadIniItem: TOnLoadIniItem);
var
  BtnCount, I: Integer;
  CommandItem: TKASCommandItem;
  ProgramItem: TKASProgramItem;
  Command, Menu, Button, Param, Path, Misk: String;
  Item: TKASToolItem;
  IniFile: TIniFileEx = nil;
begin
  if (FDepthLevel < 10) then
  begin
    IniFile := TIniFileEx.Create(IniFileName, fmOpenRead or fmShareDenyNone);
    if Assigned(IniFile) then
    try
      Inc(FDepthLevel);
      BtnCount := IniFile.ReadInteger('Buttonbar', 'Buttoncount', 0);
      for I := 1 to BtnCount do
      begin
        Command := IniFile.ReadString('Buttonbar', 'cmd' + IntToStr(I), '');
        Menu    := IniFile.ReadString('Buttonbar', 'menu' + IntToStr(I), '');
        Button  := IniFile.ReadString('Buttonbar', 'button' + IntToStr(I), '');
        Param   := IniFile.ReadString('Buttonbar', 'param' + IntToStr(I), '');
        Path    := IniFile.ReadString('Buttonbar', 'path' + IntToStr(I), '');
        Misk    := IniFile.ReadString('Buttonbar', 'misk' + IntToStr(I), '');

        Item := nil;
        if Menu = '-' then
        begin
          Item := TKASSeparatorItem.Create;
        end
        else if (Length(Command) > 3) and (Copy(Command, 1, 3) = 'cm_') then
        begin
          CommandItem := TKASCommandItem.Create;
          CommandItem.Command := Command;
          CommandItem.Hint := Menu;
          CommandItem.Icon := Button;
          if Param <> EmptyStr then
            AddString(CommandItem.Params, Param);
          if Misk <> EmptyStr then
            AddString(CommandItem.Shortcuts, Misk);
          Item := CommandItem;
        end
        else
        begin
          ProgramItem := TKASProgramItem.Create;
          ProgramItem.Command := Command;
          ProgramItem.Hint := Menu;
          ProgramItem.Icon := Button;
          ProgramItem.Params := Param;
          ProgramItem.StartPath := Path;
          if Misk <> EmptyStr then
            AddString(CommandItem.Shortcuts, Misk);
          Item := ProgramItem;
        end;

        if Assigned(OnLoadIniItem) then
          OnLoadIniItem(Self, Item);
        if Assigned(ToolBar) then
          ToolBar.AddButton(Item);
        if Assigned(ToolItemMenu) then
          ToolItemMenu.SubItems.Add(Item);
      end;
    finally
      IniFile.Free;
      Dec(FDepthLevel);
    end;
  end;
end;

end.


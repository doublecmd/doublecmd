{
    Double Commander
    -------------------------------------------------------------------------
    Basic tool items types for KASToolBar

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

unit KASToolItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, KASXmlConfig;

type
  TDynamicStringArray = array of String;
  TKASToolBarItems = class;
  TKASToolItem = class;

  TOnLoadToolItem = procedure (Item: TKASToolItem) of object;

  { TKASToolBarLoader }

  TKASToolBarLoader = class
  protected
    function CreateItem(Node: TXmlNode): TKASToolItem; virtual;
  public
    procedure Load(Config: TXmlConfig; RootNode: TXmlNode; OnLoadToolItem: TOnLoadToolItem); virtual;
  end;

  { TKASToolItem }

  TKASToolItem = class
  private
    FUserData: Pointer;
  public
    procedure Assign(OtherItem: TKASToolItem); virtual;
    function Clone: TKASToolItem; virtual; abstract;
    function ConfigNodeName: String; virtual; abstract;
    function GetEffectiveHint: String; virtual; abstract;
    function GetEffectiveText: String; virtual; abstract;
    procedure Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader); virtual; abstract;
    procedure Save(Config: TXmlConfig; Node: TXmlNode);
    procedure SaveContents(Config: TXmlConfig; Node: TXmlNode); virtual; abstract;
    property UserData: Pointer read FUserData write FUserData;
  end;
  TKASToolItemClass = class of TKASToolItem;

  { TKASSeparatorItem }

  TKASSeparatorItem = class(TKASToolItem)
    procedure Assign(OtherItem: TKASToolItem); override;
    function Clone: TKASToolItem; override;
    function ConfigNodeName: String; override;
    function GetEffectiveHint: String; override;
    function GetEffectiveText: String; override;
    procedure Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader); override;
    procedure SaveContents(Config: TXmlConfig; Node: TXmlNode); override;
  end;

  { TKASNormalItem }

  TKASNormalItem = class(TKASToolItem)
  strict private
    FID:  String;          // Unique identificator of the button
    function GetID: String;
  public
    Icon: String;
    Text: String;
    Hint: String;
    Shortcuts: TDynamicStringArray;
    procedure Assign(OtherItem: TKASToolItem); override;
    function Clone: TKASToolItem; override;
    function ConfigNodeName: String; override;
    function GetEffectiveHint: String; override;
    function GetEffectiveText: String; override;
    procedure Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader); override;
    procedure SaveContents(Config: TXmlConfig; Node: TXmlNode); override;
    property ID: String read GetID;
  end;

  { TKASMenuItem }

  TKASMenuItem = class(TKASNormalItem)
    procedure ToolItemLoaded(Item: TKASToolItem);
  private
    FItems: TKASToolBarItems;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Assign(OtherItem: TKASToolItem); override;
    function Clone: TKASToolItem; override;
    function ConfigNodeName: String; override;
    procedure Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader); override;
    procedure SaveContents(Config: TXmlConfig; Node: TXmlNode); override;
    property SubItems: TKASToolBarItems read FItems;
  end;

  { TKASToolBarItems }

  TKASToolBarItems = class
  private
    FButtons: TFPList;
    function GetButton(Index: Integer): TKASToolItem;
    function GetButtonCount: Integer;
    procedure SetButton(Index: Integer; const AValue: TKASToolItem);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TKASToolItem): Integer;
    procedure Clear;
    function Insert(InsertAt: Integer; Item: TKASToolItem): Integer;
    procedure Remove(Index: Integer);
    property Count: Integer read GetButtonCount;
    property Items[Index: Integer]: TKASToolItem read GetButton write SetButton; default;
  end;

  { TKASToolBarSerializer }

  TKASToolBarSerializer = class
  private
    FDeserializedItem: TKASToolItem;
    procedure SetDeserializedItem(Item: TKASToolItem);
  public
    function Deserialize(Stream: TStream; Loader: TKASToolBarLoader): TKASToolItem;
    procedure Serialize(Stream: TStream; Item: TKASToolItem);
  end;

const
  MenuItemConfigNode = 'Menu';
  NormalItemConfigNode = 'Normal';
  SeparatorItemConfigNode = 'Separator';

implementation

procedure AddString(var anArray: TDynamicStringArray; const sToAdd: String);
var
  Len: Integer;
begin
  Len := Length(anArray);
  SetLength(anArray, Len + 1);
  anArray[Len] := sToAdd;
end;

procedure AddStrWithSep(var SourceString: String; const StringToAdd: String; const Separator: Char);
begin
  if Length(SourceString) > 0 then
    SourceString := SourceString + Separator;
  SourceString := SourceString + StringToAdd;
end;

function ArrayToString(const anArray: TDynamicStringArray; const Separator: Char): String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(anArray) to High(anArray) do
    AddStrWithSep(Result, anArray[i], Separator);
end;

procedure SaveIfNotEmpty(Config: TXmlConfig; Node: TXmlNode; Name, Value: String);
begin
  if Value <> EmptyStr then
    Config.AddValue(Node, Name, Value);
end;

{ TKASToolItem }

procedure TKASToolItem.Assign(OtherItem: TKASToolItem);
begin
  FUserData := OtherItem.FUserData;
end;

procedure TKASToolItem.Save(Config: TXmlConfig; Node: TXmlNode);
begin
  Node := Config.AddNode(Node, ConfigNodeName);
  SaveContents(Config, Node);
end;

{ TKASToolBarSerializer }

function TKASToolBarSerializer.Deserialize(Stream: TStream; Loader: TKASToolBarLoader): TKASToolItem;
var
  Config: TXmlConfig;
begin
  Result := nil;
  FDeserializedItem := nil;
  Config := TXmlConfig.Create;
  try
    Config.ReadFromStream(Stream);
    Loader.Load(Config, Config.RootNode, @SetDeserializedItem);
    Result := FDeserializedItem;
  finally
    Config.Free;
  end;
end;

procedure TKASToolBarSerializer.Serialize(Stream: TStream; Item: TKASToolItem);
var
  Config: TXmlConfig;
begin
  Config := TXmlConfig.Create;
  try
    Item.Save(Config, Config.RootNode);
    Config.WriteToStream(Stream);
  finally
    Config.Free;
  end;
end;

procedure TKASToolBarSerializer.SetDeserializedItem(Item: TKASToolItem);
begin
  FDeserializedItem := Item;
end;

{ TKASToolBarLoader }

function TKASToolBarLoader.CreateItem(Node: TXmlNode): TKASToolItem;
begin
  if Node.CompareName(MenuItemConfigNode) = 0 then
    Result := TKASMenuItem.Create
  else if Node.CompareName(NormalItemConfigNode) = 0 then
    Result := TKASNormalItem.Create
  else if Node.CompareName(SeparatorItemConfigNode) = 0 then
    Result := TKASSeparatorItem.Create
  else
    Result := nil;
end;

procedure TKASToolBarLoader.Load(Config: TXmlConfig; RootNode: TXmlNode; OnLoadToolItem: TOnLoadToolItem);
var
  Node: TXmlNode;
  Item: TKASToolItem;
begin
  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    Item := CreateItem(Node);

    if Assigned(Item) then
      try
        Item.Load(Config, Node, Self);
        OnLoadToolItem(Item);
        Item := nil;
      finally
        FreeAndNil(Item);
      end;

    Node := Node.NextSibling;
  end;
end;

{ TKASMenuItem }

procedure TKASMenuItem.Assign(OtherItem: TKASToolItem);
var
  MenuItem: TKASMenuItem;
  I: Integer;
begin
  inherited Assign(OtherItem);
  if OtherItem is TKASMenuItem then
  begin
    MenuItem := TKASMenuItem(OtherItem);
    FItems.Clear;
    for I := 0 to MenuItem.SubItems.Count - 1 do
      FItems.Add(MenuItem.SubItems.Items[I].Clone);
  end;
end;

function TKASMenuItem.Clone: TKASToolItem;
begin
  Result := TKASMenuItem.Create;
  Result.Assign(Self);
end;

function TKASMenuItem.ConfigNodeName: String;
begin
  Result := MenuItemConfigNode;
end;

constructor TKASMenuItem.Create;
begin
  FItems := TKASToolBarItems.Create;
end;

destructor TKASMenuItem.Destroy;
begin
  inherited Destroy;
  FItems.Free;
end;

procedure TKASMenuItem.Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader);
begin
  inherited Load(Config, Node, Loader);
  SubItems.Clear;
  Node := Config.FindNode(Node, 'MenuItems', False);
  if Assigned(Node) then
    Loader.Load(Config, Node, @ToolItemLoaded);
end;

procedure TKASMenuItem.SaveContents(Config: TXmlConfig; Node: TXmlNode);
var
  I: Integer;
begin
  inherited SaveContents(Config, Node);
  if SubItems.Count > 0 then
  begin
    Node := Config.AddNode(Node, 'MenuItems');
    for I := 0 to SubItems.Count - 1 do
      SubItems.Items[I].Save(Config, Node);
  end;
end;

procedure TKASMenuItem.ToolItemLoaded(Item: TKASToolItem);
begin
  SubItems.Add(Item);
end;

{ TKASDividerItem }

procedure TKASSeparatorItem.Assign(OtherItem: TKASToolItem);
begin
  inherited Assign(OtherItem);
end;

function TKASSeparatorItem.Clone: TKASToolItem;
begin
  Result := TKASSeparatorItem.Create;
  Result.Assign(Self);
end;

function TKASSeparatorItem.ConfigNodeName: String;
begin
  Result := SeparatorItemConfigNode;
end;

function TKASSeparatorItem.GetEffectiveHint: String;
begin
  Result := '';
end;

function TKASSeparatorItem.GetEffectiveText: String;
begin
  Result := '';
end;

procedure TKASSeparatorItem.Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader);
begin
  // Empty.
end;

procedure TKASSeparatorItem.SaveContents(Config: TXmlConfig; Node: TXmlNode);
begin
  // Empty.
end;

{ TKASNormalItem }

procedure TKASNormalItem.Assign(OtherItem: TKASToolItem);
var
  NormalItem: TKASNormalItem;
begin
  inherited Assign(OtherItem);
  if OtherItem is TKASNormalItem then
  begin
    // Don't copy ID.
    NormalItem := TKASNormalItem(OtherItem);
    Icon       := NormalItem.Icon;
    Text       := NormalItem.Text;
    Hint       := NormalItem.Hint;
    Shortcuts  := Copy(NormalItem.Shortcuts);
  end;
end;

function TKASNormalItem.Clone: TKASToolItem;
begin
  Result := TKASNormalItem.Create;
  Result.Assign(Self);
end;

function TKASNormalItem.ConfigNodeName: String;
begin
  Result := NormalItemConfigNode;
end;

function TKASNormalItem.GetEffectiveHint: String;
begin
  Result := Hint;
  if Length(Shortcuts) > 0 then
  begin
    if Result <> '' then
      Result := Result + ' ('
    else
      Result := Result + '(';
    Result := Result + ArrayToString(Shortcuts, ' ') + ')';
  end;
end;

function TKASNormalItem.GetEffectiveText: String;
begin
  Result := Text;
end;

function TKASNormalItem.GetID: String;
var
  Guid: TGuid;
begin
  if FID = EmptyStr then
  begin
    if CreateGUID(Guid) = 0 then
      FID := GUIDToString(Guid)
    else
      FID := IntToStr(Random(MaxInt));
  end;
  Result := FID;
end;

procedure TKASNormalItem.Load(Config: TXmlConfig; Node: TXmlNode; Loader: TKASToolBarLoader);
begin
  Shortcuts := nil;
  Node      := Node.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('ID') = 0 then
      FID     := Config.GetContent(Node)
    else if Node.CompareName('Text') = 0 then
      Text    := Config.GetContent(Node)
    else if Node.CompareName('Icon') = 0 then
      Icon    := Config.GetContent(Node)
    else if Node.CompareName('Hint') = 0 then
      Hint := Config.GetContent(Node)
    else if Node.CompareName('Shortcut') = 0 then
      AddString(Shortcuts, Config.GetContent(Node));
    Node := Node.NextSibling;
  end;
end;

procedure TKASNormalItem.SaveContents(Config: TXmlConfig; Node: TXmlNode);
var
  AShortcut: String;
begin
  Config.AddValue(Node, 'ID', ID);
  SaveIfNotEmpty(Config, Node, 'Text', Text);
  SaveIfNotEmpty(Config, Node, 'Icon', Icon);
  SaveIfNotEmpty(Config, Node, 'Hint', Hint);
  for AShortcut in Shortcuts do
    SaveIfNotEmpty(Config, Node, 'Shortcut', AShortcut);
end;

{ TKASToolBarItems }

constructor TKASToolBarItems.Create;
begin
  FButtons := TFPList.Create;
end;

destructor TKASToolBarItems.Destroy;
begin
  Clear;
  inherited Destroy;
  FButtons.Free;
end;

function TKASToolBarItems.Insert(InsertAt: Integer; Item: TKASToolItem): Integer;
begin
  FButtons.Insert(InsertAt, Item);
  Result := InsertAt;
end;

function TKASToolBarItems.Add(Item: TKASToolItem): Integer;
begin
  Result := FButtons.Add(Item);
end;

procedure TKASToolBarItems.Remove(Index: Integer);
begin
  TKASToolItem(FButtons[Index]).Free;
  FButtons.Delete(Index);
end;

procedure TKASToolBarItems.Clear;
var
  i: Integer;
begin
  for i := 0 to FButtons.Count - 1 do
    TKASToolItem(FButtons[i]).Free;
  FButtons.Clear;
end;

function TKASToolBarItems.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TKASToolBarItems.GetButton(Index: Integer): TKASToolItem;
begin
  Result := TKASToolItem(FButtons[Index]);
end;

procedure TKASToolBarItems.SetButton(Index: Integer; const AValue: TKASToolItem);
begin
  TKASToolItem(FButtons[Index]).Free;
  FButtons[Index] := AValue;
end;

end.

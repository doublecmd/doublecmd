{
   Double Commander components
   -------------------------------------------------------------------------
   Toolbar panel class

   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)
   
   contributors:
     2012 Przemyslaw Nagay (cobines@gmail.com)
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit KASToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ComCtrls,
  Graphics, Dialogs, ExtCtrls, Buttons, FileUtil, Menus,
  DCXmlConfig, KASToolItems;

type
  TOnToolButtonClick = procedure (Sender: TObject) of object;
  TOnToolButtonMouseUpDown = procedure (Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer) of object;
  TOnToolButtonMouseMove = procedure (Sender: TObject; Shift:TShiftState; X,Y:Integer; NumberOfButton: Integer) of object;
  TOnToolButtonDragOver = procedure(Sender, Source: TObject; X,Y: Integer;
               State: TDragState; var Accept: Boolean; NumberOfButton: Integer) of object;
  TOnToolButtonDragDrop = procedure(Sender, Source: TObject; X, Y: Integer) of object;
  TOnToolButtonEndDrag = procedure(Sender, Target: TObject; X,Y: Integer) of object;
  TOnLoadButtonGlyph = function (ToolItem: TKASToolItem; iIconSize: Integer; clBackColor: TColor): TBitmap of object;
  TOnToolItemExecute = procedure (ToolItem: TKASToolItem) of object;
  TOnConfigLoadItem = function (Config: TXmlConfig; Node: TXmlNode): TKASToolItem of object;
  TOnToolItemShortcutsHint = function (ToolItem: TKASNormalItem): String of object;

  TKASToolBar = class;

  { TKASToolButton }

  TKASToolButton = class(TSpeedButton)
  private
    FToolItem: TKASToolItem;
    function GetToolBar: TKASToolBar;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent; Item: TKASToolItem); reintroduce;
    property ToolBar: TKASToolBar read GetToolBar;
    property ToolItem: TKASToolItem read FToolItem;
  end;

  { TKASToolDivider }

  TKASToolDivider = class(TKASToolButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure Paint; override;
  end;

  { TKASToolBar }

  TKASToolBar = class(TToolBar, IToolOwner)
  private
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FFlat: Boolean;
    FGlyphSize: Integer;
    FRadioToolBar: Boolean;
    FRowHeight: Integer;
    FShowDividerAsButton: Boolean;
    FToolItemExecutors: TFPList;
    FToolItems: TKASToolBarItems;
    FToolPopupMenu: TPopupMenu;
    FOwnsToolItems: Boolean;
    FUpdateCount: Integer;
    FOnToolButtonClick: TOnToolButtonClick;
    FOnToolButtonMouseDown: TOnToolButtonMouseUpDown;
    FOnToolButtonMouseUp: TOnToolButtonMouseUpDown;
    FOnToolButtonMouseMove: TOnToolButtonMouseMove;
    FOnToolButtonDragOver: TOnToolButtonDragOver;
    FOnToolButtonDragDrop: TOnToolButtonDragDrop;
    FOnToolButtonEndDrag: TOnToolButtonEndDrag;
    FOnLoadButtonGlyph: TOnLoadButtonGlyph;
    FOnToolItemExecute: TOnToolItemExecute;
    FOnToolItemShortcutsHint: TOnToolItemShortcutsHint;
    FKASToolBarFlags: TToolBarFlags;
    FResizeButtonsNeeded: Boolean;
    procedure AssignToolButtonProperties(ToolButton: TKASToolButton);
    procedure ClearExecutors;
    function CreateButton(Item: TKASToolItem): TKASToolButton;
    function DoExecuteToolItem(Item: TKASToolItem): Boolean;
    function FindButton(Button: TKASToolButton): Integer;
    function GetChangePath: String;
    function GetEnvVar: String;
    function GetToolItemShortcutsHint(Item: TKASToolItem): String;
    function LoadBtnIcon(IconPath: String): TBitMap;
    procedure DrawLinkIcon(Image: TBitMap);
    function GetButton(Index: Integer): TKASToolButton;
    procedure InsertButton(InsertAt: Integer; ToolButton: TKASToolButton);
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetChangePath(const AValue: String);
    procedure SetEnvVar(const AValue: String);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyphSize(const AValue: Integer);
    procedure ShowMenu(ToolButton: TKASToolButton);
    procedure ToolButtonClick(Sender: TObject);
    procedure ToolButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure ToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure ToolButtonMouseMove(Sender: TObject; Shift:TShiftState; X,Y:Integer);
    procedure ToolButtonDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ToolButtonDragDrop(Sender, Source: TObject; X,Y: Integer);
    procedure ToolButtonEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ToolItemLoaded(Item: TKASToolItem);
    procedure ToolMenuClicked(Sender: TObject);
    procedure UpdateButtonsTags;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
        PreferredHeight: Integer; WithThemeSpace: Boolean); override;
    procedure ControlsAligned; override;
    procedure FontChanged(Sender: TObject); override;
    function WrapButtons(UseWidth: integer;
        out NewWidth, NewHeight: Integer; Simulate: boolean): Boolean;
    procedure ResizeButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    function AddButton(Item: TKASToolItem): TKASToolButton;
    procedure AddToolItemExecutor(ToolItemClass: TKASToolItemClass;
                                  ExecuteFunction: TOnToolItemExecute);
    procedure Clear;
    procedure ClickItem(ToolItemID: String); overload;
    function InsertButton(InsertAt: Integer; Item: TKASToolItem): TKASToolButton;
    function InsertButton(InsertAt: TKASToolButton; Item: TKASToolItem): TKASToolButton;
    procedure MoveButton(ButtonIndex, MovePosition: Integer);
    procedure MoveButton(SourceButton: TKASToolButton; TargetToolBar: TKASToolBar; InsertAt: TKASToolButton);
    procedure RemoveButton(Index: Integer);
    procedure RemoveButton(Button: TKASToolButton);
    procedure RemoveToolItemExecutor(ExecuteFunction: TOnToolItemExecute);
    procedure UncheckAllButtons;
    procedure UpdateIcon(ToolButton: TKASToolButton);
    procedure UseItems(AItems: TKASToolBarItems);

    procedure LoadConfiguration(Config: TXmlConfig; RootNode: TXmlNode;
                                Loader: TKASToolBarLoader);
    procedure SaveConfiguration(Config: TXmlConfig; RootNode: TXmlNode);

    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure SetButtonSize(NewButtonWidth, NewButtonHeight: Integer);

    property Buttons[Index: Integer]: TKASToolButton read GetButton;
    property RowHeight: Integer read FRowHeight;

  published
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
    property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
    property OnToolButtonMouseDown: TOnToolButtonMouseUpDown read FOnToolButtonMouseDown write FOnToolButtonMouseDown;
    property OnToolButtonMouseUp: TOnToolButtonMouseUpDown read FOnToolButtonMouseUp write FOnToolButtonMouseUp;
    property OnToolButtonMouseMove: TOnToolButtonMouseMove read FOnToolButtonMouseMove write FOnToolButtonMouseMove;
    property OnToolButtonDragDrop: TOnToolButtonDragDrop read FOnToolButtonDragDrop write FOnToolButtonDragDrop;
    property OnToolButtonEndDrag: TOnToolButtonEndDrag read FOnToolButtonEndDrag write FOnToolButtonEndDrag;
    property OnToolButtonDragOver: TOnToolButtonDragOver read FOnToolButtonDragOver write FOnToolButtonDragOver;
    property OnToolItemExecute: TOnToolItemExecute read FOnToolItemExecute write FOnToolItemExecute;
    property OnToolItemShortcutsHint: TOnToolItemShortcutsHint read FOnToolItemShortcutsHint write FOnToolItemShortcutsHint;
    property RadioToolBar: Boolean read FRadioToolBar write FRadioToolBar default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GlyphSize: Integer read FGlyphSize write SetGlyphSize;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property ShowDividerAsButton: Boolean read FShowDividerAsButton write FShowDividerAsButton default False;

    property ChangePath: String read GetChangePath write SetChangePath;
    property EnvVar: String read GetEnvVar write SetEnvVar;
  end;

procedure Register;

implementation

uses
  Themes, types, math, DCOSUtils;

type
  PToolItemExecutor = ^TToolItemExecutor;
  TToolItemExecutor = record
    ToolItemClass: TKASToolItemClass;
    ToolItemExecute: TOnToolItemExecute;
  end;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASToolBar]);
end;

{ TKASToolBar }

procedure TKASToolBar.InsertButton(InsertAt: Integer; ToolButton: TKASToolButton);
begin
  if InsertAt < 0 then
    InsertAt:= 0;
  if InsertAt > ButtonList.Count then
    InsertAt:= ButtonList.Count;

  ButtonList.Insert(InsertAt, ToolButton);
  FToolItems.Insert(InsertAt, ToolButton.ToolItem);

  UpdateButtonsTags;
  ResizeButtons;
end;

function TKASToolBar.InsertButton(InsertAt: TKASToolButton; Item: TKASToolItem): TKASToolButton;
var
  Index: Integer;
begin
  Index := ButtonList.IndexOf(InsertAt);
  if Index < 0 then
    Index := ButtonCount;
  Result := InsertButton(Index, Item);
end;

procedure TKASToolBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  WrapButtons(Width, PreferredWidth, PreferredHeight, True);
end;

procedure TKASToolBar.ControlsAligned;
var
  NewWidth, NewHeight: integer;
begin
  if tbfPlacingControls in FKASToolBarFlags then exit;
  Include(FKASToolBarFlags, tbfPlacingControls);
  try
    WrapButtons(Width, NewWidth, NewHeight, False);
  finally
    Exclude(FKASToolBarFlags, tbfPlacingControls);
  end;
end;

procedure TKASToolBar.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  ResizeButtons;
end;

function TKASToolBar.WrapButtons(UseWidth: integer;
  out NewWidth, NewHeight: Integer; Simulate: boolean): Boolean;
var
  ARect: TRect;
  x: Integer;
  y: Integer;
  CurControl: TControl;
  StartX: Integer;

  procedure CalculatePosition;
  var
    NewBounds: TRect;
  begin
    NewBounds := Bounds(x, y, CurControl.Width, RowHeight);
    repeat
      if (not Wrapable) or
         (NewBounds.Right <= ARect.Right) or
         (NewBounds.Left = StartX) then
      begin
        // control fits into the row
        x := NewBounds.Left;
        y := NewBounds.Top;
        break;
      end;

      // try next row
      NewBounds.Left := StartX;
      NewBounds.Right := NewBounds.Left + CurControl.Width;
      inc(NewBounds.Top, RowHeight);
      inc(NewBounds.Bottom, RowHeight);
    until false;
  end;

var
  CurClientRect: TRect;
  AdjustClientFrame: TRect;
  i: Integer;
  w, h: Longint;
begin
  Result := True;
  NewWidth := 0;
  NewHeight := 0;
  DisableAlign;
  BeginUpdate;
  try
    CurClientRect := ClientRect;
    inc(CurClientRect.Right, UseWidth - Width);
    ARect := CurClientRect;
    AdjustClientRect(ARect);
    AdjustClientFrame.Left := ARect.Left - CurClientRect.Left;
    AdjustClientFrame.Top := ARect.Top - CurClientRect.Top;
    AdjustClientFrame.Right := CurClientRect.Right - ARect.Right;
    AdjustClientFrame.Bottom := CurClientRect.Bottom - ARect.Bottom;
    //DebugLn(['TToolBar.WrapButtons ',DbgSName(Self),' ARect=',dbgs(ARect)]);
    // important: top, left button must start in the AdjustClientRect top, left
    // otherwise Toolbar.AutoSize=true will create an endless loop
    StartX := ARect.Left;
    x := StartX;
    y := ARect.Top;
    for i := 0 to ButtonList.Count - 1 do
    begin
      CurControl := TControl(ButtonList[i]);
      if not CurControl.IsControlVisible then
        Continue;
      CalculatePosition;

      w := CurControl.Width;
      h := CurControl.Height;

      if (not Simulate) and ((CurControl.Left <> x) or (CurControl.Top <> y)) then
      begin
        CurControl.SetBounds(x,y,w,h); // Note: do not use SetBoundsKeepBase
      end;

      // adjust NewWidth, NewHeight
      NewWidth := Max(NewWidth, x + w + AdjustClientFrame.Right);
      NewHeight := Max(NewHeight, y + h + AdjustClientFrame.Bottom);

      // step to next position
      inc(x,w);
    end;
  finally
    EndUpdate;
    EnableAlign;
  end;
end;

procedure TKASToolBar.ResizeButtons;
var
  w, h: LongInt;
  i: Integer;
  CurControl: TControl;
begin
  if FUpdateCount > 0 then
  begin
    FResizeButtonsNeeded := True;
    Exit;
  end;

  InvalidatePreferredChildSizes;
  FRowHeight := ButtonHeight;  // Row height is at least initial button height

  // First recalculate RowHeight.
  for i := 0 to ButtonList.Count - 1 do
  begin
    CurControl := TControl(ButtonList[i]);
    w := ButtonWidth;
    h := ButtonHeight;
    CurControl.GetPreferredSize(w, h);
    if FRowHeight < h then
      FRowHeight := h;
  end;

  FResizeButtonsNeeded := False;

  // Now resize buttons.
  DisableAlign;
  BeginUpdate;
  try
    for i := 0 to ButtonList.Count - 1 do
    begin
      CurControl := TControl(ButtonList[i]);
      w := ButtonWidth;
      h := RowHeight;
      CurControl.GetPreferredSize(w, h);
      if (CurControl.Width <> w) or (CurControl.Height <> h) then
        CurControl.SetBounds(CurControl.Left, CurControl.Top, w, h);
    end;
    InvalidatePreferredSize;
    AdjustSize;
  finally
    EndUpdate;
    EnableAlign;
  end;
end;

procedure TKASToolBar.SaveConfiguration(Config: TXmlConfig; RootNode: TXmlNode);
var
  Node: TXmlNode;
  Item: TKASToolItem;
  i:    Integer;
begin
  if ButtonCount > 0 then
  begin
    Node := Config.AddNode(RootNode, 'Row');
    for i := 0 to ButtonCount - 1 do
    begin
      Item := TKASToolButton(Buttons[i]).ToolItem;
      Item.Save(Config, Node);
    end;
  end;
end;

procedure TKASToolBar.DrawLinkIcon(Image: TBitMap);
var
  sizeLink : Integer;
  bmLinkIcon : TBitmap;
{$IFDEF LCLGTK2}
  bmTempIcon : TBitmap;
{$ENDIF}
  ToolItem: TKASNormalItem;
begin
  if (Image = nil) or (FOnLoadButtonGlyph = nil) then Exit;
  sizeLink := FGlyphSize div 2;
  ToolItem := TKASNormalItem.Create;
  ToolItem.Icon := 'emblem-symbolic-link';
  bmLinkIcon:= FOnLoadButtonGlyph(ToolItem, sizeLink, clBtnFace);
  ToolItem.Free;
  if Assigned(bmLinkIcon) then
  begin
{$IFDEF LCLGTK2} // Under GTK2 can not draw over alpha transparent pixels
    bmTempIcon := TBitmap.Create;
    bmTempIcon.Assign(Image);
    Image.FreeImage;
    Image.SetSize(FGlyphSize, FGlyphSize);
    Image.Canvas.Brush.Color := clBtnFace;
    Image.Canvas.FillRect(0, 0, FGlyphSize, FGlyphSize);
    Image.Canvas.Draw(0, 0, bmTempIcon);
    bmTempIcon.Free;
{$ENDIF}
    Image.Canvas.Draw(FGlyphSize-sizeLink+2,FGlyphSize-sizeLink+2, bmLinkIcon);
    Image.TransparentColor:= clBtnFace;
    Image.Transparent:= True;
    bmLinkIcon.Free;
  end;
end;

function TKASToolBar.LoadBtnIcon(IconPath: String): TBitMap;
var
  picture: TPicture;
begin
  if (IconPath = '') or (not mbFileExists(IconPath)) then Exit(nil);

  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(IconPath);
    Result := TBitmap.Create;
    Result.Assign(Picture.Bitmap);
  finally
    FreeAndNil(Picture);
  end;
end;

procedure TKASToolBar.LoadConfiguration(Config: TXmlConfig; RootNode: TXmlNode;
                                        Loader: TKASToolBarLoader);
var
  Node: TXmlNode;
begin
  Clear;
  BeginUpdate;
  try
    Node := Config.FindNode(RootNode, 'Row', False);
    if Assigned(Node) then
      Loader.Load(Config, Node, @ToolItemLoaded);
  finally
    EndUpdate;
  end;
end;

procedure TKASToolBar.AssignToolButtonProperties(ToolButton: TKASToolButton);
begin
  ToolButton.OnClick:= @ToolButtonClick;
  ToolButton.OnMouseDown:= @ToolButtonMouseDown;
  ToolButton.OnMouseUp:= @ToolButtonMouseUp;
  ToolButton.OnMouseMove:= @ToolButtonMouseMove;
  ToolButton.OnDragDrop:= @ToolButtonDragDrop;
  ToolButton.OnDragOver:= @ToolButtonDragOver;
  ToolButton.OnEndDrag:= @ToolButtonEndDrag;
end;

function TKASToolBar.GetChangePath: String;
begin
end;

function TKASToolBar.GetEnvVar: String;
begin
end;

function TKASToolBar.GetToolItemShortcutsHint(Item: TKASToolItem): String;
begin
  Result := '';
  if Assigned(FOnToolItemShortcutsHint) and (Item is TKASNormalItem) then
    Result := FOnToolItemShortcutsHint(TKASNormalItem(Item));
end;

function TKASToolBar.GetButton(Index: Integer): TKASToolButton;
begin
  Result:= TKASToolButton(ButtonList.Items[Index]);
end;

procedure TKASToolBar.SetChangePath(const AValue: String);
begin
end;

procedure TKASToolBar.SetEnvVar(const AValue: String);
begin
end;

procedure TKASToolBar.SetFlat(const AValue: Boolean);
var
  I: Integer;
begin
  FFlat:= AValue;
  for I:= 0 to ButtonList.Count - 1 do
    TKASToolButton(ButtonList.Items[I]).Flat:= FFlat;
end;

procedure TKASToolBar.SetGlyphSize(const AValue: Integer);
var
  I: Integer;
begin
  if FGlyphSize = AValue then Exit;
  FGlyphSize:= AValue;

  BeginUpdate;
  try
    for I := 0 to ButtonList.Count - 1 do
      UpdateIcon(TKASToolButton(ButtonList[i]));
  finally
    EndUpdate;
  end;
end;

procedure TKASToolBar.ShowMenu(ToolButton: TKASToolButton);
var
  Depth: Integer = 0;
  procedure MakeMenu(PopupMenu: TMenuItem; MenuItem: TKASMenuItem);
  var
    I: Integer;
    Item: TKASToolItem;
    PopupMenuItem: TMenuItem;
    BitmapTmp: TBitmap = nil;
    sText: String;
  begin
    if Depth < 10 then
    begin
      Inc(Depth);
      for I := 0 to MenuItem.SubItems.Count - 1 do
      begin
        Item := MenuItem.SubItems.Items[I];
        if Item is TKASSeparatorItem then
        begin
          PopupMenu.AddSeparator;
        end
        else
        begin
          PopupMenuItem := TMenuItem.Create(PopupMenu);
          sText := Item.GetEffectiveText;
          if sText = '' then
            sText := Item.GetEffectiveHint;
          PopupMenuItem.Caption := sText;

          if Item is TKASNormalItem then
          begin
            if Assigned(FOnLoadButtonGlyph) then
              BitmapTmp := FOnLoadButtonGlyph(Item, 16, clMenu);
            if not Assigned(BitmapTmp) then
              BitmapTmp := LoadBtnIcon(TKASNormalItem(Item).Icon);

            PopupMenuItem.Bitmap := BitmapTmp;
            FreeAndNil(BitmapTmp);
          end;

          PopupMenuItem.Tag := PtrInt(Item);
          PopupMenuItem.OnClick := TNotifyEvent(@ToolMenuClicked);
          PopupMenu.Add(PopupMenuItem);

          if Item is TKASMenuItem then
            MakeMenu(PopupMenuItem, TKASMenuItem(Item));
        end;
      end;
    end;
  end;
var
  Point: TPoint;
begin
  FToolPopupMenu.Free;
  FToolPopupMenu := TPopupMenu.Create(Self);
  MakeMenu(FToolPopupMenu.Items, ToolButton.ToolItem as TKASMenuItem);
  Point.x := ToolButton.Left;
  Point.y := ToolButton.Top + ToolButton.Height;
  Point := Self.ClientToScreen(Point);
  FToolPopupMenu.PopUp(Point.x, Point.y);
end;

procedure TKASToolBar.ToolButtonClick(Sender: TObject);
var
  Button: TKASToolButton;
begin
  Button := Sender as TKASToolButton;

  // Do not allow depressing down buttons.
  if FRadioToolBar and not Button.Down then
    Button.Down := True;

  if not DoExecuteToolItem(Button.ToolItem) then
  begin
    if Assigned(FOnToolButtonClick) then
      FOnToolButtonClick(Button)
    else if Button.ToolItem is TKASMenuItem then
    begin
      ShowMenu(Button);
    end;
  end;
end;

procedure TKASToolBar.ToolButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Assigned(FOnToolButtonMouseDown) then
    FOnToolButtonMouseDown(Sender, Button, Shift, X,Y);
end;

procedure TKASToolBar.ToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Assigned(FOnToolButtonMouseUp) then
    FOnToolButtonMouseUp(Sender, Button, Shift, X,Y);
end;

procedure TKASToolBar.ToolItemLoaded(Item: TKASToolItem);
begin
  AddButton(Item);
end;

procedure TKASToolBar.ToolMenuClicked(Sender: TObject);
begin
  DoExecuteToolItem(TKASToolItem((Sender as TMenuItem).Tag));
end;

procedure TKASToolBar.ToolButtonMouseMove(Sender: TObject; Shift:TShiftState; X,Y:Integer);
begin
  if Assigned(FOnToolButtonMouseMove) then
    FOnToolButtonMouseMove(Sender, Shift, X,Y, (Sender as TSpeedButton).Tag);
end;

procedure TKASToolBar.ToolButtonDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnToolButtonDragOver) then
    FOnToolButtonDragOver(Sender, Source, X,Y, State, Accept, (Sender as TSpeedButton).Tag);
end;

procedure TKASToolBar.ToolButtonDragDrop(Sender, Source: TObject; X,Y: Integer);
begin
  if Assigned(FOnToolButtonDragDrop) then
    FOnToolButtonDragDrop(Sender, Source, X, Y);
end;

procedure TKASToolBar.ToolButtonEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnToolButtonEndDrag) then
    FOnToolButtonEndDrag(Sender, Target, X, Y);
end;

procedure TKASToolBar.MoveButton(ButtonIndex, MovePosition: Integer);
begin
  ButtonList.Move(ButtonIndex, MovePosition);
  FToolItems.Move(ButtonIndex, MovePosition);
  UpdateButtonsTags;
  ResizeButtons;
end;

procedure TKASToolBar.MoveButton(SourceButton: TKASToolButton; TargetToolBar: TKASToolBar; InsertAt: TKASToolButton);
var
  Index: Integer;
  ToolItem: TKASToolItem;
begin
  Index := FindButton(SourceButton);
  if (Index <> -1) and (FToolItems[Index] = SourceButton.ToolItem) then
  begin
    SourceButton.FToolItem := nil;
    TargetToolBar.InsertButton(InsertAt, FToolItems.ReleaseItem(Index));
    ButtonList.Delete(Index);
    Application.ReleaseComponent(SourceButton); // Free later
    UpdateButtonsTags;
    Resize;
  end;
end;

procedure TKASToolBar.UpdateButtonsTags;
var
  I: Integer;
begin
  for I:= 0 to ButtonList.Count - 1 do
    TKASToolButton(ButtonList.Items[I]).Tag:= I;
end;

procedure TKASToolBar.UpdateIcon(ToolButton: TKASToolButton);
var
  Bitmap: TBitmap = nil;
begin
  try
    if Assigned(FOnLoadButtonGlyph) then
      Bitmap := FOnLoadButtonGlyph(ToolButton.ToolItem, FGlyphSize, clBtnFace);

    if not Assigned(Bitmap) and (ToolButton.ToolItem is TKASNormalItem) then
      Bitmap := LoadBtnIcon(TKASNormalItem(ToolButton.ToolItem).Icon);

    if (ToolButton.ToolItem is TKASMenuItem) and Assigned(Bitmap) then
      DrawLinkIcon(Bitmap);

    ToolButton.Glyph.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TKASToolBar.UseItems(AItems: TKASToolBarItems);
var
  i: Integer;
  Button: TKASToolButton;
begin
  if Assigned(AItems) then
  begin
    BeginUpdate;

    Clear;
    if FOwnsToolItems then
      FToolItems.Free;
    FToolItems := AItems;
    FOwnsToolItems := False;

    // Insert the existing items as buttons.
    for i := 0 to FToolItems.Count - 1 do
    begin
      Button := CreateButton(FToolItems.Items[i]);
      if Assigned(Button) then
        ButtonList.Insert(ButtonCount, Button);
    end;
    UpdateButtonsTags;
    ResizeButtons;

    EndUpdate;
  end;
end;

procedure TKASToolBar.Clear;
var
  I: Integer;
begin
  BeginUpdate;

  for I := 0 to ButtonList.Count - 1 do
    TKASToolButton(ButtonList.Items[I]).Free;
  ButtonList.Clear;
  if Assigned(FToolItems) then
    FToolItems.Clear;

  EndUpdate;
end;

procedure TKASToolBar.ClearExecutors;
var
  I: Integer;
begin
  for I := 0 to FToolItemExecutors.Count - 1 do
    Dispose(PToolItemExecutor(FToolItemExecutors[I]));
  FToolItemExecutors.Clear;
end;

procedure TKASToolBar.ClickItem(ToolItemID: String);
var
  I: Integer;
  Button: TKASToolButton;
  NormalItem: TKASNormalItem;
begin
  for I := 0 to ButtonList.Count - 1 do
  begin
    Button := TKASToolButton(ButtonList.Items[I]);
    if Button.ToolItem is TKASNormalItem then
    begin
      NormalItem := TKASNormalItem(Button.ToolItem);
      if NormalItem.ID = ToolItemID then
      begin
        Button.Click;
        Break;
      end;
    end;
    // TODO Handle TKASMenuItem
  end;
end;

procedure TKASToolBar.SetButtonHeight(const AValue: Integer);
begin
  SetButtonSize(ButtonWidth, AValue);
end;

procedure TKASToolBar.SetButtonWidth(const AValue: Integer);
begin
  SetButtonSize(AValue, ButtonHeight);
end;

constructor TKASToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FGlyphSize:= 16; // by default
  FUpdateCount:= 0;
  FButtonWidth := 23;
  FButtonHeight := 22;
  FKASToolBarFlags := [];
  FToolItemExecutors := TFPList.Create;
  FToolItems := TKASToolBarItems.Create;
  FOwnsToolItems := True;
end;

function TKASToolBar.CreateButton(Item: TKASToolItem): TKASToolButton;
begin
  if Assigned(Item) then
  begin
    if FOwnsToolItems then
      Item.SetToolOwner(Self);

    if Item is TKASSeparatorItem then
    begin
      Result := TKASToolDivider.Create(Self, Item);
    end
    else
    begin
      Result := TKASToolButton.Create(Self, Item);
      Result.ShowHint := True;
      Result.Caption  := Item.GetEffectiveText;
      Result.Hint     := Item.GetEffectiveHint;
    end;

    Result.Flat := FFlat;
    if FRadioToolBar then
      begin
        Result.GroupIndex := 1;
        Result.AllowAllUp := True;
      end;

    Result.ShowCaption := ShowCaptions;
    UpdateIcon(Result);
    AssignToolButtonProperties(Result);

    Result.Parent := Self;
  end
  else
    Result := nil;
end;

destructor TKASToolBar.Destroy;
begin
  if not FOwnsToolItems then
    FToolItems := nil;  // Unassign before Clear so that items are not cleared.
  Clear;
  inherited Destroy;
  ClearExecutors;
  FToolItemExecutors.Free;
  if FOwnsToolItems then
    FToolItems.Free;
end;

function TKASToolBar.DoExecuteToolItem(Item: TKASToolItem): Boolean;
var
  I: Integer;
  Executor: PToolItemExecutor;
  BestMatch: PToolItemExecutor = nil;
begin
  for I := 0 to FToolItemExecutors.Count - 1 do
  begin
    Executor := PToolItemExecutor(FToolItemExecutors[I]);
    if Assigned(Executor^.ToolItemExecute) and
       Item.InheritsFrom(Executor^.ToolItemClass) and
       (not Assigned(BestMatch) or
        (Executor^.ToolItemClass.InheritsFrom(BestMatch^.ToolItemClass))) then
    begin
      BestMatch := Executor;
    end;
  end;
  Result := Assigned(BestMatch);
  if Result then
    BestMatch^.ToolItemExecute(Item);
end;

procedure TKASToolBar.BeginUpdate;
begin
  Inc(FUpdateCount);
  inherited BeginUpdate;
  DisableAutoSizing;
end;

procedure TKASToolBar.EndUpdate;
begin
  EnableAutoSizing;
  inherited EndUpdate;
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FResizeButtonsNeeded then
    ResizeButtons;
end;

function TKASToolBar.FindButton(Button: TKASToolButton): Integer;
var
  I: Integer;
begin
  for I := 0 to ButtonList.Count - 1 do
    if TKASToolButton(ButtonList[I]) = Button then
      Exit(I);
  Result := -1;
end;

procedure TKASToolBar.SetButtonSize(NewButtonWidth, NewButtonHeight: Integer);
begin
  FButtonWidth  := NewButtonWidth;
  FButtonHeight := NewButtonHeight;
  ResizeButtons;
end;

function TKASToolBar.AddButton(Item: TKASToolItem): TKASToolButton;
begin
  Result := InsertButton(ButtonCount, Item);
end;

procedure TKASToolBar.AddToolItemExecutor(ToolItemClass: TKASToolItemClass; ExecuteFunction: TOnToolItemExecute);
var
  Executor: PToolItemExecutor;
begin
  New(Executor);
  FToolItemExecutors.Add(Executor);
  Executor^.ToolItemClass := ToolItemClass;
  Executor^.ToolItemExecute := ExecuteFunction;
end;

function TKASToolBar.InsertButton(InsertAt: Integer; Item: TKASToolItem): TKASToolButton;
begin
  Result := CreateButton(Item);
  if Assigned(Result) then
    InsertButton(InsertAt, Result);
end;

procedure TKASToolBar.RemoveButton(Index: Integer);
var
  Button: TKASToolButton;
begin
  Button := TKASToolButton(ButtonList.Items[Index]);
  ButtonList.Delete(Index);
  Button.Free;
  FToolItems.Remove(Index);
  UpdateButtonsTags;
  Resize;
end;

procedure TKASToolBar.RemoveButton(Button: TKASToolButton);
var
  Index: Integer;
begin
  Index := FindButton(Button);
  if Index <> -1 then
    RemoveButton(Index);
end;

procedure TKASToolBar.RemoveToolItemExecutor(ExecuteFunction: TOnToolItemExecute);
var
  Executor: PToolItemExecutor;
  I: Integer;
begin
  for I := FToolItemExecutors.Count - 1 downto 0 do
  begin
    Executor := PToolItemExecutor(FToolItemExecutors[I]);
    if (TMethod(Executor^.ToolItemExecute).Code = TMethod(ExecuteFunction).Code) and
       (TMethod(Executor^.ToolItemExecute).Data = TMethod(ExecuteFunction).Data) then
    begin
      Dispose(Executor);
      FToolItemExecutors.Delete(I);
    end;
  end;
end;

procedure TKASToolBar.UncheckAllButtons;
var
  I: Integer;
begin
  for I:= 0 to ButtonCount - 1 do
    Buttons[I].Down:= False;
end;

{ TKASToolButton }

procedure TKASToolButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  TextSize: TSize;
begin
  if Assigned(Parent) then
  begin
    if ShowCaption and (Caption <> EmptyStr) then
    begin
      // Size to extent of the icon + caption.
      // Minimum size is the ButtonWidth x RowHeight of the toolbar.
      TextSize := Canvas.TextExtent(Caption);
      PreferredWidth  := Max(TextSize.cx + Glyph.Width + 16, ToolBar.ButtonWidth);
      PreferredHeight := Max(TextSize.cy + 4, ToolBar.RowHeight);
    end
    else
    begin
      PreferredWidth  := ToolBar.ButtonWidth;
      PreferredHeight := ToolBar.RowHeight;
    end;
  end
  else
    inherited;
end;

constructor TKASToolButton.Create(AOwner: TComponent; Item: TKASToolItem);
begin
  inherited Create(AOwner);
  FToolItem := Item;
end;

function TKASToolButton.GetToolBar: TKASToolBar;
begin
  Result := Parent as TKASToolBar;
end;

{ TKASToolDivider }

procedure TKASToolDivider.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if Assigned(Parent) and (Parent is TKASToolBar) and
     not TKASToolBar(Parent).FShowDividerAsButton then
  begin
    PreferredWidth  := 5;
    PreferredHeight := TKASToolBar(Parent).RowHeight;
  end
  else
    inherited;
end;

procedure TKASToolDivider.Paint;
var
  DividerRect: TRect;
  Details: TThemedElementDetails;
begin
  if Assigned(Parent) and (Parent is TKASToolBar) and
     not TKASToolBar(Parent).FShowDividerAsButton then
  begin
    DividerRect:= ClientRect;
    Details:= ThemeServices.GetElementDetails(ttbSeparatorNormal);
    // Theme services have no strict rule to draw divider in the center,
    // so we should calculate rectangle here
    // on windows 7 divider can't be less than 4 pixels
    if (DividerRect.Right - DividerRect.Left) > 5 then
    begin
      DividerRect.Left := (DividerRect.Left + DividerRect.Right) div 2 - 3;
      DividerRect.Right := DividerRect.Left + 5;
    end;
    ThemeServices.DrawElement(Canvas.GetUpdatedHandle([csBrushValid, csPenValid]), Details, DividerRect);
  end
  else
    inherited Paint;
end;

end.


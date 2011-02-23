{
   Double Commander components
   -------------------------------------------------------------------------
   Toolbar panel class

   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)
   
   contributors:
   
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
  Graphics, Dialogs, ExtCtrls, Buttons, IniFiles, FileUtil, KASBarFiles;

type

  TOnToolButtonClick = procedure (Sender: TObject; NumberOfButton: Integer) of object;
  TOnToolButtonMouseUpDown = procedure (Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer; NumberOfButton: Integer) of object;
  TOnToolButtonMouseMove = procedure (Sender: TObject; Shift:TShiftState; X,Y:Integer; NumberOfButton: Integer) of object;
  TOnToolButtonDragOver = procedure(Sender, Source: TObject; X,Y: Integer;
               State: TDragState; var Accept: Boolean; NumberOfButton: Integer) of object;
  TOnToolButtonDragDrop = procedure(Sender, Source: TObject; X,Y: Integer; NumberOfButton: Integer) of object;
  TOnToolButtonEndDrag = procedure(Sender, Target: TObject; X,Y: Integer; NumberOfButton: Integer) of object;
  TOnLoadButtonGlyph = function (sIconFileName: String; iIconSize: Integer; clBackColor: TColor): TBitmap of object;

  { TKASToolButton }

  TKASToolButton = class(TSpeedButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TKASToolDivider }

  TKASToolDivider = class(TKASToolButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure Paint; override;
  end;

  { TKASToolBar }

  TKASToolBar = class(TToolBar)
  private
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FRowHeight: Integer;
    FUpdateCount: Integer;
    FGlyphSize: Integer;
    FRadioToolBar: Boolean;
    FShowDividerAsButton: Boolean;
    FFlat: Boolean;
    FBarFile: TBarClass;
    FOnToolButtonClick: TOnToolButtonClick;
    FOnToolButtonMouseDown: TOnToolButtonMouseUpDown;
    FOnToolButtonMouseUp: TOnToolButtonMouseUpDown;
    FOnToolButtonMouseMove: TOnToolButtonMouseMove;
    FOnToolButtonDragOver: TOnToolButtonDragOver;
    FOnToolButtonDragDrop: TOnToolButtonDragDrop;
    FOnToolButtonEndDrag: TOnToolButtonEndDrag;
    FOnLoadButtonGlyph: TOnLoadButtonGlyph;
    FKASToolBarFlags: TToolBarFlags;
    FResizeButtonsNeeded: Boolean;
    procedure AssignToolButtonProperties(ToolButton: TKASToolButton);
    function GetChangePath: String;
    function GetEnvVar: String;
    function LoadBtnIcon(IconPath: String): TBitMap;
    function GetButton(Index: Integer): TSpeedButton;
    function GetCommand(Index: Integer): String;
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetChangePath(const AValue: String);
    procedure SetCommand(Index: Integer; const AValue: String);
    procedure SetEnvVar(const AValue: String);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyphSize(const AValue: Integer);
    procedure ToolButtonClick(Sender: TObject);
    procedure ToolButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure ToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure ToolButtonMouseMove(Sender: TObject; Shift:TShiftState; X,Y:Integer);
    procedure ToolButtonDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ToolButtonDragDrop(Sender, Source: TObject; X,Y: Integer);
    procedure ToolButtonEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure UpdateButtonsTags;
  protected
    { Protected declarations }
    procedure InsertButton(InsertAt: Integer; ToolButton: TSpeedButton);
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
    
    function AddDivider: Integer;
    function AddButton(sCaption, sCommand, sHint: String; Bitmap: TBitmap): Integer;
    function AddButton(sCaption, sCommand, sHint, sBitmap : String): Integer;
    function AddButtonX(sCaption, CmdX, ParamX, PathX, MenuX, MiskX: String; Bitmap: TBitmap): Integer;
    function AddButtonX(sCaption, CmdX, ParamX, PathX, MenuX, MiskX, ButtonX: String): Integer;

    function InsertButton(InsertAt: Integer; sCaption, sCommand, sHint: String; Bitmap: TBitmap): Integer;
    function InsertButton(InsertAt: Integer; sCaption, sCommand, sHint, sBitmap : String) : Integer;
    function InsertButtonX(InsertAt: Integer; sCaption, CmdX, ParamX, PathX, MenuX, MiskX: String; Bitmap: TBitmap): Integer;
    function InsertButtonX(InsertAt: Integer; sCaption, CmdX, ParamX, PathX, MenuX, MiskX, ButtonX: String): Integer;

    procedure Clear;
    procedure RemoveButton(Index: Integer);
    procedure MoveButton(ButtonIndex, MovePosition: integer);
    procedure UncheckAllButtons;

    function GetButtonX(Index: Integer; What: TInfor): String;
    procedure SetButtonX(Index: Integer; What: Tinfor; Value: String);

    procedure LoadFromIniFile(IniFile: TIniFile);
    procedure SaveToIniFile(IniFile: TIniFile);
    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);

    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure SetButtonSize(NewButtonWidth, NewButtonHeight: Integer);

    property Buttons[Index: Integer]: TSpeedButton read GetButton;
    property Commands[Index: Integer]: String read GetCommand write SetCommand;
    property BarFile: TBarClass read FBarFile;
    property RowHeight: Integer read FRowHeight;
  published
    { Published declarations }
    property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
    property OnToolButtonMouseDown: TOnToolButtonMouseUpDown read FOnToolButtonMouseDown write FOnToolButtonMouseDown;
    property OnToolButtonMouseUp: TOnToolButtonMouseUpDown read FOnToolButtonMouseUp write FOnToolButtonMouseUp;
    property OnToolButtonMouseMove: TOnToolButtonMouseMove read FOnToolButtonMouseMove write FOnToolButtonMouseMove;
    property OnToolButtonDragDrop: TOnToolButtonDragDrop read FOnToolButtonDragDrop write FOnToolButtonDragDrop;
    property OnToolButtonEndDrag: TOnToolButtonEndDrag read FOnToolButtonEndDrag write FOnToolButtonEndDrag;
    property OnToolButtonDragOver: TOnToolButtonDragOver read FOnToolButtonDragOver write FOnToolButtonDragOver;
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
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
  GraphType, Themes, types, math;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASToolBar]);
end;

{ TKASToolBar }

procedure TKASToolBar.InsertButton(InsertAt: Integer; ToolButton: TSpeedButton);
begin
  if InsertAt < 0 then
    InsertAt:= 0;
  if InsertAt > ButtonList.Count then
    InsertAt:= ButtonList.Count;

  ToolButton.Parent:= Self;
  ButtonList.Insert(InsertAt, ToolButton);

  UpdateButtonsTags;
  ResizeButtons;
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

function TKASToolBar.GetButtonX(Index: Integer; What: TInfor): String;
begin
  Result:= FBarFile.GetButtonX(Index, What);
end;

procedure TKASToolBar.SetButtonX(Index: Integer; What: TInfor; Value: String);
var
  Bitmap: TBitmap;
begin
  FBarFile.SetButtonX(Index, What, Value);
  if What = ButtonX then
    begin
      if FBarFile.GetButtonX(Index, MenuX)= '-' then Value:= '-'; // To pass separator to FOnLoadButtonGlyph
      if Assigned(FOnLoadButtonGlyph) then
        Bitmap := FOnLoadButtonGlyph(Value, FGlyphSize, Color)
      else
        Bitmap := LoadBtnIcon(Value);
      Buttons[Index].Glyph.Assign(Bitmap);
      if Assigned(Bitmap) then
        FreeAndNil(Bitmap);
    end;
end;

function TKASToolBar.LoadBtnIcon(IconPath: String): TBitMap;
var
  picture: TPicture;
begin
  if (IconPath = '') or (not FileExists(IconPath)) then Exit(nil);

  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(IconPath);
    Result := TBitmap.Create;
    Result.Assign(Picture.Bitmap);
  finally
    FreeAndNil(Picture);
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
  Result:= FBarFile.ChangePath;
end;

function TKASToolBar.GetEnvVar: String;
begin
  Result:= FBarFile.EnvVar;
end;

function TKASToolBar.GetButton(Index: Integer): TSpeedButton;
begin
  Result:= TSpeedButton(ButtonList.Items[Index]);
end;

procedure TKASToolBar.SetChangePath(const AValue: String);
begin
  FBarFile.ChangePath:= AValue;
end;

procedure TKASToolBar.SetCommand(Index: Integer; const AValue: String);
begin
  SetButtonX(Index, CmdX, AValue);
end;

procedure TKASToolBar.SetEnvVar(const AValue: String);
begin
  FBarFile.EnvVar:= AValue;
end;

procedure TKASToolBar.SetFlat(const AValue: Boolean);
var
  I: Integer;
begin
  FFlat:= AValue;
  for I:= 0 to ButtonList.Count - 1 do
    TSpeedButton(ButtonList.Items[I]).Flat:= FFlat;
end;

procedure TKASToolBar.SetGlyphSize(const AValue: Integer);
var
  I: Integer;
begin
  if FGlyphSize = AValue then Exit;
  FGlyphSize:= AValue;

  BeginUpdate;

  for I:= 0 to ButtonList.Count - 1 do
  begin
    SetButtonX(I, ButtonX, GetButtonX(I, ButtonX));
  end;

  EndUpdate;
end;

procedure TKASToolBar.ToolButtonClick(Sender: TObject);
begin
  if Assigned(FOnToolButtonClick) then
     FOnToolButtonClick(Self, (Sender as TSpeedButton).Tag);
end;

procedure TKASToolBar.ToolButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Assigned(FOnToolButtonMouseDown) then
     FOnToolButtonMouseDown(Sender, Button, Shift, X,Y, (Sender as TSpeedButton).Tag);
end;

procedure TKASToolBar.ToolButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Assigned(FOnToolButtonMouseUp) then
     FOnToolButtonMouseUp(Sender, Button, Shift, X,Y, (Sender as TSpeedButton).Tag);
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
     FOnToolButtonDragDrop(Sender, Source, X,Y, (Sender as TSpeedButton).Tag)
end;

procedure TKASToolBar.ToolButtonEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnToolButtonEndDrag) then
     FOnToolButtonEndDrag(Sender, Target, X,Y, (Sender as TSpeedButton).Tag)
end;

procedure TKASToolBar.MoveButton(ButtonIndex, MovePosition: integer);
var
  NewPosition: integer;
begin
  if ButtonIndex > MovePosition then NewPosition:= MovePosition else NewPosition:= MovePosition + 1;
  FBarFile.InsertButtonX(NewPosition, FBarFile.GetButtonX(ButtonIndex,ButtonX),
                                      FBarFile.GetButtonX(ButtonIndex,CmdX),
                                      FBarFile.GetButtonX(ButtonIndex,ParamX),
                                      FBarFile.GetButtonX(ButtonIndex,PathX),
                                      FBarFile.GetButtonX(ButtonIndex,MenuX),
                                      FBarFile.GetButtonX(ButtonIndex,MiskX));
  FBarFile.SetButtonX(NewPosition, IconicX, FBarFile.GetButtonX(ButtonIndex,IconicX)); // Because IconicX is not set in InsertButtonX
  ButtonList.Move(ButtonIndex, MovePosition);
  if ButtonIndex > MovePosition then
    FBarFile.RemoveButton(ButtonIndex + 1)
  else
    FBarFile.RemoveButton(ButtonIndex);
  UpdateButtonsTags;
  ResizeButtons;
end;

procedure TKASToolBar.UpdateButtonsTags;
var
  I: Integer;
begin
  for I:= 0 to ButtonList.Count - 1 do
    TSpeedButton(ButtonList.Items[I]).Tag:= I;
end;

procedure TKASToolBar.Clear;
var
  I: Integer;
begin
  BeginUpdate;

  for I:= ButtonList.Count - 1 downto 0 do
    begin
      TSpeedButton(ButtonList.Items[0]).Free;
      ButtonList.Delete(0);
    end;
  FBarFile.DeleteAllButtons;

  EndUpdate;
end;

function TKASToolBar.GetCommand(Index: Integer): String;
begin
 Result:= GetButtonX(Index, CmdX);
end;

procedure TKASToolBar.SetButtonHeight(const AValue: Integer);
begin
  SetButtonSize(ButtonWidth, AValue);
end;

procedure TKASToolBar.SetButtonWidth(const AValue: Integer);
begin
  SetButtonSize(AValue, ButtonHeight);
end;

{
function TKASToolBar.GetIconPath(Index: Integer): String;
begin
//  Result := FIconList[Index];
 Result := GetButtonX(Index,ButtonX);
end;
}

constructor TKASToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBarFile:= TBarClass.Create;
  FGlyphSize:= 16; // by default
  FUpdateCount:= 0;
  FButtonWidth := 23;
  FButtonHeight := 22;
  FKASToolBarFlags := [];
end;

destructor TKASToolBar.Destroy;
begin
  Clear;

  if Assigned(FBarFile) then
    FreeAndNil(FBarFile);

  inherited Destroy;
end;

procedure TKASToolBar.LoadFromIniFile(IniFile: TIniFile);
var  
  I: Integer;
  sMenu: String;
begin
  BeginUpdate;
  try
    Clear;
    FBarFile.LoadFromIniFile(IniFile);
    for I:= 0 to FBarFile.ButtonCount - 1 do
      begin
        sMenu:= FBarFile.GetButtonX(I, MenuX);
        if (sMenu = '-') and not FShowDividerAsButton then
          AddDivider
        else
          AddButton('', FBarFile.GetButtonX(I, CmdX), sMenu, FBarFile.GetButtonX(I, ButtonX));
      end;
  finally
    EndUpdate;
  end;
end;

procedure TKASToolBar.SaveToIniFile(IniFile: TIniFile);
begin
  FBarFile.SaveToIniFile(IniFile);
end;

procedure TKASToolBar.LoadFromFile(FileName: String);
var
  IniFile: TIniFile = nil;
begin
  try
    IniFile:= TIniFile.Create(FileName);
    LoadFromIniFile(IniFile);
  finally
    if Assigned(IniFile) then
      FreeAndNil(IniFile);
  end;
end;

procedure TKASToolBar.SaveToFile(FileName: String);
var
  IniFile: TIniFile = nil;
begin
  try
    IniFile:= TIniFile.Create(FileName);
    FBarFile.SaveToIniFile(IniFile);
  finally
    if Assigned(IniFile) then
      FreeAndNil(IniFile);
  end;
end;

procedure TKASToolBar.BeginUpdate;
begin
  Inc(FUpdateCount);
  inherited BeginUpdate;
end;

procedure TKASToolBar.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FResizeButtonsNeeded then
    ResizeButtons;
  inherited EndUpdate;
end;

procedure TKASToolBar.SetButtonSize(NewButtonWidth, NewButtonHeight: Integer);
begin
  FButtonWidth  := NewButtonWidth;
  FButtonHeight := NewButtonHeight;
  ResizeButtons;
end;

function TKASToolBar.AddDivider: Integer;
var
  ToolDivider: TKASToolDivider;
begin
  ToolDivider:= TKASToolDivider.Create(Self);
  AssignToolButtonProperties(ToolDivider);

  InsertButton(ButtonList.Count, ToolDivider);

  Result:= ToolDivider.Tag;
end;

function TKASToolBar.AddButton(sCaption, sCommand, sHint: String; Bitmap: TBitmap): Integer;
begin
  Result:= InsertButton(ButtonList.Count, sCaption, sCommand, sHint, Bitmap);
end;

function TKASToolBar.AddButton(sCaption, sCommand, sHint, sBitmap: String): Integer;
begin
  Result:= InsertButton(ButtonList.Count, sCaption, sCommand, sHint, sBitmap);
end;

function TKASToolBar.AddButtonX(sCaption, CmdX, ParamX, PathX, MenuX, MiskX: String; Bitmap: TBitmap): Integer;
begin
  Result:= InsertButton(ButtonList.Count, sCaption, CmdX, MenuX, Bitmap);
  FBarFile.AddButtonX('', CmdX, ParamX, PathX, MenuX, MiskX);
end;

function TKASToolBar.AddButtonX(sCaption, CmdX, ParamX, PathX, MenuX, MiskX, ButtonX: String): Integer;
begin
  Result:= InsertButton(ButtonList.Count, sCaption, CmdX, MenuX, ButtonX);
  FBarFile.AddButtonX(ButtonX, CmdX, ParamX, PathX, MenuX, MiskX);
end;

function TKASToolBar.InsertButton(InsertAt: Integer; sCaption, sCommand, sHint: String; Bitmap: TBitmap): Integer;
var
  ToolButton: TKASToolButton;
begin
  ToolButton:= TKASToolButton.Create(Self);
  ToolButton.ShowHint:= True;
  ToolButton.Hint:= sHint;
  ToolButton.Flat:= FFlat;
  ToolButton.Caption:= sCaption;
  AssignToolButtonProperties(ToolButton);

  ToolButton.Glyph.Assign(Bitmap);

  if FRadioToolBar then
    begin
      ToolButton.GroupIndex:= 1;
      ToolButton.AllowAllUp:= True;
    end;

  InsertButton(InsertAt, ToolButton);

  Result:= ToolButton.Tag;
end;

function TKASToolBar.InsertButton(InsertAt: Integer; sCaption, sCommand, sHint, sBitmap: String): Integer;
var
  Bitmap: TBitmap = nil;
begin
  if sHint = '-' then sBitmap:= sHint;  // To pass separator to FOnLoadButtonGlyph
  if Assigned(FOnLoadButtonGlyph) then
    Bitmap:= FOnLoadButtonGlyph(sBitmap, FGlyphSize, clBtnFace)
  else
    Bitmap:= LoadBtnIcon(sBitmap);

  Result:= InsertButton(InsertAt, sCaption, sCommand, sHint, Bitmap);

  if Assigned(Bitmap) then
    FreeAndNil(Bitmap);
end;

function TKASToolBar.InsertButtonX(InsertAt: Integer; sCaption, CmdX, ParamX, PathX, MenuX, MiskX: String; Bitmap: TBitmap): Integer;
begin
  Result:= InsertButton(InsertAt, sCaption, CmdX, MenuX, Bitmap);
  FBarFile.InsertButtonX(InsertAt, '', CmdX, ParamX, PathX, MenuX, MiskX);
end;

function TKASToolBar.InsertButtonX(InsertAt: Integer; sCaption, CmdX, ParamX, PathX, MenuX, MiskX, ButtonX: String): Integer;
begin
  Result:= InsertButton(InsertAt, sCaption, CmdX, MenuX, ButtonX);
  FBarFile.InsertButtonX(InsertAt, ButtonX, CmdX, ParamX, PathX, MenuX, MiskX);
end;

procedure TKASToolBar.RemoveButton(Index: Integer);
begin
  try
    TSpeedButton(ButtonList.Items[Index]).Visible:= False;
    TSpeedButton(ButtonList.Items[Index]).Free;
    ButtonList.Delete(Index);
    UpdateButtonsTags;
    FBarFile.RemoveButton(Index);
    Resize;
  finally
    Repaint;
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
  ToolBar: TKASToolBar;
begin
  if Assigned(Parent) and (Parent is TKASToolBar) then
  begin
    ToolBar := TKASToolBar(Parent);
    if ToolBar.ShowCaptions and ShowCaption and (Caption <> EmptyStr) then
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

{ TKASToolDivider }

procedure TKASToolDivider.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if Assigned(Parent) and (Parent is TKASToolBar) then
  begin
    PreferredWidth  := 3;
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
  DividerRect:= ClientRect;
  Details:= ThemeServices.GetElementDetails(ttbSeparatorNormal);
  if (DividerRect.Right - DividerRect.Left) > 3 then
    begin
      DividerRect.Left:= (DividerRect.Left + DividerRect.Right) div 2 - 1;
      DividerRect.Right:= DividerRect.Left + 3;
    end;
  ThemeServices.DrawElement(Canvas.GetUpdatedHandle([csBrushValid, csPenValid]), Details, DividerRect);
end;

end.


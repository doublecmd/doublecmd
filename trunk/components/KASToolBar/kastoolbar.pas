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
  TOnLoadButtonGlyph = function (sIconFileName: String; iIconSize: Integer; clBackColor: TColor): TBitmap of object;

  { TSpeedDivider }

  TSpeedDivider = class(TSpeedButton)
  protected
    procedure Paint; override;
  end;

  { TKASToolBar }

  TKASToolBar = class(TToolBar)
  private
    FUpdateCount: Integer;
    FGlyphSize: Integer;
    FRadioToolBar: Boolean;
    FShowDividerAsButton: Boolean;
    FFlat: Boolean;
    FBarFile: TBarClass;
    FOnToolButtonClick: TOnToolButtonClick;
    FOnLoadButtonGlyph: TOnLoadButtonGlyph;
    function GetButtonHeight: Integer;
    function GetButtonWidth: Integer;
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
    procedure UpdateButtonsTags;
  protected
    { Protected declarations }
    function CreateButton: TSpeedButton;
    function CreateDivider: TSpeedDivider;
    procedure InsertButton(InsertAt: Integer; ToolButton: TSpeedButton);
    procedure CalculatePreferredSize(var PreferredWidth,
                    PreferredHeight: Integer; WithThemeSpace: Boolean); override;
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
  published
    { Published declarations }
    property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
    property RadioToolBar: Boolean read FRadioToolBar write FRadioToolBar default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GlyphSize: Integer read FGlyphSize write SetGlyphSize;
    property ButtonHeight: Integer read GetButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 23;
    property ShowDividerAsButton: Boolean read FShowDividerAsButton write FShowDividerAsButton default False;

    property ChangePath: String read GetChangePath write SetChangePath;
    property EnvVar: String read GetEnvVar write SetEnvVar;
  end;


procedure Register;

implementation

uses
  GraphType, Themes;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASToolBar]);
end;

function TKASToolBar.CreateButton: TSpeedButton;
begin
  Result:= TSpeedButton.Create(Self);
  Result.Parent:= Self;

  Result.Height:= ButtonHeight;
  Result.ParentShowHint:= False;
  Result.ShowHint:= True;
end;

function TKASToolBar.CreateDivider: TSpeedDivider;
begin
  Result:= TSpeedDivider.Create(Self);
  Result.Parent:= Self;

  Result.ParentShowHint:= False;
  Result.Height:= ButtonHeight;
  Result.Width:= 3;
end;

procedure TKASToolBar.InsertButton(InsertAt: Integer; ToolButton: TSpeedButton);
begin
  if (InsertAt > 0) and (InsertAt = ButtonList.Count) then
    begin
      ToolButton.Left:= Buttons[InsertAt-1].Left + Buttons[InsertAt-1].Width;
      ToolButton.Top:= Buttons[InsertAt-1].Top + Buttons[InsertAt-1].Height;
    end
  else if (InsertAt > 0) and (InsertAt < ButtonList.Count) then
    begin
      ToolButton.Left:= Buttons[InsertAt-1].Left;
      ToolButton.Top:= Buttons[InsertAt-1].Top;
    end
  else
    begin
      ToolButton.Left:= BorderWidth;
      ToolButton.Top:= BorderWidth;
    end;
  ButtonList.Insert(InsertAt, ToolButton);
end;

procedure TKASToolBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  WrapButtons(Width, PreferredWidth, PreferredHeight, True);
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

function TKASToolBar.GetChangePath: String;
begin
  Result:= FBarFile.ChangePath;
end;

function TKASToolBar.GetButtonHeight: Integer;
begin
  Result:= inherited ButtonHeight;
end;

function TKASToolBar.GetButtonWidth: Integer;
begin
  Result:= inherited ButtonWidth;
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
  inherited Click;
  if Assigned(FOnToolButtonClick) then
     FOnToolButtonClick(Self, (Sender as TSpeedButton).Tag);
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
end;

procedure TKASToolBar.SaveToIniFile(IniFile: TIniFile);
begin
  FBarFile.SaveToIniFile(IniFile);
end;

procedure TKASToolBar.LoadFromFile(FileName: String);
var
  IniFile: TIniFile;
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
  IniFile: TIniFile;
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
  inherited EndUpdate;
end;

procedure TKASToolBar.SetButtonSize(NewButtonWidth, NewButtonHeight: Integer);
var
  CurControl: TControl;
  NewWidth: Integer;
  NewHeight: Integer;
  I: Integer;
  ChangeW, ChangeH: Boolean;
begin
  ChangeW := ButtonWidth <> NewButtonWidth;
  ChangeH := ButtonHeight <> NewButtonHeight;
  if not (ChangeW or ChangeH) then Exit;

  // set all childs to ButtonWidth ButtonHeight
  BeginUpdate;
  try
    // Change FButtonWidth and FButtonHeight, we called this procedure after
    // BeginUpdate, so only FButtonWidth and FButtonHeight will be changed
    // without real button size update
    inherited SetButtonSize(NewButtonWidth, NewButtonHeight);

    // After setting FButtonWidth and FButtonHeight we can exit
    // if real resizing of controls is not needed.
    // We're under influence of BeginUpdate, so it is at least 1.
    if FUpdateCount > 1 then Exit;
    if [csLoading, csDestroying] * ComponentState <> [] then Exit;

    for I:= ControlCount - 1 downto 0 do
    begin
      CurControl := Controls[I];
      NewWidth := CurControl.Width;
      NewHeight := CurControl.Height;

      // width
      if ChangeW
      and (ButtonWidth > 0)
      and not CurControl.AutoSize
      and (CurControl.Align in [alNone, alLeft, alRight])
      then NewWidth := ButtonWidth;

      // height
      // in horizontal toolbars the height is set by the toolbar independent of autosize
      if ChangeH
      and (ButtonHeight > 0)
      and ((Align in [alTop, alBottom]) or not CurControl.AutoSize)
      then NewHeight := ButtonHeight;

      CurControl.SetBounds(CurControl.Left, CurControl.Top, NewWidth, NewHeight);
    end;
  finally
    EndUpdate;
  end;
end;

function TKASToolBar.AddDivider: Integer;
var
  ToolDivider: TSpeedDivider;
begin
  BeginUpdate;

  ToolDivider:= CreateDivider;

  InsertButton(ButtonList.Count, ToolDivider);

  if Assigned(OnMouseUp) then
    ToolDivider.OnMouseUp:= OnMouseUp;

  EndUpdate;

  UpdateButtonsTags;

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
  ToolButton: TSpeedButton;
begin
  if InsertAt < 0 then
    InsertAt:= 0;
  if InsertAt > ButtonList.Count then
    InsertAt:= ButtonList.Count;

  BeginUpdate;

  ToolButton:= CreateButton;

  ToolButton.ShowHint:= True;
  ToolButton.Hint:= sHint;

  if Assigned(Bitmap) then
    ToolButton.Glyph.Assign(Bitmap);

  if ShowCaptions then
    begin
      ToolButton.Caption:= sCaption;
      ToolButton.Width:= ToolButton.Canvas.TextWidth(sCaption) + ToolButton.Glyph.Width + 16
    end
  else
    begin
      ToolButton.Caption:= EmptyStr;
      ToolButton.Width:= ButtonWidth;
    end;

  InsertButton(InsertAt, ToolButton);

  if Assigned(OnMouseUp) then
    ToolButton.OnMouseUp:= OnMouseUp;

  if FRadioToolBar then
    begin
      ToolButton.GroupIndex:= 1;
      ToolButton.AllowAllUp:= True;
    end;

  ToolButton.Flat:= FFlat;

  ToolButton.OnClick:= TNotifyEvent(@ToolButtonClick);

  EndUpdate;

  UpdateButtonsTags;

  // Recalculate positions of buttons if a new button was inserted in the middle.
  if InsertAt < ButtonCount - 1 then
    Resize;

  Result:= InsertAt;
end;

function TKASToolBar.InsertButton(InsertAt: Integer; sCaption, sCommand, sHint, sBitmap: String): Integer;
var
  Bitmap: TBitmap = nil;
begin
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

{ TSpeedDivider }

procedure TSpeedDivider.Paint;
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
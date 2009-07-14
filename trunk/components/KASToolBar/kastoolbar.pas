{
   Double Commander components
   -------------------------------------------------------------------------
   Toolbar panel class

   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)
   
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
  Graphics, Dialogs, ExtCtrls, Buttons, IniFiles, FileUtil,KASBarFiles;

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
    FGlyphSize: Integer;
    FOnToolButtonClick: TOnToolButtonClick;
    FOnLoadButtonGlyph: TOnLoadButtonGlyph;
    FRadioToolBar: Boolean;
    FShowDividerAsButton: Boolean;
    FFlat: Boolean;
    FBarFile: TBarClass;
    function GetChangePath: String;
    function GetEnvVar: String;
    function LoadBtnIcon(IconPath: String) : TBitMap;
    function GetButton(Index: Integer): TSpeedButton;
    function GetCommand(Index: Integer): String;
    procedure SetChangePath(const AValue: String);
    procedure SetCommand(Index: Integer; const AValue: String);
    procedure SetEnvVar(const AValue: String);
    procedure SetFlat(const AValue : Boolean);
    procedure ToolButtonClick(Sender: TObject);
    procedure UpdateButtonsTag;

  protected
    { Protected declarations }
    function CreateButton: TSpeedButton;
    function CreateDivider: TSpeedDivider;
    procedure InsertButton(InsertAt: Integer; ToolButton: TSpeedButton);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    function AddDivider: Integer;
    function AddButton(sCaption, sCommand, sHint: String; Bitmap: TBitmap): Integer;
    function AddButton(sCaption, Cmd, BtnHint, IconPath : String) : Integer;
    function AddButtonX(sCaption, CmdX, ParamX, PathX, MenuX, MiskX: String; Bitmap: TBitmap): Integer;
    function AddButtonX(sCaption, CmdX, ParamX, PathX, MenuX, MiskX, ButtonX: String): Integer;

    function InsertButton(InsertAt: Integer; sCaption, Cmd, BtnHint: String; Bitmap: TBitmap): Integer;
    function InsertButton(InsertAt: Integer; sCaption, Cmd, BtnHint, IconPath : String) : Integer;
    function InsertButtonX(InsertAt: Integer; sCaption, CmdX, ParamX, PathX, MenuX, MiskX: String; Bitmap: TBitmap): Integer;
    function InsertButtonX(InsertAt: Integer; sCaption, CmdX, ParamX, PathX, MenuX, MiskX, ButtonX: String): Integer;
    function GetButtonX(Index:integer; What:TInfor):string;

    procedure SetButtonX(Index:integer; What:Tinfor;Value: string);
    procedure LoadFromIniFile(IniFile : TIniFile);
    procedure SaveToIniFile(IniFile : TIniFile);
    procedure LoadFromFile(FileName : String);
    procedure SaveToFile(FileName : String);
    procedure RemoveButton(Index: Integer);
    procedure DeleteAllToolButtons;
    procedure UncheckAllButtons;

    property Buttons[Index: Integer]: TSpeedButton read GetButton;
    property Commands[Index: Integer]: String read GetCommand write SetCommand;
    property BarFile: TBarClass read FBarFile;
  published
    { Published declarations }
    property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
    property RadioToolBar : Boolean read FRadioToolBar write FRadioToolBar default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GlyphSize : Integer read FGlyphSize write FGlyphSize;
    property ShowDividerAsButton: Boolean read FShowDividerAsButton write FShowDividerAsButton default False;

    property ChangePath : String read GetChangePath write SetChangePath;
    property EnvVar : String read GetEnvVar write SetEnvVar;
  end;


procedure Register;

implementation

uses
  GraphType, Themes;

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

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASToolBar]);
end;

function TKASToolBar.GetButtonX(Index: integer; What: TInfor): string;
begin
  Result:= FBarFile.GetButtonX(Index, What);
end;

procedure TKASToolBar.SetButtonX(Index: integer; What: Tinfor; Value: string);
begin
  FBarFile.SetButtonX(Index, What, Value);
end;

function TKASToolBar.LoadBtnIcon(IconPath: String): TBitMap;
var
  PNG : TPortableNetworkGraphic;
begin
  Result := nil;
  if IconPath <> '' then
  if FileExists(IconPath) then
   begin
   if CompareFileExt(IconPath, 'png', false) = 0 then
      begin
        PNG := TPortableNetworkGraphic.Create;
        try
          PNG.LoadFromFile(IconPath);
          Result := Graphics.TBitmap.Create;
          Result.Assign(PNG);
        finally
          FreeAndNil(PNG);
        end;
      end
   else
      begin
         Result := TBitMap.Create;
         Result.LoadFromFile(IconPath);
      end;
   end;
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

{procedure TKASToolBar.SetIconPath(Index: Integer; const AValue: String);
var
  PNG : TPortableNetworkGraphic;
begin
//  FIconList[Index] := AValue;
 SetButtonX(Index,ButtonX,AValue);
  with TSpeedButton(FButtonsList.Items[Index]) do
  if Assigned(FOnLoadButtonGlyph) then
    Glyph := FOnLoadButtonGlyph(AValue, FIconSize, Color)
  else
    Glyph := LoadBtnIcon(AValue);
end;
}
procedure TKASToolBar.SetFlat(const AValue: Boolean);
var
  I :Integer;
begin
  FFlat := AValue;
  for I := 0 to ButtonList.Count - 1 do
    TSpeedButton(ButtonList.Items[I]).Flat := FFlat;
end;

procedure TKASToolBar.ToolButtonClick(Sender: TObject);
begin
  inherited Click;
  if Assigned(FOnToolButtonClick) then
     FOnToolButtonClick(Self, (Sender as TSpeedButton).Tag);
end;

procedure TKASToolBar.UpdateButtonsTag;
var
  I :Integer;
begin
  for I := 0 to ButtonList.Count - 1 do
    TSpeedButton(ButtonList.Items[I]).Tag := I;
end;

procedure TKASToolBar.DeleteAllToolButtons;
var
  BtnCount,
  I: Integer;
begin
  BeginUpdate;
      
  BtnCount := ButtonList.Count - 1;
  for I := 0 to BtnCount do
    begin
      TSpeedButton(ButtonList.Items[0]).Free;
      ButtonList.Delete(0);
    end;
  FBarFile.DeleteAllButtons;
  EndUpdate;
end;

function TKASToolBar.GetCommand(Index: Integer): String;
begin
 Result := GetButtonX(Index,CmdX);
end;

{function TKASToolBar.GetIconPath(Index: Integer): String;
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
end;

destructor TKASToolBar.Destroy;
var
  I: Integer;
begin
  for I := 0 to ButtonList.Count - 1 do
    if TControl(ButtonList[I]) is TSpeedButton then
      TSpeedButton(ButtonList.Items[I]).Free;

  if Assigned(FBarFile) then
    FBarFile.Free;

  inherited Destroy;
end;

procedure TKASToolBar.LoadFromIniFile(IniFile : TIniFile);
var  
  I : Integer;
  sMenu: String;
begin
  DeleteAllToolButtons;
  FBarFile.LoadFromIniFile(IniFile);

  
  for I := 0 to FBarFile.ButtonCount - 1 do
    begin
      sMenu:= FBarFile.GetButtonX(I, MenuX);
      if (sMenu = '-') and not FShowDividerAsButton then
        AddDivider
      else
        AddButton('', FBarFile.GetButtonX(I, CmdX),
                  sMenu,
                  FBarFile.GetButtonX(I, ButtonX));


    end;
end;

procedure TKASToolBar.SaveToIniFile(IniFile : TIniFile);
begin
  FBarFile.SaveToIniFile(IniFile);
end;

procedure TKASToolBar.LoadFromFile(FileName: String);
var
  IniFile : Tinifile;
begin
  IniFile:= TIniFile.Create(FileName);
  LoadFromIniFile(IniFile);
  IniFile.Free;
end;

procedure TKASToolBar.SaveToFile(FileName: String);
var
  IniFile : Tinifile;
begin
  //For cleaning. Without this saved file will contain removed buttons
  if FileExists(FileName) then
    DeleteFile(FileName);

  IniFile := TInifile.Create(FileName);
  SaveToIniFile(IniFile);
  IniFile.Free;
end;

function TKASToolBar.AddDivider: Integer;
var
  ToolDivider: TSpeedDivider;
begin
  // lock on resize handler
  BeginUpdate;

  ToolDivider:= CreateDivider;

  InsertButton(ButtonList.Count, ToolDivider);

  if Assigned(OnMouseUp) then
    ToolDivider.OnMouseUp:= OnMouseUp;

  // unlock on resize handler
  EndUpdate;

  Result:= ToolDivider.Tag;
end;

function TKASToolBar.AddButton(sCaption, sCommand, sHint: String; Bitmap: TBitmap): Integer;
begin
  Result:= InsertButton(ButtonList.Count, sCaption, sCommand, sHint, Bitmap);
end;

function TKASToolBar.AddButton(sCaption, Cmd, BtnHint, IconPath : String) : Integer;
begin
  Result:= InsertButton(ButtonList.Count, sCaption, Cmd, BtnHint, IconPath);
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

function TKASToolBar.InsertButton(InsertAt: Integer; sCaption, Cmd, BtnHint: String; Bitmap: TBitmap): Integer;
var
  ToolButton: TSpeedButton;
begin
  if InsertAt < 0 then
    InsertAt := 0;
  if InsertAt > ButtonList.Count then
    InsertAt := ButtonList.Count;

  // lock on resize handler
  BeginUpdate;

  ToolButton:= CreateButton;

  ToolButton.ShowHint:= True;
  ToolButton.Hint:= BtnHint;

  if ShowCaptions then
    begin
      ToolButton.Caption:= sCaption;
      ToolButton.Width:= ToolButton.Canvas.TextWidth(sCaption) + ToolButton.Glyph.Width + 32
    end
  else
    begin
      ToolButton.Caption:= EmptyStr;
      ToolButton.Width:= ButtonWidth;
    end;

  InsertButton(InsertAt, ToolButton);

  if Assigned(OnMouseUp) then
    ToolButton.OnMouseUp := OnMouseUp;

  if FRadioToolBar then
  begin
    ToolButton.GroupIndex:= 1;
    ToolButton.AllowAllUp:= True;
  end;

  ToolButton.Flat:= FFlat;

  if Assigned(Bitmap) then
    ToolButton.Glyph.Assign(Bitmap);

  ToolButton.OnClick:=TNotifyEvent(@ToolButtonClick);


  // unlock on resize handler
  EndUpdate;

  UpdateButtonsTag;

  // Recalculate positions of buttons if a new button was inserted in the middle.
  if InsertAt < ButtonCount - 1 then
  begin
    Resize;
  end;

  Result := InsertAt;
end;

function TKASToolBar.InsertButton(InsertAt: Integer; sCaption, Cmd, BtnHint, IconPath : String) : Integer;
var
  Bitmap: TBitmap = nil;
begin
  if Assigned(FOnLoadButtonGlyph) then
    Bitmap := FOnLoadButtonGlyph(IconPath, FGlyphSize, clBtnFace)
  else
    Bitmap := LoadBtnIcon(IconPath);

  Result:= InsertButton(InsertAt, sCaption, Cmd, BtnHint, Bitmap);

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
    TSpeedButton(ButtonList.Items[Index]).Visible := False;
    TSpeedButton(ButtonList.Items[Index]).Free;
    ButtonList.Delete(Index);
    UpdateButtonsTag;
    //---------------------
    FBarFile.RemoveButton(Index);
    //---------------------
    Resize;

  finally
    Repaint;
  end;
end;

procedure TKASToolBar.UncheckAllButtons;
var
  i : Integer;
begin
  for i := 0 to ButtonCount - 1 do
    Buttons[i].Down := False;
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
  ThemeServices.DrawElement(Canvas.GetUpdatedHandle([csBrushValid, csPenValid]),
                            Details, DividerRect);
end;

end.

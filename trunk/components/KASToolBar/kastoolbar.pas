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



unit KAStoolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ComCtrls,
  Graphics, Dialogs, ExtCtrls, Buttons, IniFiles, FileUtil,KASBarFiles;

type

  TOnToolButtonClick = procedure (Sender: TObject; NumberOfButton : Integer) of object;
  TOnLoadButtonGlyph = function (sIconFileName : String; iIconSize : Integer; clBackColor : TColor) : TBitmap of object;

  { TSpeedDivider }

  TSpeedDivider = class(TSpeedButton)
  protected
    procedure Paint; override;
  end;

  { TKAStoolBar }

  TKAStoolBar = class(TToolBar)
  private
    FIconSize : Integer;
    FOnToolButtonClick : TOnToolButtonClick;
    FOnLoadButtonGlyph : TOnLoadButtonGlyph;
    FCheckToolButton : Boolean;
    FDriveToolBar: Boolean;
    FDividerAsButton: Boolean;
    FFlat: Boolean;
    FChangePath : String;
    FEnvVar : String;
    FBarFile: TBarClass;
    CurrentBar:string;
    //---------------------
    function LoadBtnIcon(IconPath : String) : TBitMap;
    function GetButton(Index: Integer): TSpeedButton;
    function GetCommand(Index: Integer): String;
    procedure SetButton(Index : Integer; Value : TSpeedButton);
    procedure SetCommand(Index: Integer; const AValue: String);
    procedure SetFlat(const AValue : Boolean);
    procedure ToolButtonClick(Sender: TObject);
    procedure UpdateButtonsTag;

  protected
    { Protected declarations }
    function GetCmdDirFromEnvVar(sPath: String): String;
    function SetCmdDirAsEnvVar(sPath: String): String;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    function AddDivider: Integer;
    function AddX(ButtonX, CmdX, ParamX, PathX, MenuX : String): Integer;
    function AddButton(sCaption, Cmd, BtnHint, IconPath : String) : Integer;
    function InsertX(InsertAt: Integer; ButtonX, CmdX, ParamX, PathX, MenuX : String): Integer;
    function InsertButton(InsertAt: Integer; sCaption, Cmd, BtnHint, IconPath : String) : Integer;
    function GetButtonX(Index:integer; What:TInfor):string;

    procedure SetButtonX(Index:integer; What:Tinfor;Value: string);
    procedure LoadFromIniFile(IniFile : TIniFile);
    procedure SaveToIniFile(IniFile : TIniFile);
    procedure LoadFromFile(FileName : String);
    procedure SaveToFile(FileName : String);
    procedure RemoveButton(Index: Integer);
    procedure DeleteAllToolButtons;
    procedure UncheckAllButtons;

    property Buttons[Index: Integer]: TSpeedButton read GetButton write SetButton;
    property Commands[Index: Integer]: String read GetCommand write SetCommand;

  published
    { Published declarations }
    property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
    property CheckToolButton : Boolean read FCheckToolButton write FCheckToolButton default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property DriveToolBar : Boolean read FDriveToolBar write FDriveToolBar default False;
    property ButtonGlyphSize : Integer read FIconSize write FIconSize;
    property ShowDividerAsButton: Boolean read FDividerAsButton write FDividerAsButton default False;

    property ChangePath : String read FChangePath write FChangePath;
    property EnvVar : String read FEnvVar write FEnvVar;
  end;


procedure Register;

implementation

uses
  GraphType, Themes;

function TKAStoolBar.GetCmdDirFromEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(FEnvVar, sPath) <> 0 then
    Result := StringReplace(sPath, FEnvVar, ExcludeTrailingPathDelimiter(FChangePath), [rfIgnoreCase])
  else
    Result := sPath;
end;

function TKAStoolBar.SetCmdDirAsEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(FChangePath, sPath) <> 0 then
    Result := StringReplace(sPath, ExcludeTrailingPathDelimiter(FChangePath), FEnvVar, [rfIgnoreCase])
  else
    Result := sPath;
end;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKAStoolBar]);
end;

function TKAStoolBar.GetButtonX(Index: integer; What: TInfor): string;
begin
  Result:= FBarFile.GetButtonX(Index, What);
end;

procedure TKAStoolBar.SetButtonX(Index: integer; What: Tinfor; Value: string);
begin
  FBarFile.SetButtonX(Index, What, Value);
end;

function TKAStoolBar.LoadBtnIcon(IconPath: String): TBitMap;
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

function TKAStoolBar.GetButton(Index: Integer): TSpeedButton;
begin
  Result := TSpeedButton(ButtonList.Items[Index]);
end;

procedure TKAStoolBar.SetButton(Index : Integer; Value : TSpeedButton);
begin
 ButtonList.Items[Index] := Value;
end;

procedure TKAStoolBar.SetCommand(Index: Integer; const AValue: String);
begin
SetButtonX(Index,CmdX,AValue);
end;

{procedure TKAStoolBar.SetIconPath(Index: Integer; const AValue: String);
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
procedure TKAStoolBar.SetFlat(const AValue: Boolean);
var
  I :Integer;
begin
  FFlat := AValue;
  for I := 0 to ButtonList.Count - 1 do
    TSpeedButton(ButtonList.Items[I]).Flat := FFlat;
end;

procedure TKAStoolBar.ToolButtonClick(Sender: TObject);
begin
  inherited Click;
  if Assigned(FOnToolButtonClick) then
     FOnToolButtonClick(Self, (Sender as TSpeedButton).Tag);
end;

procedure TKAStoolBar.UpdateButtonsTag;
var
  I :Integer;
begin
  for I := 0 to ButtonList.Count - 1 do
    TSpeedButton(ButtonList.Items[I]).Tag := I;
end;

procedure TKAStoolBar.DeleteAllToolButtons;
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

function TKAStoolBar.GetCommand(Index: Integer): String;
begin
 Result := GetButtonX(Index,CmdX);
end;

{function TKAStoolBar.GetIconPath(Index: Integer): String;
begin
//  Result := FIconList[Index];
 Result := GetButtonX(Index,ButtonX);
end;
}
constructor TKAStoolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBarFile := TBarClass.Create;
  FIconSize := 16; // default
end;

destructor TKAStoolBar.Destroy;
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

procedure TKAStoolBar.LoadFromIniFile(IniFile : TIniFile);
var  
  I : Integer;
  sMenu: String;
begin
  DeleteAllToolButtons;
  FBarFile.LoadFromIniFile(IniFile);

  
  for I := 0 to FBarFile.ButtonCount - 1 do
    begin
      sMenu:= FBarFile.GetButtonX(I, MenuX);
      if (sMenu = '-') and not FDividerAsButton then
        AddDivider
      else
        AddButton('', FBarFile.GetButtonX(I, CmdX),
                  sMenu,
                  FBarFile.GetButtonX(I, ButtonX));


    end;
end;

procedure TKAStoolBar.SaveToIniFile(IniFile : TIniFile);
begin
  FBarFile.SaveToIniFile(IniFile);
end;

procedure TKAStoolBar.LoadFromFile(FileName: String);
var
  IniFile : Tinifile;
begin
  IniFile:= TIniFile.Create(FileName);
  CurrentBar:= FileName;
  LoadFromIniFile(IniFile);
  IniFile.Free;
end;

procedure TKAStoolBar.SaveToFile(FileName: String);
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

function TKAStoolBar.AddDivider: Integer;
var
  ToolDivider: TSpeedDivider;
begin
  // lock on resize handler
  BeginUpdate;

  ToolDivider:= TSpeedDivider.Create(Self);
  ToolDivider.Parent:= Self;
  ToolDivider.Visible:= True;
  ToolDivider.ParentShowHint:= False;
  ToolDivider.Height:= ButtonHeight;
  ToolDivider.Width:= 3;


  if ButtonList.Count > 0 then
    begin
      ToolDivider.Left:= Buttons[ButtonList.Count-1].Left + Buttons[ButtonList.Count-1].Width;
      ToolDivider.Top:= Buttons[ButtonList.Count-1].Top + Buttons[ButtonList.Count-1].Height;
    end
  else
    begin
      ToolDivider.Left:= BorderWidth;
      ToolDivider.Top:= BorderWidth;
    end;
  //WriteLN('ToolDivider.Left == ' + IntToStr(ToolButton.Left));

  if Assigned(OnMouseUp) then
    ToolDivider.OnMouseUp:= OnMouseUp;



  ToolDivider.Tag:= ButtonList.Add(ToolDivider);

  // unlock on resize handler
  EndUpdate;

  Result:= ToolDivider.Tag;
end;

function TKAStoolBar.AddX(ButtonX, CmdX, ParamX, PathX, MenuX : String) : Integer;
begin
  Result := FBarFile.AddButtonX(ButtonX, CmdX, ParamX, PathX, MenuX, '');
end;

function TKAStoolBar.AddButton(sCaption, Cmd, BtnHint, IconPath : String) : Integer;
begin
  Result := InsertButton(ButtonList.Count, sCaption, Cmd, BtnHint, IconPath);
end;

function TKAStoolBar.InsertX(InsertAt: Integer; ButtonX, CmdX, ParamX, PathX, MenuX : String): Integer;
begin
  Result:= FBarFile.InsertButtonX(InsertAt, ButtonX, CmdX, ParamX, PathX, MenuX, '');
end;

function TKAStoolBar.InsertButton(InsertAt: Integer; sCaption, Cmd, BtnHint, IconPath : String) : Integer;
var
  ToolButton: TSpeedButton;
  Bitmap: TBitmap = nil;
begin
  if InsertAt < 0 then
    InsertAt := 0;
  if InsertAt > ButtonList.Count then
    InsertAt := ButtonList.Count;

  // lock on resize handler
  BeginUpdate;

  ToolButton:= TSpeedButton.Create(Self);



  //Include(ToolButton.ComponentStyle, csSubComponent);
  ToolButton.Parent := Self;
  ToolButton.Visible := True;

  ToolButton.Height := ButtonHeight;
  ToolButton.ParentShowHint := False;
  ToolButton.Caption := sCaption;
  ToolButton.ShowHint := True;
  ToolButton.Hint := BtnHint;

  if FDriveToolBar then
    begin
      ToolButton.Width := ToolButton.Canvas.TextWidth(sCaption) + ToolButton.Glyph.Width + 32;
    end
  else
    ToolButton.Width := ButtonWidth;


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
  //WriteLN('ToolButton.Left == ' + IntToStr(ToolButton.Left));

  if Assigned(OnMouseUp) then
    ToolButton.OnMouseUp := OnMouseUp;

  if FCheckToolButton then
  begin
    ToolButton.GroupIndex := 1;
    ToolButton.AllowAllUp := True;
  end;

  ToolButton.Flat := FFlat;

  if Assigned(FOnLoadButtonGlyph) then
    Bitmap := FOnLoadButtonGlyph(IconPath, FIconSize, ToolButton.Color)
  else
    Bitmap := LoadBtnIcon(IconPath);

  ToolButton.Glyph := Bitmap;

  if Assigned(Bitmap) then
    FreeAndNil(Bitmap);

  ToolButton.OnClick:=TNotifyEvent(@ToolButtonClick);



  // this is temporarly
  if FDriveToolBar then
    InsertX(InsertAt, sCaption,Cmd, '', '', '');

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

procedure TKAStoolBar.RemoveButton(Index: Integer);
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

procedure TKAStoolBar.UncheckAllButtons;
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

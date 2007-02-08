{
   File name: kastoolbar.pas

   Author:    Koblov Alexander (Alexx2000@mail.ru)

   Toolbar panel

   Copyright (C) 2006
   
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
  Classes, SysUtils, LResources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Buttons, IniFiles, FileUtil;

type

  TOnToolButtonClick = procedure (NumberOfButton : Integer) of object;

  { TKAStoolBar }

  TKAStoolBar = class(TPanel)
  private
    FButtonsList: TList;
    FCmdList,
    FIconList : TStringList;
    FPositionX : Integer;
    FPositionY : Integer;
    FMaxBtnCount : Integer;
    FLineBtnCount : Integer;
    FButtonSize : Integer;
    FNeedMore : Boolean;
    FOnToolButtonClick : TOnToolButtonClick;
    FTotalBevelWidth : Integer;
    FCheckToolButton : Boolean;
    FFlatButtons: Boolean;
    FDiskPanel: Boolean;
    function LoadBtnIcon(IconPath : String) : TBitMap;
    function GetButton(Index: Integer): TSpeedButton;
    function GetButtonCount: Integer;
    function GetCommand(Index: Integer): String;
    function GetIconPath(Index: Integer): String;
    procedure SetButton(Index : Integer; Value : TSpeedButton);
    procedure SetCommand(Index: Integer; const AValue: String);
    procedure SetIconPath(Index: Integer; const AValue: String);
    procedure ToolButtonClick(Sender: TObject);
    procedure UpdateButtonsTag;

  protected
    { Protected declarations }
  public
     constructor Create(TheOwner: TComponent); override;
     destructor Destroy; override;
     procedure CreateWnd; override;
     procedure LoadFromFile(FileName : String);
     procedure SaveToFile(FileName : String);
     function AddButton(Cmd, BtnHint, IconPath : String) : Integer;
     procedure RemoveButton(Index: Integer);
     procedure DeleteAllToolButtons;
     property ButtonCount: Integer read GetButtonCount;
     property Buttons[Index: Integer]: TSpeedButton read GetButton write SetButton;
     property Commands[Index: Integer]: String read GetCommand write SetCommand;
     property Icons[Index: Integer]: String read GetIconPath write SetIconPath;
     property ButtonList: TList read FButtonsList;

  published
    { Published declarations }
      property OnToolButtonClick: TOnToolButtonClick read FOnToolButtonClick write FOnToolButtonClick;
      property CheckToolButton : Boolean read FCheckToolButton write FCheckToolButton default False;
      property FlatButtons : Boolean read FFlatButtons write FFlatButtons default False;
      property IsDiskPanel : Boolean read FDiskPanel write FDiskPanel default False;
  end;


procedure Register;

implementation

uses GraphType;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKAStoolBar]);
end;

function TKAStoolBar.LoadBtnIcon(IconPath: String): TBitMap;
var
PNG : TPortableNetworkGraphic;
begin
  if IconPath <> '' then
  if FileExists(IconPath) then
   begin
   if CompareFileExt(IconPath, 'png', false) = 0 then
      begin
      PNG := TPortableNetworkGraphic.Create;
      PNG.LoadFromFile(IconPath);
      Result := TBitMap(PNG);
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
  Result := TSpeedButton(FButtonsList.Items[Index]);
end;

procedure TKAStoolBar.SetButton(Index : Integer; Value : TSpeedButton);
begin
 TSpeedButton(FButtonsList.Items[Index]) := Value;
end;

procedure TKAStoolBar.SetCommand(Index: Integer; const AValue: String);
begin
  FCmdList[Index] := AValue;
end;

procedure TKAStoolBar.SetIconPath(Index: Integer; const AValue: String);
var
PNG : TPortableNetworkGraphic;
begin
  FIconList[Index] := AValue;
  if FileExists(AValue) then
  TSpeedButton(FButtonsList.Items[Index]).Glyph := LoadBtnIcon(AValue)
  else
  ShowMessage('File "' + AValue + '" not found!' );
end;

procedure TKAStoolBar.ToolButtonClick(Sender: TObject);
begin
  inherited Click;
  if Assigned(FOnToolButtonClick) then
     FOnToolButtonClick((Sender as TSpeedButton).Tag);
end;

procedure TKAStoolBar.UpdateButtonsTag;
var
I :Integer;
begin
for I := 0 to FButtonsList.Count - 1 do
TSpeedButton(FButtonsList.Items[I]).Tag := I;
end;

procedure TKAStoolBar.DeleteAllToolButtons;
var
  BtnCount,
  I: Integer;
begin
  BtnCount := FButtonsList.Count - 1;
  for I := 0 to BtnCount do
  begin

  TSpeedButton(FButtonsList.Items[0]).Free;
  FButtonsList.Delete(0);

      
  FCmdList.Delete(0);
  FIconList.Delete(0);
  end;
Height := FButtonSize + FTotalBevelWidth * 2;
FLineBtnCount := 0;
FNeedMore := False;
end;

function TKAStoolBar.GetButtonCount: Integer;
begin
  Result := FButtonsList.Count;
end;

function TKAStoolBar.GetCommand(Index: Integer): String;
begin
  Result := FCmdList[Index];
end;

function TKAStoolBar.GetIconPath(Index: Integer): String;
begin
  Result := FIconList[Index];
end;

constructor TKAStoolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButtonsList := TList.Create;
  FCmdList := TStringList.Create;
  FIconList := TStringList.Create;
  FNeedMore := False;
end;

destructor TKAStoolBar.Destroy;
var
  I: Integer;
begin
  for I := 0 to FButtonsList.Count - 1 do
    if TControl(FButtonsList[I]) is TSpeedButton then
      TSpeedButton(FButtonsList.Items[I]).Free;

  FreeAndNil(FButtonsList);
  FreeAndNil(FCmdList);
  FreeAndNil(FIconList);
  inherited Destroy;
end;

procedure TKAStoolBar.CreateWnd;
begin

  inherited CreateWnd;
  Caption := '';
  if (BevelInner <> bvNone) and (BevelOuter <> bvNone) then
  FTotalBevelWidth := BevelWidth * 2
  else
  FTotalBevelWidth := BevelWidth;
  
  FButtonSize := Height - FTotalBevelWidth * 2;
  //writeln('FButtonSize = ' + IntToStr(FButtonSize));
  if Width < Height then
     Width := Height;


  FMaxBtnCount := (Width - FTotalBevelWidth * 2) div FButtonSize;
  if not FDiskPanel then
  Width := (FButtonSize * FMaxBtnCount) + FTotalBevelWidth * 2;
  
  FPositionX := FTotalBevelWidth;
  FPositionY := FTotalBevelWidth;
end;

procedure TKAStoolBar.LoadFromFile(FileName: String);
var
IniFile : Tinifile;
BtnCount, I : Integer;
begin
DeleteAllToolButtons;
FPositionX := FTotalBevelWidth;
FPositionY := FTotalBevelWidth;
FMaxBtnCount := Width div FButtonSize;
IniFile := Tinifile.Create(FileName);
BtnCount := IniFile.ReadInteger('Buttonbar', 'Buttoncount', 0);

for I := 1 to BtnCount do
AddButton(IniFile.ReadString('Buttonbar', 'cmd' + IntToStr(I), ''),
          IniFile.ReadString('Buttonbar', 'menu' + IntToStr(I), ''),
          IniFile.ReadString('Buttonbar', 'button' + IntToStr(I), ''));
IniFile.Free;
end;

procedure TKAStoolBar.SaveToFile(FileName: String);
var
IniFile : Tinifile;
I : Integer;
begin
IniFile := Tinifile.Create(FileName);
IniFile.WriteInteger('Buttonbar', 'Buttoncount', FButtonsList.Count);

for I := 0 to FButtonsList.Count - 1 do
    begin
    IniFile.WriteString('Buttonbar', 'button' + IntToStr(I + 1), FIconList[I]);
    IniFile.WriteString('Buttonbar', 'cmd' + IntToStr(I + 1), FCmdList[I]);
    IniFile.WriteString('Buttonbar', 'menu' + IntToStr(I + 1), TSpeedButton(FButtonsList.Items[I]).Hint);
    end;
IniFile.Free;
end;

function TKAStoolBar.AddButton(Cmd, BtnHint, IconPath : String) : Integer;
var
ToolButton: TSpeedButton;

begin
ToolButton:= TSpeedButton.Create(Self);
Include(ToolButton.ComponentStyle, csSubComponent);
ToolButton.Parent:=Self;
ToolButton.Visible := True;
ToolButton.Left:=FPositionX;
ToolButton.Top := FPositionY;
ToolButton.Height := FButtonSize;
ToolButton.ParentShowHint := False;
ToolButton.ShowHint := True;
ToolButton.Hint := BtnHint;

if Assigned(OnMouseDown) then
   ToolButton.OnMouseDown := OnMouseDown;

if FCheckToolButton then
   ToolButton.GroupIndex := 1;

ToolButton.Flat := FFlatButtons;

if FileExists(IconPath) then
ToolButton.Glyph := LoadBtnIcon(IconPath);

if FDiskPanel then
   ToolButton.Width := Self.Canvas.TextWidth(BtnHint) + ToolButton.Glyph.Width + 24
else
   ToolButton.Width := FButtonSize;

ToolButton.OnClick:=TNotifyEvent(@ToolButtonClick);

FPositionX:= FPositionX + ToolButton.Width;

ToolButton.Tag := FButtonsList.Add(ToolButton);
FCmdList.Add(Cmd);
FIconList.Add(IconPath);
Inc(FLineBtnCount);

if FNeedMore then
   begin
   Height := Height + FButtonSize;
   FNeedMore := False;
   end;
   
if FMaxBtnCount <= FLineBtnCount then
   begin
   FLineBtnCount := 0;
   FMaxBtnCount := Width div ToolButton.Width;
   FPositionY:= FPositionY + ToolButton.Height;
   FPositionX := FTotalBevelWidth;
   FNeedMore := True;
   end;
Result := ToolButton.Tag;
end;

procedure TKAStoolBar.RemoveButton(Index: Integer);
var
I, OldLeft, PrevLeft,
OldTop, PrevTop : integer;
begin
try
 TSpeedButton(FButtonsList.Items[Index]).Visible := false;
 FPositionX := FPositionX - TSpeedButton(FButtonsList.Items[Index]).Width;
 OldLeft := TSpeedButton(FButtonsList.Items[Index]).Left;
 OldTop := TSpeedButton(FButtonsList.Items[Index]).Top;
 TSpeedButton(FButtonsList.Items[Index]).Free;
 FButtonsList.Delete(Index);
 UpdateButtonsTag;
 FCmdList.Delete(Index);
 Dec(FLineBtnCount);

 if (FLineBtnCount = 0) and (FButtonsList.Count <> 0) then
   begin
   Height := Height - FButtonSize;
   FNeedMore := True;
   end
   else
   FNeedMore := False;

 if FLineBtnCount < 0 then
    begin
    FLineBtnCount := FMaxBtnCount - 1;
    FPositionX:= FTotalBevelWidth + FLineBtnCount * TSpeedButton(FButtonsList.Items[Index]).Width;
    FPositionY:= FPositionY - TSpeedButton(FButtonsList.Items[Index]).Height;
    end;
finally
 for I:= Index to FButtonsList.Count-1 do
  begin
  PrevLeft := TSpeedButton(FButtonsList.Items[i]).Left;
  PrevTop := TSpeedButton(FButtonsList.Items[i]).Top;
  TSpeedButton(FButtonsList.Items[i]).Left:= OldLeft;
  TSpeedButton(FButtonsList.Items[i]).Top:= OldTop;
  OldLeft := PrevLeft;
  OldTop := PrevTop;
  end;
 Repaint;
 end;

end;

end.

{
   File name: kasbarmenu.pas

   KASBarMenu  Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Popup menu with *.bar's buttons as MenuItems

   Based on KASToolBar functions
   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)


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


unit KASBarMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, KASBarFiles, FileUtil, IniFiles;
  
type

TOnLoadButtonGlyph   = function (sIconFileName : String; iIconSize : Integer; clBackColor : TColor) : TBitmap of object;
TOnMenuButtonClick = procedure (Sender: TObject; NumberOfButton : Integer) of object;

  { TKASBarMenu }

  TKASBarMenu = class(TPopupMenu)
  private
   FBar:TBarClass;
   FOnLoadButtonGlyph : TOnLoadButtonGlyph;
   FOnMenuButtonClick : TOnMenuButtonClick;

  //------------------------------------------------------
   procedure MenuOnClick(Sender: TObject);
   function LoadBtnIcon(IconPath: String): TBitMap;
  //------------------------------------------------------

  protected
  public
   constructor Create(TheOwner: TComponent); override;
   destructor Destroy; override;
   //---------------------

   procedure Clear;
   procedure LoadFromStringList(List: TStringList);
   procedure LoadFromIniFile(IniFile : TIniFile);
   procedure SaveToIniFile(IniFile : TIniFile);
   procedure LoadBarFile(FileName:string);
   procedure SaveToFile(FileName : String);
   
   procedure MakeMenu;
  //------------------------------------------------------

  published
    property BarFile: TBarClass read FBar write FBar;
    property OnLoadButtonGlyph : TOnLoadButtonGlyph read FOnLoadButtonGlyph write FOnLoadButtonGlyph;
    property OnMenuButtonClick: TOnMenuButtonClick read FOnMenuButtonClick write FOnMenuButtonClick;
  //------------------------------------------------------
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASBarMenu]);
end;

{ TKASBarMenu }

function TKASBarMenu.LoadBtnIcon(IconPath: String): TBitMap;
var
  PNG : TPortableNetworkGraphic;
begin
  Result := Graphics.TBitmap.Create;
  if (IconPath <> '') and FileExists(IconPath) then
   begin
     if CompareFileExt(IconPath, 'png', false) = 0 then
        begin
          PNG := TPortableNetworkGraphic.Create;
          try
            PNG.LoadFromFile(IconPath);
            Result.Assign(PNG);
          finally
            FreeAndNil(PNG);
          end;
        end
     else
        begin
          Result.LoadFromFile(IconPath);
        end;
   end;
end;

procedure TKASBarMenu.MenuOnClick(Sender: TObject);
begin
  if Assigned(FOnMenuButtonClick) then
     FOnMenuButtonClick(Self, (Sender as TMenuItem).Tag);
end;

constructor TKASBarMenu.Create(TheOwner: TComponent);
begin
  FBar:=TBarClass.Create;
  inherited Create(TheOwner);
end;

destructor TKASBarMenu.Destroy;
begin
  FBar.DeleteAllButtons;
  FreeAndNil(FBar);
  inherited Destroy;
end;

procedure TKASBarMenu.Clear;
begin
  FBar.DeleteAllButtons;
end;

procedure TKASBarMenu.MakeMenu;
var
  I:Integer;
  Item:TMenuItem;
  BitmapTmp: TBitmap;
begin
  For I:=0 to Fbar.ButtonCount-1 do
    begin
      Item:=TMenuItem.Create(Self);
      Item.Caption:=Fbar.GetButtonX(I,MenuX);
      //------------------------------------------------------
      if Assigned(FOnLoadButtonGlyph) then
        BitmapTmp := FOnLoadButtonGlyph(FBar.GetButtonX(I,ButtonX), 16, clFuchsia)
      else
        BitmapTmp := LoadBtnIcon(FBar.GetButtonX(I,ButtonX));

      Item.Bitmap := BitmapTmp;
      if Assigned(BitmapTmp) then
        FreeAndNil(BitmapTmp);
      //------------------------------------------------------
      Item.Tag:=I;
      Item.OnClick:=TNotifyEvent(@MenuOnClick);
      Self.Items.Insert(I,Item);
    end;
end;

procedure TKASBarMenu.LoadBarFile(FileName: string);
begin
  FBar.DeleteAllButtons;
  Self.Items.Clear;
  FBar.LoadFromFile(FileName);
  MakeMenu;
end;

procedure TKASBarMenu.LoadFromStringList(List: TStringList);
begin
  FBar.DeleteAllButtons;
  Self.Items.Clear;
  FBar.LoadFromStringList(List);
  MakeMenu;
end;

procedure TKASBarMenu.LoadFromIniFile(IniFile: TIniFile);
begin
  FBar.DeleteAllButtons;
  Self.Items.Clear;
  FBar.LoadFromIniFile(IniFile);
  MakeMenu;
end;

procedure TKASBarMenu.SaveToIniFile(IniFile: TIniFile);
begin
  FBar.SaveToIniFile(IniFile);
end;

procedure TKASBarMenu.SaveToFile(FileName: String);
begin
  FBar.SaveToFile(FileName);
end;

end.

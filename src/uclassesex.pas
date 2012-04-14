{
   Double commander
   -------------------------------------------------------------------------
   This module contains additional or extended classes.

   Copyright (C) 2008-2011  Koblov Alexander (Alexx2000@mail.ru)

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

unit uClassesEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniPropStorage;

type
  { TIniPropStorageEx }

  TIniPropStorageEx = class(TCustomIniPropStorage)
  private
    FPercentSize: Integer;
    function ChangeIdent(const Ident: String): String;
  protected
    function IniFileClass: TIniFileClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Restore; override;
    function DoReadString(const Section, Ident, default: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;

    property PercentSize: Integer read FPercentSize write FPercentSize;
  end;

implementation

uses
  Forms, DCStrUtils, DCClassesUtf8;

{ TIniPropStorageEx }

function TIniPropStorageEx.IniFileClass: TIniFileClass;
begin
  Result:= TIniFileEx;
end;

constructor TIniPropStorageEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPercentSize:= 5;
end;

procedure TIniPropStorageEx.Restore;
var
  mLeft, mTop, // monitor left and top
  mWidth, mHeight, // monitor width and height
  pWidth, pHeight: Integer;
begin
  inherited Restore;
  if Self.Owner is TCustomForm then
    with Self.Owner as TCustomForm do
    begin
      mLeft:= Monitor.Left;
      mTop:= Monitor.Top;
      mWidth:= Monitor.Width;
      mHeight:= Monitor.Height;

      pWidth:= (mWidth * FPercentSize) div 100;
      pHeight:= (mHeight * FPercentSize) div 100;

      if (mWidth < Width) or (mHeight < Height) then
        begin
          Width:= mWidth - pWidth;
          Height:= mHeight - (pHeight * 2);
        end;

      if (Top > (mTop + mHeight - pHeight)) or (Top < mTop) then
        Top:= mTop + pHeight;
      if (Left > (mLeft + mWidth - pWidth)) or ((Left + Width - pWidth) < mLeft) then
        Left:= mLeft + pWidth;
    end;
end;

function TIniPropStorageEx.DoReadString(const Section, Ident, default: string): string;
begin
  Result := inherited DoReadString(Section, ChangeIdent(Ident), default);
end;

procedure TIniPropStorageEx.DoWriteString(const Section, Ident, Value: string);
begin
  inherited DoWriteString(Section, ChangeIdent(Ident), Value);
end;

function TIniPropStorageEx.ChangeIdent(const Ident: String): String;
begin
  // Change component name to class name.
  if StrBegins(Ident, Owner.Name) then
    Result := Owner.ClassName + Copy(Ident, 1 + Length(Owner.Name), MaxInt)
  else
    Result := Ident;
end;

end.
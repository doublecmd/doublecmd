{
   Double Commander
   -------------------------------------------------------------------------
   Options groups

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsGroups;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  fOptionsFrame;

type

  { TOptionsGroup }

  TOptionsGroup = class(TOptionsEditor)
  public
    class function IsEmpty: Boolean; override;
  end;

  { TOptionsColorsGroup }

  TOptionsColorsGroup = class(TOptionsGroup)
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

  { TOptionsToolsGroup }

  TOptionsToolsGroup = class(TOptionsGroup)
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

uses
  uLng;

{ TOptionsGroup }

class function TOptionsGroup.IsEmpty: Boolean;
begin
  Result := True;
end;

{ TOptionsToolsGroup }

class function TOptionsToolsGroup.GetIconIndex: Integer;
begin
  Result := 2;
end;

class function TOptionsToolsGroup.GetTitle: String;
begin
  Result := rsOptionsEditorTools;
end;

{ TOptionsColorsGroup }

class function TOptionsColorsGroup.GetIconIndex: Integer;
begin
  Result := 4;
end;

class function TOptionsColorsGroup.GetTitle: String;
begin
  Result := rsOptionsEditorColors;
end;

end.


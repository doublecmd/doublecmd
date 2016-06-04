{
   Double Commander
   -------------------------------------------------------------------------
   Tools options page for the differ tool

   Copyright (C) 2006-2016  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsToolsDiffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Dialogs,
  Buttons, Menus, fOptionsFrame, fOptionsToolBase;

type

  { TfrmOptionsDiffer }

  TfrmOptionsDiffer = class(TfrmOptionsToolBase)
    rgResultingFramePositionAfterCompare: TRadioGroup;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng;

{ TfrmOptionsDiffer }

procedure TfrmOptionsDiffer.Init;
begin
  ExternalTool := etDiffer;
  inherited Init;
  ParseLineToList(rsOptDifferFramePosition, rgResultingFramePositionAfterCompare.Items);
end;

procedure TfrmOptionsDiffer.Load;
begin
  inherited;
  rgResultingFramePositionAfterCompare.ItemIndex := Integer(gResultingFramePositionAfterCompare);
end;

function TfrmOptionsDiffer.Save: TOptionsEditorSaveFlags;
begin
  Result := inherited;
  gResultingFramePositionAfterCompare := TResultingFramePositionAfterCompare(rgResultingFramePositionAfterCompare.ItemIndex);
end;

class function TfrmOptionsDiffer.GetIconIndex: Integer;
begin
  Result := 25;
end;

class function TfrmOptionsDiffer.GetTitle: String;
begin
  Result := rsToolDiffer;
end;

end.


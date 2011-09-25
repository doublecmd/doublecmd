{
   Double Commander
   -------------------------------------------------------------------------
   Mouse options page

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

unit fOptionsMouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls, Spin;

type

  { TfrmOptionsMouse }

  TfrmOptionsMouse = class(TOptionsEditor)
    cbMouseMode: TComboBox;
    cbSelectionByMouse: TCheckBox;
    gbScrolling: TGroupBox;
    gbSelection: TGroupBox;
    lblMouseMode: TLabel;
    rbScrollLineByLine: TRadioButton;
    rbScrollLineByLineCursor: TRadioButton;
    rbScrollPageByPage: TRadioButton;
    seWheelScrollLines: TSpinEdit;
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
  uDCUtils, uGlobs, uLng;

{ TfrmOptionsMouse }

procedure TfrmOptionsMouse.Init;
begin
  ParseLineToList(rsOptMouseSelectionButton, cbMouseMode.Items);
end;

procedure TfrmOptionsMouse.Load;
begin
  cbSelectionByMouse.Checked:=gMouseSelectionEnabled;
  cbMouseMode.ItemIndex := gMouseSelectionButton;
  seWheelScrollLines.Value:= gWheelScrollLines;

  case gScrollMode of
    smLineByLineCursor:
      rbScrollLineByLineCursor.Checked:= True;
    smLineByLine:
      rbScrollLineByLine.Checked:= True;
    smPageByPage:
      rbScrollPageByPage.Checked:= True;
    else
      rbScrollLineByLine.Checked:= True;
  end;
end;

function TfrmOptionsMouse.Save: TOptionsEditorSaveFlags;
begin
  gMouseSelectionEnabled := cbSelectionByMouse.Checked;
  gMouseSelectionButton := cbMouseMode.ItemIndex;
  gWheelScrollLines:= seWheelScrollLines.Value;

  if rbScrollLineByLineCursor.Checked then
    gScrollMode:= smLineByLineCursor
  else if rbScrollLineByLine.Checked then
    gScrollMode:= smLineByLine
  else if rbScrollPageByPage.Checked then
    gScrollMode:= smPageByPage;

  Result := [];
end;

class function TfrmOptionsMouse.GetIconIndex: Integer;
begin
  Result := 27;
end;

class function TfrmOptionsMouse.GetTitle: String;
begin
  Result := rsOptionsEditorMouse;
end;

end.


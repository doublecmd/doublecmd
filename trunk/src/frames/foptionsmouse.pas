{
   Double Commander
   -------------------------------------------------------------------------
   Mouse options page

   Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

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
  fOptionsFrame, StdCtrls, Spin, ExtCtrls;

type

  { TfrmOptionsMouse }

  TfrmOptionsMouse = class(TOptionsEditor)
    cbMouseMode: TComboBox;
    cbSelectionByMouse: TCheckBox;
    chkCursorNoFollow: TCheckBox;
    chkMouseSelectionIconClick: TCheckBox;
    gbScrolling: TGroupBox;
    gbSelection: TGroupBox;
    gbOpenWith: TGroupBox;
    lblMouseMode: TLabel;
    rbDoubleClick: TRadioButton;
    rbSingleClickBoth: TRadioButton;
    rbSingleClickFolders: TRadioButton;
    rbScrollLineByLine: TRadioButton;
    rbScrollLineByLineCursor: TRadioButton;
    rbScrollPageByPage: TRadioButton;
    seWheelScrollLines: TSpinEdit;
    procedure cbSelectionByMouseChange(Sender: TObject);
    procedure rbDoubleClickChange(Sender: TObject);
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

{ TfrmOptionsMouse }

procedure TfrmOptionsMouse.cbSelectionByMouseChange(Sender: TObject);
begin
  cbMouseMode.Enabled:= cbSelectionByMouse.Checked;
  chkMouseSelectionIconClick.Enabled:= cbSelectionByMouse.Checked;
  if not cbSelectionByMouse.Checked then chkMouseSelectionIconClick.Checked:= False;
end;

procedure TfrmOptionsMouse.rbDoubleClickChange(Sender: TObject);
begin
  chkCursorNoFollow.Enabled:= not rbDoubleClick.Checked;
  if rbDoubleClick.Checked then chkCursorNoFollow.Checked:= False;
end;

procedure TfrmOptionsMouse.Init;
begin
  ParseLineToList(rsOptMouseSelectionButton, cbMouseMode.Items);
end;

procedure TfrmOptionsMouse.Load;
begin
  cbSelectionByMouse.Checked:=gMouseSelectionEnabled;
  cbMouseMode.ItemIndex := gMouseSelectionButton;
  seWheelScrollLines.Value:= gWheelScrollLines;
  chkMouseSelectionIconClick.Checked:= Boolean(gMouseSelectionIconClick);

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

  case gMouseSingleClickStart of
    0: rbDoubleClick.Checked:= True;
    1, 5: rbSingleClickBoth.Checked:= True;
    2, 6: rbSingleClickFolders.Checked:= True;
  end;
  chkCursorNoFollow.Enabled:= gMouseSingleClickStart > 0;
  chkCursorNoFollow.Checked:= gMouseSingleClickStart > 4;

  cbSelectionByMouseChange(cbSelectionByMouse);
end;

function TfrmOptionsMouse.Save: TOptionsEditorSaveFlags;
begin
  gMouseSelectionEnabled := cbSelectionByMouse.Checked;
  gMouseSelectionButton := cbMouseMode.ItemIndex;
  gWheelScrollLines:= seWheelScrollLines.Value;
  gMouseSelectionIconClick:= Integer(chkMouseSelectionIconClick.Checked);

  if rbScrollLineByLineCursor.Checked then
    gScrollMode:= smLineByLineCursor
  else if rbScrollLineByLine.Checked then
    gScrollMode:= smLineByLine
  else if rbScrollPageByPage.Checked then
    gScrollMode:= smPageByPage;

  if rbDoubleClick.Checked then
    gMouseSingleClickStart:= 0
  else if rbSingleClickBoth.Checked then
    gMouseSingleClickStart:= 1
  else if rbSingleClickFolders.Checked then
    gMouseSingleClickStart:= 2;

  if chkCursorNoFollow.Checked then
    gMouseSingleClickStart += 4;

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


{
   Double Commander
   -------------------------------------------------------------------------
   Fonts options page

   Copyright (C) 2006-2020 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fOptionsFonts;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, StdCtrls, Spin, Dialogs,

  //DC
  fOptionsFrame, uGlobs;
type
  { TVisualFontElements }
  TVisualFontElements = record
    FontEdit: TEdit;
    FontSpindEdit: TSpinEdit;
  end;

  { TfrmOptionsFonts }
  TfrmOptionsFonts = class(TOptionsEditor)
    dlgFnt: TFontDialog;
    procedure edtFontExit(Sender: TObject);
    procedure edtMouseWheelDown(Sender: TObject; Shift: TShiftState; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);
    procedure edtMouseWheelUp(Sender: TObject; Shift: TShiftState; {%H-}MousePos: TPoint; var {%H-}Handled: boolean);
    procedure edtFontSizeChange(Sender: TObject);
    procedure btnSelFontClick(Sender: TObject);
  private
    LocalVisualFontElements: array[0..pred(Length(TDCFontsOptions))] of TVisualFontElements;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Controls,

  //DC
  uLng;

{ TfrmOptionsFonts }

{ TfrmOptionsFonts.GetIconIndex }
class function TfrmOptionsFonts.GetIconIndex: integer;
begin
  Result := 3;
end;

{ TfrmOptionsFonts.GetTitle }
class function TfrmOptionsFonts.GetTitle: string;
begin
  Result := rsOptionsEditorFonts;
end;

{ TfrmOptionsFonts.Init }
// We draw manually the whole thing from the gFont array instead of having designed the form at the conception time.
// This way, we're sure to don't forget a font, for one, and second, if we ever add a font, no modification will be required here, in the configuration section.
// ...or maybe just if the font has to be monospace.
procedure TfrmOptionsFonts.Init;
var
  ALabelFont: TLabel;
  AEditFont: TEdit;
  APreviousEditFont: TEdit = nil;
  ASpinEditFontSize: TSpinEdit;
  AButtonFont: TButton;
  iFontIndex: integer;
begin
  for iFontIndex := 0 to pred(Length(TDCFontsOptions)) do
  begin
    ALabelFont := TLabel.Create(Self);
    ALabelFont.Parent := Self;
    ALabelFont.Caption := gFonts[TDCFont(iFontIndex)].Usage;

    AEditFont := TEdit.Create(Self);
    LocalVisualFontElements[iFontIndex].FontEdit := AEditFont;
    AEditFont.Parent := Self;
    AEditFont.Tag := iFontIndex;
    AEditFont.OnExit := @edtFontExit;
    AEditFont.OnMouseWheelDown := @edtMouseWheelDown;
    AEditFont.OnMouseWheelUp := @edtMouseWheelUp;
    AEditFont.Anchors := [akTop, akLeft, akRight];
    ALabelFont.FocusControl := AEditFont;

    ASpinEditFontSize := TSpinEdit.Create(Self);
    LocalVisualFontElements[iFontIndex].FontSpindEdit := ASpinEditFontSize;
    ASpinEditFontSize.Tag := iFontIndex;
    ASpinEditFontSize.Parent := Self;
    ASpinEditFontSize.OnChange := @edtFontSizeChange;
    ASpinEditFontSize.MinValue := gFonts[TDCFont(iFontIndex)].MinValue;
    ASpinEditFontSize.MaxValue := gFonts[TDCFont(iFontIndex)].MaxValue;
    ASpinEditFontSize.Width := 55;
    ASpinEditFontSize.Anchors := [akTop, akRight];

    AButtonFont := TButton.Create(Self);
    AButtonFont.Tag := iFontIndex;
    AButtonFont.Parent := Self;;
    AButtonFont.AutoSize := True;
    AButtonFont.Caption := '...';
    AButtonFont.OnClick := @btnSelFontClick;
    AButtonFont.Anchors := [akTop, akRight];

    ALabelFont.AnchorSideLeft.Control := Self;
    if APreviousEditFont <> nil then
    begin
      ALabelFont.AnchorSideTop.Control := APreviousEditFont;
      ALabelFont.AnchorSideTop.Side := asrBottom;
      ALabelFont.BorderSpacing.Top := 6;
    end
    else
    begin
      ALabelFont.AnchorSideTop.Control := Self;
    end;

    AEditFont.AnchorSideLeft.Control := ALabelFont;
    AEditFont.AnchorSideTop.Control := ALabelFont;
    AEditFont.AnchorSideTop.Side := asrBottom;
    AEditFont.AnchorSideRight.Control := ASpinEditFontSize;

    ASpinEditFontSize.AnchorSideTop.Control := AEditFont;
    ASpinEditFontSize.AnchorSideTop.Side := asrCenter;
    ASpinEditFontSize.AnchorSideRight.Control := AButtonFont;

    AButtonFont.AnchorSideTop.Control := AEditFont;
    AButtonFont.AnchorSideTop.Side := asrCenter;
    AButtonFont.AnchorSideRight.Control := Self;
    AButtonFont.AnchorSideRight.Side := asrBottom;
    AButtonFont.AnchorSideBottom.Side := asrBottom;

    APreviousEditFont := AEditFont;
  end;
end;

{ TfrmOptionsFonts.Load }
// The idea here is to take the general font style and apply them to TEdit in the page.
// User plays with that to set the properties he wants.
// Then at the end we recuperate the font from the TEdit's and store properties user set back to the general fonts.
procedure TfrmOptionsFonts.Load;
var
  iFontIndex: integer;
begin
  for iFontIndex := 0 to pred(Length(TDCFontsOptions)) do
  begin
    LocalVisualFontElements[iFontIndex].FontEdit.Text := gFonts[TDCFont(iFontIndex)].Name;
    FontOptionsToFont(gFonts[TDCFont(iFontIndex)], LocalVisualFontElements[iFontIndex].FontEdit.Font);
    LocalVisualFontElements[iFontIndex].FontSpindEdit.Value := gFonts[TDCFont(iFontIndex)].Size;
  end;
end;

{ TfrmOptionsFonts.Save }
function TfrmOptionsFonts.Save: TOptionsEditorSaveFlags;
var
  iFontIndex: integer;
begin
  Result := [];
  for iFontIndex := 0 to pred(Length(TDCFontsOptions)) do
    FontToFontOptions(LocalVisualFontElements[iFontIndex].FontEdit.Font, gFonts[TDCFont(iFontIndex)]);
end;

{ TfrmOptionsFonts.edtFontExit }
procedure TfrmOptionsFonts.edtFontExit(Sender: TObject);
begin
  TEdit(Sender).Font.Name := TEdit(Sender).Text;
end;

{ TfrmOptionsFonts.edtMouseWheelDown }
procedure TfrmOptionsFonts.edtMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  if (ssCtrl in Shift) and (LocalVisualFontElements[TEdit(Sender).Tag].FontSpindEdit.Value > gFonts[TDCFont(TEdit(Sender).Tag)].MinValue) then
  begin
    TEdit(Sender).Font.Size := TEdit(Sender).Font.Size - 1;
    LocalVisualFontElements[TEdit(Sender).Tag].FontSpindEdit.Value := TEdit(Sender).Font.Size;
  end;
end;

{ TfrmOptionsFonts.edtMouseWheelUp }
procedure TfrmOptionsFonts.edtMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  if (ssCtrl in Shift) and (LocalVisualFontElements[TEdit(Sender).Tag].FontSpindEdit.Value < gFonts[TDCFont(TEdit(Sender).Tag)].MaxValue) then
  begin
    TEdit(Sender).Font.Size := TEdit(Sender).Font.Size + 1;
    LocalVisualFontElements[TEdit(Sender).Tag].FontSpindEdit.Value := TEdit(Sender).Font.Size;
  end;
end;

{ TfrmOptionsFonts.edtFontSizeChange }
procedure TfrmOptionsFonts.edtFontSizeChange(Sender: TObject);
begin
  if (LocalVisualFontElements[TSpinEdit(Sender).Tag].FontEdit.Font.Size <> TSpinEdit(Sender).Value) then
    LocalVisualFontElements[TSpinEdit(Sender).Tag].FontEdit.Font.Size := TSpinEdit(Sender).Value;
end;

{ TfrmOptionsFonts.btnSelFontClick }
procedure TfrmOptionsFonts.btnSelFontClick(Sender: TObject);
const
  cMonoFonts = [dcfEditor, dcfViewer, dcfLog, dcfConsole];
begin
  begin
    dlgFnt.Font := LocalVisualFontElements[TButton(Sender).Tag].FontEdit.Font;
    if (TDCFont(TButton(Sender).Tag) in cMonoFonts) then
      dlgFnt.Options := dlgFnt.Options + [fdFixedPitchOnly, fdNoStyleSel]
    else
      dlgFnt.Options := dlgFnt.Options - [fdFixedPitchOnly, fdNoStyleSel];
    if dlgFnt.Execute then
    begin
      LocalVisualFontElements[TButton(Sender).Tag].FontEdit.Font := dlgFnt.Font;
      LocalVisualFontElements[TButton(Sender).Tag].FontEdit.Text := dlgFnt.Font.Name;
      LocalVisualFontElements[TButton(Sender).Tag].FontSpindEdit.Value := dlgFnt.Font.Size;
    end;
  end;
end;

end.


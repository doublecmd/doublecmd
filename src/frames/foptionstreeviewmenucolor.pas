{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of Tree View Menu Color and Layout.

   Copyright (C) 2016-2023 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsTreeViewMenuColor;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus,
  Dialogs, ComCtrls, KASComboBox, Spin, LMessages,

  //DC
  uGlobs, fOptionsFrame, fTreeViewMenu, Types;

type
  { TfrmOptionsTreeViewMenuColor }
  TfrmOptionsTreeViewMenuColor = class(TOptionsEditor)
    btFont: TButton;
    dlgFnt: TFontDialog;
    edFontName: TEdit;
    sedFont: TSpinEdit;
    gbFont: TGroupBox;
    gbLayoutAndColors: TGroupBox;
    cbkUsageKeyboardShortcut: TCheckBox;
    lblBackgroundColor: TLabel;
    cbBackgroundColor: TKASColorBoxButton;
    lblShortcutColor: TLabel;
    cbShortcutColor: TKASColorBoxButton;
    lblNormalTextColor: TLabel;
    cbNormalTextColor: TKASColorBoxButton;
    lblSecondaryTextColor: TLabel;
    cbSecondaryTextColor: TKASColorBoxButton;
    lblFoundTextColor: TLabel;
    cbFoundTextColor: TKASColorBoxButton;
    lblUnselectableTextColor: TLabel;
    cbUnselectableTextColor: TKASColorBoxButton;
    lblCursorColor: TLabel;
    cbCursorColor: TKASColorBoxButton;
    lblShortcutUnderCursor: TLabel;
    cbShortcutUnderCursor: TKASColorBoxButton;
    lblNormalTextUnderCursor: TLabel;
    cbNormalTextUnderCursor: TKASColorBoxButton;
    lblSecondaryTextUnderCursor: TLabel;
    cbSecondaryTextUnderCursor: TKASColorBoxButton;
    lblFoundTextUnderCursor: TLabel;
    cbFoundTextUnderCursor: TKASColorBoxButton;
    lblUnselectableUnderCursor: TLabel;
    cbUnselectableUnderCursor: TKASColorBoxButton;
    lblPreview: TLabel;
    TreeViewMenuSample: TTreeView;
    optColorDialog: TColorDialog;
    procedure btFontClick(Sender: TObject);
    procedure RefreshColorOfOurSampleClick(Sender: TObject);
    procedure sedFontChange(Sender: TObject);
    procedure TreeViewMenuSampleMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure TreeViewMenuSampleMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure CMThemeChanged(var Message: TLMessage); message CM_THEMECHANGED;
  private
    { Private declarations }
    TreeViewMenuGenericRoutineAndVarHolder: TTreeViewMenuGenericRoutineAndVarHolder;
    TempoFont: TDCFontOptions;
    procedure ApplyTempoFontToVisual;
  public
    { Public declarations }
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType, LCLProc, LCLIntf,

  //DC
  uLng, uDCUtils, fmain, DCOSUtils;

{ TfrmOptionsTreeViewMenuColor.Init }
procedure TfrmOptionsTreeViewMenuColor.Init;
var
  iLonguestName: integer = 150;
  BaseLevelNode, SubLevelNode: TTreeNode;

  procedure ProcessLabelLength(ALabel: TLabel);
  begin
    if ALabel.Canvas.TextWidth(ALabel.Caption) > iLonguestName then iLonguestName := ALabel.Canvas.TextWidth(ALabel.Caption);
  end;

begin
  // All the combobox are referenced to "cbBackgroundColor".
  // Let's determine the longuest label and then we'll set the "cbBackgroundColor" to a location far enough on right so all labels will be visible correctly.
  ProcessLabelLength(lblBackgroundColor);
  ProcessLabelLength(lblShortcutColor);
  ProcessLabelLength(lblNormalTextColor);
  ProcessLabelLength(lblSecondaryTextColor);
  ProcessLabelLength(lblFoundTextColor);
  ProcessLabelLength(lblUnselectableTextColor);
  ProcessLabelLength(lblCursorColor);
  ProcessLabelLength(lblShortcutUnderCursor);
  ProcessLabelLength(lblNormalTextUnderCursor);
  ProcessLabelLength(lblSecondaryTextUnderCursor);
  ProcessLabelLength(lblFoundTextUnderCursor);
  ProcessLabelLength(lblUnselectableUnderCursor);
  cbBackgroundColor.Left := 10 + iLonguestName + 6 + 10;
  cbBackgroundColor.BorderSpacing.Left:=10 + iLonguestName + 6 + 10;

  TreeViewMenuGenericRoutineAndVarHolder := TTreeViewMenuGenericRoutineAndVarHolder.Create;
  TreeViewMenuGenericRoutineAndVarHolder.SearchingText := rsStrPreviewSearchingLetters;
  TreeViewMenuGenericRoutineAndVarHolder.CaseSensitive := False;
  TreeViewMenuGenericRoutineAndVarHolder.IgnoreAccents := True;
  TreeViewMenuGenericRoutineAndVarHolder.ShowWholeBranchIfMatch := True;
  TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode := False;
  TreeViewMenuGenericRoutineAndVarHolder.ShowShortcut := gTreeViewMenuUseKeyboardShortcut;
  with gColors.TreeViewMenu^ do
  begin
    TreeViewMenuGenericRoutineAndVarHolder.BackgroundColor := BackgroundColor;
    TreeViewMenuGenericRoutineAndVarHolder.ShortcutColor := ShortcutColor;
    TreeViewMenuGenericRoutineAndVarHolder.NormalTextColor := NormalTextColor;
    TreeViewMenuGenericRoutineAndVarHolder.SecondaryTextColor := SecondaryTextColor;
    TreeViewMenuGenericRoutineAndVarHolder.FoundTextColor := FoundTextColor;
    TreeViewMenuGenericRoutineAndVarHolder.UnselectableTextColor := UnselectableTextColor;
    TreeViewMenuGenericRoutineAndVarHolder.CursorColor := CursorColor;
    TreeViewMenuGenericRoutineAndVarHolder.ShortcutUnderCursor := ShortcutUnderCursor;
    TreeViewMenuGenericRoutineAndVarHolder.NormalTextUnderCursor := NormalTextUnderCursor;
    TreeViewMenuGenericRoutineAndVarHolder.SecondaryTextUnderCursor := SecondaryTextUnderCursor;
    TreeViewMenuGenericRoutineAndVarHolder.FoundTextUnderCursor := FoundTextUnderCursor;
    TreeViewMenuGenericRoutineAndVarHolder.UnselectableUnderCursor := UnselectableUnderCursor;
  end;
  TreeViewMenuSample.OnAdvancedCustomDrawItem := @TreeViewMenuGenericRoutineAndVarHolder.TreeViewMenuAdvancedCustomDrawItem;

  // Let's populate our treeview sample with at least an example of each.
  TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, nil, rsStrPreviewJustPreview);
  BaseLevelNode := TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, nil, 'Double Commander');
  SubLevelNode := TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, BaseLevelNode, rsStrPreviewWordWithSearched1, rsStrPreviewSideNote);
  TTreeMenuItem(SubLevelNode.Data).KeyboardShortcut := '1';
  SubLevelNode := TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, BaseLevelNode, rsStrPreviewWordWithSearched2, rsStrPreviewSideNote);
  TTreeMenuItem(SubLevelNode.Data).KeyboardShortcut := '2';
  SubLevelNode := TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, BaseLevelNode, rsStrPreviewWordWithSearched3, rsStrPreviewSideNote);
  TTreeMenuItem(SubLevelNode.Data).KeyboardShortcut := '3';
  BaseLevelNode := TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, nil, rsStrPreviewOthers);
  TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, BaseLevelNode, rsStrPreviewWordWithoutSearched1);
  TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, BaseLevelNode, rsStrPreviewWordWithoutSearched2);
  TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(TreeViewMenuSample, BaseLevelNode, rsStrPreviewWordWithoutSearched3);
  TreeViewMenuSample.FullExpand;
  TreeViewMenuSample.Items[0].Selected := True;
end;

{ TfrmOptionsTreeViewMenuColor.Load }
procedure TfrmOptionsTreeViewMenuColor.Load;
begin
  cbkUsageKeyboardShortcut.Checked := gTreeViewMenuUseKeyboardShortcut;
  with gColors.TreeViewMenu^ do
  begin
    cbBackgroundColor.Selected := BackgroundColor;
    cbShortcutColor.Selected := ShortcutColor;
    cbNormalTextColor.Selected := NormalTextColor;
    cbSecondaryTextColor.Selected := SecondaryTextColor;
    cbFoundTextColor.Selected := FoundTextColor;
    cbUnselectableTextColor.Selected := UnselectableTextColor;
    cbCursorColor.Selected := CursorColor;
    cbShortcutUnderCursor.Selected := ShortcutUnderCursor;
    cbNormalTextUnderCursor.Selected := NormalTextUnderCursor;
    cbSecondaryTextUnderCursor.Selected := SecondaryTextUnderCursor;
    cbFoundTextUnderCursor.Selected := FoundTextUnderCursor;
    cbUnselectableUnderCursor.Selected := UnselectableUnderCursor;
  end;
  TempoFont := gFonts[dcfTreeViewMenu];
  ApplyTempoFontToVisual;
end;

{ TfrmOptionsTreeViewMenuColor.Save }
function TfrmOptionsTreeViewMenuColor.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  gTreeViewMenuUseKeyboardShortcut := cbkUsageKeyboardShortcut.Checked;
  with gColors.TreeViewMenu^ do
  begin
    BackgroundColor := cbBackgroundColor.Selected;
    ShortcutColor := cbShortcutColor.Selected;
    NormalTextColor := cbNormalTextColor.Selected;
    SecondaryTextColor := cbSecondaryTextColor.Selected;
    FoundTextColor := cbFoundTextColor.Selected;
    UnselectableTextColor := cbUnselectableTextColor.Selected;
    CursorColor := cbCursorColor.Selected;
    ShortcutUnderCursor := cbShortcutUnderCursor.Selected;
    NormalTextUnderCursor := cbNormalTextUnderCursor.Selected;
    SecondaryTextUnderCursor := cbSecondaryTextUnderCursor.Selected;
    FoundTextUnderCursor := cbFoundTextUnderCursor.Selected;
    UnselectableUnderCursor := cbUnselectableUnderCursor.Selected;
  end;
  gFonts[dcfTreeViewMenu] := TempoFont;
end;

procedure TfrmOptionsTreeViewMenuColor.CMThemeChanged(var Message: TLMessage);
begin
  LoadSettings;
  RefreshColorOfOurSampleClick(Self);
end;

{ TfrmOptionsTreeViewMenuColor.GetIconIndex }
class function TfrmOptionsTreeViewMenuColor.GetIconIndex: integer;
begin
  Result := 40;
end;

{ TfrmOptionsTreeViewMenuColor.GetTitle }
class function TfrmOptionsTreeViewMenuColor.GetTitle: string;
begin
  Result := rsOptionsEditorTreeViewMenuColors;
end;

{ TfrmOptionsTreeViewMenuColor.Destroy }
destructor TfrmOptionsTreeViewMenuColor.Destroy;
begin
  FreeAndNil(TreeViewMenuGenericRoutineAndVarHolder);
  inherited Destroy;
end;

{ TfrmOptionsTreeViewMenuColor.RefreshColorOfOurSampleClick }
procedure TfrmOptionsTreeViewMenuColor.RefreshColorOfOurSampleClick(Sender: TObject);
begin
  TreeViewMenuGenericRoutineAndVarHolder.ShowShortcut := cbkUsageKeyboardShortcut.Checked;
  TreeViewMenuGenericRoutineAndVarHolder.BackgroundColor := cbBackgroundColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.ShortcutColor := cbShortcutColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.NormalTextColor := cbNormalTextColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.SecondaryTextColor := cbSecondaryTextColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.FoundTextColor := cbFoundTextColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.UnselectableTextColor := cbUnselectableTextColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.CursorColor := cbCursorColor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.ShortcutUnderCursor := cbShortcutUnderCursor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.NormalTextUnderCursor := cbNormalTextUnderCursor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.SecondaryTextUnderCursor := cbSecondaryTextUnderCursor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.FoundTextUnderCursor := cbFoundTextUnderCursor.Selected;
  TreeViewMenuGenericRoutineAndVarHolder.UnselectableUnderCursor := cbUnselectableUnderCursor.Selected;
  TreeViewMenuSample.Refresh;
end;

{ TfrmOptionsTreeViewMenuColor.ApplyTempoFontToVisual }
procedure TfrmOptionsTreeViewMenuColor.ApplyTempoFontToVisual;
begin
  FontOptionsToFont(TempoFont, edFontName.Font);
  FontOptionsToFont(TempoFont, TreeViewMenuSample.Font);
  FontOptionsToFont(TempoFont, sedFont.Font);
  FontOptionsToFont(TempoFont, btFont.Font);
  edFontName.Text := TempoFont.Name;
  if sedFont.Value <> TempoFont.Size then
    sedFont.Value := TempoFont.Size;
end;

{ TfrmOptionsTreeViewMenuColor.sedFontChange }
procedure TfrmOptionsTreeViewMenuColor.sedFontChange(Sender: TObject);
begin
  if TempoFont.Size <> TSpinEdit(Sender).Value then
  begin
    TempoFont.Size := TSpinEdit(Sender).Value;
    ApplyTempoFontToVisual;
  end;
end;

{ TfrmOptionsTreeViewMenuColor.btFontClick }
procedure TfrmOptionsTreeViewMenuColor.btFontClick(Sender: TObject);
begin
  FontOptionsToFont(TempoFont, dlgFnt.Font);
  if dlgFnt.Execute then
  begin
    FontToFontOptions(dlgFnt.Font, TempoFont);
    ApplyTempoFontToVisual;
  end;
end;

{ TfrmOptionsTreeViewMenuColor.TreeViewMenuSampleMouseWheelDown }
procedure TfrmOptionsTreeViewMenuColor.TreeViewMenuSampleMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) and (TempoFont.Size > TempoFont.MinValue) then
  begin
    dec(TempoFont.Size);
    ApplyTempoFontToVisual;
    Handled := True;
  end;

end;

{ TfrmOptionsTreeViewMenuColor.TreeViewMenuSampleMouseWheelUp }
procedure TfrmOptionsTreeViewMenuColor.TreeViewMenuSampleMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) and (TempoFont.Size < TempoFont.MaxValue) then
  begin
    inc(TempoFont.Size);
    ApplyTempoFontToVisual;
    Handled := True;
  end;
end;


end.

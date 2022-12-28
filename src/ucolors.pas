{
   Double Commander
   -------------------------------------------------------------------------
   Color themes unit

   Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

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

unit uColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpjson, DCXmlConfig;

const
  THEME_COUNT = 2;
  DARK_THEME = 'Dark';
  LIGHT_THEME = 'Light';
  THEME_NAME: array[0..Pred(THEME_COUNT)] of String = (LIGHT_THEME, DARK_THEME);

type

  TFilePanelColors = record
    CursorBorderColor: TColor;
    ForeColor,
    BackColor,
    BackColor2,
    MarkColor,
    CursorColor,
    CursorText,
    InactiveCursorColor,
    InactiveMarkColor: TColor;
  end;
  PFilePanelColors = ^TFilePanelColors;

  TFreeSpaceIndColors = record
    ForeColor,
    BackColor,
    ThresholdForeColor: TColor;
  end;
  PFreeSpaceIndColors = ^TFreeSpaceIndColors;

  TPathColors = record
    ActiveColor,
    ActiveFontColor,
    InactiveColor,
    InactiveFontColor: TColor;
  end;
  PPathColors = ^TPathColors;

  TLogColors = record
     InfoColor,
     ErrorColor,
     SuccessColor: TColor;
   end;
  PLogColors = ^TLogColors;

  TSyncDirsColors = record
    LeftColor,
    RightColor,
    UnknownColor: TColor;
  end;
  PSyncDirsColors = ^TSyncDirsColors;

  TViewerColors = record
    ImageBackColor1,
    ImageBackColor2: TColor;
    BookBackgroundColor,
    BookFontColor: TColor;
  end;
  PViewerColors = ^TViewerColors;

  TDifferColors = record
    AddedColor: TColor;
    DeletedColor: TColor;
    ModifiedColor: TColor;
    ModifiedBinaryColor: TColor;
  end;
  PDifferColors = ^TDifferColors;

  TTreeViewMenuColors = record
    BackgroundColor: TColor;
    ShortcutColor: TColor;
    NormalTextColor: TColor;
    SecondaryTextColor: TColor;
    FoundTextColor: TColor;
    UnselectableTextColor: TColor;
    CursorColor: TColor;
    ShortcutUnderCursor: TColor;
    NormalTextUnderCursor: TColor;
    SecondaryTextUnderCursor: TColor;
    FoundTextUnderCursor: TColor;
    UnselectableUnderCursor: TColor;
  end;
  PTreeViewMenuColors = ^TTreeViewMenuColors;

  { TColorTheme }

  TColorTheme = class
  public
    Log: TLogColors;
    Path: TPathColors;
    Viewer: TViewerColors;
    Differ: TDifferColors;
    SyncDirs: TSyncDirsColors;
    FilePanel: TFilePanelColors;
    FreeSpaceInd: TFreeSpaceIndColors;
    TreeViewMenu: TTreeViewMenuColors;
  public
    procedure Assign(ATheme: TColorTheme);
  end;

  { TColorThemes }

  TColorThemes = class
  private
    FColors: array[0..Pred(THEME_COUNT)] of TColorTheme;
  private
    procedure CreateDefault;
  public
    function Log: PLogColors;
    function Path: PPathColors;
    function Differ: PDifferColors;
    function Viewer: PViewerColors;
    function SyncDirs: PSyncDirsColors;
    function FilePanel: PFilePanelColors;
    function FreeSpaceInd: PFreeSpaceIndColors;
    function TreeViewMenu: PTreeViewMenuColors;
  public
    constructor Create;
    destructor Destroy; override;
    function Current: TColorTheme;
    class function StyleIndex: Integer;
    procedure LoadFromXml(AConfig: TXmlConfig);
    procedure Save(AConfig: TJSONObject);
    procedure Load(AConfig: TJSONObject);
    function GetTheme(const AName: String): TColorTheme;
  end;

implementation

uses
  DCClassesUtf8, uSynDiffControls, uOSForms, uGlobs;

{ TColorTheme }

procedure TColorTheme.Assign(ATheme: TColorTheme);
begin
  Log:= ATheme.Log;
  Path:= ATheme.Path;
  Differ:= ATheme.Differ;
  Viewer:= ATheme.Viewer;
  SyncDirs:= ATheme.SyncDirs;
  FilePanel:= ATheme.FilePanel;
  FreeSpaceInd:= ATheme.FreeSpaceInd;
  TreeViewMenu:= ATheme.TreeViewMenu;
end;

{ TColorThemes }

function TColorThemes.Log: PLogColors;
begin
  Result:= @Current.Log;
end;

function TColorThemes.Path: PPathColors;
begin
  Result:= @Current.Path;
end;

function TColorThemes.Differ: PDifferColors;
begin
  Result:= @Current.Differ;
end;

function TColorThemes.Viewer: PViewerColors;
begin
  Result:= @Current.Viewer;
end;

function TColorThemes.SyncDirs: PSyncDirsColors;
begin
  Result:= @Current.SyncDirs;
end;

function TColorThemes.FilePanel: PFilePanelColors;
begin
  Result:= @Current.FilePanel;
end;

function TColorThemes.FreeSpaceInd: PFreeSpaceIndColors;
begin
  Result:= @Current.FreeSpaceInd;
end;

function TColorThemes.TreeViewMenu: PTreeViewMenuColors;
begin
  Result:= @Current.TreeViewMenu;
end;

constructor TColorThemes.Create;
begin
  CreateDefault;
end;

destructor TColorThemes.Destroy;
begin
  FColors[0].Free;
  FColors[1].Free;
  inherited Destroy;
end;

class function TColorThemes.StyleIndex: Integer;
begin
  if DarkStyle then
    Result:= 1
  else begin
    Result:= 0;
  end;
end;

procedure TColorThemes.CreateDefault;
begin
  // Light theme
  FColors[0]:= TColorTheme.Create;

  with FColors[0].FilePanel do
  begin
    CursorBorderColor := clHighlight;
    ForeColor := clWindowText;
    BackColor := clWindow;
    BackColor2 := clWindow;
    MarkColor := clRed;
    CursorColor := clHighlight;
    CursorText := clHighlightText;
    InactiveCursorColor := clInactiveCaption;
    InactiveMarkColor := clMaroon;
  end;
  with FColors[0].Path do
  begin
    ActiveColor := clHighlight;
    ActiveFontColor := clHighlightText;
    InactiveColor := clBtnFace;
    InactiveFontColor := clBtnText;
  end;
  with FColors[0].FreeSpaceInd do
  begin
    ForeColor := clBlack;
    BackColor := clWhite;
    ThresholdForeColor := clRed;
  end;
  with FColors[0].Log do
  begin
    InfoColor:= clNavy;
    ErrorColor:= clRed;
    SuccessColor:= clGreen;
  end;
  with FColors[0].SyncDirs do
  begin
    LeftColor:= clGreen;
    RightColor:= clBlue;
    UnknownColor:= clRed;
  end;
  with FColors[0].Viewer do
  begin
    ImageBackColor1 := clWindow;
    ImageBackColor2 := clDefault;
    BookBackgroundColor := clBlack;
    BookFontColor := clWhite;
  end;
  with FColors[0].Differ do
  begin
    AddedColor := clPaleGreen;
    DeletedColor := clPaleRed;
    ModifiedColor := clPaleBlue;
    ModifiedBinaryColor := clRed;
  end;
  with FColors[0].TreeViewMenu do
  begin
    BackgroundColor := clForm;
    ShortcutColor := clRed;
    NormalTextColor := clWindowText;
    SecondaryTextColor := clWindowFrame;
    FoundTextColor := clHighLight;
    UnselectableTextColor := clGrayText;
    CursorColor := clHighlight;
    ShortcutUnderCursor := clHighlightText;
    NormalTextUnderCursor := clHighlightText;
    SecondaryTextUnderCursor := clBtnHighlight;
    FoundTextUnderCursor := clYellow;
    UnselectableUnderCursor := clGrayText;
  end;

  // Dark theme
  FColors[1]:= TColorTheme.Create;
  FColors[1].Assign(FColors[0]);

  with FColors[1].Log do
  begin
    InfoColor:= $C09B61;
    ErrorColor:= $6166C0;
    SuccessColor:= $8AD277;
  end;
  with FColors[1].Differ do
  begin
    AddedColor:= $8AD277;
    DeletedColor:= $6166C0;
    ModifiedColor:= $C09B61;
    ModifiedBinaryColor:= $6166C0;
  end;
  with FColors[1].SyncDirs do
  begin
    LeftColor:= $8AD277;
    RightColor:= $C09B61;
    UnknownColor:= $6166C0;
  end;
end;

procedure TColorThemes.LoadFromXml(AConfig: TXmlConfig);
var
  Root, Node: TXmlNode;
  ColorTheme: TColorTheme;
  LoadedConfigVersion: Integer;
begin
  with AConfig do
  begin
    Root := RootNode;

    LoadedConfigVersion := GetAttr(Root, 'ConfigVersion', ConfigVersion);

    if (LoadedConfigVersion >= ConfigVersion) then Exit;

    ColorTheme:= Current;

    { Colors }
    Node := Root.FindNode('Colors');
    if Assigned(Node) then
    begin
      with ColorTheme.FilePanel do
      begin
        CursorBorderColor:= GetValue(Node, 'CursorBorderColor', CursorBorderColor);
        ForeColor:= GetValue(Node, 'Foreground', ForeColor);
        BackColor:= GetValue(Node, 'Background', BackColor);
        BackColor2:= GetValue(Node, 'Background2', BackColor2);
        MarkColor:= GetValue(Node, 'Mark', MarkColor);
        CursorColor:= GetValue(Node, 'Cursor', CursorColor);
        CursorText:= GetValue(Node, 'CursorText', CursorText);
        InactiveCursorColor:= GetValue(Node, 'InactiveCursor', InactiveCursorColor);
        InactiveMarkColor:= GetValue(Node, 'InactiveMark', InactiveMarkColor);
      end;
      with ColorTheme.Path do
      begin
        ActiveColor := GetValue(Node, 'PathLabel/ActiveColor', ActiveColor);
        ActiveFontColor := GetValue(Node, 'PathLabel/ActiveFontColor', ActiveFontColor);
        InactiveColor := GetValue(Node, 'PathLabel/InactiveColor', InactiveColor);
        InactiveFontColor := GetValue(Node, 'PathLabel/InactiveFontColor', InactiveFontColor);
      end;
      with ColorTheme.FreeSpaceInd do
      begin
        ForeColor := GetValue(Node, 'FreeSpaceIndicator/ForeColor', ForeColor);
        BackColor := GetValue(Node, 'FreeSpaceIndicator/BackColor', BackColor);
        ThresholdForeColor := GetValue(Node, 'FreeSpaceIndicator/ThresholdForeColor', ThresholdForeColor);
      end;
      with ColorTheme.Log do
      begin
        InfoColor:= GetValue(Node, 'LogWindow/Info', InfoColor);
        ErrorColor:= GetValue(Node, 'LogWindow/Error', ErrorColor);
        SuccessColor:= GetValue(Node, 'LogWindow/Success', SuccessColor);
      end;
    end;

    { Differ }
    Node:= Root.FindNode('Differ/Colors');
    if Assigned(Node) then
    begin
      with ColorTheme.Differ do
      begin
        AddedColor := GetValue(Node, 'Added', AddedColor);
        DeletedColor := GetValue(Node, 'Deleted', DeletedColor);
        ModifiedColor := GetValue(Node, 'Modified', ModifiedColor);
        Node := FindNode(Node, 'Colors/Binary');
        if Assigned(Node) then begin
          ModifiedBinaryColor := GetValue(Node, 'Modified', ModifiedBinaryColor);
        end;
      end;
    end;

        { Viewer }
    Node := Root.FindNode('Viewer');
    if Assigned(Node) then
    begin
      with ColorTheme.Viewer do
      begin
        ImageBackColor1:= GetValue(Node, 'ImageBackColor1', ImageBackColor1);
        ImageBackColor2:= GetValue(Node, 'ImageBackColor2', ImageBackColor2);
        BookBackgroundColor := GetValue(Node, 'BackgroundColor', BookBackgroundColor);
        BookFontColor := GetValue(Node, 'FontColor', BookFontColor);
      end;
    end;

    { Tree View Menu }
    Node := Root.FindNode('TreeViewMenu');
    if Assigned(Node) then
    begin
      with ColorTheme.TreeViewMenu do
      begin
        BackgroundColor := GetValue(Node, 'BackgroundColor', BackgroundColor);
        ShortcutColor := GetValue(Node, 'ShortcutColor', ShortcutColor);
        NormalTextColor := GetValue(Node, 'NormalTextColor', NormalTextColor);
        SecondaryTextColor := GetValue(Node, 'SecondaryTextColor', SecondaryTextColor);
        FoundTextColor := GetValue(Node, 'FoundTextColor', FoundTextColor);
        UnselectableTextColor := GetValue(Node, 'UnselectableTextColor', UnselectableTextColor);
        CursorColor := GetValue(Node, 'CursorColor', CursorColor);
        ShortcutUnderCursor := GetValue(Node, 'ShortcutUnderCursor', ShortcutUnderCursor);
        NormalTextUnderCursor := GetValue(Node, 'NormalTextUnderCursor', NormalTextUnderCursor);
        SecondaryTextUnderCursor := GetValue(Node, 'SecondaryTextUnderCursor', SecondaryTextUnderCursor);
        FoundTextUnderCursor := GetValue(Node, 'FoundTextUnderCursor', FoundTextUnderCursor);
        UnselectableUnderCursor := GetValue(Node, 'UnselectableUnderCursor', UnselectableUnderCursor);
      end;
    end;
  end;
end;

procedure TColorThemes.Save(AConfig: TJSONObject);
var
  Index: Integer;
  Theme: TJSONObject;
  Themes: TJSONArray;
  Group: TJSONObject;
  ColorTheme: TColorTheme;
begin
  if AConfig.Find('Styles', Themes) then
    Themes.Clear
  else begin
    Themes:= TJSONArray.Create;
    AConfig.Add('Styles', Themes);
  end;

  for Index:= 0 to High(FColors) do
  begin
    ColorTheme:= FColors[Index];

    Theme:= TJSONObject.Create;
    Themes.Add(Theme);

    Theme.Add('Name', THEME_NAME[Index]);

    Group:= TJSONObject.Create;
    Theme.Add('FilePanel', Group);

    Group.Add('CursorBorderColor', ColorTheme.FilePanel.CursorBorderColor);
    Group.Add('ForeColor', ColorTheme.FilePanel.ForeColor);
    Group.Add('BackColor', ColorTheme.FilePanel.BackColor);
    Group.Add('BackColor2', ColorTheme.FilePanel.BackColor2);
    Group.Add('MarkColor', ColorTheme.FilePanel.MarkColor);
    Group.Add('CursorColor', ColorTheme.FilePanel.CursorColor);
    Group.Add('CursorText', ColorTheme.FilePanel.CursorText);
    Group.Add('InactiveCursorColor', ColorTheme.FilePanel.InactiveCursorColor);
    Group.Add('InactiveMarkColor', ColorTheme.FilePanel.InactiveMarkColor);

    Group:= TJSONObject.Create;
    Theme.Add('FreeSpaceIndicator', Group);

    Group.Add('ForeColor', ColorTheme.FreeSpaceInd.ForeColor);
    Group.Add('BackColor', ColorTheme.FreeSpaceInd.BackColor);
    Group.Add('ThresholdForeColor', ColorTheme.FreeSpaceInd.ThresholdForeColor);

    Group:= TJSONObject.Create;
    Theme.Add('Path', Group);

    Group.Add('ActiveColor', ColorTheme.Path.ActiveColor);
    Group.Add('ActiveFontColor', ColorTheme.Path.ActiveFontColor);
    Group.Add('InactiveColor', ColorTheme.Path.InactiveColor);
    Group.Add('InactiveFontColor', ColorTheme.Path.InactiveFontColor);

    Group:= TJSONObject.Create;
    Theme.Add('Log', Group);

    Group.Add('InfoColor', ColorTheme.Log.InfoColor);
    Group.Add('ErrorColor', ColorTheme.Log.ErrorColor);
    Group.Add('SuccessColor', ColorTheme.Log.SuccessColor);

    Group:= TJSONObject.Create;
    Theme.Add('SyncDirs', Group);

    Group.Add('LeftColor', ColorTheme.SyncDirs.LeftColor);
    Group.Add('RightColor', ColorTheme.SyncDirs.RightColor);
    Group.Add('UnknownColor', ColorTheme.SyncDirs.UnknownColor);

    Group:= TJSONObject.Create;
    Theme.Add('Viewer', Group);

    Group.Add('ImageBackColor1', ColorTheme.Viewer.ImageBackColor1);
    Group.Add('ImageBackColor2', ColorTheme.Viewer.ImageBackColor2);
    Group.Add('BookBackgroundColor', ColorTheme.Viewer.BookBackgroundColor);
    Group.Add('BookFontColor', ColorTheme.Viewer.BookFontColor);

    Group:= TJSONObject.Create;
    Theme.Add('Differ', Group);

    Group.Add('AddedColor', ColorTheme.Differ.AddedColor);
    Group.Add('DeletedColor', ColorTheme.Differ.DeletedColor);
    Group.Add('ModifiedColor', ColorTheme.Differ.ModifiedColor);
    Group.Add('ModifiedBinaryColor', ColorTheme.Differ.ModifiedBinaryColor);

    Group:= TJSONObject.Create;
    Theme.Add('TreeViewMenu', Group);

    Group.Add('BackgroundColor', ColorTheme.TreeViewMenu.BackgroundColor);
    Group.Add('ShortcutColor', ColorTheme.TreeViewMenu.ShortcutColor);
    Group.Add('NormalTextColor', ColorTheme.TreeViewMenu.NormalTextColor);
    Group.Add('SecondaryTextColor', ColorTheme.TreeViewMenu.SecondaryTextColor);
    Group.Add('FoundTextColor', ColorTheme.TreeViewMenu.FoundTextColor);
    Group.Add('UnselectableTextColor', ColorTheme.TreeViewMenu.UnselectableTextColor);
    Group.Add('CursorColor', ColorTheme.TreeViewMenu.CursorColor);
    Group.Add('ShortcutUnderCursor', ColorTheme.TreeViewMenu.ShortcutUnderCursor);
    Group.Add('NormalTextUnderCursor', ColorTheme.TreeViewMenu.NormalTextUnderCursor);
    Group.Add('SecondaryTextUnderCursor', ColorTheme.TreeViewMenu.SecondaryTextUnderCursor);
    Group.Add('FoundTextUnderCursor', ColorTheme.TreeViewMenu.FoundTextUnderCursor);
    Group.Add('UnselectableUnderCursor', ColorTheme.TreeViewMenu.UnselectableUnderCursor);
  end;
end;

procedure TColorThemes.Load(AConfig: TJSONObject);
var
  AName: String;
  Index: Integer;
  Theme: TJSONObject;
  Themes: TJSONArray;
  Group: TJSONObject;
  Empty: TJSONObject;
  ColorTheme: TColorTheme;
begin
  Themes:= AConfig.Get('Styles', TJSONArray(nil));

  if Assigned(Themes) then
  try
    Empty:= TJSONObject.Create;
    for Index:= 0 to Themes.Count - 1 do
    begin
      Theme:= Themes.Objects[Index];
      AName:= Theme.Get('Name', EmptyStr);

      ColorTheme:= GetTheme(AName);
      if (ColorTheme = nil) then Continue;

      Group:= Theme.Get('FilePanel', Empty);
      with ColorTheme.FilePanel do
      begin
        CursorBorderColor:= Group.Get('CursorBorderColor', CursorBorderColor);
        ForeColor:= Group.Get('ForeColor', ForeColor);
        BackColor:= Group.Get('BackColor', BackColor);
        BackColor2:= Group.Get('BackColor2', BackColor2);
        MarkColor:= Group.Get('MarkColor', MarkColor);
        CursorColor:= Group.Get('CursorColor', CursorColor);
        CursorText:= Group.Get('CursorText', CursorText);
        InactiveCursorColor:= Group.Get('InactiveCursorColor', InactiveCursorColor);
        InactiveMarkColor:= Group.Get('InactiveMarkColor', InactiveMarkColor);
      end;
      Group:= Theme.Get('FreeSpaceIndicator', Empty);
      with ColorTheme.FreeSpaceInd do
      begin
        ForeColor:= Group.Get('ForeColor', ForeColor);
        BackColor:= Group.Get('BackColor', BackColor);
        ThresholdForeColor:= Group.Get('ThresholdForeColor', ThresholdForeColor);
      end;
      Group:= Theme.Get('Path', Empty);
      with ColorTheme.Path do
      begin
        ActiveColor:= Group.Get('ActiveColor', ActiveColor);
        ActiveFontColor:= Group.Get('ActiveFontColor', ActiveFontColor);
        InactiveColor:= Group.Get('InactiveColor', InactiveColor);
        InactiveFontColor:= Group.Get('InactiveFontColor', InactiveFontColor);
      end;
      Group:= Theme.Get('Log', Empty);
      with ColorTheme.Log do
      begin
        InfoColor:= Group.Get('InfoColor', InfoColor);
        ErrorColor:= Group.Get('ErrorColor', ErrorColor);
        SuccessColor:= Group.Get('SuccessColor', SuccessColor);
      end;
      Group:= Theme.Get('SyncDirs', Empty);
      with ColorTheme.SyncDirs do
      begin
        LeftColor:= Group.Get('LeftColor', LeftColor);
        RightColor:= Group.Get('RightColor', RightColor);
        UnknownColor:= Group.Get('UnknownColor', UnknownColor);
      end;
      Group:= Theme.Get('Viewer', Empty);
      with ColorTheme.Viewer do
      begin
        ImageBackColor1:= Group.Get('ImageBackColor1', ImageBackColor1);
        ImageBackColor2:= Group.Get('ImageBackColor2', ImageBackColor2);
        BookBackgroundColor:= Group.Get('BookBackgroundColor', BookBackgroundColor);
        BookFontColor:= Group.Get('BookFontColor', BookFontColor);
      end;
      Group:= Theme.Get('Differ', Empty);
      with ColorTheme.Differ do
      begin
        AddedColor:= Group.Get('AddedColor', AddedColor);
        DeletedColor:= Group.Get('DeletedColor', DeletedColor);
        ModifiedColor:= Group.Get('ModifiedColor', ModifiedColor);
        ModifiedBinaryColor:= Group.Get('ModifiedBinaryColor', ModifiedBinaryColor);
      end;
      Group:= Theme.Get('TreeViewMenu', Empty);
      with ColorTheme.TreeViewMenu do
      begin
        BackgroundColor:= Group.Get('BackgroundColor', BackgroundColor);
        ShortcutColor:= Group.Get('ShortcutColor', ShortcutColor);
        NormalTextColor:= Group.Get('NormalTextColor', NormalTextColor);
        SecondaryTextColor:= Group.Get('SecondaryTextColor', SecondaryTextColor);
        FoundTextColor:= Group.Get('FoundTextColor', FoundTextColor);
        UnselectableTextColor:= Group.Get('UnselectableTextColor', UnselectableTextColor);
        CursorColor:= Group.Get('CursorColor', CursorColor);
        ShortcutUnderCursor:= Group.Get('ShortcutUnderCursor', ShortcutUnderCursor);
        NormalTextUnderCursor:= Group.Get('NormalTextUnderCursor', NormalTextUnderCursor);
        SecondaryTextUnderCursor:= Group.Get('SecondaryTextUnderCursor', SecondaryTextUnderCursor);
        FoundTextUnderCursor:= Group.Get('FoundTextUnderCursor', FoundTextUnderCursor);
        UnselectableUnderCursor:= Group.Get('UnselectableUnderCursor', UnselectableUnderCursor);
      end;
    end;
  finally
    Empty.Free;
  end;
end;

function TColorThemes.GetTheme(const AName: String): TColorTheme;
begin
  if (AName = LIGHT_THEME) then
    Result:= FColors[0]
  else if (AName = DARK_THEME) then
    Result:= FColors[1]
  else begin
    Result:= nil;
  end;
end;

function TColorThemes.Current: TColorTheme;
begin
  if DarkStyle then
    Result:= FColors[1]
  else begin
    Result:= FColors[0];
  end;
end;

end.


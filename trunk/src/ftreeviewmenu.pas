{
   Double Commander
   -------------------------------------------------------------------------
   Menu offered to user via a Tree View look where user might type sequence of letters

   Copyright (C) 2016-2020  Alexander Koblov (alexx2000@mail.ru)

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

unit fTreeViewMenu;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, Types,

  //DC
  kastoolitems, KASToolBar, uKASToolItemsExtended;

type
  // *IMPORTANT: "tvmcLASTONE" always must be the last one as it is used to give the number of element here.
  tvmContextMode = (tvmcHotDirectory, tvmcFavoriteTabs, tvmcDirHistory, tvmcViewHistory, tvmcKASToolBar, tvmcMainMenu, tvmcCommandLineHistory, tvmcFileSelectAssistant, tvmcLASTONE);

  TTreeViewMenuOptions = record
    CaseSensitive: boolean;
    IgnoreAccents: boolean;
    ShowWholeBranchIfMatch: boolean;
  end;

  { TTreeMenuItem }
  // In out TreeView, the "pointer" will actually point this type of element where the "FPointerSourceData" might actually point the actual vital items user actually choose.
  TTreeMenuItem = class
  private
    FPointerSourceData: Pointer;
    FTypeDispatcher: integer;
    FSecondaryText: string;
    FKeyboardShortcut: char;
  public
    constructor Create(PointerSourceData: Pointer);
    property PointerSourceData: Pointer read FPointerSourceData;
    property KeyboardShortcut: char read FKeyboardShortcut write FKeyboardShortcut;
    property SecondaryText: string read FSecondaryText write FSecondaryText;
    property TypeDispatcher: integer read FTypeDispatcher write FTypeDispatcher;
  end;

  { TTreeViewMenuGenericRoutineAndVarHolder }
  // Everything could have been placed into the "TfrmTreeViewMenu" form.
  // But this "sub-object" exists just to allow the configuration form to use the *same* routine to draw the tree so the test color could be tested this way.
  TTreeViewMenuGenericRoutineAndVarHolder = class(TObject)
  private
    FContextMode: tvmContextMode;
    FCaseSensitive: boolean;
    FIgnoreAccents: boolean;
    FShowWholeBranchIfMatch: boolean;
    FSearchingText: string;
    FShowShortcut: boolean;
    FMayStopOnNode: boolean;
    FBackgroundColor: TColor;
    FShortcutColor: TColor;
    FNormalTextColor: TColor;
    FSecondaryTextColor: TColor;
    FFoundTextColor: TColor;
    FUnselectableTextColor: TColor;
    FCursorColor: TColor;
    FShortcutUnderCursor: TColor;
    FNormalTextUnderCursor: TColor;
    FSecondaryTextUnderCursor: TColor;
    FFoundTextUnderCursor: TColor;
    FUnselectableUnderCursor: TColor;
  public
    function AddTreeViewMenuItem(ATreeView: TTreeView; ParentNode: TTreeNode; const S: string; const SecondaryText: string = ''; TypeDispatcher: integer = 0; Data: Pointer = nil): TTreeNode;
    procedure TreeViewMenuAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var {%H-}PaintImages, DefaultDraw: boolean);
    property ContextMode: tvmContextMode read FContextMode write FContextMode;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property IgnoreAccents: boolean read FIgnoreAccents write FIgnoreAccents;
    property ShowWholeBranchIfMatch: boolean read FShowWholeBranchIfMatch write FShowWholeBranchIfMatch;
    property SearchingText: string read FSearchingText write FSearchingText;
    property ShowShortcut: boolean read FShowShortcut write FShowShortcut;
    property MayStopOnNode: boolean read FMayStopOnNode write FMayStopOnNode;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property ShortcutColor: TColor read FShortcutColor write FShortcutColor;
    property NormalTextColor: TColor read FNormalTextColor write FNormalTextColor;
    property SecondaryTextColor: TColor read FSecondaryTextColor write FSecondaryTextColor;
    property FoundTextColor: TColor read FFoundTextColor write FFoundTextColor;
    property UnselectableTextColor: TColor read FUnselectableTextColor write FUnselectableTextColor;
    property CursorColor: TColor read FCursorColor write FCursorColor;
    property ShortcutUnderCursor: TColor read FShortcutUnderCursor write FShortcutUnderCursor;
    property NormalTextUnderCursor: TColor read FNormalTextUnderCursor write FNormalTextUnderCursor;
    property SecondaryTextUnderCursor: TColor read FSecondaryTextUnderCursor write FSecondaryTextUnderCursor;
    property FoundTextUnderCursor: TColor read FFoundTextUnderCursor write FFoundTextUnderCursor;
    property UnselectableUnderCursor: TColor read FUnselectableUnderCursor write FUnselectableUnderCursor;
  end;

  { TfrmTreeViewMenu }
  TfrmTreeViewMenu = class(TForm)
    pnlAll: TPanel;
    lblSearchingEntry: TLabel;
    edSearchingEntry: TEdit;
    tvMainMenu: TTreeView;
    tbOptions: TToolBar;
    tbCaseSensitive: TToolButton;
    tbIgnoreAccents: TToolButton;
    tbShowWholeBranchOrNot: TToolButton;
    tbDivider: TToolButton;
    tbFullExpandOrNot: TToolButton;
    tbClose: TToolBar;
    tbConfigurationTreeViewMenus: TToolButton;
    tbConfigurationTreeViewMenusColors: TToolButton;
    tbCancelAndQuit: TToolButton;
    pmCaseSensitiveOrNot: TPopupMenu;
    pmiCaseSensitive: TMenuItem;
    pmiNotCaseSensitive: TMenuItem;
    pmIgnoreAccentsOrNot: TPopupMenu;
    pmiIgnoreAccents: TMenuItem;
    pmiNotIgnoreAccents: TMenuItem;
    pmShowWholeBranchIfMatchOrNot: TPopupMenu;
    pmiShowWholeBranchIfMatch: TMenuItem;
    pmiNotShowWholeBranchIfMatch: TMenuItem;
    pmFullExpandOrNot: TPopupMenu;
    pmiFullExpand: TMenuItem;
    pmiFullCollapse: TMenuItem;
    imgListButton: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure tbCaseSensitiveClick(Sender: TObject);
    procedure pmiCaseSensitiveOrNotClick(Sender: TObject);
    procedure tbIgnoreAccentsClick(Sender: TObject);
    procedure pmiIgnoreAccentsOrNotClick(Sender: TObject);
    procedure tbShowWholeBranchOrNotClick(Sender: TObject);
    procedure pmiShowWholeBranchIfMatchOrNotClick(Sender: TObject);
    procedure tbFullExpandOrNotClick(Sender: TObject);
    procedure pmiFullExpandOrNotClick(Sender: TObject);
    procedure tbConfigurationTreeViewMenusClick(Sender: TObject);
    procedure tbConfigurationTreeViewMenusColorsClick(Sender: TObject);
    procedure tbCancelAndQuitClick(Sender: TObject);
    procedure edSearchingEntryChange(Sender: TObject);
    procedure tvMainMenuClick(Sender: TObject);
    procedure tvMainMenuDblClick(Sender: TObject);
    procedure tvMainMenuEnter(Sender: TObject);
    procedure tvMainMenuMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: integer);
    procedure tvMainMenuMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure tvMainMenuMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure tvMainMenuSelectionChanged(Sender: TObject);
    procedure tvMainMenuExpandOrCollapseClick(Sender: TObject; {%H-}Node: TTreeNode);
    function isAtLeastOneItemVisibleAndSelectable: boolean;
    procedure SelectNextVisibleItem;
    procedure SelectPreviousVisibleItem;
    procedure SelectFirstVisibleItem;
    procedure SelectLastVisibleItem;
    procedure SetShortcuts;
    function WasAbleToSelectShortCutLetter(SearchKey: char): boolean;
    function AttemptToExitWithCurrentSelection: boolean;
    procedure SetSizeToLargestElement;
  private
    { private declarations }
    bTargetFixedWidth: boolean;
    LastMousePos: TPoint;
  public
    { public declarations }
    iFinalSelectedIndex: integer;
    TreeViewMenuGenericRoutineAndVarHolder: TTreeViewMenuGenericRoutineAndVarHolder;
    procedure SetContextMode(WantedContextMode: tvmContextMode; WantedPosX, WantedPosY: integer; WantedWidth: integer = 0; WantedHeight: integer = 0);
    procedure HideUnmatchingNode;
  end;

// Actual routine called from the outside to help user to quickly select something using the "Tree View Menu" concept.
function GetUserChoiceFromTStrings(ATStrings: TStrings; ContextMode: tvmContextMode; WantedPosX, WantedPosY: integer; WantedWidth: integer = 0; WantedHeight: integer = 0): string;
function GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(pmAnyMenu: TMenu; ContextMode: tvmContextMode; WantedPosX, WantedPosY: integer; WantedWidth: integer = 0; WantedHeight: integer = 0): TMenuItem;
function GetUserChoiceFromKASToolBar(AKASToolBar: TKASToolBar; ContextMode: tvmContextMode; WantedPosX, WantedPosY, WantedWidth, WantedHeight: integer; var ReturnedTypeDispatcher: integer): Pointer;

var
  frmTreeViewMenu: TfrmTreeViewMenu;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LCLType, LCLIntf, LazUTF8,

  //DC
  uLng, fMain, uGlobs, uAccentsUtils;

const
  CONST_CANCEL_ACTION = -1;
  CONST_CONFIG_ACTION = -2;
  CONST_CONFIG_COLOR_ACTION = -3;

var
  sTreeViewMenuShortcutString: string = '0123456789abcdefghijklmnopqrstuvwxyz';

{ TTreeMenuItem.Create }
constructor TTreeMenuItem.Create(PointerSourceData: Pointer);
begin
  FPointerSourceData := PointerSourceData;
  FTypeDispatcher := 0;
  FSecondaryText := '';
  FKeyboardShortcut := ' ';
end;

{ TTreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem }
function TTreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(ATreeView: TTreeView; ParentNode: TTreeNode; const S: string; const SecondaryText: string = ''; TypeDispatcher: integer = 0; Data: Pointer = nil): TTreeNode;
var
  ATreeMenuItem: TTreeMenuItem;
begin
  ATreeMenuItem := TTreeMenuItem.Create(Data);
  ATreeMenuItem.TypeDispatcher := TypeDispatcher;
  ATreeMenuItem.KeyboardShortcut := ' ';
  ATreeMenuItem.SecondaryText := SecondaryText;
  Result := ATreeView.Items.AddChildObject(ParentNode, S, ATreeMenuItem);
end;

{ TTreeViewMenuGenericRoutineAndVarHolder.TreeViewMenuAdvancedCustomDrawItem }
procedure TTreeViewMenuGenericRoutineAndVarHolder.TreeViewMenuAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: boolean);
var
  NodeRect: TRect;
  sPart, sStringToShow: string;
  iRenduX: integer;
  iPosNormal: integer = 0;
  iMatchingLengthInSource: integer = 0;
  iTotalWidth: integer;
  local_TextColor: TColor;
  local_ShortcutColor: TColor;
  local_SecondaryTextColor: TColor;
  local_FoundTextColor: TColor;
begin
  if TCustomTreeView(Sender).BackgroundColor <> BackgroundColor then
    TCustomTreeView(Sender).BackgroundColor := BackgroundColor;
  if TCustomTreeView(Sender).Color <> BackgroundColor then
    TCustomTreeView(Sender).Color := BackgroundColor;

  if Stage = cdPostPaint then
  begin
    if Node <> nil then
    begin
      NodeRect := Node.DisplayRect(True);

      iTotalWidth := ((TCustomTreeView(Sender).Width - Node.DisplayTextLeft) - 25);
      NodeRect.Right := NodeRect.Left + iTotalWidth;

      if cdsSelected in State then
      begin
        // Draw something under selection.
        TTreeView(Sender).Canvas.Brush.Color := CursorColor;
        local_ShortcutColor := ShortcutUnderCursor;
        local_SecondaryTextColor := SecondaryTextUnderCursor;
        if (Node.Count = 0) or (FMayStopOnNode) then
          local_TextColor := NormalTextUnderCursor
        else
          local_TextColor := UnselectableUnderCursor;
        local_FoundTextColor := FoundTextUnderCursor;
      end
      else
      begin
        // Draw something unselected.
        TTreeView(Sender).Canvas.Brush.Color := BackgroundColor;
        local_ShortcutColor := ShortcutColor;
        local_SecondaryTextColor := SecondaryTextColor;
        if (Node.Count = 0) or (FMayStopOnNode) then
          local_TextColor := NormalTextColor
        else
          local_TextColor := UnselectableTextColor;
        local_FoundTextColor := FoundTextColor;
      end;
      TTreeView(Sender).Canvas.Brush.Style := bsSolid;
      TTreeView(Sender).Canvas.FillRect(NodeRect);

      TTreeView(Sender).Canvas.Brush.Style := bsClear;
      sStringToShow := Node.Text;
      iRenduX := NodeRect.Left + 3;

      // Short the shortcut name if config wants it AND if we have one to give.
      if (FShowShortcut) and (TTreeMenuItem(Node.Data).KeyboardShortcut <> ' ') then
      begin
        TTreeView(Sender).Canvas.Font.Color := local_ShortcutColor;
        sPart := '[' + TTreeMenuItem(Node.Data).KeyboardShortcut + '] ';
        TTreeView(Sender).Canvas.TextOut(iRenduX, NodeRect.Top + 1, sPart);
        iRenduX := iRenduX + TTreeView(Sender).Canvas.TextWidth(sPart);
      end;

      if (Node.Count = 0) or (FMayStopOnNode or ShowWholeBranchIfMatch) then
      begin
        while sStringToShow <> '' do
        begin
          iPosNormal := PosOfSubstrWithVersatileOptions(FSearchingText, sStringToShow, CaseSensitive, IgnoreAccents, iMatchingLengthInSource);

          if iPosNormal > 0 then
          begin
            if iPosNormal > 1 then
            begin
              // What we have in black prior the red...
              TTreeView(Sender).Canvas.Font.Color := local_TextColor;
              sPart := UTF8LeftStr(sStringToShow, pred(iPosNormal));
              TTreeView(Sender).Canvas.TextOut(iRenduX, NodeRect.Top + 1, sPart);
              iRenduX := iRenduX + TTreeView(Sender).Canvas.TextWidth(sPart);
              sStringToShow := UTF8RightStr(sStringToShow, ((UTF8Length(sStringToShow) - iPosNormal) + 1));
            end;

            // What we have in red...
            TTreeView(Sender).Canvas.Font.Style := TTreeView(Sender).Canvas.Font.Style + [fsUnderline, fsBold];
            TTreeView(Sender).Canvas.Font.Color := local_FoundTextColor;
            sPart := UTF8Copy(sStringToShow, 1, iMatchingLengthInSource);
            TTreeView(Sender).Canvas.TextOut(iRenduX, NodeRect.Top + 1, sPart);
            iRenduX := iRenduX + TTreeView(Sender).Canvas.TextWidth(sPart);
            TTreeView(Sender).Canvas.Font.Style := TTreeView(Sender).Canvas.Font.Style - [fsUnderline, fsBold];
            sStringToShow := UTF8RightStr(sStringToShow, ((UTF8Length(sStringToShow) - iMatchingLengthInSource)));
          end
          else
          begin
            TTreeView(Sender).Canvas.Font.Color := local_TextColor;
            TTreeView(Sender).Canvas.TextOut(iRenduX, NodeRect.Top + 1, sStringToShow);
            iRenduX := iRenduX + TTreeView(Sender).Canvas.TextWidth(sStringToShow);
            sStringToShow := '';
          end;
        end;
      end
      else
      begin
        TTreeView(Sender).Canvas.Font.Color := local_TextColor;
        TTreeView(Sender).Canvas.TextOut(iRenduX, NodeRect.Top + 1, sStringToShow);
        iRenduX := iRenduX + TTreeView(Sender).Canvas.TextWidth(sStringToShow);
      end;

      if TTreeMenuItem(Node.Data).SecondaryText <> '' then
      begin
        TTreeView(Sender).Canvas.Font.Color := local_SecondaryTextColor;
        TTreeView(Sender).Canvas.Font.Style := TTreeView(Sender).Canvas.Font.Style + [fsItalic];
        TTreeView(Sender).Canvas.TextOut(iRenduX + 4, NodeRect.Top + 1 + 1, TTreeMenuItem(Node.Data).SecondaryText);
        //If we ever add something else after one day: iRenduX := iRenduX+4 + TTreeView(Sender).Canvas.TextWidth(TTreeMenuItem(Node.Data).SecondaryText);
        TTreeView(Sender).Canvas.Font.Style := TTreeView(Sender).Canvas.Font.Style - [fsItalic];
      end;

      DefaultDraw := False;
    end;
  end;
end;

{ TfrmTreeViewMenu.FormCreate }
procedure TfrmTreeViewMenu.FormCreate(Sender: TObject);
begin
  bTargetFixedWidth := False;
  LastMousePos.x := -1;
  LastMousePos.y := -1;
  iFinalSelectedIndex := CONST_CANCEL_ACTION;
  FontOptionsToFont(gFonts[dcfTreeViewMenu], tvMainMenu.Font);
  TreeViewMenuGenericRoutineAndVarHolder := TTreeViewMenuGenericRoutineAndVarHolder.Create;
  TreeViewMenuGenericRoutineAndVarHolder.BackgroundColor := gTVMBackgroundColor;
  TreeViewMenuGenericRoutineAndVarHolder.ShortcutColor := gTVMShortcutColor;
  TreeViewMenuGenericRoutineAndVarHolder.NormalTextColor := gTVMNormalTextColor;
  TreeViewMenuGenericRoutineAndVarHolder.SecondaryTextColor := gTVMSecondaryTextColor;
  TreeViewMenuGenericRoutineAndVarHolder.FoundTextColor := gTVMFoundTextColor;
  TreeViewMenuGenericRoutineAndVarHolder.UnselectableTextColor := gTVMUnselectableTextColor;
  TreeViewMenuGenericRoutineAndVarHolder.CursorColor := gTVMCursorColor;
  TreeViewMenuGenericRoutineAndVarHolder.ShortcutUnderCursor := gTVMShortcutUnderCursor;
  TreeViewMenuGenericRoutineAndVarHolder.NormalTextUnderCursor := gTVMNormalTextUnderCursor;
  TreeViewMenuGenericRoutineAndVarHolder.SecondaryTextUnderCursor := gTVMSecondaryTextUnderCursor;
  TreeViewMenuGenericRoutineAndVarHolder.FoundTextUnderCursor := gTVMFoundTextUnderCursor;
  TreeViewMenuGenericRoutineAndVarHolder.UnselectableUnderCursor := gTVMUnselectableUnderCursor;
  tvMainMenu.BackgroundColor := gTVMBackgroundColor;
  tvMainMenu.Color := gTVMBackgroundColor;
  tvMainMenu.OnAdvancedCustomDrawItem := @TreeViewMenuGenericRoutineAndVarHolder.TreeViewMenuAdvancedCustomDrawItem;
  edSearchingEntryChange(nil);
end;

{ TfrmTreeViewMenu.FormClose }
procedure TfrmTreeViewMenu.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  case iFinalSelectedIndex of
    CONST_CANCEL_ACTION: ModalResult := mrCancel;
    CONST_CONFIG_ACTION: ModalResult := mrYes;
    CONST_CONFIG_COLOR_ACTION: ModalResult := mrAll;
    else
      ModalResult := mrOk;
  end;
end;

{ TfrmTreeViewMenu.FormCloseQuery }
procedure TfrmTreeViewMenu.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  tvMainMenu.OnExpanded := nil;
  tvMainMenu.OnCollapsed := nil;
  tvMainMenu.OnSelectionChanged := nil;
  Application.ProcessMessages;

  //We saved our options. We're aware it will save it even if user CANCEL the action but after a few test, the author of these lines feels it is better this way.
  gTreeViewMenuOptions[Ord(TreeViewMenuGenericRoutineAndVarHolder.ContextMode)].CaseSensitive := TreeViewMenuGenericRoutineAndVarHolder.CaseSensitive;
  gTreeViewMenuOptions[Ord(TreeViewMenuGenericRoutineAndVarHolder.ContextMode)].IgnoreAccents := TreeViewMenuGenericRoutineAndVarHolder.IgnoreAccents;
  gTreeViewMenuOptions[Ord(TreeViewMenuGenericRoutineAndVarHolder.ContextMode)].ShowWholeBranchIfMatch := TreeViewMenuGenericRoutineAndVarHolder.ShowWholeBranchIfMatch;
end;

{ TfrmTreeViewMenu.FormDestroy }
procedure TfrmTreeViewMenu.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TreeViewMenuGenericRoutineAndVarHolder);
  inherited;
end;

{ TfrmTreeViewMenu.FormKeyDown }
procedure TfrmTreeViewMenu.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ChoiceNode: TTreeNode;
begin
  if edSearchingEntry.Focused then
  begin
    case Key of
      VK_HOME: // Home Key
      begin
        if SSCTRL in Shift then
        begin
          SelectFirstVisibleItem;
          Key := 0;
        end;
      end;

      VK_END: // End Key
      begin
        if SSCTRL in Shift then
        begin
          SelectLastVisibleItem;
          Key := 0;
        end;
      end;
    end;
  end;

  if ssALT in Shift then
  begin
    case Key of
      VK_0..VK_9, VK_A..VK_Z: if WasAbleToSelectShortCutLetter(char(Key)) then
          Key := 0;
    end;

    if (Key = 0) and gTreeViewMenuShortcutExit then
      AttemptToExitWithCurrentSelection;
  end;

  case Key of
    VK_UP: // Up Arrow Key
    begin
      SelectPreviousVisibleItem;
      Key := 0;
    end;

    VK_DOWN: // Down Arrow Key
    begin
      SelectNextVisibleItem;
      Key := 0;
    end;

    VK_END: // End Key - Let's play tricky: if cursor is at the end into the edit box, let's assume user pressed the "end" key to go to the end in the list.
    begin
      if edSearchingEntry.SelStart >= utf8Length(edSearchingEntry.Text) then
      begin
        SelectLastVisibleItem;
        Key := 0;
      end;
    end;

    VK_HOME: // Home Key - Let's play tricky: if cursor is at the beginning into the edit box, let's assume user pressed the "home" key to go to the first in the list.
    begin
      if edSearchingEntry.SelStart = 0 then
      begin
        SelectFirstVisibleItem;
        Key := 0;
      end;
    end;

    VK_RETURN: // Enter key
    begin
      ChoiceNode := tvMainMenu.Selected;
      if ChoiceNode <> nil then
      begin
        if (TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode) or (ChoiceNode.Count = 0) then
        begin
          Key := 0;
          AttemptToExitWithCurrentSelection;
        end;
      end;
    end;

    VK_ESCAPE: // Escape key
    begin
      Key := 0;
      Close;
    end;
  end;
end;

{ TfrmTreeViewMenu.tbCaseSensitiveClick }
procedure TfrmTreeViewMenu.tbCaseSensitiveClick(Sender: TObject);
var
  pmiToSwitchTo: TMenuItem = nil;
begin
  if pmiNotCaseSensitive.Checked then  pmiToSwitchTo := pmiCaseSensitive
  else if pmiCaseSensitive.Checked then  pmiToSwitchTo := pmiNotCaseSensitive;
  if pmiToSwitchTo <> nil then
  begin
    pmiToSwitchTo.Checked := True;
    pmiCaseSensitiveOrNotClick(pmiToSwitchTo);
  end;
end;

{ TfrmTreeViewMenu.pmiCaseSensitiveOrNotClick }
procedure TfrmTreeViewMenu.pmiCaseSensitiveOrNotClick(Sender: TObject);
begin
  begin
    with Sender as TMenuItem do
    begin
      tbCaseSensitive.ImageIndex := ImageIndex;
      tbCaseSensitive.Hint := Caption;
    end;
    edSearchingEntryChange(edSearchingEntry);
  end;
end;

{ TfrmTreeViewMenu.tbIgnoreAccentsClick }
procedure TfrmTreeViewMenu.tbIgnoreAccentsClick(Sender: TObject);
var
  pmiToSwitchTo: TMenuItem = nil;
begin
  if pmiIgnoreAccents.Checked then  pmiToSwitchTo := pmiNotIgnoreAccents
  else if pmiNotIgnoreAccents.Checked then  pmiToSwitchTo := pmiIgnoreAccents;
  if pmiToSwitchTo <> nil then
  begin
    pmiToSwitchTo.Checked := True;
    pmiIgnoreAccentsOrNotClick(pmiToSwitchTo);
  end;
end;

{ TfrmTreeViewMenu.pmiIgnoreAccentsOrNotClick }
procedure TfrmTreeViewMenu.pmiIgnoreAccentsOrNotClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    tbIgnoreAccents.ImageIndex := ImageIndex;
    tbIgnoreAccents.Hint := Caption;
  end;
  edSearchingEntryChange(edSearchingEntry);
end;

{ TfrmTreeViewMenu.tbShowWholeBranchOrNotClick }
procedure TfrmTreeViewMenu.tbShowWholeBranchOrNotClick(Sender: TObject);
var
  pmiToSwitchTo: TMenuItem = nil;
begin
  if pmiShowWholeBranchIfMatch.Checked then  pmiToSwitchTo := pmiNotShowWholeBranchIfMatch
  else if pmiNotShowWholeBranchIfMatch.Checked then  pmiToSwitchTo := pmiShowWholeBranchIfMatch;
  if pmiToSwitchTo <> nil then
  begin
    pmiToSwitchTo.Checked := True;
    pmiShowWholeBranchIfMatchOrNotClick(pmiToSwitchTo);
  end;
end;

{ TfrmTreeViewMenu.pmiShowWholeBranchIfMatchOrNotClick }
procedure TfrmTreeViewMenu.pmiShowWholeBranchIfMatchOrNotClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    tbShowWholeBranchOrNot.ImageIndex := ImageIndex;
    tbShowWholeBranchOrNot.Hint := Caption;
  end;
  edSearchingEntryChange(edSearchingEntry);
end;

{ TfrmTreeViewMenu.tbFullExpandOrNotClick }
procedure TfrmTreeViewMenu.tbFullExpandOrNotClick(Sender: TObject);
var
  pmiToSwitchTo: TMenuItem = nil;
begin
  if pmiFullExpand.Checked then  pmiToSwitchTo := pmiFullCollapse
  else if pmiFullCollapse.Checked then  pmiToSwitchTo := pmiFullExpand;
  if pmiToSwitchTo <> nil then
  begin
    pmiToSwitchTo.Checked := True;
    pmiFullExpandOrNotClick(pmiToSwitchTo);
  end;
end;

{ TfrmTreeViewMenu.pmiFullExpandOrNotClick }
procedure TfrmTreeViewMenu.pmiFullExpandOrNotClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    tbFullExpandOrNot.ImageIndex := ImageIndex;
    tbFullExpandOrNot.Hint := Caption;
  end;
  if pmiFullExpand.Checked then
    tvMainMenu.FullExpand
  else
    tvMainMenu.FullCollapse;
end;

{ TfrmTreeViewMenu.tbConfigurationTreeViewMenusClick }
procedure TfrmTreeViewMenu.tbConfigurationTreeViewMenusClick(Sender: TObject);
begin
  iFinalSelectedIndex := CONST_CONFIG_ACTION;
  Close;
end;

{ TfrmTreeViewMenu.tbConfigurationTreeViewMenusColorsClick }
procedure TfrmTreeViewMenu.tbConfigurationTreeViewMenusColorsClick(Sender: TObject);
begin
  iFinalSelectedIndex := CONST_CONFIG_COLOR_ACTION;
  Close;
end;

{ TfrmTreeViewMenu.tbCancelAndQuitClick }
procedure TfrmTreeViewMenu.tbCancelAndQuitClick(Sender: TObject);
begin
  Close;
end;

{ TfrmTreeViewMenu.edSearchingEntryChange }
procedure TfrmTreeViewMenu.edSearchingEntryChange(Sender: TObject);
begin
  TreeViewMenuGenericRoutineAndVarHolder.CaseSensitive := pmiCaseSensitive.Checked;
  TreeViewMenuGenericRoutineAndVarHolder.IgnoreAccents := pmiIgnoreAccents.Checked;
  TreeViewMenuGenericRoutineAndVarHolder.ShowWholeBranchIfMatch := pmiShowWholeBranchIfMatch.Checked;
  TreeViewMenuGenericRoutineAndVarHolder.SearchingText := edSearchingEntry.Text;
  TreeViewMenuGenericRoutineAndVarHolder.ShowShortcut := gTreeViewMenuUseKeyboardShortcut;
  if pmiIgnoreAccents.Checked then  TreeViewMenuGenericRoutineAndVarHolder.SearchingText := NormalizeAccentedChar(TreeViewMenuGenericRoutineAndVarHolder.SearchingText);
  if not pmiCaseSensitive.Checked then  TreeViewMenuGenericRoutineAndVarHolder.SearchingText := UTF8UpperCase(TreeViewMenuGenericRoutineAndVarHolder.SearchingText);
  HideUnmatchingNode;
end;

{ TfrmTreeViewMenu.tvMainMenuClick }
procedure TfrmTreeViewMenu.tvMainMenuClick(Sender: TObject);
begin
  if gTreeViewMenuSingleClickExit then  AttemptToExitWithCurrentSelection;
end;

{ TfrmTreeViewMenu.tvMainMenuDblClick }
procedure TfrmTreeViewMenu.tvMainMenuDblClick(Sender: TObject);
begin
  if gTreeViewMenuDoubleClickExit then  AttemptToExitWithCurrentSelection;
end;

{ TfrmTreeViewMenu.tvMainMenuEnter }
procedure TfrmTreeViewMenu.tvMainMenuEnter(Sender: TObject);
begin
  if edSearchingEntry.CanFocus then  edSearchingEntry.SetFocus;
end;

{ TfrmTreeViewMenu.tvMainMenuMouseMove }
procedure TfrmTreeViewMenu.tvMainMenuMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ANode: TTreeNode;
begin
  if (LastMousePos.x <> -1) and (LastMousePos.y <> -1) then
  begin
    ANode := tvMainMenu.GetNodeAt(X, Y);

    if ANode <> nil then
      if not ANode.Selected then
        ANode.Selected := True;
  end;

  LastMousePos.x := X;
  LastMousePos.y := Y;
end;

{ TfrmTreeViewMenu.tvMainMenuMouseWheelDown }
procedure TfrmTreeViewMenu.tvMainMenuMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) and (gFonts[dcfTreeViewMenu].Size > gFonts[dcfTreeViewMenu].MinValue) then
  begin
    dec(gFonts[dcfTreeViewMenu].Size);
    tvMainMenu.Font.Size := gFonts[dcfTreeViewMenu].Size;
    Handled := True;
  end;
end;

{ TfrmTreeViewMenu.tvMainMenuMouseWheelUp }
procedure TfrmTreeViewMenu.tvMainMenuMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssCtrl]) and (gFonts[dcfTreeViewMenu].Size < gFonts[dcfTreeViewMenu].MaxValue) then
  begin
    inc(gFonts[dcfTreeViewMenu].Size);
    tvMainMenu.Font.Size := gFonts[dcfTreeViewMenu].Size;
    Handled := True;
  end;
end;

{ TfrmTreeViewMenu.tvMainMenuSelectionChanged }
procedure TfrmTreeViewMenu.tvMainMenuSelectionChanged(Sender: TObject);
begin
  tvMainMenu.BeginUpdate;
  SetShortcuts;
  tvMainMenu.EndUpdate;
end;

{ TfrmTreeViewMenu.ExpandOrCollapseClick }
procedure TfrmTreeViewMenu.tvMainMenuExpandOrCollapseClick(Sender: TObject; Node: TTreeNode);
begin
  if edSearchingEntry.Text = '' then
  begin
    tvMainMenu.BeginUpdate;
    SetShortcuts;
    tvMainMenu.EndUpdate;
  end;
end;

{ TfrmTreeViewMenu.isAtLeastOneItemVisibleAndSelectable }
function TfrmTreeViewMenu.isAtLeastOneItemVisibleAndSelectable: boolean;
var
  iSearchIndex: integer;
begin
  Result := False;
  if tvMainMenu.Items.Count > 0 then
  begin
    iSearchIndex := 0;
    while (not Result) and (iSearchIndex < tvMainMenu.Items.Count) do
    begin
      if tvMainMenu.Items[iSearchIndex].Visible then
        Result := ((tvMainMenu.Items[iSearchIndex].Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode);
      Inc(iSearchIndeX);
    end;
  end;
end;

{ TfrmTreeViewMenu.SelectNextVisibleItem }
procedure TfrmTreeViewMenu.SelectNextVisibleItem;
var
  iCurrentIndex: integer;
begin
  if isAtLeastOneItemVisibleAndSelectable then
  begin
    if tvMainMenu.Selected = nil then
      iCurrentIndex := -1
    else
      iCurrentIndex := tvMainMenu.Selected.AbsoluteIndex;
    begin
      repeat
        iCurrentIndex := ((iCurrentIndex + 1) mod tvMainMenu.Items.Count);
      until (tvMainMenu.Items[iCurrentIndex].Visible and ((tvMainMenu.Items[iCurrentIndex].Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode));
      tvMainMenu.Items[iCurrentIndex].Selected := True;
    end;
  end;
end;

{ TfrmTreeViewMenu.SelectPreviousVisibleItem }
procedure TfrmTreeViewMenu.SelectPreviousVisibleItem;
var
  iCurrentIndex: integer;
begin
  if isAtLeastOneItemVisibleAndSelectable then
  begin
    if tvMainMenu.Selected = nil then
      iCurrentIndex := -1
    else
      iCurrentIndex := tvMainMenu.Selected.AbsoluteIndex;
    begin
      repeat
        if iCurrentIndex = 0 then
          iCurrentIndex := pred(tvMainMenu.Items.Count)
        else
          Dec(iCurrentIndex);
      until (tvMainMenu.Items[iCurrentIndex].Visible and ((tvMainMenu.Items[iCurrentIndex].Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode));
      tvMainMenu.Items[iCurrentIndex].Selected := True;
    end;
  end;
end;

{ TfrmTreeViewMenu.SelectFirstVisibleItem }
procedure TfrmTreeViewMenu.SelectFirstVisibleItem;
var
  iCurrentIndex: integer;
begin
  if isAtLeastOneItemVisibleAndSelectable then
  begin
    iCurrentIndex := -1;
    repeat
      iCurrentIndex := ((iCurrentIndex + 1) mod tvMainMenu.Items.Count);
    until (tvMainMenu.Items[iCurrentIndex].Visible and ((tvMainMenu.Items[iCurrentIndex].Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode));
    tvMainMenu.Items[iCurrentIndex].Selected := True;
  end;
end;

{ TfrmTreeViewMenu.SelectLastVisibleItem }
procedure TfrmTreeViewMenu.SelectLastVisibleItem;
var
  iCurrentIndex: integer;
begin
  if isAtLeastOneItemVisibleAndSelectable then
  begin
    iCurrentIndex := tvMainMenu.Items.Count;
    repeat
      if iCurrentIndex = 0 then
        iCurrentIndex := pred(tvMainMenu.Items.Count)
      else
        Dec(iCurrentIndex);
    until (tvMainMenu.Items[iCurrentIndex].Visible and ((tvMainMenu.Items[iCurrentIndex].Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode));
    tvMainMenu.Items[iCurrentIndex].Selected := True;
  end;
end;

{ TfrmTreeViewMenu.SetShortcuts }
procedure TfrmTreeViewMenu.SetShortcuts;
var
  iCurrentShortcut: integer = 1;

  function GetCurrentShortcutLetter: char;
  begin
    if iCurrentShortcut > 0 then
    begin
      Result := sTreeViewMenuShortcutString[iCurrentShortcut];
      Inc(iCurrentShortcut);
      if iCurrentShortcut > length(sTreeViewMenuShortcutString) then
        iCurrentShortcut := 0;
    end
    else
    begin
      Result := ' ';
    end;
  end;

  function GetShortcutLetterForThisNode(paramNode: TTreeNode): char;
  begin
    Result := ' ';
    if paramNode.Visible then
      if (paramNode.Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode then
        Result := GetCurrentShortcutLetter;
  end;

var
  iNode, iNbOfVisibleNode: integer;
  ANode: TTreeNode;
begin
  for iNode := 0 to pred(tvMainMenu.Items.Count) do
    TTreeMenuItem(tvMainMenu.Items[iNode].Data).KeyboardShortcut := ' ';

  iNbOfVisibleNode := tvMainMenu.Height div tvMainMenu.DefaultItemHeight;
  iNode := 0;
  while iNode < iNbOfVisibleNode do
  begin
    ANode := tvMainMenu.GetNodeAt(100, (iNode * tvMainMenu.DefaultItemHeight));
    if ANode <> nil then
    begin
      if (ANode.Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode then
        TTreeMenuItem(ANode.Data).KeyboardShortcut :=
          GetShortcutLetterForThisNode(ANode);
    end;
    Inc(iNode);
  end;
end;

{ TfrmTreeViewMenu.WasAbleToSelectShortCutLetter }
function TfrmTreeViewMenu.WasAbleToSelectShortCutLetter(SearchKey: char): boolean;
var
  iSearchIndex: integer;
begin
  Result := False;
  if tvMainMenu.Items.Count > 0 then
  begin
    iSearchIndex := 0;
    while (not Result) and (iSearchIndex < tvMainMenu.Items.Count) do
    begin
      if (LowerCase(TTreeMenuItem(tvMainMenu.Items[iSearchIndex].Data).KeyboardShortcut) = LowerCase(SearchKey)) and (tvMainMenu.Items[iSearchIndex].Visible) then
        Result := True
      else
        Inc(iSearchIndeX);
    end;
  end;
  if Result then
    tvMainMenu.Items[iSearchIndex].Selected := True;
end;

{ TfrmTreeViewMenu.AttemptToExitWithCurrentSelection }
function TfrmTreeViewMenu.AttemptToExitWithCurrentSelection: boolean;
begin
  Result := False;
  if tvMainMenu.Selected <> nil then
    if (TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode) or (tvMainMenu.Selected.Count = 0) then
    begin
      Result := True;
      iFinalSelectedIndex := tvMainMenu.Selected.AbsoluteIndex;
      Close;
    end;
end;

{ TfrmTreeViewMenu.SetSizeToLargestElement }
procedure TfrmTreeViewMenu.SetSizeToLargestElement;
var
  iNode, iLargest: integer;
begin
  iLargest := 0;
  for iNode := 0 to pred(tvMainMenu.Items.Count) do
    if tvMainMenu.Items[iNode].DisplayRect(True).Right > iLargest then
      iLargest := tvMainMenu.Items[iNode].DisplayRect(True).Right;

  Width := iLargest + 50;
end;

{ TfrmTreeViewMenu.SetContextMode }
procedure TfrmTreeViewMenu.SetContextMode(WantedContextMode: tvmContextMode; WantedPosX, WantedPosY: integer; WantedWidth: integer = 0; WantedHeight: integer = 0);
var
  pmiToSwitchTo: TMenuItem = nil;
begin
  TreeViewMenuGenericRoutineAndVarHolder.ContextMode := WantedContextMode;

  // Let's set our option checked menu item AND our internal options according to settings saved previously for that context.
  if gTreeViewMenuOptions[Ord(TreeViewMenuGenericRoutineAndVarHolder.ContextMode)].CaseSensitive then  pmiToSwitchTo := pmiCaseSensitive  else pmiToSwitchTo := pmiNotCaseSensitive;
  pmiToSwitchTo.Checked := True;
  pmiCaseSensitiveOrNotClick(pmiToSwitchTo);
  if gTreeViewMenuOptions[Ord(TreeViewMenuGenericRoutineAndVarHolder.ContextMode)].IgnoreAccents then  pmiToSwitchTo := pmiIgnoreAccents  else pmiToSwitchTo := pmiNotIgnoreAccents;
  pmiToSwitchTo.Checked := True;
  pmiIgnoreAccentsOrNotClick(pmiToSwitchTo);
  if gTreeViewMenuOptions[Ord(TreeViewMenuGenericRoutineAndVarHolder.ContextMode)].ShowWholeBranchIfMatch then  pmiToSwitchTo := pmiShowWholeBranchIfMatch  else pmiToSwitchTo := pmiNotShowWholeBranchIfMatch;
  pmiToSwitchTo.Checked := True;
  pmiShowWholeBranchIfMatchOrNotClick(pmiToSwitchTo);

  // We set the appropriate title to give feedback to user.
  case TreeViewMenuGenericRoutineAndVarHolder.ContextMode of
    tvmcHotDirectory: lblSearchingEntry.Caption := rsStrTVMChooseHotDirectory;
    tvmcFavoriteTabs: lblSearchingEntry.Caption := rsStrTVMChooseFavoriteTabs;
    tvmcDirHistory: lblSearchingEntry.Caption := rsStrTVMChooseDirHistory;
    tvmcViewHistory: lblSearchingEntry.Caption := rsStrTVMChooseViewHistory;
    tvmcKASToolBar: lblSearchingEntry.Caption := rsStrTVMChooseFromToolbar;
    tvmcMainMenu: lblSearchingEntry.Caption := rsStrTVMChooseFromMainMenu;
    tvmcCommandLineHistory: lblSearchingEntry.Caption := rsStrTVMChooseFromCmdLineHistory;
    tvmcFileSelectAssistant: lblSearchingEntry.Caption := rsStrTVMChooseYourFileOrDir;
    else
      raise Exception.Create(rsMsgUnexpectedUsageTreeViewMenu);
  end;

  // We set the "look and feel" of the form for the user.
  case TreeViewMenuGenericRoutineAndVarHolder.ContextMode of
    tvmcHotDirectory, tvmcFavoriteTabs, tvmcKASToolBar, tvmcMainMenu: TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode := False;
    tvmcDirHistory, tvmcViewHistory, tvmcCommandLineHistory: TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode := False;
    tvmcFileSelectAssistant: TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode := True; // But on first revision, won't happen
    else
      raise Exception.Create(rsMsgUnexpectedUsageTreeViewMenu);
  end;

  case TreeViewMenuGenericRoutineAndVarHolder.ContextMode of
    tvmcHotDirectory, tvmcFavoriteTabs, tvmcDirHistory, tvmcViewHistory,
    tvmcKASToolBar, tvmcMainMenu, tvmcCommandLineHistory, tvmcFileSelectAssistant:
    begin
      Left := WantedPosX;
      Top := WantedPosY;
      if WantedHeight <> 0 then  Height := WantedHeight;
      if (WantedWidth <> 0) and (WantedHeight <> 0) then
      begin
        bTargetFixedWidth := True;
        Width := WantedWidth;
      end;
      BorderStyle := bsNone;
    end;
    else
    begin
      raise Exception.Create(rsMsgUnexpectedUsageTreeViewMenu);
    end;
  end;
end;

{ TfrmTreeViewMenu.HideUnmatchingNode }
// The *key* routine off all this.
// Routine will make visible in tree view the items that match with what the user has typed.
// Eliminating from the view the non matching item helps user to quickly see what he was looking for.
// So choosing it through a lot of data speed up things.
procedure TfrmTreeViewMenu.HideUnmatchingNode;
var
  iDummy: integer = 0;
  iNode: integer;
  nFirstMatchingNode: TTreeNode = nil;

  //WARNING: The following procedure is recursive and so may call itself back!
  procedure KeepMeThisWholeBranch(paramNode: TTreeNode);
  begin
    while paramNode <> nil do
    begin
      paramNode.Visible := True;
      if paramNode.Count > 0 then  KeepMeThisWholeBranch(paramNode.Items[0]);
      paramNode := paramNode.GetNextSibling;
    end;
  end;

  //WARNING: The following procedure is recursive and so may call itself back!
  function UpdateVisibilityAccordingToSearchingString(paramNode: TTreeNode): boolean;
  begin
    Result := False;

    while paramNode <> nil do
    begin
      if paramNode.Count = 0 then
      begin
        paramNode.Visible := (PosOfSubstrWithVersatileOptions(TreeViewMenuGenericRoutineAndVarHolder.SearchingText, paramNode.Text, pmiCaseSensitive.Checked, pmiIgnoreAccents.Checked, iDummy) <> 0);
      end
      else
      begin
        if pmiShowWholeBranchIfMatch.Checked then
        begin
          paramNode.Visible := (PosOfSubstrWithVersatileOptions(TreeViewMenuGenericRoutineAndVarHolder.SearchingText, paramNode.Text, pmiCaseSensitive.Checked, pmiIgnoreAccents.Checked, iDummy) <> 0);
          if paramNode.Visible then
          begin
            KeepMeThisWholeBranch(paramNode);
          end
          else
          begin
            paramNode.Visible := UpdateVisibilityAccordingToSearchingString(paramNode.Items[0]);
          end;
        end
        else
        begin
          paramNode.Visible := UpdateVisibilityAccordingToSearchingString(paramNode.Items[0]);
          if not paramNode.Visible then
          begin
            if TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode then
              paramNode.Visible := (PosOfSubstrWithVersatileOptions(TreeViewMenuGenericRoutineAndVarHolder.SearchingText, paramNode.Text, pmiCaseSensitive.Checked, pmiIgnoreAccents.Checked, iDummy) <> 0);
          end;
        end;
      end;

      if paramNode.Visible then
      begin
        Result := True;
        if nFirstMatchingNode = nil then
          if (paramNode.Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode then
            nFirstMatchingNode := paramNode;
      end;

      paramNode := paramNode.GetNextSibling;
    end;

  end;

begin
  tbFullExpandOrNot.Visible := (TreeViewMenuGenericRoutineAndVarHolder.SearchingText = '');

  if tvMainMenu.Items.Count > 0 then
  begin
    tvMainMenu.BeginUpdate;
    try
      if TreeViewMenuGenericRoutineAndVarHolder.SearchingText <> '' then
      begin
        UpdateVisibilityAccordingToSearchingString(tvMainMenu.Items.Item[0]);
      end
      else
      begin
        for iNode := 0 to pred(tvMainMenu.Items.Count) do
        begin
          tvMainMenu.Items.Item[iNode].Visible := True;
          if nFirstMatchingNode = nil then
            if (tvMainMenu.Items.Item[iNode].Count = 0) or TreeViewMenuGenericRoutineAndVarHolder.MayStopOnNode then
              nFirstMatchingNode := tvMainMenu.Items.Item[iNode];
        end;
      end;

      if TreeViewMenuGenericRoutineAndVarHolder.SearchingText <> '' then
      begin
        for iNode := pred(tvMainMenu.Items.Count) downto 0 do
          tvMainMenu.Items.Item[iNode].MakeVisible;
      end
      else
      begin
        pmiFullExpand.Checked := True;
        pmiFullExpandOrNotClick(pmiFullExpand);
      end;

      // It might happen we hit no direct found BUT we're still displaying item because of branch name matching. If so, let's select the first item of a branch matching name.
      if nFirstMatchingNode=nil then
      begin
        iNode:=0;
        while (iNode<tvMainMenu.Items.Count) AND (nFirstMatchingNode=nil) do
          if (tvMainMenu.Items[iNode].Visible) AND (tvMainMenu.Items[iNode].Count=0) then
            nFirstMatchingNode:=tvMainMenu.Items[iNode]
          else
            inc(iNode);
      end;

      if nFirstMatchingNode <> nil then
        nFirstMatchingNode.Selected := True;

      SetShortcuts;
    finally
      tvMainMenu.EndUpdate;
    end;
  end;
end;

{ GetUserChoiceFromTStrings }
// We provide a "TStrings" for input.
// Function will show strings into a ttreeview.
// User select the one he wants.
// Function returns the chosen string.
// If user cancel action, returned string is empty.
function GetUserChoiceFromTStrings(ATStrings: TStrings; ContextMode: tvmContextMode; WantedPosX, WantedPosY: integer; WantedWidth: integer = 0; WantedHeight: integer = 0): string;
var
  iIndex: integer;
  local_Result: integer;
begin
  Result := '';

  if ATStrings.Count > 0 then
  begin
    frmTreeViewMenu := TfrmTreeViewMenu.Create(frmMain);
    try
      frmTreeViewMenu.SetContextMode(ContextMode, WantedPosX, WantedPosY, WantedWidth, WantedHeight);

      frmTreeViewMenu.tvMainMenu.BeginUpdate;
      for iIndex := 0 to pred(ATStrings.Count) do
        frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, nil, ATStrings.Strings[iIndex], '', 0, nil);

      frmTreeViewMenu.HideUnmatchingNode;
      if not frmTreeViewMenu.bTargetFixedWidth then  frmTreeViewMenu.SetSizeToLargestElement;
      frmTreeViewMenu.tvMainMenu.EndUpdate;

      local_Result := frmTreeViewMenu.ShowModal;

      case local_Result of
        mrOk: Result := frmTreeViewMenu.tvMainMenu.Items[frmTreeViewMenu.iFinalSelectedIndex].Text;
        mrYes: frmMain.Commands.cm_ConfigTreeViewMenus([]);
        mrAll: frmMain.Commands.cm_ConfigTreeViewMenusColors([]);
      end;

    finally
      FreeAndNil(frmTreeViewMenu);
    end;
  end;
end;

{ GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu }
// We provide a "TMenu" for input (either a popup menu or a mainmenu).
// Function will show items into a ttreeview.
// User select the one he wants.
// Function returns the chosen TMenuItem.
// If user cancel action, returned TMenuItem is nil.
function GetUserChoiceFromTreeViewMenuLoadedFromPopupMenu(pmAnyMenu: TMenu; ContextMode: tvmContextMode; WantedPosX, WantedPosY: integer; WantedWidth: integer = 0; WantedHeight: integer = 0): TMenuItem;
var
  RootNode: TTreeNode;
  iMenuItem: integer;
  local_Result: integer;

  function NormalizeMenuCaption(sMenuCaption: string): string;
  var
    iChar: integer;
  begin
    if UTF8Pos('&', sMenuCaption) = 0 then
    begin
      Result := sMenuCaption;
    end
    else
    begin
      Result := '';
      iChar := 1;
      while iChar <= UTF8Length(sMenuCaption) do
      begin
        if copy(sMenuCaption, iChar, 1) <> '&' then
          Result := Result + copy(sMenuCaption, iChar, 1)
        else
        begin
          if iChar < UTF8Length(sMenuCaption) then
          begin
            if copy(sMenuCaption, iChar + 1, 1) = '&' then
            begin
              Result := Result + '&';
              Inc(iChar);
            end;
          end;
        end;
        Inc(iChar);
      end;
    end;
  end;

  //WARNING: This procedure is recursive and may call itself!
  procedure RecursiveAddMenuBranch(AMenuItem: TMenuItem; ANode: TTreeNode);
  var
    iIndexSubMenuItem: integer;
    ASubNode: TTreeNode;
  begin
    for iIndexSubMenuItem := 0 to pred(AMenuItem.Count) do
    begin
      if AMenuItem.Items[iIndexSubMenuItem].Caption <> '-' then
      begin
        if (AMenuItem.Items[iIndexSubMenuItem].Enabled) and (AMenuItem.Items[iIndexSubMenuItem].Visible) then
        begin
          ASubNode := frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, ANode, NormalizeMenuCaption(AMenuItem.Items[iIndexSubMenuItem].Caption),
            '', 0, AMenuItem.Items[iIndexSubMenuItem]);
          if AMenuItem.Items[iIndexSubMenuItem].Count > 0 then
            RecursiveAddMenuBranch(AMenuItem.Items[iIndexSubMenuItem], ASubNode);
        end;
      end;
    end;
  end;

begin
  Result := nil;

  frmTreeViewMenu := TfrmTreeViewMenu.Create(frmMain);
  try
    frmTreeViewMenu.SetContextMode(ContextMode, WantedPosX, WantedPosY,
      WantedWidth, WantedHeight);

    frmTreeViewMenu.tvMainMenu.BeginUpdate;
    for iMenuItem := 0 to pred(pmAnyMenu.Items.Count) do
    begin
      if pmAnyMenu.Items[iMenuItem].Caption <> '-' then
      begin
        if (pmAnyMenu.Items[iMenuItem].Enabled) and (pmAnyMenu.Items[iMenuItem].Visible) then
        begin
          RootNode := frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, nil, NormalizeMenuCaption(pmAnyMenu.Items[iMenuItem].Caption), '', 0, pmAnyMenu.Items[iMenuItem]);
          if pmAnyMenu.Items[iMenuItem].Count > 0 then
            RecursiveAddMenuBranch(pmAnyMenu.Items[iMenuItem], RootNode);
        end;
      end;
    end;
    frmTreeViewMenu.HideUnmatchingNode;
    if not frmTreeViewMenu.bTargetFixedWidth then  frmTreeViewMenu.SetSizeToLargestElement;
    frmTreeViewMenu.tvMainMenu.EndUpdate;

    local_Result := frmTreeViewMenu.ShowModal;

    case local_Result of
      mrOk: Result := TMenuItem(TTreeMenuItem(frmTreeViewMenu.tvMainMenu.Items[frmTreeViewMenu.iFinalSelectedIndex].Data).PointerSourceData);
      mrYes: frmMain.Commands.cm_ConfigTreeViewMenus([]);
      mrAll: frmMain.Commands.cm_ConfigTreeViewMenusColors([]);
    end;

  finally
    FreeAndNil(frmTreeViewMenu);
  end;
end;

{ GetUserChoiceFromKASToolBar }
function GetUserChoiceFromKASToolBar(AKASToolBar: TKASToolBar; ContextMode: tvmContextMode; WantedPosX, WantedPosY, WantedWidth, WantedHeight: integer; var ReturnedTypeDispatcher: integer): Pointer;
var
  frmTreeViewMenu: TfrmTreeViewMenu;
  sSimiliCaptionToAddToMenu: string;
  sSecondaryText: string;

  procedure AddToSecondyText(sInfo: string);
  begin
    if sInfo <> '' then
    begin
      if sSecondaryText <> '' then
        sSecondaryText := sSecondaryText + ' / ';
      sSecondaryText := sSecondaryText + sInfo;
    end;
  end;

  //WARNING: This procedure is recursive and may call itself!
  procedure RecursiveAddTheseTKASToolItems(AKASMenuItem: TKASMenuItem; ANode: TTreeNode);
  var
    ASubNode: TTreeNode;
    iIndexKASMenuItem: integer;
    AKASToolItem: TKASToolItem;
  begin
    for iIndexKASMenuItem := 0 to pred(AKASMenuItem.SubItems.Count) do
    begin
      sSimiliCaptionToAddToMenu := '';
      sSecondaryText := '';
      AKASToolItem := AKASMenuItem.SubItems.Items[iIndexKASMenuItem];

      if AKASToolItem is TKASNormalItem then
        sSimiliCaptionToAddToMenu := TKASNormalItem(AKASToolItem).Hint;

      if AKASToolItem is TKASCommandItem then
      begin
        AddToSecondyText(TKASCommandItem(AKASToolItem).Command);
        frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, ANode, sSimiliCaptionToAddToMenu, sSecondaryText, 2, AKASToolItem);
      end
      else
      begin
        if AKASToolItem is TKASProgramItem then
        begin
          AddToSecondyText(TKASProgramItem(AKASToolItem).Command);
          frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(
            frmTreeViewMenu.tvMainMenu, ANode, sSimiliCaptionToAddToMenu,
            sSecondaryText, 2, AKASToolItem);
        end
        else
        begin
          if AKASToolItem is TKASMenuItem then
          begin
            if TKASMenuItem(AKASToolItem).SubItems.Count > 0 then
            begin
              ASubNode := frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, ANode, sSimiliCaptionToAddToMenu, sSecondaryText, 0, nil);
              RecursiveAddTheseTKASToolItems(TKASMenuItem(AKASToolItem), ASubNode);
            end;
          end;
        end;
      end;
    end;
  end;

var
  // Variables declared *afer* the recursive block to make sure we won't use it.
  RootNode: TTreeNode;
  iKASToolButton: integer;
  local_Result: integer;
  AKASToolButton: TKASToolButton;

begin
  Result := nil;
  ReturnedTypeDispatcher := -1;

  frmTreeViewMenu := TfrmTreeViewMenu.Create(frmMain);
  try
    frmTreeViewMenu.SetContextMode(ContextMode, WantedPosX, WantedPosY,
      WantedWidth, WantedHeight);

    frmTreeViewMenu.tvMainMenu.BeginUpdate;
    for iKASToolButton := 0 to pred(AKASToolBar.ButtonList.Count) do
    begin
      sSimiliCaptionToAddToMenu := '';
      sSecondaryText := '';

      AKASToolButton := TKASToolButton(AKASToolBar.ButtonList.Items[iKASToolButton]);

      if AKASToolButton.ToolItem is TKASNormalItem then
        sSimiliCaptionToAddToMenu := TKASNormalItem(AKASToolButton.ToolItem).Hint;

      if AKASToolButton.ToolItem is TKASCommandItem then
      begin
        AddToSecondyText(TKASCommandItem(AKASToolButton.ToolItem).Command);
        frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, nil, sSimiliCaptionToAddToMenu, sSecondaryText, 1, AKASToolButton);
      end
      else
      begin
        if AKASToolButton.ToolItem is TKASProgramItem then
        begin
          AddToSecondyText(TKASProgramItem(AKASToolButton.ToolItem).Command);
          frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, nil, sSimiliCaptionToAddToMenu, sSecondaryText, 1, AKASToolButton);
        end
        else
        begin
          if AKASToolButton.ToolItem is TKASMenuItem then
          begin
            if TKASMenuItem(AKASToolButton.ToolItem).SubItems.Count > 0 then
            begin
              RootNode := frmTreeViewMenu.TreeViewMenuGenericRoutineAndVarHolder.AddTreeViewMenuItem(frmTreeViewMenu.tvMainMenu, nil, sSimiliCaptionToAddToMenu, sSecondaryText, 0, nil);
              RecursiveAddTheseTKASToolItems(TKASMenuItem(AKASToolButton.ToolItem), RootNode);
            end;
          end;
        end;
      end;
    end;

    frmTreeViewMenu.HideUnmatchingNode;
    if not frmTreeViewMenu.bTargetFixedWidth then  frmTreeViewMenu.SetSizeToLargestElement;
    frmTreeViewMenu.tvMainMenu.EndUpdate;

    local_Result := frmTreeViewMenu.ShowModal;

    case local_Result of
      mrOk:
      begin
        ReturnedTypeDispatcher := TTreeMenuItem(frmTreeViewMenu.tvMainMenu.Items[frmTreeViewMenu.iFinalSelectedIndex].Data).TypeDispatcher;
        Result := TTreeMenuItem(frmTreeViewMenu.tvMainMenu.Items[frmTreeViewMenu.iFinalSelectedIndex].Data).PointerSourceData;
      end;
      mrYes: frmMain.Commands.cm_ConfigTreeViewMenus([]);
      mrAll: frmMain.Commands.cm_ConfigTreeViewMenusColors([]);
    end;

  finally
    FreeAndNil(frmTreeViewMenu);
  end;
end;

end.

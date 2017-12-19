{
   Double Commander
   -------------------------------------------------------------------------
   Internal Main Commands Selection Dialog Window

   Copyright (C) 2015  Alexander Koblov (alexx2000@mail.ru)

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

unit fMainCommandsDlg;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, Menus,
  ExtCtrls,

  //DC
  KASComboBox, uFormCommands, types;

type
  { TfrmMainCommandsDlg }
  TfrmMainCommandsDlg = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    cbCategorySortOrNot: TComboBoxAutoWidth;
    cbCommandsSortOrNot: TComboBoxAutoWidth;
    cbSelectAllCategoryDefault: TCheckBox;
    gbSelection: TGroupBox;
    imgCommandIcon: TImage;
    lblSelectedCommandCategory: TLabel;
    lblSelectedCommandHotkey: TLabel;
    lblHotKey: TLabel;
    lblSelectedCommandHelp: TLabel;
    lbledtFilter: TLabeledEdit;
    lblCategory: TLabel;
    lblCommandName: TLabel;
    lblHint: TLabel;
    lbCategory: TListBox;
    lbCommands: TListBox;
    lblSelectedCommand: TLabel;
    lblSelectedCommandHint: TLabel;
    pnlImage: TPanel;
    procedure cbCategorySortOrNotChange(Sender: TObject);
    procedure cbCommandsSortOrNotChange(Sender: TObject);
    procedure cbSelectAllCategoryDefaultChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbCategoryEnter(Sender: TObject);
    procedure lbCategoryExit(Sender: TObject);
    procedure lbCategorySelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure lbCommandsDblClick(Sender: TObject);
    procedure lbCommandsDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure lbCommandsEnter(Sender: TObject);
    procedure lbCommandsExit(Sender: TObject);
    procedure lbCommandsKeyPress(Sender: TObject; var Key: char);
    procedure lbledtFilterChange(Sender: TObject);
    procedure AttemptToSetupForThisCommand(CommandToShow: string);
    procedure lbledtFilterEnter(Sender: TObject);
    procedure lbledtFilterExit(Sender: TObject);
    procedure lbledtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblPlaceCaptionInClipClick(Sender: TObject);
    procedure lblSelectedCommandHelpClick(Sender: TObject);
    procedure lblSelectedCommandHelpMouseEnter(Sender: TObject);
    procedure lblSelectedCommandHelpMouseLeave(Sender: TObject);

  private
    { Private declarations }
    FFormCommands: IFormCommands;
    ListCommands: TStringList;
    OffsetForHotKey: integer;
    OffsetForHint: integer;
    lbCommandsItemHeight: Integer;
  public
    { Public declarations }
    procedure LoadCategoryListbox(CategoryToSelectIfAny: string);
    procedure LoadCommandsListbox(WantedCommandToSelect: string);
  end;

{ ShowSplitterFileForm:
  "TMainCommands.cm_FileSpliter" function from "uMainCommands.pas" is calling this routine.}
function ShowMainCommandDlgForm(DefaultCmd: string; var ReturnedCmd: string): boolean;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Clipbrd, LCLType, Graphics, LazUTF8, LCLIntf, Math,

  //DC
  DCStrUtils, dmHelpManager, uLng, uPixMapManager, uGlobs, fMain, uDebug, uClipboard;

function ShowMainCommandDlgForm(DefaultCmd: string; var ReturnedCmd: string): boolean;
var
  frmMainCommandsDlg: TfrmMainCommandsDlg;
begin
  ReturnedCmd := '';

  frmMainCommandsDlg := TfrmMainCommandsDlg.Create(Application); //Did not use the "with..." here to make absolutely sure of what is referenced in the following.
  try
    // Show form
    frmMainCommandsDlg.AttemptToSetupForThisCommand(DefaultCmd);

    Result := (frmMainCommandsDlg.ShowModal = mrOk) and (frmMainCommandsDlg.lbCommands.ItemIndex <> -1);

    if Result then
    begin
      ReturnedCmd := frmMainCommandsDlg.lbCommands.Items.Strings[frmMainCommandsDlg.lbCommands.ItemIndex];
      if pos('|', ReturnedCmd) <> 0 then
        ReturnedCmd := leftstr(ReturnedCmd, (pos('|', ReturnedCmd) - 1));
    end;
  finally
    frmMainCommandsDlg.Free;
  end;
end;

{ TfrmMainCommandsDlg.FormCreate }
procedure TfrmMainCommandsDlg.FormCreate(Sender: TObject);
begin
  ParseLineToList(rsCmdKindOfSort, cbCategorySortOrNot.Items);
  ParseLineToList(rsCmdKindOfSort, cbCommandsSortOrNot.Items);
  InitPropStorage(Self); // Initialize property storage
  FFormCommands := frmMain as IFormCommands;
  ListCommands := TStringList.Create;
end;

{ TfrmMainCommandsDlg.FormDestroy }
procedure TfrmMainCommandsDlg.FormDestroy(Sender: TObject);
begin
  ListCommands.Free;
end;

{ TfrmMainCommandsDlg.lbCategoryEnter }
procedure TfrmMainCommandsDlg.lbCategoryEnter(Sender: TObject);
begin
  lblCategory.Font.Style := [fsBold];
end;

{ TfrmMainCommandsDlg.lbCategoryExit }
procedure TfrmMainCommandsDlg.lbCategoryExit(Sender: TObject);
begin
  lblCategory.Font.Style := [];
end;

{ TfrmMainCommandsDlg.lbCategorySelectionChange }
procedure TfrmMainCommandsDlg.lbCategorySelectionChange(Sender: TObject; User: boolean);
begin
  LoadCommandsListbox('');
  lbledtFilter.OnChange := nil;
  lbledtFilter.Text := '';
  lbledtFilter.OnChange := @lbledtFilterChange;
end;

{ TfrmMainCommandsDlg.lbCommandsDblClick }
procedure TfrmMainCommandsDlg.lbCommandsDblClick(Sender: TObject);
begin
  ModalResult := mrOk; //No need to call "CLOSE", setting the "ModalResult" close the window there.
end;

{ TfrmMainCommandsDlg.lbCommandsDrawItem }
procedure TfrmMainCommandsDlg.lbCommandsDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  sCommand: string = '';
  sHint: string = '';
  sHotKey: string = '';
  sCategory: string = '';
  FlagCategoryTitle: boolean = False;
  Bitmap: TBitmap = nil;
begin
  lbCommandsItemHeight := ARect.Height;
  with Control as TListbox do
  begin
    FFormCommands.ExtractCommandFields(Items.Strings[Index], sCategory, sCommand, sHint, sHotKey, FlagCategoryTitle);

    if FlagCategoryTitle then
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(ARect);
      Canvas.Font.Style := [fsItalic, fsBold];
      if (odSelected in State) then
        Canvas.Font.Color := clBlack;
      Canvas.TextOut(ARect.Left, ARect.Top, '       ' + rsSimpleWordCategory + ': ' + sCommand); //A little offset to the right, it's prettier.
    end
    else
    begin
      Canvas.FillRect(ARect);
      Canvas.TextOut(ARect.Left, ARect.Top, sCommand);
      Canvas.TextOut(ARect.Left + OffsetForHint, ARect.Top, sHint);
      if not (odSelected in State) then
        Canvas.Font.Color := clRed;
      Canvas.TextOut(ARect.Left + OffsetForHotKey, ARect.Top, sHotKey);
    end;

    if odSelected in State then
    begin
      if not FlagCategoryTitle then
      begin
        lblSelectedCommand.Caption := sCommand;
        if sHotKey <> '' then
          lblSelectedCommandHotkey.Caption := '(' + sHotKey + ')'
        else
          lblSelectedCommandHotkey.Caption := '';
        if sCategory <> '' then
          lblSelectedCommandCategory.Caption := rsSimpleWordCategory + ': ' + sCategory + ' -'
        else
          lblSelectedCommandCategory.Caption := '';

        lblSelectedCommandHint.Caption := sHint;
        try
          Bitmap := PixMapManager.LoadBitmapEnhanced(LowerCase(sCommand), 32, True, clDefault, nil);
          imgCommandIcon.Picture.Bitmap.Assign(Bitmap);
        finally
          Bitmap.Free;
        end;
      end
      else
      begin
        lblSelectedCommand.Caption := '';
        lblSelectedCommandHotkey.Caption := '';
        lblSelectedCommandHint.Caption := '';
        lblSelectedCommandCategory.Caption := '';
        imgCommandIcon.Picture.Bitmap.Clear;
      end;
    end;
  end;
end;

{ TfrmMainCommandsDlg.lbCommandsEnter }
procedure TfrmMainCommandsDlg.lbCommandsEnter(Sender: TObject);
begin
  lblCommandName.Font.Style := [fsBold];
end;

{ TfrmMainCommandsDlg.lbCommandsExit }
procedure TfrmMainCommandsDlg.lbCommandsExit(Sender: TObject);
begin
  lblCommandName.Font.Style := [];
end;

procedure TfrmMainCommandsDlg.lbCommandsKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #$0D:
    begin
      Key := #$00;
      ModalResult := mrOk;
    end;
    #$1B:
    begin
      Key := #$00;
      ModalResult := mrCancel;
    end;
    else
      inherited;
  end;
end;

{ TfrmMainCommandsDlg.lbledtFilterChange }
procedure TfrmMainCommandsDlg.lbledtFilterChange(Sender: TObject);
var
  IndexItem: longint;
  LastSelectedText: string;
begin
  lblSelectedCommand.Caption := '';
  lblSelectedCommandHotkey.Caption := '';
  lblSelectedCommandHint.Caption := '';
  lblSelectedCommandCategory.Caption := '';
  imgCommandIcon.Picture.Bitmap.Clear;
  if lbCommands.ItemIndex <> -1 then
    LastSelectedText := lbCommands.Items.Strings[lbCommands.ItemIndex]
  else
    LastSelectedText := '';

  lbCommands.Items.Clear;
  for IndexItem := 0 to pred(ListCommands.Count) do
  begin
    if (lbledtFilter.Text = '') or (Pos(UTF8LowerCase(lbledtFilter.Text), UTF8LowerCase(ListCommands.Strings[IndexItem])) <> 0) then
      lbCommands.Items.Add(ListCommands.Strings[IndexItem]);
  end;

  if LastSelectedText <> '' then
    lbCommands.ItemIndex := lbCommands.Items.IndexOf(LastSelectedText);

  if (lbCommands.ItemIndex = -1) and (lbCommands.Items.Count > 0) then
    lbCommands.ItemIndex := 0;
end;

{ procedure TfrmMainCommandsDlg.cbCategorySortOrNotChange }
procedure TfrmMainCommandsDlg.cbCategorySortOrNotChange(Sender: TObject);
begin
  LoadCategoryListbox('');
  lbledtFilter.OnChange := nil;
  lbledtFilter.Text := '';
  lbledtFilter.OnChange := @lbledtFilterChange;
end;

{ TfrmMainCommandsDlg.cbCommandsSortOrNotChange }
procedure TfrmMainCommandsDlg.cbCommandsSortOrNotChange(Sender: TObject);
begin
  LoadCommandsListbox('');
  lbledtFilter.OnChange := nil;
  lbledtFilter.Text := '';
  lbledtFilter.OnChange := @lbledtFilterChange;
end;

{ TfrmMainCommandsDlg.cbSelectAllCategoryDefaultChange }
procedure TfrmMainCommandsDlg.cbSelectAllCategoryDefaultChange(Sender: TObject);
begin
  if cbSelectAllCategoryDefault.Checked then
    if (lbCategory.ItemIndex <> 0) and (lbCategory.Count > 0) then
      lbCategory.ItemIndex := 0;
end;

procedure TfrmMainCommandsDlg.FormActivate(Sender: TObject);
begin
  lbCommands.MakeCurrentVisible; //Looks like it's not necessary with Windows, but with Linux it is.
  lbCategory.MakeCurrentVisible;
end;

{ TfrmMainCommandsDlg.LoadCategoryListbox }
procedure TfrmMainCommandsDlg.LoadCategoryListbox(CategoryToSelectIfAny: string);
var
  ListCategory: TStringList;
  LastCategorySelected: string;
begin
  ListCategory := TStringList.Create;
  try
    if lbCategory.ItemIndex <> -1 then
      LastCategorySelected := lbCategory.Items.Strings[lbCategory.ItemIndex]
    else
      LastCategorySelected := '';

    FFormCommands.GetCommandCategoriesList(ListCategory, TCommandCategorySortOrder(cbCategorySortOrNot.ItemIndex));
    lbCategory.Items.Assign(ListCategory);

    lbCategory.ItemIndex := lbCategory.Items.IndexOf(CategoryToSelectIfAny);

    if lbCategory.ItemIndex = -1 then
    begin
      if LastCategorySelected <> '' then
        lbCategory.ItemIndex := lbCategory.Items.IndexOf(LastCategorySelected);

      if (lbCategory.ItemIndex = -1) and (lbCategory.Items.Count > 0) then
        lbCategory.ItemIndex := 0;
    end;
  finally
    ListCategory.Free;
  end;
end;

{ TfrmMainCommandsDlg.LoadCommandsListbox }
procedure TfrmMainCommandsDlg.LoadCommandsListbox(WantedCommandToSelect: string);
var
  LastSelectedCommand: string;
  SearchingIndex, WantedCommandIndex, LastSelectedIndex: longint;
  sCommand: string = '';
  sHint: string = '';
  sHotKey: string = '';
  sCategory: string;
  FlagCategoryTitle: boolean = False;
  LargestCommandName, LargestHotKeyName: longint;
begin
  LastSelectedCommand := '';
  if lbCommands.ItemIndex <> -1 then
  begin
    FFormCommands.ExtractCommandFields(ListCommands.Strings[lbCommands.ItemIndex], sCategory, sCommand, sHint, sHotKey, FlagCategoryTitle);
    if not FlagCategoryTitle then
      LastSelectedCommand := sCommand;
  end;

  FFormCommands.GetCommandsListForACommandCategory(ListCommands, lbCategory.Items.Strings[lbCategory.ItemIndex], TCommandSortOrder(cbCommandsSortOrNot.ItemIndex));

  LargestCommandName := lblCommandName.Canvas.TextWidth(lblCommandName.Caption);
  LargestHotKeyName := lblCommandName.Canvas.TextWidth(lblHotKey.Caption); //This way, if the word "hotkey" once translated is longer than a hotkey, label will not be overwritten.
  WantedCommandIndex := -1;
  LastSelectedIndex := -1;

  SearchingIndex := 0;
  while (SearchingIndex < ListCommands.Count) do
  begin
    FFormCommands.ExtractCommandFields(ListCommands.Strings[SearchingIndex], sCategory, sCommand, sHint, sHotKey, FlagCategoryTitle);
    if not FlagCategoryTitle then
    begin
      if lblCommandName.Canvas.TextWidth(sCommand) > LargestCommandName then
        LargestCommandName := lblCommandName.Canvas.TextWidth(sCommand);
      if lblCommandName.Canvas.TextWidth(sHotKey) > LargestHotKeyName then
        LargestHotKeyName := lblCommandName.Canvas.TextWidth(sHotKey);
      if (WantedCommandToSelect <> '') and (WantedCommandToSelect = sCommand) then
        WantedCommandIndex := SearchingIndex;
      if (LastSelectedCommand <> '') and (LastSelectedCommand = sCommand) then
        LastSelectedIndex := SearchingIndex;
    end;
    Inc(SearchingIndex);
  end;

  OffsetForHotKey := LargestCommandName + 10;
  lblHotKey.BorderSpacing.Left := OffsetForHotKey + 1;
  OffsetForHint := LargestCommandName + 10 + LargestHotKeyName + 10;
  lblHint.BorderSpacing.Left := OffsetForHint + 1;

  lbCommands.Items.Assign(ListCommands);

  if WantedCommandIndex <> -1 then
    lbCommands.ItemIndex := WantedCommandIndex
  else
  if LastSelectedIndex <> -1 then
    lbCommands.ItemIndex := LastSelectedIndex
  else
  if lbCommands.Items.Count > 0 then
    lbCommands.ItemIndex := 0;
end;

{ TfrmMainCommandsDlg.AttemptToSetupForThisCommand }
procedure TfrmMainCommandsDlg.AttemptToSetupForThisCommand(CommandToShow: string);
var
  CommandRec: PCommandRec;
begin
  CommandRec := frmMain.Commands.Commands.GetCommandRec(CommandToShow);
  if Assigned(CommandRec) then
  begin
    if Assigned(CommandRec^.Action) and CommandRec^.Action.Enabled then
    begin
      if cbSelectAllCategoryDefault.Checked then
        LoadCategoryListbox('')
      else
        LoadCategoryListbox(CommandRec^.Action.Category);
      LoadCommandsListbox(CommandToShow);
    end;
  end;
end;

{ TfrmMainCommandsDlg.lbledtFilterEnter }
procedure TfrmMainCommandsDlg.lbledtFilterEnter(Sender: TObject);
begin
  lbledtFilter.EditLabel.Font.Style := [fsBold];
end;

{ TfrmMainCommandsDlg.lbledtFilterExit }
procedure TfrmMainCommandsDlg.lbledtFilterExit(Sender: TObject);
begin
  lbledtFilter.EditLabel.Font.Style := [];
end;

procedure TfrmMainCommandsDlg.lbledtFilterKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var NewIndex: Integer;
begin
  case Key of
    VK_UP:
      NewIndex := lbCommands.ItemIndex - 1;
    VK_DOWN:
      NewIndex := lbCommands.ItemIndex + 1;
    VK_PRIOR:
      NewIndex := lbCommands.ItemIndex - (lbCommands.ClientHeight div lbCommandsItemHeight) + 1;
    VK_NEXT:
      NewIndex := lbCommands.ItemIndex + (lbCommands.ClientHeight div lbCommandsItemHeight) - 1;
    VK_HOME:
      if (ssCtrl in Shift) then
        NewIndex := 0
      else
        Exit;
    VK_END:
      if (ssCtrl in Shift) then
        NewIndex := lbCommands.Items.Count - 1
      else
        Exit;
    else
      Exit;
  end;
  Key := 0;
  if lbCommands.Items.Count > 0 then
    lbCommands.ItemIndex := EnsureRange(NewIndex, 0, lbCommands.Items.Count - 1);
end;

{ TfrmMainCommandsDlg.lblPlaceCaptionInClipClick }
procedure TfrmMainCommandsDlg.lblPlaceCaptionInClipClick(Sender: TObject);
begin
  with Sender as TLabel do
    ClipboardSetText(Caption);
  ShowMessage(Format(rsMsgThisIsNowInClipboard, [Clipboard.AsText]));
end;

{ TfrmMainCommandsDlg.lblSelectedCommandHelpClick }
procedure TfrmMainCommandsDlg.lblSelectedCommandHelpClick(Sender: TObject);
begin
  ShowHelpForKeywordWithAnchor('/cmds.html#' + lblSelectedCommand.Caption);
end;

{ TfrmMainCommandsDlg.lblSelectedCommandHelpClick }
procedure TfrmMainCommandsDlg.lblSelectedCommandHelpMouseEnter(Sender: TObject);
begin
  lblSelectedCommandHelp.Font.Color := clRed;
end;

{ TfrmMainCommandsDlg.lblSelectedCommandHelpMouseLeave }
procedure TfrmMainCommandsDlg.lblSelectedCommandHelpMouseLeave(Sender: TObject);
begin
  lblSelectedCommandHelp.Font.Color := clDefault;
end;

end.

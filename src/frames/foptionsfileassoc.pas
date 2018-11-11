{
    Double Commander
    -------------------------------------------------------------------------
    File associations configuration

    Copyright (C) 2008-2018  Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsFileAssoc;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, EditBtn, ExtDlgs, Menus, ActnList, Types,
  //DC
  uExts, fOptionsFrame;
type

  { TfrmOptionsFileAssoc }
  TfrmOptionsFileAssoc = class(TOptionsEditor)
    btnAddAct: TButton;
    btnCloneAct: TButton;
    btnCommands: TSpeedButton;
    btnInsertExt: TButton;
    btnInsertAct: TButton;
    btnAddExt: TButton;
    btnAddNewType: TButton;
    btnDownAct: TButton;
    btnEditExt: TButton;
    btnParametersHelper: TSpeedButton;
    btnRelativePathIcon: TSpeedButton;
    btnIconSelectFilename: TSpeedButton;
    btnStartPathVarHelper: TSpeedButton;
    btnRelativeCommand: TSpeedButton;
    btnStartPathPathHelper: TSpeedButton;
    btnUpAct: TButton;
    btnRemoveAct: TButton;
    btnRemoveExt: TButton;
    btnRemoveType: TButton;
    btnRenameType: TButton;
    deStartPath: TDirectoryEdit;
    edbActionName: TEditButton;
    edIconFileName: TEdit;
    edtParams: TEdit;
    fneCommand: TFileNameEdit;
    gbActionDescription: TGroupBox;
    gbFileTypes: TGroupBox;
    gbIcon: TGroupBox;
    gbExts: TGroupBox;
    gbActions: TGroupBox;
    lbActions: TListBox;
    lbExts: TListBox;
    lbFileTypes: TListBox;
    lblAction: TLabel;
    lblCommand: TLabel;
    lblExternalParameters: TLabel;
    lblStartPath: TLabel;
    MenuItem1: TMenuItem;
    miInternalViewer: TMenuItem;
    miInternalEditor: TMenuItem;
    miSeparator: TMenuItem;
    miCustom: TMenuItem;
    MenuItem3: TMenuItem;
    miOpenWith: TMenuItem;
    miViewWith: TMenuItem;
    miEditWith: TMenuItem;
    miGetOutputFromCommand: TMenuItem;
    miShell: TMenuItem;
    miViewer: TMenuItem;
    miEditor: TMenuItem;
    miEdit: TMenuItem;
    miView: TMenuItem;
    miOpen: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    pmVariableStartPathHelper: TPopupMenu;
    pnlBottomSettings: TPanel;
    pmVariableParamsHelper: TPopupMenu;
    pmPathHelper: TPopupMenu;
    pnlLeftSettings: TPanel;
    pnlActsEdits: TPanel;
    pnlActsButtons: TPanel;
    pnlExtsButtons: TPanel;
    pnlRightSettings: TPanel;
    pnlSettings: TPanel;
    pmActions: TPopupMenu;
    pmCommands: TPopupMenu;
    sbtnIcon: TSpeedButton;
    procedure btnActionsClick(Sender: TObject);
    procedure btnCloneActClick(Sender: TObject);
    procedure btnInsertAddActClick(Sender: TObject);
    procedure btnInsertAddExtClick(Sender: TObject);
    procedure btnParametersHelperClick(Sender: TObject);
    procedure btnRelativeCommandClick(Sender: TObject);
    procedure btnRelativePathIconClick(Sender: TObject);
    procedure btnStartPathPathHelperClick(Sender: TObject);
    procedure btnStartPathVarHelperClick(Sender: TObject);
    procedure deStartPathChange(Sender: TObject);
    procedure edtParamsChange(Sender: TObject);
    procedure edIconFileNameChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    function InsertAddSingleExtensionToLists(sExt: string; iInsertPosition: integer): boolean;
    procedure InsertAddExtensionToLists(sParamExt: string; iPositionToInsert: integer);
    procedure btnAddNewTypeClick(Sender: TObject);
    procedure btnCommandsClick(Sender: TObject);
    procedure btnDownActClick(Sender: TObject);
    procedure btnEditExtClick(Sender: TObject);
    procedure btnRemoveActClick(Sender: TObject);
    procedure btnRemoveExtClick(Sender: TObject);
    procedure btnRemoveTypeClick(Sender: TObject);
    procedure btnRemoveTypeResize(Sender: TObject);
    procedure btnRenameTypeClick(Sender: TObject);
    procedure btnRenameTypeResize(Sender: TObject);
    procedure btnUpActClick(Sender: TObject);
    procedure lbActionsDragDrop(Sender, {%H-}Source: TObject; {%H-}X, Y: integer);
    procedure lbActionsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure lbExtsDragDrop(Sender, {%H-}Source: TObject; X, Y: integer);
    procedure lbGenericListDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure lbExtsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure lbFileTypesDragDrop(Sender, {%H-}Source: TObject; {%H-}X, Y: integer);
    procedure lbGenericDragOver(Sender, Source: TObject; {%H-}X, Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure lbFileTypesDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure lbFileTypesSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure edbActionNameChange(Sender: TObject);
    procedure fneCommandChange(Sender: TObject);
    procedure miActionsClick(Sender: TObject);
    procedure miCommandsClick(Sender: TObject);
    procedure pnlRightSettingsResize(Sender: TObject);
    procedure pnlSettingsResize(Sender: TObject);
    procedure sbtnIconClick(Sender: TObject);
    procedure MakeUsInPositionToWorkWithActiveFile;
    procedure actSelectFileTypeExecute(Sender: TObject);
    procedure actSelectIconExecute(Sender: TObject);
    procedure actSelectExtensionsExecute(Sender: TObject);
    procedure actSelectActionsExecute(Sender: TObject);
    procedure actSelectActionDescriptionExecute(Sender: TObject);

  private
    Exts: TExts;
    FUpdatingControls: boolean;
    liveActionList: TActionList;
    actSelectFileType: TAction;
    actSelectIcon: TAction;
    actSelectExtensions: TAction;
    actSelectActions: TAction;
    actSelectActionDescription: TAction;
    procedure UpdateEnabledButtons;
    {en
       Frees icon cached in lbFileTypes.Items.Objects[Index].
    }
    procedure FreeIcon(iIndex: integer);
    procedure SetIconFileName(const sFileName: string);
    procedure SetMinimumSize;

  protected
    procedure Init; override;
    procedure Done; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  LCLProc, Math, LCLType, LazUTF8,

  //DC
  uOSForms, fMain, uFile, uGlobs, uPixMapManager, uLng, uDCUtils, DCOSUtils,
  uShowMsg, uSpecialDir;
const
  ACTUAL_ADD_ACTION = 1;
  SET_ACTION_WORD = 2;

{ TfrmOptionsFileAssoc }

{ TfrmOptionsFileAssoc.Init }
procedure TfrmOptionsFileAssoc.Init;
begin
  inherited Init;
  Exts := TExts.Create;
  FUpdatingControls := False;
  btnIconSelectFilename.Hint := sbtnIcon.Hint;

  // The following section is to help to speed up the the user with keyboard to pass to a section to another.
  // Each TGroupBox has their caption with 1, 2, 3... with underscore under each digit.
  // This suggest to user keyboard accelerator shortcut so he will type Alt+1, Alt+2, etc to pass to a section to another.
  // Unfortunately, at this moment in Windows at least, even if we have underscore, it does not work as keyboard accelerator...
  // It does not switch focus to the proper TGroupbox...
  // So what we will do is to mimic that.
  // We will display the caption of the TGroupBox with underscore to suggest the Alt+1, Alt+2, etc.
  // And we will add in our TActionList function to set the focus on proper TGroupBox and set the keyboard shortcut to Alt+1, Alt+2, etc.
  // So at the end it does the job.
  // If we defined that run-time here instead of having it in the form itself it's to avoid to have annoying empty caption yo appear in the languages files.
  liveActionList := TActionList.Create(Self);
  actSelectFileType := TAction.Create(nil);
  actSelectFileType.OnExecute := @actSelectFileTypeExecute;
  actSelectFileType.ShortCut := 32817; //Alt+1
  actSelectFileType.ActionList := liveActionList;
  actSelectIcon := TAction.Create(nil);
  actSelectIcon.OnExecute := @actSelectIconExecute;
  actSelectIcon.ShortCut := 32818; //Alt+2
  actSelectIcon.ActionList := liveActionList;
  actSelectExtensions := TAction.Create(nil);
  actSelectExtensions.OnExecute := @actSelectExtensionsExecute;
  actSelectExtensions.ShortCut := 32819; //Alt+3
  actSelectExtensions.ActionList := liveActionList;
  actSelectActions := TAction.Create(nil);
  actSelectActions.OnExecute := @actSelectActionsExecute;
  actSelectActions.ShortCut := 32820; //Alt-4
  actSelectActions.ActionList := liveActionList;
  actSelectActionDescription := TAction.Create(nil);
  actSelectActionDescription.OnExecute := @actSelectActionDescriptionExecute;
  actSelectActionDescription.ShortCut := 32821; //Alt-5
  actSelectActionDescription.ActionList := liveActionList;
end;

{ TfrmOptionsFileAssoc.Done }
procedure TfrmOptionsFileAssoc.Done;
var
  I: integer;
begin
  for I := 0 to lbFileTypes.Items.Count - 1 do
    FreeIcon(I);
  FreeAndNil(Exts);
  FreeAndNil(actSelectActionDescription);
  FreeAndNil(actSelectActions);
  FreeAndNil(actSelectExtensions);
  FreeAndNil(actSelectIcon);
  FreeAndNil(actSelectFileType);
  FreeAndNil(liveActionList);
  inherited Done;
end;

{ TfrmOptionsFileAssoc.Load }
procedure TfrmOptionsFileAssoc.Load;
var
  I: integer;
  sName: string;
  Bitmap: TBitmap;
begin
  //Let's preserve the precious legacy .po translated groupbox name that we will re-use with dialog window concerning them
  gbFileTypes.hint := gbFileTypes.Caption;
  gbIcon.hint := gbIcon.Caption;
  gbExts.hint := gbExts.Caption;
  gbActions.hint := gbActions.Caption;
  gbActionDescription.hint := gbActionDescription.Caption;

  // Give some numerical step number to help user without losing legacy .po translated groubox name
  gbFileTypes.Caption := '&1 - ' + gbFileTypes.Caption;
  gbIcon.Caption := '&2 - ' + gbIcon.Caption;
  gbExts.Caption := '&3 - ' + gbExts.Caption;
  gbActions.Caption := '&4 - ' + gbActions.Caption;
  gbActionDescription.Caption := '&5 - ' + gbActionDescription.Caption;

  // load extension file
  Exts.Load;

  //'Pp'! A letter with the upper part and a letter with the lower part! This should give us approximation of highest room required! :-)
  lbFileTypes.ItemHeight := Max(gIconsSize, lbFileTypes.Canvas.TextHeight('Pp')) + 4;

  // fill file types list box
  for I := 0 to Exts.Count - 1 do
  begin
    sName := Exts.Items[I].Name;

    // load icon for use in OnDrawItem procedure
    Bitmap := PixMapManager.LoadBitmapEnhanced(Exts.Items[I].Icon, gIconsSize, True, lbFileTypes.Color);
    lbFileTypes.Items.AddObject(sName, Bitmap);
  end;

  if Exts.Count > 0 then
    lbFileTypes.ItemIndex := 0;

  UpdateEnabledButtons;

  // Populate helper menu
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  gSupportForVariableHelperMenu.PopulateMenuWithVariableHelper(pmVariableParamsHelper, edtParams);
  gSupportForVariableHelperMenu.PopulateMenuWithVariableHelper(pmVariableStartPathHelper, deStartPath);

  inherited Load;
end;

{ TfrmOptionsFileAssoc.Save }
function TfrmOptionsFileAssoc.Save: TOptionsEditorSaveFlags;
var
  iExt: integer;
begin
  Exts.SaveXMLFile;
  gExts.Clear;
  gExts.Load;

  // The "gExts.Clear" has flush the "IconIndex" that have been set via "Load" of PixMapManager.
  // Since it has been lost AND PixMapManager.Load won't set again our iconindex, let's do manually here.
  // It is required so the icon next to our actions in SheelContextMenu will be the correct ones.
  for iExt := 0 to pred(gExts.Count) do
    TExtAction(gExts.Items[iExt]).IconIndex := PixMapManager.GetIconByName(TExtAction(gExts.Items[iExt]).Icon);

  Result := inherited Save;
end;

{ TfrmOptionsFileAssoc.GetIconIndex }
class function TfrmOptionsFileAssoc.GetIconIndex: integer;
begin
  Result := 34;
end;

{ TfrmOptionsFileAssoc.GetTitle }
class function TfrmOptionsFileAssoc.GetTitle: string;
begin
  Result := rsOptionsEditorFileAssoc;
end;

{ TfrmOptionsFileAssoc.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsFileAssoc.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  Result := False;
end;

{ TfrmOptionsFileAssoc.ExtraOptionsSignature }
function TfrmOptionsFileAssoc.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  Result := Exts.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsFileAssoc.UpdateEnabledButtons }
procedure TfrmOptionsFileAssoc.UpdateEnabledButtons;
begin
  if (lbFileTypes.Items.Count = 0) or (lbFileTypes.ItemIndex = -1) then
  begin
    sbtnIcon.Enabled := False;
    btnInsertExt.Enabled := False;
    btnAddExt.Enabled := False;
    btnInsertAct.Enabled := False;
    btnAddAct.Enabled := False;
  end
  else
  begin
    btnInsertExt.Enabled := (lbExts.Items.Count > 0);
    btnAddExt.Enabled := True;
    btnInsertAct.Enabled := (lbExts.Items.Count > 0) and (lbActions.ItemIndex <> -1);
    btnAddAct.Enabled := btnInsertExt.Enabled;
    sbtnIcon.Enabled := btnInsertExt.Enabled;
  end;

  btnRemoveExt.Enabled := ((lbExts.Items.Count <> 0) and (lbExts.ItemIndex <> -1));
  btnEditExt.Enabled := btnRemoveExt.Enabled;

  if (lbActions.Items.Count = 0) or (lbActions.ItemIndex = -1) then
  begin
    btnUpAct.Enabled := False;
    btnDownAct.Enabled := False;
    btnRemoveAct.Enabled := False;
    edbActionName.Enabled := False;
    fneCommand.Enabled := False;
    edtParams.Enabled := False;
    deStartPath.Enabled := False;
    btnCommands.Enabled := False;
    btnRelativeCommand.Enabled := False;
    btnParametersHelper.Enabled := False;
    btnStartPathPathHelper.Enabled := False;
    btnStartPathVarHelper.Enabled := False;
    edbActionName.Text := '';
    fneCommand.FileName := '';
    edtParams.Text := '';
    deStartPath.Text := '';
  end
  else
  begin
    btnUpAct.Enabled := (lbActions.ItemIndex > 0);
    btnDownAct.Enabled := (lbActions.ItemIndex < lbActions.Items.Count - 1);
    btnRemoveAct.Enabled := True;
    edbActionName.Enabled := True;
    fneCommand.Enabled := True;
    btnRelativeCommand.Enabled := True;
    btnParametersHelper.Enabled := True;
    btnStartPathPathHelper.Enabled := True;
    btnStartPathVarHelper.Enabled := True;
    edtParams.Enabled := True;
    deStartPath.Enabled := True;
    btnCommands.Enabled := True;
  end;

  btnCloneAct.Enabled := btnRemoveAct.Enabled;
end;

{ TfrmOptionsFileAssoc.btnAddNewTypeClick }
procedure TfrmOptionsFileAssoc.btnAddNewTypeClick(Sender: TObject);
var
  ExtAction: TExtAction;
  s: string;
begin
  s := InputBox(GetTitle + ' - ' + gbFileTypes.Hint, rsMsgEnterName, '');
  if s = '' then exit;
  ExtAction := TExtAction.Create;
  ExtAction.IconIndex := -1;
  with lbFileTypes do
  begin
    ExtAction.Name := s;
    Items.AddObject(ExtAction.Name, nil);

    // add file type to TExts object
    Exts.AddItem(ExtAction);
    ItemIndex := Items.Count - 1;
  end;
  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.btnRemoveTypeClick }
procedure TfrmOptionsFileAssoc.btnRemoveTypeClick(Sender: TObject);
var
  iIndex: integer;
begin
  with lbFileTypes do
  begin
    iIndex := ItemIndex;
    if iIndex < 0 then Exit;
    FreeIcon(iIndex);
    Items.Delete(iIndex);
    Exts.DeleteItem(iIndex);
    if Items.Count = 0 then
    begin
      lbExts.Clear;
      lbActions.Clear;
    end
    else
    begin
      if iIndex = 0 then
        ItemIndex := 0
      else
        ItemIndex := iIndex - 1;
    end;
  end;
  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.btnRemoveTypeResize }
procedure TfrmOptionsFileAssoc.btnRemoveTypeResize(Sender: TObject);
begin
  SetMinimumSize;
end;

{ TfrmOptionsFileAssoc.btnRenameTypeClick }
procedure TfrmOptionsFileAssoc.btnRenameTypeClick(Sender: TObject);
var
  iIndex: integer;
  sName: string;
begin
  iIndex := lbFileTypes.ItemIndex;
  if iIndex < 0 then Exit;
  sName := lbFileTypes.Items[iIndex];
  sName := InputBox(GetTitle + ' - ' + gbFileTypes.Hint, rsMsgEnterName, sName);
  lbFileTypes.Items[iIndex] := sName;
  // rename file type in TExts object
  Exts.Items[iIndex].Name := sName;
  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.btnRenameTypeResize }
procedure TfrmOptionsFileAssoc.btnRenameTypeResize(Sender: TObject);
begin
  SetMinimumSize;
end;

{ TfrmOptionsFileAssoc.lbActionsSelectionChange }
procedure TfrmOptionsFileAssoc.lbActionsSelectionChange(Sender: TObject; User: boolean);
var
  iIndex: integer;
begin
  iIndex := lbActions.ItemIndex;
  if (iIndex < 0) or (lbActions.Tag = 1) then Exit;
  edbActionName.Tag := 1;
  edbActionName.Text := TExtActionCommand(lbActions.Items.Objects[iIndex]).ActionName;
  fneCommand.FileName := TExtActionCommand(lbActions.Items.Objects[iIndex]).CommandName;
  edtParams.Text := TExtActionCommand(lbActions.Items.Objects[iIndex]).Params;
  deStartPath.Text := TExtActionCommand(lbActions.Items.Objects[iIndex]).StartPath;
  edbActionName.Tag := 0;
  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.lbExtsDragDrop }
procedure TfrmOptionsFileAssoc.lbExtsDragDrop(Sender, Source: TObject; X, Y: integer);
var
  SrcIndex, DestIndex: integer;
begin
  // exchange positions in extensions listbox
  SrcIndex := lbExts.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbExts.GetIndexAtXY(X, Y);
  if (DestIndex < 0) or (DestIndex >= lbExts.Count) then
    DestIndex := lbExts.Count - 1;

  lbExts.Items.Move(SrcIndex, DestIndex);
  lbExts.ItemIndex := DestIndex;

  // change extension in TExts object
  Exts.Items[lbFileTypes.ItemIndex].Extensions.Move(SrcIndex, DestIndex);
end;

procedure TfrmOptionsFileAssoc.lbGenericListDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox) do
  begin
    if (odSelected in State) then
    begin
      if focused then
      begin
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
      end
      else
      begin
        Canvas.Font.Color := clWindowText;
        Canvas.Brush.Color := clGradientActiveCaption;
      end;
    end
    else
    begin
      Canvas.Font.Color := Font.Color;
      Canvas.Brush.Color := Color;
    end;
    Canvas.FillRect(ARect);
    Canvas.TextOut(ARect.Left, ARect.Top, Items[Index]);
  end;
end;

{ TfrmOptionsFileAssoc.lbExtsSelectionChange }
procedure TfrmOptionsFileAssoc.lbExtsSelectionChange(Sender: TObject; User: boolean);
begin
  if (lbExts.ItemIndex < 0) then Exit;
  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.lbFileTypesDragDrop }
procedure TfrmOptionsFileAssoc.lbFileTypesDragDrop(Sender, Source: TObject; X, Y: integer);
var
  SrcIndex, DestIndex: integer;
begin
  // Validate if the move is okay
  SrcIndex := lbFileTypes.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbFileTypes.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lbFileTypes.Count) then
    DestIndex := lbFileTypes.Count - 1;

  // exchange positions in actions listbox
  lbActions.Tag := 1; // start moving
  try
    lbFileTypes.Items.Move(SrcIndex, DestIndex);
    lbFileTypes.ItemIndex := DestIndex;

    // exchange actions in TExts object
    Exts.MoveItem(SrcIndex, DestIndex);
  finally
    lbActions.Tag := 0; // end moving
  end;

  // trig the "SelectionChange" event to refresh extension and action lists
  lbFileTypesSelectionChange(lbFileTypes, False);

  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.lbGenericDragOver }
procedure TfrmOptionsFileAssoc.lbGenericDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  //Accept if it's coming from the same ListBox
  Accept := (Source is TListBox) and (TListBox(Source).Name = TListBox(Sender).Name);

  //Let's scroll up/down if user is dragging near the top/bottom
  if Y > (TListBox(Sender).Height - 15) then
    TListBox(Sender).TopIndex := TListBox(Sender).TopIndex + 1
  else
  if Y < 15 then
    TListBox(Sender).TopIndex := TListBox(Sender).TopIndex - 1;
end;

{ TfrmOptionsFileAssoc.lbFileTypesDrawItem }
procedure TfrmOptionsFileAssoc.lbFileTypesDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  iDrawTop: integer;
begin
  with (Control as TListBox) do
  begin
    if odSelected in State then
    begin
      if focused then
      begin
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
      end
      else
      begin
        Canvas.Font.Color := clWindowText;
        Canvas.Brush.Color := clGradientActiveCaption;
      end;
      Canvas.FillRect(ARect);
    end
    else
    begin
      Canvas.Font.Color := Font.Color;
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ARect);
    end;

    if (Canvas.Locked = False) and (Assigned(Items.Objects[Index])) then
    begin
      iDrawTop := ARect.Top + ((lbFileTypes.ItemHeight - gIconsSize) div 2);
      Canvas.Draw(ARect.Left + 2, iDrawTop, TBitmap(Items.Objects[Index]));
    end;
    iDrawTop := ARect.Top + ((lbFileTypes.ItemHeight - Canvas.TextHeight(Items[Index])) div 2);
    Canvas.TextOut(ARect.Left + gIconsSize + 6, iDrawTop, Items[Index]);
  end;
end;

{ TfrmOptionsFileAssoc.lbFileTypesSelectionChange }
procedure TfrmOptionsFileAssoc.lbFileTypesSelectionChange(Sender: TObject; User: boolean);
var
  ExtCommand: TExtAction;
  I: integer;
  bmpTemp: TBitmap = nil;
begin
  if (lbFileTypes.ItemIndex >= 0) and (lbFileTypes.ItemIndex < Exts.Count) then
  begin
    lbActions.Items.Clear;
    Application.ProcessMessages;
    lbExts.Items.Clear;
    Application.ProcessMessages;

    ExtCommand := Exts.Items[lbFileTypes.ItemIndex];
    lbExts.Items.Assign(ExtCommand.Extensions);
    if lbExts.Count > 0 then lbExts.ItemIndex := 0;
    for I := 0 to pred(ExtCommand.ActionList.Count) do
    begin
      lbActions.Items.AddObject(ExtCommand.ActionList.ExtActionCommand[I].ActionName, ExtCommand.ActionList.ExtActionCommand[I]);
    end;
    if lbActions.Count > 0 then lbActions.ItemIndex := 0;

    bmpTemp := PixMapManager.LoadBitmapEnhanced(ExtCommand.Icon, 32, True, sbtnIcon.Color);
    try
      sbtnIcon.Glyph := bmpTemp;
    finally
      if Assigned(bmpTemp) then
        FreeAndNil(bmpTemp);
    end;

    FUpdatingControls := True; // Don't trigger OnChange
    edIconFileName.Text := ExtCommand.Icon;
    FUpdatingControls := False;
  end
  else
  begin
    lbExts.Items.Clear;
    lbActions.Items.Clear;
    sbtnIcon.Glyph.Clear;
    edIconFileName.Text := '';
  end;

  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.edbActionChange }
procedure TfrmOptionsFileAssoc.edbActionNameChange(Sender: TObject);
var
  iIndex: integer;
begin
  if edbActionName.Tag = 1 then exit;
  iIndex := lbActions.ItemIndex;
  if (iIndex < 0) or (edbActionName.Text = '') then Exit;
  TExtActionCommand(lbActions.Items.Objects[iIndex]).ActionName := edbActionName.Text;
  lbActions.Items[iIndex] := edbActionName.Text;
end;

{ TfrmOptionsFileAssoc.fneCommandChange }
procedure TfrmOptionsFileAssoc.fneCommandChange(Sender: TObject);
var
  iIndex: integer;
begin
  iIndex := lbActions.ItemIndex;
  if (edbActionName.Tag = 1) or (iIndex < 0) then Exit;
  TExtActionCommand(lbActions.Items.Objects[iIndex]).CommandName := fneCommand.Text;
  if fneCommand.InitialDir <> ExtractFilePath(fneCommand.Text) then
    fneCommand.InitialDir := ExtractFilePath(fneCommand.Text);
end;

{ TfrmOptionsFileAssoc.edtExternalParametersChange }
procedure TfrmOptionsFileAssoc.edtParamsChange(Sender: TObject);
var
  iIndex: integer;
begin
  iIndex := lbActions.ItemIndex;
  if (edbActionName.Tag = 1) or (iIndex < 0) then Exit;
  TExtActionCommand(lbActions.Items.Objects[iIndex]).Params := edtParams.Text;
end;

{ TfrmOptionsFileAssoc.deStartPathChange }
procedure TfrmOptionsFileAssoc.deStartPathChange(Sender: TObject);
var
  iIndex: integer;
begin
  iIndex := lbActions.ItemIndex;
  if (edbActionName.Tag = 1) or (iIndex < 0) then Exit;
  TExtActionCommand(lbActions.Items.Objects[iIndex]).StartPath := deStartPath.Text;
  if deStartPath.Directory <> deStartPath.Text then
    deStartPath.Directory := deStartPath.Text;
end;

{ TfrmOptionsFileAssoc.miActionsClick }
// The "tag" of the menu item calling this will give us information about the task to do.

// xxxx xx10 : Bit 1 & 0 indicate if user wants "Open", "View", "Edit" or "Custom action".
// xxx4 xxxx : Bit 4 indicates if user wants to specify immediately a .exe file for the command. (0=no, 1=yes).
// xx5x xxxx : Bit 5 indicates if user wants to "Add" or "Insert" action in the current list. (0=add, 1=insert).

procedure TfrmOptionsFileAssoc.miActionsClick(Sender: TObject);
var
  miMenuItem: TMenuItem absolute Sender;
  I, iDispatcher: integer;
  ExtAction: TExtAction;
  sActionWords: string = '';
  sCommandFilename: string = '';
begin
  with Sender as TComponent do iDispatcher := tag;

  // Transform the task in "add" if currently there is no action, nothing selected, any incoherence.
  if (Exts.Items[lbFileTypes.ItemIndex].ActionList.Count = 0) or (lbActions.Items.Count = 0) or (lbActions.ItemIndex = -1) then iDispatcher := (iDispatcher and $DF);

  if iDispatcher and $03 = $03 then
    gFileAssociationLastCustomAction := InputBox(GetTitle + ' - ' + gbActions.Hint, rsMsgEnterCustomAction, gFileAssociationLastCustomAction);

  case (iDispatcher and $DF) of
    $00: sActionWords := 'Open';
    $01: sActionWords := 'View';
    $02: sActionWords := 'Edit';
    $03: sActionWords := gFileAssociationLastCustomAction;
    $10: OpenDialog.Title := rsMsgSelectExecutableFile + ' "Open"...';
    $11: OpenDialog.Title := rsMsgSelectExecutableFile + ' "View"...';
    $12: OpenDialog.Title := rsMsgSelectExecutableFile + ' "Edit"...';
    $13: OpenDialog.Title := rsMsgSelectExecutableFile + ' "' + gFileAssociationLastCustomAction + '"...';
  end;

  case iDispatcher and $10 of
    $10:
    begin
      if OpenDialog.Execute then
      begin
        sCommandFilename := OpenDialog.Filename;

        case (iDispatcher and $03) of
          $00: sActionWords := 'Open ' + rsMsgWithActionWith + ' ' + ExtractFilename(OpenDialog.Filename);
          $01: sActionWords := 'View ' + rsMsgWithActionWith + ' ' + ExtractFilename(OpenDialog.Filename);
          $02: sActionWords := 'Edit ' + rsMsgWithActionWith + ' ' + ExtractFilename(OpenDialog.Filename);
          $03: sActionWords := gFileAssociationLastCustomAction + ' ' + rsMsgWithActionWith + ' ' + ExtractFilename(OpenDialog.Filename);
        end;
      end;
    end;
  end;

  case pmActions.Tag of
    ACTUAL_ADD_ACTION:
    begin
      ExtAction := Exts.Items[lbFileTypes.ItemIndex];

      // insert/add action to TExts object
      case iDispatcher and $20 of
        $20: // it is an "insert"
        begin
          I := lbActions.ItemIndex;

          case (iDispatcher and $DF) of
            $00, $01, $02, $03: ExtAction.ActionList.Insert(I, TExtActionCommand.Create(sActionWords, '', '', ''));
            $10, $11, $12, $13: ExtAction.ActionList.Insert(I, TExtActionCommand.Create(sActionWords, sCommandFilename, '%p', ''));
          end;

          // add action to actions listbox
          lbActions.Items.InsertObject(I, '', ExtAction.ActionList.ExtActionCommand[I]);
        end
        else
        begin // it is a "add"
          case (iDispatcher and $DF) of
            $00, $01, $02, $03: I := ExtAction.ActionList.Add(TExtActionCommand.Create(sActionWords, '', '', ''));
            $10, $11, $12, $13: I := ExtAction.ActionList.Add(TExtActionCommand.Create(sActionWords, sCommandFilename, '%p', ''));
          end;

          // add action to actions listbox
          lbActions.Items.AddObject(ExtAction.ActionList.ExtActionCommand[I].ActionName, ExtAction.ActionList.ExtActionCommand[I]);
        end;
      end;

      lbActions.ItemIndex := I;
      edbActionNameChange(edbActionName); //<--Trig this event to force redraw of lbActions current selected element because in case of switch from "open" to identical "open" for exemple, "edbActionChange" is not trig and so our element in the list is not drawn!

      // Update icon if possible, if necessary
      case (iDispatcher and $10) of
        $10: if Exts.Items[lbFileTypes.ItemIndex].Icon = '' then edIconFileName.Text := OpenDialog.FileName; //No quote required here! So "sCommandFilename" is not used.
      end;

      UpdateEnabledButtons;

      if edbActionName.CanFocus then
      begin
        edbActionName.SetFocus;
        edbActionName.SelStart := UTF8Length(edbActionName.Text);
      end;
    end;

    SET_ACTION_WORD:
    begin
      case (iDispatcher and $DF) of
        $00, $01, $02, $03: edbActionName.Text := sActionWords;
        $10, $11, $12, $13:
        begin
          edbActionName.Text := sActionWords;
          fneCommand.Text := sCommandFilename;
          edtParams.Text := '%p';
        end;
      end;
    end;

    else
    begin
      if miMenuItem.Name = 'miOpen' then
        edbActionName.Text := 'Open'
      else if miMenuItem.Name = 'miView' then
        edbActionName.Text := 'View'
      else if miMenuItem.Name = 'miEdit' then
        edbActionName.Text := 'Edit';
    end;
  end;

end;

{ TfrmOptionsFileAssoc.miCommandsClick }
procedure TfrmOptionsFileAssoc.miCommandsClick(Sender: TObject);
begin
  with Sender as TComponent do
  begin
    case tag of
      2: fneCommand.SelText := '{!VIEWER}';
      3: fneCommand.SelText := '{!DC-VIEWER}';
      4: fneCommand.SelText := '{!EDITOR}';
      5: fneCommand.SelText := '{!DC-EDITOR}';
      6: fneCommand.SelText := '{!SHELL}';
      7:
      begin
        fneCommand.SelText := fneCommand.Text + '<??>';
        fneCommand.SetFocus;
        fneCommand.SelStart := Pos('?>', fneCommand.Text) - 1;
      end;
    end;
  end;
end;

{ TfrmOptionsFileAssoc.pnlRightSettingsResize }
procedure TfrmOptionsFileAssoc.pnlRightSettingsResize(Sender: TObject);
begin
  gbExts.Height := pnlRightSettings.ClientHeight div 4;
end;

{ TfrmOptionsFileAssoc.pnlSettingsResize }
procedure TfrmOptionsFileAssoc.pnlSettingsResize(Sender: TObject);
begin
  pnlLeftSettings.Width := pnlSettings.ClientWidth div 3;
end;

{ TfrmOptionsFileAssoc.sbtnIconClick }
procedure TfrmOptionsFileAssoc.sbtnIconClick(Sender: TObject);
var
  sFileName: string;
begin
  sFileName := mbExpandFileName(edIconFileName.Text);
  if ShowOpenIconDialog(Self, sFileName) then
    edIconFileName.Text := sFileName;
end;

{ TfrmOptionsFileAssoc.InsertAddSingleExtensionToLists }
function TfrmOptionsFileAssoc.InsertAddSingleExtensionToLists(sExt: string; iInsertPosition: integer): boolean;
begin
  Result := False;

  if (sExt <> '') then
  begin
    if Exts.Items[lbFileTypes.ItemIndex].Extensions.IndexOf(sExt) = -1 then
    begin
      if iInsertPosition = -1 then
        iInsertPosition := lbExts.Items.Add(sExt)
      else
        lbExts.Items.Insert(iInsertPosition, sExt);

      lbExts.ItemIndex := iInsertPosition;

      //add extension in TExts object
      Exts.Items[lbFileTypes.ItemIndex].Extensions.Insert(iInsertPosition, sExt);

      Result := True;
    end
    else
    begin
      lbExts.ItemIndex := Exts.Items[lbFileTypes.ItemIndex].Extensions.IndexOf(sExt);
    end;

    UpdateEnabledButtons;
  end;
end;

{ TfrmOptionsFileAssoc.InsertAddExtensionToLists }
procedure TfrmOptionsFileAssoc.InsertAddExtensionToLists(sParamExt: string; iPositionToInsert: integer);
var
  iIndex: integer;
  sExt: string;
begin
  sParamExt := sParamExt + '|';
  while Pos('|', sParamExt) <> 0 do
  begin
    iIndex := Pos('|', sParamExt);
    sExt := Copy(sParamExt, 1, iIndex - 1);
    Delete(sParamExt, 1, iIndex);
    if InsertAddSingleExtensionToLists(sExt, iPositionToInsert) then
      if iPositionToInsert <> -1 then Inc(iPositionToInsert);
  end;
end;

{ TfrmOptionsFileAssoc.btnInsertAddExtClick }
procedure TfrmOptionsFileAssoc.btnInsertAddExtClick(Sender: TObject);
var
  sExt: string;
  Dispatcher: integer;
begin
  with Sender as TComponent do Dispatcher := tag;
  if (lbExts.Items.Count = 0) or (lbExts.ItemIndex = -1) then Dispatcher := 0;
  sExt := InputBox(GetTitle + ' - ' + gbExts.Hint, rsMsgEnterFileExt, '');
  InsertAddExtensionToLists(sExt, ifthen((Dispatcher = 0), -1, lbExts.ItemIndex));
  if lbExts.CanFocus then lbExts.SetFocus;
end;

{ TfrmOptionsFileAssoc.btnParametersHelperClick }
procedure TfrmOptionsFileAssoc.btnParametersHelperClick(Sender: TObject);
begin
  pmVariableParamsHelper.PopUp;
end;

{ TfrmOptionsFileAssoc.btnRelativeCommandClick }
procedure TfrmOptionsFileAssoc.btnRelativeCommandClick(Sender: TObject);
begin
  fneCommand.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneCommand, pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsFileAssoc.btnRelativePathIconClick }
procedure TfrmOptionsFileAssoc.btnRelativePathIconClick(Sender: TObject);
begin
  edIconFileName.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edIconFileName, pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsFileAssoc.btnStartPathPathHelperClick }
procedure TfrmOptionsFileAssoc.btnStartPathPathHelperClick(Sender: TObject);
begin
  deStartPath.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(deStartPath, pfPATH);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsFileAssoc.btnStartPathVarHelperClick }
procedure TfrmOptionsFileAssoc.btnStartPathVarHelperClick(Sender: TObject);
begin
  pmVariableStartPathHelper.PopUp;
end;

{ TfrmOptionsFileAssoc.edIconFileNameChange }
procedure TfrmOptionsFileAssoc.edIconFileNameChange(Sender: TObject);
begin
  if not FUpdatingControls then
    SetIconFileName(edIconFileName.Text);
end;

{ TfrmOptionsFileAssoc.FrameResize }
procedure TfrmOptionsFileAssoc.FrameResize(Sender: TObject);
begin
  lbExts.Columns := (lbExts.Width div 120);
end;

{ TfrmOptionsFileAssoc.btnRemoveExtClick }
procedure TfrmOptionsFileAssoc.btnRemoveExtClick(Sender: TObject);
var
  I: integer;
begin
  // remove extension from extensions listbox
  with lbExts do
  begin
    I := ItemIndex;
    if I < 0 then Exit;
    Items.Delete(I);
    if I < Items.Count then
      ItemIndex := I
    else
    if (I - 1) < Items.Count then
      ItemIndex := (I - 1);
  end;
  // remove extension from TExts object
  Exts.Items[lbFileTypes.ItemIndex].Extensions.Delete(I);
  UpdateEnabledButtons;
  if lbExts.CanFocus then lbExts.SetFocus;
end;

{ TfrmOptionsFileAssoc.btnUpActClick }
procedure TfrmOptionsFileAssoc.btnUpActClick(Sender: TObject);
var
  I: integer;
begin
  // move action in actions listbox
  with lbActions do
  begin
    Tag := 1; // start moving
    I := ItemIndex;
    if I = -1 then exit;
    if I > 0 then
    begin
      Items.Move(I, I - 1);
      ItemIndex := I - 1;
    end;
  end;

  // move action in TExts object
  with lbFileTypes do
  begin
    Exts.Items[ItemIndex].ActionList.Move(I, I - 1);
  end;
  lbActions.Tag := 0; // end moving
  UpdateEnabledButtons;
  if lbActions.CanFocus then lbActions.SetFocus;
end;

{ TfrmOptionsFileAssoc.lbActionsDragDrop }
procedure TfrmOptionsFileAssoc.lbActionsDragDrop(Sender, Source: TObject; X, Y: integer);
var
  SrcIndex, DestIndex: integer;
begin
  // Validate if the move is okay
  SrcIndex := lbActions.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbActions.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lbActions.Count) then
    DestIndex := lbActions.Count - 1;

  // exchange positions in actions listbox
  lbActions.Tag := 1; // start moving

  try
    lbActions.Items.Move(SrcIndex, DestIndex);
    lbActions.ItemIndex := DestIndex;

    // exchange actions in TExts object
    Exts.Items[lbFileTypes.ItemIndex].ActionList.Move(SrcIndex, DestIndex);
  finally
    lbActions.Tag := 0; // end moving
  end;

  UpdateEnabledButtons;
end;

{ TfrmOptionsFileAssoc.btnDownActClick }
procedure TfrmOptionsFileAssoc.btnDownActClick(Sender: TObject);
var
  I: integer;
begin
  // move action in actions listbox
  with lbActions do
  begin
    Tag := 1; // start moving
    I := ItemIndex;
    if I = -1 then exit;
    if (I < Items.Count - 1) and (I > -1) then
    begin
      Items.Move(I, I + 1);
      ItemIndex := I + 1;
    end;
  end;
  // move action in TExts object
  with lbFileTypes do
  begin
    Exts.Items[ItemIndex].ActionList.Move(I, I + 1);
  end;
  lbActions.Tag := 0; // end moving
  UpdateEnabledButtons;
  if lbActions.CanFocus then lbActions.SetFocus;
end;

{ TfrmOptionsFileAssoc.btnEditExtClick }
procedure TfrmOptionsFileAssoc.btnEditExtClick(Sender: TObject);
var
  iRememberIndex: integer;
  sExt: string;
begin
  iRememberIndex := lbExts.ItemIndex;
  if iRememberIndex < 0 then Exit;

  // change extension from extensions listbox
  sExt := InputBox(GetTitle + ' - ' + gbExts.Hint, rsMsgEnterFileExt, lbExts.Items[iRememberIndex]);
  if sExt <> lbExts.Items[iRememberIndex] then
  begin
    btnRemoveExtClick(btnRemoveExt); //We remove the old value
    if iRememberIndex >= lbExts.Items.Count then iRememberIndex := -1;
    InsertAddExtensionToLists(sExt, iRememberIndex); //We may add new one, maybe not, maybe bunch of them, etc.
  end;
  if lbExts.CanFocus then lbExts.SetFocus;
end;

{ TfrmOptionsFileAssoc.btnInsertAddActClick }
procedure TfrmOptionsFileAssoc.btnInsertAddActClick(Sender: TObject);
var
  iSubMenu: integer;
begin
  pmActions.Tag := ACTUAL_ADD_ACTION;
  for iSubMenu := 0 to pred(pmActions.Items.Count) do
    with pmActions.Items[iSubMenu] do
      Tag := ifthen((TComponent(Sender).Tag = $20), (Tag or $20), (Tag and $DF));
  pmActions.PopUp();
end;

{ TfrmOptionsFileAssoc.btnActionsClick }
procedure TfrmOptionsFileAssoc.btnActionsClick(Sender: TObject);
begin
  pmActions.tag := SET_ACTION_WORD;
  pmActions.PopUp();
end;

{ TfrmOptionsFileAssoc.btnCloneActClick }
procedure TfrmOptionsFileAssoc.btnCloneActClick(Sender: TObject);
var
  ExtAction: TExtAction;
  I: integer;
begin
  ExtAction := Exts.Items[lbFileTypes.ItemIndex];

  I := lbActions.ItemIndex;
  if (I + 1) <= pred(ExtAction.ActionList.Count) then
  begin
    ExtAction.ActionList.Insert(I + 1, ExtAction.ActionList.ExtActionCommand[I].CloneExtAction); // add action to TExtAction
    lbActions.Items.InsertObject(I + 1, '', ExtAction.ActionList.ExtActionCommand[I + 1]); // add action to actions listbox
  end
  else
  begin
    ExtAction.ActionList.Add(ExtAction.ActionList.ExtActionCommand[I].CloneExtAction); // add action to TExtAction
    lbActions.Items.AddObject(ExtAction.ActionList.ExtActionCommand[I + 1].ActionName, ExtAction.ActionList.ExtActionCommand[I + 1]); // add action to actions listbox
  end;

  lbActions.ItemIndex := I + 1;
  edbActionNameChange(edbActionName); //<--Trig this event to force redraw of lbActions current selected element because in case of switch from "open" to identical "open" for exemple, "edbActionChange" is not trig and so our element in the list is not drawn!

  UpdateEnabledButtons;

  if edbActionName.CanFocus then
  begin
    edbActionName.SetFocus;
    edbActionName.SelStart := UTF8Length(edbActionName.Text);
  end;
end;

{ TfrmOptionsFileAssoc.btnRemoveActClick }
procedure TfrmOptionsFileAssoc.btnRemoveActClick(Sender: TObject);
var
  I: integer;
begin
  // remove action from actions listbox
  with lbActions do
  begin
    I := ItemIndex;
    if I < 0 then Exit;
    Items.Delete(I);

    if I < Items.Count then
      ItemIndex := I
    else
    if (I - 1) < Items.Count then
      ItemIndex := (I - 1);
  end;
  // remove action from TExts object
  with lbFileTypes do
  begin
    Exts.Items[ItemIndex].ActionList.Delete(I);
  end;

  UpdateEnabledButtons;
  if lbActions.CanFocus then lbActions.SetFocus;
end;

{ TfrmOptionsFileAssoc.btnCommandsClick }
procedure TfrmOptionsFileAssoc.btnCommandsClick(Sender: TObject);
begin
  pmCommands.PopUp();
end;

{ TfrmOptionsFileAssoc.FreeIcon }
procedure TfrmOptionsFileAssoc.FreeIcon(iIndex: integer);
begin
  with lbFileTypes do
  begin
    Canvas.Lock;
    try
      if Assigned(Items.Objects[iIndex]) then
      begin
        Items.Objects[iIndex].Free;
        Items.Objects[iIndex] := nil;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

{ TfrmOptionsFileAssoc.SetIconFileName }
procedure TfrmOptionsFileAssoc.SetIconFileName(const sFileName: string);
var
  bmpTemp: TBitmap;
  Index: integer;
begin
  if sFileName <> EmptyStr then
  begin
    bmpTemp := PixMapManager.LoadBitmapEnhanced(sFileName, 32, True, sbtnIcon.Color);
    if Assigned(bmpTemp) then
    begin
      sbtnIcon.Glyph.Assign(bmpTemp);
      FreeAndNil(bmpTemp);
    end
    else
      sbtnIcon.Glyph.Clear;
  end
  else
    sbtnIcon.Glyph.Clear;

  Index := lbFileTypes.ItemIndex;
  if (Index >= 0) and (Index < Exts.Count) then
  begin
    FreeIcon(Index);
    // save icon for use in OnDrawItem procedure
    if sFileName <> EmptyStr then lbFileTypes.Items.Objects[Index] := PixMapManager.LoadBitmapEnhanced(sFileName, gIconsSize, True, Color);
    lbFileTypes.Repaint;

    Exts.Items[Index].Icon := sFileName;
    Exts.Items[Index].IconIndex := -1;
  end;
end;

{ TfrmOptionsFileAssoc.SetMinimumSize }
procedure TfrmOptionsFileAssoc.SetMinimumSize;
begin
  gbFileTypes.Constraints.MinWidth :=
    gbFileTypes.BorderSpacing.Left + btnRemoveType.Left + btnRemoveType.Width + 5 + // space between
    btnRenameType.Width + gbFileTypes.Width - (btnRenameType.Left + btnRenameType.Width) + gbFileTypes.BorderSpacing.Right;

  pnlLeftSettings.Constraints.MinWidth :=
    gbFileTypes.Constraints.MinWidth + gbFileTypes.BorderSpacing.Around;

  Constraints.MinWidth :=
    pnlLeftSettings.Constraints.MinWidth + pnlLeftSettings.BorderSpacing.Left + pnlLeftSettings.BorderSpacing.Right + pnlLeftSettings.BorderSpacing.Around + pnlRightSettings.Constraints.MinWidth + pnlRightSettings.BorderSpacing.Left +
    pnlRightSettings.BorderSpacing.Right + pnlRightSettings.BorderSpacing.Around;
end;

{ TfrmOptionsFileAssoc.MakeUsInPositionToWorkWithActiveFile }
procedure TfrmOptionsFileAssoc.MakeUsInPositionToWorkWithActiveFile;
var
  aFile: TFile;
  InnerActionList: TExtActionList;
  IndexOfFirstPossibleFileType: integer;
  sFileType, sDummy: string;
  ExtAction: TExtAction;
  iInsertPosition: integer;
  iSelectedFileType: integer = -1;
  InnerFileTypeNameList: TStringList;
begin
  aFile := frmMain.ActiveFrame.CloneActiveFile;
  if Assigned(aFile) then
  begin
    if (not aFile.IsDirectory) and (not aFile.IsLink) and (not (aFile.Extension = '')) then
    begin
      InnerActionList := TExtActionList.Create;
      try
        if gExts.GetExtActions(aFile, InnerActionList, @IndexOfFirstPossibleFileType) then
        begin
          if (IndexOfFirstPossibleFileType <> -1) and (lbFileTypes.Items.Count > IndexOfFirstPossibleFileType) then //Double verification, but unnecessary.
          begin
            lbFileTypes.ItemIndex := IndexOfFirstPossibleFileType;
            lbFileTypesSelectionChange(lbFileTypes, False);
            lbExts.ItemIndex := lbExts.Items.IndexOf(aFile.Extension);
            if (lbExts.ItemIndex = -1) and (lbExts.Items.Count > 0) then lbExts.ItemIndex := 0;
          end;
        end
        else
        begin
          // Extension of current selected file is not in our file associations list.
          if gOfferToAddToFileAssociations then
          begin
            InnerFileTypeNameList := TStringList.Create;
            try
              InnerFileTypeNameList.Assign(lbFileTypes.Items);
              InnerFileTypeNameList.Insert(0, Format(rsMsgCreateANewFileType, [UpperCase(aFile.Extension)]));

              if ShowInputListBox(rsMsgTitleExtNotInFileType, Format(rsMsgSekectFileType, [aFile.Extension]), InnerFileTypeNameList, sDummy, iSelectedFileType) then
              begin
                if iSelectedFileType <> -1 then
                begin
                  if iSelectedFileType <> 0 then
                  begin
                    Dec(iSelectedFileType);

                    //1. Select the specified file type
                    lbFileTypes.ItemIndex := iSelectedFileType;
                    lbFileTypesSelectionChange(lbFileTypes, False);

                    //2. Add the extension to listbox AND structure
                    iInsertPosition := lbExts.Items.Add(aFile.Extension);
                    lbExts.ItemIndex := iInsertPosition;
                    Exts.Items[lbFileTypes.ItemIndex].Extensions.Add(aFile.Extension);
                  end
                  else
                  begin
                    sFileType := UpperCase(aFile.Extension) + ' ' + rsSimpleWordFiles;

                    if InputQuery(rsMsgTitleExtNotInFileType, Format(rsMsgEnterNewFileTypeName, [aFile.Extension]), sFileType) then
                    begin
                      //1. Create the file type
                      ExtAction := TExtAction.Create;
                      ExtAction.IconIndex := -1;
                      ExtAction.Name := sFileType;
                      lbFileTypes.Items.AddObject(ExtAction.Name, nil);

                      //2. Add it to our structure AND listbox
                      Exts.AddItem(ExtAction);
                      lbFileTypes.ItemIndex := pred(lbFileTypes.Items.Count);

                      //3. Add the extension to listbox AND structure
                      iInsertPosition := lbExts.Items.Add(aFile.Extension);
                      lbExts.ItemIndex := iInsertPosition;
                      Exts.Items[lbFileTypes.ItemIndex].Extensions.Add(aFile.Extension);

                      //4. Select an action for "open"
                      pmActions.tag := ACTUAL_ADD_ACTION;
                      miActionsClick(miOpenWith);

                      //5. Refresh display to have appropriate button shown.
                      UpdateEnabledButtons;
                    end;
                  end;
                end;
              end;

            finally
              FreeAndNil(InnerFileTypeNameList);
            end;
          end;
        end;

      finally
        FreeAndNil(InnerActionList);
      end;

      FreeAndNil(aFile);
    end;
  end;
  if lbFileTypes.CanFocus then lbFileTypes.SetFocus;
end;

procedure TfrmOptionsFileAssoc.actSelectFileTypeExecute(Sender: TObject);
begin
  if lbFileTypes.CanFocus then
    lbFileTypes.SetFocus;
end;

procedure TfrmOptionsFileAssoc.actSelectIconExecute(Sender: TObject);
begin
  if edIconFileName.CanFocus then
    edIconFileName.SetFocus;
end;

procedure TfrmOptionsFileAssoc.actSelectExtensionsExecute(Sender: TObject);
begin
  if lbExts.CanFocus then
    lbExts.SetFocus;
end;

procedure TfrmOptionsFileAssoc.actSelectActionsExecute(Sender: TObject);
begin
  if lbActions.CanFocus then
    lbActions.SetFocus;
end;

procedure TfrmOptionsFileAssoc.actSelectActionDescriptionExecute(Sender: TObject);
begin
  if edbActionName.CanFocus then
    edbActionName.SetFocus;
end;

end.

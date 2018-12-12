{
   Double Commander
   -------------------------------------------------------------------------
   Archivers options page

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
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fOptionsArchivers;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  DividerBevel, Classes, SysUtils, StdCtrls, ExtCtrls, ComCtrls, EditBtn,
  Buttons, Menus, Dialogs,
  //DC
  uMultiArc, fOptionsFrame, Controls;
type

  { TfrmOptionsArchivers }
  TfrmOptionsArchivers = class(TOptionsEditor)
    chkFileNameOnlyList: TCheckBox;
    pnlFileNameOnlyList: TPanel;
    pnlArchiverListbox: TPanel;
    lblArchiverListBox: TLabel;
    lbxArchiver: TListBox;
    splArchiver: TSplitter;
    pnlArchiverCommands: TPanel;
    pnlArchiverButtons: TPanel;
    chkArchiverEnabled: TCheckBox;
    btnArchiverApply: TBitBtn;
    btnArchiverAdd: TBitBtn;
    btnArchiverCopy: TBitBtn;
    btnArchiverRename: TBitBtn;
    btnArchiverDelete: TBitBtn;
    btnArchiverOther: TBitBtn;
    pcArchiverCommands: TPageControl;
    tbArchiverGeneral: TTabSheet;
    lblArchiverDescription: TLabel;
    edtArchiverDescription: TEdit;
    lblArchiverArchiver: TLabel;
    edtArchiverArchiver: TEdit;
    btnArchiverSelectFileArchiver: TSpeedButton;
    btnArchiverRelativer: TSpeedButton;
    lblArchiverExtension: TLabel;
    edtArchiverExtension: TEdit;
    lblArchiverList: TLabel;
    edtArchiverList: TEdit;
    btnArchiverListHelper: TSpeedButton;
    lblArchiverListStart: TLabel;
    edtArchiverListStart: TEdit;
    lblArchiverListEnd: TLabel;
    edtArchiverListEnd: TEdit;
    lblArchiverListFormat: TLabel;
    memArchiverListFormat: TMemo;
    lblArchiverExtract: TLabel;
    edtArchiverExtract: TEdit;
    btnArchiverExtractHelper: TSpeedButton;
    lblArchiverAdd: TLabel;
    edtArchiverAdd: TEdit;
    btnArchiverAddHelper: TSpeedButton;
    tbArchiverAdditional: TTabSheet;
    lblArchiverDelete: TLabel;
    edtArchiverDelete: TEdit;
    btnArchiverDeleteHelper: TSpeedButton;
    lblArchiverTest: TLabel;
    edtArchiverTest: TEdit;
    btnArchiverTestHelper: TSpeedButton;
    lblArchiverExtractWithoutPath: TLabel;
    edtArchiverExtractWithoutPath: TEdit;
    btnArchiverExtractWithoutPathHelper: TSpeedButton;
    lblArchiverSelfExtract: TLabel;
    edtArchiverSelfExtract: TEdit;
    btnArchiverSelfExtractHelper: TSpeedButton;
    lblArchiverPasswordQuery: TLabel;
    edtArchiverPasswordQuery: TEdit;
    bvlArchiverIds: TDividerBevel;
    lblArchiverIds: TLabel;
    edtArchiverId: TEdit;
    lblArchiverIdPosition: TLabel;
    edtArchiverIdPosition: TEdit;
    lblArchiverIdSeekRange: TLabel;
    edtArchiverIdSeekRange: TEdit;
    bvlArchiverParsingMode: TDividerBevel;
    ckbArchiverUnixPath: TCheckBox;
    ckbArchiverWindowsPath: TCheckBox;
    ckbArchiverUnixFileAttributes: TCheckBox;
    ckbArchiverWindowsFileAttributes: TCheckBox;
    bvlArchiverOptions: TDividerBevel;
    chkArchiverMultiArcOutput: TCheckBox;
    chkArchiverMultiArcDebug: TCheckBox;
    pmArchiverOther: TPopupMenu;
    miArchiverAutoConfigure: TMenuItem;
    miArchiverDiscardModification: TMenuItem;
    miSeparator1: TMenuItem;
    miArchiverSortArchivers: TMenuItem;
    miArchiverDisableAll: TMenuItem;
    miArchiverEnableAll: TMenuItem;
    miSeparator2: TMenuItem;
    miArchiverExport: TMenuItem;
    miArchiverImport: TMenuItem;
    pmArchiverPathHelper: TPopupMenu;
    pmArchiverParamHelper: TPopupMenu;
    SaveArchiverDialog: TSaveDialog;
    OpenArchiverDialog: TOpenDialog;
    procedure chkFileNameOnlyListChange(Sender: TObject);
    procedure lbxArchiverSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure lbxArchiverDragOver(Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure lbxArchiverDragDrop(Sender, {%H-}Source: TObject; {%H-}X, Y: integer);
    procedure edtAnyChange(Sender: TObject);
    procedure ckbArchiverUnixPathChange(Sender: TObject);
    procedure ckbArchiverWindowsPathChange(Sender: TObject);
    procedure ckbArchiverUnixFileAttributesChange(Sender: TObject);
    procedure ckbArchiverWindowsFileAttributesChange(Sender: TObject);
    procedure chkArchiverEnabledChange(Sender: TObject);
    procedure SetConfigurationState(bConfigurationSaved: boolean);
    procedure SetControlsState(bWantedState: boolean);
    procedure SetActiveButtonsBasedOnArchiversQuantity;
    procedure ActualSaveCurrentMultiArcItem;
    procedure btnArchiverApplyClick(Sender: TObject);
    procedure btnArchiverAddClick(Sender: TObject);
    procedure btnArchiverCopyClick(Sender: TObject);
    procedure btnArchiverRenameClick(Sender: TObject);
    procedure btnArchiverDeleteClick(Sender: TObject);
    procedure btnArchiverOtherClick(Sender: TObject);
    procedure miArchiverAutoConfigureClick(Sender: TObject);
    procedure miArchiverDiscardModificationClick(Sender: TObject);
    procedure miArchiverSortArchiversClick(Sender: TObject);
    procedure miAdjustEnableAllClick(Sender: TObject);
    procedure miArchiverExportClick(Sender: TObject);
    procedure miArchiverImportClick(Sender: TObject);
    procedure miHelperClick(Sender: TObject);
    procedure btnHelperClick(Sender: TObject);
    procedure btnArchiverSelectFileArchiverClick(Sender: TObject);
    procedure btnArchiverRelativerClick(Sender: TObject);
    procedure PopulateParamHelperMenu;
  private
    MultiArcListTemp: TMultiArcList;
    bCurrentlyFilling: boolean;
    bCurrentlyLoadingSettings: boolean;
    edtHelperRequested: TEdit; //Used as a kind of pointer of TEdit when it's time to use the % helper.
    procedure FillListBoxWithArchiverList;
  protected
    procedure Init; override;
    procedure Load; override;
    procedure Done; override;
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

  //DC
  DCStrUtils, uGlobs, uLng, uSpecialDir, uGlobsPaths, uShowMsg;

const
  CONFIG_NOTSAVED = False;
  CONFIG_SAVED = True;

var
  iLastDisplayedIndex: integer = -1;

{ TfrmOptionsArchivers }

{ TfrmOptionsArchivers.Init }
procedure TfrmOptionsArchivers.Init;
begin
  OpenArchiverDialog.Filter := ParseLineToFileFilter([rsFilterArchiverConfigFiles, '*.ini;*.addon', rsFilterAnyFiles, '*.*']);
  SaveArchiverDialog.Filter := ParseLineToFileFilter([rsFilterArchiverConfigFiles, '*.ini', rsFilterAnyFiles, '*.*']);
end;

{ TfrmOptionsArchivers.Load }
procedure TfrmOptionsArchivers.Load;
begin
  bCurrentlyLoadingSettings := True;
  bCurrentlyFilling := True;
  btnArchiverSelectFileArchiver.Hint := rsOptArchiverArchiver;
  FreeAndNil(MultiArcListTemp);
  MultiArcListTemp := gMultiArcList.Clone;
  FillListBoxWithArchiverList;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmArchiverPathHelper, mp_PATHHELPER, nil);
  PopulateParamHelperMenu;
  pcArchiverCommands.ActivePage := tbArchiverGeneral;
end;

{ TfrmOptionsArchivers.Done }
procedure TfrmOptionsArchivers.Done;
begin
  if lbxArchiver.ItemIndex <> -1 then
    if lbxArchiver.ItemIndex < MultiArcListTemp.Count then
      iLastDisplayedIndex := lbxArchiver.ItemIndex; // Let's preserve the last item we were at to select it if we come back here in this session.
  FreeAndNil(MultiArcListTemp);
end;

{ TfrmOptionsArchivers.Save }
function TfrmOptionsArchivers.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  if not lbxArchiver.Enabled then
    ActualSaveCurrentMultiArcItem;
  MultiArcListTemp.SaveToFile(gpCfgDir + sMULTIARC_FILENAME);
  FreeAndNil(gMultiArcList);
  gMultiArcList := MultiArcListTemp.Clone;
  LastLoadedOptionSignature := ComputeCompleteOptionsSignature;
end;

{ TfrmOptionsArchivers.GetIconIndex }
class function TfrmOptionsArchivers.GetIconIndex: integer;
begin
  Result := 18;
end;

{ TfrmOptionsArchivers.GetTitle }
class function TfrmOptionsArchivers.GetTitle: string;
begin
  Result := rsOptionsEditorArchivers;
end;

{ TfrmOptionsArchivers.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsArchivers.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  Result := False;
end;

{ TfrmOptionsArchivers.ExtraOptionsSignature }
function TfrmOptionsArchivers.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  if not lbxArchiver.Enabled then //If currently our Listbox is disabled, it's because we did at least one modification...
    Result := (LastLoadedOptionSignature xor $01) //...so let's make sure the reported signature for the whole thing is affected.
  else
    Result := MultiArcListTemp.ComputeSignature(CurrentSignature);
end;

{ TfrmOptionsArchivers.FillListBoxWithArchiverList }
procedure TfrmOptionsArchivers.FillListBoxWithArchiverList;
var
  I, iRememberIndex: integer;
begin
  bCurrentlyFilling := True;
  iRememberIndex := lbxArchiver.ItemIndex;
  lbxArchiver.Clear;
  for I := 0 to MultiArcListTemp.Count - 1 do lbxArchiver.Items.AddObject(MultiArcListTemp.Names[I], MultiArcListTemp[I]);
  pcArchiverCommands.Enabled := (lbxArchiver.Items.Count <> 0);
  chkArchiverEnabled.Enabled := (lbxArchiver.Items.Count <> 0);
  if lbxArchiver.Items.Count > 0 then
  begin
    if (iRememberIndex <> -1) and (iRememberIndex < lbxArchiver.Items.Count) then
      lbxArchiver.ItemIndex := iRememberIndex
    else
    if (iLastDisplayedIndex <> -1) and (iLastDisplayedIndex < lbxArchiver.Items.Count) then
      lbxArchiver.ItemIndex := iLastDisplayedIndex
    else
      lbxArchiver.ItemIndex := 0;
  end;
  SetActiveButtonsBasedOnArchiversQuantity;
  btnArchiverApply.Enabled := False;
  bCurrentlyFilling := False;
  lbxArchiverSelectionChange(lbxArchiver, False);
end;

{ TfrmOptionsArchivers.lbxArchiverSelectionChange }
procedure TfrmOptionsArchivers.lbxArchiverSelectionChange(Sender: TObject; User: boolean);
begin
  if not bCurrentlyFilling then
  begin
    bCurrentlyLoadingSettings := True;

    if lbxArchiver.ItemIndex < 0 then
    begin
      edtArchiverDescription.Text := EmptyStr;
      edtArchiverArchiver.Text := EmptyStr;
      edtArchiverExtension.Text := EmptyStr;
      edtArchiverList.Text := EmptyStr;
      edtArchiverListStart.Text := EmptyStr;
      edtArchiverListEnd.Text := EmptyStr;
      memArchiverListFormat.Lines.Clear;
      edtArchiverExtract.Text := EmptyStr;
      edtArchiverAdd.Text := EmptyStr;
      edtArchiverDelete.Text := EmptyStr;
      edtArchiverTest.Text := EmptyStr;
      edtArchiverExtractWithoutPath.Text := EmptyStr;
      edtArchiverSelfExtract.Text := EmptyStr;
      edtArchiverPasswordQuery.Text := EmptyStr;
      edtArchiverId.Text := EmptyStr;
      edtArchiverIdPosition.Text := EmptyStr;
      edtArchiverIdSeekRange.Text := EmptyStr;
      ckbArchiverUnixPath.Checked := False;
      ckbArchiverWindowsPath.Checked := False;
      ckbArchiverUnixFileAttributes.Checked := False;
      ckbArchiverWindowsFileAttributes.Checked := False;
      chkArchiverMultiArcOutput.Checked := False;
      chkArchiverMultiArcDebug.Checked := False;
      chkArchiverEnabled.Checked := False;
      pcArchiverCommands.Enabled := (lbxArchiver.Items.Count <> 0);
      chkArchiverEnabled.Enabled := (lbxArchiver.Items.Count <> 0);
    end
    else
    begin
      with TMultiArcItem(lbxArchiver.Items.Objects[lbxArchiver.ItemIndex]) do
      begin
        edtArchiverDescription.Text := FDescription;
        edtArchiverArchiver.Text := FArchiver;
        edtArchiverExtension.Text := FExtension;
        edtArchiverList.Text := FList;
        edtArchiverListStart.Text := FStart;
        edtArchiverListEnd.Text := FEnd;
        memArchiverListFormat.Lines.Assign(FFormat);
        edtArchiverExtract.Text := FExtract;
        edtArchiverAdd.Text := FAdd;
        edtArchiverDelete.Text := FDelete;
        edtArchiverTest.Text := FTest;
        edtArchiverExtractWithoutPath.Text := FExtractWithoutPath;
        edtArchiverSelfExtract.Text := FAddSelfExtract;
        edtArchiverPasswordQuery.Text := FPasswordQuery;
        edtArchiverId.Text := FID;
        edtArchiverIdPosition.Text := FIDPos;
        edtArchiverIdSeekRange.Text := FIDSeekRange;
        chkFileNameOnlyList.Checked:= mafFileNameList in FFlags;
        ckbArchiverUnixPath.Checked := (FFormMode and $01 <> $00);
        ckbArchiverWindowsPath.Checked := (FFormMode and $02 <> $00);
        ckbArchiverUnixFileAttributes.Checked := (FFormMode and $04 <> $00);
        ckbArchiverWindowsFileAttributes.Checked := (FFormMode and $08 <> $00);
        chkArchiverMultiArcOutput.Checked := FOutput;
        chkArchiverMultiArcDebug.Checked := FDebug;
        chkArchiverEnabled.Checked := FEnabled;
      end;
    end;
    chkFileNameOnlyListChange(chkFileNameOnlyList);
    SetControlsState(chkArchiverEnabled.Checked);

    SetConfigurationState(CONFIG_SAVED);
    bCurrentlyLoadingSettings := False;
  end;
end;

procedure TfrmOptionsArchivers.chkFileNameOnlyListChange(Sender: TObject);
var
  AEnabled: Boolean;
begin
  AEnabled:= (not chkFileNameOnlyList.Checked) and chkArchiverEnabled.Checked;
  edtArchiverList.Enabled:= AEnabled;
  btnArchiverListHelper.Enabled:= AEnabled;
  edtArchiverListStart.Enabled:= AEnabled;
  edtArchiverListEnd.Enabled:= AEnabled;
  memArchiverListFormat.Enabled:= AEnabled;
  edtAnyChange(Sender);
end;

{ TfrmOptionsArchivers.lbxArchiverDragOver }
procedure TfrmOptionsArchivers.lbxArchiverDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

{ TfrmOptionsArchivers.lbxArchiverDragDrop }
procedure TfrmOptionsArchivers.lbxArchiverDragDrop(Sender, Source: TObject; X, Y: integer);
var
  SrcIndex, DestIndex: integer;
begin
  SrcIndex := lbxArchiver.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbxArchiver.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lbxArchiver.Count) then
    DestIndex := lbxArchiver.Count - 1;

  lbxArchiver.Items.Move(SrcIndex, DestIndex);
  MultiArcListTemp.FList.Move(SrcIndex, DestIndex);
  lbxArchiver.ItemIndex := DestIndex;
  lbxArchiverSelectionChange(lbxArchiver, False);
end;

{ TfrmOptionsArchivers.edtAnyChange }
procedure TfrmOptionsArchivers.edtAnyChange(Sender: TObject);
begin
  if not bCurrentlyLoadingSettings then
    if lbxArchiver.Enabled then
      SetConfigurationState(CONFIG_NOTSAVED);
end;

{ TfrmOptionsArchivers.ckbArchiverUnixPathChange }
procedure TfrmOptionsArchivers.ckbArchiverUnixPathChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    if ckbArchiverWindowsPath.Checked then
      ckbArchiverWindowsPath.Checked := False;
  edtAnyChange(Sender);
end;

{ TfrmOptionsArchivers.ckbArchiverWindowsPathChange }
procedure TfrmOptionsArchivers.ckbArchiverWindowsPathChange(Sender: TObject);
begin
  if TCheckbox(Sender).Checked then
    if ckbArchiverUnixPath.Checked then
      ckbArchiverUnixPath.Checked := False;
  edtAnyChange(Sender);
end;

{ TfrmOptionsArchivers.ckbArchiverUnixFileAttributesChange }
procedure TfrmOptionsArchivers.ckbArchiverUnixFileAttributesChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    if ckbArchiverWindowsFileAttributes.Checked then
      ckbArchiverWindowsFileAttributes.Checked := False;
  edtAnyChange(Sender);
end;

{ TfrmOptionsArchivers.ckbArchiverWindowsFileAttributesChange }
procedure TfrmOptionsArchivers.ckbArchiverWindowsFileAttributesChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    if ckbArchiverUnixFileAttributes.Checked then
      ckbArchiverUnixFileAttributes.Checked := False;
  edtAnyChange(Sender);
end;

{ TfrmOptionsArchivers.chkArchiverEnabledChange }
procedure TfrmOptionsArchivers.chkArchiverEnabledChange(Sender: TObject);
begin
  if not bCurrentlyLoadingSettings then
  begin
    SetControlsState(chkArchiverEnabled.Checked);
    edtAnyChange(Sender);
  end;
end;

{ TfrmOptionsArchivers.SetConfigurationState }
procedure TfrmOptionsArchivers.SetConfigurationState(bConfigurationSaved: boolean);
begin
  if lbxArchiver.Enabled <> bConfigurationSaved then
  begin
    lbxArchiver.Enabled := bConfigurationSaved;
    btnArchiverApply.Enabled := not bConfigurationSaved;
    btnArchiverAdd.Enabled := bConfigurationSaved;
    btnArchiverCopy.Enabled := bConfigurationSaved;
    btnArchiverRename.Enabled := bConfigurationSaved;
    miArchiverImport.Enabled := bConfigurationSaved;
    miArchiverSortArchivers.Enabled := bConfigurationSaved;
    miArchiverExport.Enabled := bConfigurationSaved;
    miArchiverDiscardModification.Enabled := not bConfigurationSaved;
    miArchiverDisableAll.Enabled := bConfigurationSaved;
    miArchiverEnableAll.Enabled := bConfigurationSaved;

    if bConfigurationSaved = CONFIG_SAVED then
      lbxArchiver.Hint := ''
    else
      lbxArchiver.Hint := rsOptArchiveConfigureSaveToChange;
  end;
end;

{ TfrmOptionsArchivers.SetControlsState }
procedure TfrmOptionsArchivers.SetControlsState(bWantedState: boolean);
var
  iComponentIndex: integer;
begin
  if lbxArchiver.ItemIndex < 0 then Exit;
  TMultiArcItem(lbxArchiver.Items.Objects[lbxArchiver.ItemIndex]).FEnabled := bWantedState;

  if bWantedState <> edtArchiverDescription.Enabled then //Let's use "edtDescription" as a reference.
    for iComponentIndex := 0 to pred(ComponentCount) do
      if Components[iComponentIndex].Owner <> nil then
        if Components[iComponentIndex].InheritsFrom(TControl) then
          if (TControl(Components[iComponentIndex]).Parent = tbArchiverGeneral) or (TControl(Components[iComponentIndex]).Parent = tbArchiverAdditional) then
            if Components[iComponentIndex].Name <> chkArchiverEnabled.Name then
              TControl(Components[iComponentIndex]).Enabled := bWantedState;
end;

{ TfrmOptionsArchivers.SetActiveButtonsBasedOnArchiversQuantity }
procedure TfrmOptionsArchivers.SetActiveButtonsBasedOnArchiversQuantity;
begin
  btnArchiverCopy.Enabled := ((lbxArchiver.Items.Count > 0) and (lbxArchiver.Enabled));
  btnArchiverRename.Enabled := btnArchiverCopy.Enabled;
  btnArchiverDelete.Enabled := btnArchiverCopy.Enabled;
  miArchiverAutoConfigure.Enabled := btnArchiverCopy.Enabled;
  miArchiverSortArchivers.Enabled := ((lbxArchiver.Items.Count > 1) and (lbxArchiver.Enabled));
  miArchiverExport.Enabled := btnArchiverCopy.Enabled;
end;

{ TfrmOptionsArchivers.ActualSaveCurrentMultiArcItem }
procedure TfrmOptionsArchivers.ActualSaveCurrentMultiArcItem;
begin
  if lbxArchiver.ItemIndex < 0 then
    Exit;
  with TMultiArcItem(lbxArchiver.Items.Objects[lbxArchiver.ItemIndex]) do
  begin
    FDescription := edtArchiverDescription.Text;
    FArchiver := edtArchiverArchiver.Text;
    FExtension := edtArchiverExtension.Text;
    FList := edtArchiverList.Text;
    FStart := edtArchiverListStart.Text;
    FEnd := edtArchiverListEnd.Text;
    FFormat.Assign(memArchiverListFormat.Lines);
    FExtract := edtArchiverExtract.Text;
    FAdd := edtArchiverAdd.Text;
    FDelete := edtArchiverDelete.Text;
    FTest := edtArchiverTest.Text;
    FExtractWithoutPath := edtArchiverExtractWithoutPath.Text;
    FAddSelfExtract := edtArchiverSelfExtract.Text;
    FPasswordQuery := edtArchiverPasswordQuery.Text;
    FID := edtArchiverId.Text;
    FIDPos := edtArchiverIdPosition.Text;
    FIDSeekRange := edtArchiverIdSeekRange.Text;
    FFlags := [];
    if chkFileNameOnlyList.Checked then Include(FFlags, mafFileNameList);
    FFormMode := 0;
    if ckbArchiverUnixPath.Checked then  FFormMode := FFormMode or $01;
    if ckbArchiverWindowsPath.Checked then  FFormMode := FFormMode or $02;
    if ckbArchiverUnixFileAttributes.Checked then  FFormMode := FFormMode or $04;
    if ckbArchiverWindowsFileAttributes.Checked then  FFormMode := FFormMode or $08;
    FOutput := chkArchiverMultiArcOutput.Checked;
    FDebug := chkArchiverMultiArcDebug.Checked;
    SetConfigurationState(CONFIG_SAVED);
  end;
end;

{ TfrmOptionsArchivers.btnArchiverApplyClick }
procedure TfrmOptionsArchivers.btnArchiverApplyClick(Sender: TObject);
begin
  Save;
  if lbxArchiver.CanFocus then
    lbxArchiver.SetFocus;
end;

{ TfrmOptionsArchivers.btnArchiverAddClick }
procedure TfrmOptionsArchivers.btnArchiverAddClick(Sender: TObject);
var
  sName: string;
  MultiArcItem: TMultiArcItem;
begin
  if InputQuery(Caption, rsOptArchiveTypeName, sName) then
  begin
    MultiArcItem := TMultiArcItem.Create;
    MultiArcItem.FEnabled:=True;
    lbxArchiver.Items.AddObject(sName, MultiArcItem);
    MultiArcListTemp.Add(sName, MultiArcItem);
    lbxArchiver.ItemIndex := lbxArchiver.Items.Count - 1;
    lbxArchiverSelectionChange(lbxArchiver, False);
    pcArchiverCommands.Enabled := (lbxArchiver.Items.Count <> 0);
    chkArchiverEnabled.Enabled := (lbxArchiver.Items.Count <> 0);
    SetActiveButtonsBasedOnArchiversQuantity;
    if pcArchiverCommands.ActivePage<>tbArchiverGeneral then pcArchiverCommands.ActivePage:=tbArchiverGeneral;
    if edtArchiverDescription.CanFocus then edtArchiverDescription.SetFocus;
  end;
end;

{ TfrmOptionsArchivers.btnArchiverCopyClick }
procedure TfrmOptionsArchivers.btnArchiverCopyClick(Sender: TObject);
var
  ANewMultiArcItem: TMultiArcItem;
  sCurrentSelectedName, sNewName: string;
  iIndexCopy, iPosOpenPar, iNewInsertedPosition: integer;
begin
  if lbxArchiver.ItemIndex < 0 then Exit;

  sCurrentSelectedName := lbxArchiver.Items.Strings[lbxArchiver.ItemIndex];
  if LastDelimiter(')', sCurrentSelectedName) = length(sCurrentSelectedName) then
  begin
    iPosOpenPar := LastDelimiter('(', sCurrentSelectedName);
    if (iPosOpenPar > 0) and (iPosOpenPar > (length(sCurrentSelectedName) - 4)) then
      sCurrentSelectedName := LeftStr(sCurrentSelectedName, pred(pred(iPosOpenPar)));
  end;

  iIndexCopy := 2;
  while lbxArchiver.Items.IndexOf(Format('%s (%d)', [sCurrentSelectedName, iIndexCopy])) <> -1 do
    Inc(iIndexCopy);
  sNewName := Format('%s (%d)', [sCurrentSelectedName, iIndexCopy]);

  ANewMultiArcItem := TMultiArcItem(lbxArchiver.Items.Objects[lbxArchiver.ItemIndex]).Clone;
  //Let's place our copy right after the original one.
  iNewInsertedPosition := succ(lbxArchiver.ItemIndex);
  if iNewInsertedPosition < MultiArcListTemp.Count then
  begin
    lbxArchiver.Items.InsertObject(iNewInsertedPosition, sNewName, ANewMultiArcItem);
    MultiArcListTemp.Insert(iNewInsertedPosition, sNewName, aNewMultiArcItem);
  end
  else
  begin
    lbxArchiver.Items.AddObject(sNewName, ANewMultiArcItem);
    MultiArcListTemp.Add(sNewName, aNewMultiArcItem);
  end;
  lbxArchiver.ItemIndex := iNewInsertedPosition;
  SetActiveButtonsBasedOnArchiversQuantity;
end;

{ TfrmOptionsArchivers.btnArchiverRenameClick }
procedure TfrmOptionsArchivers.btnArchiverRenameClick(Sender: TObject);
var
  sNewName: string;
begin
  if lbxArchiver.ItemIndex < 0 then
    Exit;
  sNewName := lbxArchiver.Items[lbxArchiver.ItemIndex];
  if InputQuery(Caption, rsOptArchiveTypeName, sNewName) then
  begin
    lbxArchiver.Items[lbxArchiver.ItemIndex] := sNewName;
    MultiArcListTemp.Names[lbxArchiver.ItemIndex] := sNewName;
  end;
end;

{ TfrmOptionsArchivers.btnArchiverDeleteClick }
procedure TfrmOptionsArchivers.btnArchiverDeleteClick(Sender: TObject);
var
  iIndexDelete: integer;
begin
  if lbxArchiver.ItemIndex < 0 then
    Exit;

  if MsgBox(Format(rsOptArchiverConfirmDelete, [lbxArchiver.Items.Strings[lbxArchiver.ItemIndex]]), [msmbYes, msmbCancel], msmbCancel, msmbCancel) = mmrYes then
  begin
    iIndexDelete := lbxArchiver.ItemIndex;
    lbxArchiver.Items.Delete(iIndexDelete);
    MultiArcListTemp.Delete(iIndexDelete);
    if iIndexDelete >= MultiArcListTemp.Count then
      lbxArchiver.ItemIndex := lbxArchiver.Items.Count - 1
    else
      lbxArchiver.ItemIndex := iIndexDelete;
    pcArchiverCommands.Enabled := (lbxArchiver.Items.Count <> 0);
    chkArchiverEnabled.Enabled := (lbxArchiver.Items.Count <> 0);
    lbxArchiverSelectionChange(lbxArchiver, False);
    if lbxArchiver.CanFocus then
      lbxArchiver.SetFocus;
  end;
  SetActiveButtonsBasedOnArchiversQuantity;
end;

{ TfrmOptionsArchivers.btnArchiverOtherClick }
procedure TfrmOptionsArchivers.btnArchiverOtherClick(Sender: TObject);
var
  pWantedPos: TPoint;
begin
  pWantedPos := btnArchiverOther.ClientToScreen(Point(btnArchiverOther.Width div 2, btnArchiverOther.Height - 5)); // Position this way instead of using mouse cursor since it will work for keyboard user.
  pmArchiverOther.PopUp(pWantedPos.X, pWantedPos.Y);
end;

{ TfrmOptionsArchivers.miArchiverAutoConfigureClick }
procedure TfrmOptionsArchivers.miArchiverAutoConfigureClick(Sender: TObject);
begin
  MultiArcListTemp.AutoConfigure;
  lbxArchiverSelectionChange(lbxArchiver, False);
end;

{ TfrmOptionsArchivers.miArchiverDiscardModificationClick }
procedure TfrmOptionsArchivers.miArchiverDiscardModificationClick(Sender: TObject);
begin
  if MultiArcListTemp <> nil then
    MultiArcListTemp.Free;
  MultiArcListTemp := gMultiArcList.Clone;
  lbxArchiverSelectionChange(lbxArchiver, False);
end;

{ TfrmOptionsArchivers.miArchiverSortArchiversClick }
procedure TfrmOptionsArchivers.miArchiverSortArchiversClick(Sender: TObject);
begin
  if MultiArcListTemp.Count > 0 then
  begin
    MultiArcListTemp.FList.Sort;
    FillListBoxWithArchiverList;
    lbxArchiver.ItemIndex := 0;
    lbxArchiverSelectionChange(lbxArchiver, False);
  end;
end;

{ TfrmOptionsArchivers.miAdjustEnableAllClick }
procedure TfrmOptionsArchivers.miAdjustEnableAllClick(Sender: TObject);
var
  iIndex: integer;
begin
  for iIndex := 0 to pred(MultiArcListTemp.Count) do
    MultiArcListTemp.Items[iIndex].FEnabled := (TComponent(Sender).Tag = 1);
  lbxArchiverSelectionChange(lbxArchiver, False);
end;

{ TfrmOptionsArchivers.miArchiverExportClick }
procedure TfrmOptionsArchivers.miArchiverExportClick(Sender: TObject);
var
  slValueList, slOutputIndexSelected: TStringList;
  ExportedMultiArcList: TMultiArcList;
  iIndex, iExportedIndex: integer;
begin
  if MultiArcListTemp.Count > 0 then
  begin
    slValueList := TStringList.Create;
    slOutputIndexSelected := TStringList.Create;
    try
      for iIndex := 0 to pred(MultiArcListTemp.Count) do
        slValueList.Add(MultiArcListTemp.FList.Strings[iIndex]);

      if ShowInputMultiSelectListBox(rsOptArchiverExportCaption, rsOptArchiverExportPrompt, slValueList, slOutputIndexSelected) then
      begin
        ExportedMultiArcList := TMultiArcList.Create;
        try
          for iIndex := 0 to pred(slOutputIndexSelected.Count) do
          begin
            iExportedIndex := StrToIntDef(slOutputIndexSelected.Strings[iIndex], -1);
            if iExportedIndex <> -1 then
              ExportedMultiArcList.Add(MultiArcListTemp.FList.Strings[iExportedIndex], MultiArcListTemp.Items[iExportedIndex].Clone);
          end;

          if ExportedMultiArcList.Count > 0 then
          begin
            SaveArchiverDialog.DefaultExt := '*.ini';
            SaveArchiverDialog.FilterIndex := 1;
            SaveArchiverDialog.Title := rsOptArchiverWhereToSave;
            SaveArchiverDialog.FileName := rsOptArchiverDefaultExportFilename;
            if SaveArchiverDialog.Execute then
            begin
              ExportedMultiArcList.SaveToFile(SaveArchiverDialog.FileName);
              msgOK(Format(rsOptArchiverExportDone, [ExportedMultiArcList.Count, SaveArchiverDialog.FileName]));
            end;
          end;
        finally
          ExportedMultiArcList.Free;
        end;
      end;
    finally
      slOutputIndexSelected.Free;
      slValueList.Free;
    end;
  end;
end;

{ TfrmOptionsArchivers.miArchiverImportClick }
procedure TfrmOptionsArchivers.miArchiverImportClick(Sender: TObject);
var
  ImportedMultiArcList: TMultiArcList;
  slValueList, slOutputIndexSelected: TStringList;
  iIndex, iImportedIndex, iNbImported: integer;
begin
  OpenArchiverDialog.DefaultExt := '*.ini';
  OpenArchiverDialog.FilterIndex := 1;
  OpenArchiverDialog.Title := rsOptArchiverImportFile;
  if OpenArchiverDialog.Execute then
  begin
    ImportedMultiArcList := TMultiArcList.Create;
    try
      ImportedMultiArcList.LoadFromFile(OpenArchiverDialog.FileName);
      if ImportedMultiArcList.Count > 0 then
      begin
        slValueList := TStringList.Create;
        slOutputIndexSelected := TStringList.Create;
        try
          for iIndex := 0 to pred(ImportedMultiArcList.Count) do
            slValueList.Add(ImportedMultiArcList.FList.Strings[iIndex]);
          if ShowInputMultiSelectListBox(rsOptArchiverImportCaption, rsOptArchiverImportPrompt, slValueList, slOutputIndexSelected) then
          begin
            iNbImported := 0;
            for iIndex := 0 to pred(slOutputIndexSelected.Count) do
            begin
              iImportedIndex := StrToIntDef(slOutputIndexSelected.Strings[iIndex], -1);
              if iImportedIndex <> -1 then
              begin
                MultiArcListTemp.Add(ImportedMultiArcList.FList.Strings[iImportedIndex], ImportedMultiArcList.Items[iImportedIndex].Clone);
                lbxArchiver.Items.AddObject(MultiArcListTemp.FList.Strings[pred(MultiArcListTemp.Count)], MultiArcListTemp.Items[pred(MultiArcListTemp.Count)]);
                MultiArcListTemp.Items[pred(MultiArcListTemp.Count)].FEnabled := True; //;
                Inc(iNbImported);
              end;
            end;
            lbxArchiver.ItemIndex := lbxArchiver.Items.Count - 1;
            if iNbImported > 0 then
            begin
              SetActiveButtonsBasedOnArchiversQuantity;
              msgOK(Format(rsOptArchiverImportDone, [iNbImported, OpenArchiverDialog.FileName]));
            end;
          end;
        finally
          slOutputIndexSelected.Free;
          slValueList.Free;
        end;
      end;
    finally
      ImportedMultiArcList.Free;
    end;
  end;
end;

{ TfrmOptionsArchivers.miHelperClick }
procedure TfrmOptionsArchivers.miHelperClick(Sender: TObject);
begin
  if edtHelperRequested <> nil then
    edtHelperRequested.SelText := Trim(LeftStr(TMenuItem(Sender).Caption, pred(pos('-', TMenuItem(Sender).Caption))));
end;

{ TfrmOptionsArchivers.btnHelperClick }
procedure TfrmOptionsArchivers.btnHelperClick(Sender: TObject);
begin
  edtHelperRequested := TEdit(TSpeedButton(Sender).AnchorSideTop.Control);
  pmArchiverParamHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsArchivers.btnArchiverSlectFileArchiverClick }
procedure TfrmOptionsArchivers.btnArchiverSelectFileArchiverClick(Sender: TObject);
begin
  OpenArchiverDialog.DefaultExt := '*.*';
  OpenArchiverDialog.FilterIndex := 2;
  OpenArchiverDialog.Title := rsOptArchiverArchiver;
  if OpenArchiverDialog.Execute then
  begin
    edtArchiverArchiver.Text := OpenArchiverDialog.FileName;
  end;
end;

{ TfrmOptionsArchivers.btnArchiverRelativerClick }
procedure TfrmOptionsArchivers.btnArchiverRelativerClick(Sender: TObject);
begin
  if edtArchiverArchiver.CanFocus then edtArchiverArchiver.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edtArchiverArchiver, pfFILE);
  pmArchiverPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsArchivers.PopulateParamHelperMenu }
procedure TfrmOptionsArchivers.PopulateParamHelperMenu;

  procedure AddThisItem(sParameter, sDescription: string);
  var
    AMenuItem: TMenuItem;
  begin
    AMenuItem := TMenuItem.Create(pmArchiverParamHelper);
    if sDescription <> '' then
    begin
      AMenuItem.Caption := Format('%s - %s', [sParameter, sDescription]);
      AMenuItem.OnClick := @miHelperClick;
    end
    else
      AMenuItem.Caption := sParameter;
    pmArchiverParamHelper.Items.Add(AMenuItem);
  end;

begin
  pmArchiverParamHelper.Items.Clear;
  AddThisItem('%P', rsOptArchiverProgramL);
  AddThisItem('%p', rsOptArchiverProgramS);
  AddThisItem('%A', rsOptArchiverArchiveL);
  AddThisItem('%a', rsOptArchiverArchiveS);
  AddThisItem('%L', rsOptArchiverFileListL);
  AddThisItem('%l', rsOptArchiverFileListS);
  AddThisItem('%F', rsOptArchiverSingleFProcess);
  AddThisItem('%E', rsOptArchiverErrorLevel);
  AddThisItem('%O', rsOptArchiverChangeEncoding);
  AddThisItem('%R', rsOptArchiverTargetSubDir);
  AddThisItem('%S', rsOptArchiverAdditonalCmd);
  AddThisItem('{}', rsOptArchiverAddOnlyNotEmpty);
  AddThisItem('-', '');
  AddThisItem('Q', rsOptArchiverQuoteWithSpace);
  AddThisItem('q', rsOptArchiverQuoteAll);
  AddThisItem('W', rsOptArchiverJustName);
  AddThisItem('P', rsOptArchiverJustPath);
  AddThisItem('A', rsOptArchiverUseAnsi);
  AddThisItem('U', rsOptArchiverUseUTF8);
end;

end.

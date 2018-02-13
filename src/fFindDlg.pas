{
   Double Commander
   -------------------------------------------------------------------------
   Find dialog, with searching in thread

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
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

unit fFindDlg;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Graphics, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, EditBtn, Spin, Buttons, DateTimePicker, KASComboBox,
  fAttributesEdit, uDsxModule, DsxPlugin, uFindThread, uFindFiles,
  uSearchTemplate, fSearchPlugin, uFileView, types, DCStrUtils,
  ActnList, uOSForms, uShellContextMenu, uExceptions, uFileSystemFileSource,
  uFormCommands, uHotkeyManager, LCLVersion;

{$IF DEFINED(LCLGTK2) or DEFINED(LCLQT) or DEFINED(LCLQT5)}
  {$DEFINE FIX_DEFAULT}
{$ENDIF}

const
  HotkeysCategory = 'Find files';

type
  { TfrmFindDlg }
  TfrmFindDlg = class(TForm, IFormCommands)
    actIntelliFocus: TAction;
    actCancel: TAction;
    actClose: TAction;
    actEdit: TAction;
    actGoToFile: TAction;
    actFeedToListbox: TAction;
    actCancelClose: TAction;
    actPagePrev: TAction;
    actPageNext: TAction;
    actPageResults: TAction;
    actPageLoadSave: TAction;
    actPagePlugins: TAction;
    actPageAdvanced: TAction;
    actPageStandard: TAction;
    actView: TAction;
    actLastSearch: TAction;
    actNewSearch: TAction;
    actStart: TAction;
    actList: TActionList;
    Bevel2: TBevel;
    btnAddAttribute: TButton;
    btnAttrsHelp: TButton;
    btnClose: TButton;
    btnGoToPath: TButton;
    btnNewSearch: TButton;
    btnLastSearch: TButton;
    btnSaveTemplate: TButton;
    btnSearchDelete: TButton;
    btnSearchLoad: TButton;
    btnSearchSave: TButton;
    btnSearchSaveWithStartingPath: TButton;
    btnStart: TButton;
    btnUseTemplate: TButton;
    btnStop: TButton;
    btnView: TButton;
    btnEdit: TButton;
    btnWorkWithFound: TButton;
    cbFindText: TCheckBox;
    cbNotContainingText: TCheckBox;
    cbDateFrom: TCheckBox;
    cbNotOlderThan: TCheckBox;
    cbFileSizeFrom: TCheckBox;
    cbDateTo: TCheckBox;
    cbFileSizeTo: TCheckBox;
    cbReplaceText: TCheckBox;
    cbTimeFrom: TCheckBox;
    cbTimeTo: TCheckBox;
    cbPartialNameSearch: TCheckBox;
    cbFollowSymLinks: TCheckBox;
    cbUsePlugin: TCheckBox;
    cbSelectedFiles: TCheckBox;
    cbTextRegExp: TCheckBox;
    cbFindInArchive: TCheckBox;
    cbOpenedTabs: TCheckBox;
    cmbExcludeDirectories: TComboBoxWithDelItems;
    cmbNotOlderThanUnit: TComboBox;
    cmbFileSizeUnit: TComboBox;
    cmbEncoding: TComboBox;
    cmbSearchDepth: TComboBox;
    cbRegExp: TCheckBox;
    cmbPlugin: TComboBox;
    cmbReplaceText: TComboBoxWithDelItems;
    cmbFindText: TComboBoxWithDelItems;
    cmbExcludeFiles: TComboBoxWithDelItems;
    edtAttrib: TEdit;
    cmbFindPathStart: TComboBoxWithDelItems;
    frmContentPlugins: TfrmSearchPlugin;
    gbDirectories: TGroupBox;
    gbFiles: TGroupBox;
    lblAttributes: TLabel;
    lblExcludeDirectories: TLabel;
    lblCurrent: TLabel;
    lblExcludeFiles: TLabel;
    lblFound: TLabel;
    lblStatus: TLabel;
    lblTemplateHeader: TLabel;
    lbSearchTemplates: TListBox;
    lblSearchContents: TPanel;
    lblSearchDepth: TLabel;
    lblEncoding: TLabel;
    lsFoundedFiles: TListBox;
    CheksPanel: TPanel;
    miOpenInNewTab: TMenuItem;
    miShowInEditor: TMenuItem;
    miShowAllFound: TMenuItem;
    miRemoveFromLlist: TMenuItem;
    pnlDirectoriesDepth: TPanel;
    pnlLoadSaveBottomButtons: TPanel;
    pnlLoadSaveBottom: TPanel;
    pnlButtons: TPanel;
    pnlResultsBottomButtons: TPanel;
    pnlResults: TPanel;
    pnlStatus: TPanel;
    pnlResultsBottom: TPanel;
    seNotOlderThan: TSpinEdit;
    seFileSizeFrom: TSpinEdit;
    seFileSizeTo: TSpinEdit;
    pnlFindFile: TPanel;
    pgcSearch: TPageControl;
    btnChooseFolder: TSpeedButton;
    tsPlugins: TTabSheet;
    tsResults: TTabSheet;
    tsLoadSave: TTabSheet;
    tsStandard: TTabSheet;
    lblFindPathStart: TLabel;
    lblFindFileMask: TLabel;
    cmbFindFileMask: TComboBoxWithDelItems;
    gbFindData: TGroupBox;
    cbCaseSens: TCheckBox;
    tsAdvanced: TTabSheet;
    PopupMenuFind: TPopupMenu;
    miShowInViewer: TMenuItem;
    ZVDateFrom: TDateTimePicker;
    ZVDateTo: TDateTimePicker;
    ZVTimeFrom: TDateTimePicker;
    ZVTimeTo: TDateTimePicker;
    actFreeFromMem: TAction;
    actFreeFromMemAllOthers: TAction;
    actConfigFileSearchHotKeys: TAction;
    actNewSearchClearFilters: TAction;
    mmMainMenu: TMainMenu;
    miNewSearchClearFilters: TMenuItem;
    miConfigFileSearchHotKeys: TMenuItem;
    miOptions: TMenuItem;
    miAction: TMenuItem;
    miNewSearch: TMenuItem;
    miLastSearch: TMenuItem;
    miStart: TMenuItem;
    miCancel: TMenuItem;
    miFreeFromMem: TMenuItem;
    miFreeFromMemAllOthers: TMenuItem;
    miSeparator1: TMenuItem;
    miCancelClose: TMenuItem;
    miClose: TMenuItem;
    miViewTab: TMenuItem;
    miPageStandard: TMenuItem;
    miPageAdvanced: TMenuItem;
    miPagePlugins: TMenuItem;
    miPageLoadSave: TMenuItem;
    miPageResults: TMenuItem;
    miSeparator2: TMenuItem;
    miResult: TMenuItem;
    miView: TMenuItem;
    miEdit: TMenuItem;
    miFeedToListbox: TMenuItem;
    miGoToFile: TMenuItem;
    procedure actExecute(Sender: TObject);
    procedure btnAddAttributeClick(Sender: TObject);
    procedure btnAttrsHelpClick(Sender: TObject);
    procedure btnNewSearchKeyDown(Sender: TObject; var Key: word;
      {%H-}Shift: TShiftState);
    procedure btnSearchDeleteClick(Sender: TObject);
    procedure btnSearchLoadClick(Sender: TObject);
    procedure btnSearchSaveWithStartingPathClick(Sender: TObject);
    procedure btnSearchSaveClick(Sender: TObject);
    procedure cbCaseSensChange(Sender: TObject);
    procedure cbDateFromChange(Sender: TObject);
    procedure cbDateToChange(Sender: TObject);
    procedure cbFindInArchiveChange(Sender: TObject);
    procedure cbOpenedTabsChange(Sender: TObject);
    procedure cbPartialNameSearchChange(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure cbTextRegExpChange(Sender: TObject);
    procedure cbSelectedFilesChange(Sender: TObject);
    procedure cmbEncodingSelect(Sender: TObject);
    procedure cbFindTextChange(Sender: TObject);
    procedure cbUsePluginChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSelDirClick(Sender: TObject);
    procedure cbFileSizeFromChange(Sender: TObject);
    procedure cbFileSizeToChange(Sender: TObject);
    procedure cbNotOlderThanChange(Sender: TObject);
    procedure cbReplaceTextChange(Sender: TObject);
    procedure cbTimeFromChange(Sender: TObject);
    procedure cbTimeToChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
{$IF DEFINED(FIX_DEFAULT)}
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
    procedure frmFindDlgClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure frmFindDlgShow(Sender: TObject);
    procedure gbDirectoriesResize(Sender: TObject);
    procedure lbSearchTemplatesDblClick(Sender: TObject);
    procedure lbSearchTemplatesSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure lsFoundedFilesDblClick(Sender: TObject);
    procedure lsFoundedFilesKeyDown(Sender: TObject;
      var Key: word; Shift: TShiftState);
    procedure lsFoundedFilesMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: integer);
    procedure lsFoundedFilesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure lsFoundedFilesMouseWheelDown(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: boolean);
    procedure lsFoundedFilesMouseWheelUp(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: boolean);
    procedure miOpenInNewTabClick(Sender: TObject);
    procedure miRemoveFromLlistClick(Sender: TObject);
    procedure miShowAllFoundClick(Sender: TObject);
    procedure miShowInEditorClick(Sender: TObject);
    procedure miShowInViewerClick(Sender: TObject);
    procedure pgcSearchChange(Sender: TObject);
    procedure seFileSizeFromChange(Sender: TObject);
    procedure seFileSizeToChange(Sender: TObject);
    procedure seNotOlderThanChange(Sender: TObject);
    procedure tsLoadSaveShow(Sender: TObject);
    procedure tsStandardEnter(Sender: TObject);
    procedure ZVDateFromChange(Sender: TObject);
    procedure ZVDateToChange(Sender: TObject);
    procedure ZVTimeFromChange(Sender: TObject);
    procedure ZVTimeToChange(Sender: TObject);
    procedure PopupMenuFindPopup(Sender: TObject);
    procedure CancelCloseAndFreeMem;
    procedure LoadHistory;
    procedure SaveHistory;
  private
    FSelectedFiles: TStringList;
    FFindThread: TFindThread;
    FTimeSearch: string;
    DsxPlugins: TDSXModuleList;
    FSearchingActive: boolean;
    FFrmAttributesEdit: TfrmAttributesEdit;
    FLastTemplateName: string;
    FLastSearchTemplate: TSearchTemplate;
    FUpdateTimer: TTimer;
    FUpdating: boolean;
    FRButtonPanelSender: TObject; // last focused button on Right Panel (pnlButtons)
    FCommands: TFormCommands;
    FSearchWithDSXPluginInProgress: boolean;
    FSearchWithWDXPluginInProgress: boolean;
    FFreeOnClose: boolean;
    FAtLeastOneSearchWasDone: boolean;

    property Commands: TFormCommands read FCommands implements IFormCommands;

    procedure DisableControlsForTemplate;
    procedure StopSearch;
    procedure AfterSearchStopped;  //update button states after stop search(ThreadTerminate call this method)
    procedure AfterSearchFocus;     //set correct focus after search stopped

    procedure FillFindOptions(out FindOptions: TSearchTemplateRec; SetStartPath: boolean);
    procedure FindOptionsToDSXSearchRec(const AFindOptions: TSearchTemplateRec;
                                        out SRec: TDsxSearchRecord);
    procedure FoundedStringCopyChanged(Sender: TObject);
    procedure LoadTemplate(const Template: TSearchTemplateRec);
    procedure LoadSelectedTemplate;
    procedure SaveTemplate(SaveStartingPath: boolean);
    procedure SelectTemplate(const ATemplateName: string);
    procedure UpdateTemplatesList;
    procedure OnUpdateTimer(Sender: TObject);
    procedure OnAddAttribute(Sender: TObject);
    function InvalidRegExpr(AChecked: boolean; const ARegExpr: string): boolean;
    procedure SetWindowCaption(AWindowCaptionStyle: byte);
    function GetFileMask: String;
  public
    FoundedStringCopy: TStringList;
    class function Instance: TfrmFindDlg;
  public
    LastClickResultsPath: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearFilter(bClearSearchLocation: boolean = True);
    procedure ClearResults;
    procedure ThreadTerminate(Sender: TObject);
    procedure FocusOnResults(Sender: TObject); // if press VK_LEFT or VK_RIGHT when on any button on left panel  - focus on results and remember button in FRButtonPanelSender
  published
    procedure cm_IntelliFocus(const {%H-}Params: array of string);
    procedure cm_Start(const {%H-}Params: array of string);
    procedure cm_CancelClose(const {%H-}Params: array of string);
    procedure cm_Cancel(const {%H-}Params: array of string);
    procedure cm_Close(const {%H-}Params: array of string);
    procedure cm_NewSearch(const {%H-}Params: array of string);
    procedure cm_LastSearch(const {%H-}Params: array of string);
    procedure cm_View(const {%H-}Params: array of string);
    procedure cm_Edit(const {%H-}Params: array of string);
    procedure cm_GoToFile(const {%H-}Params: array of string);
    procedure cm_FeedToListbox(const {%H-}Params: array of string);
    procedure cm_PageNext(const Params: array of string);
    procedure cm_PagePrev(const Params: array of string);
    procedure cm_PageStandard(const {%H-}Params: array of string);
    procedure cm_PageAdvanced(const {%H-}Params: array of string);
    procedure cm_PagePlugins(const {%H-}Params: array of string);
    procedure cm_PageLoadSave(const {%H-}Params: array of string);
    procedure cm_PageResults(const {%H-}Params: array of string);
    procedure cm_NewSearchClearFilters(const {%H-}Params: array of string);
    procedure cm_FreeFromMem(const {%H-}Params: array of string);
    procedure cm_FreeFromMemAllOthers(const {%H-}Params: array of string);
    procedure cm_ConfigFileSearchHotKeys(const {%H-}Params: array of string);
  end;

{en
   Shows the find files dialog.
   Cannot store FileView reference as it might get destroyed while Find Dialog is running.
   We can store FileSource though, if needed in future (as it is reference counted).
   @param(FileView
          For which file view the find dialog is executed,
          to get file source, current path and a list of selected files.)
}

  { TListOffrmFindDlgInstance }
  TListOffrmFindDlgInstance = class(TList)
  private
    function GetfrmFindDlgInstance(Index: integer): TfrmFindDlg;
  public
    constructor Create;
    procedure Clear; override;
    function Add(AfrmFindDlg: TfrmFindDlg): integer;
    property frmFindDlgInstance[Index: integer]: TfrmFindDlg read GetfrmFindDlgInstance;
  end;

var
  // [ ListOffrmFindDlgInstance ]
  // This list will hold in memory pointers to our find dialog forms.
  ListOffrmFindDlgInstance: TListOffrmFindDlgInstance;
  frmFindDlgUsingPluginDSX: TfrmFindDlg = nil;
  frmFindDlgUsingPluginWDX: TfrmFindDlg = nil;

procedure ShowFindDlg(FileView: TFileView; const TemplateName: string; bCreateNewFindDlg: boolean = False);
function ShowDefineTemplateDlg(var TemplateName: string): boolean;
function ShowUseTemplateDlg(var Template: TSearchTemplate): boolean;

implementation

{$R *.lfm}

uses
  LCLProc, LCLType, LConvEncoding, StrUtils, HelpIntfs, fViewer, fMain,
  uLng, uGlobs, uShowForm, uDCUtils, uFileSource, uFileSourceUtil,
  uSearchResultFileSource, uFile,
  uFileViewNotebook, uKeyboard, uOSUtils, uArchiveFileSourceUtil,
  DCOSUtils, RegExpr, uDebug, uShowMsg, uConvEncoding;

const
  TimeUnitToComboIndex: array[TTimeUnit] of integer = (0, 1, 2, 3, 4, 5, 6);
  ComboIndexToTimeUnit: array[0..6] of TTimeUnit = (tuSecond, tuMinute, tuHour, tuDay, tuWeek, tuMonth, tuYear);
  FileSizeUnitToComboIndex: array[TFileSizeUnit] of integer = (0, 1, 2, 3, 4);
  ComboIndexToFileSizeUnit: array[0..4] of TFileSizeUnit = (suBytes, suKilo, suMega, suGiga, suTera);

  wcs_NewSearch = $01;
  wcs_StartSearch = $0A;
  wcs_EndSearch = $1B;

type
  { TStringListTemp }
  TStringListTemp = class(TStringList)
  public
    function AddObject(const S: string; AObject: TObject): integer; override;
  end;

var
  gSearchWithDSXPluginInProgress: boolean = False;
  gSearchWithWDXPluginInProgress: boolean = False;

{ TListOffrmFindDlgInstance.Create }
constructor TListOffrmFindDlgInstance.Create;
begin
  inherited Create;
end;

{ TListOffrmFindDlgInstance.Clear }
procedure TListOffrmFindDlgInstance.Clear;
var
  i: integer;
begin
  for i := pred(Count) downto 0 do
    if frmFindDlgInstance[i] <> nil then
      frmFindDlgInstance[i].Free;
  inherited Clear;
end;

{ TListOffrmFindDlgInstance.Add }
function TListOffrmFindDlgInstance.Add(AfrmFindDlg: TfrmFindDlg): integer;
begin
  Result := inherited Add(AfrmFindDlg);
end;

{ TListOffrmFindDlgInstance.GetfrmFindDlgInstance }
function TListOffrmFindDlgInstance.GetfrmFindDlgInstance(Index: integer): TfrmFindDlg;
begin
  Result := TfrmFindDlg(Items[Index]);
end;

procedure SAddFileProc({%H-}PlugNr: integer; FoundFile: PChar); dcpcall;
var
  s: string;
begin
  s := string(FoundFile);
  if s = '' then
  begin
    TfrmFindDlg.Instance.AfterSearchStopped;
    TfrmFindDlg.Instance.btnStart.Default := True;
  end
  else
  begin
    TfrmFindDlg.Instance.FoundedStringCopy.Add(s);
    Application.ProcessMessages;
  end;
end;

procedure SUpdateStatusProc({%H-}PlugNr: integer; CurrentFile: PChar; FilesScanned: integer); dcpcall;
var
  sCurrentFile: string;
begin
  sCurrentFile := string(CurrentFile);
  TfrmFindDlg.Instance.lblStatus.Caption := Format(rsFindScanned, [FilesScanned]) + TfrmFindDlg.Instance.FTimeSearch;
  if sCurrentFile = '' then
    TfrmFindDlg.Instance.lblCurrent.Caption := ''
  else
    TfrmFindDlg.Instance.lblCurrent.Caption := rsFindScanning + ': ' + sCurrentFile;
  Application.ProcessMessages;
end;

{ ShowFindDlg }
procedure ShowFindDlg(FileView: TFileView; const TemplateName: string; bCreateNewFindDlg: boolean = False);
var
  ASelectedFiles: TFiles = nil;
  I: integer;
  AfrmFindDlgInstance: TfrmFindDlg;
  bFirstFindDlg: boolean;
begin
  if not Assigned(FileView) then
    raise Exception.Create('ShowFindDlg: FileView=nil');

  bFirstFindDlg := (ListOffrmFindDlgInstance.Count = 0);

  // 1. We create a new form: if it's the first search we do OR if we've been instructed to do so (cm_AddNewSearch)
  if bFirstFindDlg or bCreateNewFindDlg then
  begin
    AfrmFindDlgInstance := TfrmFindDlg.Create(nil);
    ListOffrmFindDlgInstance.add(AfrmFindDlgInstance);
  end
  else
  begin
    AfrmFindDlgInstance := ListOffrmFindDlgInstance.frmFindDlgInstance[pred(ListOffrmFindDlgInstance.Count)];
  end;

  // 2. If we don't have a search in progress, then clear and set a few things.
  if not AfrmFindDlgInstance.FSearchingActive then
  begin
    with AfrmFindDlgInstance do
    begin
      // Prepare window for search files
      LoadHistory;
      ClearFilter;
      // SetWindowCaption(wcs_NewSearch);
      cmbFindPathStart.Text := FileView.CurrentPath;

      // Get paths of selected files, if any.
      FSelectedFiles.Clear;
      ASelectedFiles := FileView.CloneSelectedFiles;
      if Assigned(ASelectedFiles) then
        try
          if ASelectedFiles.Count > 0 then
          begin
            for I := 0 to ASelectedFiles.Count - 1 do
              FSelectedFiles.Add(ASelectedFiles[I].FullPath);
          end;
        finally
          FreeAndNil(ASelectedFiles);
        end;

      if Length(TemplateName) > 0 then
      begin
        FUpdating := True;
        UpdateTemplatesList;
        SelectTemplate(TemplateName);
        LoadSelectedTemplate;
        FUpdating := False;
      end;

    end;
  end;

  AfrmFindDlgInstance.ShowOnTop;
end;

{ ShowDefineTemplateDlg }
function ShowDefineTemplateDlg(var TemplateName: string): boolean;
var
  AIndex: integer;
  AForm: TfrmFindDlg;
begin
  AForm := TfrmFindDlg.Create(nil);
  try
    with AForm do
    begin
      // Prepare window for define search template
      LoadHistory;
      Caption := rsFindDefineTemplate;
      DisableControlsForTemplate;
      btnSaveTemplate.Visible := True;
      btnSaveTemplate.Default := True;
      BorderIcons := [biSystemMenu, biMaximize];
      if Length(TemplateName) > 0 then
      begin
        UpdateTemplatesList;
        AIndex := lbSearchTemplates.Items.IndexOf(TemplateName);
        if AIndex >= 0 then
        begin
          lbSearchTemplates.ItemIndex := AIndex;
          AForm.LoadSelectedTemplate;
        end;
      end;
      Result := (ShowModal = mrOk);
      if Result and (lbSearchTemplates.Count > 0) then
      begin
        TemplateName := lbSearchTemplates.Items[lbSearchTemplates.Count - 1];
      end;
    end;
  finally
    AForm.Free;
  end;
end;

{ ShowUseTemplateDlg }
function ShowUseTemplateDlg(var Template: TSearchTemplate): boolean;
var
  AForm: TfrmFindDlg;
  SearchRec: TSearchTemplateRec;
begin
  AForm := TfrmFindDlg.Create(nil);
  try
    with AForm do
    begin
      // Prepare window for define search template
      LoadHistory;
      Caption := rsFindDefineTemplate;
      DisableControlsForTemplate;
      btnUseTemplate.Visible := True;
      btnUseTemplate.Default := True;
      BorderIcons := [biSystemMenu, biMaximize];
      if Assigned(Template) then
        AForm.LoadTemplate(Template.SearchRecord);
      Result := (ShowModal = mrOk);
      if Result then
      begin
        if not Assigned(Template) then
          Template := TSearchTemplate.Create;
        try
          Template.TemplateName := AForm.FLastTemplateName;
          AForm.FillFindOptions(SearchRec, False);
          Template.SearchRecord := SearchRec;
        except
          FreeAndNil(Template);
          raise;
        end;
      end;
    end;
  finally
    AForm.Free;
  end;
end;

{ TStringListTemp }

{ TStringListTemp.AddObject }
function TStringListTemp.AddObject(const S: string; AObject: TObject): integer;
begin
  Result := Count;
  InsertItem(Result, S, AObject);
end;

{ TfrmFindDlg }

{ TfrmFindDlg.FormCreate }
procedure TfrmFindDlg.FormCreate(Sender: TObject);
var
  I: integer;
  HMFindFiles: THMForm;
begin
  if not gShowMenuBarInFindFiles then FreeAndNil(mmMainMenu);
  Height := pnlFindFile.Height + 22;
  DsxPlugins := TDSXModuleList.Create;
  DsxPlugins.Assign(gDSXPlugins);
  FoundedStringCopy := TStringListTemp.Create;
  FoundedStringCopy.OnChange := @FoundedStringCopyChanged;
  FFreeOnClose := False;
  FAtLeastOneSearchWasDone := False;
  FSearchWithDSXPluginInProgress := False;
  FSearchWithWDXPluginInProgress := False;

  // load language
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitSecond);
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitMinute);
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitHour);
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitDay);
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitWeek);
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitMonth);
  cmbNotOlderThanUnit.Items.Add(rsTimeUnitYear);
  cmbFileSizeUnit.Items.Add(rsSizeUnitBytes);
  cmbFileSizeUnit.Items.Add(rsSizeUnitKBytes);
  cmbFileSizeUnit.Items.Add(rsSizeUnitMBytes);
  cmbFileSizeUnit.Items.Add(rsSizeUnitGBytes);
  cmbFileSizeUnit.Items.Add(rsSizeUnitTBytes);

  // fill search depth combobox
  cmbSearchDepth.Items.Add(rsFindDepthAll);
  cmbSearchDepth.Items.Add(rsFindDepthCurDir);
  for I := 1 to 100 do
    cmbSearchDepth.Items.Add(Format(rsFindDepth, [IntToStr(I)]));
  cmbSearchDepth.ItemIndex := 0;
  // fill encoding combobox
  cmbEncoding.Clear;
  GetSupportedEncodings(cmbEncoding.Items);
  I := cmbEncoding.Items.IndexOf('UTF-8BOM');
  if I >= 0 then cmbEncoding.Items.Delete(I);
  cmbEncoding.Items.Insert(0, 'Default');
  cmbEncoding.ItemIndex := 0;

  // gray disabled fields
  cbUsePluginChange(Sender);
  cbFindTextChange(Sender);
  cbReplaceTextChange(Sender);
  cbNotOlderThanChange(Sender);
  cbFileSizeFromChange(Sender);
  cbFileSizeToChange(Sender);
  ZVDateFrom.DateTime := Now();
  ZVDateTo.DateTime := Now();
  ZVTimeFrom.DateTime := Now();
  ZVTimeTo.DateTime := Now();
  cbDateFrom.Checked := False;
  cbDateTo.Checked := False;
  cbTimeFrom.Checked := False;
  cbTimeTo.Checked := False;

  btnStart.Default := True;

  cmbNotOlderThanUnit.ItemIndex := 3; // Days
  cmbFileSizeUnit.ItemIndex := 1; // Kilobytes

  InitPropStorage(Self);

  HMFindFiles := HotMan.Register(Self, HotkeysCategory);
  HMFindFiles.RegisterActionList(actList);

  CloneMainAction(frmMain.actAddNewSearch, actList, miViewTab, -1);
  CloneMainAction(frmMain.actViewSearches, actList, miViewTab, -1);
  CloneMainAction(frmMain.actDeleteSearches, actList, miAction, -1);
  CloneMainAction(frmMain.actConfigSearches, actList, miOptions, 0);

{$IF DEFINED(FIX_DEFAULT)}
  if (ListOffrmFindDlgInstance.Count = 0) then
    Application.AddOnKeyDownBeforeHandler(@FormKeyDown);
{$ENDIF}
end;

{ TfrmFindDlg.cbUsePluginChange }
procedure TfrmFindDlg.cbUsePluginChange(Sender: TObject);
begin
  EnableControl(cmbPlugin, cbUsePlugin.Checked);

  if not FUpdating and cmbPlugin.Enabled and cmbPlugin.CanFocus and (Sender = cbUsePlugin) then
  begin
    cmbPlugin.SetFocus;
    cmbPlugin.SelectAll;
  end;
end;

{ TfrmFindDlg.cmbEncodingSelect }
procedure TfrmFindDlg.cmbEncodingSelect(Sender: TObject);
begin
  cbTextRegExp.Enabled := cbFindText.Checked and SingleByteEncoding(cmbEncoding.Text);
  if not cbTextRegExp.Enabled then cbTextRegExp.Checked := False;
end;

{ TfrmFindDlg.Create }
constructor TfrmFindDlg.Create(TheOwner: TComponent);
var
  C: TPortableNetworkGraphic;
begin
  FSelectedFiles := TStringList.Create;
  inherited Create(TheOwner);
  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Interval := 100;
  FUpdateTimer.Enabled := False;
  FUpdateTimer.OnTimer := @OnUpdateTimer;

  try
    C := TPortableNetworkGraphic.Create;
    C.LoadFromResourceName(hInstance, ResBtnSelDir);
    btnChooseFolder.Glyph.Assign(C);
  finally
    C.Free;
  end;

  FCommands := TFormCommands.Create(Self, actList);
end;

{ TfrmFindDlg.Destroy }
destructor TfrmFindDlg.Destroy;
begin
  inherited Destroy;
  FSelectedFiles.Free;
  FLastSearchTemplate.Free;
end;

{ TfrmFindDlg.DisableControlsForTemplate }
procedure TfrmFindDlg.DisableControlsForTemplate;
begin
  lblFindPathStart.Visible := False;
  cmbFindPathStart.Visible := False;
  cbFollowSymLinks.Visible := False;
  cbSelectedFiles.Visible := False;
  cbOpenedTabs.Visible := False;
  btnStart.Visible := False;
  btnStop.Visible := False;
  btnNewSearch.Visible := False;
  btnLastSearch.Visible := False;
  btnSearchSaveWithStartingPath.Visible := False;
  gbFindData.Visible := False;
  tsResults.TabVisible := False;
  actPageResults.Enabled := False;
  if mmMainMenu <> nil then FreeAndNil(mmMainMenu);
end;

{ TfrmFindDlg.cbFindTextChange }
procedure TfrmFindDlg.cbFindTextChange(Sender: TObject);
begin
  EnableControl(cmbFindText, cbFindText.Checked);
  EnableControl(cmbEncoding, cbFindText.Checked);
  EnableControl(cbCaseSens, cbFindText.Checked);
  EnableControl(cbReplaceText, cbFindText.Checked and not cbFindInArchive.Checked);
  EnableControl(cbNotContainingText, cbFindText.Checked);
  EnableControl(cbTextRegExp, cbFindText.Checked);
  lblEncoding.Enabled := cbFindText.Checked;
  cbReplaceText.Checked := False;
  cmbEncodingSelect(nil);

  if not FUpdating and cmbFindText.Enabled and cmbFindText.CanFocus and (Sender = cbFindText) then
  begin
    cmbFindText.SetFocus;
    cmbFindText.SelectAll;
  end;
end;

{ TfrmFindDlg.ClearFilter }
procedure TfrmFindDlg.ClearFilter(bClearSearchLocation: boolean = True);
var
  FreezeTime: TDateTime;
begin
  FUpdating := True;

  FLastTemplateName := '';
  if bClearSearchLocation then
  begin
    cmbFindPathStart.Text := '';
    cmbExcludeDirectories.Text := '';
  end;

  if gInitiallyClearFileMask then
    cmbFindFileMask.Text := ''
  else if glsMaskHistory.Count > 0 then begin
    cmbFindFileMask.Text:= glsMaskHistory[0];
  end;

  // If we already search text then use last searched text
  if not gFirstTextSearch then
  begin
    if glsSearchHistory.Count > 0 then
      cmbFindText.Text := glsSearchHistory[0];
  end;

  cmbSearchDepth.ItemIndex := 0;
  cmbExcludeFiles.Text := '';
  cbPartialNameSearch.Checked := gPartialNameSearch;
  cbRegExp.Checked := False;

  // attributes
  edtAttrib.Text := '';

  // file date/time
  FreezeTime := Now;
  ZVDateFrom.DateTime := FreezeTime;
  ZVDateTo.DateTime := FreezeTime;
  ZVTimeFrom.DateTime := FreezeTime;
  ZVTimeTo.DateTime := FreezeTime;
  cbDateFrom.Checked := False;
  cbDateTo.Checked := False;
  cbTimeFrom.Checked := False;
  cbTimeTo.Checked := False;

  // not older then
  cbNotOlderThan.Checked := False;
  seNotOlderThan.Value := 1;
  cmbNotOlderThanUnit.ItemIndex := 3; // Days

  // file size
  cbFileSizeFrom.Checked := False;
  cbFileSizeTo.Checked := False;
  seFileSizeFrom.Value := 0;
  seFileSizeTo.Value := 10;
  cmbFileSizeUnit.ItemIndex := 1; // Kilobytes

  // find/replace text
  // do not clear search/replace text just clear checkbox
  cbFindText.Checked := False;
  cbReplaceText.Checked := False;
  cbCaseSens.Checked := False;
  cbNotContainingText.Checked := False;
  cmbEncoding.ItemIndex := 0;
  cmbEncodingSelect(nil);

  // plugins
  cmbPlugin.Text := '';

  FUpdating := False;
end;

{ TfrmFindDlg.ClearResults }
procedure TfrmFindDlg.ClearResults;
begin
  lsFoundedFiles.Clear;
  lsFoundedFiles.Tag := 0;
  lsFoundedFiles.ScrollWidth := 0;
  FoundedStringCopy.Clear;
end;

{ TfrmFindDlg.btnSearchLoadClick }
procedure TfrmFindDlg.btnSearchLoadClick(Sender: TObject);
begin
  LoadSelectedTemplate;
end;

{ TfrmFindDlg.btnSearchSaveWithStartingPathClick }
procedure TfrmFindDlg.btnSearchSaveWithStartingPathClick(Sender: TObject);
begin
  SaveTemplate(True);
end;

{ TfrmFindDlg.btnSearchDeleteClick }
procedure TfrmFindDlg.btnSearchDeleteClick(Sender: TObject);
var
  OldIndex: integer;
begin
  OldIndex := lbSearchTemplates.ItemIndex;
  if OldIndex < 0 then Exit;
  gSearchTemplateList.DeleteTemplate(OldIndex);
  lbSearchTemplates.Items.Delete(OldIndex);
  if OldIndex < lbSearchTemplates.Count then
    lbSearchTemplates.ItemIndex := OldIndex
  else if lbSearchTemplates.Count > 0 then
    lbSearchTemplates.ItemIndex := lbSearchTemplates.Count - 1;
end;

{ TfrmFindDlg.btnAttrsHelpClick }
procedure TfrmFindDlg.btnAttrsHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', edtAttrib.HelpKeyword);
end;

{ TfrmFindDlg.actExecute }
procedure TfrmFindDlg.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.ExecuteCommand(cmd, []);
end;

{ TfrmFindDlg.btnAddAttributeClick }
procedure TfrmFindDlg.btnAddAttributeClick(Sender: TObject);
begin
  if not Assigned(FFrmAttributesEdit) then
  begin
    FFrmAttributesEdit := TfrmAttributesEdit.Create(Self);
    FFrmAttributesEdit.OnOk := @OnAddAttribute;
  end;
  FFrmAttributesEdit.Reset;
  if not (fsModal in FormState) then
    FFrmAttributesEdit.Show
  else
  begin
    FFrmAttributesEdit.ShowModal;
  end;
end;

{ TfrmFindDlg.btnSearchSaveClick }
procedure TfrmFindDlg.btnSearchSaveClick(Sender: TObject);
begin
  SaveTemplate(False);
end;

{ TfrmFindDlg.cbCaseSensChange }
procedure TfrmFindDlg.cbCaseSensChange(Sender: TObject);
begin
  if cbCaseSens.Checked then cbTextRegExp.Checked := False;
end;

{ TfrmFindDlg.cbDateFromChange }
procedure TfrmFindDlg.cbDateFromChange(Sender: TObject);
begin
  UpdateColor(ZVDateFrom, cbDateFrom.Checked);
end;

{ TfrmFindDlg.cbDateToChange }
procedure TfrmFindDlg.cbDateToChange(Sender: TObject);
begin
  UpdateColor(ZVDateTo, cbDateTo.Checked);
end;

{ TfrmFindDlg.cbFindInArchiveChange }
procedure TfrmFindDlg.cbFindInArchiveChange(Sender: TObject);
begin
  EnableControl(cbReplaceText, cbFindText.Checked and not cbFindInArchive.Checked);
  if cbReplaceText.Checked then cbReplaceText.Checked := cbReplaceText.Enabled;
  actView.Enabled := not cbFindInArchive.Checked;
  actEdit.Enabled := not cbFindInArchive.Checked;
  actFeedToListbox.Enabled := not cbFindInArchive.Checked;
  cbReplaceTextChange(cbReplaceText);
end;

{ TfrmFindDlg.cbOpenedTabsChange }
procedure TfrmFindDlg.cbOpenedTabsChange(Sender: TObject);
begin
  cbSelectedFiles.Enabled := not cbOpenedTabs.Checked;
  cbFollowSymLinks.Enabled := not cbOpenedTabs.Checked;
  cmbFindPathStart.Enabled := not cbOpenedTabs.Checked;
end;

{ TfrmFindDlg.cbPartialNameSearchChange }
procedure TfrmFindDlg.cbPartialNameSearchChange(Sender: TObject);
begin
  if cbPartialNameSearch.Checked then cbRegExp.Checked := False;
end;

{ TfrmFindDlg.cbRegExpChange }
procedure TfrmFindDlg.cbRegExpChange(Sender: TObject);
begin
  if cbRegExp.Checked then cbPartialNameSearch.Checked := False;
end;

{ TfrmFindDlg.cbTextRegExpChange }
procedure TfrmFindDlg.cbTextRegExpChange(Sender: TObject);
begin
  if cbTextRegExp.Checked then
  begin
    if cbCaseSens.Enabled then
    begin
      cbCaseSens.Tag := integer(cbCaseSens.Checked);
      cbCaseSens.Checked := False;
      cbCaseSens.Enabled := False;
    end;
  end
  else if not cbCaseSens.Enabled then
  begin
    cbCaseSens.Checked := boolean(cbCaseSens.Tag);
    cbCaseSens.Enabled := True;
  end;
end;

{ TfrmFindDlg.cbSelectedFilesChange }
procedure TfrmFindDlg.cbSelectedFilesChange(Sender: TObject);
begin
  cmbFindPathStart.Enabled := not cbSelectedFiles.Checked;
end;

{ TfrmFindDlg.btnSelDirClick }
procedure TfrmFindDlg.btnSelDirClick(Sender: TObject);
var
  S, AFolder: String;
begin
  S := cmbFindPathStart.Text;
  AFolder:= ExtractFilePath(ExcludeTrailingBackslash(S));
  if not mbDirectoryExists(AFolder) then AFolder := EmptyStr;
  if SelectDirectory(rsFindWhereBeg, AFolder, S, gShowSystemFiles) then
    cmbFindPathStart.Text := S;
end;

{ TfrmFindDlg.btnNewSearchKeyDown }
procedure TfrmFindDlg.btnNewSearchKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if ((Key = VK_LEFT) or (Key = VK_RIGHT)) and (lsFoundedFiles.Count > 0) then FocusOnResults(Sender);
end;

{ TfrmFindDlg.FillFindOptions }
procedure TfrmFindDlg.FillFindOptions(out FindOptions: TSearchTemplateRec; SetStartPath: boolean);
begin
  with FindOptions do
  begin
    if SetStartPath then
      StartPath := cmbFindPathStart.Text
    else
      StartPath := '';
    ExcludeDirectories := cmbExcludeDirectories.Text;
    FilesMasks := GetFileMask;
    ExcludeFiles := cmbExcludeFiles.Text;
    SearchDepth := cmbSearchDepth.ItemIndex - 1;
    RegExp := cbRegExp.Checked;
    IsPartialNameSearch := cbPartialNameSearch.Checked;
    FollowSymLinks := cbFollowSymLinks.Checked;
    FindInArchives := cbFindInArchive.Checked;

    { File attributes }
    AttributesPattern := edtAttrib.Text;

    { Date/time }
    DateTimeFrom := 0;
    DateTimeTo := 0;
    IsDateFrom := False;
    IsDateTo := False;
    IsTimeFrom := False;
    IsTimeTo := False;
    if cbDateFrom.Checked then
    begin
      IsDateFrom := True;
      DateTimeFrom := ZVDateFrom.Date;
    end;
    if cbDateTo.Checked then
    begin
      IsDateTo := True;
      DateTimeTo := ZVDateTo.Date;
    end;
    if cbTimeFrom.Checked then
    begin
      IsTimeFrom := True;
      DateTimeFrom := DateTimeFrom + ZVTimeFrom.Time;
    end;
    if cbTimeTo.Checked then
    begin
      IsTimeTo := True;
      DateTimeTo := DateTimeTo + ZVTimeTo.Time;
    end;

    { Not Older Than }
    IsNotOlderThan := cbNotOlderThan.Checked;
    NotOlderThan := seNotOlderThan.Value;
    NotOlderThanUnit := ComboIndexToTimeUnit[cmbNotOlderThanUnit.ItemIndex];

    { File size }
    IsFileSizeFrom := cbFileSizeFrom.Checked;
    IsFileSizeTo := cbFileSizeTo.Checked;
    FileSizeFrom := seFileSizeFrom.Value;
    FileSizeTo := seFileSizeTo.Value;
    FileSizeUnit := ComboIndexToFileSizeUnit[cmbFileSizeUnit.ItemIndex];

    { Find/replace text }
    IsFindText := cbFindText.Checked;
    FindText := cmbFindText.Text;
    IsReplaceText := cbReplaceText.Checked;
    ReplaceText := cmbReplaceText.Text;
    CaseSensitive := cbCaseSens.Checked;
    NotContainingText := cbNotContainingText.Checked;
    TextRegExp := cbTextRegExp.Checked;
    TextEncoding := cmbEncoding.Text;
    { Plugins }
    SearchPlugin := cmbPlugin.Text;
    frmContentPlugins.Save(FindOptions);
  end;
end;

{ TfrmFindDlg.FindOptionsToDSXSearchRec }
procedure TfrmFindDlg.FindOptionsToDSXSearchRec(
  const AFindOptions: TSearchTemplateRec;
  out SRec: TDsxSearchRecord);
begin
  with AFindOptions do
  begin
    FillByte(SRec{%H-}, SizeOf(SRec), 0);

    SRec.StartPath := Copy(StartPath, 1, SizeOf(SRec.StartPath));

    if IsPartialNameSearch then
      SRec.FileMask := '*' + Copy(FilesMasks, 1, SizeOf(SRec.FileMask) - 2) + '*'
    else
      SRec.FileMask := Copy(FilesMasks, 1, SizeOf(SRec.FileMask));

    SRec.Attributes := faAnyFile;  // AttrStrToFileAttr?
    SRec.AttribStr := Copy(AttributesPattern, 1, SizeOf(SRec.AttribStr));

    SRec.CaseSensitive := CaseSensitive;
    {Date search}
    SRec.IsDateFrom := IsDateFrom;
    SRec.IsDateTo := IsDateTo;
    SRec.DateTimeFrom := DateTimeFrom;
    SRec.DateTimeTo := DateTimeTo;
    {Time search}
    SRec.IsTimeFrom := IsTimeFrom;
    SRec.IsTimeTo := IsTimeTo;
    (* File size search *)
    SRec.IsFileSizeFrom := IsFileSizeFrom;
    SRec.IsFileSizeTo := IsFileSizeTo;
    SRec.FileSizeFrom := FileSizeFrom;
    SRec.FileSizeTo := FileSizeTo;
    (* Find text *)
    SRec.NotContainingText := NotContainingText;
    SRec.IsFindText := IsFindText;
    SRec.FindText := Copy(FindText, 1, SizeOf(SRec.FindText));
    (* Replace text *)
    SRec.IsReplaceText := IsReplaceText;
    SRec.ReplaceText := Copy(ReplaceText, 1, SizeOf(SRec.ReplaceText));
  end;
end;

{ TfrmFindDlg.StopSearch }
procedure TfrmFindDlg.StopSearch;
begin
  if FSearchingActive then
  begin
    if (cbUsePlugin.Checked) and (cmbPlugin.ItemIndex <> -1) then
    begin
      if FSearchWithDSXPluginInProgress then
      begin
        DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallStopSearch;
        DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallFinalize;
      end;
      AfterSearchStopped;
      AfterSearchFocus;
    end;

    if Assigned(FFindThread) then
    begin
      FFindThread.Terminate;
      FFindThread := nil;
    end;
  end;
end;

{ TfrmFindDlg.Instance }
class function TfrmFindDlg.Instance: TfrmFindDlg;
begin
  Result:=frmFindDlgUsingPluginDSX;
end;

{ TfrmFindDlg.lbSearchTemplatesDblClick }
procedure TfrmFindDlg.lbSearchTemplatesDblClick(Sender: TObject);
begin
  LoadSelectedTemplate;
end;

{ TfrmFindDlg.AfterSearchStopped }
procedure TfrmFindDlg.AfterSearchStopped;
begin
  actCancel.Enabled := False;
  actStart.Enabled := True;;
  actClose.Enabled := True;
  actNewSearch.Enabled := True;
  actNewSearchClearFilters.Enabled := True;
  actLastSearch.Enabled := True;
  FSearchingActive := False;
  if FSearchWithDSXPluginInProgress then
  begin
    FSearchWithDSXPluginInProgress := False;
    gSearchWithDSXPluginInProgress := False;
  end;
  if FSearchWithWDXPluginInProgress then
  begin
    FSearchWithWDXPluginInProgress := False;
    gSearchWithWDXPluginInProgress := False;
  end;
end;

{ TfrmFindDlg.AfterSearchFocus }
procedure TfrmFindDlg.AfterSearchFocus;
var
  LastButton: TButton;
begin
  if Assigned(Self) and Visible then
  begin
    if FRButtonPanelSender <> nil then  // if user press a keys while search - keep focus on it
    begin
      LastButton := (FRButtonPanelSender as TButton);
      if LastButton.Enabled then LastButton.SetFocus else btnNewSearch.SetFocus;
    end
    else
    begin                          // if user don't press anything - focus on results
      if (pgcSearch.ActivePage = tsResults) and (lsFoundedFiles.Count > 0) then
      begin
        lsFoundedFiles.SetFocus;
        if (lsFoundedFiles.ItemIndex <> -1) then
        begin
          lsFoundedFiles.Selected[lsFoundedFiles.ItemIndex] := True;
        end;
      end
      else
      begin
        if actNewSearch.Enabled then
          btnNewSearch.SetFocus
        else
          btnStart.SetFocus;
      end;
    end;
  end;
end;

{ TfrmFindDlg.FoundedStringCopyChanged }
procedure TfrmFindDlg.FoundedStringCopyChanged(Sender: TObject);
var
  sText: string;
  iTemp: integer;
begin
  if FoundedStringCopy.Count > 0 then
  begin
    iTemp := FoundedStringCopy.Count - 1;
    Sender := FoundedStringCopy.Objects[iTemp];
    sText := FoundedStringCopy[iTemp];
    iTemp := Length(sText);
    if iTemp > lsFoundedFiles.Tag then
    begin
      lsFoundedFiles.Tag := iTemp;
      iTemp := lsFoundedFiles.Canvas.TextWidth(sText);
      if iTemp > lsFoundedFiles.ScrollWidth then
        lsFoundedFiles.ScrollWidth := iTemp + 32;
    end;
    lsFoundedFiles.Items.AddObject(sText, Sender);
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
    Application.ProcessMessages;
{$ENDIF}
  end;
end;

{ TfrmFindDlg.cbFileSizeFromChange }
procedure TfrmFindDlg.cbFileSizeFromChange(Sender: TObject);
begin
  UpdateColor(seFileSizeFrom, cbFileSizeFrom.Checked);
  EnableControl(cmbFileSizeUnit, cbFileSizeFrom.Checked or cbFileSizeTo.Checked);
end;

{ TfrmFindDlg.cbFileSizeToChange }
procedure TfrmFindDlg.cbFileSizeToChange(Sender: TObject);
begin
  UpdateColor(seFileSizeTo, cbFileSizeTo.Checked);
  EnableControl(cmbFileSizeUnit, cbFileSizeFrom.Checked or cbFileSizeTo.Checked);
end;

{ TfrmFindDlg.cbNotOlderThanChange }
procedure TfrmFindDlg.cbNotOlderThanChange(Sender: TObject);
begin
  UpdateColor(seNotOlderThan, cbNotOlderThan.Checked);
  EnableControl(cmbNotOlderThanUnit, cbNotOlderThan.Checked);
end;

{ TfrmFindDlg.cbReplaceTextChange }
procedure TfrmFindDlg.cbReplaceTextChange(Sender: TObject);
begin
  EnableControl(cmbReplaceText, cbReplaceText.Checked and cbFindText.Checked);
  cbNotContainingText.Checked := False;
  cbNotContainingText.Enabled := (not cbReplaceText.Checked and cbFindText.Checked);

  if not FUpdating and cmbReplaceText.Enabled and cmbReplaceText.CanFocus then
  begin
    cmbReplaceText.SetFocus;
    cmbReplaceText.SelectAll;
  end;
end;

{ TfrmFindDlg.cbTimeFromChange }
procedure TfrmFindDlg.cbTimeFromChange(Sender: TObject);
begin
  UpdateColor(ZVTimeFrom, cbTimeFrom.Checked);
end;

{ TfrmFindDlg.cbTimeToChange }
procedure TfrmFindDlg.cbTimeToChange(Sender: TObject);
begin
  UpdateColor(ZVTimeTo, cbTimeTo.Checked);
end;

{ TfrmFindDlg.ThreadTerminate }
procedure TfrmFindDlg.ThreadTerminate(Sender: TObject);
begin
  FFindThread := TFindThread(Sender);
  if FFindThread.TimeOfScan <> 0 then FTimeSearch := ' , ' + rsFindTimeOfScan + formatdatetime('hh:nn:ss.zzz', FFindThread.TimeOfScan);
  FUpdateTimer.OnTimer(FUpdateTimer);
  FUpdateTimer.Enabled := False;
  FFindThread := nil;
  SetWindowCaption(wcs_EndSearch);
  AfterSearchStopped;
  AfterSearchFocus;
end;

{ TfrmFindDlg.FocusOnResults }
procedure TfrmFindDlg.FocusOnResults(Sender: TObject);
begin
  FRButtonPanelSender := Sender;

  if pgcSearch.ActivePage = tsResults then
  begin
    btnStart.Default := False;
    if lsFoundedFiles.SelCount = 0 then lsFoundedFiles.ItemIndex := 0;
    lsFoundedFiles.SetFocus;
    lsFoundedFiles.Selected[lsFoundedFiles.ItemIndex] := True;

  end;
end;

{ TfrmFindDlg.cm_IntelliFocus }
procedure TfrmFindDlg.cm_IntelliFocus(const Params: array of string);
begin
  if FFindThread <> nil then
  begin
    FFindThread.OnTerminate := nil;
    FFindThread.Terminate;
    FUpdateTimer.OnTimer(FUpdateTimer);
    FUpdateTimer.Enabled := False;
    FFindThread := nil;
  end;

  AfterSearchStopped;

  btnStart.Default := True;

  if cmbFindText.Focused then // if F7 on already focused textSearch field- disable text search and set focun on file mask
  begin
    cbFindText.Checked := False;
    cmbFindFileMask.SetFocus;
    cmbFindFileMask.SelectAll;
    exit;
  end
  else
  begin
    pgcSearch.PageIndex := 0;
    cbFindText.Checked := True;
    cmbFindText.SetFocus;
    cmbFindText.SelectAll;
  end;
end;

{ TfrmFindDlg.cm_Start }
procedure TfrmFindDlg.cm_Start(const Params: array of string);
var
  sPath: String;
  sr: TDsxSearchRecord;
  SearchTemplate, TmpTemplate: TSearchTemplateRec;
  PassedSelectedFiles: TStringList = nil;
begin
  cm_Cancel([]);
  Self.Repaint;
  Application.ProcessMessages;

  if (cmbFindPathStart.Text = '') then begin
    cmbFindPathStart.Text:= mbGetCurrentDir;
  end;
  for sPath in SplitPath(cmbFindPathStart.Text) do
  begin
    if not mbDirectoryExists(sPath) then
    begin
      ShowMessage(Format(rsFindDirNoEx, [sPath]));
      Exit;
    end;
  end;

  SaveHistory;
  FAtLeastOneSearchWasDone := True;

  if cbSelectedFiles.Checked and (FSelectedFiles.Count = 0) then
  begin
    ShowMessage(rsMsgNoFilesSelected);
    cbSelectedFiles.Checked := False;
    Exit;
  end;

  // Show search results page
  pgcSearch.ActivePage := tsResults;

  if lsFoundedFiles.CanFocus then
    lsFoundedFiles.SetFocus;

  ClearResults;
  miShowAllFound.Enabled := False;

  FSearchingActive := True;
  actCancel.Enabled := True;

  btnStop.Default := True;

  actStart.Enabled := False;
  actClose.Enabled := False;
  actNewSearch.Enabled := False;
  actNewSearchClearFilters.Enabled := False;
  actLastSearch.Enabled := False;

  if (not frmContentPlugins.chkUsePlugins.Checked) OR (not gSearchWithWDXPluginInProgress) then
  begin
    FillFindOptions(SearchTemplate, True);
    if frmContentPlugins.chkUsePlugins.Checked then
    begin
      gSearchWithWDXPluginInProgress := True;
      FSearchWithWDXPluginInProgress := True;
      frmFindDlgUsingPluginWDX := Self;
    end;

    if not Assigned(FLastSearchTemplate) then
      FLastSearchTemplate := TSearchTemplate.Create;
    TmpTemplate := SearchTemplate;
    TmpTemplate.StartPath := ''; // Don't remember starting path.
    FLastSearchTemplate.SearchRecord := TmpTemplate;

    try
      if (cbUsePlugin.Checked) and (cmbPlugin.ItemIndex <> -1) then
      begin
        if not gSearchWithDSXPluginInProgress then
        begin
          gSearchWithDSXPluginInProgress := True;
          FSearchWithDSXPluginInProgress := True;
          frmFindDlgUsingPluginDSX := Self;
          if DSXPlugins.LoadModule(cmbPlugin.ItemIndex) then
          begin
            FindOptionsToDSXSearchRec(SearchTemplate, sr);
            DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallInit(@SAddFileProc, @SUpdateStatusProc);
            DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallStartSearch(sr);
          end
          else
            StopSearch;
        end
        else
        begin
          MsgError(rsSearchWithDSXPluginInProgress);
          StopSearch;
        end;
      end
      else
      begin
        if cbSelectedFiles.Checked then PassedSelectedFiles := FSelectedFiles;

        if cbOpenedTabs.Checked then
        begin
          frmMain.GetListOpenedPaths(FSelectedFiles);
          PassedSelectedFiles := FSelectedFiles;
        end;

        FFindThread := TFindThread.Create(SearchTemplate, PassedSelectedFiles);
        with FFindThread do
        begin
          Items := FoundedStringCopy;
          OnTerminate := @ThreadTerminate; // will update the buttons after search is finished
        end;

        SetWindowCaption(wcs_StartSearch);

        FTimeSearch := '';
        FFindThread.Start;
        FUpdateTimer.Enabled := True;
        FUpdateTimer.OnTimer(FUpdateTimer);

        FRButtonPanelSender := nil;
      end;
    except
      StopSearch;
      raise;
    end;
  end
  else
  begin
    MsgError(rsSearchWithWDXPluginInProgress);
    StopSearch;
    AfterSearchStopped;
    AfterSearchFocus;
  end;
end; //cm_Start

{ TfrmFindDlg.cm_CancelClose }
procedure TfrmFindDlg.cm_CancelClose(const Params: array of string);
begin
  if FSearchingActive then
    StopSearch
  else
    Close;
end;

{ TfrmFindDlg.cm_Cancel }
procedure TfrmFindDlg.cm_Cancel(const Params: array of string);
begin
  StopSearch;
  AfterSearchStopped;
  AfterSearchFocus;
end;

{ TfrmFindDlg.cm_NewSearch }
procedure TfrmFindDlg.cm_NewSearch(const Params: array of string);
var
  Param: string;
  sActionWithFilters: string = '';
begin
  StopSearch;

  if length(Params) = 0 then
  begin
    case gNewSearchClearFiltersAction of
      fonsClear: sActionWithFilters := 'clear';
      fonsPrompt: if msgYesNo(rsClearFiltersOrNot) then sActionWithFilters := 'clear';
    end;
  end;
  for Param in Params do
    GetParamValue(Param, 'filters', sActionWithFilters);
  if sActionWithFilters = 'clear' then ClearFilter(False);

  pgcSearch.PageIndex := 0;
  ClearResults;
  miShowAllFound.Enabled := False;
  lblStatus.Caption := EmptyStr;
  lblCurrent.Caption := EmptyStr;
  lblFound.Caption := EmptyStr;
  SetWindowCaption(wcs_NewSearch);
  if pgcSearch.ActivePage = tsStandard then cmbFindFileMask.SetFocus;

  btnStart.Default := True;
end;

{ TfrmFindDlg.cm_LastSearch }
procedure TfrmFindDlg.cm_LastSearch(const Params: array of string);
begin
  if Assigned(FLastSearchTemplate) then
  begin
    LoadTemplate(FLastSearchTemplate.SearchRecord);
    pgcSearch.ActivePage := tsStandard;
    cmbFindFileMask.SetFocus;
  end;
end;

{ TfrmFindDlg.cm_View }
procedure TfrmFindDlg.cm_View(const Params: array of string);
begin
  if pgcSearch.ActivePage = tsResults then
    if lsFoundedFiles.ItemIndex <> -1 then
    begin
      if (lsFoundedFiles.Items.Objects[lsFoundedFiles.ItemIndex] <> nil) then
        msgError(rsMsgErrNotSupported)
      else
        ShowViewerByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
    end;
end;

{ TfrmFindDlg.cm_Edit }
procedure TfrmFindDlg.cm_Edit(const Params: array of string);
begin
  if pgcSearch.ActivePage = tsResults then
    if lsFoundedFiles.ItemIndex <> -1 then
    begin
      if (lsFoundedFiles.Items.Objects[lsFoundedFiles.ItemIndex] <> nil) then
        msgError(rsMsgErrNotSupported)
      else
        ShowEditorByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
    end;
end;

{ TfrmFindDlg.cm_GoToFile }
procedure TfrmFindDlg.cm_GoToFile(const Params: array of string);
var
  AFile: TFile = nil;
  TargetFile: string;
  ArchiveFile: string;
  FileSource: IFileSource;
begin
  if lsFoundedFiles.ItemIndex <> -1 then
    try
      StopSearch;
      TargetFile := lsFoundedFiles.Items[lsFoundedFiles.ItemIndex];
      if (lsFoundedFiles.Items.Objects[lsFoundedFiles.ItemIndex] <> nil) then
      begin
        ArchiveFile := ExtractWord(1, TargetFile, [ReversePathDelim]);
        TargetFile := PathDelim + ExtractWord(2, TargetFile, [ReversePathDelim]);
        AFile := TFileSystemFileSource.CreateFileFromFile(ArchiveFile);
        try
          FileSource:= GetArchiveFileSource(TFileSystemFileSource.GetFileSource, AFile, EmptyStr, False, False);
        finally
          AFile.Free;
        end;
        if Assigned(FileSource) then
        begin
          frmMain.ActiveFrame.AddFileSource(FileSource, ExtractFilePath(TargetFile));
          frmMain.ActiveFrame.SetActiveFile(ExtractFileName(TargetFile));
        end;
      end
      else
      begin
        if not mbFileSystemEntryExists(TargetFile) then begin
          msgError(rsMsgObjectNotExists + LineEnding + TargetFile);
          Exit;
        end;
        SetFileSystemPath(frmMain.ActiveFrame, ExtractFilePath(TargetFile));
        frmMain.ActiveFrame.SetActiveFile(ExtractFileName(TargetFile));
      end;
      frmMain.RestoreWindow;
      Close;
    except
      on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
end;

{ TfrmFindDlg.cm_FeedToListbox }
procedure TfrmFindDlg.cm_FeedToListbox(const Params: array of string);
var
  I: integer;
  sFileName: string;
  SearchResultFS: ISearchResultFileSource;
  FileList: TFileTree;
  aFile: TFile;
  Notebook: TFileViewNotebook;
  NewPage: TFileViewPage;
begin
  StopSearch;

  FileList := TFileTree.Create;
  for i := 0 to lsFoundedFiles.Items.Count - 1 do
  begin
    sFileName := lsFoundedFiles.Items[I];
    try
      aFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
      FileList.AddSubNode(aFile);
    except
      on EFileNotFound do ;
    end;
  end;

  // Create search result file source.
  // Currently only searching FileSystem is supported.
  SearchResultFS := TSearchResultFileSource.Create;
  SearchResultFS.AddList(FileList, TFileSystemFileSource.GetFileSource);

  // Add new tab for search results.
  Notebook := frmMain.ActiveNotebook;
  NewPage := Notebook.NewPage(Notebook.ActiveView);
  NewPage.FileView.AddFileSource(SearchResultFS, SearchResultFS.GetRootDir);
  NewPage.FileView.FlatView := True;
  NewPage.MakeActive;

  Close;
end;

procedure TfrmFindDlg.cm_PageNext(const Params: array of string);
begin
  with pgcSearch do
  begin
    if PageIndex = PageCount - 1 then
      ActivePage := Pages[0]
    else
      ActivePage := Pages[PageIndex + 1];
  end;
end;

procedure TfrmFindDlg.cm_PagePrev(const Params: array of string);
begin
  with pgcSearch do
  begin
    if PageIndex = 0 then
      ActivePage := Pages[PageCount - 1]
    else
      ActivePage := Pages[PageIndex - 1];
  end;
end;

{ TfrmFindDlg.cm_PageStandard }
procedure TfrmFindDlg.cm_PageStandard(const Params: array of string);
begin
  pgcSearch.ActivePage := tsStandard;
end;

{ TfrmFindDlg.cm_PageAdvanced }
procedure TfrmFindDlg.cm_PageAdvanced(const Params: array of string);
begin
  pgcSearch.ActivePage := tsAdvanced;
end;

{ TfrmFindDlg.cm_PagePlugins }
procedure TfrmFindDlg.cm_PagePlugins(const Params: array of string);
begin
  pgcSearch.ActivePage := tsPlugins;
end;

{ TfrmFindDlg.cm_PageLoadSave }
procedure TfrmFindDlg.cm_PageLoadSave(const Params: array of string);
begin
  pgcSearch.ActivePage := tsLoadSave;
end;

{ TfrmFindDlg.cm_PageResults }
procedure TfrmFindDlg.cm_PageResults(const Params: array of string);
begin
  pgcSearch.ActivePage := tsResults;
end;

{ TfrmFindDlg.FormCloseQuery }
procedure TfrmFindDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FFindThread <> nil then  // We can't call StopSearch because it method will set focus on unavailable field
  begin
    FFindThread.OnTerminate := nil;
    FFindThread.Terminate;
    FUpdateTimer.OnTimer(FUpdateTimer);
    FUpdateTimer.Enabled := False;
    FFindThread := nil;
  end;

  AfterSearchStopped;

  btnStart.Default := True;

  CanClose := not Assigned(FFindThread);
end;

{ TfrmFindDlg.FormDestroy }
procedure TfrmFindDlg.FormDestroy(Sender: TObject);
begin
{$IF DEFINED(FIX_DEFAULT)}
  if ListOffrmFindDlgInstance.Count = 0 then
    Application.RemoveOnKeyDownBeforeHandler(@FormKeyDown);
{$ENDIF}
  FreeAndNil(FoundedStringCopy);
  FreeAndNil(DsxPlugins);
end;

{$IF DEFINED(FIX_DEFAULT)}
procedure TfrmFindDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AParentForm: TCustomForm;
begin
  if Key = VK_RETURN then
  begin
    if Sender is TControl then
    begin
      AParentForm := GetParentForm(TControl(Sender));
      if (AParentForm is TfrmFindDlg) then
      begin
        if (Sender = TfrmFindDlg(AParentForm).lsFoundedFiles) then
          TCustomListBox(Sender).OnKeyDown(Sender, Key, Shift)
        else if (Sender is TCustomButton) then
        begin
          TCustomButton(Sender).Click;
          Key:= 0;
        end
{$if (lcl_fullversion < 1090000) and defined(lclgtk2)}
        else begin
          Key := 0;
          if btnStart.Enabled then
            btnStart.Click
          else
            btnStop.Click;
        end;
{$endif}
      end;
    end;
  end;
end;
{$ENDIF}

{ TfrmFindDlg.FormClose }
procedure TfrmFindDlg.frmFindDlgClose(Sender: TObject; var CloseAction: TCloseAction);
const
  CLOSETAG: longint = $233528DE;
var
  iSearchingForm: integer;
begin
  if Assigned(FFrmAttributesEdit) then
  begin
    FFrmAttributesEdit.Close;
    FreeAndNil(FFrmAttributesEdit);
  end;

  // Remove the whole thing from memory if no search was made at all.
  // We remove it also if we've been asked to remove it.
  // We ned to remove it from our list of instances "ListOffrmFindDlgInstance".
  // We'll use the trick to give current form a magic tag number and then pass the list to delete the matching one.
  if (not FAtLeastOneSearchWasDone) or FFreeOnClose then
  begin
    tag := CLOSETAG;
    for iSearchingForm := pred(ListOffrmFindDlgInstance.Count) downto 0 do
      if ListOffrmFindDlgInstance.frmFindDlgInstance[iSearchingForm].Tag = CLOSETAG then
        ListOffrmFindDlgInstance.Delete(iSearchingForm);

    CloseAction := caFree; // This will destroy the from on next step in the flow.
  end;
end;

{ TfrmFindDlg.SetWindowCaption }
procedure TfrmFindDlg.SetWindowCaption(AWindowCaptionStyle: byte);
var
  sBuildingCaptionName: string;
begin
  sBuildingCaptionName := rsFindSearchFiles;

  case (AWindowCaptionStyle and $07) of
    2: sBuildingCaptionName := sBuildingCaptionName + ' - ' + rsFindScanning;
    3: sBuildingCaptionName := sBuildingCaptionName + ' - ' + rsOperFinished;
  end;

  if (AWindowCaptionStyle and $10) <> 0 then
    sBuildingCaptionName := sBuildingCaptionName + ' - ' + lblFound.Caption;

  if (AWindowCaptionStyle and $08) <> 0 then
  begin
    sBuildingCaptionName := sBuildingCaptionName + ' - File: ' + GetFileMask;
    if cbFindText.Checked then
      sBuildingCaptionName := sBuildingCaptionName + ' - Text:' + cmbFindText.Text;
  end;

  Caption := sBuildingCaptionName;
end;

function TfrmFindDlg.GetFileMask: String;
begin
  if Length(cmbFindFileMask.Text) = 0 then
    Result := AllFilesMask
  else begin
    Result := cmbFindFileMask.Text;
  end;
end;

{ TfrmFindDlg.LoadHistory }
procedure TfrmFindDlg.LoadHistory;
begin
  cmbFindFileMask.Items.Assign(glsMaskHistory);
  cmbFindPathStart.Items.Assign(glsSearchDirectories);
  cmbExcludeDirectories.Items.Assign(glsSearchExcludeDirectories);
  cmbExcludeFiles.Items.Assign(glsSearchExcludeFiles);
  cmbFindText.Items.Assign(glsSearchHistory);
  cmbReplaceText.Items.Assign(glsReplaceHistory);
end;

{ TfrmFindDlg.SaveHistory }
procedure TfrmFindDlg.SaveHistory;
begin
  // 1. Add to find mask history
  InsertFirstItem(cmbFindFileMask.Text, cmbFindFileMask);
  glsMaskHistory.Assign(cmbFindFileMask.Items);

  // 1. Add to find directory history
  InsertFirstItem(cmbFindPathStart.Text, cmbFindPathStart);
  glsSearchDirectories.Assign(cmbFindPathStart.Items);

  // 2. Add to exclude directories history
  InsertFirstItem(cmbExcludeDirectories.Text, cmbExcludeDirectories);
  glsSearchExcludeFiles.Assign(cmbExcludeFiles.Items);

  // 3. Add to exclude files history
  InsertFirstItem(cmbExcludeFiles.Text, cmbExcludeFiles);
  glsSearchExcludeDirectories.Assign(cmbExcludeDirectories.Items);

  // 4. Add to search text history
  if cbFindText.Checked then
  begin
    InsertFirstItem(cmbFindText.Text, cmbFindText);
    // Update search history, so it can be used in
    // Viewer/Editor opened from find files dialog
    gFirstTextSearch := False;
    glsSearchHistory.Assign(cmbFindText.Items);
  end;

  // 5. Add to replace text history
  if cbReplaceText.Checked then
  begin
    InsertFirstItem(cmbReplaceText.Text, cmbReplaceText);
    // Update replace history, so it can be used in
    // Editor opened from find files dialog (issue 0000539)
    glsReplaceHistory.Assign(cmbReplaceText.Items);
  end;
end;

{ TfrmFindDlg.FormShow }
procedure TfrmFindDlg.frmFindDlgShow(Sender: TObject);
var
  I: integer;
begin
  pgcSearch.PageIndex := 0;

  if cmbFindFileMask.Visible then
    cmbFindFileMask.SelectAll;

  cbPartialNameSearch.Checked := gPartialNameSearch;
  lsFoundedFiles.Canvas.Font := lsFoundedFiles.Font;

  cmbPlugin.Clear;
  for I := 0 to DSXPlugins.Count - 1 do
    cmbPlugin.AddItem(DSXPlugins.GetDSXModule(i).Name + ' (' + DSXPlugins.GetDSXModule(I).Descr + ' )', nil);
  if (cmbPlugin.Items.Count > 0) then cmbPlugin.ItemIndex := 0;

  if pgcSearch.ActivePage = tsStandard then
    if cmbFindFileMask.CanFocus then
      cmbFindFileMask.SetFocus;

  cbSelectedFiles.Checked := FSelectedFiles.Count > 0;
  cbSelectedFiles.Enabled := cbSelectedFiles.Checked;
end;

{ TfrmFindDlg.gbDirectoriesResize }
procedure TfrmFindDlg.gbDirectoriesResize(Sender: TObject);
begin
  pnlDirectoriesDepth.Width := gbDirectories.Width div 3;
end;

{ TfrmFindDlg.lbSearchTemplatesSelectionChange }
procedure TfrmFindDlg.lbSearchTemplatesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbSearchTemplates.ItemIndex < 0 then
    lblSearchContents.Caption := ''
  else
  begin
    with gSearchTemplateList.Templates[lbSearchTemplates.ItemIndex].SearchRecord do
    begin
      if StartPath <> '' then
        lblSearchContents.Caption := '"' + FilesMasks + '" -> "' + StartPath + '"'
      else
        lblSearchContents.Caption := '"' + FilesMasks + '"';
    end;
  end;
end;

{ TfrmFindDlg.LoadSelectedTemplate }
procedure TfrmFindDlg.LoadSelectedTemplate;
var
  SearchTemplate: TSearchTemplate;
begin
  if lbSearchTemplates.ItemIndex < 0 then Exit;
  SearchTemplate := gSearchTemplateList.Templates[lbSearchTemplates.ItemIndex];
  if Assigned(SearchTemplate) then
  begin
    FLastTemplateName := SearchTemplate.TemplateName;
    LoadTemplate(SearchTemplate.SearchRecord);
  end;
end;

{ TfrmFindDlg.LoadTemplate }
procedure TfrmFindDlg.LoadTemplate(const Template: TSearchTemplateRec);
begin
  with Template do
  begin
    if StartPath <> '' then
      cmbFindPathStart.Text := StartPath;
    cmbExcludeDirectories.Text := ExcludeDirectories;
    cmbFindFileMask.Text := FilesMasks;
    cmbExcludeFiles.Text := ExcludeFiles;
    if (SearchDepth + 1 >= 0) and (SearchDepth + 1 < cmbSearchDepth.Items.Count) then
      cmbSearchDepth.ItemIndex := SearchDepth + 1
    else
      cmbSearchDepth.ItemIndex := 0;
    cbRegExp.Checked := RegExp;
    cbPartialNameSearch.Checked := IsPartialNameSearch;
    cbFollowSymLinks.Checked := FollowSymLinks;
    cbFindInArchive.Checked := FindInArchives;
    // attributes
    edtAttrib.Text := AttributesPattern;
    // file date/time
    cbDateFrom.Checked := IsDateFrom;
    if IsDateFrom then
      ZVDateFrom.Date := DateTimeFrom;

    cbDateTo.Checked := IsDateTo;
    if IsDateTo then
      ZVDateTo.Date := DateTimeTo;

    cbTimeFrom.Checked := IsTimeFrom;
    if IsTimeFrom then
      ZVTimeFrom.Time := DateTimeFrom;

    cbTimeTo.Checked := IsTimeTo;
    if IsTimeTo then
      ZVTimeTo.Time := DateTimeTo;

    // not older then
    cbNotOlderThan.Checked := IsNotOlderThan;
    seNotOlderThan.Value := NotOlderThan;
    cmbNotOlderThanUnit.ItemIndex := TimeUnitToComboIndex[NotOlderThanUnit];
    // file size
    cbFileSizeFrom.Checked := IsFileSizeFrom;
    cbFileSizeTo.Checked := IsFileSizeTo;
    seFileSizeFrom.Value := FileSizeFrom;
    seFileSizeTo.Value := FileSizeTo;
    cmbFileSizeUnit.ItemIndex := FileSizeUnitToComboIndex[FileSizeUnit];
    // find/replace text
    cbFindText.Checked := IsFindText;
    cmbFindText.Text := FindText;
    cbReplaceText.Checked := IsReplaceText;
    cmbReplaceText.Text := ReplaceText;
    cbCaseSens.Checked := CaseSensitive;
    cbNotContainingText.Checked := NotContainingText;
    cbTextRegExp.Checked := TextRegExp;
    cmbEncoding.Text := TextEncoding;
    // plugins
    cmbPlugin.Text := SearchPlugin;
    frmContentPlugins.Load(Template);
  end;
end;

{ TfrmFindDlg.lsFoundedFilesDblClick }
procedure TfrmFindDlg.lsFoundedFilesDblClick(Sender: TObject);
begin
  cm_GoToFile([]);
end;

{ TfrmFindDlg.lsFoundedFilesKeyDown }
procedure TfrmFindDlg.lsFoundedFilesKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if (Shift = []) and (lsFoundedFiles.ItemIndex <> -1) then
  begin
    case Key of
      VK_DELETE:
      begin
        miRemoveFromLlistClick(Sender);
        Key := 0;
      end;

      VK_RETURN:
      begin
        if not FSearchingActive then
        begin
          cm_GotoFile([]);
          Key := 0;
        end;
      end;

      VK_RIGHT, VK_LEFT:
      begin
        if not FSearchingActive then
        begin
          if FRButtonPanelSender <> nil then (FRButtonPanelSender as TButton).SetFocus else btnNewSearch.SetFocus;
          Key := 0;
        end
        else
        begin
          Key := 0;
          btnStop.SetFocus;
        end;
      end;

    end;

  end;
end;

{ TfrmFindDlg.lsFoundedFilesMouseDown }
procedure TfrmFindDlg.lsFoundedFilesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  i := lsFoundedFiles.ItemAtPos(Point(X, Y), False);

  if (i >= 0) then
  begin
    LastClickResultsPath := GetDeepestExistingPath(lsFoundedFiles.Items[i]);

    if (Button = mbRight) and (lsFoundedFiles.Selected[i] <> True) then
    begin
      lsFoundedFiles.ClearSelection;
      lsFoundedFiles.Selected[i] := True;
    end;
  end;
end;

{ TfrmFindDlg.lsFoundedFilesMouseUp }
procedure TfrmFindDlg.lsFoundedFilesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i: integer;
  sPath: string;
  AFile: TFile;
  AFiles: TFiles;
  pt: TPoint;
begin
  if Button = mbRight then
  begin

    if Shift = [ssCtrl] then // Show System context menu
    begin

      {$IF DEFINED(MSWINDOWS)}
      try
        AFiles := TFiles.Create(LastClickResultsPath);
        AFiles.Path := LastClickResultsPath;

        i := 0;
        while i < lsFoundedFiles.Count do
        begin
          if lsFoundedFiles.Selected[i] then
          begin
            sPath := lsFoundedFiles.Items[i];
            AFile := TFileSystemFileSource.CreateFile(sPath);
            AFiles.Add(aFile);
          end;
          Inc(i);
        end;

        try
          pt.X := X;
          pt.Y := Y;
          pt := ClientToScreen(pt);
          ShowContextMenu(lsFoundedFiles, AFiles, pt.X, pt.Y, False, nil);
        finally
          FreeAndNil(AFiles);
        end;


      except
        on E: EContextMenuException do
          ShowException(E)
        else;
      end;

      {$ENDIF}

    end
    else
    begin
      PopupMenuFind.PopUp;  // Show DC menu
    end;

  end;
end;

{ TfrmFindDlg.lsFoundedFilesMouseWheelDown }
procedure TfrmFindDlg.lsFoundedFilesMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  if (Shift = [ssCtrl]) and (gFonts[dcfSearchResults].Size > MIN_FONT_SIZE_FILE_SEARCH_RESULTS) then
  begin
    lsFoundedFiles.Font.Size := lsFoundedFiles.Font.Size - 1;
    Handled := True;
  end;
end;

{ TfrmFindDlg.lsFoundedFilesMouseWheelUp }
procedure TfrmFindDlg.lsFoundedFilesMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  if (Shift = [ssCtrl]) and (gFonts[dcfSearchResults].Size < MAX_FONT_SIZE_FILE_SEARCH_RESULTS) then
  begin
    lsFoundedFiles.Font.Size := lsFoundedFiles.Font.Size + 1;
    Handled := True;
  end;
end;

{ TfrmFindDlg.miOpenInNewTabClick }
procedure TfrmFindDlg.miOpenInNewTabClick(Sender: TObject);
var
  i: integer;
  sPath: string;

  Notebook: TFileViewNotebook;
  NewPage: TFileViewPage;

begin
  Notebook := frmMain.ActiveNotebook;

  i := 0;
  while i < lsFoundedFiles.Count do
  begin
    if lsFoundedFiles.Selected[i] then
    begin
      sPath := lsFoundedFiles.Items[i];
      sPath := GetDeepestExistingPath(sPath);

      NewPage := Notebook.NewPage(Notebook.ActiveView);
      NewPage.FileView.CurrentPath := sPath;
      NewPage.FileView.SetActiveFile(ExtractFileName(lsFoundedFiles.Items[i]));
    end;
    Inc(i);
  end;

end;

{ TfrmFindDlg.miRemoveFromLlistClick }
procedure TfrmFindDlg.miRemoveFromLlistClick(Sender: TObject);
var
  i: integer;
begin
  if lsFoundedFiles.ItemIndex = -1 then Exit;
  if lsFoundedFiles.SelCount = 0 then Exit;

  for i := lsFoundedFiles.Items.Count - 1 downto 0 do
    if lsFoundedFiles.Selected[i] then
      lsFoundedFiles.Items.Delete(i);

  miShowAllFound.Enabled := True;
end;

{ TfrmFindDlg.miShowAllFoundClick }
procedure TfrmFindDlg.miShowAllFoundClick(Sender: TObject);
begin
  lsFoundedFiles.Clear;
  lsFoundedFiles.Items.AddStrings(FoundedStringCopy);

  miShowAllFound.Enabled := False;
end;

{ TfrmFindDlg.miShowInEditorClick }
procedure TfrmFindDlg.miShowInEditorClick(Sender: TObject);
begin
  if lsFoundedFiles.ItemIndex >= 0 then
    ShowEditorByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
end;

{ TfrmFindDlg.miShowInViewerClick }
procedure TfrmFindDlg.miShowInViewerClick(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  if lsFoundedFiles.ItemIndex = -1 then Exit;

  sl := TStringList.Create;
  try
    for i := 0 to lsFoundedFiles.Items.Count - 1 do
      if lsFoundedFiles.Selected[i] then
        sl.Add(lsFoundedFiles.Items[i]);
    ShowViewer(sl);
  finally
    sl.Free;
  end;
end;

{ TfrmFindDlg.seFileSizeFromChange }
procedure TfrmFindDlg.seFileSizeFromChange(Sender: TObject);
begin
  if not FUpdating then
    cbFileSizeFrom.Checked := (seFileSizeFrom.Value > 0);
end;

{ TfrmFindDlg.seFileSizeToChange }
procedure TfrmFindDlg.seFileSizeToChange(Sender: TObject);
begin
  if not FUpdating then
    cbFileSizeTo.Checked := (seFileSizeTo.Value > 0);
end;

{ TfrmFindDlg.SelectTemplate }
procedure TfrmFindDlg.SelectTemplate(const ATemplateName: string);
var
  i: integer;
begin
  for i := 0 to lbSearchTemplates.Count - 1 do
    if lbSearchTemplates.Items[i] = ATemplateName then
    begin
      lbSearchTemplates.ItemIndex := i;
      Break;
    end;
end;

{ TfrmFindDlg.seNotOlderThanChange }
procedure TfrmFindDlg.seNotOlderThanChange(Sender: TObject);
begin
  if not FUpdating then
    cbNotOlderThan.Checked := (seNotOlderThan.Value > 0);
end;

{ TfrmFindDlg.tsLoadSaveShow }
procedure TfrmFindDlg.tsLoadSaveShow(Sender: TObject);
begin
  UpdateTemplatesList;
  if (lbSearchTemplates.Count > 0) and (lbSearchTemplates.ItemIndex = -1) then
    lbSearchTemplates.ItemIndex := 0;
end;

{ TfrmFindDlg.tsStandardEnter }
procedure TfrmFindDlg.tsStandardEnter(Sender: TObject);
begin
  btnStart.Default := True;
end;

{ TfrmFindDlg.UpdateTemplatesList }
procedure TfrmFindDlg.UpdateTemplatesList;
var
  OldIndex: integer;
begin
  OldIndex := lbSearchTemplates.ItemIndex;
  gSearchTemplateList.LoadToStringList(lbSearchTemplates.Items);
  if OldIndex <> -1 then
    lbSearchTemplates.ItemIndex := OldIndex;
end;

{ TfrmFindDlg.OnUpdateTimer }
procedure TfrmFindDlg.OnUpdateTimer(Sender: TObject);
begin
  if Assigned(FFindThread) then
  begin
    lblStatus.Caption := Format(rsFindScanned, [FFindThread.FilesScanned]) + FTimeSearch;
    lblFound.Caption := Format(rsFindFound, [FFindThread.FilesFound]);
    lblCurrent.Caption := rsFindScanning + ': ' + FFindThread.CurrentDir;
  end;
end;

{ TfrmFindDlg.ZVDateFromChange }
procedure TfrmFindDlg.ZVDateFromChange(Sender: TObject);
begin
  if not FUpdating then
    cbDateFrom.Checked := True;
end;

{ TfrmFindDlg.ZVDateToChange }
procedure TfrmFindDlg.ZVDateToChange(Sender: TObject);
begin
  if not FUpdating then
    cbDateTo.Checked := True;
end;

{ TfrmFindDlg.ZVTimeFromChange }
procedure TfrmFindDlg.ZVTimeFromChange(Sender: TObject);
begin
  if not FUpdating then
    cbTimeFrom.Checked := True;
end;

{ TfrmFindDlg.ZVTimeToChange }
procedure TfrmFindDlg.ZVTimeToChange(Sender: TObject);
begin
  if not FUpdating then
    cbTimeTo.Checked := True;
end;

procedure TfrmFindDlg.PopupMenuFindPopup(Sender: TObject);
begin
  if (lsFoundedFiles.ItemIndex <> -1) then
  begin
    miShowInViewer.Enabled:= (lsFoundedFiles.Items.Objects[lsFoundedFiles.ItemIndex] = nil);
    miShowInEditor.Enabled:= (lsFoundedFiles.Items.Objects[lsFoundedFiles.ItemIndex] = nil);
  end;
end;

{ TfrmFindDlg.OnAddAttribute }
procedure TfrmFindDlg.OnAddAttribute(Sender: TObject);
var
  sAttr: string;
begin
  sAttr := edtAttrib.Text;
  if edtAttrib.SelStart > 0 then
    // Insert at caret position.
    Insert((Sender as TfrmAttributesEdit).AttrsAsText, sAttr, edtAttrib.SelStart + 1)
  else
    sAttr := sAttr + (Sender as TfrmAttributesEdit).AttrsAsText;
  edtAttrib.Text := sAttr;
end;

{ TfrmFindDlg.InvalidRegExpr }
function TfrmFindDlg.InvalidRegExpr(AChecked: boolean; const ARegExpr: string): boolean;
var
  sMsg: string;
begin
  Result := False;
  if AChecked then
    try
      ExecRegExpr(ARegExpr, '');
    except
      on E: Exception do
      begin
        Result := True;
        sMsg := StringReplace(cbRegExp.Caption, '&', '', [rfReplaceAll]);
        MessageDlg(sMsg + ': ' + E.Message, mtError, [mbOK], 0);
      end;
    end;
end;

{ TfrmFindDlg.pgcSearchChange }
procedure TfrmFindDlg.pgcSearchChange(Sender: TObject);
begin
  if pgcSearch.ActivePage = tsStandard then
  begin
    if (not cmbFindFileMask.Focused) and (cmbFindFileMask.CanFocus) then
      cmbFindFileMask.SetFocus;
  end
  else
  if pgcSearch.ActivePage = tsResults then
  begin
    if (not lsFoundedFiles.Focused) and (lsFoundedFiles.CanFocus) then
      lsFoundedFiles.SetFocus;
  end;
end;

{ TfrmFindDlg.SaveTemplate }
procedure TfrmFindDlg.SaveTemplate(SaveStartingPath: boolean);
var
  sName: string;
  SearchTemplate: TSearchTemplate;
  SearchRec: TSearchTemplateRec;
begin
  if InvalidRegExpr(cbRegExp.Checked, cmbFindFileMask.Text) or
     InvalidRegExpr(cbTextRegExp.Checked, cmbFindText.Text) then
    Exit;

  sName := FLastTemplateName;
  if not InputQuery(rsFindSaveTemplateCaption, rsFindSaveTemplateTitle, sName) then
  begin
    ModalResult := mrCancel;
    Exit;
  end;

  FLastTemplateName := sName;
  SearchTemplate := gSearchTemplateList.TemplateByName[sName];
  if Assigned(SearchTemplate) then
  begin
    // TODO: Ask for overwriting existing template.
    FillFindOptions(SearchRec, SaveStartingPath);
    SearchTemplate.SearchRecord := SearchRec;
    Exit;
  end;

  SearchTemplate := TSearchTemplate.Create;
  try
    SearchTemplate.TemplateName := sName;
    FillFindOptions(SearchRec, SaveStartingPath);
    SearchTemplate.SearchRecord := SearchRec;
    gSearchTemplateList.Add(SearchTemplate);
  except
    FreeAndNil(SearchTemplate);
    raise;
  end;
  UpdateTemplatesList;
  SelectTemplate(FLastTemplateName);
end;

procedure TfrmFindDlg.CancelCloseAndFreeMem;
begin
  cm_FreeFromMem([]);
end;

{ TfrmFindDlg.cm_Close }
procedure TfrmFindDlg.cm_Close(const Params: array of string);
begin
  Close;
end;

{ TfrmFindDlg.cm_NewSearchClearFilters }
procedure TfrmFindDlg.cm_NewSearchClearFilters(const Params: array of string);
begin
  cm_NewSearch(['filters=clear']);
end;


{ TfrmFindDlg.cm_FreeFromMem }
// We will set the flag "FFreeOnClose" to "true" (it was to "false" since "FormCreate".
// This flag will be checked in "FormClose" to set "CloseAction" to "caFree" so form will be destroy.
// But we need to remove the pointer to that form from our "ListOffrmFindDlgInstance".
// To determine which one to remove, we set the tag to a magic number, then scan our list and delete the one pointing the form with that magic number.
// We just delete the pointer. The actual form will be destroyed properly because of the "CloseAction" set to "caFree".
procedure TfrmFindDlg.cm_FreeFromMem(const {%H-}Params: array of string);
var
  iSearchingForm: integer;
const
  CLOSETAG: longint = $233528DE;
begin
  if FSearchingActive then StopSearch;

  // Remove our pointer from our list of forms
  tag := CLOSETAG;
  for iSearchingForm := pred(ListOffrmFindDlgInstance.Count) downto 1 do
  begin
    if ListOffrmFindDlgInstance.frmFindDlgInstance[iSearchingForm].Tag = CLOSETAG then
      ListOffrmFindDlgInstance.Delete(iSearchingForm);
  end;

  FFreeOnClose := True; // Prepare the "free mem"

  // Do the "close"
  Close;
end;

{ TfrmFindDlg.cm_FreeFromMemAllOthers }
// We set the tag of our actual current form to a magic number and then scan
// all forms in our list to close all the ones that does not have that magic
// tag number.
procedure TfrmFindDlg.cm_FreeFromMemAllOthers(const {%H-}Params: array of string);
const
  KEEPOPENTAG: longint = $270299;
var
  iIndex: integer;
begin
  if ListOffrmFindDlgInstance.Count > 1 then
  begin
    tag := KEEPOPENTAG;
    try
      for iIndex := pred(ListOffrmFindDlgInstance.Count) downto 0 do
        if ListOffrmFindDlgInstance.frmFindDlgInstance[iIndex].Tag <> KEEPOPENTAG then
          ListOffrmFindDlgInstance.frmFindDlgInstance[iIndex].CancelCloseAndFreeMem;
    finally
      tag := 0; // Don't forget to set back tag to 0!!!
    end;
  end
  else
  begin
    msgOK(rsNoOtherFindFilesWindowToClose);
  end;
end;

{ TfrmFindDlg.cm_ConfigFileSearchHotKeys }
procedure TfrmFindDlg.cm_ConfigFileSearchHotKeys(const {%H-}Params: array of string);
begin
  frmMain.Commands.cm_ConfigHotKeys([Format('category=%s', [rsHotkeyCategoryFindFiles])]);
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmFindDlg, HotkeysCategory, @rsHotkeyCategoryFindFiles);
  ListOffrmFindDlgInstance := TListOffrmFindDlgInstance.Create;

finalization
  ListOffrmFindDlgInstance.Destroy; // "Destroy" does call the "Clear" who will free the forms.
end.

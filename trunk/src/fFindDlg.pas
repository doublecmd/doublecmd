{
   Double Commander
   -------------------------------------------------------------------------
   Find dialog, with searching in thread

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)

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
  ExtCtrls, Menus, EditBtn, Spin, Buttons, ZVDateTimePicker,
  fAttributesEdit, uDsxModule, DsxPlugin, uFindThread, uFindFiles;

type

  { TfrmFindDlg }

  TfrmFindDlg = class(TForm)
    Bevel2: TBevel;
    btnAddAttribute: TButton;
    btnAttrsHelp: TButton;
    btnClose: TButton;
    btnGoToPath: TButton;
    btnNewSearch: TButton;
    btnSaveTemplate: TButton;
    btnSearchDelete: TButton;
    btnSearchLoad: TButton;
    btnSearchSave: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnView: TButton;
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
    cmbFollowSymLinks: TCheckBox;
    cmbNotOlderThanUnit: TComboBox;
    cmbFileSizeUnit: TComboBox;
    cbUsePlugin: TCheckBox;
    cmbPlugin: TComboBox;
    cmbEncoding: TComboBox;
    cbSearchDepth: TComboBox;
    cbRegExp: TCheckBox;
    cmbReplaceText: TComboBox;
    cmbFindText: TComboBox;
    edtFindPathStart: TDirectoryEdit;
    edtAttrib: TEdit;
    gbAttributes: TGroupBox;
    gbFindOptions: TGroupBox;
    lblCurrent: TLabel;
    lblFound: TLabel;
    lblStatus: TLabel;
    lblTemplateHeader: TLabel;
    lbSearchTemplates: TListBox;
    lblSearchContents: TPanel;
    lblSearchDepth: TLabel;
    lblEncoding: TLabel;
    lsFoundedFiles: TListBox;
    CheksPanel: TPanel;
    miShowAllFound: TMenuItem;
    miRemoveFromLlist: TMenuItem;
    pnlLoadSaveBottomButtons: TPanel;
    pnlLoadSaveBottom: TPanel;
    pnlRightButtons: TPanel;
    pnlButtons: TPanel;
    pnlResultsBottomButtons: TPanel;
    pnlMainButtons: TPanel;
    pnlResults: TPanel;
    pnlStatus: TPanel;
    pnlResultsBottom: TPanel;
    seNotOlderThan: TSpinEdit;
    seFileSizeFrom: TSpinEdit;
    seFileSizeTo: TSpinEdit;
    pnlFindFile: TPanel;
    pgcSearch: TPageControl;
    tsResults: TTabSheet;
    tsLoadSave: TTabSheet;
    tsStandard: TTabSheet;
    lblFindPathStart: TLabel;
    lblFindFileMask: TLabel;
    cmbFindFileMask: TComboBox;
    gbFindData: TGroupBox;
    cbCaseSens: TCheckBox;
    tsAdvanced: TTabSheet;
    PopupMenuFind: TPopupMenu;
    miShowInViewer: TMenuItem;
    ZVDateFrom: TZVDateTimePicker;
    ZVDateTo: TZVDateTimePicker;
    ZVTimeFrom: TZVDateTimePicker;
    ZVTimeTo: TZVDateTimePicker;
    procedure btnAddAttributeClick(Sender: TObject);
    procedure btnAttrsHelpClick(Sender: TObject);
    procedure btnSearchDeleteClick(Sender: TObject);
    procedure btnSearchLoadClick(Sender: TObject);
    procedure btnSearchSaveClick(Sender: TObject);
    procedure cbDateFromChange(Sender: TObject);
    procedure cbDateToChange(Sender: TObject);
    procedure cbPartialNameSearchChange(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure cmbEncodingSelect(Sender: TObject);
    procedure cbFindTextChange(Sender: TObject);
    procedure cbUsePluginChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGoToPathClick(Sender: TObject);
    procedure btnNewSearchClick(Sender: TObject);
    procedure btnSelDirClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnWorkWithFoundClick(Sender: TObject);
    procedure cbDirectoryChange(Sender: TObject);
    procedure cbFileSizeFromChange(Sender: TObject);
    procedure cbFileSizeToChange(Sender: TObject);
    procedure cbNotOlderThanChange(Sender: TObject);
    procedure cbReplaceTextChange(Sender: TObject);
    procedure cbTimeFromChange(Sender: TObject);
    procedure cbTimeToChange(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmFindDlgClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmFindDlgShow(Sender: TObject);
    procedure lbSearchTemplatesSelectionChange(Sender: TObject; User: boolean);
    procedure lsFoundedFilesDblClick(Sender: TObject);
    procedure lsFoundedFilesKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure miRemoveFromLlistClick(Sender: TObject);
    procedure miShowAllFoundClick(Sender: TObject);
    procedure miShowInViewerClick(Sender: TObject);
    procedure seFileSizeFromChange(Sender: TObject);
    procedure seFileSizeToChange(Sender: TObject);
    procedure seNotOlderThanChange(Sender: TObject);
    procedure tsLoadSaveShow(Sender: TObject);
    procedure ZVDateFromChange(Sender: TObject);
    procedure ZVDateToChange(Sender: TObject);
    procedure ZVTimeFromChange(Sender: TObject);
    procedure ZVTimeToChange(Sender: TObject);
  private
    { Private declarations }
    FFindThread:TFindThread;
    DsxPlugins: TDSXModuleList;
    FSearchingActive: Boolean;
    FFrmAttributesEdit: TfrmAttributesEdit;
    procedure StopSearch;
    procedure AfterSearchStopped;
    procedure FillFindOptions(var FindOptions: TSearchTemplateRec);
    procedure FindOptionsToDSXSearchRec(const AFindOptions: TSearchTemplateRec;
                                        var SRec: TDsxSearchRecord);
    procedure OnAddAttribute(Sender: TObject);
    procedure FoundedStringCopyChanged(Sender: TObject);
  public
    { Public declarations }
    procedure ThreadTerminate(Sender:TObject);
  end;

var
  frmFindDlg: TfrmFindDlg = nil;
  FoundedStringCopy: TStringlist = nil;

procedure ShowFindDlg(const sActPath: UTF8String);
function ShowDefineTemplateDlg(out TemplateName: UTF8String): Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, LCLType, LConvEncoding, StrUtils, HelpIntfs, fViewer, fMain,
  uLng, uGlobs, uShowForm, uOSUtils, uSearchTemplate, uDCUtils,
  uSearchResultFileSource, uFile, uFileSystemFileSource,
  uFileViewNotebook, uFileView, uColumnsFileView, uKeyboard;

const
  TimeUnitToComboIndex: array[TTimeUnit] of Integer = (0, 1, 2, 3, 4, 5, 6);
  ComboIndexToTimeUnit: array[0..6] of TTimeUnit = (tuSecond, tuMinute, tuHour, tuDay, tuWeek, tuMonth, tuYear);
  FileSizeUnitToComboIndex: array[TFileSizeUnit] of Integer = (0, 1, 2, 3, 4);
  ComboIndexToFileSizeUnit: array[0..4] of TFileSizeUnit = (suBytes, suKilo, suMega, suGiga, suTera);

procedure SAddFileProc(PlugNr: Integer; FoundFile: PChar); dcpcall;
var
  s: string;
begin
  s := string(FoundFile);
  if s='' then
    begin
      frmFindDlg.AfterSearchStopped;
    end
  else
    begin
     FoundedStringCopy.Add(s);
     Application.ProcessMessages;
    end;
end;

procedure SUpdateStatusProc(PlugNr: Integer; CurrentFile: PChar; FilesScanned: Integer); dcpcall;
var
  sCurrentFile: String;
begin
  sCurrentFile := String(CurrentFile);
  frmFindDlg.lblStatus.Caption:=Format(rsFindScanned,[FilesScanned]);
  if sCurrentFile = '' then
    frmFindDlg.lblCurrent.Caption := ''
  else
    frmFindDlg.lblCurrent.Caption:=rsFindScanning + ': ' + sCurrentFile;
  Application.ProcessMessages;
end;

procedure ShowFindDlg(const sActPath: UTF8String);
begin
  if not Assigned(frmFindDlg) then
    frmFindDlg:= TfrmFindDlg.Create(nil);

  if Assigned(frmFindDlg) then
    with frmFindDlg do
    begin
      // Prepare window for search files
      Caption := rsFindSearchFiles;
      edtFindPathStart.Enabled:= True;
      edtFindPathStart.Text := sActPath;
      btnSaveTemplate.Visible:= False;
      btnStart.Visible:= True;
      Show;
      BringToFront;
    end;
end;

function ShowDefineTemplateDlg(out TemplateName: UTF8String): Boolean;
begin
  if not Assigned(frmFindDlg) then
    frmFindDlg:= TfrmFindDlg.Create(nil);

  if Assigned(frmFindDlg) then
    with frmFindDlg do
    begin
      // Prepare window for define search template
      Caption := rsFindDefineTemplate;
      edtFindPathStart.Enabled:= False;
      edtFindPathStart.Text:= EmptyStr;
      btnSaveTemplate.Visible:= True;
      btnStart.Visible:= False;
      Result:= (ShowModal = mrOK);
      if Result and (lbSearchTemplates.Count > 0) then
      begin
        TemplateName:= lbSearchTemplates.Items[lbSearchTemplates.Count - 1];
      end;
    end;
end;

procedure TfrmFindDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FFindThread:= nil;
  FSearchingActive := False;
  FFrmAttributesEdit := nil;
  edtFindPathStart.Text:= mbGetCurrentDir;
  lblCurrent.Caption:= '';
  lblStatus.Caption:= '';
  lblFound.Caption:= '';
  Height:= pnlFindFile.Height + 22;
  DsxPlugins := TDSXModuleList.Create;
  DsxPlugins.Assign(gDSXPlugins);
  FoundedStringCopy := TStringlist.Create;
  FoundedStringCopy.OnChange:=@FoundedStringCopyChanged;

  // load language
  edtFindPathStart.DialogTitle:= rsFindWhereBeg;
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
  cbSearchDepth.Items.Add(rsFindDepthAll);
  cbSearchDepth.Items.Add(rsFindDepthCurDir);
  for I:= 1 to 100 do
    cbSearchDepth.Items.Add(Format(rsFindDepth, [IntToStr(I)]));
  cbSearchDepth.ItemIndex:= 0;
  // fill encoding combobox
  cmbEncoding.Clear;
  GetSupportedEncodings(cmbEncoding.Items);
  cmbEncoding.ItemIndex:= cmbEncoding.Items.IndexOf(EncodingAnsi);

  // gray disabled fields
  cbUsePluginChange(Sender);
  cbFindTextChange(Sender);
  cbNotOlderThanChange(Sender);
  cbFileSizeFromChange(Sender);
  cbFileSizeToChange(Sender);
  ZVDateFrom.DateTime:=Now();
  ZVDateTo.DateTime:=Now();
  ZVTimeFrom.DateTime:=Now();
  ZVTimeTo.DateTime:=Now();
  cbDateFrom.Checked:=False;
  cbDateTo.Checked:=False;
  cbTimeFrom.Checked:=False;
  cbTimeTo.Checked:=False;


{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStart.Default := True;
{$ENDIF}

  cmbNotOlderThanUnit.ItemIndex := 3; // Days
  cmbFileSizeUnit.ItemIndex := 1; // Kilobytes
  edtFindPathStart.ShowHidden := gShowSystemFiles;
  cbPartialNameSearch.Checked:= gPartialNameSearch;

  InitPropStorage(Self);
end;

procedure TfrmFindDlg.cbUsePluginChange(Sender: TObject);
begin
  EnableControl(cmbPlugin, cbUsePlugin.Checked);

  if cmbPlugin.Enabled and cmbPlugin.CanFocus and (Sender = cbUsePlugin) then
  begin
    cmbPlugin.SetFocus;
    cmbPlugin.SelectAll;
  end;
end;

procedure TfrmFindDlg.cmbEncodingSelect(Sender: TObject);
begin
  if cmbEncoding.ItemIndex <> cmbEncoding.Items.IndexOf(EncodingAnsi) then
    begin
      cbCaseSens.Tag:= Integer(cbCaseSens.Checked);
      cbCaseSens.Checked:= True;
      cbCaseSens.Enabled:= False;
    end
  else
    begin
      cbCaseSens.Checked:= Boolean(cbCaseSens.Tag);
      cbCaseSens.Enabled:= True;
    end;
end;

procedure TfrmFindDlg.cbFindTextChange(Sender: TObject);
begin
  EnableControl(cmbFindText, cbFindText.Checked);
  EnableControl(cmbReplaceText, cbFindText.Checked);
  EnableControl(cmbEncoding, cbFindText.Checked);
  EnableControl(cbCaseSens, cbFindText.Checked);
  EnableControl(cbReplaceText, cbFindText.Checked);
  EnableControl(cbNotContainingText, cbFindText.Checked);
  lblEncoding.Enabled:=cbFindText.Checked;
  cbReplaceText.Checked:= False;
  cbReplaceTextChange(Sender);

  if cmbFindText.Enabled and cmbFindText.CanFocus and (Sender = cbFindText)then
  begin
    cmbFindText.SetFocus;
    cmbFindText.SelectAll;
  end;
end;

procedure TfrmFindDlg.btnSearchLoadClick(Sender: TObject);
var
  SearchTemplate: TSearchTemplate;
begin
  if lbSearchTemplates.ItemIndex < 0 then Exit;
  SearchTemplate:= gSearchTemplateList.Templates[lbSearchTemplates.ItemIndex];
  with SearchTemplate.SearchRecord do
  begin
    cmbFindFileMask.Text:= FilesMasks;
    if (StartPath <> '') then
      edtFindPathStart.Text:= StartPath;
    if (SearchDepth + 1 >= 0) and (SearchDepth + 1 < cbSearchDepth.Items.Count) then
      cbSearchDepth.ItemIndex:= SearchDepth + 1
    else
      cbSearchDepth.ItemIndex:= 0;
    cbRegExp.Checked := RegExp;
    cbPartialNameSearch.Checked := IsPartialNameSearch;
    // attributes
    edtAttrib.Text:= AttributesPattern;
    // file date/time
    cbDateFrom.Checked:= IsDateFrom;
    cbDateTo.Checked:= IsDateTo;
    ZVDateFrom.Date:= DateTimeFrom;
    ZVDateTo.Date:= DateTimeTo;
    cbTimeFrom.Checked:= IsTimeFrom;
    cbTimeTo.Checked:= IsTimeTo;
    ZVTimeFrom.Time:= DateTimeFrom;
    ZVTimeTo.Time:= DateTimeTo;
    // not older then
    cbNotOlderThan.Checked:= IsNotOlderThan;
    seNotOlderThan.Value:= NotOlderThan;
    cmbNotOlderThanUnit.ItemIndex := TimeUnitToComboIndex[NotOlderThanUnit];
    // file size
    cbFileSizeFrom.Checked:= IsFileSizeFrom;
    cbFileSizeTo.Checked:= IsFileSizeTo;
    seFileSizeFrom.Value:= FileSizeFrom;
    seFileSizeTo.Value:= FileSizeTo;
    cmbFileSizeUnit.ItemIndex := FileSizeUnitToComboIndex[FileSizeUnit];
    // find/replace text
    cbFindText.Checked:= IsFindText;
    cmbFindText.Text:= FindText;
    cbReplaceText.Checked:= IsReplaceText;
    cmbReplaceText.Text:= ReplaceText;
    cbCaseSens.Checked:= CaseSensitive;
    cbNotContainingText.Checked:= NotContainingText;
    cmbEncoding.Text:= TextEncoding;
    cmbPlugin.Text:= SearchPlugin;
  end;
end;

procedure TfrmFindDlg.btnSearchDeleteClick(Sender: TObject);
begin
  if lbSearchTemplates.ItemIndex < 0 then Exit;
  gSearchTemplateList.DeleteTemplate(lbSearchTemplates.ItemIndex);
  tsLoadSaveShow(nil);
end;

procedure TfrmFindDlg.btnAttrsHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', edtAttrib.HelpKeyword);
end;

procedure TfrmFindDlg.btnAddAttributeClick(Sender: TObject);
begin
  if not Assigned(FFrmAttributesEdit) then
  begin
    FFrmAttributesEdit := TfrmAttributesEdit.Create(Self);
    FFrmAttributesEdit.OnOk := @OnAddAttribute;
  end;
  FFrmAttributesEdit.Reset;
  FFrmAttributesEdit.Show;
end;

procedure TfrmFindDlg.btnSearchSaveClick(Sender: TObject);
var
  sName: UTF8String;
  SearchTemplate: TSearchTemplate;
begin
  if not InputQuery(rsFindSaveTemplateCaption, rsFindSaveTemplateTitle, sName) then
  begin
    ModalResult:= mrCancel;
    Exit;
  end;

  SearchTemplate := gSearchTemplateList.TemplateByName[sName];
  if Assigned(SearchTemplate) then
  begin
    // TODO: Ask for overwriting existing template.
    FillFindOptions(SearchTemplate.SearchRecord);
    Exit;
  end;

  SearchTemplate:= TSearchTemplate.Create;
  SearchTemplate.TemplateName:= sName;
  FillFindOptions(SearchTemplate.SearchRecord);
  gSearchTemplateList.Add(SearchTemplate);
  tsLoadSaveShow(nil);
end;

procedure TfrmFindDlg.cbDateFromChange(Sender: TObject);
begin
  UpdateColor(ZVDateFrom, cbDateFrom.Checked);
end;

procedure TfrmFindDlg.cbDateToChange(Sender: TObject);
begin
  UpdateColor(ZVDateTo, cbDateTo.Checked);
end;

procedure TfrmFindDlg.cbPartialNameSearchChange(Sender: TObject);
begin
  if cbPartialNameSearch.Checked then cbRegExp.Checked:=False;
end;

procedure TfrmFindDlg.cbRegExpChange(Sender: TObject);
begin
  if cbRegExp.Checked then cbPartialNameSearch.Checked:=False;
end;

procedure TfrmFindDlg.btnSelDirClick(Sender: TObject);
var
  s:String;
begin
  s:=edtFindPathStart.Text;
  if not mbDirectoryExists(s) then s:='';
  SelectDirectory(rsFindWhereBeg,'',s, False);
  edtFindPathStart.Text:=s;
end;

procedure TfrmFindDlg.btnNewSearchClick(Sender: TObject);
begin
  StopSearch;
  pgcSearch.PageIndex:= 0;
  lsFoundedFiles.Clear;
  FoundedStringCopy.Clear;
  miShowAllFound.Enabled:=False;
  lblStatus.Caption:= EmptyStr;
  lblCurrent.Caption:= EmptyStr;
  lblFound.Caption:= EmptyStr;
  if pgcSearch.ActivePage = tsStandard then
    cmbFindFileMask.SetFocus;
end;

procedure TfrmFindDlg.btnGoToPathClick(Sender: TObject);
begin
  if lsFoundedFiles.ItemIndex <> -1 then
  begin
    frmMain.ActiveFrame.CurrentPath := ExtractFilePath(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
    frmMain.ActiveFrame.SetActiveFile(ExtractFileName(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]));
    Close;
  end;
end;

procedure TfrmFindDlg.FillFindOptions(var FindOptions: TSearchTemplateRec);
begin
  with FindOptions do
  begin
    StartPath      := edtFindPathStart.Text;
    FilesMasks     := cmbFindFileMask.Text;
    SearchDepth    := cbSearchDepth.ItemIndex - 1;
    RegExp         := cbRegExp.Checked;
    IsPartialNameSearch := cbPartialNameSearch.Checked;
    FollowSymLinks := cmbFollowSymLinks.Checked;

    { File attributes }
    AttributesPattern := edtAttrib.Text;

    { Date/time }
    DateTimeFrom := 0;
    DateTimeTo   := 0;
    IsDateFrom   := False;
    IsDateTo     := False;
    IsTimeFrom   := False;
    IsTimeTo     := False;
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
    IsNotOlderThan   := cbNotOlderThan.Checked;
    NotOlderThan     := seNotOlderThan.Value;
    NotOlderThanUnit := ComboIndexToTimeUnit[cmbNotOlderThanUnit.ItemIndex];

    { File size }
    IsFileSizeFrom := cbFileSizeFrom.Checked;
    IsFileSizeTo   := cbFileSizeTo.Checked;
    FileSizeFrom   := seFileSizeFrom.Value;
    FileSizeTo     := seFileSizeTo.Value;
    FileSizeUnit   := ComboIndexToFileSizeUnit[cmbFileSizeUnit.ItemIndex];

    { Find/replace text }
    IsFindText        := cbFindText.Checked;
    FindText          := cmbFindText.Text;
    IsReplaceText     := cbReplaceText.Checked;
    ReplaceText       := cmbReplaceText.Text;
    CaseSensitive     := cbCaseSens.Checked;
    NotContainingText := cbNotContainingText.Checked;
    TextEncoding      := cmbEncoding.Text;
    SearchPlugin      := cmbPlugin.Text;
  end;
end;

procedure TfrmFindDlg.FindOptionsToDSXSearchRec(
  const AFindOptions: TSearchTemplateRec;
  var SRec: TDsxSearchRecord);
begin
  with AFindOptions do
  begin
    FillByte(SRec, SizeOf(TDsxSearchRecord), 0);

    SRec.StartPath:= Copy(StartPath, 1, SizeOf(SRec.StartPath));

    if IsPartialNameSearch then
      SRec.FileMask:= '*' + Copy(FilesMasks, 1, SizeOf(SRec.FileMask) - 2) + '*'
    else
      SRec.FileMask:= Copy(FilesMasks, 1, SizeOf(SRec.FileMask));

    SRec.Attributes:= faAnyFile;  // AttrStrToFileAttr?
    SRec.AttribStr:= Copy(AttributesPattern, 1, SizeOf(SRec.AttribStr));

    SRec.CaseSensitive:=CaseSensitive;
    {Date search}
    SRec.IsDateFrom:=IsDateFrom;
    SRec.IsDateTo:=IsDateTo;
    SRec.DateTimeFrom:=DateTimeFrom;
    SRec.DateTimeTo:=DateTimeTo;
    {Time search}
    SRec.IsTimeFrom:=IsTimeFrom;
    SRec.IsTimeTo:=IsTimeTo;
    (* File size search *)
    SRec.IsFileSizeFrom:=IsFileSizeFrom;
    SRec.IsFileSizeTo:=IsFileSizeTo;
    SRec.FileSizeFrom:=FileSizeFrom;
    SRec.FileSizeTo:=FileSizeTo;
    (* Find text *)
    SRec.NotContainingText:=NotContainingText;
    SRec.IsFindText:=IsFindText;
    SRec.FindText:= Copy(FindText, 1, SizeOf(SRec.FindText));
    (* Replace text *)
    SRec.IsReplaceText:=IsReplaceText;
    SRec.ReplaceText:= Copy(ReplaceText, 1, SizeOf(SRec.ReplaceText));
  end;
end;

procedure TfrmFindDlg.StopSearch;
begin
  if FSearchingActive then
  begin
    if (cbUsePlugin.Checked) and (cmbPlugin.ItemIndex<>-1) then
      begin
        DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallStopSearch;
        DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallFinalize;
        AfterSearchStopped;
      end;

    if Assigned(FFindThread) then
    begin
      FFindThread.Terminate;
      FFindThread := nil;
    end;
  end;
end;

procedure TfrmFindDlg.AfterSearchStopped;
begin
  btnStop.Enabled:= False;
  btnStart.Enabled:= True;
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStart.Default:= True;
{$ENDIF}
  btnClose.Enabled:= True;
  btnNewSearch.Enabled:= True;
  FSearchingActive := False;
end;

procedure TfrmFindDlg.btnStartClick(Sender: TObject);
var
  sTemp,
  sPath : UTF8String;
  sr: TDsxSearchRecord;
  FindOptions: TSearchTemplateRec;
begin
  sTemp:= edtFindPathStart.Text;
  repeat
    sPath:= Copy2SymbDel(sTemp, ';');
    if not mbDirectoryExists(sPath) then
      begin
        ShowMessage(Format(rsFindDirNoEx,[sPath]));
        Exit;
      end;
  until sTemp = EmptyStr;
  // add to find mask history
  InsertFirstItem(cmbFindFileMask.Text, cmbFindFileMask);
  // add to search text history
  if cbFindText.Checked then
    begin
      InsertFirstItem(cmbFindText.Text, cmbFindText);
      glsSearchHistory.Insert(0, cmbFindText.Text);
    end;
  // add to replace text history
  if cbReplaceText.Checked then
    InsertFirstItem(cmbReplaceText.Text, cmbReplaceText);

  // Show search results page
  pgcSearch.ActivePageIndex:= pgcSearch.PageCount - 1;

  if lsFoundedFiles.CanFocus then
    lsFoundedFiles.SetFocus;

  lsFoundedFiles.Items.Clear;
  FoundedStringCopy.Clear;
  miShowAllFound.Enabled:=False;

  FSearchingActive := True;
  btnStop.Enabled:=True;
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStop.Default:=True;
{$ENDIF}
  btnStart.Enabled:= False;
  btnClose.Enabled:= False;
  btnNewSearch.Enabled:= False;

  FillFindOptions(FindOptions);
  try
    if (cbUsePlugin.Checked) and (cmbPlugin.ItemIndex<>-1) then
      begin
        if DSXPlugins.LoadModule(cmbPlugin.ItemIndex) then
        begin
          FindOptionsToDSXSearchRec(FindOptions, sr);
          DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallInit(@SAddFileProc,@SUpdateStatusProc);
          DSXPlugins.GetDSXModule(cmbPlugin.ItemIndex).CallStartSearch(sr);
        end
        else
          StopSearch;
      end
    else
      begin
        FFindThread := TFindThread.Create(FindOptions);
        with FFindThread do
        begin
          Items := FoundedStringCopy;
          Status := lblStatus;
          Current := lblCurrent;
          Found := lblFound;
          OnTerminate := @ThreadTerminate; // will update the buttons after search is finished
        end;
        FFindThread.Resume;
      end;
  except
    StopSearch;
    raise;
  end;
end;
procedure TfrmFindDlg.FoundedStringCopyChanged(Sender: TObject);
begin
  if FoundedStringCopy.Count > 0 then
    lsFoundedFiles.Items.Add(FoundedStringCopy[FoundedStringCopy.Count - 1]);
end;

procedure TfrmFindDlg.btnViewClick(Sender: TObject);
begin
  if lsFoundedFiles.ItemIndex <> -1 then
    ShowViewerByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
end;

procedure TfrmFindDlg.btnWorkWithFoundClick(Sender: TObject);
var
  I: Integer;
  sFileName: String;
  SearchResultFS: ISearchResultFileSource;
  FileList: TFileTree;
  aFile: TFile;
  Notebook: TFileViewNotebook;
  NewPage: TFileViewPage;
  FileView: TFileView;
begin
  FileList := TFileTree.Create;
  for i := 0 to lsFoundedFiles.Items.Count - 1 do
  begin
    sFileName:= lsFoundedFiles.Items[I];
    aFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
    FileList.AddSubNode(aFile);
  end;

  // Create search result file source.
  // Currently only searching FileSystem is supported.
  SearchResultFS := TSearchResultFileSource.Create;
  SearchResultFS.AddList(FileList, TFileSystemFileSource.GetFileSource);

  // Add new tab for search results.
  Notebook := frmMain.ActiveNotebook;
  if tb_open_new_near_current in gDirTabOptions then
    NewPage := Notebook.InsertPage(Notebook.PageIndex + 1)
  else
    NewPage := Notebook.AddPage;

  // Hard-coded Columns file view for now (later user will be able to change default view).
  FileView := TColumnsFileView.Create(NewPage, SearchResultFS, SearchResultFS.GetRootDir);
  frmMain.AssignEvents(FileView);
  NewPage.UpdateCaption(rsSearchResult);
  NewPage.MakeActive;

  Close;
end;

procedure TfrmFindDlg.cbDirectoryChange(Sender: TObject);
begin
end;

procedure TfrmFindDlg.cbFileSizeFromChange(Sender: TObject);
begin
  UpdateColor(seFileSizeFrom, cbFileSizeFrom.Checked);
  EnableControl(cmbFileSizeUnit,cbFileSizeFrom.Checked or cbFileSizeTo.Checked);
end;

procedure TfrmFindDlg.cbFileSizeToChange(Sender: TObject);
begin
  UpdateColor(seFileSizeTo, cbFileSizeTo.Checked);
  EnableControl(cmbFileSizeUnit,cbFileSizeFrom.Checked or cbFileSizeTo.Checked);
end;

procedure TfrmFindDlg.cbNotOlderThanChange(Sender: TObject);
begin
   UpdateColor(seNotOlderThan, cbNotOlderThan.Checked);
   EnableControl(cmbNotOlderThanUnit,cbNotOlderThan.Checked);
end;

procedure TfrmFindDlg.cbReplaceTextChange(Sender: TObject);
begin
  EnableControl(cmbReplaceText, cbReplaceText.Checked and cbFindText.Checked);
  cbNotContainingText.Checked := False;
  cbNotContainingText.Enabled := (not cbReplaceText.Checked and cbFindText.Checked);

  if cmbReplaceText.Enabled and cmbReplaceText.CanFocus then
  begin
    cmbReplaceText.SetFocus;
    cmbReplaceText.SelectAll;
  end;
end;

procedure TfrmFindDlg.cbTimeFromChange(Sender: TObject);
begin
  UpdateColor(ZVTimeFrom, cbTimeFrom.Checked);
end;

procedure TfrmFindDlg.cbTimeToChange(Sender: TObject);
begin
  UpdateColor(ZVTimeTo, cbTimeTo.Checked);
end;

procedure TfrmFindDlg.ThreadTerminate(Sender:TObject);
begin
  FFindThread:= nil;
  AfterSearchStopped;
end;

procedure TfrmFindDlg.btnStopClick(Sender: TObject);
begin
  StopSearch;
end;

procedure TfrmFindDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:= not Assigned(FFindThread);
end;

procedure TfrmFindDlg.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFindDlg.FormDestroy(Sender: TObject);
begin
  FreeThenNil(FoundedStringCopy);
  FreeThenNil(DsxPlugins);
end;

procedure TfrmFindDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
    // On LCLGTK2 default button on Enter does not work.
    VK_RETURN, VK_SELECT:
      begin
        Key := 0;
        if btnStart.Enabled then
          btnStart.Click
        else
          btnStop.Click;
      end;
{$ENDIF}
    VK_ESCAPE:
      begin
        Key := 0;
        if FSearchingActive then
          StopSearch
        else
          Close;
      end;
    VK_1..VK_4:
      begin
        if Shift * KeyModifiersShortcut = [ssAlt] then
          begin
            pgcSearch.PageIndex := Key - VK_1;
            Key := 0;
          end;
      end;
    VK_TAB:
      begin
        if Shift * KeyModifiersShortcut = [ssCtrl] then
        begin
          pgcSearch.SelectNextPage(True);
          Key := 0;
        end
        else if Shift * KeyModifiersShortcut = [ssCtrl, ssShift] then
        begin
          pgcSearch.SelectNextPage(False);
          Key := 0;
        end;
      end;
  end;
end;

procedure TfrmFindDlg.frmFindDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  glsMaskHistory.Assign(cmbFindFileMask.Items);

  if Assigned(FFrmAttributesEdit) then
  begin
    FFrmAttributesEdit.Close;
    FreeAndNil(FFrmAttributesEdit);
  end;
end;

procedure TfrmFindDlg.frmFindDlgShow(Sender: TObject);
var
  I: Integer;
begin
  pgcSearch.PageIndex:= 0;

  if cmbFindFileMask.Visible then
    cmbFindFileMask.SelectAll;

  cmbFindFileMask.Items.Assign(glsMaskHistory);
  cmbFindText.Items.Assign(glsSearchHistory);
  // if we already search text then use last searched text
  if not gFirstTextSearch then
    begin
      if glsSearchHistory.Count > 0 then
        cmbFindText.Text:= glsSearchHistory[0];
    end;
  cmbReplaceText.Items.Assign(glsReplaceHistory);

  cbFindText.Checked := False;

  cmbPlugin.Clear;
  for I:= 0 to DSXPlugins.Count-1 do
    begin
      cmbPlugin.AddItem(DSXPlugins.GetDSXModule(i).Name+' (' + DSXPlugins.GetDSXModule(I).Descr+' )',nil);
    end;
  if (cmbPlugin.Items.Count>0) then cmbPlugin.ItemIndex:=0;

  if pgcSearch.ActivePage = tsStandard then
    if cmbFindFileMask.CanFocus then
      cmbFindFileMask.SetFocus;
end;

procedure TfrmFindDlg.lbSearchTemplatesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbSearchTemplates.ItemIndex < 0 then Exit;
  with gSearchTemplateList.Templates[lbSearchTemplates.ItemIndex].SearchRecord do
    lblSearchContents.Caption := '"' + FilesMasks + '" in "' + StartPath + '"';
end;

procedure TfrmFindDlg.lsFoundedFilesDblClick(Sender: TObject);
begin
  miShowInViewer.Click;
end;

procedure TfrmFindDlg.lsFoundedFilesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and (lsFoundedFiles.ItemIndex <> -1) then
  begin
    case Key of
      VK_F3:
      begin
        ShowViewerByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
        Key := 0;
      end;

      VK_F4:
      begin
        ShowEditorByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
        Key := 0;
      end;

      VK_DELETE:
      begin
        miRemoveFromLlistClick(Sender);
        Key := 0;
      end;
    end;
  end;
end;

procedure TfrmFindDlg.miRemoveFromLlistClick(Sender: TObject);
var
  i:Integer;
begin
  if lsFoundedFiles.ItemIndex=-1 then Exit;
  if lsFoundedFiles.SelCount = 0 then Exit;

  for i:=lsFoundedFiles.Items.Count-1 downto 0 do
    if lsFoundedFiles.Selected[i] then
      lsFoundedFiles.Items.Delete(i);

  miShowAllFound.Enabled:=True;
end;

procedure TfrmFindDlg.miShowAllFoundClick(Sender: TObject);
begin
  lsFoundedFiles.Clear;
  lsFoundedFiles.Items.AddStrings(FoundedStringCopy);

  miShowAllFound.Enabled:=False;
end;

procedure TfrmFindDlg.miShowInViewerClick(Sender: TObject);
var
  sl:TStringList;
  i:Integer;
begin
  if lsFoundedFiles.ItemIndex=-1 then Exit;

  sl:=TStringList.Create;
  try
    for i:=0 to lsFoundedFiles.Items.Count-1 do
      if lsFoundedFiles.Selected[i] then
        sl.Add(lsFoundedFiles.Items[i]);
    ShowViewer(sl);
  finally
    sl.Free;
  end;
end;

procedure TfrmFindDlg.seFileSizeFromChange(Sender: TObject);
begin
  cbFileSizeFrom.Checked:= (seFileSizeFrom.Value > 0);
end;

procedure TfrmFindDlg.seFileSizeToChange(Sender: TObject);
begin
  cbFileSizeTo.Checked:= (seFileSizeTo.Value > 0);
end;

procedure TfrmFindDlg.seNotOlderThanChange(Sender: TObject);
begin
  cbNotOlderThan.Checked:= (seNotOlderThan.Value > 0);
end;

procedure TfrmFindDlg.tsLoadSaveShow(Sender: TObject);
begin
  gSearchTemplateList.LoadToStringList(lbSearchTemplates.Items);
  lblSearchContents.Caption:= '';
end;

procedure TfrmFindDlg.ZVDateFromChange(Sender: TObject);
begin
    cbDateFrom.Checked:= True;
end;

procedure TfrmFindDlg.ZVDateToChange(Sender: TObject);
begin
  cbDateTo.Checked:= True;
end;

procedure TfrmFindDlg.ZVTimeFromChange(Sender: TObject);
begin
  cbTimeFrom.Checked:= True;
end;

procedure TfrmFindDlg.ZVTimeToChange(Sender: TObject);
begin
    cbTimeTo.Checked:= True;
end;

procedure TfrmFindDlg.OnAddAttribute(Sender: TObject);
var
  sAttr: String;
begin
  sAttr := edtAttrib.Text;
  if edtAttrib.SelStart > 0 then
    // Insert at caret position.
    Insert((Sender as TfrmAttributesEdit).AttrsAsText, sAttr, edtAttrib.SelStart + 1)
  else
    sAttr := sAttr + (Sender as TfrmAttributesEdit).AttrsAsText;
  edtAttrib.Text := sAttr;
end;

finalization
  if Assigned(frmFindDlg) then
    FreeAndNil(frmFindDlg);
end.

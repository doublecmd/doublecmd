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

interface

uses
  LResources,
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, EditBtn, Spin, MaskEdit,
  uDsxModule, DsxPlugin, uFindThread, uFindFiles;

type

  { TfrmFindDlg }

  TfrmFindDlg = class(TForm)
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnClose: TButton;
    btnGoToPath: TButton;
    btnNewSearch: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnView: TButton;
    btnWorkWithFound: TButton;
    btnAttrsHelp: TButton;
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
    cmbNotOlderThanUnit: TComboBox;
    cmbFileSizeUnit: TComboBox;
    cbUsePlugin: TCheckBox;
    cmbPlugin: TComboBox;
    cmbEncoding: TComboBox;
    cbSearchDepth: TComboBox;
    cbRegExp: TCheckBox;
    cmbReplaceText: TComboBox;
    cmbFindText: TComboBox;
    deDateFrom: TEditButton;
    deDateTo: TEditButton;
    edtFindPathStart: TDirectoryEdit;
    edtAttrib: TEdit;
    edtTimeFrom: TEdit;
    edtTimeTo: TEdit;
    gbAttributes: TGroupBox;
    btnSearchDelete: TButton;
    btnSearchLoad: TButton;
    lblCurrent: TLabel;
    lblFound: TLabel;
    lblStatus: TLabel;
    lblTemplateHeader: TLabel;
    lbSearchTemplates: TListBox;
    btnSearchSave: TButton;
    lblSearchContents: TPanel;
    lblSearchDepth: TLabel;
    lblEncoding: TLabel;
    lsFoundedFiles: TListBox;
    pnlResults: TPanel;
    pnlStatus: TPanel;
    pnlResultsButtons: TPanel;
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
    procedure btnAttrsHelpClick(Sender: TObject);
    procedure btnSearchDeleteClick(Sender: TObject);
    procedure btnSearchLoadClick(Sender: TObject);
    procedure btnSearchSaveClick(Sender: TObject);
    procedure cmbEncodingSelect(Sender: TObject);
    procedure cbFindTextChange(Sender: TObject);
    procedure cbUsePluginChange(Sender: TObject);
    procedure deDateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGoToPathClick(Sender: TObject);
    procedure btnNewSearchClick(Sender: TObject);
    procedure btnSelDirClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnWorkWithFoundClick(Sender: TObject);
    procedure cbDateFromChange(Sender: TObject);
    procedure cbDateToChange(Sender: TObject);
    procedure cbDirectoryChange(Sender: TObject);
    procedure cbFileSizeFromChange(Sender: TObject);
    procedure cbFileSizeToChange(Sender: TObject);
    procedure cbNotOlderThanChange(Sender: TObject);
    procedure cbReplaceTextChange(Sender: TObject);
    procedure cbSymLinkChange(Sender: TObject);
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
    procedure meTimeChange(Sender: TObject);
    procedure miShowInViewerClick(Sender: TObject);
    procedure tsLoadSaveShow(Sender: TObject);
  private
    { Private declarations }
    FFindThread:TFindThread;
    DsxPlugins: TDSXModuleList;
    FSearchingActive: Boolean;
    procedure StopSearch;
    procedure AfterSearchStopped;
    procedure FillFindOptions(var FindOptions: TSearchTemplateRec);
    procedure FindOptionsToDSXSearchRec(const AFindOptions: TSearchTemplateRec;
                                        var SRec: TDsxSearchRecord);
  public
    { Public declarations }
    procedure ThreadTerminate(Sender:TObject);
  end;

var
  frmFindDlg: TfrmFindDlg =nil;

procedure ShowFindDlg(const sActPath:String);

implementation

uses
  LCLProc, LCLType, LConvEncoding, StrUtils, HelpIntfs, fCalendar, fViewer, fMain,
  uLng, uGlobs, uShowForm, uOSUtils, uSearchTemplate, uDCUtils;

const
  TimeUnitToComboIndex: array[TTimeUnit] of Integer = (0, 1, 2, 3, 4, 5, 6);
  ComboIndexToTimeUnit: array[0..6] of TTimeUnit = (tuSecond, tuMinute, tuHour, tuDay, tuWeek, tuMonth, tuYear);
  FileSizeUnitToComboIndex: array[TFileSizeUnit] of Integer = (0, 1, 2, 3, 4);
  ComboIndexToFileSizeUnit: array[0..4] of TFileSizeUnit = (suBytes, suKilo, suMega, suGiga, suTera);

procedure SAddFileProc(PlugNr: Integer; FoundFile: PChar); stdcall;
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
     frmFindDlg.lsFoundedFiles.Items.Add(s);
     Application.ProcessMessages;
    end;
end;

procedure SUpdateStatusProc(PlugNr: Integer; CurrentFile: PChar; FilesScanned: Integer); stdcall;
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

procedure ShowFindDlg(const sActPath:String);
begin
  if not assigned (frmFindDlg) then
    frmFindDlg:=TfrmFindDlg.Create(nil);

  if Assigned(frmFindDlg) then
    with frmFindDlg do
    begin
      edtFindPathStart.Text := sActPath;
      Show;
      BringToFront;

      if pgcSearch.ActivePage = tsStandard then
        if cmbFindFileMask.CanFocus then
          cmbFindFileMask.SetFocus;
    end;
end;

procedure TfrmFindDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // load language
  edtFindPathStart.DialogTitle:= rsFindWhereBeg;
  FFindThread:= nil;
  FSearchingActive := False;
  edtFindPathStart.Text:= mbGetCurrentDir;
  lblCurrent.Caption:= '';
  lblStatus.Caption:= '';
  lblFound.Caption:= '';
  Height:= pnlFindFile.Height + 22;
  DsxPlugins := TDSXModuleList.Create;
  DsxPlugins.Assign(gDSXPlugins);
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

{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStart.Default := True;
{$ENDIF}

  cmbNotOlderThanUnit.ItemIndex:= 3;
  edtFindPathStart.ShowHidden := gShowSystemFiles;
end;

procedure TfrmFindDlg.cbUsePluginChange(Sender: TObject);
begin
  cmbPlugin.Enabled:=cbUsePlugin.Checked;

  if cmbPlugin.Enabled and cmbPlugin.CanFocus then
  begin
    cmbPlugin.SetFocus;
    cmbPlugin.SelectAll;
  end;
end;

procedure TfrmFindDlg.deDateButtonClick(Sender: TObject);
var
  ebDate: TEditButton absolute Sender;
begin
  ebDate.Text:= ShowCalendarDialog(ebDate.Text, Mouse.CursorPos);
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
  gbFindData.Enabled:=cbFindText.Checked;

  if cmbFindText.Enabled and cmbFindText.CanFocus then
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
    edtFindPathStart.Text:= StartPath;
    if (SearchDepth + 1 >= 0) and (SearchDepth + 1 < cbSearchDepth.Items.Count) then
      cbSearchDepth.ItemIndex:= SearchDepth + 1
    else
      cbSearchDepth.ItemIndex:= 0;
    cbRegExp.Checked := RegExp;
    // attributes
    edtAttrib.Text:= AttributesPattern;
    // file date/time
    cbDateFrom.Checked:= IsDateFrom;
    cbDateTo.Checked:= IsDateTo;
    deDateFrom.Text:= DateToStr(DateTimeFrom);
    deDateTo.Text:= DateToStr(DateTimeTo);
    cbTimeFrom.Checked:= IsTimeFrom;
    cbTimeTo.Checked:= IsTimeTo;
    edtTimeFrom.Text:= TimeToStr(DateTimeFrom);
    edtTimeTo.Text:= TimeToStr(DateTimeTo);
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

procedure TfrmFindDlg.btnSearchSaveClick(Sender: TObject);
var
  sName: UTF8String;
  SearchTemplate: TSearchTemplate;
begin
  if not InputQuery(rsFindSaveTemplateCaption, rsFindSaveTemplateTitle, sName) then Exit;

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
var
  dtTemp: TDateTime;
begin
  with FindOptions do
  begin
    StartPath   := edtFindPathStart.Text;
    FilesMasks  := cmbFindFileMask.Text;
    SearchDepth := cbSearchDepth.ItemIndex - 1;
    RegExp      := cbRegExp.Checked;

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
        if TryStrToDate(deDateFrom.Text, dtTemp) then
          begin
            IsDateFrom := True;
            DateTimeFrom := dtTemp;
          end;
      end;
    if cbDateTo.Checked then
      begin
        if TryStrToDate(deDateTo.Text, dtTemp) then
          begin
            IsDateTo := True;
            DateTimeTo := dtTemp;
          end;
      end;
    if cbTimeFrom.Checked then
      begin
        if TryStrToTime(edtTimeFrom.Text, dtTemp) then
          begin
            IsTimeFrom := True;
            DateTimeFrom := DateTimeFrom + dtTemp;
          end;
      end;
    if cbTimeTo.Checked then
      begin
        if TryStrToTime(edtTimeTo.Text, dtTemp) then
          begin
            IsTimeTo := True;
            DateTimeTo := DateTimeTo + dtTemp;
          end;
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
  btnStop.Enabled:=False;
  btnStart.Enabled:=True;
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStart.Default:=True;
{$ENDIF}
  btnClose.Enabled:=True;
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
    InsertFirstItem(cmbFindText.Text, cmbFindText);
  // add to replace text history
  if cbReplaceText.Checked then
    InsertFirstItem(cmbReplaceText.Text, cmbReplaceText);

  // Show search results page
  pgcSearch.ActivePageIndex:= pgcSearch.PageCount - 1;

  if lsFoundedFiles.CanFocus then
    lsFoundedFiles.SetFocus;

  lsFoundedFiles.Items.Clear;

  FSearchingActive := True;
  btnStop.Enabled:=True;
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStop.Default:=True;
{$ENDIF}
  btnStart.Enabled:=False;
  btnClose.Enabled:=False;

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
          Items := lsFoundedFiles.Items;
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

procedure TfrmFindDlg.btnViewClick(Sender: TObject);
begin
  if lsFoundedFiles.ItemIndex <> -1 then
    ShowViewerByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
end;

(* Not working full now *)

procedure TfrmFindDlg.btnWorkWithFoundClick(Sender: TObject);
var
  I, Count: Integer;
  sFileName: String;
begin
  // This should create a new file source (virtual).
(*
  frmMain.ActiveFrame.CurrentPath := '';

  Count:= lsFoundedFiles.Items.Count - 1;
//  frmMain.ActiveFrame.pnlFile.FileList.Clear;
  with FileRecItem do
  for I:= 0 to Count do
    begin
      sFileName:= lsFoundedFiles.Items[I];
      FileRecItem:= LoadFilebyName(sFileName);
      sNameNoExt:= sFileName;
      sName:= sFileName;
{
      frmMain.ActiveFrame.pnlFile.FileList.AddItem(@FileRecItem);
}
    end;
{
  frmMain.ActiveFrame.pnlFile.FileList.UpdateFileInformation(pmDirectory);
  frmMain.ActiveFrame.pnlFile.Sort;
}
*)
  Close;
end;

procedure TfrmFindDlg.cbDateFromChange(Sender: TObject);
begin
  deDateFrom.Enabled := cbDateFrom.Checked;
end;

procedure TfrmFindDlg.cbDateToChange(Sender: TObject);
begin
  deDateTo.Enabled := cbDateTo.Checked;
end;

procedure TfrmFindDlg.cbDirectoryChange(Sender: TObject);
begin
end;

procedure TfrmFindDlg.cbFileSizeFromChange(Sender: TObject);
begin
  seFileSizeFrom.Enabled := cbFileSizeFrom.Checked;

  if seFileSizeFrom.Enabled or seFileSizeTo.Enabled then
    cmbFileSizeUnit.Enabled := True
  else
    cmbFileSizeUnit.Enabled := False;
end;

procedure TfrmFindDlg.cbFileSizeToChange(Sender: TObject);
begin
  seFileSizeTo.Enabled := cbFileSizeTo.Checked;

  if seFileSizeFrom.Enabled or seFileSizeTo.Enabled then
    cmbFileSizeUnit.Enabled := True
  else
    cmbFileSizeUnit.Enabled := False;
end;

procedure TfrmFindDlg.cbNotOlderThanChange(Sender: TObject);
begin
  seNotOlderThan.Enabled := cbNotOlderThan.Checked;
  cmbNotOlderThanUnit.Enabled := cbNotOlderThan.Checked;
end;

procedure TfrmFindDlg.cbReplaceTextChange(Sender: TObject);
begin
  cmbReplaceText.Enabled := cbReplaceText.Checked;
  cbNotContainingText.Checked := False;
  cbNotContainingText.Enabled := not cbReplaceText.Checked;

  if cmbReplaceText.Enabled and cmbReplaceText.CanFocus then
  begin
    cmbReplaceText.SetFocus;
    cmbReplaceText.SelectAll;
  end;
end;

procedure TfrmFindDlg.cbSymLinkChange(Sender: TObject);
begin

end;

procedure TfrmFindDlg.cbTimeFromChange(Sender: TObject);
var
  sTime : String;
begin
  edtTimeFrom.Enabled := cbTimeFrom.Checked;
  DateTimeToString(sTime, 'hh:mm:ss', Time);
  edtTimeFrom.Text := sTime;
end;

procedure TfrmFindDlg.cbTimeToChange(Sender: TObject);
var
  sTime : String;
begin
  edtTimeTo.Enabled := cbTimeTo.Checked;
  DateTimeToString(sTime, 'hh:mm:ss', Time);
  edtTimeTo.Text := sTime;
end;

procedure TfrmFindDlg.ThreadTerminate(Sender:TObject);
begin
  FFindThread:=nil;
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
  FreeThenNil(DsxPlugins);
end;

procedure TfrmFindDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
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
        if Shift = [ssAlt] then
          begin
            pgcSearch.PageIndex := Key - VK_1;
            Key := 0;
          end;
      end;
  end;
end;

procedure TfrmFindDlg.frmFindDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  glsMaskHistory.Assign(cmbFindFileMask.Items);
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

  cmbPlugin.Clear;
  for I:= 0 to DSXPlugins.Count-1 do
    begin
      cmbPlugin.AddItem(DSXPlugins.GetDSXModule(i).Name+' (' + DSXPlugins.GetDSXModule(I).Descr+' )',nil);
    end;
  if (cmbPlugin.Items.Count>0) then cmbPlugin.ItemIndex:=0;
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
  if lsFoundedFiles.ItemIndex <> -1 then
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
    end;
  end;
end;

procedure TfrmFindDlg.meTimeChange(Sender: TObject);
var
  ME : TMaskEdit;
begin
  ME := TMaskEdit(Sender);

  if StrToIntDef(Copy(ME.EditText, 1, 2), 24) > 23 then
    ME.EditText := '00' + Copy(ME.EditText, 3, 6);

  if StrToIntDef(Copy(ME.EditText, 4, 2), 60) > 59 then
    ME.EditText := Copy(ME.EditText, 1, 3) + '00' + Copy(ME.EditText, 6, 3);

  if StrToIntDef(Copy(ME.EditText, 7, 2), 60) > 59 then
    ME.EditText := Copy(ME.EditText, 1, 6) + '00';

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

procedure TfrmFindDlg.tsLoadSaveShow(Sender: TObject);
begin
  gSearchTemplateList.LoadToStringList(lbSearchTemplates.Items);
  lblSearchContents.Caption:= '';
end;

initialization
 {$I fFindDlg.lrs}
finalization
  if Assigned(frmFindDlg) then
    FreeAndNil(frmFindDlg);
end.
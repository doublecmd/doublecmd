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
  SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons, uFindThread, Menus,
  EditBtn, Spin, MaskEdit, udsxmodule, DsxPlugin;

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
    cbFindInFile: TCheckBox;
    cbNoThisText: TCheckBox;
    cbDateFrom: TCheckBox;
    cbNotOlderThan: TCheckBox;
    cbFileSizeFrom: TCheckBox;
    cbDateTo: TCheckBox;
    cbFileSizeTo: TCheckBox;
    cbReplaceText: TCheckBox;
    cbTimeFrom: TCheckBox;
    cbTimeTo: TCheckBox;
    cbDelayUnit: TComboBox;
    cbUnitOfMeasure: TComboBox;
    cbDirectory: TCheckBox;
    cbSymLink: TCheckBox;
    cbMore: TCheckBox;
    cbAttrib: TCheckBox;
    cbUsePlugin: TCheckBox;
    cbbSPlugins: TComboBox;
    cbEncoding: TComboBox;
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
    lblInfo: TLabel;
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
    procedure btnSearchDeleteClick(Sender: TObject);
    procedure btnSearchLoadClick(Sender: TObject);
    procedure btnSearchSaveClick(Sender: TObject);
    procedure cbEncodingSelect(Sender: TObject);
    procedure cbFindInFileChange(Sender: TObject);
    procedure cbUsePluginChange(Sender: TObject);
    procedure deDateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGoToPathClick(Sender: TObject);
    procedure btnNewSearchClick(Sender: TObject);
    procedure btnSelDirClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnWorkWithFoundClick(Sender: TObject);
    procedure cbAttribChange(Sender: TObject);
    procedure cbDateFromChange(Sender: TObject);
    procedure cbDateToChange(Sender: TObject);
    procedure cbDirectoryChange(Sender: TObject);
    procedure cbFileSizeFromChange(Sender: TObject);
    procedure cbFileSizeToChange(Sender: TObject);
    procedure cbMoreChange(Sender: TObject);
    procedure cbNotOlderThanChange(Sender: TObject);
    procedure cbReplaceTextChange(Sender: TObject);
    procedure cbSymLinkChange(Sender: TObject);
    procedure cbTimeFromChange(Sender: TObject);
    procedure cbTimeToChange(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure frmFindDlgClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmFindDlgShow(Sender: TObject);
    procedure lbSearchTemplatesSelectionChange(Sender: TObject; User: boolean);
    procedure lsFoundedFilesDblClick(Sender: TObject);
    procedure lsFoundedFilesKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure meTimeChange(Sender: TObject);
    procedure miShowInViewerClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure tsLoadSaveShow(Sender: TObject);
  private
    { Private declarations }
    FFindThread:TFindThread;
    DsxPlugins: TDSXModuleList;
    procedure PrepareSearch;
  public
    { Public declarations }
    procedure ThreadTerminate(Sender:TObject);
  end;

const
  cKilo = 1024;
  cMega = 1024*1024;
  cGiga = 1024*1024*1024;

var
  frmFindDlg: TfrmFindDlg =nil;

procedure ShowFindDlg(const sActPath:String);

implementation

uses
  LCLProc, LCLType, LConvEncoding, DateUtils, fCalendar, fViewer, uLng, uGlobs, uShowForm, fMain,
  uTypes, uOSUtils, uSearchTemplate, uDCUtils;

procedure SAddFileProc(PlugNr:integer; FoundFile:pchar); stdcall;
var s:string;
begin
s:=string(FoundFile);
  if s='' then
    begin
      frmFindDlg.ThreadTerminate(nil);
    end
  else
    begin
     frmFindDlg.lsFoundedFiles.Items.Add(s);
     DebugLn('fFindLlg: '+S);
     Application.ProcessMessages;
    end;

end;

procedure SUpdateStatusProc(PlugNr:integer; CurrentFile:pchar; FilesScanned:integer); stdcall;
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
  cbEncoding.Clear;
  GetSupportedEncodings(cbEncoding.Items);
  cbEncoding.ItemIndex:= cbEncoding.Items.IndexOf(EncodingAnsi);

{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStart.Default := True;
{$ENDIF}

  cbDelayUnit.ItemIndex:= 2;
  edtFindPathStart.ShowHidden := gShowSystemFiles;
end;

procedure TfrmFindDlg.cbUsePluginChange(Sender: TObject);
begin
  cbbSPlugins.Enabled:=cbUsePlugin.Checked;

  if cbbSPlugins.Enabled and cbbSPlugins.CanFocus then
  begin
    cbbSPlugins.SetFocus;
    cbbSPlugins.SelectAll;
  end;
end;

procedure TfrmFindDlg.deDateButtonClick(Sender: TObject);
var
  ebDate: TEditButton absolute Sender;
begin
  ebDate.Text:= ShowCalendarDialog(ebDate.Text, Mouse.CursorPos);
end;

procedure TfrmFindDlg.cbEncodingSelect(Sender: TObject);
begin
  if cbEncoding.ItemIndex <> cbEncoding.Items.IndexOf(EncodingAnsi) then
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

procedure TfrmFindDlg.cbFindInFileChange(Sender: TObject);
begin
  gbFindData.Enabled:=cbFindInFile.Checked;

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
    cmbFindFileMask.Text:= rFileMask;
    edtFindPathStart.Text:= SearchTemplate.StartPath;
    // attributes
    cbAttrib.Checked:= False;
    cbDirectory.State:= cbGrayed;
    cbSymLink.State:= cbGrayed;
    cbMore.Checked:= False;
    edtAttrib.Text:= '';
    if rAttributes <> faAnyFile then
      begin
        cbAttrib.Checked:= True;
        cbDirectory.Checked:= FPS_ISDIR(rAttributes);
        cbSymLink.Checked:= FPS_ISLNK(rAttributes);
      end;
    if (rAttribStr <> '') and (rAttribStr <> '?????????') then
      begin
        cbAttrib.Checked:= True;
        cbMore.Checked:= True;
        edtAttrib.Text:= rAttribStr;
      end;
    // file date
    cbDateFrom.Checked:= rIsDateFrom;
    deDateFrom.Text:= '';
    deDateTo.Text:= '';
    if rIsDateFrom then
      deDateFrom.Text:= DateToStr(rDateTimeFrom);
    if rIsDateTo then
      deDateTo.Text:= DateToStr(rDateTimeTo);
    // file time
    cbTimeFrom.Checked:= rIsTimeFrom;
    cbTimeTo.Checked:= rIsTimeTo;
    edtTimeFrom.Text:= '';
    edtTimeTo.Text:= '';
    if rIsTimeFrom then
      edtTimeFrom.Text:= TimeToStr(rDateTimeFrom);
    if rIsTimeTo then
      edtTimeTo.Text:= TimeToStr(rDateTimeTo);
    // not older then
    cbNotOlderThan.Checked:= SearchTemplate.IsNotOlderThan;
    if SearchTemplate.IsNotOlderThan then
      begin
        seNotOlderThan.Value:= Trunc(SearchTemplate.NotOlderThan);
        cbDelayUnit.ItemIndex:= Round(Frac(SearchTemplate.NotOlderThan)*10);
      end;
    // file size
    cbFileSizeFrom.Checked:= rIsFileSizeFrom;
    cbFileSizeTo.Checked:= rIsFileSizeTo;
    if rIsFileSizeFrom or rIsFileSizeTo then
      begin
        if (rFileSizeFrom >= cGiga) or (rFileSizeTo >= cGiga) then
          begin
            cbUnitOfMeasure.ItemIndex:= 3;
            seFileSizeFrom.Value:= rFileSizeFrom div cGiga;
            seFileSizeTo.Value:= rFileSizeTo div cGiga;
          end
        else if (rFileSizeFrom >= cMega) or (rFileSizeTo >= cMega) then
          begin
            cbUnitOfMeasure.ItemIndex:= 2;
            seFileSizeFrom.Value:= rFileSizeFrom div cMega;
            seFileSizeTo.Value:= rFileSizeTo div cMega;
          end
        else if (rFileSizeFrom >= cKilo) or (rFileSizeTo >= cKilo) then
          begin
            cbUnitOfMeasure.ItemIndex:= 1;
            seFileSizeFrom.Value:= rFileSizeFrom div cKilo;
            seFileSizeTo.Value:= rFileSizeTo div cKilo;
          end
        else
          begin
            cbUnitOfMeasure.ItemIndex:= 0;
            seFileSizeFrom.Value:= rFileSizeFrom;
            seFileSizeTo.Value:= rFileSizeTo;
          end;
      end;
    if not rIsFileSizeFrom then
      seFileSizeFrom.Text:= '';
    if not rIsFileSizeTo then
      seFileSizeTo.Text:= '';
    // find text
    cbNoThisText.Checked:= rIsNoThisText;
    cbFindInFile.Checked:= rFindInFiles;
    cbCaseSens.Checked:= rCaseSens;
    cmbFindText.Text:= '';
    if rFindInFiles then
      cmbFindText.Text:= rFindData;
    // replace text
    cbReplaceText.Checked:= rReplaceInFiles;
    cmbReplaceText.Text:= '';
    if rReplaceInFiles then
      cmbReplaceText.Text:= rReplaceData;
  end;
end;

procedure TfrmFindDlg.btnSearchDeleteClick(Sender: TObject);
begin
  if lbSearchTemplates.ItemIndex < 0 then Exit;
  gSearchTemplateList.DeleteTemplate(lbSearchTemplates.ItemIndex);
  tsLoadSaveShow(nil);
end;

procedure TfrmFindDlg.btnSearchSaveClick(Sender: TObject);
var
  sName: UTF8String;
  SearchTemplate: TSearchTemplate;
begin
  if not InputQuery(rsFindSaveTemplateCaption, rsFindSaveTemplateTitle, sName) then Exit;
  SearchTemplate:= TSearchTemplate.Create;
  SearchTemplate.TemplateName:= sName;
  SearchTemplate.StartPath:= edtFindPathStart.Text;
  SearchTemplate.IsNotOlderThan:= cbNotOlderThan.Checked;
  SearchTemplate.NotOlderThan:= seNotOlderThan.Value + cbDelayUnit.ItemIndex/10;
  PrepareSearch;
  FFindThread.FillSearchRecord(SearchTemplate.SearchRecord);
  if SearchTemplate.IsNotOlderThan then
    begin
      SearchTemplate.SearchRecord.rIsDateFrom:= False;
      SearchTemplate.SearchRecord.rIsTimeFrom:= False;
    end;
  gSearchTemplateList.Add(SearchTemplate);
  FreeAndNil(FFindThread);
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
  btnStopClick(Sender);
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

procedure TfrmFindDlg.PrepareSearch;
var
  dtTemp, dtNow: TDateTime;
  iCount: Integer;
begin
  FFindThread:=TFindThread.Create;
  if Assigned(FFindThread) then
  try
    with FFindThread do
    begin
      FilterMask:= cmbFindFileMask.Text;
      PathStart:= edtFindPathStart.Text;
      Items:= lsFoundedFiles.Items;
      RegularExpressions:= cbRegExp.Checked;
      SearchDepth:= cbSearchDepth.ItemIndex - 1;
      IsNoThisText:= cbNoThisText.Checked;
      FindInFiles:= cbFindInFile.Checked;
      FindData:= ConvertEncoding(cmbFindText.Text, EncodingUTF8, cbEncoding.Text);
      CaseSensitive:= cbCaseSens.Checked;
      ReplaceInFiles:= cbReplaceText.Checked;
      ReplaceData:= cmbReplaceText.Text;
      (* Date search *)
      if cbDateFrom.Checked then
         begin
           IsDateFrom := True;
           if TryStrToDate(deDateFrom.Text, dtTemp) then
             DateTimeFrom := dtTemp;
         end;
      if cbDateTo.Checked then
         begin
           IsDateTo := True;
           if TryStrToDate(deDateTo.Text, dtTemp) then
             DateTimeTo := dtTemp;
         end;
      (* Time search *)
      if cbTimeFrom.Checked then
         begin
           IsTimeFrom := True;
           dtTemp := 0;
           if TryStrToTime(edtTimeFrom.Text, dtTemp) then
             DateTimeFrom := DateTimeFrom + dtTemp;
         end;

      if cbTimeTo.Checked then
         begin
           IsTimeTo := True;
           dtTemp := 0;
           if TryStrToTime(edtTimeTo.Text, dtTemp) then
             DateTimeTo := DateTimeTo +  dtTemp;
         end;
      (* Not Older Than *)
       if cbNotOlderThan.Checked then
         begin
           dtNow:= Now;
           iCount:= -StrToInt(seNotOlderThan.Text);
           case cbDelayUnit.ItemIndex of
             0:  //Minute(s)
               begin
                 IsDateFrom := True;
                 IsTimeFrom := True;
                 DateTimeFrom := IncMinute(dtNow, iCount);
               end;
             1:  //Hour(s)
               begin
                 IsDateFrom := True;
                 IsTimeFrom := True;
                 DateTimeFrom := IncHour(dtNow, iCount);
               end;
             2:  //Day(s)
               begin
                 IsDateFrom := True;
                 DateTimeFrom := IncDay(dtNow, iCount);
               end;
             3:  //Week(s)
               begin
                 IsDateFrom := True;
                 DateTimeFrom := IncWeek(dtNow, iCount);
               end;
             4:  //Month(s)
               begin
                 IsDateFrom := True;
                 DateTimeFrom := IncMonth(dtNow, iCount);
               end;
             5:  //Year(s)
               begin
                 IsDateFrom := True;
                 DateTimeFrom := IncYear(dtNow, iCount);
               end;
           end;
         end;

      (* File size search *)
       if cbFileSizeFrom.Checked then
         begin
           IsFileSizeFrom := True;
           case cbUnitOfMeasure.ItemIndex of
             0:
               FileSizeFrom := seFileSizeFrom.Value;   //Byte
             1:
               FileSizeFrom := Int64(seFileSizeFrom.Value) * cKilo; //KiloByte
             2:
               FileSizeFrom := Int64(seFileSizeFrom.Value) * cMega; //MegaByte
             3:
               FileSizeFrom := Int64(seFileSizeFrom.Value) * cGiga; //GigaByte
           end;
         end;
      if cbFileSizeTo.Checked then
         begin
           IsFileSizeTo := True;
           case cbUnitOfMeasure.ItemIndex of
             0:
               FileSizeTo := seFileSizeTo.Value;   //Byte
             1:
               FileSizeTo := Int64(seFileSizeTo.Value) * cKilo; //KiloByte
             2:
               FileSizeTo := Int64(seFileSizeTo.Value) * cMega; //MegaByte
             3:
               FileSizeTo := Int64(seFileSizeTo.Value) * cGiga; //GigaByte
           end;
         end;
      (* File attributes *)
      if cbAttrib.Checked then
        begin
          Attributes := 0;

          if cbDirectory.Checked then
            Attributes := Attributes or faDirectory;

          DebugLn('Attributes == ' + IntToStr(Attributes));

          if cbSymLink.Checked then
            Attributes := Attributes or uOSUtils.faSymLink;

          if Attributes = 0 then
            Attributes := faAnyFile;

          if cbMore.Checked then
            AttribStr := edtAttrib.Text;

        end;
    end;
  except
    FreeAndNil(FFindThread);
  end;
end;

procedure TfrmFindDlg.btnStartClick(Sender: TObject);
var
  sr:TSearchAttrRecord;
begin
  if not mbDirectoryExists(edtFindPathStart.Text) then
  begin
    ShowMessage(Format(rsFindDirNoEx,[edtFindPathStart.Text]));
    Exit;
  end;

  // add to find mask history
  InsertFirstItem(cmbFindFileMask.Text, cmbFindFileMask);
  // add to search text history
  if cbFindInFile.Checked then
    InsertFirstItem(cmbFindText.Text, cmbFindText);
  // add to replace text history
  if cbReplaceText.Checked then
    InsertFirstItem(cmbReplaceText.Text, cmbReplaceText);

  // Show search results page
  pgcSearch.ActivePageIndex:= pgcSearch.PageCount - 1;

  if lsFoundedFiles.CanFocus then
    lsFoundedFiles.SetFocus;

  lsFoundedFiles.Items.Clear;
  btnStop.Enabled:=True;
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStop.Default:=True;
{$ENDIF}
  btnStart.Enabled:=False;
  btnClose.Enabled:=False;

  PrepareSearch;
  if Assigned(FFindThread) then
  try
    with FFindThread do
    begin
      Status:=lblStatus;
      Current:=lblCurrent;
      Found:=lblFound;

       //---------------------
       if (cbUsePlugin.Checked) and (cbbSPlugins.ItemIndex<>-1) then
         begin
           DSXPlugins.LoadModule(cbbSPlugins.ItemIndex);
           FillSearchRecord(sr);
           FreeAndNil(FFindThread);

           DSXPlugins.GetDSXModule(cbbSPlugins.ItemIndex).CallInit(@SAddFileProc,@SUpdateStatusProc);
           DSXPlugins.GetDSXModule(cbbSPlugins.ItemIndex).CallStartSearch(PChar(edtFindPathStart.Text),sr);
         end
      else
        begin
          OnTerminate:=@ThreadTerminate; // will update the buttons after search is finished
          Resume;
        end;
    end;
  except
    if Assigned(FFindThread) then
      FreeAndNil(FFindThread);
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
  FileRecItem: TFileRecItem;
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

procedure TfrmFindDlg.cbAttribChange(Sender: TObject);
begin
  gbAttributes.Enabled := cbAttrib.Checked;
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
    cbUnitOfMeasure.Enabled := True
  else
    cbUnitOfMeasure.Enabled := False;
end;

procedure TfrmFindDlg.cbFileSizeToChange(Sender: TObject);
begin
  seFileSizeTo.Enabled := cbFileSizeTo.Checked;

  if seFileSizeFrom.Enabled or seFileSizeTo.Enabled then
    cbUnitOfMeasure.Enabled := True
  else
    cbUnitOfMeasure.Enabled := False;
end;

procedure TfrmFindDlg.cbMoreChange(Sender: TObject);
begin
  edtAttrib.Enabled := cbMore.Checked;
end;

procedure TfrmFindDlg.cbNotOlderThanChange(Sender: TObject);
begin
  seNotOlderThan.Enabled := cbNotOlderThan.Checked;
  cbDelayUnit.Enabled := cbNotOlderThan.Checked;
end;

procedure TfrmFindDlg.cbReplaceTextChange(Sender: TObject);
begin
  cmbReplaceText.Enabled := cbReplaceText.Checked;
  cbNoThisText.Checked := False;
  cbNoThisText.Enabled := not cbReplaceText.Checked;

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
  btnStop.Enabled:=False;
  btnStart.Enabled:=True;
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  btnStart.Default:=True;
{$ENDIF}
  btnClose.Enabled:=True;
  FFindThread:=nil;
end;

procedure TfrmFindDlg.btnStopClick(Sender: TObject);
begin
  if (cbUsePlugin.Checked) and (cbbSPlugins.ItemIndex<>-1) then
    begin
      DSXPlugins.GetDSXModule(cbbSPlugins.ItemIndex).CallStopSearch;
      DSXPlugins.GetDSXModule(cbbSPlugins.ItemIndex).CallFinalize;
      ThreadTerminate(nil);
    end;
    
  if Assigned(FFindThread) then
  begin
    FFindThread.Terminate;
  end;
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

  cbbSPlugins.Clear;
  for I:= 0 to DSXPlugins.Count-1 do
    begin
      cbbSPlugins.AddItem(DSXPlugins.GetDSXModule(i).Name+' (' + DSXPlugins.GetDSXModule(I).Descr+' )',nil);
    end;
  if (cbbSPlugins.Items.Count>0) then cbbSPlugins.ItemIndex:=0;
end;

procedure TfrmFindDlg.lbSearchTemplatesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbSearchTemplates.ItemIndex < 0 then Exit;
  with gSearchTemplateList.Templates[lbSearchTemplates.ItemIndex] do
    lblSearchContents.Caption:= '"'+SearchRecord.rFileMask+'" in "'+StartPath+'"';
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

procedure TfrmFindDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if key=#13 then
  begin
    Key := #0;
    if btnStart.Enabled then
      btnStart.Click
    else
      btnStop.Click;
  end else
{$ENDIF}
  if key=#27 then
  begin
    Key:=#0;
    Close;
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

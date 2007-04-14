{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Find dialog, with searching in thread

contributors:
Copyright (C) 2006-2007 Alexander Koblov (Alexx2000@mail.ru)

}
{ $threading on}
unit fFindDlg;
{$mode objfpc}{$H+}
{$DEFINE NOFAKETHREAD}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons, uFindThread, Menus,
  fLngForm, Calendar, EditBtn, Spin, MaskEdit;

type

  { TfrmFindDlg }

  TfrmFindDlg = class(TfrmLng)
    btnClose: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnView: TButton;
    btnNewSearch: TButton;
    btnGoToPath: TButton;
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
    deDateFrom: TDateEdit;
    deDateTo: TDateEdit;
    edtReplaceText: TEdit;
    meTimeFrom: TMaskEdit;
    meTimeTo: TMaskEdit;
    Panel4: TPanel;
    seNotOlderThan: TSpinEdit;
    seFileSizeFrom: TSpinEdit;
    seFileSizeTo: TSpinEdit;
    Splitter1: TSplitter;
    Panel2: TPanel;
    pgcSearch: TPageControl;
    tsStandard: TTabSheet;
    lblFindPathStart: TLabel;
    edtFindPathStart: TEdit;
    btnSelDir: TButton;
    lblFindFileMask: TLabel;
    cmbFindFileMask: TComboBox;
    gbFindData: TGroupBox;
    cbCaseSens: TCheckBox;
    edtFindText: TEdit;
    tsAdvanced: TTabSheet;
    Panel1: TPanel;
    Panel3: TPanel;
    lsFoundedFiles: TListBox;
    lblStatus: TLabel;
    lblCurrent: TLabel;
    PopupMenuFind: TPopupMenu;
    miShowInViewer: TMenuItem;
    procedure btnGoToPathClick(Sender: TObject);
    procedure btnNewSearchClick(Sender: TObject);
    procedure btnSelDirClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnWorkWithFoundClick(Sender: TObject);
    procedure cbDateFromChange(Sender: TObject);
    procedure cbDateToChange(Sender: TObject);
    procedure cbFileSizeFromChange(Sender: TObject);
    procedure cbFileSizeToChange(Sender: TObject);
    procedure cbNotOlderThanChange(Sender: TObject);
    procedure cbReplaceTextChange(Sender: TObject);
    procedure cbTimeFromChange(Sender: TObject);
    procedure cbTimeToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure cbFindInFileClick(Sender: TObject);
    procedure frmFindDlgClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmFindDlgShow(Sender: TObject);
    procedure lsFoundedFilesDblClick(Sender: TObject);
    procedure meTimeChange(Sender: TObject);
    procedure miShowInViewerClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FFindThread:TFindThread;
  public
    { Public declarations }
    procedure ThreadTerminate(Sender:TObject);
    procedure LoadLng; override;
  end;

var
  frmFindDlg: TfrmFindDlg =nil;

procedure ShowFindDlg(const sActPath:String);

implementation

uses
  fViewer, uLng, uShowForm, fMain, uTypes, uFileOp;
  
procedure ShowFindDlg(const sActPath:String);
begin
  if not assigned (frmFindDlg) then
    frmFindDlg:=TfrmFindDlg.Create(nil);
  frmFindDlg.Show;
  frmFindDlg.BringToFront;
  frmFindDlg.cmbFindFileMask.SetFocus;

end;

procedure TfrmFindDlg.LoadLng;
begin
// load language

  Caption:=lngGetString(clngFindFile);
  tsStandard.Caption:=  lngGetString(clngFindStandard);
  tsAdvanced.Caption:=  lngGetString(clngFindAdvanced);
  lblFindPathStart.Caption:= lngGetString(clngFindFileDir);
  lblFindFileMask.Caption:= lngGetString (clngFindFileMask);
  cbFindInFile.Caption:= lngGetString(clngFindFndInFl);
  gbFindData.Caption:= lngGetString(clngFindData);
  cbCaseSens.Caption:= lngGetString(clngFindCase);
  miShowInViewer.Caption:=lngGetString(clngFindShowView);

end;


procedure TfrmFindDlg.btnSelDirClick(Sender: TObject);
var
  s:String;
begin
  s:=edtFindPathStart.Text;
  if not DirectoryExists(s) then s:='';
  SelectDirectory(lngGetString(clngFindWhereBeg),'',s, False);
  edtFindPathStart.Text:=s;
end;

procedure TfrmFindDlg.btnNewSearchClick(Sender: TObject);
begin
  Panel1.Visible := False;
  Splitter1.Visible := False;
  Height := Panel2.Height;
end;

procedure TfrmFindDlg.btnGoToPathClick(Sender: TObject);
begin
  frmMain.ActiveFrame.pnlFile.ActiveDir := ExtractFilePath(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
  frmMain.ActiveFrame.pnlFile.LoadPanel;
  frmMain.ActiveFrame.edtSearch.Text := ExtractFileName(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
  Close;
end;

procedure TfrmFindDlg.btnStartClick(Sender: TObject);
var
  dtTime : TDateTime;
begin
  if not DirectoryExists(edtFindPathStart.Text) then
  begin
    ShowMessage(Format(lngGetString(clngFindDirNoEx),[edtFindPathStart.Text]));
    Exit;
  end;
  
  Panel1.Visible := True;
  Splitter1.Visible := True;
  Height := (Screen.Height * 4) div 5;
  
  lsFoundedFiles.Items.Clear;
  btnStop.Enabled:=True;
  btnStart.Enabled:=False;
  btnClose.Enabled:=False;
  FFindThread:=TFindThread.Create;
  with FFindThread do
  begin
    FilterMask:=cmbFindFileMask.Text;
    PathStart:=edtFindPathStart.Text;
    Items:=lsFoundedFiles.Items;
    IsNoThisText := cbNoThisText.Checked;
    FindInFiles:=cbFindInFile.Checked;
    FindData:=edtFindText.Text;
    CaseSensitive:=cbCaseSens.Checked;
    ReplaceInFiles := cbReplaceText.Checked;
    ReplaceData := edtReplaceText.Text;
    (* Date search *)
    if cbDateFrom.Checked then
       begin
         IsDateFrom := True;
         DateTimeFrom := deDateFrom.Date;
       end;
    if cbDateTo.Checked then
       begin
         IsDateTo := True;
         DateTimeTo := deDateTo.Date;
       end;
    (* Time search *)
    if cbTimeFrom.Checked then
       begin
         IsTimeFrom := True;
         dtTime := 0;
         if TryStrToTime(meTimeFrom.Text, dtTime) then
           DateTimeFrom := DateTimeFrom + dtTime;
       end;
       
    if cbTimeTo.Checked then
       begin
         IsTimeTo := True;
         dtTime := 0;
         if TryStrToTime(meTimeTo.Text, dtTime) then
           DateTimeTo := DateTimeTo +  dtTime;
       end;
    (* Not Older Than *)
     if cbNotOlderThan.Checked then
       begin
         case cbDelayUnit.ItemIndex of
           0:  //Minute(s)
             begin
               IsTimeFrom := True;
               IsDateFrom := True;
               DateTimeFrom := Now -  0.0006945 * StrToInt(seNotOlderThan.Text);
             end;
           1:  //Hour(s)
             begin
               IsTimeFrom := True;
               IsDateFrom := True;
               DateTimeFrom := Now -  0.0416667 * StrToInt(seNotOlderThan.Text);
             end;
           2:  //Day(s)
             begin
               IsDateFrom := True;
               DateTimeFrom := Now - 1 * StrToInt(seNotOlderThan.Text);
             end;
           3:  //Week(s)
             begin
               IsDateFrom := True;
               DateTimeFrom := Now - 7 * StrToInt(seNotOlderThan.Text);
             end;
           4:  //Month(s)
             begin
               IsDateFrom := True;
               DateTimeFrom := Now - 31 * StrToInt(seNotOlderThan.Text);
             end;
           5:  //Year(s)
             begin
               IsDateFrom := True;
               DateTimeFrom := Now - 365 * StrToInt(seNotOlderThan.Text);
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
             FileSizeFrom := seFileSizeFrom.Value * 1024; //KiloByte
           2:
             FileSizeFrom := seFileSizeFrom.Value * 1048576; //MegaByte
           3:
             FileSizeFrom := seFileSizeFrom.Value * 1073741824; //GigaByte
         end;
       end;
    if cbFileSizeTo.Checked then
       begin
         IsFileSizeTo := True;
         case cbUnitOfMeasure.ItemIndex of
           0:
             FileSizeTo := seFileSizeTo.Value;   //Byte
           1:
             FileSizeTo := seFileSizeTo.Value * 1024; //KiloByte
           2:
             FileSizeTo := seFileSizeTo.Value * 1048576; //MegaByte
           3:
             FileSizeTo := seFileSizeTo.Value * 1073741824; //GigaByte
         end;
       end;

    Status:=lblStatus;
    Current:=lblCurrent;
    writeln('thread a');
{$IFDEF NOFAKETHREAD}
    FreeOnTerminate:=False;
    OnTerminate:=@ThreadTerminate; // napojime udalost na obsluhu tlacitka
    writeln('thread a1');
    Resume;
  end;
{$ELSE}
    Resume;
    //WaitFor;      //remove
  end;
  //ThreadTerminate(self); //remove if thread is Ok
{$ENDIF}
    writeln('thread a2');

end;

procedure TfrmFindDlg.btnViewClick(Sender: TObject);
begin
  ShowViewerByGlob(lsFoundedFiles.Items[lsFoundedFiles.ItemIndex]);
end;

(* Not working full now *)

procedure TfrmFindDlg.btnWorkWithFoundClick(Sender: TObject);
var
  I, Count : Integer;
  fr:TFileRecItem;
  sr : TSearchRec;
begin
  Count := lsFoundedFiles.Items.Count - 1;
  frmMain.ActiveFrame.pnlFile.FileList.Clear;
  for I := 0 to Count do
    begin
      fr.sNameNoExt := lsFoundedFiles.Items[I];
      fr.sName := fr.sNameNoExt;
      FindFirst(fr.sNameNoExt, faAnyFile, sr);
      fr.sExt := ExtractFileExt(fr.sNameNoExt);
      fr.iSize := sr.Size;
      fr.sTime := DateTimeToStr(Trunc(FileDateToDateTime(sr.Time)));
      fr.iMode := sr.Attr;
      fr.sModeStr := AttrToStr(sr.Attr);
      fr.bLinkIsDir:=False;
      fr.bSelected:=False;
      frmMain.ActiveFrame.pnlFile.FileList.AddItem(@fr);
    end;
  frmMain.ActiveFrame.pnlFile.FileList.UpdateFileInformation;
  frmMain.ActiveFrame.pnlFile.Sort;
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

procedure TfrmFindDlg.cbFileSizeFromChange(Sender: TObject);
begin
  seFileSizeFrom.Enabled := cbFileSizeFrom.Checked;
end;

procedure TfrmFindDlg.cbFileSizeToChange(Sender: TObject);
begin
  seFileSizeTo.Enabled := cbFileSizeTo.Checked;
end;

procedure TfrmFindDlg.cbNotOlderThanChange(Sender: TObject);
begin
  seNotOlderThan.Enabled := cbNotOlderThan.Checked;
  cbDelayUnit.Enabled := cbNotOlderThan.Checked;
end;

procedure TfrmFindDlg.cbReplaceTextChange(Sender: TObject);
begin
  edtReplaceText.Enabled := cbReplaceText.Checked;
  cbNoThisText.Checked := False;
  cbNoThisText.Enabled := not cbReplaceText.Checked;
end;

procedure TfrmFindDlg.cbTimeFromChange(Sender: TObject);
var
  sTime : String;
begin
  meTimeFrom.Enabled := cbTimeFrom.Checked;
  DateTimeToString(sTime, 'hh:mm:ss', Time);
  meTimeFrom.EditText := sTime;
end;

procedure TfrmFindDlg.cbTimeToChange(Sender: TObject);
var
  sTime : String;
begin
  meTimeTo.Enabled := cbTimeTo.Checked;
  DateTimeToString(sTime, 'hh:mm:ss', Time);
  meTimeTo.EditText := sTime;
end;

procedure TfrmFindDlg.ThreadTerminate(Sender:TObject);
begin
  writeln('thread terminate end');
{  FFindThread.Terminate;
  FFindThread.WaitFor;}
  btnStop.Enabled:=False;
  btnStart.Enabled:=True;
  btnClose.Enabled:=True;  
  FFindThread:=nil;
end;

procedure TfrmFindDlg.FormCreate(Sender: TObject);
{ar
  s:String;}
begin
  inherited;
  FFindThread:=nil;
  edtFindPathStart.Text:=GetCurrentDir;
  lblCurrent.Caption:='';
  lblStatus.Caption:='';
  Panel1.Visible := False;
  Splitter1.Visible := False;
  Height := Panel2.Height;
end;

procedure TfrmFindDlg.btnStopClick(Sender: TObject);
begin
  if not assigned(FFindThread) then Exit;
  FFindThread.Terminate;
//  FFindThread.WaitFor;
//  FFindThread:=nil;
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

procedure TfrmFindDlg.cbFindInFileClick(Sender: TObject);
begin
  gbFindData.Enabled:=cbFindInFile.Checked;
end;

procedure TfrmFindDlg.frmFindDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
//  CloseAction:=caFree;
  Panel1.Visible := False;
  Height := Panel2.Height;
end;

procedure TfrmFindDlg.frmFindDlgShow(Sender: TObject);
begin
  cmbFindFileMask.SelectAll;
  //cmbFindFileMask.SetFocus;
end;

procedure TfrmFindDlg.lsFoundedFilesDblClick(Sender: TObject);
begin
  miShowInViewer.Click;
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
  if key=#13 then
  begin
    if btnStart.Enabled then
      btnStart.Click
    else
      btnStop.Click;
  end;
  if key=#27 then
  begin
    Key:=#0;
    Close;
  end;
end;

initialization
 {$I fFindDlg.lrs}
finalization
  if assigned(frmFindDlg) then
    FreeAndNil(frmFindDlg);
end.

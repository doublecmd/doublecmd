{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Find dialog, with searching in thread

contributors:


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
  fLngForm, Calendar, EditBtn, Spin;

type

  { TfrmFindDlg }

  TfrmFindDlg = class(TfrmLng)
    btnClose: TButton;
    btnStart: TButton;
    btnStop: TButton;
    cbFindInFile: TCheckBox;
    cbNoThisText: TCheckBox;
    cbDateFrom: TCheckBox;
    cbNotOlderThan: TCheckBox;
    cbFileSizeFrom: TCheckBox;
    cbDateTo: TCheckBox;
    cbFileSizeTo: TCheckBox;
    ComboBox1: TComboBox;
    deDateFrom: TDateEdit;
    deDateTo: TDateEdit;
    SpinEdit1: TSpinEdit;
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
    procedure btnSelDirClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure cbDateFromChange(Sender: TObject);
    procedure cbDateToChange(Sender: TObject);
    procedure cbFileSizeFromChange(Sender: TObject);
    procedure cbFileSizeToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure cbFindInFileClick(Sender: TObject);
    procedure frmFindDlgClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmFindDlgShow(Sender: TObject);
    procedure lsFoundedFilesDblClick(Sender: TObject);
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
  fViewer, uLng;
  
procedure ShowFindDlg(const sActPath:String);
begin
  if not assigned (frmFindDlg) then
    frmFindDlg:=TfrmFindDlg.Create(nil);
  frmFindDlg.Show;
  frmFindDlg.BringToFront;
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

procedure TfrmFindDlg.btnStartClick(Sender: TObject);
begin
  if not DirectoryExists(edtFindPathStart.Text) then
  begin
    ShowMessage(Format(lngGetString(clngFindDirNoEx),[edtFindPathStart.Text]));
    Exit;
  end;
  
  //Panel1.Visible := True;
  //Height := Panel2.Height + Panel1.Height;
  
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
    FindInFiles:=cbFindInFile.Checked;
    FindData:=edtFindText.Text;
    CaseSensitive:=cbCaseSens.Checked;
    (* Date search *)
    if cbDateFrom.Checked then
       begin
         IsDateFrom := True;
         DateFrom := deDateFrom.Date;
       end;
    if cbDateTo.Checked then
       begin
         IsDateTo := True;
         DateTo := deDateTo.Date;
       end;
    (* File size search *)
     if cbFileSizeFrom.Checked then
       begin
         IsFileSizeFrom := True;
         FileSizeFrom := seFileSizeFrom.Value;
       end;
    if cbFileSizeTo.Checked then
       begin
         IsFileSizeTo := True;
         FileSizeTo := seFileSizeTo.Value;
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
  //Panel1.Visible := False;
  //Height := Panel2.Height + 4;
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

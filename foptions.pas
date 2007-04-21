{
Seksi Commander
----------------------------
Implementing of Options dialog

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

}
unit fOptions;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, fLngForm, Spin, ColorBox;

type

  { TfrmOptions }

  TfrmOptions = class(TfrmLng)
    btnSelMainFnt: TButton;
    btnSelEditFnt: TButton;
    btnSelViewFnt: TButton;
    Button1: TButton;
    Button2: TButton;
    btSetHotKey: TButton;
    cbSeparateExt: TCheckBox;
    cbActions: TComboBox;
    cTextColor: TColorBox;
    cbackgrndcolor: TColorBox;
    edHotKey: TEdit;
    lblActions: TLabel;
    lblHotKey: TLabel;
    optColorDialog: TColorDialog;
    dlgFnt: TFontDialog;
    cTextLabel: TLabel;
    cBackGrndLabel: TLabel;
    PageControl1: TPageControl;
    edtMainSize: TSpinEdit;
    edtEditorSize: TSpinEdit;
    edtViewerSize: TSpinEdit;
    tsColor: TTabSheet;
    tfHotKey: TTabSheet;
    tsLng: TTabSheet;
    tsBehav: TTabSheet;
    Panel1: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lngList: TListBox;
    lblTerm: TLabel;
    edtTerm: TEdit;
    gb: TGroupBox;
    cbDirSelect: TCheckBox;
    cbCaseSensitiveSort: TCheckBox;
    cbLynxLike: TCheckBox;
    cbShortFileSizeFormat: TCheckBox;
    tsTools: TTabSheet;
    cbExtEditor: TCheckBox;
    edtExtEditor: TEdit;
    cbExtDiffer: TCheckBox;
    edtExtDiffer: TEdit;
    cbExtViewer: TCheckBox;
    edtExtViewer: TEdit;
    lblRunTerm: TLabel;
    edtRunTerm: TEdit;
    tsFonts: TTabSheet;
    lblMainFont: TLabel;
    cbMainFont: TComboBox;
    cbEditorFont: TComboBox;
    lblEditorFont: TLabel;
    edtTest1: TEdit;
    edtTest2: TEdit;
    edtTest3: TEdit;
    cbViewerFont: TComboBox;
    lblViewerFont: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edtEditorSizeChange(Sender: TObject);
    procedure edtMainSizeChange(Sender: TObject);
    procedure edtViewerSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSetHotKeyClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelEditFntClick(Sender: TObject);
    procedure btnSelMainFntClick(Sender: TObject);
    procedure btnSelViewFntClick(Sender: TObject);
    procedure cbExtEditorClick(Sender: TObject);
    procedure cbExtDifferClick(Sender: TObject);
    procedure cbExtViewerClick(Sender: TObject);
    procedure cbMainFontChange(Sender: TObject);
    procedure cbEditorFontChange(Sender: TObject);
    procedure cbViewerFontChange(Sender: TObject);
    procedure edHotKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    { Private declarations }
    vShortCut: TShortCut;
  public
    { Public declarations }
    procedure FillLngListBox;
    procedure LoadLng; override;
    procedure FillFontLists;
    procedure FillActionLists;
  end;


implementation

uses
  uLng, uGlobs, uGlobsPaths, fMain, ActnList, LCLProc, menus;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  inherited;
  FillActionLists;
  FillLngListBox;
  FillFontLists;
  DebugLn(gTerm);
  edtTerm.Text:=gTerm;
end;

procedure TfrmOptions.btSetHotKeyClick(Sender: TObject);
var vNum: integer;
    vActions: TAction;
begin
  // ToDo Black list HotKey which can't use

  for vNum := 0 to cbActions.Items.Count - 1 do
  begin
    vActions := cbActions.Items.Objects[vNum] as TAction;
    if vActions.ShortCut = vShortCut then
    begin
      ShowMessage('ShortCut used by '+vActions.Name);// ToDo lang
      Exit;
    end;
  end;

  vActions := cbActions.Items.Objects[cbActions.ItemIndex] as TAction;
  vActions.ShortCut := vShortCut;
  cbActions.Items[cbActions.ItemIndex] := vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')';
  cbActions.Text := vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')';
end;

procedure TfrmOptions.Button1Click(Sender: TObject);
begin
 if optColorDialog.Execute then
   begin
   end;
end;

procedure TfrmOptions.Button2Click(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
   end;
end;

procedure TfrmOptions.edtEditorSizeChange(Sender: TObject);
begin
  edtTest2.Font.Size := edtEditorSize.Value;
end;

procedure TfrmOptions.edtMainSizeChange(Sender: TObject);
begin
  edtTest1.Font.Size := edtMainSize.Value;
end;

procedure TfrmOptions.edtViewerSizeChange(Sender: TObject);
begin
  edtTest3.Font.Size := edtViewerSize.Value;
end;

procedure TfrmOptions.LoadLng;
begin
  btnCancel.Caption:= lngGetString(clngbutCancel);
//  btnOK.Caption:= lngGetString(clngbutOK);

  Caption:=lngGetString(clngDlgOpt);
  tsLng.Caption:=lngGetString(clngDlgOptSelLng);
  lblTerm.Caption:=lngGetString(clngDlgOptTerm);
  cbDirSelect.Caption:=lngGetString(clngDlgOptSelDir);
  cbCaseSensitiveSort.Caption:=lngGetString(clngDlgOptCaseSens);
  cbLynxLike.Caption:=lngGetString(clngDlgOptLynx);

  cbShortFileSizeFormat.Caption:=lngGetString(clngDlgOptShortFileSize);
  tsBehav.Caption:=lngGetString(clngDlgOptBehaviourTab);
  tsTools.Caption:=lngGetString(clngDlgOptToolsTab);

  cbExtEditor.Caption:=lngGetString(clngDlgOptExtEdit);
  cbExtViewer.Caption:=lngGetString(clngDlgOptExtView);
  cbExtDiffer.Caption:=lngGetString(clngDlgOptExtDiff);
  cbSeparateExt.Caption:=lngGetString(clngDlgOptSeparateExt);

  cbDirSelect.Checked:=gDirSelect;
  cbCaseSensitiveSort.Checked:=gCaseSensitiveSort;
  cbLynxLike.Checked:=gLynxLike;
  cbShortFileSizeFormat.Checked:=gShortFileSizeFormat;

  cbExtEditor.Checked:=gUseExtEdit;
  cbExtViewer.Checked:=gUseExtView;
  cbExtDiffer.Checked:=gUseExtDiff;
  cbSeparateExt.Checked:=gSeparateExt;

  edtExtEditor.Text:= gExtEdit;
  edtExtViewer.Text:=gExtView;
  edtExtDiffer.Text:=gExtDiff;
  
  edtExtEditor.Enabled:= cbExtEditor.Checked;
  edtExtDiffer.Enabled:= cbExtDiffer.Checked;
  edtExtViewer.Enabled:= cbExtViewer.Checked;

  edtRunTerm.Text:=gRunTerm;
  lblRunTerm.Caption:=lngGetString(clngDlgOptRunTerm);

  tsFonts.Caption:=lngGetString(clngDlgOptFonts);
  lblMainFont.Caption:= lngGetString(clngDlgOptMainFont);
  lblEditorFont.Caption:= lngGetString(clngDlgOptEditorFont);
  lblViewerFont.Caption:= lngGetString(clngDlgOptViewerFont);
  // ToDo lang to tsColor tsHotKey
end;

procedure TfrmOptions.FillLngListBox;
var
  fr:TSearchRec;
  iIndex:Integer;
begin
  lngList.Clear;
  DebugLn('Language dir:'+gpLngDir);
  if FindFirst(gpLngDir+'*.lng', faAnyFile, fr)<>0 then
  begin
    FindClose(fr);
    Exit;
  end;
  repeat
    lngList.Items.Add(fr.Name);
  until FindNext(fr)<>0;
  
  FindClose(fr);

  iIndex:=lngList.Items.IndexOf(gLng);
  if iIndex>=0 then
    lngList.Selected[iIndex]:=True;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  gTerm:=edtTerm.Text;
  if lngList.ItemIndex>-1 then
    gLng:=lngList.Items[lngList.ItemIndex];
  gDirSelect:=cbDirSelect.Checked;
  gCaseSensitiveSort:=cbCaseSensitiveSort.Checked;
  gLynxLike:=cbLynxLike.Checked;
  gShortFileSizeFormat:=cbShortFileSizeFormat.Checked;

  gUseExtEdit:=cbExtEditor.Checked;
  gUseExtView:=cbExtViewer.Checked;
  gUseExtDiff:=cbExtDiffer.Checked;
  gSeparateExt:=cbSeparateExt.Checked;

  gExtEdit:= edtExtEditor.Text;
  gExtView:= edtExtViewer.Text;
  gExtDiff:= edtExtDiffer.Text;
  gRunTerm:= edtRunTerm.Text;
  
  gFontName:=cbMainFont.Text;
  if (fsBold in EdtTest1.Font.Style) then
    gFontWeight := 700
  else
    gFontWeight := 400;
  gEditorFontName:=cbEditorFont.Text;
  gViewerFontName:=cbViewerFont.Text;
  
  gEditorSize:=Round(edtEditorSize.Value);
  gViewerSize:=Round(edtViewerSize.Value);
  gFontSize:=Round(edtMainSize.Value);
  
  frmMain.SaveShortCuts;
end;

procedure TfrmOptions.btnSelEditFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name:=cbEditorFont.Text;
  if dlgFnt.Execute then
    begin
      cbEditorFont.Text:=dlgFnt.Font.Name;
      edtEditorSize.Value := dlgFnt.Font.Size;
      edtTest2.Font := dlgFnt.Font;
    end;
end;

procedure TfrmOptions.btnSelMainFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name:=cbMainFont.Text;
  if dlgFnt.Execute then
    begin
      cbMainFont.Text:=dlgFnt.Font.Name;
      edtMainSize.Value := dlgFnt.Font.Size;
      edtTest1.Font := dlgFnt.Font;
    end;
end;

procedure TfrmOptions.btnSelViewFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name:=cbViewerFont.Text;
  if dlgFnt.Execute then
    begin
      cbViewerFont.Text:=dlgFnt.Font.Name;
      edtViewerSize.Value:= dlgFnt.Font.Size;
      edtTest3.Font := dlgFnt.Font;
    end;
end;

procedure TfrmOptions.cbExtEditorClick(Sender: TObject);
begin
  inherited;
  edtExtEditor.Enabled:=cbExtEditor.Checked
end;

procedure TfrmOptions.cbExtDifferClick(Sender: TObject);
begin
  inherited;
  edtExtDiffer.Enabled:=cbExtDiffer.Checked
end;

procedure TfrmOptions.cbExtViewerClick(Sender: TObject);
begin
  inherited;
  edtExtViewer.Enabled:=cbExtViewer.Checked
end;

procedure TfrmOptions.FillFontLists;
begin
  cbMainFont.Text := gFontName;
  cbViewerFont.Text := gViewerFontName;
  cbEditorFont.Text := gEditorFontName;

  if gFontWeight = 700 then
    EdtTest1.Font.Style := [fsBold];
    
  edtEditorSize.Value:=gEditorSize;
  edtViewerSize.Value:=gViewerSize;
  edtMainSize.Value:=gFontSize;
  
  with edtTest1.Font do
  begin
    Name := gFontName;
    Size := gFontSize;
  end; // with
  
  with edtTest2.Font do
  begin
    Name := gEditorFontName;
    Size := gEditorSize;
  end; // with
  
  with edtTest3.Font do
  begin
    Name := gViewerFontName;
    Size := gViewerSize;
  end; // with
end;

procedure TfrmOptions.FillActionLists;
var vNum: integer;
var vActions: TAction;
begin
  for vNum := 0 to frmMain.actionLst.ActionCount -1 do
  begin
    vActions := frmMain.actionLst.Actions[vNum] as TAction;
    cbActions.Items.AddObject(vActions.Name+'('+ShortCutToText(vActions.ShortCut)+')',vActions);
  end;
end;

procedure TfrmOptions.cbMainFontChange(Sender: TObject);
begin
//  edtTest1.Font.Name:=cbMainFont.Text;
end;

procedure TfrmOptions.cbEditorFontChange(Sender: TObject);
begin
//  edtTest2.Font.Name:=cbEditorFont.Text;
end;

procedure TfrmOptions.cbViewerFontChange(Sender: TObject);
begin
//  edtTest3.Font.Name:=cbViewerFont.Text;
end;

procedure TfrmOptions.edHotKeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  vShortCut := ShortCut(Key,Shift);
  TEdit(Sender).Text := ShortCutToText(vShortCut);
  Key := 0;
end;


initialization
 {$I foptions.lrs}

end.

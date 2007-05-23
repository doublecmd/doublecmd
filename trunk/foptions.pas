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
    btnSelEditFnt: TButton;
    btnSelMainFnt: TButton;
    btnSelViewFnt: TButton;
    btSetHotKey: TButton;
    Button1: TButton;
    Button2: TButton;
    cbackgrndcolor: TColorBox;
    cBackGrndLabel: TLabel;
    cbActions: TComboBox;
    cbCaseSensitiveSort: TCheckBox;
    cbDirSelect: TCheckBox;
    cbEditorFont: TComboBox;
    cbExtDiffer: TCheckBox;
    cbExtEditor: TCheckBox;
    cbExtViewer: TCheckBox;
    cbLynxLike: TCheckBox;
    cbMainFont: TComboBox;
    cbSeparateExt: TCheckBox;
    cbShortFileSizeFormat: TCheckBox;
    cbViewerFont: TComboBox;
    cTextColor: TColorBox;
    cTextLabel: TLabel;
    dlgFnt: TFontDialog;
    edHotKey: TEdit;
    edtEditorSize: TSpinEdit;
    edtExtDiffer: TEdit;
    edtExtEditor: TEdit;
    edtExtViewer: TEdit;
    edtMainSize: TSpinEdit;
    edtRunTerm: TEdit;
    edtTerm: TEdit;
    edtTest1: TEdit;
    edtTest2: TEdit;
    edtTest3: TEdit;
    edtViewerSize: TSpinEdit;
    gb: TGroupBox;
    ilTreeView: TImageList;
    lblActions: TLabel;
    lblEditorFont: TLabel;
    lblHotKey: TLabel;
    lblMainFont: TLabel;
    lblRunTerm: TLabel;
    lblTerm: TLabel;
    lblViewerFont: TLabel;
    lngList: TListBox;
    nbNotebook: TNotebook;
    optColorDialog: TColorDialog;
    pgPlugins: TPage;
    pnlCaption: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pgBehav: TPage;
    pgColor: TPage;
    pgFonts: TPage;
    pgHotKey: TPage;
    pgLng: TPage;
    pgTools: TPage;
    tvTreeView: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbackgrndcolorChange(Sender: TObject);
    procedure cbackgrndcolorDropDown(Sender: TObject);
    procedure cTextColorChange(Sender: TObject);
    procedure cTextColorDropDown(Sender: TObject);
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
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);
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
     cTextColor.Text := '';
     cTextColor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.Button2Click(Sender: TObject);
begin
   if optColorDialog.Execute then
   begin
     cbackgrndcolor.Text := '';
     cbackgrndcolor.Color := optColorDialog.Color;
   end;
end;

procedure TfrmOptions.cbackgrndcolorChange(Sender: TObject);
begin
  cbackgrndcolor.Color := cbackgrndcolor.Selection;
end;

procedure TfrmOptions.cbackgrndcolorDropDown(Sender: TObject);
begin
  cbackgrndcolor.Color := clWindow;
end;

procedure TfrmOptions.cTextColorChange(Sender: TObject);
begin
  cTextColor.Color := cTextColor.Selection;
end;

procedure TfrmOptions.cTextColorDropDown(Sender: TObject);
begin
  cTextColor.Color := clWindow;
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
  tvTreeView.Items.Item[0].Text := lngGetString(clngDlgOptSelLng);
  pnlCaption.Caption := tvTreeView.Items.Item[0].Text;
  lblTerm.Caption:=lngGetString(clngDlgOptTerm);
  cbDirSelect.Caption:=lngGetString(clngDlgOptSelDir);
  cbCaseSensitiveSort.Caption:=lngGetString(clngDlgOptCaseSens);
  cbLynxLike.Caption:=lngGetString(clngDlgOptLynx);

  cbShortFileSizeFormat.Caption:=lngGetString(clngDlgOptShortFileSize);
  tvTreeView.Items.Item[1].Text := lngGetString(clngDlgOptBehaviourTab);
  tvTreeView.Items.Item[2].Text := lngGetString(clngDlgOptToolsTab);

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

  tvTreeView.Items.Item[3].Text := lngGetString(clngDlgOptFonts);
  lblMainFont.Caption:= lngGetString(clngDlgOptMainFont);
  lblEditorFont.Caption:= lngGetString(clngDlgOptEditorFont);
  lblViewerFont.Caption:= lngGetString(clngDlgOptViewerFont);
  
  cTextColor.Selection := gForeColor;
  cTextColor.Color := gForeColor;
  cbackgrndcolor.Selection := gBackColor;
  cbackgrndcolor.Color := gBackColor;
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
  
  gForeColor := cTextColor.Color;
  gBackColor := cbackgrndcolor.Color;
  
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

procedure TfrmOptions.tvTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  nbNotebook.PageIndex := tvTreeView.Selected.Index;
  pnlCaption.Caption := tvTreeView.Selected.Text;
end;


initialization
 {$I foptions.lrs}

end.

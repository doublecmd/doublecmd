{
Build-in Editor for Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

This form used SynEdit and his Highlighters


contributors:

}


unit fEditor;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, Menus, SynEdit,
  ComCtrls,  SynEditSearch;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditSelectAll: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    ilImageList: TImageList;
    MainMenu1: TMainMenu;
    ActListEdit: TActionList;
    actAbout: TAction;
    actFileOpen: TAction;
    actFileClose: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileNew: TAction;
    actFileExit: TAction;
    miDelete: TMenuItem;
    miSelectAll: TMenuItem;
    miRedo: TMenuItem;
    miDeleteContext: TMenuItem;
    miSelectAllContext: TMenuItem;
    miSeparator2: TMenuItem;
    miPasteContext: TMenuItem;
    miCopyContext: TMenuItem;
    miCutContext: TMenuItem;
    miSeparator1: TMenuItem;
    miUndoContext: TMenuItem;
    miFile: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    pmContextMenu: TPopupMenu;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    miEdit: TMenuItem;
    miUndo: TMenuItem;
    N3: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    N4: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    Help1: TMenuItem;
    miAbout: TMenuItem;
    actSaveAll: TAction;
    StatusBar: TStatusBar;
    Editor: TSynEdit;
    miHighlight: TMenuItem;
    actEditFind: TAction;
    actEditRplc: TAction;
    actConfHigh: TAction;
    miDiv: TMenuItem;
    miConfHigh: TMenuItem;
    tbToolBar: TToolBar;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    tbSeparator1: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    tbSeparator2: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    tbSeparator3: TToolButton;
    tbConfig: TToolButton;
    tbHelp: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorReplaceText(Sender: TObject; const ASearch, AReplace: string;
       Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure actAboutExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure actEditFindExecute(Sender: TObject);
    procedure actEditRplcExecute(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure actSave2Execute(Sender: TObject);
    procedure actConfHighExecute(Sender: TObject);
    procedure frmEditorClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
    bChanged:Boolean;
    bNoName: Boolean;
    bIsShortCut:Boolean;
    bSearchBackwards:Boolean;
    bSearchCaseSensitive:Boolean;
    bSearchFromCaret:Boolean;
    bSearchSelectionOnly:Boolean;
    bSearchWholeWords:Boolean;
    sSearchText, sReplaceText:String;
    sReplaceTextHistory, sSearchTextHistory:String;    
  public
    { Public declarations }
    SynEditSearch: TSynEditSearch;
    procedure LoadFromIni;
    procedure SaveToIni;
{    Function CreateNewTab:Integer; // return tab number
    Function OpenFileNewTab(const sFileName:String):Integer;}
    procedure OpenFile(const sFileName:String);
    procedure UpdateStatus;
    procedure SetHighLighter(Sender:TObject);
    procedure UpdateHighlighterStatus;
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);    
  end;

  procedure ShowEditor(const sFileName:String);

implementation

uses
  dmDialogs, dmHigh, uLng,
  SynEditHighlighter, uShowMsg, fMsg, fEditSearch,
  SynEditTypes, uGlobsPaths, uGlobs, fEditorConf, LCLType;

procedure ShowEditor(const sFileName:String);
var editor: TfrmEditor;
begin
  editor := TfrmEditor.Create(Application);
  gEditorPos.Restore(editor);
  try
    LoadAttrFromFile(gpIniDir + csDefaultName);
    if sFileName='' then
      editor.actFileNew.Execute
    else
      editor.OpenFile(sFileName);
    editor.ShowOnTop;
  finally
    //editor.Free;
  end;
end;

procedure TfrmEditor.FormCreate(Sender: TObject);
var
  i:Integer;
  mi:TMenuItem;
begin
  LoadFromIni;
  Editor.Font.Name:=gEditorFontName;
  Editor.Font.Size:=gEditorSize;

// update menu highlighting
  miHighlight.Clear;
  for i:=0 to dmHighl.ComponentCount -1 do
    if dmHighl.Components[i] is TSynCustomHighlighter then
    begin
      mi:=TMenuItem.Create(miHighlight);
      mi.Caption:=TSynCustomHighlighter(dmHighl.Components[i]).GetLanguageName;
      mi.Tag:=i;
//      mi.Name:='miHigh'+IntToStr(i);
      mi.Enabled:=True;
      mi.OnClick:=@SetHighLighter;
      miHighlight.Add(mi);
    end;

end;

procedure TfrmEditor.OpenFile(const sFileName:String);
var
  h:TSynCustomHighlighter;
begin
  Editor.Lines.LoadFromFile(sFileName);
  h:= dmHighl.GetHighlighterByExt(ExtractFileExt(sFileName));
  SetupColorOfHighlighter(h);
  Editor.Highlighter:=h;
  UpdateHighlighterStatus;
  Caption:=sFileName;
  bChanged:=False;
  bNoname:=False;
  UpdateStatus;
end;

procedure TfrmEditor.actFileNewExecute(Sender: TObject);
begin
  inherited;
  Caption := rsMsgNewFile;
  Editor.Lines.Clear;
  bChanged:=False;
  bNoname:=True;
  UpdateStatus;
end;


procedure TfrmEditor.EditorReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction );
begin

  if ASearch = AReplace then
    ReplaceAction := raSkip
  else begin
    case MsgBox('Replace this text?',[msmbYes, msmbNo, msmbCancel, msmbAll], msmbYes, msmbNo) of
      mmrYes: ReplaceAction := raReplace;
      mmrAll: ReplaceAction := raReplaceAll;
      mmrNo: ReplaceAction := raSkip;
      else
        ReplaceAction := raCancel;
    end;
  end;
end;

procedure TfrmEditor.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
// this is hack, action hot key not work yet

  case Key of
   VK_F2:
     begin
       actFileSave.Execute;
       Key:=0;
     end;
   VK_N:
     begin
       if Shift=[ssCtrl] then
       begin
         actFileNew.Execute;
         Key:=0;
       end;
     end;
   VK_S:
     begin
       if Shift=[ssCtrl] then
       begin
         actFileSave.Execute;
         Key:=0;
       end;
     end;
   VK_F:
     begin
       if Shift=[ssCtrl] then
       begin
         actEditFind.Execute;
         Key:=0;
       end;
     end;
  end;
end;

procedure TfrmEditor.EditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=27 then
  begin
    Key:=0;
    Close;
  end;
end;

procedure TfrmEditor.actAboutExecute(Sender: TObject);
begin
  ShowMessage('Internal editor, part of Double Commander');
end;

procedure TfrmEditor.actEditCopyExecute(Sender: TObject);
begin
  editor.CopyToClipboard;
end;

procedure TfrmEditor.actEditCutExecute(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TfrmEditor.actEditPasteExecute(Sender: TObject);
begin
  editor.PasteFromClipboard;
end;

procedure TfrmEditor.actEditDeleteExecute(Sender: TObject);
begin
  Editor.ClearSelection;
end;

procedure TfrmEditor.actEditRedoExecute(Sender: TObject);
begin
  editor.Redo;
end;

procedure TfrmEditor.actEditSelectAllExecute(Sender: TObject);
begin
  editor.SelectAll;
end;

procedure TfrmEditor.actFileOpenExecute(Sender: TObject);
begin
  //inherited;
  dmDlg.OpenDialog.Filter:='*.*';
  if not dmDlg.OpenDialog.Execute then Exit;
  OpenFile(dmDlg.OpenDialog.FileName);
  UpdateStatus;
end;

procedure TfrmEditor.SetHighLighter(Sender:TObject);
var
  h:TSynCustomHighlighter;
begin
//  TQSynHighlighter(dmHigh.Components[TMenuItem(Sender).HelpContext]);
  h:=TSynCustomHighlighter(dmHighl.Components[TMenuItem(Sender).Tag]);
  SetupColorOfHighlighter(h);
  Editor.Highlighter:=h;
  UpdateHighlighterStatus;
end;


(*
This is code for multi tabs editor, it's buggy because
Synedit bad handle scrollbars in page control, maybe in
future, workaround: new tab must be visible and maybe must have focus

procedure TfrmEditor.actFileNewExecute(Sender: TObject);
var
  iPageIndex:Integer;
begin
  inherited;
  iPageIndex:=CreateNewTab;
  with pgEditor.Pages[iPageIndex] do
  begin
    Caption:='New'+IntToStr(iPageIndex);
    Hint:=''; // filename
  end;
end;


Function TfrmEditor.CreateNewTab:Integer; // return tab number
var
  iPageIndex:Integer;
begin
  with TTabSheet.Create(pgEditor) do // create Tab
  begin
    PageControl:=pgEditor;
    iPageIndex:=PageIndex;
    // now create Editor
    with TSynEdit.Create(pgEditor.Pages[PageIndex]) do
    begin
      Parent:=pgEditor.Pages[PageIndex];
      Align:=alClient;
      Lines.Clear;
    end;
  end;
end;


procedure TfrmEditor.actFileOpenExecute(Sender: TObject);
var
  iPageIndex:Integer;
begin
  inherited;
  dmDlg.OpenDialog.Filter:='*.*';
  if dmDlg.OpenDialog.Execute then
    OpenFileNewTab(dmDlg.OpenDialog.FileName);
end;

Function TfrmEditor.OpenFileNewTab(const sFileName:String):Integer;
var
  iPageIndex:Integer;
begin
  inherited;
  iPageIndex:=CreateNewTab;
  pgEditor.ActivePageIndex:=iPageIndex;
  with pgEditor.Pages[iPageIndex] do
  begin
    Caption:=sFileName;
    Hint:=sFileName;
    TSynEdit(pgEditor.Pages[iPageIndex].Components[0]).Lines.LoadFromFile(sFileName);
  end;
end;

procedure ShowEditor(lsFiles:TStringList);
var
  i:Integer;
begin
  with TfrmEditor.Create(Application) do
  begin
    try
      for i:=0 to lsFiles.Count-1 do
        OpenFileNewTab(lsFiles.Strings[i]);
      ShowModal;
    finally
      Free;
    end;
  end;
end;
 *)

procedure TfrmEditor.actEditUndoExecute(Sender: TObject);
begin
  inherited;
  Editor.Undo;
  UpdateStatus;
end;

procedure TfrmEditor.EditorChange(Sender: TObject);
begin
  inherited;
  bChanged:=True;
  UpdateStatus;
end;

procedure TfrmEditor.actFileSaveExecute(Sender: TObject);
begin
  inherited;
  if bNoname then
    actFileSaveAs.Execute
  else
  begin
    Editor.Lines.SaveToFile(Caption);
    bChanged:=False;
    UpdateStatus;
  end;
end;

procedure TfrmEditor.actFileSaveAsExecute(Sender: TObject);
begin
  inherited;
  dmDlg.SaveDialog.FileName:=Caption;
  dmDlg.SaveDialog.Filter:='*.*'; // rewrite for highlighter
  if not dmDlg.SaveDialog.Execute then Exit;

  Editor.Lines.SaveToFile(dmDlg.SaveDialog.FileName);
  bChanged:=False;
  bNoname:=False;
  Caption:=dmDlg.SaveDialog.FileName;

  UpdateStatus;
  Editor.Highlighter:= dmHighl.GetHighlighterByExt(ExtractFileExt(dmDlg.SaveDialog.FileName));
  UpdateHighlighterStatus;
end;

procedure TfrmEditor.UpdateStatus;
begin
  if bChanged then
    StatusBar.Panels[0].Text:='*'
  else
    StatusBar.Panels[0].Text:='';
  StatusBar.Panels[1].Text:=Format('%d:%d',[Editor.CaretX, Editor.CaretY]);
//  StatusBar.Panels[2].Text:=IntToStr(Length(Editor.Lines.Text));
end;

procedure TfrmEditor.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  inherited;
  UpdateStatus;
end;

procedure TfrmEditor.UpdateHighlighterStatus;
begin
  if assigned(Editor.Highlighter) then
    StatusBar.Panels[3].Text:= Editor.Highlighter.GetLanguageName;
end;

procedure TfrmEditor.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=False;
  if bChanged then
    case msgYesNoCancel(Format(rsMsgFileChangedSave,[Caption])) of
      mmrYes: actFileSave.Execute;
      mmrNo: bChanged:=False;
    else
      Exit;
    end;
  CanClose:=True;
end;

procedure TfrmEditor.EditorKeyPress(Sender: TObject; var Key: Char);
begin
//  inherited;
end;

procedure TfrmEditor.DoSearchReplaceText(AReplace: boolean;
  ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if bSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not bSearchFromCaret then
    Include(Options, ssoEntireScope);
  if bSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if bSearchWholeWords then
    Include(Options, ssoWholeWord);
  if Editor.SearchReplace(sSearchText, sReplaceText, Options) = 0 then
  begin
    if ssoBackwards in Options then
      Editor.BlockEnd := Editor.BlockBegin
    else
      Editor.BlockBegin := Editor.BlockEnd;
    Editor.CaretXY := Editor.BlockBegin;
  end;
end;

procedure TfrmEditor.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TfrmEditSearch;
begin
//  Statusbar.SimpleText := '';
  if AReplace then
    dlg := TfrmEditSearchReplace.Create(Self)
  else
    dlg := TfrmEditSearch.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := bSearchBackwards;
    SearchCaseSensitive := bSearchCaseSensitive;
    SearchFromCursor := bSearchFromCaret;
    SearchInSelectionOnly := bSearchSelectionOnly;
    // start with last search text
    SearchText := sSearchText;
{    if fSearchTextAtCaret then begin}
      // if something is selected search for that text
      if Editor.SelAvail and (Editor.BlockBegin.Y = Editor.BlockEnd.Y)
      then
        SearchText := Editor.SelText
      else
        SearchText := Editor.GetWordAtRowCol(Editor.CaretXY);
//    end;
    SearchTextHistory := sSearchTextHistory;
    if AReplace then with dlg as TfrmEditSearchReplace do begin
      ReplaceText := sReplaceText;
      ReplaceTextHistory := sReplaceTextHistory;
    end;
    SearchWholeWords := bSearchWholeWords;
    if ShowModal = mrOK then begin
      bSearchBackwards := SearchBackwards;
      bSearchCaseSensitive := SearchCaseSensitive;
      bSearchFromCaret := SearchFromCursor;
      bSearchSelectionOnly := SearchInSelectionOnly;
      bSearchWholeWords := SearchWholeWords;
      sSearchText := SearchText;
      sSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TfrmEditSearchReplace do begin
        sReplaceText := ReplaceText;
        sReplaceTextHistory := ReplaceTextHistory;
      end;
//      bSearchFromCaret := gbSearchFromCaret;
      if sSearchText <> '' then begin
        DoSearchReplaceText(AReplace, bSearchBackwards);
        bSearchFromCaret := TRUE;
      end;
    end;
  finally
    FreeAndNil(dlg);
  end;
end;

procedure TfrmEditor.actEditFindExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(False);
end;

procedure TfrmEditor.actEditRplcExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(True);
end;

procedure TfrmEditor.LoadFromIni;
var
  f:TextFile;
begin
  if FileExists(gpIniDir+'edithistory.txt') then
  begin
    assignFile(f,gpIniDir+'edithistory.txt');
    reset(f);
    try
      readln(f,sSearchTextHistory);
      readln(f,sReplaceTextHistory);
    finally
      closefile(f);
    end;
  end
  else
  begin
    sSearchTextHistory:='';
    sReplaceTextHistory:='';
  end;
end;

procedure TfrmEditor.SaveToIni;
var
  f:TextFile;
begin
  assignFile(f,gpIniDir+'edithistory.txt');
  rewrite(f);
  try
    writeln(f,sSearchTextHistory);
    writeln(f,sReplaceTextHistory);
  finally
    closefile(f);
  end;
  
  gEditorPos.Save(Self);
end;


procedure TfrmEditor.FormDestroy(Sender: TObject);
begin
  SaveToIni;
  inherited;
end;

procedure TfrmEditor.actSave2Execute(Sender: TObject);
begin
  inherited;
  actFileSave.Execute;
end;

procedure TfrmEditor.actConfHighExecute(Sender: TObject);
begin
  inherited;
  with TfrmEditorConf.Create(Application) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmEditor.frmEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;


initialization
 {$I feditor.lrs}

end.

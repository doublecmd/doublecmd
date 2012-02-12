{
Build-in Editor for Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

This form used SynEdit and his Highlighters

contributors:

Copyright (C) 2006-2009 Alexander Koblov (Alexx2000@mail.ru)

}

unit fEditor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, ActnList, Menus, SynEdit,
  ComCtrls, SynEditSearch, uDebug;

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
    actEditFindNext: TAction;
    actEditLineEndCrLf: TAction;
    actEditLineEndCr: TAction;
    actEditLineEndLf: TAction;
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
    miEditLineEndCr: TMenuItem;
    miEditLineEndLf: TMenuItem;
    miEditLineEndCrLf: TMenuItem;
    miLineEndType: TMenuItem;
    N5: TMenuItem;
    miEncodingOut: TMenuItem;
    miEncodingIn: TMenuItem;
    miEncoding: TMenuItem;
    miFindNext: TMenuItem;
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
    procedure actEditFindNextExecute(Sender: TObject);
    procedure actEditLineEndCrExecute(Sender: TObject);
    procedure actEditLineEndCrLfExecute(Sender: TObject);
    procedure actEditLineEndLfExecute(Sender: TObject);
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
    procedure actSave2Execute(Sender: TObject);
    procedure actConfHighExecute(Sender: TObject);
    procedure frmEditorClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
    bChanged:Boolean;
    bNoName: Boolean;
    bSearchBackwards:Boolean;
    bSearchCaseSensitive:Boolean;
    bSearchFromCaret:Boolean;
    bSearchSelectionOnly:Boolean;
    bSearchWholeWords:Boolean;
    bSearchRegExp:Boolean;
    FFileName: UTF8String;
    sSearchText, sReplaceText:String;
    sEncodingIn,
    sEncodingOut,
    sOriginalText: String;

    procedure ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
    {en
       Saves editor content to a file.
       @returns(@true if successful)
    }
    function SaveFile(const aFileName: UTF8String): Boolean;
    procedure SetFileName(const AValue: UTF8String);

  public
    { Public declarations }
    SynEditSearch: TSynEditSearch;
    {
    Function CreateNewTab:Integer; // return tab number
    Function OpenFileNewTab(const sFileName:String):Integer;
    }
    {en
       Opens a file.
       @returns(@true if successful)
    }
    function OpenFile(const aFileName: UTF8String): Boolean;
    procedure UpdateStatus;
    procedure SetEncodingIn(Sender:TObject);
    procedure SetEncodingOut(Sender:TObject);
    procedure SetHighLighter(Sender:TObject);
    procedure UpdateHighlighterStatus;
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);

    property FileName: UTF8String read FFileName write SetFileName;
  end;

  procedure ShowEditor(const sFileName:String);

implementation

{$R *.lfm}

uses
  dmCommonData, dmHigh, SynEditHighlighter, SynEditTypes, SynEditLines, LCLType,
  LConvEncoding, uLng, uShowMsg, fEditSearch, uGlobsPaths, uGlobs, fEditorConf,
  uOSUtils, uConvEncoding, uSynEditFiler;

procedure ShowEditor(const sFileName:String);
var
  editor: TfrmEditor;
begin
  editor := TfrmEditor.Create(Application);
  LoadAttrFromFile(gpCfgDir + csDefaultName);

  if sFileName = '' then
    editor.actFileNew.Execute
  else
  begin
    if not editor.OpenFile(sFileName) then
      Exit;
  end;

  editor.ShowOnTop;
end;

procedure TfrmEditor.FormCreate(Sender: TObject);
var
  i:Integer;
  mi:TMenuItem;
  EncodingsList: TStringList;
begin
  InitPropStorage(Self);

  FontOptionsToFont(gFonts[dcfEditor], Editor.Font);

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
// update menu encoding
  miEncodingIn.Clear;
  miEncodingOut.Clear;
  EncodingsList:= TStringList.Create;
  GetSupportedEncodings(EncodingsList);
  for I:= 0 to EncodingsList.Count - 1 do
    begin
      mi:= TMenuItem.Create(miEncodingIn);
      mi.Caption:= EncodingsList[I];
      mi.AutoCheck:= True;
      mi.RadioItem:= True;
      mi.GroupIndex:= 1;
      mi.OnClick:= @SetEncodingIn;
      miEncodingIn.Add(mi);
    end;
  for I:= 0 to EncodingsList.Count - 1 do
    begin
      mi:= TMenuItem.Create(miEncodingOut);
      mi.Caption:= EncodingsList[I];
      mi.AutoCheck:= True;
      mi.RadioItem:= True;
      mi.GroupIndex:= 2;
      mi.OnClick:= @SetEncodingOut;
      miEncodingOut.Add(mi);
    end;
  EncodingsList.Free;
  // if we already search text then use last searched text
  if not gFirstTextSearch then
    begin
      if glsSearchHistory.Count > 0 then
        sSearchText:= glsSearchHistory[0];
    end;

  FixFormIcon(Handle);
end;

procedure TfrmEditor.actEditFindNextExecute(Sender: TObject);
begin
  if gFirstTextSearch then
    begin
      ShowSearchReplaceDialog(False);
      Exit;
    end;
  if sSearchText <> '' then
    begin
      DoSearchReplaceText(False, bSearchBackwards);
      bSearchFromCaret:= True;
    end;
end;

procedure TfrmEditor.actEditLineEndCrExecute(Sender: TObject);
begin
  with (Editor.Lines as TSynEditLines) do
  FileWriteLineEndType:= sfleCr;
end;

procedure TfrmEditor.actEditLineEndCrLfExecute(Sender: TObject);
begin
  with (Editor.Lines as TSynEditLines) do
  FileWriteLineEndType:= sfleCrLf;
end;

procedure TfrmEditor.actEditLineEndLfExecute(Sender: TObject);
begin
  with (Editor.Lines as TSynEditLines) do
  FileWriteLineEndType:= sfleLf;
end;

function TfrmEditor.OpenFile(const aFileName: UTF8String): Boolean;
var
  h: TSynCustomHighlighter;
  Reader: TSynEditFileReader;
begin
  Result := False;
  try
    Reader := TSynEditFileReader.Create(aFileName);
    try
      Editor.Lines.BeginUpdate;
      try
        Editor.Lines.Clear;
        while not Reader.EOF do
          Editor.Lines.Add(Reader.ReadLine);
      finally
        Editor.Lines.EndUpdate;
      end;
    finally
      with (Editor.Lines as TSynEditLines) do
      FileWriteLineEndType:= Reader.LineEndType;
      case Reader.LineEndType of
        sfleCrLf: actEditLineEndCrLf.Checked:= True;
        sfleCr:   actEditLineEndCr.Checked:= True;
        sfleLf:   actEditLineEndLf.Checked:= True;
      end;
      Reader.Free;
    end;
    Result := True;
  except
    on E: EFCreateError do
      begin
        DCDebug(E.Message);
        msgWarning(rsMsgErrECreate + ' ' + aFileName);
        Exit;
      end;
    on E: EFOpenError do
      begin
        DCDebug(E.Message);
        msgWarning(rsMsgErrEOpen + ' ' + aFileName);
        Exit;
      end;
  end;

  // set up text encoding
  sOriginalText := Editor.Lines.Text; // save original text
  // try to detect encoding by first 4 kb of text
  sEncodingIn := DetectEncoding(Copy(sOriginalText, 1, 4096));
  ChooseEncoding(miEncodingIn, sEncodingIn);
  sEncodingOut := sEncodingIn; // by default
  ChooseEncoding(miEncodingOut, sEncodingOut);
  if sEncodingIn <> EncodingUTF8 then
    Editor.Lines.Text := ConvertEncoding(sOriginalText, sEncodingIn, EncodingUTF8);
  // set up highlighter
  h := dmHighl.GetHighlighterByExt(ExtractFileExt(aFileName));
  SetupColorOfHighlighter(h);
  Editor.Highlighter := h;
  UpdateHighlighterStatus;
  FileName := aFileName;
  bChanged := False;
  bNoname := False;
  UpdateStatus;
end;

function TfrmEditor.SaveFile(const aFileName: UTF8String): Boolean;
var
  I: Integer;
  Writer: TSynEditFileWriter;
begin
  Result := False;
  try
    Writer := TSynEditFileWriter.Create(aFileName);
    try
      Writer.LineEndType := (Editor.Lines as TSynEditLines).FileWriteLineEndType;
      for I := 0 to Editor.Lines.Count - 1 do
        Writer.WriteLine(ConvertEncoding(Editor.Lines[I], EncodingUTF8, sEncodingOut));
    finally
      Writer.Free;
    end;

    Editor.Modified:= False; // needed for the undo stack
    Editor.MarkTextAsSaved;
    Result := True;
  except
    on e: EFCreateError do
    begin
      DCDebug(e.Message);
      msgWarning(rsMsgErrSaveFile + ' ' + aFileName);
    end;
    on e: EFOpenError do
    begin
      DCDebug(e.Message);
      msgWarning(rsMsgErrSaveFile + ' ' + aFileName);
    end;
  end;
end;

procedure TfrmEditor.SetFileName(const AValue: UTF8String);
begin
  if FFileName = AValue then
    Exit;

  FFileName := AValue;
  Caption := FFileName;
end;

procedure TfrmEditor.actFileNewExecute(Sender: TObject);
begin
  inherited;
  FileName := rsMsgNewFile;
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
  // To prevent else one editor or viewer open on key F4 in viewer
  VK_F4:
    begin
      if Shift <> [ssAlt] then
        Key:=0;
    end;
  VK_F3:
    begin
      actEditFindNext.Execute;
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
  msgOK(rsEditAboutText);
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
  dmComData.OpenDialog.Filter:='*.*';
  if not dmComData.OpenDialog.Execute then Exit;
  if OpenFile(dmComData.OpenDialog.FileName) then
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
  if bNoname then
    actFileSaveAs.Execute
  else
  begin
    SaveFile(FileName);
    bChanged:=False;
    UpdateStatus;
  end;
end;

procedure TfrmEditor.actFileSaveAsExecute(Sender: TObject);
begin
  dmComData.SaveDialog.FileName := FileName;
  dmComData.SaveDialog.Filter:='*.*'; // rewrite for highlighter
  if not dmComData.SaveDialog.Execute then
    Exit;

  FileName := dmComData.SaveDialog.FileName;
  SaveFile(FileName);
  bChanged:=False;
  bNoname:=False;

  UpdateStatus;
  Editor.Highlighter:= dmHighl.GetHighlighterByExt(ExtractFileExt(FileName));
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

procedure TfrmEditor.SetEncodingIn(Sender: TObject);
begin
  sEncodingIn:= (Sender as TMenuItem).Caption;
  sEncodingOut:= sEncodingIn;
  ChooseEncoding(miEncodingOut, sEncodingOut);
  Editor.Lines.Text:= ConvertEncoding(sOriginalText, sEncodingIn, EncodingUTF8);
end;

procedure TfrmEditor.SetEncodingOut(Sender: TObject);
begin
  sEncodingOut:= (Sender as TMenuItem).Caption;
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
    case msgYesNoCancel(Format(rsMsgFileChangedSave,[FileName])) of
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
  if bSearchRegExp then
    Include(Options, ssoRegExpr);
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
  dlg: TfrmEditSearchReplace;
begin
//  Statusbar.SimpleText := '';
  dlg := TfrmEditSearchReplace.Create(Self, AReplace);
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
    SearchTextHistory := glsSearchHistory.Text;
    if AReplace then with dlg as TfrmEditSearchReplace do begin
      ReplaceText := sReplaceText;
      ReplaceTextHistory := glsReplaceHistory.Text;
    end;
    SearchWholeWords := bSearchWholeWords;
    if ShowModal = mrOK then begin
      bSearchBackwards := SearchBackwards;
      bSearchCaseSensitive := SearchCaseSensitive;
      bSearchFromCaret := SearchFromCursor;
      bSearchSelectionOnly := SearchInSelectionOnly;
      bSearchWholeWords := SearchWholeWords;
      bSearchRegExp := SearchRegExp;
      sSearchText := SearchText;
      glsSearchHistory.Text := SearchTextHistory;
      if AReplace then with dlg as TfrmEditSearchReplace do begin
        sReplaceText := ReplaceText;
        glsReplaceHistory.Text := ReplaceTextHistory;
      end;
//      bSearchFromCaret := gbSearchFromCaret;
      if sSearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, bSearchBackwards);
        bSearchFromCaret:= True;
        gFirstTextSearch:= False;
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

procedure TfrmEditor.ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
var
  I: Integer;
begin
  sEncoding:= NormalizeEncoding(sEncoding);
  for I:= 0 to mnuMenuItem.Count - 1 do
    if SameText(NormalizeEncoding(mnuMenuItem.Items[I].Caption), sEncoding) then
      mnuMenuItem.Items[I].Checked:= True;
end;

end.
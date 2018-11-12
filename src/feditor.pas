{
   Double Commander
   -------------------------------------------------------------------------
   Build-in Editor using SynEdit and his Highlighters

   Copyright (C) 2006-2018  Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.

   Legacy comment from its origin:
     Build-in Editor for Seksi Commander
     ----------------------------
     Licence  : GNU GPL v 2.0
     Author   : radek.cervinka@centrum.cz
     This form used SynEdit and his Highlighters
     contributors:
     Copyright (C) 2006-2015 Alexander Koblov (Alexx2000@mail.ru)
}

unit fEditor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, ActnList, Menus, SynEdit,
  ComCtrls, SynEditSearch, SynEditHighlighter, uDebug, uOSForms, uShowForm, types, Graphics,
  uFormCommands, uHotkeyManager;

const
  HotkeysCategory = 'Editor';

type
  { TfrmEditor }
  TfrmEditor = class(TAloneForm,IFormCommands)
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
    actEditGotoLine: TAction;
    actEditFindPrevious: TAction;
    actFileReload: TAction;
    MainMenu1: TMainMenu;
    ActListEdit: TActionList;
    actAbout: TAction;
    actFileOpen: TAction;
    actFileClose: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileNew: TAction;
    actFileExit: TAction;
    MenuItem1: TMenuItem;
    miFileReload: TMenuItem;
    miFindPrevious: TMenuItem;
    miGotoLine: TMenuItem;
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
    procedure actExecute(Sender: TObject);
    procedure EditorMouseWheelDown(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure EditorMouseWheelUp(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure EditorReplaceText(Sender: TObject; const ASearch, AReplace: string;
       {%H-}Line, {%H-}Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure EditorChange(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject;
      {%H-}Changes: TSynStatusChanges);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    FFileName: String;
    sSearchText, sReplaceText:String;
    sEncodingIn,
    sEncodingOut,
    sOriginalText: String;
    FWaitData: TWaitData;
    FCommands: TFormCommands;

    property Commands: TFormCommands read FCommands implements IFormCommands;

    procedure ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
    {en
       Saves editor content to a file.
       @returns(@true if successful)
    }
    function SaveFile(const aFileName: String): Boolean;
    procedure SetFileName(const AValue: String);

  public
    { Public declarations }
    SynEditSearch: TSynEditSearch;
    {
    Function CreateNewTab:Integer; // return tab number
    Function OpenFileNewTab(const sFileName:String):Integer;
    }
    destructor Destroy; override;
    {en
       Opens a file.
       @returns(@true if successful)
    }
    function OpenFile(const aFileName: String): Boolean;
    procedure UpdateStatus;
    procedure SetEncodingIn(Sender:TObject);
    procedure SetEncodingOut(Sender:TObject);
    procedure SetHighLighter(Sender:TObject);
    procedure UpdateHighlighter(Highlighter: TSynCustomHighlighter);
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure LoadGlobalOptions;

    property FileName: String read FFileName write SetFileName;

   published
     procedure cm_FileReload(const Params: array of string);
     procedure cm_EditFind(const {%H-}Params:array of string);
     procedure cm_EditFindNext(const {%H-}Params:array of string);
     procedure cm_EditFindPrevious(const {%H-}Params:array of string);
     procedure cm_EditGotoLine(const {%H-}Params:array of string);
     procedure cm_EditLineEndCr(const {%H-}Params:array of string);
     procedure cm_EditLineEndCrLf(const {%H-}Params:array of string);
     procedure cm_EditLineEndLf(const {%H-}Params:array of string);
     procedure cm_EditDelete(const {%H-}Params:array of string);
     procedure cm_EditRedo(const {%H-}Params:array of string);
     procedure cm_About(const {%H-}Params:array of string);
     procedure cm_EditCopy(const {%H-}Params:array of string);
     procedure cm_EditCut(const {%H-}Params:array of string);
     procedure cm_EditPaste(const {%H-}Params:array of string);
     procedure cm_EditSelectAll(const {%H-}Params:array of string);
     procedure cm_FileNew(const {%H-}Params:array of string);
     procedure cm_FileOpen(const {%H-}Params:array of string);
     procedure cm_EditUndo(const {%H-}Params:array of string);
     procedure cm_FileSave(const {%H-}Params:array of string);
     procedure cm_FileSaveAs(const {%H-}Params:array of string);
     procedure cm_FileExit(const {%H-}Params:array of string);
     procedure cm_ConfHigh(const {%H-}Params:array of string);
     procedure cm_EditRplc(const {%H-}Params:array of string);
  end;

  procedure ShowEditor(const sFileName: String; WaitData: TWaitData = nil);

  var
    LastEditorUsedForConfiguration: TfrmEditor = nil;

implementation

{$R *.lfm}

uses
  Clipbrd, dmCommonData, dmHigh, SynEditTypes, LCLType, LConvEncoding,
  uLng, uShowMsg, fEditSearch, uGlobs, fOptions, DCClassesUtf8,
  uOSUtils, uConvEncoding, fOptionsToolsEditor, uDCUtils, uClipboard;

procedure ShowEditor(const sFileName: String; WaitData: TWaitData = nil);
var
  Editor: TfrmEditor;
begin
  Editor := TfrmEditor.Create(Application);
  Editor.FWaitData := WaitData;

  if sFileName = '' then
    Editor.cm_FileNew([''])
  else
  begin
    if not Editor.OpenFile(sFileName) then
      Exit;
  end;

  Editor.ShowOnTop;
  LastEditorUsedForConfiguration := Editor;
end;

procedure TfrmEditor.FormCreate(Sender: TObject);
var
  i:Integer;
  mi:TMenuItem;
  EncodingsList: TStringList;
  HMEditor: THMForm;
begin
  InitPropStorage(Self);

  Menu.Images:= dmComData.ilEditorImages;

  LoadGlobalOptions;

// update menu highlighting
  miHighlight.Clear;
  for i:=0 to dmHighl.SynHighlighterList.Count - 1 do
    begin
      mi:=TMenuItem.Create(miHighlight);
      mi.Caption:=TSynCustomHighlighter(dmHighl.SynHighlighterList.Objects[i]).LanguageName;
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

  HMEditor := HotMan.Register(Self, HotkeysCategory);
  HMEditor.RegisterActionList(ActListEdit);
  FCommands := TFormCommands.Create(Self, ActListEdit);
end;

procedure TfrmEditor.LoadGlobalOptions;
begin
  Editor.Options:= gEditorSynEditOptions;
  FontOptionsToFont(gFonts[dcfEditor], Editor.Font);
  Editor.TabWidth := gEditorSynEditTabWidth;
end;

procedure TfrmEditor.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.ExecuteCommand(cmd, []);
end;

procedure TfrmEditor.EditorMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
   t:integer;
begin
  if (Shift=[ssCtrl])and(gFonts[dcfEditor].Size>MIN_FONT_SIZE_EDITOR) then
  begin
    t:=Editor.TopLine;
    gFonts[dcfEditor].Size:=gFonts[dcfEditor].Size-1;
    FontOptionsToFont(gFonts[dcfEditor], Editor.Font);
    Editor.TopLine:=t;
    Editor.Refresh;
    Handled:=True;
  end;

end;

procedure TfrmEditor.EditorMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
   t:integer;
begin
  if (Shift=[ssCtrl])and(gFonts[dcfEditor].Size<MAX_FONT_SIZE_EDITOR) then
  begin
    t:=Editor.TopLine;
    gFonts[dcfEditor].Size:=gFonts[dcfEditor].Size+1;
    FontOptionsToFont(gFonts[dcfEditor], Editor.Font);
    Editor.TopLine:=t;
    Editor.Refresh;
    Handled:=True;
  end;

end;


function TfrmEditor.OpenFile(const aFileName: String): Boolean;
var
  Buffer: AnsiString;
  Reader: TFileStreamEx;
  Highlighter: TSynCustomHighlighter;
begin
  Result := False;
  try
    Reader := TFileStreamEx.Create(aFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(sOriginalText, Reader.Size);
      Reader.Read(Pointer(sOriginalText)^, Length(sOriginalText));
    finally
      Reader.Free;
    end;

    // Try to detect encoding by first 4 kb of text
    Buffer := Copy(sOriginalText, 1, 4096);
    sEncodingIn := DetectEncoding(Buffer);
    ChooseEncoding(miEncodingIn, sEncodingIn);
    sEncodingOut := sEncodingIn; // by default
    ChooseEncoding(miEncodingOut, sEncodingOut);

    // Try to guess line break style
    with Editor.Lines do
    begin
      if (sEncodingIn <> EncodingUCS2LE) and (sEncodingIn <> EncodingUCS2BE) then
        TextLineBreakStyle := GuessLineBreakStyle(Buffer)
      else begin
        sOriginalText := Copy(sOriginalText, 3, MaxInt); // Skip BOM
        TextLineBreakStyle := GuessLineBreakStyle(ConvertEncoding(Buffer, sEncodingIn, EncodingUTF8));
      end;

      case TextLineBreakStyle of
        tlbsCRLF: actEditLineEndCrLf.Checked := True;
        tlbsCR:   actEditLineEndCr.Checked := True;
        tlbsLF:   actEditLineEndLf.Checked := True;
      end;
    end;

    // Convert encoding if needed
    if sEncodingIn = EncodingUTF8 then
      Buffer := sOriginalText
    else begin
      Buffer := ConvertEncoding(sOriginalText, sEncodingIn, EncodingUTF8);
    end;

    // Load text into editor
    Editor.Lines.Text := Buffer;

    // Add empty line if needed
    if (Length(Buffer) > 0) and (Buffer[Length(Buffer)] in [#10, #13]) then
      Editor.Lines.Add(EmptyStr);

    Result := True;
  except
    on E: EFCreateError do
      begin
        DCDebug(E.Message);
        msgError(rsMsgErrECreate + ' ' + aFileName);
        Exit;
      end;
    on E: EFOpenError do
      begin
        DCDebug(E.Message);
        msgError(rsMsgErrEOpen + ' ' + aFileName);
        Exit;
      end;
    on E: EReadError do
      begin
        DCDebug(E.Message);
        msgError(rsMsgErrERead + ' ' + aFileName);
        Exit;
      end;
  end;

  // set up highlighter
  Highlighter := dmHighl.GetHighlighter(Editor, ExtractFileExt(aFileName));
  UpdateHighlighter(Highlighter);
  FileName := aFileName;
  bChanged := False;
  bNoname := False;
  UpdateStatus;
end;

function TfrmEditor.SaveFile(const aFileName: String): Boolean;
var
  TextOut: String;
  Encoding: String;
  Writer: TFileStreamEx;
begin
  Result := False;
  try
    Writer := TFileStreamEx.Create(aFileName, fmCreate);
    try
      Encoding := NormalizeEncoding(sEncodingOut);
      // If file is empty and encoding with BOM then write only BOM
      if (Editor.Lines.Count = 0) then
      begin
        if (Encoding = EncodingUTF8BOM) then
          Writer.WriteBuffer(UTF8BOM, SizeOf(UTF8BOM))
        else if (Encoding = EncodingUCS2LE) then
          Writer.WriteBuffer(UTF16LEBOM, SizeOf(UTF16LEBOM))
        else if (Encoding = EncodingUCS2BE) then
          Writer.WriteBuffer(UTF16BEBOM, SizeOf(UTF16BEBOM));
      end
      else begin
        TextOut := EmptyStr;
        if (Encoding = EncodingUCS2LE) then
          TextOut := UTF16LEBOM
        else if (Encoding = EncodingUCS2BE) then begin
          TextOut := UTF16BEBOM
        end;
        TextOut += ConvertEncoding(Editor.Lines[0], EncodingUTF8, sEncodingOut);
        Writer.WriteBuffer(Pointer(TextOut)^, Length(TextOut));

        // If file has only one line then write it without line break
        if Editor.Lines.Count > 1 then
        begin
          TextOut := TextLineBreakValue[Editor.Lines.TextLineBreakStyle];
          TextOut += GetTextRange(Editor.Lines, 1, Editor.Lines.Count - 2);
          // Special case for UTF-8 and UTF-8 with BOM
          if (Encoding <> EncodingUTF8) and (Encoding <> EncodingUTF8BOM) then begin
            TextOut:= ConvertEncoding(TextOut, EncodingUTF8, sEncodingOut);
          end;
          Writer.WriteBuffer(Pointer(TextOut)^, Length(TextOut));
          // Write last line without line break
          TextOut:= Editor.Lines[Editor.Lines.Count - 1];
          // Special case for UTF-8 and UTF-8 with BOM
          if (Encoding <> EncodingUTF8) and (Encoding <> EncodingUTF8BOM) then begin
            TextOut:= ConvertEncoding(TextOut, EncodingUTF8, sEncodingOut);
          end;
          Writer.WriteBuffer(Pointer(TextOut)^, Length(TextOut));
        end;
      end;
    finally
      Writer.Free;
    end;

    Editor.Modified := False; // needed for the undo stack
    Editor.MarkTextAsSaved;
    Result := True;
  except
    on E: Exception do
      msgError(rsMsgErrSaveFile + ' ' + aFileName + LineEnding + E.Message);
  end;
end;

procedure TfrmEditor.SetFileName(const AValue: String);
begin
  if FFileName = AValue then
    Exit;

  FFileName := AValue;
  Caption := FFileName;
end;

destructor TfrmEditor.Destroy;
begin
  LastEditorUsedForConfiguration := nil;
  HotMan.UnRegister(Self);
  inherited Destroy;
  if Assigned(FWaitData) then FWaitData.Done;
end;


procedure TfrmEditor.EditorReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction );
begin
  if ASearch = AReplace then
    ReplaceAction := raSkip
  else begin
    case MsgBox(rsMsgReplaceThisText, [msmbYes, msmbNo, msmbCancel, msmbAll], msmbYes, msmbNo) of
      mmrYes: ReplaceAction := raReplace;
      mmrAll: ReplaceAction := raReplaceAll;
      mmrNo: ReplaceAction := raSkip;
      else
        ReplaceAction := raCancel;
    end;
  end;
end;

procedure TfrmEditor.SetHighLighter(Sender:TObject);
var
  Highlighter: TSynCustomHighlighter;
begin
  Highlighter:= TSynCustomHighlighter(dmHighl.SynHighlighterList.Objects[TMenuItem(Sender).Tag]);
  UpdateHighlighter(Highlighter);
end;

(*
This is code for multi tabs editor, it's buggy because
Synedit bad handle scrollbars in page control, maybe in
future, workaround: new tab must be visible and maybe must have focus

procedure TfrmEditor.cm_FileNewExecute(Sender: TObject);
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


procedure TfrmEditor.cm_FileOpenExecute(const Params:array of string);
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


procedure TfrmEditor.EditorChange(Sender: TObject);
begin
  inherited;
  bChanged:=True;
  UpdateStatus;
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

procedure TfrmEditor.UpdateHighlighter(Highlighter: TSynCustomHighlighter);
begin
  dmHighl.SetHighlighter(Editor, Highlighter);
  StatusBar.Panels[3].Text:= Highlighter.LanguageName;
end;

procedure TfrmEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:= False;
  if bChanged then
    case msgYesNoCancel(Format(rsMsgFileChangedSave,[FileName])) of
      mmrYes: cm_FileSave(['']);
      mmrNo: bChanged:= False;
    else
      Exit;
    end;
  CanClose:= True;
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
  try
    if Editor.SearchReplace(sSearchText, sReplaceText, Options) = 0 then
    begin
      if ssoBackwards in Options then
        Editor.BlockEnd := Editor.BlockBegin
      else
        Editor.BlockBegin := Editor.BlockEnd;
      Editor.CaretXY := Editor.BlockBegin;
      msgOK(Format(rsViewNotFound, ['"' + sSearchText + '"']));
    end;
  except
    on E: Exception do msgError(E.Message);
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
    SearchRegExp := bSearchRegExp;
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

procedure TfrmEditor.cm_FileReload(const Params: array of string);
begin
  if bChanged then
  begin
    if not msgYesNo(rsMsgFileReloadWarning) then
      Exit;
  end;
  OpenFile(FFileName);
end;

procedure TfrmEditor.cm_EditFind(const Params: array of string);
begin
  ShowSearchReplaceDialog(False);
end;


procedure TfrmEditor.cm_EditFindNext(const Params:array of string);
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

procedure TfrmEditor.cm_EditFindPrevious(const Params: array of string);
begin
  if gFirstTextSearch then
    begin
      bSearchBackwards:=True;
      ShowSearchReplaceDialog(False);
      Exit;
    end;
  if sSearchText <> '' then
    begin
      Editor.SelEnd:=Editor.SelStart;
      DoSearchReplaceText(False, True);
      bSearchFromCaret:= True;
    end;
end;


procedure TfrmEditor.cm_EditGotoLine(const Params:array of string);
var
  P: TPoint;
  Value: String;
  NewTopLine: Integer;
begin
  if ShowInputQuery(rsEditGotoLineTitle, rsEditGotoLineQuery, Value) then
  begin
    P.X := 1;
    P.Y := StrToIntDef(Value, 1);
    NewTopLine := P.Y - (Editor.LinesInWindow div 2);
    if NewTopLine < 1 then NewTopLine:= 1;
    Editor.CaretXY := P;
    Editor.TopLine := NewTopLine;
    Editor.SetFocus;
  end;
end;

procedure TfrmEditor.cm_EditLineEndCr(const Params:array of string);
begin
  Editor.Lines.TextLineBreakStyle:= tlbsCR;
end;

procedure TfrmEditor.cm_EditLineEndCrLf(const Params:array of string);
begin
  Editor.Lines.TextLineBreakStyle:= tlbsCRLF;
end;

procedure TfrmEditor.cm_EditLineEndLf(const Params:array of string);
begin
  Editor.Lines.TextLineBreakStyle:= tlbsLF;
end;

procedure TfrmEditor.cm_About(const Params:array of string);
begin
  msgOK(rsEditAboutText);
end;

procedure TfrmEditor.cm_EditCopy(const Params:array of string);
begin
  editor.CopyToClipboard;
  ClipboardSetText(Clipboard.AsText);
end;

procedure TfrmEditor.cm_EditCut(const Params:array of string);
begin
  Editor.CutToClipboard;
  ClipboardSetText(Clipboard.AsText);
end;

procedure TfrmEditor.cm_EditPaste(const Params:array of string);
begin
  editor.PasteFromClipboard;
end;

procedure TfrmEditor.cm_EditDelete(const Params:array of string);
begin
  Editor.ClearSelection;
end;

procedure TfrmEditor.cm_EditRedo(const Params:array of string);
begin
  editor.Redo;
end;

procedure TfrmEditor.cm_EditSelectAll(const Params:array of string);
begin
  editor.SelectAll;
end;

procedure TfrmEditor.cm_FileNew(const Params:array of string);
var
  CanClose: Boolean = False;
begin
  FormCloseQuery(Self, CanClose);
  if not CanClose then Exit;
  FileName := rsMsgNewFile;
  Editor.Lines.Clear;
  bChanged:= False;
  bNoname:= True;
  UpdateStatus;
end;

procedure TfrmEditor.cm_FileOpen(const Params:array of string);
var
  CanClose: Boolean = False;
begin
  FormCloseQuery(Self, CanClose);
  if not CanClose then Exit;
  dmComData.OpenDialog.Filter:= '*.*';
  if not dmComData.OpenDialog.Execute then Exit;
  if OpenFile(dmComData.OpenDialog.FileName) then
    UpdateStatus;
end;

procedure TfrmEditor.cm_EditUndo(const Params:array of string);
begin
  Editor.Undo;
  UpdateStatus;
end;

procedure TfrmEditor.cm_FileSave(const Params:array of string);
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

procedure TfrmEditor.cm_FileSaveAs(const Params:array of string);
var
  Highlighter: TSynCustomHighlighter;
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
  Highlighter:= dmHighl.GetHighlighter(Editor, ExtractFileExt(FileName));
  UpdateHighlighter(Highlighter);
end;

procedure TfrmEditor.cm_FileExit(const Params:array of string);
begin
  Close;
end;

procedure TfrmEditor.cm_ConfHigh(const Params:array of string);
begin
  LastEditorUsedForConfiguration := Self;
  ShowOptions(TfrmOptionsEditor);
end;

procedure TfrmEditor.cm_EditRplc(const Params: array of string);
begin
  ShowSearchReplaceDialog(True);
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

initialization
  TFormCommands.RegisterCommandsForm(TfrmEditor, HotkeysCategory, @rsHotkeyCategoryEditor);

end.

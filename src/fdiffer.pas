{
   Double Commander
   -------------------------------------------------------------------------
   Internal diff and merge tool

   Copyright (C) 2010-2024 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fDiffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Menus, ComCtrls,
  ActnList, ExtCtrls, EditBtn, Buttons, SynEdit, uSynDiffControls, LMessages,
  KASComCtrls, uPariterControls, uDiffOND, uFormCommands, uHotkeyManager, uOSForms,
  uBinaryDiffViewer, uShowForm, KASStatusBar, Graphics, StdCtrls, fEditSearch;

type

  { TStatusBar }

  TStatusBar = class(TKASStatusBar);

  { TfrmDiffer }

  TfrmDiffer = class(TAloneForm, IFormCommands)
    actBinaryCompare: TAction;
    actCopyLeftToRight: TAction;
    actCopyRightToLeft: TAction;
    actExit: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditDelete: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
    actFind: TAction;
    actFindNext: TAction;
    actFindPrev: TAction;
    actFindReplace: TAction;
    actGotoLine: TAction;
    actEditSelectAll: TAction;
    actEditPaste: TAction;
    actAbout: TAction;
    actAutoCompare: TAction;
    actLineDifferences: TAction;
    actSaveRightAs: TAction;
    actSaveLeftAs: TAction;
    actOpenRight: TAction;
    actOpenLeft: TAction;
    actReload: TAction;
    actSaveRight: TAction;
    actSaveLeft: TAction;
    actPaintBackground: TAction;
    actStartCompare: TAction;
    actFirstDifference: TAction;
    actIgnoreCase: TAction;
    actIgnoreWhiteSpace: TAction;
    actCancelCompare: TAction;
    actKeepScrolling: TAction;
    actPrevDifference: TAction;
    actLastDifference: TAction;
    actNextDifference: TAction;
    actSaveAs: TAction;
    actSave: TAction;
    ActionList: TActionList;
    edtFileNameLeft: TFileNameEdit;
    edtFileNameRight: TFileNameEdit;
    MainMenu: TMainMenu;
    miAutoCompare: TMenuItem;
    miDivider10: TMenuItem;
    miDivider11: TMenuItem;
    miLineDifferences: TMenuItem;
    miEncodingRight: TMenuItem;
    miEncodingLeft: TMenuItem;
    miAbout: TMenuItem;
    mnuEncoding: TMenuItem;
    miSaveRightAs: TMenuItem;
    miSaveLeftAs: TMenuItem;
    miCopyContext: TMenuItem;
    miCutContext: TMenuItem;
    miDeleteContext: TMenuItem;
    miFind: TMenuItem;
    miFindNext: TMenuItem;
    miFindPrevious: TMenuItem;
    miFindReplace: TMenuItem;
    miGotoLine: TMenuItem;
    miEditSelectAll: TMenuItem;
    miEditDelete: TMenuItem;
    miEditPaste: TMenuItem;
    miEditCopy: TMenuItem;
    miEditCut: TMenuItem;
    miDivider8: TMenuItem;
    miEditRedo: TMenuItem;
    miEditUndo: TMenuItem;
    miDivider7: TMenuItem;
    miPasteContext: TMenuItem;
    miReload: TMenuItem;
    miDivider6: TMenuItem;
    miExit: TMenuItem;
    miSaveRight: TMenuItem;
    miOpenRight: TMenuItem;
    miOpenLeft: TMenuItem;
    miCopyRightToLeft: TMenuItem;
    miCopyLeftToRight: TMenuItem;
    miDivider5: TMenuItem;
    miPaintBackground: TMenuItem;
    miDivider4: TMenuItem;
    miBinaryCompare: TMenuItem;
    miKeepScrolling: TMenuItem;
    miDivider3: TMenuItem;
    miLastDiff: TMenuItem;
    miFirstDiff: TMenuItem;
    miDivider2: TMenuItem;
    miPrevDiff: TMenuItem;
    miNextDiff: TMenuItem;
    miDivider1: TMenuItem;
    miCancelCompare: TMenuItem;
    miSelectAllContext: TMenuItem;
    miSeparator1: TMenuItem;
    miSeparator2: TMenuItem;
    miStartCompare: TMenuItem;
    miUndoContext: TMenuItem;
    mnuActions: TMenuItem;
    miIgnoreCase: TMenuItem;
    miIgnoreWhiteSpace: TMenuItem;
    mnuOptions: TMenuItem;
    mnuEdit: TMenuItem;
    miSaveLeft: TMenuItem;
    mnuFile: TMenuItem;
    ContextMenu: TPopupMenu;
    pnlLeftBox: TPanel;
    pnlRight: TPanel;
    pnlLeft: TPanel;
    pnlRightBox: TPanel;
    btnLeftEncoding: TSpeedButton;
    btnRightEncoding: TSpeedButton;
    btnLeftSave: TSpeedButton;
    btnLeftSaveAs: TSpeedButton;
    btnRightSave: TSpeedButton;
    btnRightSaveAs: TSpeedButton;
    pmEncodingLeft: TPopupMenu;
    pmEncodingRight: TPopupMenu;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    tmProgress: TTimer;
    ToolBar: TToolBarAdv;
    btnSave: TToolButton;
    btnSaveAs: TToolButton;
    Divider1: TToolButton;
    btnCompare: TToolButton;
    btnLast: TToolButton;
    btnNext: TToolButton;
    btnPrev: TToolButton;
    btnFirst: TToolButton;
    Divider2: TToolButton;
    Divider3: TToolButton;
    btnCancelCompare: TToolButton;
    Divider4: TToolButton;
    btnReload: TToolButton;
    btnCopyRightToLeft: TToolButton;
    btnCopyLeftToRight: TToolButton;
    Divider5: TToolButton;
    btnEditUndo: TToolButton;
    btnEditRedo: TToolButton;
    procedure actAboutExecute(Sender: TObject);
    procedure actBinaryCompareExecute(Sender: TObject);
    procedure actCancelCompareExecute(Sender: TObject);
    procedure actExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actIgnoreCaseExecute(Sender: TObject);
    procedure actLineDifferencesExecute(Sender: TObject);
    procedure actOpenLeftExecute(Sender: TObject);
    procedure actOpenRightExecute(Sender: TObject);
    procedure actPaintBackgroundExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveLeftAsExecute(Sender: TObject);
    procedure actSaveRightAsExecute(Sender: TObject);
    procedure actStartCompareExecute(Sender: TObject);
    procedure actKeepScrollingExecute(Sender: TObject);
    procedure btnLeftEncodingClick(Sender: TObject);
    procedure btnRightEncodingClick(Sender: TObject);
    procedure edtFileNameLeftAcceptFileName(Sender: TObject; var Value: String);
    procedure edtFileNameRightAcceptFileName(Sender: TObject; var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tmProgressTimer(Sender: TObject);
  private
    BinaryDiffList: TFPList;
    BinaryDiffIndex: Integer;
    BinaryCompare: TBinaryCompare;
    BinaryViewerLeft,
    BinaryViewerRight: TBinaryDiffViewer;
    procedure BinaryCompareFinish;
  private
    Diff: TDiff;
    SynDiffEditActive: TSynDiffEdit;
    SynDiffEditLeft: TSynDiffEdit;
    SynDiffEditRight: TSynDiffEdit;
    SynDiffHighlighterLeft,
    SynDiffHighlighterRight: TSynDiffHighlighter;
    HashListLeft,
    HashListRight: array of Integer;
    EncodingList: TStringList;
    ScrollLock: LongInt;
    FShowIdentical: Boolean;
    FModal: Boolean;
    FCancel: Boolean;
    frmProgress: TForm;
    FWaitData: TWaitData;
    FElevate: TDuplicates;
    FCommands: TFormCommands;
    FLeftLen, FRightLen: Integer;
    FSearchOptions: TEditSearchOptions;
private
    procedure ShowDialog;
    procedure ShowIdentical;
    procedure ShowTextIdentical;
    procedure ShowProgressDialog;
    procedure CloseProgressDialog;
    procedure Clear(bLeft, bRight: Boolean);
    procedure BuildHashList(bLeft, bRight: Boolean);
    procedure ChooseEncoding(SynDiffEdit: TSynDiffEdit);
    function GetDisplayNumber(LineNumber: Integer): Integer;
    procedure SetColors(cAdded, cDeleted, cModified: TColor);
    procedure ChooseEncoding(MenuItem: TMenuItem; Encoding: String);
    procedure FillEncodingMenu(TheOwner: TMenuItem; MenuHandler: TNotifyEvent; GroupIndex: LongInt);
    procedure LoadFromFile(SynDiffEdit: TSynDiffEdit; const FileName: String);
    procedure SaveToFile(SynDiffEdit: TSynDiffEdit; const FileName: String);
    procedure OpenFileLeft(const FileName: String);
    procedure OpenFileRight(const FileName: String);
    procedure SetEncodingLeft(Sender: TObject);
    procedure SetEncodingRight(Sender: TObject);
    procedure SynDiffEditEnter(Sender: TObject);
    procedure ShowFirstDifference(Data: PtrInt);
    procedure SynDiffEditLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynDiffEditRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);

    property Commands: TFormCommands read FCommands implements IFormCommands;
  protected
    procedure CMThemeChanged(var Message: TLMessage); message CM_THEMECHANGED;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    procedure cm_CopyLeftToRight(const Params: array of string);
    procedure cm_CopyRightToLeft(const Params: array of string);
    procedure cm_Find(const Params: array of string);
    procedure cm_FindNext(const Params: array of string);
    procedure cm_FindPrev(const Params: array of string);
    procedure cm_FindReplace(const Params: array of string);
    procedure cm_GotoLine(const Params: array of string);
    procedure cm_Exit(const Params: array of string);
    procedure cm_FirstDifference(const Params: array of string);
    procedure cm_LastDifference(const Params: array of string);
    procedure cm_NextDifference(const Params: array of string);
    procedure cm_PrevDifference(const Params: array of string);
    procedure cm_Reload(const Params: array of string);
    procedure cm_SaveLeft(const Params: array of string);
    procedure cm_SaveRight(const Params: array of string);
  end; 

procedure ShowDiffer(const FileNameLeft, FileNameRight: String; WaitData: TWaitData = nil; Modal: Boolean = False);

implementation

{$R *.lfm}

uses
  Math, LCLType, LazFileUtils, LConvEncoding, SynEditTypes, uHash, uLng, uGlobs,
  uShowMsg, DCClassesUtf8, dmCommonData, uDCUtils, uConvEncoding, uAdministrator,
  LCLStrConsts, uFileProcs;

const
  HotkeysCategory = 'Differ';

procedure ShowDiffer(const FileNameLeft, FileNameRight: String; WaitData: TWaitData = nil; Modal: Boolean = False);
var
  Binary: Boolean;
  Differ: TfrmDiffer;
begin
  Differ := TfrmDiffer.Create(Application);
  with Differ do
  begin
    FModal := Modal;
    FWaitData := WaitData;
    FShowIdentical := True;
    edtFileNameLeft.Text:= FileNameLeft;
    edtFileNameRight.Text:= FileNameRight;
    try
      PushPop(FElevate);
      try
        Binary:= not (mbFileIsText(FileNameLeft) and mbFileIsText(FileNameRight));
      finally
        PushPop(FElevate);
      end;
      if Binary then
        actBinaryCompare.Execute
      else begin
        OpenFileLeft(FileNameLeft);
        OpenFileRight(FileNameRight);
        if actStartCompare.Enabled then
          actStartCompare.Execute
        else begin
          tmProgress.Enabled:= False;
          CloseProgressDialog;
          ShowDialog;
        end;
      end;
    except
      on E: Exception do
      begin
        tmProgress.Enabled:= False;
        CloseProgressDialog;
        msgError(E.Message);
        Free;
      end;
    end;
  end;
end;

{ TfrmDiffer }

procedure TfrmDiffer.actStartCompareExecute(Sender: TObject);
var
  I: Integer;
  LineNumberLeft,
  LineNumberRight: PtrInt;
begin
  FCancel:= False;
  try
    if actBinaryCompare.Checked then
    begin
      if (BinaryViewerLeft.IsFileOpen and BinaryViewerRight.IsFileOpen) then
      begin
        actStartCompare.Enabled := False;
        actCancelCompare.Enabled := True;
        actBinaryCompare.Enabled := False;
        BinaryCompare:= TBinaryCompare.Create(BinaryViewerLeft.GetDataAdr,
                                              BinaryViewerRight.GetDataAdr,
                                              BinaryViewerLeft.FileSize,
                                              BinaryViewerRight.FileSize,
                                              BinaryDiffList);

        BinaryCompare.OnFinish:= @BinaryCompareFinish;
        BinaryCompare.Start;
      end;
    end
    else begin
      Inc(ScrollLock);
      Screen.BeginWaitCursor;
      try
        if SynDiffEditLeft.Modified then SynDiffEditLeft.Lines.RemoveFake;
        if SynDiffEditRight.Modified then SynDiffEditRight.Lines.RemoveFake;
        BuildHashList(SynDiffEditLeft.Modified, SynDiffEditRight.Modified);

        if (Length(HashListLeft) = 0) or (Length(HashListRight) = 0) then
        begin
          FCancel := True;
          Exit;
        end;

        actStartCompare.Enabled := False;
        actCancelCompare.Enabled := True;

        Diff.Execute(
                     PInteger(@HashListLeft[0]),
                     PInteger(@HashListRight[0]),
                     Length(HashListLeft),
                     Length(HashListRight)
                    );

        tmProgress.Enabled:= False;
        if Diff.Cancelled then Exit;

        SynDiffEditLeft.StartCompare;
        SynDiffEditRight.StartCompare;

        for I := 0 to Diff.Count - 1 do
        with Diff.Compares[I] do
        begin
          LineNumberLeft:= oldIndex1 + 1;
          LineNumberRight:= oldIndex2 + 1;
          case Kind of
          ckAdd:
            begin
              SynDiffEditLeft.Lines.InsertFake(I, Kind);
              SynDiffEditRight.Lines.SetKindAndNumber(I, Kind, LineNumberRight);
            end;
          ckDelete:
            begin
              SynDiffEditLeft.Lines.SetKindAndNumber(I, Kind, LineNumberLeft);
              SynDiffEditRight.Lines.InsertFake(I, Kind);
            end;
          else
            begin
              SynDiffEditLeft.Lines.SetKindAndNumber(I, Kind, LineNumberLeft);
              SynDiffEditRight.Lines.SetKindAndNumber(I, Kind, LineNumberRight);
            end;
          end;
        end;
      finally
        SynDiffEditLeft.FinishCompare;
        SynDiffEditRight.FinishCompare;
        actStartCompare.Enabled := True;
        actCancelCompare.Enabled := False;
        Screen.EndWaitCursor;
        Dec(ScrollLock);
      end;
      if actLineDifferences.Checked then
      begin
        SynDiffEditLeft.Highlighter:= SynDiffHighlighterLeft;
        SynDiffEditRight.Highlighter:= SynDiffHighlighterRight;
      end;
      with Diff.DiffStats do
      begin
        StatusBar.Panels[0].Text := rsDiffMatches + IntToStr(matches);
        StatusBar.Panels[1].Text := rsDiffModifies + IntToStr(modifies);
        StatusBar.Panels[2].Text := rsDiffAdds + IntToStr(adds);
        StatusBar.Panels[3].Text := rsDiffDeletes + IntToStr(deletes);
        if FShowIdentical then
        begin
          CloseProgressDialog;
          FShowIdentical:= (modifies = 0) and (adds = 0) and (deletes = 0);
          if FShowIdentical then
            ShowIdentical
          else begin
            FShowIdentical:= False;
            Application.QueueAsyncCall(@ShowFirstDifference, 0);
            ShowDialog;
          end;
        end
        else if (modifies = 0) and (adds = 0) and (deletes = 0) then
        begin
          if (SynDiffEditLeft.Encoding <> SynDiffEditRight.Encoding) or
             (SynDiffEditLeft.Lines.TextLineBreakStyle <> SynDiffEditRight.Lines.TextLineBreakStyle) then
          begin
            ShowTextIdentical;
          end;
        end;
      end;
    end;
  finally
    if FShowIdentical and FCancel then Free;
  end;
end;

procedure TfrmDiffer.actOpenLeftExecute(Sender: TObject);
begin
  dmComData.OpenDialog.FileName:= edtFileNameLeft.Text;
  dmComData.OpenDialog.Filter:= AllFilesMask;
  if dmComData.OpenDialog.Execute then
  begin
    edtFileNameLeft.Text:= dmComData.OpenDialog.FileName;
    actReload.Execute;
  end;
end;

procedure TfrmDiffer.actOpenRightExecute(Sender: TObject);
begin
  dmComData.OpenDialog.FileName:= edtFileNameRight.Text;
  dmComData.OpenDialog.Filter:= AllFilesMask;
  if dmComData.OpenDialog.Execute then
  begin
    edtFileNameRight.Text:= dmComData.OpenDialog.FileName;
    actReload.Execute;
  end;
end;

procedure TfrmDiffer.actPaintBackgroundExecute(Sender: TObject);
begin
  if actPaintBackground.Checked then
    begin
      SynDiffEditLeft.PaintStyle:= psBackground;
      SynDiffEditRight.PaintStyle:= psBackground;
    end
  else
    begin
      SynDiffEditLeft.PaintStyle:= psForeground;
      SynDiffEditRight.PaintStyle:= psForeground;
    end;
  SynDiffHighlighterLeft.UpdateColors;
  SynDiffHighlighterRight.UpdateColors;
end;

procedure TfrmDiffer.actSaveAsExecute(Sender: TObject);
begin
  if SynDiffEditActive = SynDiffEditLeft then
    actSaveLeftAs.Execute
  else if SynDiffEditActive = SynDiffEditRight then
    actSaveRightAs.Execute;
end;

procedure TfrmDiffer.actSaveExecute(Sender: TObject);
begin
  if SynDiffEditActive = SynDiffEditLeft then
    actSaveLeft.Execute
  else if SynDiffEditActive = SynDiffEditRight then
    actSaveRight.Execute;
end;

procedure TfrmDiffer.actSaveLeftAsExecute(Sender: TObject);
begin
  dmComData.SaveDialog.FileName:= edtFileNameLeft.FileName;
  if dmComData.SaveDialog.Execute then
  begin
    PushPop(FElevate);
    try
      SaveToFile(SynDiffEditLeft, dmComData.SaveDialog.FileName);
    finally
      PushPop(FElevate);
    end;
    edtFileNameLeft.FileName:= dmComData.SaveDialog.FileName;
  end;
end;

procedure TfrmDiffer.actSaveRightAsExecute(Sender: TObject);
begin
  dmComData.SaveDialog.FileName:= edtFileNameRight.FileName;
  if dmComData.SaveDialog.Execute then
  begin
    PushPop(FElevate);
    try
      SaveToFile(SynDiffEditRight, dmComData.SaveDialog.FileName);
    finally
      PushPop(FElevate);
    end;
    edtFileNameRight.FileName:= dmComData.SaveDialog.FileName;
  end;
end;

procedure TfrmDiffer.actBinaryCompareExecute(Sender: TObject);
begin
  mnuEdit.Enabled:= not actBinaryCompare.Checked;
  mnuEncoding.Enabled:= not actBinaryCompare.Checked;
  btnLeftEncoding.Enabled:= not actBinaryCompare.Checked;
  btnRightEncoding.Enabled:= not actBinaryCompare.Checked;
  actCopyLeftToRight.Enabled:= not actBinaryCompare.Checked;
  actCopyRightToLeft.Enabled:= not actBinaryCompare.Checked;
  actEditUndo.Enabled:= not actBinaryCompare.Checked;
  actEditRedo.Enabled:= not actBinaryCompare.Checked;
  actFind.Enabled:= not actBinaryCompare.Checked;
  actFindNext.Enabled:= not actBinaryCompare.Checked;
  actFindPrev.Enabled:= not actBinaryCompare.Checked;
  actFindReplace.Enabled:= not actBinaryCompare.Checked;
  actGotoLine.Enabled:= not actBinaryCompare.Checked;
  actSave.Enabled:= not actBinaryCompare.Checked;
  actSaveAs.Enabled:= not actBinaryCompare.Checked;
  actSaveLeft.Enabled:= not actBinaryCompare.Checked;
  actSaveLeftAs.Enabled:= not actBinaryCompare.Checked;
  actSaveRight.Enabled:= not actBinaryCompare.Checked;
  actSaveRightAs.Enabled:= not actBinaryCompare.Checked;
  actIgnoreCase.Enabled:= not actBinaryCompare.Checked;
  actIgnoreWhiteSpace.Enabled:= not actBinaryCompare.Checked;
  actPaintBackground.Enabled:= not actBinaryCompare.Checked;
  actLineDifferences.Enabled:= not actBinaryCompare.Checked;

  SynDiffEditLeft.Visible:= not actBinaryCompare.Checked;
  SynDiffEditRight.Visible:= not actBinaryCompare.Checked;
  BinaryViewerLeft.Visible:= actBinaryCompare.Checked;
  BinaryViewerRight.Visible:= actBinaryCompare.Checked;

  if actBinaryCompare.Checked then
    begin
      PushPop(FElevate);
      try
        BinaryDiffList.Clear;
        BinaryViewerLeft.FileName:= edtFileNameLeft.Text;
        BinaryViewerRight.FileName:= edtFileNameRight.Text;
      finally
        PushPop(FElevate);
      end;
      if FShowIdentical then
      begin
        if not BinaryViewerLeft.IsFileOpen then
          raise EFOpenError.Create(BinaryViewerLeft.LastError + LineEnding + edtFileNameLeft.Text);
        if not BinaryViewerRight.IsFileOpen then
          raise EFOpenError.Create(BinaryViewerRight.LastError + LineEnding + edtFileNameRight.Text);
      end;
      StatusBar.Panels[0].Text := EmptyStr;
      StatusBar.Panels[1].Text := EmptyStr;
      StatusBar.Panels[2].Text := EmptyStr;
      StatusBar.Panels[3].Text := EmptyStr;
    end
  else
    begin
      BinaryViewerLeft.FileName:= EmptyStr;
      BinaryViewerRight.FileName:= EmptyStr;
      OpenFileLeft(edtFileNameLeft.Text);
      OpenFileRight(edtFileNameRight.Text);
    end;

  actStartCompare.Execute;
end;

procedure TfrmDiffer.actCancelCompareExecute(Sender: TObject);
begin
  if not actBinaryCompare.Checked then
    Diff.Cancel
  else begin
    if Assigned(BinaryCompare) then
    begin
      BinaryCompare.Terminate;
      BinaryCompare:= nil;
    end;
  end;
end;

procedure TfrmDiffer.actAboutExecute(Sender: TObject);
begin
  ShowMessage('Internal Differ tool of Double Commander.' + LineEnding + LineEnding +
              'It is inspired by Flavio Etrusco''s Pariter tool.' + LineEnding +
              'You can find it on: http://sourceforge.net/projects/pariter' + LineEnding +
              'It is based on Angus Johnson''s excellent TDiff component.' + LineEnding +
              'You can find it on: http://www.users.on.net/johnson/delphi');
end;

procedure TfrmDiffer.actEditCopyExecute(Sender: TObject);
begin
  SynDiffEditActive.CopyToClipboard;
end;

procedure TfrmDiffer.actEditCutExecute(Sender: TObject);
begin
  SynDiffEditActive.CutToClipboard;
end;

procedure TfrmDiffer.actEditDeleteExecute(Sender: TObject);
begin
  SynDiffEditActive.ClearSelection;
end;

procedure TfrmDiffer.actEditPasteExecute(Sender: TObject);
begin
  SynDiffEditActive.PasteFromClipboard;
end;

procedure TfrmDiffer.actEditRedoExecute(Sender: TObject);
begin
  SynDiffEditActive.Redo;
end;

procedure TfrmDiffer.actEditSelectAllExecute(Sender: TObject);
begin
  SynDiffEditActive.SelectAll;
end;

procedure TfrmDiffer.actEditUndoExecute(Sender: TObject);
begin
  SynDiffEditActive.Undo;
end;

procedure TfrmDiffer.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.ExecuteCommand(cmd, []);
end;

procedure TfrmDiffer.actIgnoreCaseExecute(Sender: TObject);
begin
  if actAutoCompare.Checked then actStartCompare.Execute;
end;

procedure TfrmDiffer.actLineDifferencesExecute(Sender: TObject);
begin
  if actLineDifferences.Checked and (Diff.Count <> 0) then
    begin
      SynDiffEditLeft.Highlighter:= SynDiffHighlighterLeft;
      SynDiffEditRight.Highlighter:= SynDiffHighlighterRight;
    end
  else
    begin
      SynDiffEditLeft.Highlighter:= nil;
      SynDiffEditRight.Highlighter:= nil;
    end;
  SynDiffEditLeft.Repaint;
  SynDiffEditRight.Repaint;
end;

procedure TfrmDiffer.actKeepScrollingExecute(Sender: TObject);
begin
  BinaryViewerLeft.KeepScrolling:= actKeepScrolling.Checked;
  BinaryViewerRight.KeepScrolling:= actKeepScrolling.Checked;
end;

procedure TfrmDiffer.btnLeftEncodingClick(Sender: TObject);
begin
  pmEncodingLeft.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmDiffer.btnRightEncodingClick(Sender: TObject);
begin
  pmEncodingRight.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmDiffer.edtFileNameLeftAcceptFileName(Sender: TObject;
  var Value: String);
begin
  OpenFileLeft(Value);
  if actAutoCompare.Checked then actStartCompare.Execute;
end;

procedure TfrmDiffer.edtFileNameRightAcceptFileName(Sender: TObject;
  var Value: String);
begin
  OpenFileRight(Value);
  if actAutoCompare.Checked then actStartCompare.Execute;
end;

procedure TfrmDiffer.FormCreate(Sender: TObject);
begin
  ScrollLock:= 0;
  Diff:= TDiff.Create(Self);
  SynDiffEditLeft:= TSynDiffEdit.Create(Self);
  SynDiffEditRight:= TSynDiffEdit.Create(Self);
  SynDiffHighlighterLeft:= TSynDiffHighlighter.Create(SynDiffEditLeft);
  SynDiffHighlighterRight:= TSynDiffHighlighter.Create(SynDiffEditRight);

  SynDiffEditLeft.Parent:= pnlLeft;
  SynDiffEditRight.Parent:= pnlRight;
  SynDiffEditLeft.Align:= alClient;
  SynDiffEditRight.Align:= alClient;
  SynDiffEditLeft.PopupMenu:= ContextMenu;
  SynDiffEditRight.PopupMenu:= ContextMenu;

  SynDiffEditLeft.ModifiedFile:= SynDiffEditRight;
  SynDiffEditRight.OriginalFile:= SynDiffEditLeft;

  SynDiffEditLeft.OnEnter:= @SynDiffEditEnter;
  SynDiffEditRight.OnEnter:= @SynDiffEditEnter;
  SynDiffEditLeft.OnStatusChange:= @SynDiffEditLeftStatusChange;
  SynDiffEditRight.OnStatusChange:= @SynDiffEditRightStatusChange;
  // Set active editor
  SynDiffEditActive:= SynDiffEditLeft;

  BinaryDiffList:= TFPList.Create;
  BinaryViewerLeft:= TBinaryDiffViewer.Create(Self);
  BinaryViewerRight:= TBinaryDiffViewer.Create(Self);

  BinaryViewerLeft.OnFileOpen:= @FileOpenUAC;
  BinaryViewerRight.OnFileOpen:= @FileOpenUAC;

  BinaryViewerLeft.Visible:= False;
  BinaryViewerRight.Visible:= False;
  BinaryViewerLeft.Parent:= pnlLeft;
  BinaryViewerRight.Parent:= pnlRight;
  BinaryViewerLeft.Align:= alClient;
  BinaryViewerRight.Align:= alClient;

  BinaryViewerLeft.SecondViewer:= BinaryViewerRight;
  BinaryViewerRight.SecondViewer:= BinaryViewerLeft;

  with gColors.Differ^ do
  begin
    BinaryViewerLeft.Modified:= ModifiedBinaryColor;
    BinaryViewerRight.Modified:= ModifiedBinaryColor;
    SetColors(AddedColor, DeletedColor, ModifiedColor);
  end;

  FontOptionsToFont(gFonts[dcfEditor], SynDiffEditLeft.Font);
  FontOptionsToFont(gFonts[dcfEditor], SynDiffEditRight.Font);
  FontOptionsToFont(gFonts[dcfViewer], BinaryViewerLeft.Font);
  FontOptionsToFont(gFonts[dcfViewer], BinaryViewerRight.Font);

  // Load settings
  actIgnoreCase.Checked := gDifferIgnoreCase;
  actAutoCompare.Checked := gDifferAutoCompare;
  actKeepScrolling.Checked := gDifferKeepScrolling;
  actLineDifferences.Checked := gDifferLineDifferences;
  actPaintBackground.Checked := gDifferPaintBackground;
  actIgnoreWhiteSpace.Checked := gDifferIgnoreWhiteSpace;

  // Initialize mode
  actKeepScrollingExecute(actKeepScrolling);
  actPaintBackgroundExecute(actPaintBackground);

  // Initialize property storage
  InitPropStorage(Self);

  // Fill encoding menu
  EncodingList:= TStringList.Create;
  GetSupportedEncodings(EncodingList);
  FillEncodingMenu(miEncodingLeft, @SetEncodingLeft, 1);
  FillEncodingMenu(miEncodingRight, @SetEncodingRight, 2);
  FillEncodingMenu(pmEncodingLeft.Items, @SetEncodingLeft, 1);
  FillEncodingMenu(pmEncodingRight.Items, @SetEncodingRight, 2);
  EncodingList.Free;
end;

procedure TfrmDiffer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Diff);
  FreeAndNil(BinaryDiffList);
end;

procedure TfrmDiffer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key:= 0;
    Close;
  end;
end;

procedure TfrmDiffer.FormResize(Sender: TObject);
begin
  pnlLeft.Width:= (ClientWidth div 2) - (Splitter.Width div 2);
end;

procedure TfrmDiffer.btnCancelClick(Sender: TObject);
begin
  FCancel:= True;
  if actBinaryCompare.Checked and Assigned(BinaryCompare) then
    BinaryCompare.Terminate
  else begin
    Diff.Cancel;
  end;
  CloseProgressDialog;
end;

procedure TfrmDiffer.tmProgressTimer(Sender: TObject);
begin
  tmProgress.Enabled:= False;
  ShowProgressDialog;
end;

procedure TfrmDiffer.BinaryCompareFinish;
begin
  BinaryCompare:= nil;
  if FCancel then begin
    if FShowIdentical then Free;
    Exit;
  end;
  BinaryDiffIndex:= -1;
  tmProgress.Enabled:= False;
  StatusBar.Panels[0].Text := EmptyStr;
  StatusBar.Panels[1].Text := rsDiffModifies + IntToStr(BinaryDiffList.Count);
  StatusBar.Panels[2].Text := EmptyStr;
  StatusBar.Panels[3].Text := EmptyStr;
  actStartCompare.Enabled := True;
  actCancelCompare.Enabled := False;
  actBinaryCompare.Enabled := True;
  if FShowIdentical then
  begin
    CloseProgressDialog;
    FShowIdentical:= (BinaryDiffList.Count = 0);
    if FShowIdentical then
      ShowIdentical
    else begin
      FShowIdentical:= False;
      Application.QueueAsyncCall(@ShowFirstDifference, 0);
      ShowDialog;
    end;
  end;
end;

procedure TfrmDiffer.ShowDialog;
begin
  if FModal then
    ShowModal
  else if (FWaitData = nil) then
    ShowOnTop
  else
    FWaitData.ShowOnTop(Self);
end;

procedure TfrmDiffer.ShowIdentical;
var
  Message: String;
  Encoding, LineBreak: Boolean;
  DlgType: TMsgDlgType = mtInformation;
begin
  Message:= rsDiffFilesIdentical + LineEnding + LineEnding;
  Message+= ReplaceHome(edtFileNameLeft.Text) + LineEnding + ReplaceHome(edtFileNameRight.Text);
  if not actBinaryCompare.Checked then
  begin
    Encoding:= (SynDiffEditLeft.Encoding <> SynDiffEditRight.Encoding);
    LineBreak:= (SynDiffEditLeft.Lines.TextLineBreakStyle <> SynDiffEditRight.Lines.TextLineBreakStyle);
    if Encoding or LineBreak then
    begin
      DlgType:= mtWarning;
      Message:= rsDiffTextIdenticalNotMatch;
      if Encoding then begin
        Message+= LineEnding + rsDiffTextDifferenceEncoding +
                  Format(' (%s, %s)', [SynDiffEditLeft.Encoding, SynDiffEditRight.Encoding]);
      end;
      if LineBreak then begin
        Message+= LineEnding + rsDiffTextDifferenceLineEnding;
      end;
    end
    else if actIgnoreCase.Checked or actIgnoreWhiteSpace.Checked then
    begin
      DlgType:= mtWarning;
      Message:= rsDiffTextIdentical;
      if actIgnoreCase.Checked then begin
        Message+= LineEnding + actIgnoreCase.Caption;
      end;
      if actIgnoreWhiteSpace.Checked then begin
        Message+= LineEnding + actIgnoreWhiteSpace.Caption;
      end;
    end;
  end;
  if MsgChoiceBox(Message, rsToolDiffer, DlgType, [rsMbClose, rsDiffShow], 0, 1) = 0 then
    Close
  else begin
    FShowIdentical:= False;
    ShowDialog;
  end;
end;

procedure TfrmDiffer.ShowTextIdentical;
var
  Message: String;
begin
  Message:= rsDiffTextIdenticalNotMatch;
  if (SynDiffEditLeft.Encoding <> SynDiffEditRight.Encoding) then
    Message+= LineEnding + rsDiffTextDifferenceEncoding;
  if (SynDiffEditLeft.Lines.TextLineBreakStyle <> SynDiffEditRight.Lines.TextLineBreakStyle) then
    Message+= LineEnding + rsDiffTextDifferenceLineEnding;
  MessageDlg(rsToolDiffer, Message, mtWarning, [mbOK], 0, mbOK);
end;

procedure TfrmDiffer.ShowProgressDialog;
var
  lblPrompt : TLabel;
  btnCancel : TBitBtn;
  pbProgress: TProgressBar;
begin
  frmProgress := TModalDialog.CreateNew(nil, 0);
  with frmProgress do
  begin
    BorderStyle := bsDialog;
    Position := poOwnerFormCenter;
    AutoSize := True;
    Height := 120;
    ChildSizing.TopBottomSpacing := 8;
    ChildSizing.LeftRightSpacing := 8;
    Caption := Self.Caption;
    lblPrompt := TLabel.Create(frmProgress);
    with lblPrompt do
    begin
      Parent := frmProgress;
      Caption := rsDiffComparing;
      Top := 6;
      Left := 6;
    end;
    pbProgress:= TProgressBar.Create(frmProgress);
    with pbProgress do
    begin
      Parent := frmProgress;
      Style:= pbstMarquee;
      Left := 6;
      AnchorToNeighbour(akTop, 6, lblPrompt);
      Constraints.MinWidth := Math.Max(280, Screen.Width div 4);
    end;
    btnCancel := TBitBtn.Create(frmProgress);
    with btnCancel do
    begin
      AutoSize := True;
      Parent := frmProgress;
      Kind := bkCancel;
      Cancel := True;
      OnClick:= @btnCancelClick;
      Anchors := [akTop, akRight];
      AnchorToNeighbour(akTop, 18, pbProgress);
      AnchorSide[akRight].Control := pbProgress;
      AnchorSide[akRight].Side := asrCenter;
    end;
    if FModal then
      ShowModal
    else if (FWaitData = nil) then
      ShowOnTop
    else
      FWaitData.ShowOnTop(frmProgress);
  end;
end;

procedure TfrmDiffer.CloseProgressDialog;
begin
  if Assigned(frmProgress) then
  begin
    frmProgress.Close;
    FreeAndNil(frmProgress);
  end;
end;

procedure TfrmDiffer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
  // Save settings
  gDifferIgnoreCase := actIgnoreCase.Checked;
  gDifferAutoCompare := actAutoCompare.Checked;
  gDifferKeepScrolling := actKeepScrolling.Checked;
  gDifferPaintBackground := actPaintBackground.Checked;
  gDifferIgnoreWhiteSpace := actIgnoreWhiteSpace.Checked;
  if actLineDifferences.Enabled then begin
    gDifferLineDifferences := actLineDifferences.Checked;
  end;
end;

procedure TfrmDiffer.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Result: TMyMsgResult;
begin
  if SynDiffEditLeft.Modified then
  begin
    Result:= msgYesNoCancel(Format(rsMsgFileChangedSave, [edtFileNameLeft.FileName]));
    CanClose:= Result <> mmrCancel;
    if Result = mmrYes then
      actSaveLeft.Execute
    else if Result = mmrCancel then
      Exit;
  end;
  if SynDiffEditRight.Modified then
  begin
    Result:= msgYesNoCancel(Format(rsMsgFileChangedSave, [edtFileNameRight.FileName]));
    CanClose:= Result <> mmrCancel;
    if Result = mmrYes then
      actSaveRight.Execute
    else if Result = mmrCancel then
      Exit;
  end;
end;

procedure TfrmDiffer.Clear(bLeft, bRight: Boolean);
begin
  if bLeft then
  begin
    SynDiffEditLeft.Lines.Clear;
    SetLength(HashListLeft, 0);
  end;
  if bRight then
  begin
    SynDiffEditRight.Lines.Clear;
    SetLength(HashListRight, 0);
  end;
  Diff.Clear;
  StatusBar.Panels[0].Text := EmptyStr;
  StatusBar.Panels[1].Text := EmptyStr;
  StatusBar.Panels[2].Text := EmptyStr;
  StatusBar.Panels[3].Text := EmptyStr;
  actStartCompare.Enabled := True;
end;

procedure TfrmDiffer.cm_CopyLeftToRight(const Params: array of string);
var
  I, iStart,
  iFinish: Integer;
begin
  I := SynDiffEditLeft.CaretY - 1;
  iStart:= SynDiffEditLeft.DiffBegin(I);
  iFinish:= SynDiffEditLeft.DiffEnd(I);
  if SynDiffEditLeft.Lines.Kind[iStart] <> ckAdd then
  begin
    for I:= iStart to iFinish do
    begin
      SynDiffEditRight.Lines[I]:= SynDiffEditLeft.Lines[I];
      if SynDiffEditLeft.Lines.Kind[I] = ckDelete then
      begin
        SynDiffEditLeft.Lines.Kind[I]:= ckNone;
        SynDiffEditRight.Lines.SetKindAndNumber(I, ckNone, 0);
      end;
    end;
  end
  else begin
    for I:= iStart to iFinish do
    begin
      if SynDiffEditLeft.Lines[iStart] <> EmptyStr then
      begin
        SynDiffEditRight.Lines[iStart]:= SynDiffEditLeft.Lines[iStart];
        Inc(iStart);
      end
      else begin
        SynDiffEditLeft.Lines.Delete(iStart);
        SynDiffEditRight.Lines.Delete(iStart);
      end;
    end;
  end;
  SynDiffEditLeft.Renumber;
  SynDiffEditRight.Renumber;
end;

procedure TfrmDiffer.cm_CopyRightToLeft(const Params: array of string);
var
  I, iStart,
  iFinish: Integer;
begin
  I := SynDiffEditRight.CaretY - 1;
  iStart:= SynDiffEditRight.DiffBegin(I);
  iFinish:= SynDiffEditRight.DiffEnd(I);
  if SynDiffEditLeft.Lines.Kind[iStart] <> ckDelete then
  begin
    for I:= iStart to iFinish do
    begin
      SynDiffEditLeft.Lines[I]:= SynDiffEditRight.Lines[I];
      if SynDiffEditRight.Lines.Kind[I] = ckAdd then
      begin
        SynDiffEditRight.Lines.Kind[I]:= ckNone;
        SynDiffEditLeft.Lines.SetKindAndNumber(I, ckNone, 0);
      end;
    end;
  end
  else begin
    for I:= iStart to iFinish do
    begin
      if SynDiffEditRight.Lines[iStart] <> EmptyStr then
      begin
        SynDiffEditLeft.Lines[iStart]:= SynDiffEditRight.Lines[iStart];
        Inc(iStart);
      end
      else begin
        SynDiffEditLeft.Lines.Delete(iStart);
        SynDiffEditRight.Lines.Delete(iStart);
      end;
    end;
  end;
  SynDiffEditLeft.Renumber;
  SynDiffEditRight.Renumber;
end;

procedure TfrmDiffer.cm_Find(const Params: array of string);
begin
  if not actBinaryCompare.Checked then
  begin
    ShowSearchReplaceDialog(Self, SynDiffEditActive, cbUnchecked, FSearchOptions);
  end;
end;

procedure TfrmDiffer.cm_FindNext(const Params: array of string);
begin
  if not actBinaryCompare.Checked then
  begin
    if gFirstTextSearch then
    begin
      FSearchOptions.Flags -= [ssoBackwards];
      ShowSearchReplaceDialog(Self, SynDiffEditActive, cbUnchecked, FSearchOptions);
    end
    else if FSearchOptions.SearchText <> '' then
    begin
      DoSearchReplaceText(SynDiffEditActive, False, False, FSearchOptions);
      FSearchOptions.Flags -= [ssoEntireScope];
    end;
  end;
end;

procedure TfrmDiffer.cm_FindPrev(const Params: array of string);
begin
  if not actBinaryCompare.Checked then
  begin
    if gFirstTextSearch then
    begin
      FSearchOptions.Flags += [ssoBackwards];
      ShowSearchReplaceDialog(Self, SynDiffEditActive, cbUnchecked, FSearchOptions);
    end
    else if FSearchOptions.SearchText <> '' then
    begin
      SynDiffEditActive.SelEnd := SynDiffEditActive.SelStart;
      DoSearchReplaceText(SynDiffEditActive, False, True, FSearchOptions);
      FSearchOptions.Flags -= [ssoEntireScope];
    end;
  end;
end;

procedure TfrmDiffer.cm_FindReplace(const Params: array of string);
begin
  if not actBinaryCompare.Checked then
  begin
    ShowSearchReplaceDialog(Self, SynDiffEditActive, cbChecked, FSearchOptions);
  end;
end;

procedure TfrmDiffer.cm_GotoLine(const Params: array of string);
var
  P: TPoint;
  Value: String;
  NewTopLine: Integer;
begin
  if not actBinaryCompare.Checked then
  begin
    if ShowInputQuery(rsEditGotoLineTitle, rsEditGotoLineQuery, Value) then
    begin
      P.X := 1;
      P.Y := GetDisplayNumber(StrToIntDef(Value, 1));
      NewTopLine := P.Y - (SynDiffEditActive.LinesInWindow div 2);
      if NewTopLine < 1 then begin
        NewTopLine := 1;
      end;
      SynDiffEditActive.CaretXY := P;
      SynDiffEditActive.TopLine := NewTopLine;
      SynDiffEditActive.SetFocus;
    end;
  end;
end;

procedure TfrmDiffer.cm_Exit(const Params: array of string);
begin
  Close;
end;

procedure TfrmDiffer.cm_FirstDifference(const Params: array of string);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  if actBinaryCompare.Checked then
  begin
    if BinaryDiffList.Count > 0 then
    begin
      BinaryDiffIndex:= 0;
      BinaryViewerLeft.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
      if not actKeepScrolling.Checked then
        BinaryViewerRight.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
    end;
  end
  else begin
    // Start at first line
    Line := 0;
    if Line = SynDiffEditLeft.Lines.Count then Exit;
    // Skip unmodified lines
    Kind := ckNone;
    while (Line < SynDiffEditLeft.Lines.Count - 1) and
      (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Inc(Line);
    Inc(Line);
    SynDiffEditLeft.CaretY := Line;
    SynDiffEditLeft.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
    if not actKeepScrolling.Checked then begin
      SynDiffEditRight.TopLine := Line;
    end;
  end;
end;

procedure TfrmDiffer.cm_LastDifference(const Params: array of string);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  if actBinaryCompare.Checked then
  begin
    if BinaryDiffList.Count > 0 then
    begin
      BinaryDiffIndex:= BinaryDiffList.Count - 1;
      BinaryViewerLeft.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
      if not actKeepScrolling.Checked then
        BinaryViewerRight.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
    end;
  end
  else begin
    Line := SynDiffEditLeft.Lines.Count - 1;
    if Line = 0 then Exit;
    // Skip unmodified lines
    Kind := ckNone;
    while (Line > 0) and (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Dec(Line);
    // Find top line of previous difference
    Kind:= SynDiffEditLeft.Lines.Kind[Line];
    while (Line > 0) and (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Dec(Line);
    if (Line <> 0) then Inc(Line, 2);
    SynDiffEditLeft.CaretY := Line;
    SynDiffEditLeft.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
    if not actKeepScrolling.Checked then begin
      SynDiffEditRight.TopLine := Line;
    end;
  end;
end;

procedure TfrmDiffer.cm_NextDifference(const Params: array of string);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  if actBinaryCompare.Checked then
  begin
    if BinaryDiffIndex < BinaryDiffList.Count - 1 then
    begin
      BinaryDiffIndex:= BinaryDiffIndex + 1;
      BinaryViewerLeft.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
      if not actKeepScrolling.Checked then
        BinaryViewerRight.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
    end;
  end
  else begin
    Line := SynDiffEditLeft.CaretY - 1;
    if Line = SynDiffEditLeft.Lines.Count - 1 then Exit;
    // Skip lines with current difference type
    Kind := SynDiffEditLeft.Lines.Kind[Line];
    while (Line < SynDiffEditLeft.Lines.Count - 1) and
      (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Inc(Line);
    if SynDiffEditLeft.Lines.Kind[Line] = ckNone then
    begin
      // Skip unmodified lines
      Kind := ckNone;
      while (Line < SynDiffEditLeft.Lines.Count - 1) and
        (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Inc(Line);
    end;
    Inc(Line);
    SynDiffEditLeft.CaretY := Line;
    SynDiffEditLeft.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
    if not actKeepScrolling.Checked then begin
      SynDiffEditRight.TopLine := Line;
    end;
  end;
end;

procedure TfrmDiffer.cm_PrevDifference(const Params: array of string);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  if actBinaryCompare.Checked then
  begin
    if BinaryDiffIndex > 0 then
    begin
      BinaryDiffIndex:= BinaryDiffIndex - 1;
      BinaryViewerLeft.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
      if not actKeepScrolling.Checked then
        BinaryViewerRight.Position:= PtrInt(BinaryDiffList[BinaryDiffIndex]);
    end;
  end
  else begin
    Line := SynDiffEditLeft.CaretY - 1;
    if Line = 0 then Exit;
    // Skip lines with current difference type
    Kind := SynDiffEditLeft.Lines.Kind[Line];
    while (Line > 0) and (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Dec(Line);
    if SynDiffEditLeft.Lines.Kind[Line] = ckNone then
    begin
      // Skip unmodified lines
      Kind := ckNone;
      while (Line > 0) and (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Dec(Line);
    end;
    // Find top line of previous difference
    Kind:= SynDiffEditLeft.Lines.Kind[Line];
    while (Line > 0) and (SynDiffEditLeft.Lines.Kind[Line] = Kind) do Dec(Line);
    if (Line <> 0) then Inc(Line, 2);
    SynDiffEditLeft.CaretY := Line;
    SynDiffEditLeft.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
    if not actKeepScrolling.Checked then begin
      SynDiffEditRight.TopLine := Line;
    end;
  end;
end;

procedure TfrmDiffer.cm_Reload(const Params: array of string);
begin
  OpenFileLeft(edtFileNameLeft.FileName);
  OpenFileRight(edtFileNameRight.FileName);
  if actAutoCompare.Checked then actStartCompare.Execute;
end;

procedure TfrmDiffer.cm_SaveLeft(const Params: array of string);
begin
  PushPop(FElevate);
  try
    SaveToFile(SynDiffEditLeft, edtFileNameLeft.FileName);
  finally
    PushPop(FElevate);
  end;
end;

procedure TfrmDiffer.cm_SaveRight(const Params: array of string);
begin
  PushPop(FElevate);
  try
    SaveToFile(SynDiffEditRight, edtFileNameRight.FileName);
  finally
    PushPop(FElevate);
  end;
end;

constructor TfrmDiffer.Create(TheOwner: TComponent);
var
  HMForm: THMForm;
begin
  inherited Create(TheOwner);
  FCommands := TFormCommands.Create(Self, actionList);
  HMForm := HotMan.Register(Self, HotkeysCategory);
  HMForm.RegisterActionList(actionList);
end;

destructor TfrmDiffer.Destroy;
begin
  BinaryViewerLeft.SecondViewer:= nil;
  BinaryViewerRight.SecondViewer:= nil;
  HotMan.UnRegister(Self);
  inherited Destroy;
  if Assigned(FWaitData) then FWaitData.Done;
end;

procedure TfrmDiffer.AfterConstruction;
begin
  inherited AfterConstruction;
  ToolBar.ImagesWidth:= gToolIconsSize;
  ToolBar.SetButtonSize(gToolIconsSize + ScaleX(6, 96),
                        gToolIconsSize + ScaleY(6, 96));
end;

procedure TfrmDiffer.BuildHashList(bLeft, bRight: Boolean);
var
  S: String;
  I: Integer;
begin
  if bLeft then
  begin
    FLeftLen:= 0;
    SetLength(HashListLeft, SynDiffEditLeft.Lines.Count);
    for I := 0 to SynDiffEditLeft.Lines.Count - 1 do
    begin
      S:= SynDiffEditLeft.Lines[I];
      FLeftLen:= Max(FLeftLen, Length(S));
      HashListLeft[I]:= Integer(HashString(S, actIgnoreCase.Checked, actIgnoreWhiteSpace.Checked));
    end;
  end;
  if bRight then
  begin
    FRightLen:= 0;
    SetLength(HashListRight, SynDiffEditRight.Lines.Count);
    for I := 0 to SynDiffEditRight.Lines.Count - 1 do
    begin
      S:= SynDiffEditRight.Lines[I];
      FRightLen:= Max(FRightLen, Length(S));
      HashListRight[I]:= Integer(HashString(S, actIgnoreCase.Checked, actIgnoreWhiteSpace.Checked));
    end;
  end;

  actLineDifferences.Enabled:= (FLeftLen < High(UInt16)) and (FRightLen < High(UInt16));
  if not actLineDifferences.Enabled then actLineDifferences.Checked:= False;

  actStartCompare.Enabled := (Length(HashListLeft) > 0) and (Length(HashListRight) > 0);
  actCopyLeftToRight.Enabled := actStartCompare.Enabled;
  actCopyRightToLeft.Enabled := actStartCompare.Enabled;
end;

procedure TfrmDiffer.SetColors(cAdded, cDeleted, cModified: TColor);
begin
  with SynDiffEditLeft do
  begin
    Colors.Added:= cAdded;
    Colors.Deleted:= cDeleted;
    Colors.Modified:= cModified;
  end;
  with SynDiffEditRight do
  begin
    Colors.Added:= cAdded;
    Colors.Deleted:= cDeleted;
    Colors.Modified:= cModified;
  end;
  SynDiffHighlighterLeft.UpdateColors;
  SynDiffHighlighterRight.UpdateColors;
end;

procedure TfrmDiffer.ChooseEncoding(SynDiffEdit: TSynDiffEdit);
begin
  if SynDiffEdit = SynDiffEditLeft then
    begin
      ChooseEncoding(miEncodingLeft, SynDiffEdit.Encoding);
      ChooseEncoding(pmEncodingLeft.Items, SynDiffEdit.Encoding);
    end
  else
    begin
      ChooseEncoding(miEncodingRight, SynDiffEdit.Encoding);
      ChooseEncoding(pmEncodingRight.Items, SynDiffEdit.Encoding);
    end;
end;

function TfrmDiffer.GetDisplayNumber(LineNumber: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to SynDiffEditActive.Lines.Count - 1 do
  begin
    if SynDiffEditActive.Lines.Number[I] = LineNumber then
    begin
      Result := I + 1;
      Break;
    end;
  end;
end;

procedure TfrmDiffer.ChooseEncoding(MenuItem: TMenuItem; Encoding: String);
var
  I: Integer;
begin
  Encoding:= NormalizeEncoding(Encoding);
  for I:= 0 to MenuItem.Count - 1 do
    if SameText(NormalizeEncoding(MenuItem.Items[I].Caption), Encoding) then
      MenuItem.Items[I].Checked:= True;
end;

procedure TfrmDiffer.FillEncodingMenu(TheOwner: TMenuItem;
  MenuHandler: TNotifyEvent; GroupIndex: LongInt);
var
  I: Integer;
  mi: TMenuItem;
begin
  for I:= 0 to EncodingList.Count - 1 do
    begin
      mi:= TMenuItem.Create(TheOwner);
      mi.Caption:= EncodingList[I];
      mi.RadioItem:= True;
      mi.GroupIndex:= GroupIndex;
      mi.OnClick:= MenuHandler;
      TheOwner.Add(mi);
    end;
end;

procedure TfrmDiffer.LoadFromFile(SynDiffEdit: TSynDiffEdit; const FileName: String);
var
  AText: String;
  fsFileStream: TFileStreamUAC;
begin
  try
    fsFileStream:= TFileStreamUAC.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(AText, fsFileStream.Size);
      fsFileStream.Read(Pointer(AText)^, Length(AText));
      if Length(SynDiffEdit.Encoding) = 0 then
      begin
        SynDiffEdit.Encoding:= DetectEncoding(AText);
        ChooseEncoding(SynDiffEdit);
      end;
      with SynDiffEdit do
      begin
        if (Encoding = EncodingUTF16LE) or (Encoding = EncodingUTF16BE) then
        begin
          AText:= Copy(AText, 3, MaxInt); // Skip BOM
        end;
        AText:= ConvertEncoding(AText, Encoding, EncodingUTF8);
      end;
      SynDiffEdit.Lines.Text:= AText;
      // Add empty line if needed
      if (Length(AText) > 0) and (AText[Length(AText)] in [#10, #13]) then
        SynDiffEdit.Lines.Add(EmptyStr);
      // Determine line break style
      SynDiffEdit.Lines.TextLineBreakStyle := GuessLineBreakStyle(AText);
    finally
      FreeAndNil(fsFileStream);
    end;
  except
    on E: Exception do
    begin
      E.Message:= E.Message + LineEnding + FileName;
      if FShowIdentical then raise;
      msgError(E.Message);
    end;
  end;
end;

procedure TfrmDiffer.SaveToFile(SynDiffEdit: TSynDiffEdit; const FileName: String);
var
  AText: String;
begin
  AText := EmptyStr;
  if (SynDiffEdit.Encoding = EncodingUTF16LE) then
    AText := UTF16LEBOM
  else if (SynDiffEdit.Encoding = EncodingUTF16BE) then begin
    AText := UTF16BEBOM
  end;
  with TStringListEx.Create do
  try
    Assign(SynDiffEdit.Lines);
    SkipLastLineBreak:= True;
    // remove fake lines
    RemoveFake;
    // restore encoding
    AText+= ConvertEncoding(Text, EncodingUTF8, SynDiffEdit.Encoding);
  finally
    Free;
  end;
  // save to file
  try
    with TFileStreamUAC.Create(FileName, fmCreate) do
    try
      WriteBuffer(Pointer(AText)^, Length(AText));
    finally
      Free;
    end;
    SynDiffEdit.Modified:= False; // needed for the undo stack
  except
    on E: Exception do
      msgError(rsMsgErrSaveFile + ' ' + FileName + LineEnding + E.Message);
  end;
end;

procedure TfrmDiffer.OpenFileLeft(const FileName: String);
begin
  PushPop(FElevate);
  try
    if not FileExistsUAC(FileName) then Exit;
    if actBinaryCompare.Checked then
    begin
      BinaryDiffList.Clear;
      BinaryViewerLeft.FileName:= FileName
    end
    else begin
      Clear(True, False);
      LoadFromFile(SynDiffEditLeft, FileName);
      BuildHashList(True, False);
      SynDiffEditLeft.Repaint;
    end;
  finally
    PushPop(FElevate);
  end;
end;

procedure TfrmDiffer.OpenFileRight(const FileName: String);
begin
  PushPop(FElevate);
  try
    if not FileExistsUAC(FileName) then Exit;
    if actBinaryCompare.Checked then
    begin
      BinaryDiffList.Clear;
      BinaryViewerRight.FileName:= FileName
    end
    else begin
      Clear(False, True);
      LoadFromFile(SynDiffEditRight, FileName);
      BuildHashList(False, True);
      SynDiffEditRight.Repaint;
    end;
  finally
    PushPop(FElevate);
  end;
end;

procedure TfrmDiffer.SetEncodingLeft(Sender: TObject);
begin
  SynDiffEditLeft.Encoding:= (Sender as TMenuItem).Caption;
  ChooseEncoding(miEncodingLeft, SynDiffEditLeft.Encoding);
  ChooseEncoding(pmEncodingLeft.Items, SynDiffEditLeft.Encoding);
  actReload.Execute;
end;

procedure TfrmDiffer.SetEncodingRight(Sender: TObject);
begin
  SynDiffEditRight.Encoding:= (Sender as TMenuItem).Caption;
  ChooseEncoding(miEncodingRight, SynDiffEditRight.Encoding);
  ChooseEncoding(pmEncodingRight.Items, SynDiffEditRight.Encoding);
  actReload.Execute;
end;

procedure TfrmDiffer.SynDiffEditEnter(Sender: TObject);
begin
  SynDiffEditActive:= (Sender as TSynDiffEdit);
end;

procedure TfrmDiffer.ShowFirstDifference(Data: PtrInt);
begin
  cm_FirstDifference([]);
end;

procedure TfrmDiffer.SynDiffEditLeftStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (actKeepScrolling.Checked) and (ScrollLock = 0) and
     ((scTopLine in Changes) or (scLeftChar in Changes)) then
    try
      Inc(ScrollLock);
      while (SynDiffEditRight.PaintLock <> 0) do Sleep(1);
      SynDiffEditRight.TopLine:= SynDiffEditLeft.TopLine;
      SynDiffEditRight.LeftChar:= SynDiffEditLeft.LeftChar;
    finally
      Dec(ScrollLock);
    end;
end;

procedure TfrmDiffer.SynDiffEditRightStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (actKeepScrolling.Checked) and (ScrollLock = 0) and
     ((scTopLine in Changes) or (scLeftChar in Changes)) then
    try
      Inc(ScrollLock);
      while (SynDiffEditLeft.PaintLock <> 0) do Sleep(1);
      SynDiffEditLeft.TopLine:= SynDiffEditRight.TopLine;
      SynDiffEditLeft.LeftChar:= SynDiffEditRight.LeftChar;
    finally
      Dec(ScrollLock);
    end;
end;

procedure TfrmDiffer.CMThemeChanged(var Message: TLMessage);
begin
  with gColors.Differ^ do
  begin
    BinaryViewerLeft.Modified:= ModifiedBinaryColor;
    BinaryViewerRight.Modified:= ModifiedBinaryColor;
    SetColors(AddedColor, DeletedColor, ModifiedColor);
  end;
  if not actBinaryCompare.Checked then
  begin
    SynDiffEditLeft.Repaint;
    SynDiffEditRight.Repaint;
  end
  else begin
    BinaryViewerLeft.Repaint;
    BinaryViewerRight.Repaint;
  end;
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmDiffer, HotkeysCategory, @rsHotkeyCategoryDiffer);

end.


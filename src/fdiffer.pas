{
   Double Commander
   -------------------------------------------------------------------------
   Internal diff and merge tool

   Copyright (C) 2010-2011  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit fDiffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Menus, ComCtrls,
  ActnList, ExtCtrls, EditBtn, Buttons, SynEdit, uSynDiffControls,
  uPariterControls, uDiff;

type

  { TfrmDiffer }

  TfrmDiffer = class(TForm)
    actBinaryCompare: TAction;
    actCopyLeftToRight: TAction;
    actCopyRightToLeft: TAction;
    actClose: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditDelete: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
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
    actFirstDiff: TAction;
    actIgnoreCase: TAction;
    actIgnoreWhiteSpace: TAction;
    actCancelCompare: TAction;
    actKeepScrolling: TAction;
    actPrevDiff: TAction;
    actLastDiff: TAction;
    actNextDiff: TAction;
    actSaveAs: TAction;
    actSave: TAction;
    ActionList: TActionList;
    edtFileNameLeft: TFileNameEdit;
    edtFileNameRight: TFileNameEdit;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    miAutoCompare: TMenuItem;
    miDivider10: TMenuItem;
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
    miEditSelectAll: TMenuItem;
    miDivider9: TMenuItem;
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
    miClose: TMenuItem;
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
    ToolBar: TToolBar;
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
    procedure actAboutExecute(Sender: TObject);
    procedure actBinaryCompareExecute(Sender: TObject);
    procedure actCancelCompareExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCopyLeftToRightExecute(Sender: TObject);
    procedure actCopyRightToLeftExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actFirstDiffExecute(Sender: TObject);
    procedure actIgnoreCaseExecute(Sender: TObject);
    procedure actLastDiffExecute(Sender: TObject);
    procedure actLineDifferencesExecute(Sender: TObject);
    procedure actNextDiffExecute(Sender: TObject);
    procedure actOpenLeftExecute(Sender: TObject);
    procedure actOpenRightExecute(Sender: TObject);
    procedure actPaintBackgroundExecute(Sender: TObject);
    procedure actPrevDiffExecute(Sender: TObject);
    procedure actReloadExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveLeftAsExecute(Sender: TObject);
    procedure actSaveLeftExecute(Sender: TObject);
    procedure actSaveRightAsExecute(Sender: TObject);
    procedure actSaveRightExecute(Sender: TObject);
    procedure actStartCompareExecute(Sender: TObject);
    procedure actKeepScrollingExecute(Sender: TObject);
    procedure btnLeftEncodingClick(Sender: TObject);
    procedure btnRightEncodingClick(Sender: TObject);
    procedure edtFileNameLeftAcceptFileName(Sender: TObject; var Value: String);
    procedure edtFileNameRightAcceptFileName(Sender: TObject; var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    Diff: TDiff;
    SynDiffEditActive: TSynDiffEdit;
    SynDiffEditLeft: TSynDiffEdit;
    SynDiffEditRight: TSynDiffEdit;
    SynDiffHighlighterLeft,
    SynDiffHighlighterRight: TSynDiffHighlighter;
    HashListLeft,
    HashListRight: TList;
    EncodingList: TStringList;
    ScrollLock: LongInt;
    procedure Clear(bLeft, bRight: Boolean);
    procedure BuildHashList(bLeft, bRight: Boolean);
    procedure ChooseEncoding(SynDiffEdit: TSynDiffEdit);
    procedure ChooseEncoding(MenuItem: TMenuItem; Encoding: String);
    procedure FillEncodingMenu(TheOwner: TMenuItem; MenuHandler: TNotifyEvent; GroupIndex: LongInt);
    procedure LoadFromFile(SynDiffEdit: TSynDiffEdit; const FileName: UTF8String);
    procedure SaveToFile(SynDiffEdit: TSynDiffEdit; const FileName: UTF8String);
    procedure OpenFileLeft(const FileName: UTF8String);
    procedure OpenFileRight(const FileName: UTF8String);
    procedure SetEncodingLeft(Sender: TObject);
    procedure SetEncodingRight(Sender: TObject);
    procedure SynDiffEditEnter(Sender: TObject);
    procedure SynDiffEditLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynDiffEditRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    { public declarations }
  end; 

procedure ShowDiffer(const FileNameLeft, FileNameRight: UTF8String);

implementation

uses
  LCLProc, LConvEncoding, SynEditTypes, uHash, uLng, uGlobs, uShowMsg,
  uBinaryCompare, uClassesEx, dmCommonData, uOSUtils;

{$R *.lfm}

procedure ShowDiffer(const FileNameLeft, FileNameRight: UTF8String);
begin
  with TfrmDiffer.Create(Application) do
  begin
    edtFileNameLeft.Text:= FileNameLeft;
    edtFileNameRight.Text:= FileNameRight;
    actBinaryCompare.Checked:= not (FileIsText(FileNameLeft) or FileIsText(FileNameRight));
    if not actBinaryCompare.Checked then
    begin
      OpenFileLeft(FileNameLeft);
      OpenFileRight(FileNameRight);
      if actAutoCompare.Checked then actStartCompare.Execute;
    end;
    ShowOnTop;
  end;
end;

{ TfrmDiffer }

procedure TfrmDiffer.actStartCompareExecute(Sender: TObject);
var
  I, DiffCount: Integer;
  LineNumberLeft,
  LineNumberRight: PtrInt;
begin
  try
    Inc(ScrollLock);
    Screen.Cursor := crHourGlass;
    if actBinaryCompare.Checked then
      begin
        SynDiffEditLeft.BeginCompare(nil);
        SynDiffEditRight.BeginCompare(nil);
        DiffCount := BinaryCompare(edtFileNameLeft.Text, edtFileNameRight.Text,
                                   SynDiffEditLeft.Lines, SynDiffEditRight.Lines);
      end
    else
      begin
        if (HashListLeft.Count = 0) or (HashListRight.Count = 0) then Exit;
        actCancelCompare.Enabled := True;

        //nb: TList.list is a pointer to the bottom of the list's integer array
        Diff.Execute(
                     PInteger(HashListLeft.List),
                     PInteger(HashListRight.List),
                     HashListLeft.Count,
                     HashListRight.Count
                    );

        if Diff.Cancelled then Exit;

        SynDiffEditLeft.BeginCompare(Diff);
        SynDiffEditRight.BeginCompare(Diff);

        for I := 0 to Diff.Count - 1 do
        with Diff.Compares[I] do
        begin
          LineNumberLeft:= oldIndex1 + 1;
          LineNumberRight:= oldIndex2 + 1;
          case Kind of
          ckAdd:
            begin
              SynDiffEditLeft.InsertFakeLine(I, lkFakeAdd);
              SynDiffEditRight.LineNumber[I]:= LineNumberRight;
            end;
          ckDelete:
            begin
              SynDiffEditLeft.LineNumber[I]:= LineNumberLeft;
              SynDiffEditRight.InsertFakeLine(I, lkFakeDelete);
            end;
          else
            begin
              SynDiffEditLeft.LineNumber[I]:= LineNumberLeft;
              SynDiffEditRight.LineNumber[I]:= LineNumberRight;
            end;
          end;
        end;
        DiffCount:= Diff.Count;
        with Diff.DiffStats do
        begin
          StatusBar.Panels[0].Text := ' Matches: ' + IntToStr(matches);
          StatusBar.Panels[1].Text := ' Modifies: ' + IntToStr(modifies);
          StatusBar.Panels[2].Text := ' Adds: ' + IntToStr(adds);
          StatusBar.Panels[3].Text := ' Deletes: ' + IntToStr(deletes);
        end;
    end;
  finally
    SynDiffEditLeft.EndCompare(DiffCount);
    SynDiffEditRight.EndCompare(DiffCount);
    SynDiffEditLeft.Invalidate;
    SynDiffEditRight.Invalidate;
    Screen.Cursor := crDefault;
    actCancelCompare.Enabled := False;
    Dec(ScrollLock);
  end;
  if actLineDifferences.Checked then
  begin
    SynDiffEditLeft.Highlighter:= SynDiffHighlighterLeft;
    SynDiffEditRight.Highlighter:= SynDiffHighlighterRight;
  end;
  //mnuEdit.Enabled := true;
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

procedure TfrmDiffer.actReloadExecute(Sender: TObject);
begin
  OpenFileLeft(edtFileNameLeft.FileName);
  OpenFileRight(edtFileNameRight.FileName);
  if actAutoCompare.Checked then actStartCompare.Execute;
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
    SaveToFile(SynDiffEditLeft, dmComData.SaveDialog.FileName);
    edtFileNameLeft.FileName:= dmComData.SaveDialog.FileName;
  end;
end;

procedure TfrmDiffer.actSaveLeftExecute(Sender: TObject);
begin
  SaveToFile(SynDiffEditLeft, edtFileNameLeft.FileName);
end;

procedure TfrmDiffer.actSaveRightAsExecute(Sender: TObject);
begin
  dmComData.SaveDialog.FileName:= edtFileNameRight.FileName;
  if dmComData.SaveDialog.Execute then
  begin
    SaveToFile(SynDiffEditLeft, dmComData.SaveDialog.FileName);
    edtFileNameRight.FileName:= dmComData.SaveDialog.FileName;
  end;
end;

procedure TfrmDiffer.actSaveRightExecute(Sender: TObject);
begin
  SaveToFile(SynDiffEditRight, edtFileNameRight.FileName);
end;

procedure TfrmDiffer.actBinaryCompareExecute(Sender: TObject);
begin
  mnuEdit.Enabled:= not actBinaryCompare.Checked;
  mnuEncoding.Enabled:= not actBinaryCompare.Checked;
  btnLeftEncoding.Enabled:= not actBinaryCompare.Checked;
  btnRightEncoding.Enabled:= not actBinaryCompare.Checked;
  SynDiffEditLeft.ReadOnly:= actBinaryCompare.Checked;
  SynDiffEditRight.ReadOnly:= actBinaryCompare.Checked;
  actCopyLeftToRight.Enabled:= not actBinaryCompare.Checked;
  actCopyRightToLeft.Enabled:= not actBinaryCompare.Checked;
  actSave.Enabled:= not actBinaryCompare.Checked;
  actSaveAs.Enabled:= not actBinaryCompare.Checked;
  actSaveLeft.Enabled:= not actBinaryCompare.Checked;
  actSaveLeftAs.Enabled:= not actBinaryCompare.Checked;
  actSaveRight.Enabled:= not actBinaryCompare.Checked;
  actSaveRightAs.Enabled:= not actBinaryCompare.Checked;
  actIgnoreCase.Enabled:= not actBinaryCompare.Checked;
  actIgnoreWhiteSpace.Enabled:= not actBinaryCompare.Checked;
  actLineDifferences.Enabled:= not actBinaryCompare.Checked;
  if actBinaryCompare.Checked then
    begin
      SynDiffEditLeft.Lines.Clear;
      SynDiffEditRight.Lines.Clear;
    end
  else
    begin
      OpenFileLeft(edtFileNameLeft.Text);
      OpenFileRight(edtFileNameRight.Text);
    end;
  if actAutoCompare.Checked then actStartCompare.Execute;
end;

procedure TfrmDiffer.actCancelCompareExecute(Sender: TObject);
begin
  Diff.Cancel;
end;

procedure TfrmDiffer.actAboutExecute(Sender: TObject);
begin
  ShowMessage('Internal Differ tool of Double Commander.' + LineEnding + LineEnding +
              'It is inspired by Flavio Etrusco''s Pariter tool.' + LineEnding +
              'You can find it on: http://sourceforge.net/projects/pariter/' + LineEnding +
              'It is based on Angus Johnson''s excellent TDiff component.' + LineEnding +
              'You can find it on: http://www.users.on.net/johnson/delphi/');
end;

procedure TfrmDiffer.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDiffer.actCopyLeftToRightExecute(Sender: TObject);
var
  I, iStart,
  iFinish: Integer;
begin
  I := SynDiffEditLeft.CaretY - 1;
  iStart:= SynDiffEditLeft.DiffBegin(I);
  iFinish:= SynDiffEditLeft.DiffEnd(I);
  for I:= iStart to iFinish do
    SynDiffEditRight.Lines[I]:= SynDiffEditLeft.Lines[I];
end;

procedure TfrmDiffer.actCopyRightToLeftExecute(Sender: TObject);
var
  I, iStart,
  iFinish: Integer;
begin
  I := SynDiffEditRight.CaretY - 1;
  iStart:= SynDiffEditRight.DiffBegin(I);
  iFinish:= SynDiffEditRight.DiffEnd(I);
  for I:= iStart to iFinish do
    SynDiffEditLeft.Lines[I]:= SynDiffEditRight.Lines[I];
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

procedure TfrmDiffer.actNextDiffExecute(Sender: TObject);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  Line := SynDiffEditLeft.CaretY - 1;
  if Line = SynDiffEditLeft.Lines.Count - 1 then Exit;
  // Skip lines with current difference type
  Kind := SynDiffEditLeft.DiffKind[Line];
  while (Line < SynDiffEditLeft.Lines.Count - 1) and
    (SynDiffEditLeft.DiffKind[Line] = Kind) do Inc(Line);
  if SynDiffEditLeft.DiffKind[Line] = ckNone then
  begin
    // Skip unmodified lines
    Kind := ckNone;
    while (Line < SynDiffEditLeft.Lines.Count - 1) and
      (SynDiffEditLeft.DiffKind[Line] = Kind) do Inc(Line);
  end;
  Inc(Line);
  SynDiffEditLeft.CaretY := Line;
  SynDiffEditLeft.TopLine := Line;
  if not actKeepScrolling.Checked then
  begin
    SynDiffEditRight.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
  end;
end;

procedure TfrmDiffer.actPrevDiffExecute(Sender: TObject);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  Line := SynDiffEditLeft.CaretY - 1;
  if Line = 0 then Exit;
  // Skip lines with current difference type
  Kind := SynDiffEditLeft.DiffKind[Line];
  while (Line > 0) and (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  if SynDiffEditLeft.DiffKind[Line] = ckNone then
  begin
    // Skip unmodified lines
    Kind := ckNone;
    while (Line > 0) and (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  end;
  // Find top line of previous difference
  Kind:= SynDiffEditLeft.DiffKind[Line];
  while (Line > 0) and (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  if (Line <> 0) then Inc(Line, 2);
  SynDiffEditLeft.CaretY := Line;
  SynDiffEditLeft.TopLine := Line;
  if not actKeepScrolling.Checked then
  begin
    SynDiffEditRight.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
  end;
end;

procedure TfrmDiffer.actFirstDiffExecute(Sender: TObject);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  // Start at first line
  Line := 0;
  if Line = SynDiffEditLeft.Lines.Count then Exit;
  // Skip unmodified lines
  Kind := ckNone;
  while (Line < SynDiffEditLeft.Lines.Count - 1) and
    (SynDiffEditLeft.DiffKind[Line] = Kind) do Inc(Line);
  Inc(Line);
  SynDiffEditLeft.CaretY := Line;
  SynDiffEditLeft.TopLine := Line;
  if not actKeepScrolling.Checked then
  begin
    SynDiffEditRight.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
  end;
end;

procedure TfrmDiffer.actLastDiffExecute(Sender: TObject);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  Line := SynDiffEditLeft.Lines.Count - 1;
  if Line = 0 then Exit;
  // Skip unmodified lines
  Kind := ckNone;
  while (Line > 0) and (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  // Find top line of previous difference
  Kind:= SynDiffEditLeft.DiffKind[Line];
  while (Line > 0) and (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  if (Line <> 0) then Inc(Line, 2);
  SynDiffEditLeft.CaretY := Line;
  SynDiffEditLeft.TopLine := Line;
  if not actKeepScrolling.Checked then
  begin
    SynDiffEditRight.TopLine := Line;
    SynDiffEditRight.CaretY := Line;
  end;
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
end;

procedure TfrmDiffer.edtFileNameRightAcceptFileName(Sender: TObject;
  var Value: String);
begin
  OpenFileRight(Value);
end;

procedure TfrmDiffer.FormCreate(Sender: TObject);
begin
  ScrollLock:= 0;
  Diff:= TDiff.Create(Self);
  SynDiffEditLeft:= TSynDiffEdit.Create(Self);
  SynDiffEditRight:= TSynDiffEdit.Create(Self);
  SynDiffHighlighterLeft:= TSynDiffHighlighter.Create(SynDiffEditLeft);
  SynDiffHighlighterRight:= TSynDiffHighlighter.Create(SynDiffEditRight);
  HashListLeft:= TList.Create;
  HashListRight:= TList.Create;

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
  FreeThenNil(Diff);
  FreeThenNil(HashListLeft);
  FreeThenNil(HashListRight);
end;

procedure TfrmDiffer.FormResize(Sender: TObject);
begin
  pnlLeft.Width:= (ClientWidth div 2) - (Splitter.Width div 2);
end;

procedure TfrmDiffer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmDiffer.Clear(bLeft, bRight: Boolean);
begin
  if bLeft then
  begin
    SynDiffEditLeft.Lines.Clear;
    HashListLeft.Clear;
  end;
  if bRight then
  begin
    SynDiffEditRight.Lines.Clear;
    HashListRight.Clear;
  end;
  Diff.Clear;
  StatusBar.Panels[0].Text := EmptyStr;
  StatusBar.Panels[1].Text := EmptyStr;
  StatusBar.Panels[2].Text := EmptyStr;
  StatusBar.Panels[3].Text := EmptyStr;
  actStartCompare.Enabled := True;
end;

procedure TfrmDiffer.BuildHashList(bLeft, bRight: Boolean);
var
  I: Integer;
begin
  if bLeft then
  begin
    HashListLeft.Clear;
    for I := 0 to SynDiffEditLeft.Lines.Count - 1 do
      HashListLeft.Add(HashString(SynDiffEditLeft.Lines[i],
        actIgnoreCase.Checked, actIgnoreWhiteSpace.Checked));
  end;
  if bRight then
  begin
    HashListRight.Clear;
    for I := 0 to SynDiffEditRight.Lines.Count - 1 do
      HashListRight.Add(HashString(SynDiffEditRight.Lines[i],
        actIgnoreCase.Checked, actIgnoreWhiteSpace.Checked));
  end;

  actStartCompare.Enabled := (HashListLeft.Count > 0) and (HashListRight.Count > 0);
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

procedure TfrmDiffer.LoadFromFile(SynDiffEdit: TSynDiffEdit; const FileName: UTF8String);
var
  fsFileStream: TFileStreamEx = nil;
begin
  try
    try
      fsFileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
      SynDiffEdit.Lines.LoadFromStream(fsFileStream);
      if Length(SynDiffEdit.Encoding) = 0 then
      begin
        SynDiffEdit.Encoding:= GuessEncoding(SynDiffEdit.Lines.Text);
        ChooseEncoding(SynDiffEdit);
      end;
      SynDiffEdit.Lines.Text:= ConvertEncoding(SynDiffEdit.Lines.Text, SynDiffEdit.Encoding, EncodingUTF8);
    except
      on EFOpenError do
      begin
        msgError(rsMsgErrEOpen + ': ' + FileName);
      end;
      on EReadError do
      begin
        msgError(rsMsgErrERead + ': ' + FileName);
      end;
    end;
  finally
    FreeThenNil(fsFileStream);
  end;
end;

procedure TfrmDiffer.SaveToFile(SynDiffEdit: TSynDiffEdit;
  const FileName: UTF8String);
var
  slStringList: TStringListEx;
begin
  slStringList:= TStringListEx.Create;
  try
    slStringList.Assign(SynDiffEdit.Lines);
    // remove fake lines
    SynDiffEdit.RemoveFakeLines(slStringList);
    // restore encoding
    slStringList.Text:= ConvertEncoding(slStringList.Text, EncodingUTF8, SynDiffEdit.Encoding);
    try
      // save to file
      slStringList.SaveToFile(FileName);
      SynDiffEdit.Modified:= False; // needed for the undo stack
    except
      on EFCreateError do
      begin
        msgError(rsMsgErrECreate + ': ' + FileName);
      end;
      on EFOpenError do
      begin
        msgError(rsMsgErrEOpen + ': ' + FileName);
      end;
      on EWriteError do
      begin
        msgError(rsMsgErrEWrite + ': ' + FileName);
      end;
    end;
  finally
    slStringList.Free;
  end;
end;

procedure TfrmDiffer.OpenFileLeft(const FileName: UTF8String);
begin
  if not mbFileExists(FileName) then Exit;
  try
    Clear(True, False);
    LoadFromFile(SynDiffEditLeft, FileName);
    HashListLeft.Capacity := SynDiffEditLeft.Lines.Count;
    BuildHashList(True, False);
    SynDiffEditLeft.Repaint;
  except
    on EFOpenError do
      msgWarning(rsMsgErrEOpen + ' ' + FileName);
  end;
end;

procedure TfrmDiffer.OpenFileRight(const FileName: UTF8String);
begin
  if not mbFileExists(FileName) then Exit;
  try
    Clear(False, True);
    LoadFromFile(SynDiffEditRight, FileName);
    HashListRight.Capacity := SynDiffEditRight.Lines.Count;
    BuildHashList(False, True);
    SynDiffEditRight.Repaint;
  except
    on EFOpenError do
      msgWarning(rsMsgErrEOpen + ' ' + FileName);
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

procedure TfrmDiffer.SynDiffEditLeftStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (actKeepScrolling.Checked) and (ScrollLock = 0) and
     ((scTopLine in Changes) or (scLeftChar in Changes) or (scCaretY in Changes)) then
    try
      Inc(ScrollLock);
      while (SynDiffEditRight.PaintLock <> 0) do Sleep(1);
      SynDiffEditRight.TopLine:= SynDiffEditLeft.TopLine;
      SynDiffEditRight.LeftChar:= SynDiffEditLeft.LeftChar;
      SynDiffEditRight.CaretY:= SynDiffEditLeft.CaretY;
    finally
      Dec(ScrollLock);
    end;
end;

procedure TfrmDiffer.SynDiffEditRightStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (actKeepScrolling.Checked) and (ScrollLock = 0) and
     ((scTopLine in Changes) or (scLeftChar in Changes) or (scCaretY in Changes)) then
    try
      Inc(ScrollLock);
      while (SynDiffEditLeft.PaintLock <> 0) do Sleep(1);
      SynDiffEditLeft.TopLine:= SynDiffEditRight.TopLine;
      SynDiffEditLeft.LeftChar:= SynDiffEditRight.LeftChar;
      SynDiffEditLeft.CaretY:= SynDiffEditRight.CaretY;
    finally
      Dec(ScrollLock);
    end;
end;

end.


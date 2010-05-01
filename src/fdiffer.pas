unit fDiffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, ExtCtrls, EditBtn, StdCtrls, Buttons, SynEdit,
  uSynDiffControls, uDiff;

type

  { TfrmDiffer }

  TfrmDiffer = class(TForm)
    actBinaryCompare: TAction;
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
    miStartCompare: TMenuItem;
    mnuActions: TMenuItem;
    miIgnoreCase: TMenuItem;
    miIgnoreWhiteSpace: TMenuItem;
    mnuOptions: TMenuItem;
    mnuEdit: TMenuItem;
    miSave: TMenuItem;
    mnuFile: TMenuItem;
    pnlLeftBox: TPanel;
    pnlRight: TPanel;
    pnlLeft: TPanel;
    pnlRightBox: TPanel;
    btnLeftEncoding: TSpeedButton;
    btnRightEncoding: TSpeedButton;
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
    procedure actNextDiffExecute(Sender: TObject);
    procedure actPrevDiffExecute(Sender: TObject);
    procedure actStartCompareExecute(Sender: TObject);
    procedure actKeepScrollingExecute(Sender: TObject);
    procedure edtFileNameLeftAcceptFileName(Sender: TObject; var Value: String);
    procedure edtFileNameRightAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    Diff: TDiff;
    SynDiffEditLeft: TSynDiffEdit;
    SynDiffEditRight: TSynDiffEdit;
    HashListLeft,
    HashListRight: TList;
    procedure Clear(bLeft, bRight: Boolean);
    procedure BuildHashList(bLeft, bRight: Boolean);
    procedure OpenFileLeft(const FileName: UTF8String);
    procedure OpenFileRight(const FileName: UTF8String);
    procedure SynDiffEditLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynDiffEditRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    { public declarations }
  end; 

procedure ShowDiffer(const FileNameLeft, FileNameRight: UTF8String);

implementation

uses
  uHash, uLng, uGlobs, uShowMsg, uClassesEx, uOSUtils;

{$R *.lfm}

procedure ShowDiffer(const FileNameLeft, FileNameRight: UTF8String);
begin
  with TfrmDiffer.Create(Application) do
  begin
    try
      edtFileNameLeft.Text:= FileNameLeft;
      edtFileNameRight.Text:= FileNameRight;
      OpenFileLeft(FileNameLeft);
      OpenFileRight(FileNameRight);
      ShowModal;
    finally
      Free;
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
  if actBinaryCompare.Checked then
    begin

    end
  else
    begin
  if (HashListLeft.Count = 0) or (HashListRight.Count = 0) then Exit;
  actCancelCompare.Enabled := True;
  Screen.Cursor := crHourGlass;
  try
    //this is where it all happens  ...

    //nb: TList.list is a pointer to the bottom of the list's integer array
    Diff.Execute(
                 PInteger(HashListLeft.List),
                 PInteger(HashListRight.List),
                 HashListLeft.Count,
                 HashListRight.Count
                 );

    if Diff.Cancelled then Exit;
    try
    SynDiffEditLeft.BeginCompare(Diff, Diff.Count);
    SynDiffEditRight.BeginCompare(Diff, Diff.Count);

    for I := 0 to Diff.Count - 1 do
    with Diff.Compares[I] do
    begin
      LineNumberLeft:= oldIndex1 + 1;
      LineNumberRight:= oldIndex2 + 1;
      case Kind of
      ckAdd:
        begin
          SynDiffEditLeft.Lines.InsertObject(I, EmptyStr, nil);
          SynDiffEditRight.Lines.Objects[I]:= TObject(LineNumberRight)
        end;
      ckDelete:
        begin
          SynDiffEditLeft.Lines.Objects[I]:= TObject(LineNumberLeft);
          SynDiffEditRight.Lines.InsertObject(I, EmptyStr, nil);
        end;
      else
        begin
          SynDiffEditLeft.Lines.Objects[I]:= TObject(LineNumberLeft);
          SynDiffEditRight.Lines.Objects[I]:= TObject(LineNumberRight)
        end;
      end;
    end;
    finally
      SynDiffEditLeft.EndCompare;
      SynDiffEditRight.EndCompare;
    end;
    with Diff.DiffStats do
    begin
      StatusBar.Panels[0].Text := ' Matches: ' + IntToStr(matches);
      StatusBar.Panels[1].Text := ' Modifies: ' + IntToStr(modifies);
      StatusBar.Panels[2].Text := ' Adds: ' + IntToStr(adds);
      StatusBar.Panels[3].Text := ' Deletes: ' + IntToStr(deletes);
    end;
  finally
    SynDiffEditLeft.Invalidate;
    SynDiffEditRight.Invalidate;
    Screen.Cursor := crDefault;
    actCancelCompare.Enabled := False;
  end;
    end;
  //mnuEdit.Enabled := true;
end;

procedure TfrmDiffer.actNextDiffExecute(Sender: TObject);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  Line := SynDiffEditLeft.TopLine;
  if Line = SynDiffEditLeft.Lines.Count - 1 then Exit;
  Kind := SynDiffEditLeft.DiffKind[Line];
  while (Line < SynDiffEditLeft.Lines.Count - 1) and
    (SynDiffEditLeft.DiffKind[Line] = Kind) do Inc(Line);
  if SynDiffEditLeft.DiffKind[Line] = ckNone then
  begin
    Kind := ckNone;
    while (Line < SynDiffEditLeft.Lines.Count - 1) and
      (SynDiffEditLeft.DiffKind[Line] = Kind) do Inc(Line);
  end;
  SynDiffEditLeft.CaretY := Line;
  SynDiffEditRight.CaretY := Line;
  SynDiffEditLeft.TopLine := Line + 1;
end;

procedure TfrmDiffer.actPrevDiffExecute(Sender: TObject);
var
  Line: Integer;
  Kind: TChangeKind;
begin
  Line := SynDiffEditLeft.TopLine;
  if Line = 0 then Exit;
  Kind := SynDiffEditLeft.DiffKind[Line];
  while (Line > 0) and (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  if SynDiffEditLeft.DiffKind[Line] = ckNone then
  begin
    Kind := ckNone;
    while (Line > 0) and
      (SynDiffEditLeft.DiffKind[Line] = Kind) do Dec(Line);
  end;
  Kind:= SynDiffEditLeft.DiffKind[Line];
  repeat
    Dec(Line);
  until (SynDiffEditLeft.DiffKind[Line] = Kind);
  Inc(Line);
  SynDiffEditLeft.CaretY := Line;
  SynDiffEditRight.CaretY := Line;
  SynDiffEditLeft.TopLine := Line - 1;
end;

procedure TfrmDiffer.actKeepScrollingExecute(Sender: TObject);
begin

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
  Diff:= TDiff.Create(Self);
  SynDiffEditLeft:= TSynDiffEdit.Create(Self);
  SynDiffEditRight:= TSynDiffEdit.Create(Self);
  HashListLeft:= TList.Create;
  HashListRight:= TList.Create;

  SynDiffEditLeft.Parent:= pnlLeft;
  SynDiffEditRight.Parent:= pnlRight;
  SynDiffEditLeft.Align:= alClient;
  SynDiffEditRight.Align:= alClient;

  SynDiffEditLeft.OnStatusChange:= @SynDiffEditLeftStatusChange;
  SynDiffEditRight.OnStatusChange:= @SynDiffEditRightStatusChange;
  // Initialize property storage
  InitPropStorage(Self);
end;

procedure TfrmDiffer.FormResize(Sender: TObject);
begin
  pnlLeft.Width:= (ClientWidth div 2) - (Splitter.Width div 2);
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
  actStartCompare.Enabled := False;
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

procedure TfrmDiffer.OpenFileLeft(const FileName: UTF8String);
var
  fsFileStream: TFileStreamEx = nil;
begin
  if not mbFileExists(FileName) then Exit;
  try
    Clear(True, False);
    fsFileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SynDiffEditLeft.Lines.LoadFromStream(fsFileStream);
    finally
      fsFileStream.Free;
    end;
    HashListLeft.Capacity := SynDiffEditLeft.Lines.Count;
    BuildHashList(True, False);
  except
    on EFOpenError do
      msgWarning(rsMsgErrEOpen + ' ' + FileName);
  end;
end;

procedure TfrmDiffer.OpenFileRight(const FileName: UTF8String);
var
  fsFileStream: TFileStreamEx = nil;
begin
  if not mbFileExists(FileName) then Exit;
  try
    Clear(False, True);
    fsFileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SynDiffEditRight.Lines.LoadFromStream(fsFileStream);
    finally
      fsFileStream.Free;
    end;
    HashListRight.Capacity := SynDiffEditRight.Lines.Count;
    BuildHashList(False, True);
  except
    on EFOpenError do
      msgWarning(rsMsgErrEOpen + ' ' + FileName);
  end;
end;

procedure TfrmDiffer.SynDiffEditLeftStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (actKeepScrolling.Checked) then
    begin
      SynDiffEditRight.TopLine:= SynDiffEditLeft.TopLine;
      SynDiffEditRight.LeftChar:= SynDiffEditLeft.LeftChar;
      SynDiffEditLeft.Invalidate;
    end;
end;

procedure TfrmDiffer.SynDiffEditRightStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (actKeepScrolling.Checked) then
    begin
      SynDiffEditLeft.TopLine:= SynDiffEditRight.TopLine;
      SynDiffEditLeft.LeftChar:= SynDiffEditRight.LeftChar;
      SynDiffEditRight.Invalidate;
    end;
end;

end.


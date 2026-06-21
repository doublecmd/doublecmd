unit fDeleteDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, StdCtrls, fButtonForm, uOperationsManager, uFileSource;

type
  TDeleteMode = (dmTrash, dmDelete, dmWipe);

  { TfrmDeleteDlg }

  TfrmDeleteDlg = class(TfrmButtonForm)
    rbTrash: TRadioButton;
    rbDelete: TRadioButton;
    rbWipe: TRadioButton;
    lblMessage: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rbModeChange(Sender: TObject);
  private
    FMessages: array[TDeleteMode] of String;
  public
    { public declarations }
  end;

function ShowDeleteDialog(TheOwner: TComponent; const Message: String; FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier): Boolean; overload;
function ShowDeleteDialog(TheOwner: TComponent; const TrashMessage, DeleteMessage, WipeMessage: String;
  FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier;
  ShowTrash, ShowWipe: Boolean; var DeleteMode: TDeleteMode): Boolean; overload;

implementation

uses
  LCLType;

function ShowDeleteDialog(TheOwner: TComponent; const Message: String; FileSource: IFileSource;
  out QueueId: TOperationsManagerQueueIdentifier): Boolean;
var
  Dlg: TfrmDeleteDlg;
begin
  Dlg := TfrmDeleteDlg.Create(TheOwner, FileSource);
  try
    Dlg.Caption := Application.Title;
    Dlg.lblMessage.Caption := Message;
    Result := Dlg.ShowModal = mrOK;
    QueueId := Dlg.QueueIdentifier;
  finally
    Dlg.Free;
  end;
end;

function ShowDeleteDialog(TheOwner: TComponent; const TrashMessage, DeleteMessage, WipeMessage: String;
  FileSource: IFileSource; out QueueId: TOperationsManagerQueueIdentifier;
  ShowTrash, ShowWipe: Boolean; var DeleteMode: TDeleteMode): Boolean;
var
  Dlg: TfrmDeleteDlg;
begin
  Dlg := TfrmDeleteDlg.Create(TheOwner, FileSource);
  try
    Dlg.Caption := Application.Title;
    Dlg.FMessages[dmTrash]  := TrashMessage;
    Dlg.FMessages[dmDelete] := DeleteMessage;
    Dlg.FMessages[dmWipe]   := WipeMessage;
    Dlg.rbTrash.Visible  := ShowTrash;
    Dlg.rbDelete.Visible := True;
    Dlg.rbWipe.Visible   := ShowWipe;
    // Set initial selection; fall back to dmDelete if the preferred mode is unavailable
    case DeleteMode of
      dmTrash:  if ShowTrash then Dlg.rbTrash.Checked  := True else Dlg.rbDelete.Checked := True;
      dmWipe:   if ShowWipe  then Dlg.rbWipe.Checked   := True else Dlg.rbDelete.Checked := True;
      else           Dlg.rbDelete.Checked := True;
    end;
    Dlg.lblMessage.Caption := Dlg.FMessages[DeleteMode];
    Result := Dlg.ShowModal = mrOK;
    if Result then
    begin
      if Dlg.rbTrash.Checked then DeleteMode := dmTrash
      else if Dlg.rbWipe.Checked then DeleteMode := dmWipe
      else DeleteMode := dmDelete;
    end;
    QueueId := Dlg.QueueIdentifier;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TfrmDeleteDlg }

procedure TfrmDeleteDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Modes: array[0..2] of TRadioButton;
  Count, Cur, I: Integer;
begin
  case Key of
    VK_RETURN:
      begin
        btnOK.Click;
        Key := 0;
      end;
    VK_UP, VK_DOWN:
      begin
        { Build ordered list of visible radio buttons. }
        Count := 0;
        Cur   := -1;
        for I := 0 to 2 do
          Modes[I] := nil;
        if rbTrash.Visible  then begin Modes[Count] := rbTrash;  if rbTrash.Checked  then Cur := Count; Inc(Count); end;
        if rbDelete.Visible then begin Modes[Count] := rbDelete; if rbDelete.Checked then Cur := Count; Inc(Count); end;
        if rbWipe.Visible   then begin Modes[Count] := rbWipe;   if rbWipe.Checked   then Cur := Count; Inc(Count); end;
        if Count > 1 then
        begin
          if Key = VK_DOWN then
            Cur := (Cur + 1) mod Count
          else
            Cur := (Cur + Count - 1) mod Count;
          Modes[Cur].Checked := True;
        end;
        Key := 0;
      end;
  end;
end;

procedure TfrmDeleteDlg.rbModeChange(Sender: TObject);
var
  Mode: TDeleteMode;
begin
  if rbTrash.Checked then Mode := dmTrash
  else if rbWipe.Checked then Mode := dmWipe
  else Mode := dmDelete;
  lblMessage.Caption := FMessages[Mode];
end;

end.


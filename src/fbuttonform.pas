unit fButtonForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, uOperationsManager, uFileSource, uFormCommands;

type

  { TfrmButtonForm }

  TfrmButtonForm = class(TForm, IFormCommands)
    btnAddToQueue: TBitBtn;
    btnCancel: TBitBtn;
    btnCreateSpecialQueue: TBitBtn;
    btnOK: TBitBtn;
    mnuNewQueue: TMenuItem;
    mnuQueue1: TMenuItem;
    mnuQueue2: TMenuItem;
    mnuQueue3: TMenuItem;
    mnuQueue4: TMenuItem;
    mnuQueue5: TMenuItem;
    pmQueuePopup: TPopupMenu;
    pnlContent: TPanel;
    pnlButtons: TPanel;
    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCreateSpecialQueueClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure mnuNewQueueClick(Sender: TObject);
    procedure mnuQueueNumberClick(Sender: TObject);
  private
    FCommands: TFormCommands;
    function GetQueueIdentifier: TOperationsManagerQueueIdentifier;
    property {%H-}Commands: TFormCommands read FCommands implements IFormCommands;
  public
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent; FileSource: IFileSource); reintroduce;
    property QueueIdentifier: TOperationsManagerQueueIdentifier read GetQueueIdentifier;
  published
    procedure cm_AddToQueue(const Params: array of String);
  end;

var
  frmButtonForm: TfrmButtonForm;

implementation

uses
  LCLStrConsts, DCStrUtils, uFileSourceProperty, uHotkeyManager, uGlobs;

{$R *.lfm}

const
  HotkeysCategory = 'Confirmation';

var
  FQueueIdentifier: TOperationsManagerQueueIdentifier = SingleQueueId;

{ TfrmButtonForm }

procedure TfrmButtonForm.btnCreateSpecialQueueClick(Sender: TObject);
begin
  btnCreateSpecialQueue.PopupMenu.PopUp;
end;

procedure TfrmButtonForm.btnAddToQueueClick(Sender: TObject);
begin
  ModalResult := btnAddToQueue.ModalResult;
end;

procedure TfrmButtonForm.btnOKClick(Sender: TObject);
begin
  if FQueueIdentifier <> ModalQueueId then
    FQueueIdentifier := FreeOperationsQueueId;
end;

procedure TfrmButtonForm.mnuNewQueueClick(Sender: TObject);
begin
  FQueueIdentifier := OperationsManager.GetNewQueueIdentifier;
  ModalResult := btnAddToQueue.ModalResult;
end;

procedure TfrmButtonForm.mnuQueueNumberClick(Sender: TObject);
var
  NewQueueNumber: TOperationsManagerQueueIdentifier;
begin
  if TryStrToInt(Copy((Sender as TMenuItem).Name, 9, 1), NewQueueNumber) then
  begin
    FQueueIdentifier := NewQueueNumber;
    ModalResult := btnAddToQueue.ModalResult;
  end;
end;

function TfrmButtonForm.GetQueueIdentifier: TOperationsManagerQueueIdentifier;
begin
  Result:= FQueueIdentifier;
end;

constructor TfrmButtonForm.Create(TheOwner: TComponent);
begin
  Create(TheOwner, nil);
end;

constructor TfrmButtonForm.Create(TheOwner: TComponent; FileSource: IFileSource);
var
  HMForm: THMForm;
  Hotkey: THotkey;
begin
  FCommands := TFormCommands.Create(Self);

  inherited Create(TheOwner);

  if FQueueIdentifier <= FreeOperationsQueueId then FQueueIdentifier:= SingleQueueId;
  btnAddToQueue.Caption:= btnAddToQueue.Caption + ' #' + IntToStr(FQueueIdentifier);

  if Assigned(FileSource) and (fspListOnMainThread in FileSource.Properties) then
  begin
    btnAddToQueue.Visible:= False;
    FQueueIdentifier:= ModalQueueId;
    btnCreateSpecialQueue.Visible:= btnAddToQueue.Visible;
  end;

  HMForm := HotMan.Register(Self, HotkeysCategory);
  Hotkey := HMForm.Hotkeys.FindByCommand('cm_AddToQueue');

  if Assigned(Hotkey) then
    btnAddToQueue.Caption := btnAddToQueue.Caption + ' (' + ShortcutsToText(Hotkey.Shortcuts) + ')';
end;

procedure TfrmButtonForm.cm_AddToQueue(const Params: array of String);
var
  Value: Integer;
  sQueueId: String;
begin
  if FQueueIdentifier = ModalQueueId then Exit;
  if GetParamValue(Params, 'queueid', sQueueId) and TryStrToInt(sQueueId, Value) then
    begin
      if Value < 0 then
        mnuNewQueue.Click
      else
        FQueueIdentifier := Value
    end
  else
    FQueueIdentifier := SingleQueueId;
  ModalResult := btnAddToQueue.ModalResult;
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmButtonForm, HotkeysCategory, @rsMtConfirmation);

end.

unit fButtonForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, uOperationsManager, uFileSource;

type

  { TfrmButtonForm }

  TfrmButtonForm = class(TForm)
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
    function GetQueueIdentifier: TOperationsManagerQueueIdentifier;
  public
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent; FileSource: IFileSource); reintroduce;
    property QueueIdentifier: TOperationsManagerQueueIdentifier read GetQueueIdentifier;
  end;

var
  frmButtonForm: TfrmButtonForm;

implementation

uses
  uFileSourceProperty;

{$R *.lfm}

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
begin
  inherited Create(TheOwner);

  if FQueueIdentifier <= FreeOperationsQueueId then FQueueIdentifier:= SingleQueueId;
  btnAddToQueue.Caption:= btnAddToQueue.Caption + ' #' + IntToStr(FQueueIdentifier);

  if Assigned(FileSource) and (fspListOnMainThread in FileSource.Properties) then
  begin
    btnAddToQueue.Visible:= False;
    FQueueIdentifier:= ModalQueueId;
    btnCreateSpecialQueue.Visible:= btnAddToQueue.Visible;
  end;
end;

end.


unit fWfxPluginCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI,
  uFileSourceCopyOperation,
  uWfxPluginCopyOperation,
  uWfxPluginMoveOperation,
  uWfxPluginCopyInOperation,
  uWfxPluginCopyOutOperation;

type

  { TWfxPluginCopyMoveOperationOptionsUI }

  TWfxPluginCopyMoveOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cbCopyTime: TCheckBox;
    cbWorkInBackground: TCheckBox;
    cmbFileExists: TComboBox;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
    pnlCheckboxes: TPanel;
    pnlComboBoxes: TPanel;
    procedure cbWorkInBackgroundChange(Sender: TObject);
  private
    procedure SetCopyOptions(CopyOperation: TFileSourceCopyOperation);
    procedure SetOperationOptions(CopyOperation: TWfxPluginCopyOperation); overload;
    procedure SetOperationOptions(MoveOperation: TWfxPluginMoveOperation); overload;
    procedure SetOperationOptions(CopyInOperation: TWfxPluginCopyInOperation); overload;
    procedure SetOperationOptions(CopyOutOperation: TWfxPluginCopyOutOperation); overload;
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

  TWfxPluginCopyOperationOptionsUI = class(TWfxPluginCopyMoveOperationOptionsUI)
  end;

  TWfxPluginMoveOperationOptionsUI = class(TWfxPluginCopyMoveOperationOptionsUI)
  end;

  { TWfxPluginCopyInOperationOptionsUI }

  TWfxPluginCopyInOperationOptionsUI = class(TWfxPluginCopyMoveOperationOptionsUI)
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
  end;

  { TWfxPluginCopyOutOperationOptionsUI }

  TWfxPluginCopyOutOperationOptionsUI = class(TWfxPluginCopyMoveOperationOptionsUI)
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uLng, DCOSUtils, WfxPlugin, fCopyMoveDlg, uGlobs, uWfxPluginFileSource,
  uFileSourceOperationOptions, uOperationsManager;

{ TWfxPluginCopyMoveOperationOptionsUI }

constructor TWfxPluginCopyMoveOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited Create(AOwner, AFileSource);
  ParseLineToList(rsFileOpCopyMoveFileExistsOptions, cmbFileExists.Items);

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;

  with (AFileSource as IWfxPluginFileSource).WfxModule do
  begin
    cbCopyTime.Visible := Assigned(FsSetTime) or Assigned(FsSetTimeW);
    cbCopyTime.Checked := cbCopyTime.Visible and gOperationOptionCopyTime;
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SaveOptions;
begin
  // TODO: Saving options for each file source operation separately.
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(Operation: TObject);
begin
  if Operation is TWfxPluginCopyOperation then
    SetOperationOptions(Operation as TWfxPluginCopyOperation)
  else if Operation is TWfxPluginMoveOperation then
    SetOperationOptions(Operation as TWfxPluginMoveOperation)
  else if Operation is TWfxPluginCopyInOperation then
    SetOperationOptions(Operation as TWfxPluginCopyInOperation)
  else if Operation is TWfxPluginCopyOutOperation then
    SetOperationOptions(Operation as TWfxPluginCopyOutOperation);
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.cbWorkInBackgroundChange(
  Sender: TObject);
begin
  with (Owner as TfrmCopyDlg) do
  begin
    if not cbWorkInBackground.Checked then
      QueueIdentifier:= ModalQueueId
    else begin
      QueueIdentifier:= SingleQueueId;
    end;
    btnAddToQueue.Visible:= cbWorkInBackground.Checked;
    btnCreateSpecialQueue.Visible:= btnAddToQueue.Visible;
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetCopyOptions(CopyOperation: TFileSourceCopyOperation);
begin
  with CopyOperation do
  begin
    if cbCopyTime.Checked then
      CopyAttributesOptions := CopyAttributesOptions + [caoCopyTime]
    else begin
      CopyAttributesOptions := CopyAttributesOptions - [caoCopyTime];
    end;
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(CopyOperation: TWfxPluginCopyOperation);
begin
  with CopyOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    SetCopyOptions(CopyOperation);
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(MoveOperation: TWfxPluginMoveOperation);
begin
  with MoveOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(CopyInOperation: TWfxPluginCopyInOperation);
begin
  with CopyInOperation do
  begin
    NeedsConnection:= not cbWorkInBackground.Checked;
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    SetCopyOptions(CopyInOperation);
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(CopyOutOperation: TWfxPluginCopyOutOperation);
begin
  with CopyOutOperation do
  begin
    NeedsConnection:= not cbWorkInBackground.Checked;
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    SetCopyOptions(CopyOutOperation);
  end;
end;

{ TWfxPluginCopyInOperationOptionsUI }

constructor TWfxPluginCopyInOperationOptionsUI.Create(AOwner: TComponent;
  AFileSource: IInterface);
const
  CAN_UPLOAD = BG_UPLOAD or BG_ASK_USER;
begin
  inherited Create(AOwner, AFileSource);
  with (AFileSource as IWfxPluginFileSource) do
  begin
    cbWorkInBackground.Visible:= (WfxModule.BackgroundFlags and CAN_UPLOAD) = CAN_UPLOAD;
    if cbWorkInBackground.Visible then
      cbWorkInBackground.Checked:= False
    else
      cbWorkInBackground.Checked:= (WfxModule.BackgroundFlags and BG_UPLOAD <> 0);
  end;
  cbWorkInBackgroundChange(cbWorkInBackground);
end;

{ TWfxPluginCopyOutOperationOptionsUI }

constructor TWfxPluginCopyOutOperationOptionsUI.Create(AOwner: TComponent;
  AFileSource: IInterface);
const
  CAN_DOWNLOAD = BG_DOWNLOAD or BG_ASK_USER;
begin
  inherited Create(AOwner, AFileSource);
  with (AFileSource as IWfxPluginFileSource) do
  begin
    cbWorkInBackground.Visible:= (WfxModule.BackgroundFlags and CAN_DOWNLOAD) = CAN_DOWNLOAD;
    if cbWorkInBackground.Visible then
      cbWorkInBackground.Checked:= False
    else
      cbWorkInBackground.Checked:= (WfxModule.BackgroundFlags and BG_DOWNLOAD <> 0);
  end;
  cbWorkInBackgroundChange(cbWorkInBackground);
end;

end.


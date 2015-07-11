unit fGioCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI,
  uGioCopyOperation,
  uGioMoveOperation;

type

  { TGioCopyMoveOperationOptionsUI }

  TGioCopyMoveOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cmbFileExists: TComboBox;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
    pnlCheckboxes: TPanel;
    pnlComboBoxes: TPanel;
  private
    procedure SetOperationOptions(CopyOperation: TGioCopyOperation); overload;
    procedure SetOperationOptions(MoveOperation: TGioMoveOperation); overload;
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

  TGioCopyOperationOptionsUI = class(TGioCopyMoveOperationOptionsUI)
  end;

  TGioMoveOperationOptionsUI = class(TGioCopyMoveOperationOptionsUI)
  end;

implementation

{$R *.lfm}

uses
  WfxPlugin, uGlobs, uWfxPluginFileSource, uFileSourceOperationOptions;

{ TGioCopyMoveOperationOptionsUI }

constructor TGioCopyMoveOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited Create(AOwner, AFileSource);

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;
end;

procedure TGioCopyMoveOperationOptionsUI.SaveOptions;
begin
  // TODO: Saving options for each file source operation separately.
end;

procedure TGioCopyMoveOperationOptionsUI.SetOperationOptions(Operation: TObject);
begin
  if Operation is TGioCopyOperation then
    SetOperationOptions(Operation as TGioCopyOperation)
  else if Operation is TGioMoveOperation then
    SetOperationOptions(Operation as TGioMoveOperation);
end;

procedure TGioCopyMoveOperationOptionsUI.SetOperationOptions(
  CopyOperation: TGioCopyOperation);
begin
  with CopyOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
end;

procedure TGioCopyMoveOperationOptionsUI.SetOperationOptions(
  MoveOperation: TGioMoveOperation);
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

end.


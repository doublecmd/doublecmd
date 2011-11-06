unit fMultiArchiveCopyOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI,
  uMultiArchiveCopyInOperation,
  uMultiArchiveCopyOutOperation;

type

  { TMultiArchiveCopyOperationOptionsUI }

  TMultiArchiveCopyOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cmbFileExists: TComboBox;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
  private
    procedure SetOperationOptions(CopyInOperation: TMultiArchiveCopyInOperation); overload;
    procedure SetOperationOptions(CopyOutOperation: TMultiArchiveCopyOutOperation); overload;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uFileSourceOperationOptions;

{ TMultiArchiveCopyOperationOptionsUI }

constructor TMultiArchiveCopyOperationOptionsUI.Create(AOwner: TComponent);
begin
  inherited;

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;
end;

procedure TMultiArchiveCopyOperationOptionsUI.SaveOptions;
begin
  // TODO: Saving options for each file source operation separately.
end;

procedure TMultiArchiveCopyOperationOptionsUI.SetOperationOptions(Operation: TObject);
begin
  if Operation is TMultiArchiveCopyInOperation then
    SetOperationOptions(Operation as TMultiArchiveCopyInOperation)
  else if Operation is TMultiArchiveCopyOutOperation then
    SetOperationOptions(Operation as TMultiArchiveCopyOutOperation);
end;

procedure TMultiArchiveCopyOperationOptionsUI.SetOperationOptions(CopyInOperation: TMultiArchiveCopyInOperation);
begin
  {
  with CopyInOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
  }
end;

procedure TMultiArchiveCopyOperationOptionsUI.SetOperationOptions(CopyOutOperation: TMultiArchiveCopyOutOperation);
begin
  with CopyOutOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
end;

end.

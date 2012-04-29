unit fWcxArchiveCopyOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI,
  uWcxArchiveCopyInOperation,
  uWcxArchiveCopyOutOperation;

type

  { TWcxArchiveCopyOperationOptionsUI }

  TWcxArchiveCopyOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cmbFileExists: TComboBox;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
  private
    procedure SetOperationOptions(CopyInOperation: TWcxArchiveCopyInOperation); overload;
    procedure SetOperationOptions(CopyOutOperation: TWcxArchiveCopyOutOperation); overload;
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uFileSourceOperationOptions;

{ TWcxArchiveCopyOperationOptionsUI }

constructor TWcxArchiveCopyOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited;

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;
end;

procedure TWcxArchiveCopyOperationOptionsUI.SaveOptions;
begin
  // TODO: Saving options for each file source operation separately.
end;

procedure TWcxArchiveCopyOperationOptionsUI.SetOperationOptions(Operation: TObject);
begin
  if Operation is TWcxArchiveCopyInOperation then
    SetOperationOptions(Operation as TWcxArchiveCopyInOperation)
  else if Operation is TWcxArchiveCopyOutOperation then
    SetOperationOptions(Operation as TWcxArchiveCopyOutOperation);
end;

procedure TWcxArchiveCopyOperationOptionsUI.SetOperationOptions(CopyInOperation: TWcxArchiveCopyInOperation);
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

procedure TWcxArchiveCopyOperationOptionsUI.SetOperationOptions(CopyOutOperation: TWcxArchiveCopyOutOperation);
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


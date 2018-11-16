unit fWcxArchiveCopyOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI;

type

  { TWcxArchiveCopyOperationOptionsUI }

  TWcxArchiveCopyOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cmbFileExists: TComboBox;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uLng, uGlobs, uFileSourceOperationOptions, uFileSourceCopyOperation;

{ TWcxArchiveCopyOperationOptionsUI }

constructor TWcxArchiveCopyOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited;
  ParseLineToList(rsFileOpCopyMoveFileExistsOptions, cmbFileExists.Items);

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
  with Operation as TFileSourceCopyOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
end;

end.


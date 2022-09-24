unit fWcxArchiveCopyOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI, uWcxArchiveFileSource, uWcxArchiveCopyInOperation;

type

  { TWcxArchiveCopyOperationOptionsUI }

  TWcxArchiveCopyOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    btnConfig: TButton;
    cbEncrypt: TCheckBox;
    cmbFileExists: TComboBox;
    lblFileExists: TLabel;
    pnlCheckboxes: TPanel;
    pnlComboBoxes: TPanel;
    procedure btnConfigClick(Sender: TObject);
  private
    FFileSource: IWcxArchiveFileSource;
    procedure SetOperationOptions(CopyInOperation: TWcxArchiveCopyInOperation); overload;
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

  { TWcxArchiveCopyInOperationOptionsUI }

  TWcxArchiveCopyInOperationOptionsUI = class(TWcxArchiveCopyOperationOptionsUI)
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
  end;

implementation

{$R *.lfm}

uses
  Dialogs, DCStrUtils, WcxPlugin, uLng, uGlobs, uFileSourceOperationOptions,
  uFileSourceCopyOperation;

{ TWcxArchiveCopyInOperationOptionsUI }

constructor TWcxArchiveCopyInOperationOptionsUI.Create(AOwner: TComponent;
  AFileSource: IInterface);
begin
  FFileSource := AFileSource as IWcxArchiveFileSource;
  inherited Create(AOwner, AFileSource);
  pnlCheckboxes.Visible := True;
  btnConfig.Visible := True;
end;

{ TWcxArchiveCopyOperationOptionsUI }

procedure TWcxArchiveCopyOperationOptionsUI.btnConfigClick(Sender: TObject);
begin
  try
    FFileSource.WcxModule.VFSConfigure(Handle);
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TWcxArchiveCopyOperationOptionsUI.SetOperationOptions(
  CopyInOperation: TWcxArchiveCopyInOperation);
var
  AFlags: Integer;
begin
  AFlags := CopyInOperation.PackingFlags;
  if cbEncrypt.Checked then AFlags := AFlags or PK_PACK_ENCRYPT;
  CopyInOperation.PackingFlags := AFlags;
end;

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
  if Operation is TWcxArchiveCopyInOperation then
    SetOperationOptions(TWcxArchiveCopyInOperation(Operation));
end;

end.


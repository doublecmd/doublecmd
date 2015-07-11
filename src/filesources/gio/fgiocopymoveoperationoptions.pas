unit fGioCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI, KASComboBox,
  uGioCopyOperation,
  uGioMoveOperation;

type

  { TGioCopyMoveOperationOptionsUI }

  TGioCopyMoveOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cbFollowLinks: TCheckBox;
    cmbDirectoryExists: TComboBoxAutoWidth;
    cmbFileExists: TComboBoxAutoWidth;
    grpOptions: TGroupBox;
    lblDirectoryExists: TLabel;
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

  { TGioMoveOperationOptionsUI }

  TGioMoveOperationOptionsUI = class(TGioCopyMoveOperationOptionsUI)
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uLng, uGlobs, uFileSourceOperationOptions;

{ TGioCopyMoveOperationOptionsUI }

constructor TGioCopyMoveOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited Create(AOwner, AFileSource);

  ParseLineToList(rsFileOpFileExistsOptions, cmbFileExists.Items);
  ParseLineToList(rsFileOpDirectoryExistsOptions, cmbDirectoryExists.Items);

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;

  case gOperationOptionDirectoryExists of
    fsoodeNone     : cmbDirectoryExists.ItemIndex := 0;
    fsoodeCopyInto : cmbDirectoryExists.ItemIndex := 1;
    fsoodeSkip     : cmbDirectoryExists.ItemIndex := 2;
  end;

  case gOperationOptionSymLinks of
    fsooslFollow     : cbFollowLinks.State := cbChecked;
    fsooslDontFollow : cbFollowLinks.State := cbUnchecked;
    fsooslNone       : cbFollowLinks.State := cbGrayed;
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

    case cmbDirectoryExists.ItemIndex of
      0: DirExistsOption := fsoodeNone;
      1: DirExistsOption := fsoodeCopyInto;
      2: DirExistsOption := fsoodeSkip;
    end;

    case cbFollowLinks.State of
      cbChecked  : SymLinkOption := fsooslFollow;
      cbUnchecked: SymLinkOption := fsooslDontFollow;
      cbGrayed   : SymLinkOption := fsooslNone;
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

    case cmbDirectoryExists.ItemIndex of
      0: DirExistsOption := fsoodeNone;
      1: DirExistsOption := fsoodeCopyInto;
      2: DirExistsOption := fsoodeSkip;
    end;
  end;
end;

{ TGioMoveOperationOptionsUI }

constructor TGioMoveOperationOptionsUI.Create(AOwner: TComponent;
  AFileSource: IInterface);
begin
  inherited Create(AOwner, AFileSource);
  cbFollowLinks.Visible:= False;
end;

end.


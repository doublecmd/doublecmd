unit fFileSystemCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI,
  uFileSourceOperation,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation;

type

  { TFileSystemCopyMoveOperationOptionsUI }

  TFileSystemCopyMoveOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cbCheckFreeSpace: TCheckBox;
    cbCorrectLinks: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbFollowLinks: TCheckBox;
    cmbDirectoryExists: TComboBox;
    cmbFileExists: TComboBox;
    cmbFileType: TComboBox;
    grpOptions: TGroupBox;
    lblDirectoryExists: TLabel;
    lblFileExists: TLabel;
    lblFileType: TLabel;
    pnlComboBoxes: TPanel;
    pnlCheckboxes: TPanel;
  private
    procedure SetOperationOptions(CopyOperation: TFileSystemCopyOperation); overload;
    procedure SetOperationOptions(MoveOperation: TFileSystemMoveOperation); overload;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

  TFileSystemCopyOperationOptionsUI = class(TFileSystemCopyMoveOperationOptionsUI)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TFileSystemMoveOperationOptionsUI = class(TFileSystemCopyMoveOperationOptionsUI)
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uFileSourceOperationOptions;

{ TFileSystemCopyMoveOperationOptionsUI }

constructor TFileSystemCopyMoveOperationOptionsUI.Create(AOwner: TComponent);
begin
  inherited;

  if not gOverwriteFolder then cmbDirectoryExists.Items.Delete(1);

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;

  if gOverwriteFolder then
    case gOperationOptionDirectoryExists of
      fsoodeNone     : cmbDirectoryExists.ItemIndex := 0;
      fsoodeDelete   : cmbDirectoryExists.ItemIndex := 1;
      fsoodeCopyInto : cmbDirectoryExists.ItemIndex := 2;
      fsoodeSkip     : cmbDirectoryExists.ItemIndex := 3;
    end
  else
    case gOperationOptionDirectoryExists of
      fsoodeNone     : cmbDirectoryExists.ItemIndex := 0;
      fsoodeCopyInto : cmbDirectoryExists.ItemIndex := 1;
      fsoodeSkip     : cmbDirectoryExists.ItemIndex := 2;
    end;

  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;

  case gOperationOptionSymLinks of
    fsooslFollow     : cbFollowLinks.State := cbChecked;
    fsooslDontFollow : cbFollowLinks.State := cbUnchecked;
    fsooslNone       : cbFollowLinks.State := cbGrayed;
  end;

  cbCorrectLinks.Checked := gOperationOptionCorrectLinks;
  cbCheckFreeSpace.Checked := gOperationOptionCheckFreeSpace;
end;

procedure TFileSystemCopyMoveOperationOptionsUI.SaveOptions;
begin
  case cmbFileExists.ItemIndex of
    0: gOperationOptionFileExists := fsoofeNone;
    1: gOperationOptionFileExists := fsoofeOverwrite;
    2: gOperationOptionFileExists := fsoofeSkip;
  end;
  if gOverwriteFolder then
    case cmbDirectoryExists.ItemIndex of
      0: gOperationOptionDirectoryExists := fsoodeNone;
      1: gOperationOptionDirectoryExists := fsoodeDelete;
      2: gOperationOptionDirectoryExists := fsoodeCopyInto;
      3: gOperationOptionDirectoryExists := fsoodeSkip;
    end
  else
    case cmbDirectoryExists.ItemIndex of
      0: gOperationOptionDirectoryExists := fsoodeNone;
      1: gOperationOptionDirectoryExists := fsoodeCopyInto;
      2: gOperationOptionDirectoryExists := fsoodeSkip;
    end;
  gDropReadOnlyFlag := (cbDropReadOnlyFlag.State = cbChecked);
  case cbFollowLinks.State of
    cbChecked   : gOperationOptionSymLinks := fsooslFollow;
    cbUnchecked : gOperationOptionSymLinks := fsooslDontFollow;
    cbGrayed    : gOperationOptionSymLinks := fsooslNone;
  end;
  gOperationOptionCorrectLinks := cbCorrectLinks.Checked;
  gOperationOptionCheckFreeSpace := cbCheckFreeSpace.Checked;
end;

procedure TFileSystemCopyMoveOperationOptionsUI.SetOperationOptions(Operation: TObject);
begin
  if Operation is TFileSystemCopyOperation then
    SetOperationOptions(Operation as TFileSystemCopyOperation)
  else if Operation is TFileSystemMoveOperation then
    SetOperationOptions(Operation as TFileSystemMoveOperation);
end;

procedure TFileSystemCopyMoveOperationOptionsUI.SetOperationOptions(CopyOperation: TFileSystemCopyOperation);
begin
  with CopyOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    if gOverwriteFolder then
      case cmbDirectoryExists.ItemIndex of
        0: DirExistsOption := fsoodeNone;
        1: DirExistsOption := fsoodeDelete;
        2: DirExistsOption := fsoodeCopyInto;
        3: DirExistsOption := fsoodeSkip;
      end
    else
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
    DropReadOnlyAttribute := (cbDropReadOnlyFlag.State = cbChecked);
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
  end;
end;

procedure TFileSystemCopyMoveOperationOptionsUI.SetOperationOptions(MoveOperation: TFileSystemMoveOperation);
begin
  with MoveOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    if gOverwriteFolder then
      case cmbDirectoryExists.ItemIndex of
        0: DirExistsOption := fsoodeNone;
        1: DirExistsOption := fsoodeDelete;
        2: DirExistsOption := fsoodeCopyInto;
        3: DirExistsOption := fsoodeSkip;
      end
    else
      case cmbDirectoryExists.ItemIndex of
        0: DirExistsOption := fsoodeNone;
        1: DirExistsOption := fsoodeCopyInto;
        2: DirExistsOption := fsoodeSkip;
      end;
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
  end;
end;

{ TFileSystemCopyOperationOptionsUI }

constructor TFileSystemCopyOperationOptionsUI.Create(AOwner: TComponent);
begin
  inherited;
  cbDropReadOnlyFlag.Visible := True;
  cbFollowLinks.Visible := True;
end;

end.


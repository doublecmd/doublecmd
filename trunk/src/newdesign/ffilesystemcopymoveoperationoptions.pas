unit fFileSystemCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Buttons,
  uFileSourceOperationOptionsUI,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation,
  uSearchTemplate;

type

  { TFileSystemCopyMoveOperationOptionsUI }

  TFileSystemCopyMoveOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    btnSearchTemplate: TBitBtn;
    cbCheckFreeSpace: TCheckBox;
    cbCorrectLinks: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbFollowLinks: TCheckBox;
    cbCopyAttributes: TCheckBox;
    cbCopyTime: TCheckBox;
    cbCopyOwnership: TCheckBox;
    cbExcludeEmptyDirectories: TCheckBox;
    cmbDirectoryExists: TComboBox;
    cmbFileExists: TComboBox;
    cmbSetPropertyError: TComboBox;
    gbFileTemplate: TGroupBox;
    grpOptions: TGroupBox;
    lblSetPropertyError: TLabel;
    lblTemplateName: TLabel;
    lblDirectoryExists: TLabel;
    lblFileExists: TLabel;
    pnlCopyAttributesTime: TPanel;
    pnlComboBoxes: TPanel;
    pnlCheckboxes: TPanel;
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure cbCopyAttributesChange(Sender: TObject);
  private
    FTemplate: TSearchTemplate;
    procedure SetOperationOptions(CopyOperation: TFileSystemCopyOperation); overload;
    procedure SetOperationOptions(MoveOperation: TFileSystemMoveOperation); overload;
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
    destructor Destroy; override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

  TFileSystemCopyOperationOptionsUI = class(TFileSystemCopyMoveOperationOptionsUI)
  public
    constructor Create(AOwner: TComponent; AFileSource: IInterface); override;
  end;

  TFileSystemMoveOperationOptionsUI = class(TFileSystemCopyMoveOperationOptionsUI)
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng, uFileSourceOperationOptions, DCOSUtils, DCStrUtils,
  fFindDlg;

procedure SetCopyOption(var Options: TCopyAttributesOptions; Option: TCopyAttributesOption; IsSet: Boolean);
begin
  if IsSet then
    Options := Options + [Option]
  else
    Options := Options - [Option];
end;

{ TFileSystemCopyMoveOperationOptionsUI }

procedure TFileSystemCopyMoveOperationOptionsUI.btnSearchTemplateClick(Sender: TObject);
begin
  if ShowUseTemplateDlg(FTemplate) and Assigned(FTemplate) then
  begin
    if FTemplate.TemplateName = '' then
      lblTemplateName.Caption := rsSearchTemplateUnnamed
    else
      lblTemplateName.Caption := FTemplate.TemplateName;
  end;
end;

procedure TFileSystemCopyMoveOperationOptionsUI.cbCopyAttributesChange(Sender: TObject);
begin
  cbDropReadOnlyFlag.Enabled := cbCopyAttributes.Checked;
end;

constructor TFileSystemCopyMoveOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited;

  {$IFDEF MSWINDOWS}
  cbCopyOwnership.Visible := False;
  {$ENDIF}

  ParseLineToList(rsFileOpFileExistsOptions, cmbFileExists.Items);
  ParseLineToList(rsFileOpDirectoryExistsOptions, cmbDirectoryExists.Items);
  ParseLineToList(rsFileOpSetPropertyErrorOptions, cmbSetPropertyError.Items);

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

  case gOperationOptionSetPropertyError of
    fsoospeNone         : cmbSetPropertyError.ItemIndex := 0;
    fsoospeDontSet      : cmbSetPropertyError.ItemIndex := 1;
    fsoospeIgnoreErrors : cmbSetPropertyError.ItemIndex := 2;
  end;

  cbCopyAttributes.Checked   := gOperationOptionCopyAttributes;
  cbCopyTime.Checked         := gOperationOptionCopyTime;
  cbCopyOwnership.Checked    := gOperationOptionCopyOwnership;
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;

  case gOperationOptionSymLinks of
    fsooslFollow     : cbFollowLinks.State := cbChecked;
    fsooslDontFollow : cbFollowLinks.State := cbUnchecked;
    fsooslNone       : cbFollowLinks.State := cbGrayed;
  end;

  cbCorrectLinks.Checked := gOperationOptionCorrectLinks;
  cbCheckFreeSpace.Checked := gOperationOptionCheckFreeSpace;
end;

destructor TFileSystemCopyMoveOperationOptionsUI.Destroy;
begin
  inherited Destroy;
  FTemplate.Free;
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
  case cmbSetPropertyError.ItemIndex of
    0: gOperationOptionSetPropertyError := fsoospeNone;
    1: gOperationOptionSetPropertyError := fsoospeDontSet;
    2: gOperationOptionSetPropertyError := fsoospeIgnoreErrors;
  end;

  gOperationOptionCopyAttributes := cbCopyAttributes.Checked;
  gOperationOptionCopyTime       := cbCopyTime.Checked;
  gOperationOptionCopyOwnership  := cbCopyOwnership.Checked;
  gDropReadOnlyFlag              := cbDropReadOnlyFlag.Checked;

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
var
  Options: TCopyAttributesOptions;
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
    case cmbSetPropertyError.ItemIndex of
      0: SetPropertyError := fsoospeNone;
      1: SetPropertyError := fsoospeDontSet;
      2: SetPropertyError := fsoospeIgnoreErrors;
    end;
    case cbFollowLinks.State of
      cbChecked  : SymLinkOption := fsooslFollow;
      cbUnchecked: SymLinkOption := fsooslDontFollow;
      cbGrayed   : SymLinkOption := fsooslNone;
    end;
    Options := CopyAttributesOptions;
    SetCopyOption(Options, caoCopyAttributes, cbCopyAttributes.Checked);
    SetCopyOption(Options, caoCopyTime, cbCopyTime.Checked);
    SetCopyOption(Options, caoCopyOwnership, cbCopyOwnership.Checked);
    SetCopyOption(Options, caoRemoveReadOnlyAttr, cbDropReadOnlyFlag.Checked);
    CopyAttributesOptions := Options;
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
    if Assigned(FTemplate) then
    begin
      SearchTemplate := FTemplate;
      FTemplate := nil;
    end;
    ExcludeEmptyTemplateDirectories := cbExcludeEmptyDirectories.Checked;
  end;
end;

procedure TFileSystemCopyMoveOperationOptionsUI.SetOperationOptions(MoveOperation: TFileSystemMoveOperation);
var
  Options: TCopyAttributesOptions;
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
    case cmbSetPropertyError.ItemIndex of
      0: SetPropertyError := fsoospeNone;
      1: SetPropertyError := fsoospeDontSet;
      2: SetPropertyError := fsoospeIgnoreErrors;
    end;
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
    Options := CopyAttributesOptions;
    SetCopyOption(Options, caoCopyAttributes, cbCopyAttributes.Checked);
    SetCopyOption(Options, caoCopyTime, cbCopyTime.Checked);
    SetCopyOption(Options, caoCopyOwnership, cbCopyOwnership.Checked);
    CopyAttributesOptions := Options;
    if Assigned(FTemplate) then
    begin
      SearchTemplate := FTemplate;
      FTemplate := nil;
    end;
    ExcludeEmptyTemplateDirectories := cbExcludeEmptyDirectories.Checked;
  end;
end;

{ TFileSystemCopyOperationOptionsUI }

constructor TFileSystemCopyOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited;
  cbDropReadOnlyFlag.Visible := True;
  cbFollowLinks.Visible := True;
end;

end.


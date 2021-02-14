unit fFileSystemCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Buttons,
  uFileSourceOperationOptionsUI,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation,
  uSearchTemplate, KASComboBox;

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
    cbReserveSpace: TCheckBox;
    cbCopyPermissions: TCheckBox;
    chkVerify: TCheckBox;
    cmbDirectoryExists: TComboBoxAutoWidth;
    cmbFileExists: TComboBoxAutoWidth;
    cmbSetPropertyError: TComboBoxAutoWidth;
    gbFileTemplate: TGroupBox;
    grpOptions: TGroupBox;
    lblSetPropertyError: TLabel;
    lblTemplateName: TLabel;
    lblDirectoryExists: TLabel;
    lblFileExists: TLabel;
    pnlComboBoxes: TPanel;
    pnlCheckboxes: TPanel;
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure cbCopyAttributesChange(Sender: TObject);
    procedure cbReserveSpaceChange(Sender: TObject);
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
  fFindDlg, uFileCopyEx;

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

procedure TFileSystemCopyMoveOperationOptionsUI.cbReserveSpaceChange(Sender: TObject);
begin
  if (cbReserveSpace.Checked = False) then
    cbCheckFreeSpace.Checked := Boolean(cbCheckFreeSpace.Tag)
  else
    begin
      cbCheckFreeSpace.Tag := PtrInt(cbCheckFreeSpace.Checked);
      cbCheckFreeSpace.Checked := cbReserveSpace.Checked;
    end;
  cbCheckFreeSpace.Enabled := not cbReserveSpace.Checked;
end;

constructor TFileSystemCopyMoveOperationOptionsUI.Create(AOwner: TComponent; AFileSource: IInterface);
begin
  inherited;

  {$IFDEF DARWIN}
  chkVerify.Visible := False;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  cbCopyOwnership.Visible := False;
  cbCopyPermissions.Visible := True;
  {$ENDIF}

  if Assigned(FileCopyEx) then
  begin
    cbCopyTime.Visible:= False;
    cbReserveSpace.Visible:= False;
    cbCopyAttributes.Visible:= False;
  end;

  ParseLineToList(rsFileOpFileExistsOptions, cmbFileExists.Items);
  ParseLineToList(rsFileOpDirectoryExistsOptions, cmbDirectoryExists.Items);
  ParseLineToList(rsFileOpSetPropertyErrorOptions, cmbSetPropertyError.Items);

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone          : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite     : cmbFileExists.ItemIndex := 1;
    fsoofeOverwriteOlder: cmbFileExists.ItemIndex := 2;
    fsoofeSkip          : cmbFileExists.ItemIndex := 3;
  end;

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
  cbCopyPermissions.Checked  := gOperationOptionCopyPermissions;
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;

  case gOperationOptionSymLinks of
    fsooslFollow     : cbFollowLinks.State := cbChecked;
    fsooslDontFollow : cbFollowLinks.State := cbUnchecked;
    fsooslNone       : cbFollowLinks.State := cbGrayed;
  end;

  chkVerify.Checked := gOperationOptionVerify;
  cbCorrectLinks.Checked := gOperationOptionCorrectLinks;
  cbReserveSpace.Checked := gOperationOptionReserveSpace;
  cbCheckFreeSpace.Checked := gOperationOptionCheckFreeSpace;
  cbExcludeEmptyDirectories.Checked := gOperationOptionExcludeEmptyDirectories;
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
    2: gOperationOptionFileExists := fsoofeOverwriteOlder;
    3: gOperationOptionFileExists := fsoofeSkip;
  end;
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

  gOperationOptionVerify          :=  chkVerify.Checked;
  gOperationOptionCopyAttributes  := cbCopyAttributes.Checked;
  gOperationOptionCopyTime        := cbCopyTime.Checked;
  gOperationOptionCopyOwnership   := cbCopyOwnership.Checked;
  gOperationOptionCopyPermissions := cbCopyPermissions.Checked;
  gDropReadOnlyFlag               := cbDropReadOnlyFlag.Checked;

  case cbFollowLinks.State of
    cbChecked   : gOperationOptionSymLinks := fsooslFollow;
    cbUnchecked : gOperationOptionSymLinks := fsooslDontFollow;
    cbGrayed    : gOperationOptionSymLinks := fsooslNone;
  end;
  gOperationOptionCorrectLinks := cbCorrectLinks.Checked;
  gOperationOptionReserveSpace := cbReserveSpace.Checked;
  gOperationOptionCheckFreeSpace := cbCheckFreeSpace.Checked;
  gOperationOptionExcludeEmptyDirectories := cbExcludeEmptyDirectories.Checked;
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
      2: FileExistsOption := fsoofeOverwriteOlder;
      3: FileExistsOption := fsoofeSkip;
    end;
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
    SetCopyOption(Options, caoCopyPermissions, cbCopyPermissions.Checked);
    SetCopyOption(Options, caoRemoveReadOnlyAttr, cbDropReadOnlyFlag.Checked);
    CopyAttributesOptions := Options;
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
    ReserveSpace := cbReserveSpace.Checked;
    Verify := chkVerify.Checked;
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
      2: FileExistsOption := fsoofeOverwriteOlder;
      3: FileExistsOption := fsoofeSkip;
    end;
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
    ReserveSpace := cbReserveSpace.Checked;
    Verify := chkVerify.Checked;
    Options := CopyAttributesOptions;
    SetCopyOption(Options, caoCopyAttributes, cbCopyAttributes.Checked);
    SetCopyOption(Options, caoCopyTime, cbCopyTime.Checked);
    SetCopyOption(Options, caoCopyOwnership, cbCopyOwnership.Checked);
    SetCopyOption(Options, caoCopyPermissions, cbCopyPermissions.Checked);
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
  cbFollowLinks.Visible := True;
  cbDropReadOnlyFlag.Visible := (FileCopyEx = nil);
end;

end.


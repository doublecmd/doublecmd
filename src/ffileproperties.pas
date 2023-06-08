{
   Double Commander
   -------------------------------------------------------------------
   File Properties Dialog

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2003 Martin Matusu <xmat@volny.cz>
   Copyright (C) 2006-2023 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fFileProperties;

{$mode objfpc}{$H+}

interface

uses
  LResources, SysUtils, Classes, Graphics, Forms, StdCtrls, Buttons, ComCtrls,
  Dialogs, Controls, ExtCtrls, Grids, ButtonPanel, DividerBevel, KASCDEdit,
  DCBasicTypes, uFile, uFileProperty, uFileSource, uFileSourceOperation,
  uFileSourceCalcStatisticsOperation, uFileSourceSetFilePropertyOperation,
  DCOSUtils;

type

  { TfrmFileProperties }

  TfrmFileProperties = class(TForm)
    ButtonPanel: TButtonPanel;
    cbExecGroup: TCheckBox;
    cbExecOther: TCheckBox;
    cbExecOwner: TCheckBox;
    cbReadGroup: TCheckBox;
    cbReadOther: TCheckBox;
    cbReadOwner: TCheckBox;
    cbSgid: TCheckBox;
    cbSticky: TCheckBox;
    cbSuid: TCheckBox;
    cbWriteGroup: TCheckBox;
    cbWriteOther: TCheckBox;
    cbWriteOwner: TCheckBox;
    cbxGroups: TComboBox;
    cbxUsers: TComboBox;
    chkExecutable: TCheckBox;
    chkRecursive: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    edtOctal: TEdit;
    lblExecutable: TLabel;
    lblFileName: TLabel;
    imgFileIcon: TImage;
    lblFolder: TKASCDEdit;
    lblFolderStr: TLabel;
    lblGroupStr: TLabel;
    lblLastAccess: TKASCDEdit;
    lblLastAccessStr: TLabel;
    lblLastModif: TKASCDEdit;
    lblLastModifStr: TLabel;
    lblLastStChange: TKASCDEdit;
    lblLastStChangeStr: TLabel;
    lblCreated: TKASCDEdit;
    lblCreatedStr: TLabel;
    lblOctal: TLabel;
    lblAttrBitsStr: TLabel;
    lblAttrText: TLabel;
    lblExec: TLabel;
    lblFileStr: TLabel;
    lblFile: TLabel;
    lblAttrGroupStr: TLabel;
    lblAttrOtherStr: TLabel;
    lblAttrOwnerStr: TLabel;
    lblOwnerStr: TLabel;

    lblRead: TLabel;
    lblSize: TKASCDEdit;
    lblSizeOnDisk: TKASCDEdit;
    lblContains: TKASCDEdit;
    lblSizeStr: TLabel;
    lblSizeOnDiskStr: TLabel;
    lblContainsStr: TLabel;
    lblSymlink: TKASCDEdit;
    lblAttrTextStr: TLabel;
    lblSymlinkStr: TLabel;
    lblMediaType: TKASCDEdit;
    lblMediaTypeStr: TLabel;
    lblType: TKASCDEdit;
    lblTypeStr: TLabel;
    lblLinks: TKASCDEdit;
    lblLinksStr: TLabel;
    lblWrite: TLabel;

    pnlOwner: TPanel;
    pnlCaption: TPanel;
    pnlData: TPanel;
    pnlIcon: TPanel;
    pcPageControl: TPageControl;
    sgPlugins: TStringGrid;
    tsPlugins: TTabSheet;
    tmUpdateFolderSize: TTimer;
    tsProperties: TTabSheet;
    tsAttributes: TTabSheet;
    procedure cbChangeModeClick(Sender: TObject);
    procedure chkExecutableChange(Sender: TObject);
    procedure edtOctalKeyPress(Sender: TObject; var Key: char);
    procedure edtOctalKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OKButtonClick(Sender: TObject);
    procedure tmUpdateFolderSizeTimer(Sender: TObject);
    procedure FileSourceOperationStateChangedNotify(Operation: TFileSourceOperation;
                                                               State: TFileSourceOperationState);
  private
    bPerm: Boolean;
    FFileSource: IFileSource;
    FFiles: TFiles;
    FPropertyFormatter: IFilePropertyFormatter;
    FFileSourceCalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
    FChangeTriggersEnabled: Boolean;
    FFileAttr: TFileAttributeData;
    FFileType,
    OriginalAttr: TFileAttrs;
    OriginalUser, OriginalGroup: String;
    FOperation: TFileSourceSetFilePropertyOperation;

    procedure ShowType(Attrs: TFileAttrs);
    procedure ShowExecutable;
    procedure ShowPermissions(Mode: TFileAttrs);
    function GetModeFromForm(out ExcludeAttrs: TFileAttrs): TFileAttrs;
    function FormatSize(ASize: Int64): String;
    procedure ShowMany;
    procedure ShowFile(iIndex:Integer);
    procedure StartCalcFolderSize;
    procedure StopCalcFolderSize;
    procedure ShowPlugin(iIndex:Integer);
    procedure UpdateAllowGrayed(AllowGrayed: Boolean);
    function FormatUnixAttributesEx(iAttr: TFileAttrs): String;
  public
    constructor Create(AOwner: TComponent; aFileSource: IFileSource; theFiles: TFiles); reintroduce;
    destructor Destroy; override;
  end;


procedure ShowFileProperties(aFileSource: IFileSource; const aFiles: TFiles);

implementation

{$R *.lfm}

uses
  LCLType, LazUTF8, uLng, BaseUnix, uUsersGroups, uDCUtils,
  uDefaultFilePropertyFormatter, uMyUnix, DCFileAttributes, uGlobs, uWdxModule,
  uFileSourceOperationTypes, uFileSystemFileSource, uOperationsManager, WdxPlugin,
  uFileSourceOperationOptions, uKeyboard, DCStrUtils, uPixMapManager,
  uFileSourceProperty, DCDateTimeUtils, uTypes;

procedure ShowFileProperties(aFileSource: IFileSource; const aFiles: TFiles);
begin
  if aFiles.Count > 0 then
  begin
    with TfrmFileProperties.Create(Application, aFileSource, aFiles) do
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

constructor TfrmFileProperties.Create(AOwner: TComponent; aFileSource: IFileSource; theFiles: TFiles);
var
  ASize: Integer;
  AFiles: TFiles;
  HasAttr: Boolean;
  ActiveFile: TFile;
  aFileProperties: TFileProperties;
begin
  FFiles := theFiles.Clone;
  FFileSource:= aFileSource;
  FChangeTriggersEnabled := True;
  FPropertyFormatter := MaxDetailsFilePropertyFormatter;

  ActiveFile:= FFiles[0];

  HasAttr:= (fspDirectAccess in aFileSource.Properties) and
            (mbFileGetAttr(ActiveFile.FullPath, FFileAttr));

  if HasAttr then
  begin
{$IFDEF UNIX}
    if not (fpOwner in ActiveFile.SupportedProperties) then
    begin
      ActiveFile.Properties[fpOwner]:= TFileOwnerProperty.Create;
    end;
    ActiveFile.OwnerProperty.Group:= FFileAttr.FindData.st_gid;
    ActiveFile.OwnerProperty.Owner:= FFileAttr.FindData.st_uid;
{$ENDIF}
    if fpModificationTime in ActiveFile.SupportedProperties then
      ActiveFile.ModificationTime:= FileTimeToDateTime(DCBasicTypes.TFileTime(FFileAttr.LastWriteTime));
    if fpChangeTime in ActiveFile.SupportedProperties then
      ActiveFile.ChangeTime:= FileTimeToDateTime(DCBasicTypes.TFileTime(FFileAttr.PlatformTime));
    if fpLastAccessTime in ActiveFile.SupportedProperties then
      ActiveFile.LastAccessTime:= FileTimeToDateTime(DCBasicTypes.TFileTime(FFileAttr.LastAccessTime));
  end;

  if (fsoSetFileProperty in aFileSource.GetOperationsTypes) then
  begin
    AFiles:= FFiles.Clone;

    FillByte(aFileProperties, SizeOf(aFileProperties), 0);
    if fpAttributes in ActiveFile.SupportedProperties then
      aFileProperties[fpAttributes]:= ActiveFile.Properties[fpAttributes].Clone;
    if fpOwner in ActiveFile.SupportedProperties then
      aFileProperties[fpOwner]:= ActiveFile.Properties[fpOwner].Clone;

    FOperation:= aFileSource.CreateSetFilePropertyOperation(AFiles,
                    aFileProperties) as TFileSourceSetFilePropertyOperation;
  end;

  inherited Create(AOwner);

  tsProperties.AutoSize:= True;
  tsAttributes.AutoSize:= True;

  // Enable only supported file properties
  if Assigned(FOperation) then
  begin
    if fpAttributes in FOperation.SupportedProperties then
    begin
      UpdateAllowGrayed((FFiles.Count > 1) or FFiles[0].IsDirectory);
    end;
  end;

  ASize:= gIconsSize * Round( Application.MainForm.GetCanvasScaleFactor );
  if ASize > 48 then ASize:= 48;
  imgFileIcon.Width:= ASize;
  imgFileIcon.Height:= ASize;

  pnlOwner.Enabled:= Assigned(FOperation) and (fpOwner in FOperation.SupportedProperties);
  tsAttributes.Enabled:= Assigned(FOperation) and (fpAttributes in FOperation.SupportedProperties);
end;

destructor TfrmFileProperties.Destroy;
begin
  FFiles.Free;
  StopCalcFolderSize;
  FreeAndNil( FOperation );
  inherited Destroy;
  FPropertyFormatter := nil; // free interface
end;

function TfrmFileProperties.GetModeFromForm(out ExcludeAttrs: TFileAttrs): TFileAttrs;
begin
  Result:= 0;
  ExcludeAttrs:= 0;
  case cbReadOwner.State of
    cbChecked:  Result:= (Result or S_IRUSR);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IRUSR;
  end;
  case cbWriteOwner.State of
    cbChecked: Result:= (Result or S_IWUSR);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IWUSR;
  end;
  case cbExecOwner.State of
    cbChecked: Result:= (Result or S_IXUSR);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IXUSR;
  end;
  case cbReadGroup.State of
    cbChecked: Result:= (Result or S_IRGRP);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IRGRP;
  end;
  case cbWriteGroup.State of
    cbChecked: Result:= (Result or S_IWGRP);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IWGRP;
  end;
  case cbExecGroup.State of
    cbChecked: Result:= (Result or S_IXGRP);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IXGRP;
  end;
  case cbReadOther.State of
    cbChecked: Result:= (Result or S_IROTH);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IROTH;
  end;
  case cbWriteOther.State of
    cbChecked: Result:= (Result or S_IWOTH);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IWOTH;
  end;
  case cbExecOther.State of
    cbChecked: Result:= (Result or S_IXOTH);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_IXOTH;
  end;

  case cbSuid.State of
    cbChecked: Result:= (Result or S_ISUID);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_ISUID;
  end;
  case cbSgid.State of
    cbChecked: Result:= (Result or S_ISGID);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_ISGID;
  end;
  case cbSticky.State of
    cbChecked: Result:= (Result or S_ISVTX);
    cbUnchecked: ExcludeAttrs:= ExcludeAttrs or S_ISVTX;
  end;
end;

function TfrmFileProperties.FormatSize(ASize: Int64): String;
begin
  if gFileSizeFormat in [fsfByte, fsfPersonalizedByte] then
    Result:= cnvFormatFileSize(ASize)
  else begin
    Result:= Format('%s (%s)', [cnvFormatFileSize(ASize), IntToStrTS(ASize)]);
  end;
end;

procedure TfrmFileProperties.ShowMany;
var
  ASize: Int64;
  AFile: TFile;
  Index: Integer;
  ABitmap: TBitmap;
  UserID: Cardinal;
  Files, Directories: Integer;
begin
  ASize := 0;
  Files := 0;
  Directories := 0;

  ABitmap := PixMapManager.GetThemeIcon('edit-copy', gIconsSize);
  if Assigned(ABitmap) then
  begin
    imgFileIcon.Picture.Bitmap := ABitmap;
    ABitmap.Free;
  end;

  for Index:= 0 to FFiles.Count - 1 do
  begin
    AFile:= FFiles[Index];
    if AFile.IsDirectory then
      Inc(Directories)
    else begin
      Inc(Files);
      Inc(ASize, AFile.Size);
    end;
  end;

  chkRecursive.Visible:= (Directories > 0);
  DividerBevel4.Visible:= chkRecursive.Visible;

  if (Directories = 0) then
  begin
    lblSize.Caption := FormatSize(ASize);
  end
  else if (fsoCalcStatistics in FFileSource.GetOperationsTypes) then
  begin
    StartCalcFolderSize // Start calculate folder size operation
  end;

  // Chown
  if Assigned(FOperation.NewProperties[fpOwner]) then
  begin
    OriginalUser  := '*';
    OriginalGroup := '*';

    // Get current user ID
    UserID := fpGetUID;
    // Only owner or root can change owner
    bPerm := (UserID = FFileAttr.FindData.st_uid) or (UserID = 0);

    // Owner combo box
    cbxUsers.Text := OriginalUser;
    // Only root can change owner
    cbxUsers.Enabled := (UserID = 0);
    if cbxUsers.Enabled then
    begin
      GetUsers(cbxUsers.Items);
      cbxUsers.Sorted:= False;
      cbxUsers.Items.Insert(0, '*');
    end;

    // Group combo box
    cbxGroups.Text := OriginalGroup;
    cbxGroups.Enabled := bPerm;
    if bPerm then
    begin
      GetUsrGroups(UserID, cbxGroups.Items);
      cbxGroups.Sorted:= False;
      cbxGroups.Items.Insert(0, '*');
    end;
  end;

  lblFile.Caption := Format(rsPropsContains, [Files, Directories]);
  lblFileName.Caption := lblFile.Caption;

  lblContains.Visible:= (Directories > 0);
  lblContainsStr.Visible:= (Directories > 0);

  lblMediaType.Visible:= False;
  lblMediaTypeStr.Visible:= False;

  lblSizeOnDisk.Visible:= False;
  lblSizeOnDiskStr.Visible:= False;

  lblSymlink.Visible:= False;
  lblSymlinkStr.Visible:= False;

  lblLinks.Visible:= False;
  lblLinksStr.Visible:= False;

  lblLastAccess.Visible:= False;
  lblLastAccessStr.Visible:= False;
  lblLastModif.Visible:= False;
  lblLastModifStr.Visible:= False;
  lblLastStChange.Visible:= False;
  lblLastStChangeStr.Visible:= False;
  lblCreated.Visible := False;
  lblCreatedStr.Visible := False;

  lblFolder.Caption:= FFiles.Path;

  lblExecutable.Visible:= False;
  chkExecutable.Visible:= False;

  FFileType:= FFiles[0].Attributes and S_IFMT;

  for Index:= 1 to FFiles.Count - 1 do
  begin
    if (FFileType <> (FFiles[Index].Attributes and S_IFMT)) then
    begin
      lblType.Caption:= rsPropsMultipleTypes;
      Exit;
    end;
  end;
  ShowType(FFileType);
end;

procedure TfrmFileProperties.cbChangeModeClick(Sender: TObject);
var
  AMode, ExcludeAttrs: TFileAttrs;
  CheckBox: TCheckBox absolute Sender;
begin
  if fsCreating in FormState then exit;
  if FChangeTriggersEnabled then
  begin
    FChangeTriggersEnabled := False;
    if CheckBox.State = cbGrayed then
    begin
      edtOctal.Text:= EmptyStr;
      lblAttrText.Caption:= EmptyStr;
    end
    else begin
      AMode:= GetModeFromForm(ExcludeAttrs);
      edtOctal.Text:= DecToOct(AMode);
      lblAttrText.Caption:= FormatUnixAttributesEx(AMode);
    end;
    FChangeTriggersEnabled := True;
  end;
end;

procedure TfrmFileProperties.chkExecutableChange(Sender: TObject);
begin
  if chkExecutable.Tag = 0 then
  begin
    chkExecutable.Tag:= 1;
    case chkExecutable.State of
      cbChecked,
      cbUnchecked:
        begin
          cbExecOwner.Checked:= chkExecutable.Checked;
          cbExecGroup.Checked:= chkExecutable.Checked;
          cbExecOther.Checked:= chkExecutable.Checked;
        end;
      cbGrayed:
        begin
          cbExecOwner.Checked:= ((OriginalAttr and S_IXUSR) = S_IXUSR);
          cbExecGroup.Checked:= ((OriginalAttr and S_IXGRP) = S_IXGRP);
          cbExecOther.Checked:= ((OriginalAttr and S_IXOTH) = S_IXOTH);
        end;
    end;
   chkExecutable.Tag:= 0;
  end;
end;

procedure TfrmFileProperties.edtOctalKeyPress(Sender: TObject; var Key: char);
begin
  if not ((Key in ['0'..'7']) or (Key = Chr(VK_BACK)) or (Key = Chr(VK_DELETE))) then
    Key:= #0;
end;

procedure TfrmFileProperties.edtOctalKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AMode: TFileAttrs;
begin
  if FChangeTriggersEnabled then
  begin
    FChangeTriggersEnabled := False;
    AMode:= OctToDec(edtOctal.Text);
    lblAttrText.Caption := FormatUnixAttributesEx(AMode);
    ShowPermissions(AMode);
    FChangeTriggersEnabled := True;
  end;
end;

procedure TfrmFileProperties.ShowPermissions(Mode: TFileAttrs);
begin
  cbReadOwner.Checked:= ((Mode AND S_IRUSR) = S_IRUSR);
  cbWriteOwner.Checked:= ((Mode AND S_IWUSR) = S_IWUSR);
  cbExecOwner.Checked:= ((Mode AND S_IXUSR) = S_IXUSR);
  
  cbReadGroup.Checked:= ((Mode AND S_IRGRP) = S_IRGRP);
  cbWriteGroup.Checked:= ((Mode AND S_IWGRP) = S_IWGRP);
  cbExecGroup.Checked:= ((Mode AND S_IXGRP) = S_IXGRP);
  
  cbReadOther.Checked:= ((Mode AND S_IROTH) = S_IROTH);
  cbWriteOther.Checked:= ((Mode AND S_IWOTH) = S_IWOTH);
  cbExecOther.Checked:= ((Mode AND S_IXOTH) = S_IXOTH);

  cbSuid.Checked:= ((Mode AND S_ISUID) = S_ISUID);
  cbSgid.Checked:= ((Mode AND S_ISGID) = S_ISGID);
  cbSticky.Checked:= ((Mode AND S_ISVTX) = S_ISVTX);

  ShowExecutable;
end;

procedure TfrmFileProperties.ShowFile(iIndex:Integer);
var
  Idx: PtrInt;
  ASize: Int64;
  UserID: Cardinal;
  hasSize: Boolean;
  ABitmap: TBitmap;
  Attrs: TFileAttrs;
  AMimeType: String;
  isFileSystem: Boolean;
begin
  isFileSystem := FFileSource.IsClass(TFileSystemFileSource);

  Idx := PixMapManager.GetIconByFile(FFiles[iIndex], isFileSystem, True, sim_all_and_exe, True);
  if Idx < 0 then Idx:= PixMapManager.GetDefaultIcon(FFiles[iIndex]);
  ABitmap:= PixMapManager.GetBitmap(Idx);
  imgFileIcon.Picture.Bitmap := ABitmap;
  ABitmap.Free;

  with FFiles[iIndex] do
  begin
    lblFileName.Caption:= Name;
    lblFile.Caption:= Name;
    lblFolder.Caption:= Path;

    if not (fpCreationTime in SupportedProperties) then
    begin
      if fpCreationTime in FFileSource.RetrievableFileProperties then
      begin
        FFileSource.RetrieveProperties(FFiles[iIndex], [fpCreationTime], []);
      end;
    end;

    // Size
    hasSize := (fpSize in SupportedProperties) and (not IsLinkToDirectory);
    if hasSize then
    begin
      if IsDirectory and (fsoCalcStatistics in FFileSource.GetOperationsTypes) then
        StartCalcFolderSize // Start calculate folder size operation
      else
        lblSize.Caption := FormatSize(Size);
    end;
    lblSize.Visible := hasSize;
    lblSizeStr.Visible := hasSize;
    lblContains.Visible:= IsDirectory;
    lblContainsStr.Visible:= IsDirectory;

    // Size on disk
    hasSize:= (fpAttributes in SupportedProperties) and (FPS_ISREG(Attributes)) and
              (FFileAttr.FindData.st_ino <> 0);

    if hasSize then
    begin
      ASize:= FFileAttr.FindData.st_blocks * 512;
      lblSizeOnDisk.Caption:= FormatSize(ASize);
    end;
    lblSizeOnDisk.Visible:= hasSize;
    lblSizeOnDiskStr.Visible:= hasSize;

    // Links
    lblLinks.Visible:= isFileSystem and (FPS_ISREG(Attributes)) and (FFileAttr.FindData.st_nlink > 1);
    if lblLinks.Visible then lblLinks.Caption:= IntToStrTS(FFileAttr.FindData.st_nlink);
    lblLinksStr.Visible:= lblLinks.Visible;

    // Times
    lblLastAccess.Visible := fpLastAccessTime in SupportedProperties;
    lblLastAccessStr.Visible := fpLastAccessTime in SupportedProperties;
    if fpLastAccessTime in SupportedProperties then
      lblLastAccess.Caption := Properties[fpLastAccessTime].Format(FPropertyFormatter)
    else
      lblLastAccess.Caption := '';

    lblLastStChange.Visible := fpChangeTime in SupportedProperties;
    lblLastStChangeStr.Visible := fpChangeTime in SupportedProperties;
    if fpChangeTime in SupportedProperties then
      lblLastStChange.Caption := Properties[fpChangeTime].Format(FPropertyFormatter)
    else
      lblLastStChange.Caption := '';

    lblLastModif.Visible := fpModificationTime in SupportedProperties;
    lblLastModifStr.Visible := fpModificationTime in SupportedProperties;
    if fpModificationTime in SupportedProperties then
      lblLastModif.Caption := Properties[fpModificationTime].Format(FPropertyFormatter)
    else
      lblLastModif.Caption := '';

    lblCreated.Visible := fpCreationTime in SupportedProperties;
    lblCreatedStr.Visible := fpCreationTime in SupportedProperties;
    if fpCreationTime in SupportedProperties then
      lblCreated.Caption := Properties[fpCreationTime].Format(FPropertyFormatter)
    else
      lblCreated.Caption := '';

    // Chown
    if fpOwner in SupportedProperties then
    begin
      OriginalUser  := UIDToStr(OwnerProperty.Owner);
      OriginalGroup := GIDToStr(OwnerProperty.Group);

      // Get current user ID
      UserID := fpGetUID;
      // Only owner or root can change owner
      bPerm := (UserID = OwnerProperty.Owner) or (UserID = 0);

      // Owner combo box
      cbxUsers.Text := OriginalUser;
      // Only root can change owner
      cbxUsers.Enabled := (UserID = 0);
      if cbxUsers.Enabled then GetUsers(cbxUsers.Items);

      // Group combo box
      cbxGroups.Text := OriginalGroup;
      cbxGroups.Enabled := bPerm;
      if bPerm then GetUsrGroups(UserID, cbxGroups.Items);
    end;

    // MIME type
    hasSize:= isFileSystem;
    if hasSize then
    begin
      AMimeType:= GetFileMimeType(FullPath);
      hasSize:= Length(AMimeType) > 0;
      lblMediaType.Caption:= AMimeType;
    end;
    lblMediaType.Visible:= hasSize;
    lblMediaTypeStr.Visible:= hasSize;

    // Attributes
    if fpAttributes in SupportedProperties then
    begin
      Attrs := AttributesProperty.Value;
      FFileType:= Attrs and S_IFMT;
      OriginalAttr := Attrs and $0FFF;

      ShowPermissions(Attrs);

      lblExecutable.Visible:= FPS_ISREG(Attrs);
      chkExecutable.Visible:= lblExecutable.Visible;
      lblAttrText.Caption := Properties[fpAttributes].Format(DefaultFilePropertyFormatter);

      ShowType(Attrs);

      chkRecursive.Visible := IsDirectory;
      DividerBevel4.Visible:= chkRecursive.Visible;

      lblSymlink.Visible := FPS_ISLNK(Attrs);
      lblSymlinkStr.Visible := lblSymlink.Visible;
      if lblSymlink.Visible then
      begin
        if isFileSystem then
          lblSymlink.Caption := ReadSymLink(FullPath)
        else if (Assigned(LinkProperty) and LinkProperty.IsValid) then
          lblSymlink.Caption := LinkProperty.LinkTo
        else begin
          lblSymlink.Visible := False;
          lblSymlinkStr.Visible := False;
        end;
      end;
    end
    else
    begin
      chkRecursive.Visible:= False;
      DividerBevel4.Visible:= False;
      edtOctal.Text:= rsMsgErrNotSupported;
      lblAttrText.Caption:= rsMsgErrNotSupported;
      lblType.Caption:= rsPropsUnknownType;
      lblSymlink.Caption:= '';
    end;
  end;

  tsPlugins.Visible:= isFileSystem;
  if isFileSystem then ShowPlugin(iIndex);
end;

procedure TfrmFileProperties.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);

  if (FFiles.Count = 1) then
    ShowFile(0)
  else begin
    ShowMany;
  end;
end;

procedure TfrmFileProperties.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_TAB:
      begin
        if Shift * KeyModifiersShortcut = [ssCtrl] then
        begin
          pcPageControl.SelectNextPage(True);
          Key := 0;
        end
        else if Shift * KeyModifiersShortcut = [ssCtrl, ssShift] then
        begin
          pcPageControl.SelectNextPage(False);
          Key := 0;
        end;
      end;
  end;
end;

procedure TfrmFileProperties.OKButtonClick(Sender: TObject);
var
  theNewProperties: TFileProperties;
begin
  if Assigned(FOperation) then
  begin
    with FOperation do
    begin
      theNewProperties:= NewProperties;
      if fpAttributes in SupportedProperties then
      begin
        if theNewProperties[fpAttributes] is TUnixFileAttributesProperty then
          IncludeAttributes:= GetModeFromForm(ExcludeAttributes);
        // Nothing changed, clear new property
        if (IncludeAttributes = 0) and (ExcludeAttributes = 0) then
        begin
          theNewProperties[fpAttributes].Free;
          theNewProperties[fpAttributes]:= nil;
        end;
      end;
      if fpOwner in SupportedProperties then
      begin
        if (OriginalUser <> cbxUsers.Text) or (OriginalGroup <> cbxGroups.Text) then
        begin
          TFileOwnerProperty(theNewProperties[fpOwner]).Owner:= StrToUID(cbxUsers.Text);
          TFileOwnerProperty(theNewProperties[fpOwner]).Group:= StrToGID(cbxGroups.Text);
        end
        // Nothing changed, clear new property
        else begin
          theNewProperties[fpOwner].Free;
          theNewProperties[fpOwner]:= nil;
        end;
      end;
      NewProperties:= theNewProperties;
      Recursive:= chkRecursive.Checked;
    end;
    OperationsManager.AddOperation(FOperation);
    FOperation:= nil;
  end;
end;

procedure TfrmFileProperties.tmUpdateFolderSizeTimer(Sender: TObject);
begin
  if Assigned(FFileSourceCalcStatisticsOperation) then
    with FFileSourceCalcStatisticsOperation.RetrieveStatistics do
    begin
      lblSize.Caption := FormatSize(Size);
      lblContains.Caption := Format(rsPropsContains, [Files, Directories]);
    end;
end;

procedure TfrmFileProperties.FileSourceOperationStateChangedNotify(
  Operation: TFileSourceOperation; State: TFileSourceOperationState);
begin
  if Assigned(FFileSourceCalcStatisticsOperation) and (State = fsosStopped) then
  begin
    tmUpdateFolderSize.Enabled:= False;
    tmUpdateFolderSizeTimer(tmUpdateFolderSize);
    FFileSourceCalcStatisticsOperation := nil;
  end;
end;

procedure TfrmFileProperties.ShowType(Attrs: TFileAttrs);
begin
  if FPS_ISDIR(Attrs) then
    lblType.Caption:= rsPropsFolder
{$IFDEF UNIX}
  else if FPS_ISREG(Attrs) then
    lblType.Caption:= rsPropsFile
  else if FPS_ISCHR(Attrs) then
    lblType.Caption:= rsPropsSpChrDev
  else if FPS_ISBLK(Attrs) then
    lblType.Caption:= rsPropsSpBlkDev
  else if FPS_ISFIFO(Attrs) then
    lblType.Caption:= rsPropsNmdPipe
  else if FPS_ISLNK(Attrs) then
    lblType.Caption:= rsPropsSymLink
  else if FPS_ISSOCK(Attrs) then
    lblType.Caption:= rsPropsSocket
{$ENDIF}
  else
    lblType.Caption:= rsPropsUnknownType;
end;

procedure TfrmFileProperties.ShowExecutable;
begin
  if chkExecutable.Tag = 0 then
  begin
    if cbExecOwner.Checked and cbExecGroup.Checked and cbExecOther.Checked then
      chkExecutable.State:= cbChecked
    else if not (cbExecOwner.Checked or cbExecGroup.Checked or cbExecOther.Checked) then
      chkExecutable.State:= cbUnchecked
    else begin
      chkExecutable.AllowGrayed:= True;
      chkExecutable.State:= cbGrayed;
    end;
  end;
end;

procedure TfrmFileProperties.StartCalcFolderSize;
var
  aFiles: TFiles;
begin
  aFiles:= FFiles.Clone;
  try
    FFileSourceCalcStatisticsOperation:= FFileSource.CreateCalcStatisticsOperation(aFiles) as TFileSourceCalcStatisticsOperation;
    if Assigned(FFileSourceCalcStatisticsOperation) then
    begin
      FFileSourceCalcStatisticsOperation.SkipErrors:= True;
      FFileSourceCalcStatisticsOperation.SymLinkOption:= fsooslDontFollow;
      FFileSourceCalcStatisticsOperation.AddStateChangedListener([fsosStopped], @FileSourceOperationStateChangedNotify);
      OperationsManager.AddOperation(FFileSourceCalcStatisticsOperation, False);
      tmUpdateFolderSize.Enabled:= True;
    end;
  finally
    aFiles.Free;
  end;
end;

procedure TfrmFileProperties.StopCalcFolderSize;
begin
  if Assigned(FFileSourceCalcStatisticsOperation) then
  begin
    tmUpdateFolderSize.Enabled:= False;
    FFileSourceCalcStatisticsOperation.Stop;
  end;
  FFileSourceCalcStatisticsOperation:= nil;
end;

procedure TfrmFileProperties.ShowPlugin(iIndex: Integer);
var
  I, J: Integer;
  Value: String;
  Index: Integer;
  FileName: String;
  WdxModule: TWdxModule;
begin
  FileName:= FFiles[iIndex].FullPath;
  Value:= LowerCase(FFiles[iIndex].Extension);

  for Index:= 0 to gWdxPlugins.Count - 1 do
  begin
    WdxModule:= gWdxPlugins.GetWdxModule(Index);
    if (Length(WdxModule.DetectStr) > 0) and WdxModule.FileParamVSDetectStr(FFiles[iIndex]) then
    begin
      if not gWdxPlugins.IsLoaded(Index) then
      begin
        if not gWdxPlugins.LoadModule(Index) then
          Continue;
      end;
      J:= 0;
      sgPlugins.RowCount:= WdxModule.FieldList.Count + 1;
      for I:= 0 to WdxModule.FieldList.Count - 1 do
      begin
        if not (TWdxField(WdxModule.FieldList.Objects[I]).FType in [ft_fulltext, ft_fulltextw]) then
        begin
          Value:= WdxModule.CallContentGetValue(FileName, I, 0, CONTENT_DELAYIFSLOW);
          if (Length(Value) > 0) then
          begin
            Inc(J);
            sgPlugins.Cells[1, J]:= Value;
            sgPlugins.Cells[0, J]:= TWdxField(WdxModule.FieldList.Objects[I]).LName;
          end;
        end;
      end;
      sgPlugins.RowCount:= J + 1;
      tsPlugins.TabVisible:= J > 0;
      if tsPlugins.TabVisible then Break;
    end;
  end;
end;

procedure TfrmFileProperties.UpdateAllowGrayed(AllowGrayed: Boolean);
var
  Index: Integer;
begin
  for Index:= 0 to tsAttributes.ControlCount - 1 do
  begin
    if tsAttributes.Controls[Index] is TCheckBox then
      TCheckBox(tsAttributes.Controls[Index]).AllowGrayed:= AllowGrayed;
  end;
end;

function TfrmFileProperties.FormatUnixAttributesEx(iAttr: TFileAttrs): String;
begin
  if (FFiles.Count = 1) then
    Result:= FormatUnixAttributes(FFileType or iAttr)
  else begin
    Result:= Copy(FormatUnixAttributes(iAttr), 2, MaxInt);
  end;
end;

end.


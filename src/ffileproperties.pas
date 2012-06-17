{
   Double Commander
   -------------------------------------------------------------------
   File Properties Dialog

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2003 Martin Matusu <xmat@volny.cz>
   Copyright (C) 2006-2009 Alexander Koblov (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit fFileProperties;

{$mode objfpc}{$H+}

interface

uses
  LResources, SysUtils, Classes, Graphics, Forms, StdCtrls, Buttons, ComCtrls,
  Dialogs, Controls, ExtCtrls, DCBasicTypes, uFile, uFileProperty, uFileSource,
  uFileSourceOperation, uFileSourceCalcStatisticsOperation;

type

  { TfrmFileProperties }

  TfrmFileProperties = class(TForm)
    btnSetPropertiesToAllFiles: TBitBtn;
    btnClose: TBitBtn;
    btnSetProperties: TBitBtn;
    btnSkipFile: TBitBtn;
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
    edtOctal: TEdit;
    gbOwner: TGroupBox;
    lblFileName: TLabel;
    lblFileNameStr: TLabel;
    lblFolder: TLabel;
    lblFolderStr: TLabel;
    lblLastAccess: TLabel;
    lblLastAccessStr: TLabel;
    lblLastModif: TLabel;
    lblLastModifStr: TLabel;
    lblLastStChange: TLabel;
    lblLastStChangeStr: TLabel;
    lblOctal: TLabel;
    lblAttrBitsStr: TLabel;
    lblAttrText: TLabel;
    lblExec: TLabel;
    lblFileStr: TLabel;
    lblFile: TLabel;
    lblAttrGroupStr: TLabel;
    lblGroupStr: TLabel;
    lblAttrOtherStr: TLabel;
    lblAttrOwnerStr: TLabel;
    lblOwnerStr: TLabel;

    lblRead: TLabel;
    lblSize: TLabel;
    lblContains: TLabel;
    lblSizeStr: TLabel;
    lblContainsStr: TLabel;
    lblSymlink: TLabel;
    lblAttrTextStr: TLabel;
    lblSymlinkStr: TLabel;
    lblType: TLabel;
    lblTypeStr: TLabel;
    lblWrite: TLabel;
    pnlCaption: TPanel;
    pnlData: TPanel;
    pcPageControl: TPageControl;
    tmUpdateFolderSize: TTimer;
    tsProperties: TTabSheet;
    tsAttributes: TTabSheet;
    procedure btnSetPropertiesToAllFilesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbChangeModeClick(Sender: TObject);
    procedure edtOctalKeyPress(Sender: TObject; var Key: char);
    procedure edtOctalKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure btnSetPropertiesClick(Sender: TObject);
    procedure btnSkipFileClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tmUpdateFolderSizeTimer(Sender: TObject);
    procedure FileSourceOperationStateChangedNotify(Operation: TFileSourceOperation;
                                                               State: TFileSourceOperationState);
  private
    bPerm: Boolean;
    iCurrent: Integer;
    FFileSource: IFileSource;
    FFiles: TFiles;
    FPropertyFormatter: IFilePropertyFormatter;
    FFileSourceCalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
    ChangeTriggersEnabled: Boolean;
    OriginalAttr: TFileAttrs;
    OriginalUser, OriginalGroup: String;
    FChangedProperties: Boolean;

    procedure ShowPermissions(Mode: TFileAttrs);
    function ChangeProperties: Boolean;
    function CheckIfChangedProperties: Boolean;
    function GetModeFromForm: TFileAttrs;
    procedure ShowFile(iIndex:Integer);
    procedure AllowChange(Allow: Boolean);
    procedure StartCalcFolderSize;
    procedure StopCalcFolderSize;
  public
    constructor Create(AOwner: TComponent; aFileSource: IFileSource; theFiles: TFiles); reintroduce;
    destructor Destroy; override;
  end;


procedure ShowFileProperties(aFileSource: IFileSource; const aFiles: TFiles);

implementation

{$R *.lfm}

uses
  LCLType, FileUtil, StrUtils, uLng, BaseUnix, uUsersGroups, uDCUtils, uOSUtils,
  uDefaultFilePropertyFormatter, uMyUnix, DCFileAttributes,
  uFileSourceOperationTypes, uFileSystemFileSource, uOperationsManager,
  uFileSourceOperationOptions, uKeyboard, DCStrUtils;

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
begin
  FFileSource:= aFileSource;
  FFiles := theFiles;
  FPropertyFormatter := MaxDetailsFilePropertyFormatter;
  ChangeTriggersEnabled := True;

  inherited Create(AOwner);
end;

destructor TfrmFileProperties.Destroy;
begin
  StopCalcFolderSize;
  inherited Destroy;
  FPropertyFormatter := nil; // free interface
end;

function TfrmFileProperties.GetModeFromForm: TFileAttrs;
begin
  Result:=0;
  if cbReadOwner.Checked then Result:=(Result OR S_IRUSR);
  if cbWriteOwner.Checked then Result:=(Result OR S_IWUSR);
  if cbExecOwner.Checked then Result:=(Result OR S_IXUSR);
  if cbReadGroup.Checked then Result:=(Result OR S_IRGRP);
  if cbWriteGroup.Checked then Result:=(Result OR S_IWGRP);
  if cbExecGroup.Checked then Result:=(Result OR S_IXGRP);
  if cbReadOther.Checked then Result:=(Result OR S_IROTH);
  if cbWriteOther.Checked then Result:=(Result OR S_IWOTH);
  if cbExecOther.Checked then Result:=(Result OR S_IXOTH);

  if cbSuid.Checked then Result:=(Result OR S_ISUID);
  if cbSgid.Checked then Result:=(Result OR S_ISGID);
  if cbSticky.Checked then Result:=(Result OR S_ISVTX);
end;

procedure TfrmFileProperties.btnCloseClick(Sender: TObject);
begin
  if FChangedProperties then
    FFileSource.Reload(FFiles.Path);
  Close;
end;

procedure TfrmFileProperties.cbChangeModeClick(Sender: TObject);
begin
  if ChangeTriggersEnabled then
  begin
    ChangeTriggersEnabled := False;
    edtOctal.Text:= DecToOct(GetModeFromForm);
    ChangeTriggersEnabled := True;
  end;
end;

procedure TfrmFileProperties.edtOctalKeyPress(Sender: TObject; var Key: char);
begin
  if not ((Key in ['0'..'7']) or (Key = Chr(VK_BACK)) or (Key = Chr(VK_DELETE))) then
    Key:= #0;
end;

procedure TfrmFileProperties.edtOctalKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ChangeTriggersEnabled then
  begin
    ChangeTriggersEnabled := False;
    ShowPermissions(OctToDec(edtOctal.Text));
    ChangeTriggersEnabled := True;
  end;
end;

procedure TfrmFileProperties.btnSetPropertiesToAllFilesClick(Sender: TObject);
begin
  repeat
    if not ChangeProperties then Exit;
    Inc (iCurrent);
  until (iCurrent = FFiles.Count);
  FFileSource.Reload(FFiles.Path);
  Close;
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
end;

function TfrmFileProperties.ChangeProperties: Boolean;
begin
  Result:= True;
  // First set owner/group because it clears SUID bit.
  if bPerm then
  begin
    if fplchown(PChar(UTF8ToSys(FFiles[iCurrent].FullPath)), StrToUID(cbxUsers.Text),
                StrToGID(cbxGroups.Text)) <> 0 then
      begin
        if MessageDlg(Caption, Format(rsPropsErrChOwn, [FFiles[iCurrent].Name]), mtError, mbOKCancel, 0) = mrCancel then
          Exit(False);
      end;
  end;
  if not FFiles[iCurrent].IsLink then
  begin
    if fpchmod(PAnsiChar(UTF8ToSys(FFiles[iCurrent].FullPath)), GetModeFromForm) <> 0 then
      begin
        if MessageDlg(Caption, Format(rsPropsErrChMod, [FFiles[iCurrent].Name]), mtError, mbOKCancel, 0) = mrCancel then
          Exit(False);
      end;
  end;
end;

function TfrmFileProperties.CheckIfChangedProperties: Boolean;
begin
  Result := (OriginalAttr  <> GetModeFromForm) or
            (OriginalUser  <> cbxUsers.Text) or
            (OriginalGroup <> cbxGroups.Text);
end;

procedure TfrmFileProperties.ShowFile(iIndex:Integer);
var
  sb: BaseUnix.Stat;
  iMyUID: Cardinal;
  Attrs: TFileAttrs;
  isFileSystem: Boolean;
  hasSize: Boolean;
begin
  StopCalcFolderSize; // Stop previous calculate folder size operation
  isFileSystem := FFileSource.IsClass(TFileSystemFileSource);

  with FFiles[iIndex] do
  begin
    lblFileName.Caption:= Name;
    lblFile.Caption:= Name;
    lblFolder.Caption:= Path;

    // Size
    hasSize := (fpSize in SupportedProperties);
    if hasSize then
      begin
        if IsDirectory and (fsoCalcStatistics in FFileSource.GetOperationsTypes) then
          StartCalcFolderSize // Start calculate folder size operation
        else
          lblSize.Caption := Properties[fpSize].Format(FPropertyFormatter);
      end;
    lblSize.Visible := hasSize;
    lblSizeStr.Visible := hasSize;
    lblContains.Visible:= IsDirectory;
    lblContainsStr.Visible:= IsDirectory;

    // Times
    lblLastAccess.Visible := fpLastAccessTime in SupportedProperties;
    lblLastAccessStr.Visible := fpLastAccessTime in SupportedProperties;
    if fpLastAccessTime in SupportedProperties then
      lblLastAccess.Caption := Properties[fpLastAccessTime].Format(FPropertyFormatter)
    else
      lblLastAccess.Caption:='';

    lblLastStChange.Visible := fpCreationTime in SupportedProperties;
    lblLastStChangeStr.Visible := fpCreationTime in SupportedProperties;
    if fpCreationTime in SupportedProperties then
      lblLastStChange.Caption := Properties[fpCreationTime].Format(FPropertyFormatter)
    else
      lblLastStChange.Caption:='';

    lblLastModif.Visible := fpModificationTime in SupportedProperties;
    lblLastModifStr.Visible := fpModificationTime in SupportedProperties;
    if fpModificationTime in SupportedProperties then
      lblLastModif.Caption := Properties[fpModificationTime].Format(FPropertyFormatter)
    else
      lblLastModif.Caption:='';

    // Chown
    if isFileSystem and (fpLStat(PChar(UTF8ToSys(FullPath)), sb) = 0) then
    begin
      OriginalUser  := UIDToStr(sb.st_uid);
      OriginalGroup := GIDToStr(sb.st_gid);
      iMyUID:= fpGetUID; //get user's UID
      bPerm:= (iMyUID = sb.st_uid);
      cbxUsers.Text:= OriginalUser;
      if(imyUID = 0) then
        GetUsers(cbxUsers.Items); //huh, a ROOT :))
      cbxUsers.Enabled:= (imyUID = 0);
      cbxGroups.Text:= OriginalGroup;
      if(bPerm or (iMyUID = 0)) then
        GetUsrGroups(iMyUID, cbxGroups.Items);
      cbxGroups.Enabled:= (bPerm or (iMyUID = 0));
    end;

    // Attributes
    if fpAttributes in SupportedProperties then
    begin
      Attrs := AttributesProperty.Value;
      OriginalAttr := Attrs and $0FFF;
      //if Attrs is TUnixFileAttributesProperty
      //if Attrs is TNtfsFileAttributesProperty

      ShowPermissions(Attrs);
      edtOctal.Text:= DecToOct(GetModeFromForm);
      lblAttrText.Caption := Properties[fpAttributes].Format(DefaultFilePropertyFormatter);

      if FPS_ISDIR(Attrs) then
        lblType.Caption:=rsPropsFolder
      else if FPS_ISREG(Attrs) then
        lblType.Caption:=rsPropsFile
      else if FPS_ISCHR(Attrs) then
        lblType.Caption:=rsPropsSpChrDev
      else if FPS_ISBLK(Attrs) then
        lblType.Caption:=rsPropsSpBlkDev
      else if FPS_ISFIFO(Attrs) then
        lblType.Caption:=rsPropsNmdPipe
      else if FPS_ISLNK(Attrs) then
        lblType.Caption:=rsPropsSymLink
      else if FPS_ISSOCK(Attrs) then
        lblType.Caption:=rsPropsSocket
      else
        lblType.Caption:=rsPropsUnknownType;

      lblSymlink.Visible := FPS_ISLNK(Attrs);
      lblSymlinkStr.Visible := FPS_ISLNK(Attrs);
      if FPS_ISLNK(Attrs) and isFileSystem then
      begin
        //lblSymlink.Caption := sLinkTo; // maybe make property for this
        lblSymlink.Caption := ReadSymLink(FullPath);
      end
      else
      begin
        lblSymlink.Visible := False;
        lblSymlinkStr.Visible := False;
      end;
    end
    else
    begin
      edtOctal.Text:=rsMsgErrNotSupported;
      lblAttrText.Caption:=rsMsgErrNotSupported;
      lblType.Caption:=rsPropsUnknownType;
      lblSymlink.Caption:='';
    end;
  end;

  // Only allow changes for file system file.
  AllowChange(isFileSystem);
end;

procedure TfrmFileProperties.FormCreate(Sender: TObject);
begin
  lblFileNameStr.Font.Style:=[fsBold];
  lblFileName.Font.Style:=[fsBold];

  AllowChange(False);
  ShowFile(0);
end;

procedure TfrmFileProperties.btnSetPropertiesClick(Sender: TObject);
begin
  if CheckIfChangedProperties then
    FChangedProperties := True;
  if not ChangeProperties then Exit;
  btnSkipFileClick(Self);
end;

procedure TfrmFileProperties.btnSkipFileClick(Sender: TObject);
begin
  inc(iCurrent);
  if iCurrent >= FFiles.Count then
  begin
    if FChangedProperties then
      FFileSource.Reload(FFiles.Path);
    Close;
  end
  else
    ShowFile(iCurrent);
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

procedure TfrmFileProperties.tmUpdateFolderSizeTimer(Sender: TObject);
begin
  if Assigned(FFileSourceCalcStatisticsOperation) then
    with FFileSourceCalcStatisticsOperation.RetrieveStatistics do
    begin
      lblSize.Caption := Format('%s (%s)', [cnvFormatFileSize(Size), Numb2USA(IntToStr(Size))]);
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

procedure TfrmFileProperties.AllowChange(Allow: Boolean);
begin
  btnSetPropertiesToAllFiles.Enabled := Allow;
  btnSetProperties.Enabled := Allow;
end;

procedure TfrmFileProperties.StartCalcFolderSize;
var
  aFiles: TFiles;
begin
  aFiles:= TFiles.Create(FFiles.Path);
  aFiles.Add(FFiles.Items[iCurrent].Clone);
  FFileSourceCalcStatisticsOperation:= FFileSource.CreateCalcStatisticsOperation(aFiles) as TFileSourceCalcStatisticsOperation;
  if Assigned(FFileSourceCalcStatisticsOperation) then
    begin
      FFileSourceCalcStatisticsOperation.SkipErrors:= True;
      FFileSourceCalcStatisticsOperation.SymLinkOption:= fsooslDontFollow;
      FFileSourceCalcStatisticsOperation.AddStateChangedListener([fsosStopped], @FileSourceOperationStateChangedNotify);
      OperationsManager.AddOperation(FFileSourceCalcStatisticsOperation);
      tmUpdateFolderSize.Enabled:= True;
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

end.


{
   Double Commander
   -------------------------------------------------------------------
   File Properties Dialog

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2003 Martin Matusu <xmat@volny.cz>
   Copyright (C) 2006-2015 Alexander Koblov (alexx2000@mail.ru)

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
  Dialogs, Controls, ExtCtrls, Grids, DividerBevel, KASCDEdit, DCBasicTypes,
  uFile, uFileProperty, uFileSource, uFileSourceOperation,
  uFileSourceCalcStatisticsOperation, uExifReader;

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
    chkExecutable: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    edtOctal: TEdit;
    gbOwner: TGroupBox;
    lblExecutable: TLabel;
    lblFileName: TLabel;
    imgFileIcon: TImage;
    lblFolder: TKASCDEdit;
    lblFolderStr: TLabel;
    lblLastAccess: TKASCDEdit;
    lblLastAccessStr: TLabel;
    lblLastModif: TKASCDEdit;
    lblLastModifStr: TLabel;
    lblLastStChange: TKASCDEdit;
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
    lblSize: TKASCDEdit;
    lblContains: TKASCDEdit;
    lblSizeStr: TLabel;
    lblContainsStr: TLabel;
    lblSymlink: TKASCDEdit;
    lblAttrTextStr: TLabel;
    lblSymlinkStr: TLabel;
    lblType: TKASCDEdit;
    lblTypeStr: TLabel;
    lblWrite: TLabel;
    pnlCaption: TPanel;
    pnlData: TPanel;
    pnlIcon: TPanel;
    pcPageControl: TPageControl;
    sgImage: TStringGrid;
    tsPlugins: TTabSheet;
    tmUpdateFolderSize: TTimer;
    tsProperties: TTabSheet;
    tsAttributes: TTabSheet;
    procedure btnSetPropertiesToAllFilesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbChangeModeClick(Sender: TObject);
    procedure chkExecutableChange(Sender: TObject);
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
    FExif: TExifReader;
    FPropertyFormatter: IFilePropertyFormatter;
    FFileSourceCalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
    ChangeTriggersEnabled: Boolean;
    FFileType,
    OriginalAttr: TFileAttrs;
    OriginalUser, OriginalGroup: String;
    FChangedProperties: Boolean;

    function ShowError(const MessageFmt: String): TModalResult;
    procedure ShowExecutable;
    procedure ShowPermissions(Mode: TFileAttrs);
    function ChangeProperties: Boolean;
    function CheckIfChangedProperties: Boolean;
    function GetModeFromForm: TFileAttrs;
    procedure ShowFile(iIndex:Integer);
    procedure AllowChange(Allow: Boolean);
    procedure StartCalcFolderSize;
    procedure StopCalcFolderSize;
    procedure ShowPlugin(iIndex:Integer);
  public
    constructor Create(AOwner: TComponent; aFileSource: IFileSource; theFiles: TFiles); reintroduce;
    destructor Destroy; override;
  end;


procedure ShowFileProperties(aFileSource: IFileSource; const aFiles: TFiles);

implementation

{$R *.lfm}

uses
  LCLType, LazUTF8, StrUtils, uLng, BaseUnix, uUsersGroups, uDCUtils, DCOSUtils,
  uDefaultFilePropertyFormatter, uMyUnix, DCFileAttributes, uGlobs, uWdxModule,
  uFileSourceOperationTypes, uFileSystemFileSource, uOperationsManager, WdxPlugin,
  uFileSourceOperationOptions, uKeyboard, DCStrUtils, DCUnix, uPixMapManager;

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
  FExif:= TExifReader.Create;
  FFileSource:= aFileSource;
  FFiles := theFiles;
  FPropertyFormatter := MaxDetailsFilePropertyFormatter;
  ChangeTriggersEnabled := True;

  inherited Create(AOwner);

  imgFileIcon.Width:= gIconsSize;
  imgFileIcon.Height:= gIconsSize;
end;

destructor TfrmFileProperties.Destroy;
begin
  FExif.Free;
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
var
  AMode: TFileAttrs;
begin
  if ChangeTriggersEnabled then
  begin
    ChangeTriggersEnabled := False;
    ShowExecutable;
    AMode:= GetModeFromForm;
    edtOctal.Text:= DecToOct(AMode);
    lblAttrText.Caption := FormatUnixAttributes(FFileType or AMode);
    ChangeTriggersEnabled := True;
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
  if ChangeTriggersEnabled then
  begin
    ChangeTriggersEnabled := False;
    AMode:= OctToDec(edtOctal.Text);
    lblAttrText.Caption := FormatUnixAttributes(FFileType or AMode);
    ShowPermissions(AMode);
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

  ShowExecutable;
end;

function TfrmFileProperties.ChangeProperties: Boolean;
begin
  Result:= True;
  // First set owner/group because it clears SUID bit.
  if bPerm then
  begin
    if fplchown(FFiles[iCurrent].FullPath, StrToUID(cbxUsers.Text),
                StrToGID(cbxGroups.Text)) <> 0 then
      begin
        if ShowError(rsPropsErrChOwn) = mrCancel then Exit(False);
      end;
  end;
  if not FFiles[iCurrent].IsLink then
  begin
    if fpchmod(PAnsiChar(UTF8ToSys(FFiles[iCurrent].FullPath)), GetModeFromForm) <> 0 then
      begin
        if ShowError(rsPropsErrChMod) = mrCancel then Exit(False);
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
  Polik: PtrInt;
begin
  StopCalcFolderSize; // Stop previous calculate folder size operation
  isFileSystem := FFileSource.IsClass(TFileSystemFileSource);

  Polik := PixMapManager.GetIconByFile(FFiles[iIndex], isFileSystem, True, sim_all_and_exe, True);
  if Polik < 0 then Polik:= PixMapManager.GetDefaultIcon(FFiles[iIndex]);
  imgFileIcon.Picture.Bitmap := PixMapManager.GetBitmap(Polik);

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

    // Chown
    if isFileSystem and (fpLStat(PChar(UTF8ToSys(FullPath)), sb) = 0) then
    begin
      OriginalUser  := UIDToStr(sb.st_uid);
      OriginalGroup := GIDToStr(sb.st_gid);
      // Get current user UID
      iMyUID := fpGetUID;
      // Only owner or root can change owner
      bPerm := (iMyUID = sb.st_uid) or (iMyUID = 0);

      // Owner combo box
      cbxUsers.Text := OriginalUser;
      // Only root can change owner
      cbxUsers.Enabled := (imyUID = 0);
      if cbxUsers.Enabled then GetUsers(cbxUsers.Items);

      // Group combo box
      cbxGroups.Text := OriginalGroup;
      cbxGroups.Enabled := bPerm;
      if bPerm then GetUsrGroups(iMyUID, cbxGroups.Items);
    end;

    // Attributes
    if fpAttributes in SupportedProperties then
    begin
      Attrs := AttributesProperty.Value;
      FFileType:= Attrs and S_IFMT;
      OriginalAttr := Attrs and $0FFF;
      //if Attrs is TUnixFileAttributesProperty
      //if Attrs is TNtfsFileAttributesProperty

      ShowPermissions(Attrs);
      lblExecutable.Visible:= FPS_ISREG(Attrs);
      chkExecutable.Visible:= lblExecutable.Visible;
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
      edtOctal.Text:=rsMsgErrNotSupported;
      lblAttrText.Caption:=rsMsgErrNotSupported;
      lblType.Caption:=rsPropsUnknownType;
      lblSymlink.Caption:='';
    end;
  end;

  tsPlugins.Visible:= isFileSystem;
  if isFileSystem then ShowPlugin(iIndex);

  // Only allow changes for file system file.
  AllowChange(isFileSystem);
end;

procedure TfrmFileProperties.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);
  lblFileName.Font.Style:= [fsBold];

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

function TfrmFileProperties.ShowError(const MessageFmt: String): TModalResult;
begin
  Result:= MessageDlg(Caption, Format(MessageFmt, [FFiles[iCurrent].FullPath]) +
                      LineEnding + SysErrorMessageUTF8(fpgetErrNo), mtError, mbOKCancel, 0);
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

procedure TfrmFileProperties.ShowPlugin(iIndex: Integer);
var
  I, J: Integer;
  Value: String;
  FileName: String;
  Index: Integer = 0;
  WdxModule: TWdxModule;
begin
  FileName:= FFiles[iIndex].FullPath;
  Value:= LowerCase(FFiles[iIndex].Extension);
  tsPlugins.TabVisible:= Contains(['jpg', 'jpeg'], Value) and FExif.LoadFromFile(FileName);
  if tsPlugins.TabVisible then
  begin
    sgImage.RowCount:= 6;
    if FExif.ImageWidth <> 0 then
    begin
     Inc(Index);
     sgImage.Cells[0, Index]:= rsImageWidth;
     sgImage.Cells[1, Index]:= IntToStr(FExif.ImageWidth);
    end;
    if FExif.ImageHeight <> 0 then
    begin
     Inc(Index);
     sgImage.Cells[0, Index]:= rsImageHeight;
     sgImage.Cells[1, Index]:= IntToStr(FExif.ImageHeight);
    end;
    if Length(FExif.DateTimeOriginal) > 0 then
    begin
      Inc(Index);
      sgImage.Cells[0, Index]:= rsDateTimeOriginal;
      sgImage.Cells[1, Index]:= FExif.DateTimeOriginal;
    end;
    if Length(FExif.Make) > 0 then
    begin
      Inc(Index);
      sgImage.Cells[0, Index]:= rsMake;
      sgImage.Cells[1, Index]:= FExif.Make;
    end;
    if Length(FExif.Model) > 0 then
    begin
      Inc(Index);
      sgImage.Cells[0, Index]:= rsModel;
      sgImage.Cells[1, Index]:= FExif.Model;
    end;
    tsPlugins.TabVisible:= Index > 0;
    sgImage.RowCount:= Index + 1;
  end
  else begin
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
        sgImage.RowCount:= WdxModule.FieldList.Count + 1;
        for I:= 0 to WdxModule.FieldList.Count - 1 do
        begin
          if not (TWdxField(WdxModule.FieldList.Objects[I]).FType in [ft_fulltext, ft_fulltextw]) then
          begin
            Value:= WdxModule.CallContentGetValue(FileName, I, 0, CONTENT_DELAYIFSLOW);
            if (Length(Value) > 0) then
            begin
              Inc(J);
              sgImage.Cells[1, J]:= Value;
              sgImage.Cells[0, J]:= WdxModule.FieldList[I];
            end;
          end;
        end;
        sgImage.RowCount:= J + 1;
        tsPlugins.TabVisible:= J > 0;
        if tsPlugins.TabVisible then Break;
      end;
    end;
  end;
end;

end.


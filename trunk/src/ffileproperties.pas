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
  ExtCtrls, uTypes, uFile, uFileProperty, uFileSource, uFileSourceOperation,
  uFileSourceCalcStatisticsOperation;

type

  { TfrmFileProperties }

  TfrmFileProperties = class(TForm)
    btnAll: TBitBtn;
    btnClose: TButton;
    btnOK: TBitBtn;
    btnSkip: TBitBtn;
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
    lblSizeStr: TLabel;
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
    procedure btnAllClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cbChangeModeClick(Sender: TObject);
    procedure edtOctalKeyPress(Sender: TObject; var Key: char);
    procedure edtOctalKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure tmUpdateFolderSizeTimer(Sender: TObject);
    procedure FileSourceOperationStateChangedNotify(Operation: TFileSourceOperation;
                                                               State: TFileSourceOperationState);
  private
    bPerm: Boolean;
    iCurrent: Integer;
    FFileSource: IFileSource;
    fFiles: TFiles;
    FPropertyFormatter: IFilePropertyFormatter;
    FFileSourceCalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
    ChangeTriggersEnabled: Boolean;

    procedure ShowAttr(Mode: TFileAttrs);
    procedure ChangeMod;
    procedure ChangeOwner;
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

uses
  LCLType, StrUtils, uLng, BaseUnix, uUsersGroups, uDCUtils, uOSUtils,
  uDefaultFilePropertyFormatter, uFileSystemFile, uMyUnix, uDateTimeUtils,
  uFileAttributes, uFileSourceOperationTypes, uOperationsManager;

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
  iCurrent := 0;
  FFileSource:= aFileSource;
  fFiles := theFiles;
  FPropertyFormatter := MaxDetailsFilePropertyFormatter;
  FFileSourceCalcStatisticsOperation:= nil;
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

procedure TfrmFileProperties.ChangeMod;
begin
  if not fFiles[iCurrent].IsLink then
    fpchmod(PChar(fFiles[iCurrent].FullPath), GetModeFromForm);
  //show error
end;

procedure TfrmFileProperties.ChangeOwner;
begin
  fplchown(PChar(fFiles[iCurrent].FullPath),
           StrToUID(cbxUsers.Text),
           StrToGID(cbxGroups.Text));
  //show error
end;

procedure TfrmFileProperties.btnCloseClick(Sender: TObject);
begin
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
    ShowAttr(OctToDec(edtOctal.Text));
    ChangeTriggersEnabled := True;
  end;
end;

procedure TfrmFileProperties.btnAllClick(Sender: TObject);
begin
  repeat
    ChangeMod;
    if(bPerm) then
      ChangeOwner;
    inc (iCurrent);
  until (iCurrent = fFiles.Count);
  Close;
end;

procedure TfrmFileProperties.ShowAttr(Mode: TFileAttrs);
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

procedure TfrmFileProperties.ShowFile(iIndex:Integer);
var
  sb: BaseUnix.Stat;
  dtFileDates: TDateTime;
  iMyUID: Cardinal;
  Attrs: TFileAttrs;
  isFileSystem: Boolean;
  hasSize: Boolean;
begin
  StopCalcFolderSize; // Stop previous calculate folder size operation
  isFileSystem := fFiles[iIndex] is TFileSystemFile;

  with fFiles[iIndex] do
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

    // Times
    if isFileSystem then
    begin
      fpLStat(PChar(FullPath), sb);
      dtFileDates := FileTimeToDateTime(sb.st_atime);
      lblLastAccess.Caption := DateTimeToStr(dtFileDates);
      dtFileDates := FileTimeToDateTime(sb.st_ctime);
      lblLastStChange.Caption := DateTimeToStr(dtFileDates);
    end
    else
    begin
      lblLastAccess.Caption:='';
      lblLastStChange.Caption:='';
    end;

    // Modification time
    lblLastModif.Visible := fpModificationTime in SupportedProperties;
    lblLastModifStr.Visible := fpModificationTime in SupportedProperties;
    if fpModificationTime in SupportedProperties then
      lblLastModif.Caption := DateTimeToStr((Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value);
                             //Properties[fpModificationTime].Format(FPropertyFormatter);

    // Chown
    if isFileSystem then
    begin
      iMyUID:=fpGetUID; //get user's UID
      bPerm:=(iMyUID=sb.st_uid);
      cbxUsers.Text:=UIDToStr(sb.st_uid);
      if(imyUID=0) then
        GetUsers(cbxUsers.Items); //huh, a ROOT :))
      cbxUsers.Enabled:=(imyUID=0);
      cbxGroups.Text:=GIDToStr(sb.st_gid);
      if(bPerm or (iMyUID=0)) then
        GetUsrGroups(iMyUID, cbxGroups.Items);
      cbxGroups.Enabled:=(bPerm or (iMyUID=0));
    end;

    // Attributes
    if fpAttributes in SupportedProperties then
    begin
      Attrs := (Properties[fpAttributes] as TFileAttributesProperty).Value;
      //if Attrs is TUnixFileAttributesProperty
      //if Attrs is TNtfsFileAttributesProperty

      ShowAttr(Attrs);
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

procedure TfrmFileProperties.btnOKClick(Sender: TObject);
begin
  ChangeMod;
  if (bPerm) then
    ChangeOwner;
  btnSkipClick(Self);
end;

procedure TfrmFileProperties.btnSkipClick(Sender: TObject);
begin
  inc(iCurrent);
  if iCurrent >= fFiles.Count Then
    Close
  else
    ShowFile(iCurrent);
end;

procedure TfrmFileProperties.tmUpdateFolderSizeTimer(Sender: TObject);
begin
  if Assigned(FFileSourceCalcStatisticsOperation) then
    with FFileSourceCalcStatisticsOperation.RetrieveStatistics do
    lblSize.Caption := Format('%s (%s)', [cnvFormatFileSize(Size), Numb2USA(IntToStr(Size))]);
end;

procedure TfrmFileProperties.FileSourceOperationStateChangedNotify(
  Operation: TFileSourceOperation; State: TFileSourceOperationState);
begin
  if State = fsosStopped then
    begin
      tmUpdateFolderSize.Enabled:= False;
      tmUpdateFolderSizeTimer(tmUpdateFolderSize);
      FFileSourceCalcStatisticsOperation := nil;
    end;
end;

procedure TfrmFileProperties.AllowChange(Allow: Boolean);
begin
  btnAll.Enabled := Allow;
  btnOK.Enabled := Allow;
end;

procedure TfrmFileProperties.StartCalcFolderSize;
var
  aFiles: TFiles;
begin
  aFiles:= FFileSource.CreateFiles(FFiles.Path);
  aFiles.Add(FFiles.Items[iCurrent].Clone);
  FFileSourceCalcStatisticsOperation:= FFileSource.CreateCalcStatisticsOperation(aFiles) as TFileSourceCalcStatisticsOperation;
  if Assigned(FFileSourceCalcStatisticsOperation) then
    begin
      FFileSourceCalcStatisticsOperation.AddStateChangedListener([fsosStopped], @FileSourceOperationStateChangedNotify);
      OperationsManager.AddOperation(FFileSourceCalcStatisticsOperation, ossAutoStart);
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

initialization
 {$I ffileproperties.lrs}

end.


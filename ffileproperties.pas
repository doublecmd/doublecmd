unit fFileProperties;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, uFileList, Buttons;

type
  TfrmFileProperties = class(TfrmLng)
    btnClose: TButton;
    btnNext: TButton;
    lblFileNameStr: TLabel;
    lblSizeStr: TLabel;
    lblFolderStr: TLabel;
    lblAttributesStr: TGroupBox;
    lblAttrOwnerStr: TLabel;
    lblAttrGroupStr: TLabel;
    lblAttrOtherStr: TLabel;
    lblLastAccessStr: TLabel;
    lblSymlinkStr: TLabel;
    lblOwnerStr: TLabel;
    lblGroupStr: TLabel;
    lblAttrTextStr: TLabel;
    lblAttrBitsStr: TLabel;
    lblLastModifStr: TLabel;
    lblLastStChangeStr: TLabel;
    lblTypeStr: TLabel;

    lblFileName: TLabel;
    lblFolder: TLabel;
    lblSize: TLabel;
    lblLastAccess: TLabel;
    lblLastModif: TLabel;
    lblLastStChange: TLabel;
    lblOwner: TLabel;
    lblGroup: TLabel;
    lblSymlink: TLabel;
    lblAttrOwner: TLabel;
    lblAttrGroup: TLabel;
    lblAttrOther: TLabel;
    lblAttrBits: TLabel;
    lblAttrText: TLabel;
    lblType: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    iCurrent:Integer;
    fFileList:TFileList;

    procedure AddAttrString(lblItem:TLabel; nText:Integer);
  public
    szPath:String;

    procedure LoadLng; override;
    procedure ShowFile(iIndex:Integer);
    procedure StoreData(FileList:TFileList);
    function FindNextSelected:Boolean;
  end;


procedure ShowFileProperties(FileList:TFileList; const aPath:String);

implementation

uses
  uLng, uFileProcs, FindEx, BaseUnix, Libc;

procedure ShowFileProperties(FileList:TFileList; const aPath:String);
begin
  with TfrmFileProperties.Create(Application) do
  try
    szPath:=aPath;
    StoreData(FileList);
    if FindNextSelected then
    begin
      ShowFile(iCurrent);
      ShowModal;
    end;
  finally
    Free;
  end;
end;

procedure TfrmFileProperties.LoadLng;
begin
  Caption := lngGetString(clngPropsTitle);
  btnClose.Caption := lngGetString(clngPropsClose);
  btnNext.Caption := lngGetString(clngPropsNext);

  lblFileNameStr.Caption := lngGetString(clngPropsStrName);
  lblSizeStr.Caption := lngGetString(clngPropsStrSize);
  lblFolderStr.Caption := lngGetString(clngPropsStrPath);
  lblAttributesStr.Caption := lngGetString(clngPropsStrAttrs);
  lblAttrOwnerStr.Caption := lngGetString(clngPropsStrOwner);
  lblAttrGroupStr.Caption := lngGetString(clngPropsStrGroup);
  lblAttrOtherStr.Caption := lngGetString(clngPropsStrOther);
  lblLastAccessStr.Caption := lngGetString(clngPropsStrLastAccess);
  lblSymlinkStr.Caption := lngGetString(clngPropsStrSymlink);
  lblOwnerStr.Caption := lngGetString(clngPropsStrOwner);
  lblGroupStr.Caption := lngGetString(clngPropsStrGroup);
  lblAttrTextStr.Caption := lngGetString(clngPropsStrAttrAlt);
  lblAttrBitsStr.Caption := lngGetString(clngPropsStrBits);
  lblLastModifStr.Caption := lngGetString(clngPropsStrLastChange);
  lblLastStChangeStr.Caption := lngGetString(clngPropsStrLastStatus);
  lblTypeStr.Caption := lngGetString(clngPropsStrType);
end;

procedure TfrmFileProperties.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFileProperties.ShowFile(iIndex:Integer);
var
  sb: FindEx.Stat64;
  dtFileDates:TDateTime;
  psUidRec:PPasswordRecord;
  psGidRec:PGroup;
begin
  try
    with fFileList.GetItem(iIndex)^ do
    begin
      fpstat64(PChar(szPath + sName), sb);

      lblFileName.Caption:=sName;
      lblFolder.Caption:=szPath;
      lblSize.Caption:=IntToStr(iSize);
      dtFileDates := FileStampToDateTime(sb.st_atime);
      lblLastAccess.Caption:=DateTimeToStr(dtFileDates);
      dtFileDates := FileStampToDateTime(sb.st_mtime);
      lblLastModif.Caption:=DateTimeToStr(dtFileDates);
      dtFileDates := FileStampToDateTime(sb.st_ctime);
      lblLastStChange.Caption:=DateTimeToStr(dtFileDates);
      if (bIsLink = True) then
        lblSymlink.Caption:=Format(lngGetString(clngPropsYes), [sLinkTo])
      else
        lblSymlink.Caption:=lngGetString(clngPropsNo);


      psUidRec := getpwuid(sb.st_uid);
      if not assigned(psUidRec) then
        lblOwner.Caption:=IntToStr(sb.st_uid)
      else
        lblOwner.Caption:=psUidRec^.pw_name;
      psGidRec := getgrgid(sb.st_gid);
      if not assigned(psGidRec) then
        lblGroup.Caption:=IntToStr(sb.st_gid)
      else
        lblGroup.Caption:=psGidRec^.gr_name;

      lblAttrOwner.Caption := '';
      lblAttrGroup.Caption := '';
      lblAttrOther.Caption := '';
      lblAttrBits.Caption := '';
      if ((sb.st_mode and S_IRUSR) = S_IRUSR) then
        AddAttrString(lblAttrOwner, clngPropsAttrRead);
      if ((sb.st_mode and S_IWUSR) = S_IWUSR) then
        AddAttrString(lblAttrOwner, clngPropsAttrWrite);
      if ((sb.st_mode and S_IXUSR) = S_IXUSR) then
        AddAttrString(lblAttrOwner, clngPropsAttrExec);

      if ((sb.st_mode and S_IRGRP) = S_IRGRP) then
        AddAttrString(lblAttrGroup, clngPropsAttrRead);
      if ((sb.st_mode and S_IWGRP) = S_IWGRP) then
        AddAttrString(lblAttrGroup, clngPropsAttrWrite);
      if ((sb.st_mode and S_IXGRP) = S_IXGRP) then
        AddAttrString(lblAttrGroup, clngPropsAttrExec);

      if ((sb.st_mode and S_IROTH) = S_IROTH) then
        AddAttrString(lblAttrOther, clngPropsAttrRead);
      if ((sb.st_mode and S_IWOTH) = S_IWOTH) then
        AddAttrString(lblAttrOther, clngPropsAttrWrite);
      if ((sb.st_mode and S_IXOTH) = S_IXOTH) then
        AddAttrString(lblAttrOther, clngPropsAttrExec);

      if ((sb.st_mode and S_ISUID) = S_ISUID) then
        AddAttrString(lblAttrBits, clngPropsAttrSetUID);
      if ((sb.st_mode and S_ISGID) = S_ISGID) then
        AddAttrString(lblAttrBits, clngPropsAttrSetGID);
      if ((sb.st_mode and S_ISVTX) = S_ISVTX) then
        AddAttrString(lblAttrBits, clngPropsAttrSticky);

      lblAttrText.Caption:=sModeStr; // + 666 like

      if FPS_ISDIR(iMode) then
        lblType.Caption:=lngGetString(clngPropsFolder)
      else if FPS_ISREG(iMode) then
        lblType.Caption:=lngGetString(clngPropsFile)
      else if FPS_ISCHR(iMode) then
        lblType.Caption:=lngGetString(clngPropsSpChrDev)
      else if FPS_ISBLK(iMode) then
        lblType.Caption:=lngGetString(clngPropsSpBlkDev)
      else if FPS_ISFIFO(iMode) then
        lblType.Caption:=lngGetString(clngPropsNmdPipe)
      else if FPS_ISLNK(iMode) then
        lblType.Caption:=lngGetString(clngPropsSymLink)
      else if FPS_ISSOCK(iMode) then
        lblType.Caption:=lngGetString(clngPropsSocket)
      else
        lblType.Caption:=lngGetString(clngPropsUnknownType);
    end;
  finally
  end;
end;

procedure TfrmFileProperties.AddAttrString(lblItem:TLabel; nText:Integer);
begin
  if Length(lblItem.Caption) > 0 then
    begin
      lblItem.Caption := lblItem.Caption + ', ' + lngGetString(nText);
    end
  else
    lblItem.Caption := lngGetString(nText);
end;

procedure TfrmFileProperties.StoreData(FileList:TFileList);
var
  i, nSelCount:Integer;
begin
  fFileList:=FileList;
  iCurrent:=0;
  nSelCount:=0;

  for i:=iCurrent to fFileList.Count-1 do
  begin
    if fFileList.GetItem(i)^.bSelected then
      inc(nSelCount);
  end;
  if (nSelCount > 1) then
    btnNext.Visible := True
  else
    btnNext.Visible := False;
end;

function TfrmFileProperties.FindNextSelected:Boolean;
var
  i:Integer;
begin
  for i:=iCurrent to fFileList.Count-1 do
  begin
    if fFileList.GetItem(i)^.bSelected then
    begin
      iCurrent:=i;
      Result:=True;
      Exit;
    end;
  end;
  Result:=False;
end;

procedure TfrmFileProperties.btnNextClick(Sender: TObject);
begin
  inc(iCurrent);
  if not FindNextSelected Then
    Close
  else
    ShowFile(iCurrent);
end;

procedure TfrmFileProperties.FormCreate(Sender: TObject);
begin
  inherited;
  lblFileNameStr.Font.Style:=[fsBold];
  lblFileName.Font.Style:=[fsBold];
end;

initialization
 {$I ffileproperties.lrs}

end.





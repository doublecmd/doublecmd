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
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uFileList, Buttons, ComCtrls;

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
    gbOwner: TGroupBox;
    lblAttrBitsStr: TLabel;
    lblAttrText: TLabel;
    lblExec: TLabel;
    lblFile: TLabel;
    lblFileName: TLabel;
    lblFileName1: TLabel;
    lblFileNameStr: TLabel;
    lblFolder: TLabel;
    lblFolderStr: TLabel;
    lblAttrGroupStr: TLabel;
    lblGroupStr: TLabel;
    lblLastAccess: TLabel;
    lblLastAccessStr: TLabel;
    lblLastModif: TLabel;
    lblLastModifStr: TLabel;
    lblLastStChange: TLabel;
    lblLastStChangeStr: TLabel;
    lblAttrOtherStr: TLabel;
    lblAttrOwnerStr: TLabel;
    lblOwnerStr: TLabel;

    lblRead: TLabel;
    lblSize: TLabel;
    lblSizeStr: TLabel;
    lblSymlink: TLabel;
    lblSymlinkStr: TLabel;
    lblAttrTextStr: TLabel;
    lblType: TLabel;
    lblTypeStr: TLabel;
    lblWrite: TLabel;
    pcPageControl: TPageControl;
    tsProperties: TTabSheet;
    tsAttributes: TTabSheet;
    procedure btnAllClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
  private
    bPerm: Boolean;
    iCurrent:Integer;
    fFileList:TFileList;

    procedure ShowAttr(iMode:Integer);
    procedure ChangeMod;
    procedure ChangeOwner;
    function GetModeFromForm:Integer;
  public
    szPath:String;

    procedure ShowFile(iIndex:Integer);
    procedure StoreData(FileList:TFileList);
    function FindNextSelected:Boolean;
  end;


procedure ShowFileProperties(FileList:TFileList; const aPath:String);

implementation

uses
  uLng, uFileOp, uFileProcs, uFindEx, BaseUnix, uUsersGroups, uDCUtils;

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

function TfrmFileProperties.GetModeFromForm:Integer;
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
  fpchmod(PChar(szPath + ffileList.GetItem(iCurrent)^.sName),GetModeFromForm);
end;

procedure TfrmFileProperties.ChangeOwner;
begin
  fpchown(PChar(ffileList.GetItem(iCurrent)^.sName),StrToUID(cbxUsers.Text),
               StrToGID(cbxGroups.Text));
end;

procedure TfrmFileProperties.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFileProperties.btnAllClick(Sender: TObject);
begin
  repeat
    ChangeMod;
    if(bPerm) then
      ChangeOwner;
    inc (iCurrent);
  until not FindNextSelected;
  Close;
end;

procedure TfrmFileProperties.ShowAttr(iMode:Integer);
begin
  cbReadOwner.Checked:= ((iMode AND S_IRUSR) = S_IRUSR);
  cbWriteOwner.Checked:= ((iMode AND S_IWUSR) = S_IWUSR);
  cbExecOwner.Checked:= ((iMode AND S_IXUSR) = S_IXUSR);
  
  cbReadGroup.Checked:= ((iMode AND S_IRGRP) = S_IRGRP);
  cbWriteGroup.Checked:= ((iMode AND S_IWGRP) = S_IWGRP);
  cbExecGroup.Checked:= ((iMode AND S_IXGRP) = S_IXGRP);
  
  cbReadOther.Checked:= ((iMode AND S_IROTH) = S_IROTH);
  cbWriteOther.Checked:= ((iMode AND S_IWOTH) = S_IWOTH);
  cbExecOther.Checked:= ((iMode AND S_IXOTH) = S_IXOTH);

  cbSuid.Checked:= ((iMode AND S_ISUID) = S_ISUID);
  cbSgid.Checked:= ((iMode AND S_ISGID) = S_ISGID);
  cbSticky.Checked:= ((iMode AND S_ISVTX) = S_ISVTX);
end;

procedure TfrmFileProperties.ShowFile(iIndex:Integer);
var
  sb: BaseUnix.Stat;
  dtFileDates:TDateTime;
  iMyUID: Cardinal;
begin
  try
    with fFileList.GetItem(iIndex)^ do
    begin
      fpStat(PChar(szPath + sName), sb);

      lblFileName.Caption:= sName;
      lblFileName1.Caption:= sName;
      lblFolder.Caption:= MinimizeFilePath(szPath, lblFolder.Canvas, lblFolder.Width);
      if not FPS_ISDIR(iMode) then
        lblSize.Caption:= IntToStr(iSize);
      
      dtFileDates := FileDateToDateTime(sb.st_atime);
      lblLastAccess.Caption:=DateTimeToStr(dtFileDates);
      dtFileDates := FileDateToDateTime(sb.st_mtime);
      lblLastModif.Caption:=DateTimeToStr(dtFileDates);
      dtFileDates := FileDateToDateTime(sb.st_ctime);
      lblLastStChange.Caption:=DateTimeToStr(dtFileDates);
      
      if (bIsLink = True) then
        lblSymlink.Caption:=Format(rsPropsYes, [sLinkTo])
      else
        lblSymlink.Caption:=rsPropsNo;
        
      // Chown

      begin
        iMyUID:=fpGetUID; //get user's UID
        bPerm:=(iMyUID=iOwner);
        cbxUsers.Text:=sOwner;
        if(imyUID=0) then GetUsers(cbxUsers.Items); //huh, a ROOT :))
          cbxUsers.Enabled:=(imyUID=0);
        cbxGroups.Text:=sGroup;
        if(bPerm or (iMyUID=0)) then
          GetUsrGroups(iMyUID,cbxGroups.Items);
        cbxGroups.Enabled:=(bPerm or (iMyUID=0));
      end;


      
      ShowAttr(iMode);

      lblAttrText.Caption:=sModeStr; // + 666 like

      if FPS_ISDIR(iMode) then
        lblType.Caption:=rsPropsFolder
      else if FPS_ISREG(iMode) then
        lblType.Caption:=rsPropsFile
      else if FPS_ISCHR(iMode) then
        lblType.Caption:=rsPropsSpChrDev
      else if FPS_ISBLK(iMode) then
        lblType.Caption:=rsPropsSpBlkDev
      else if FPS_ISFIFO(iMode) then
        lblType.Caption:=rsPropsNmdPipe
      else if FPS_ISLNK(iMode) then
        lblType.Caption:=rsPropsSymLink
      else if FPS_ISSOCK(iMode) then
        lblType.Caption:=rsPropsSocket
      else
        lblType.Caption:=rsPropsUnknownType;
    end;
  finally
  end;
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


procedure TfrmFileProperties.FormCreate(Sender: TObject);
begin
  inherited;
  lblFileNameStr.Font.Style:=[fsBold];
  lblFileName.Font.Style:=[fsBold];
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
  if not FindNextSelected Then
    Close
  else
    ShowFile(iCurrent);
end;

initialization
 {$I ffileproperties.lrs}

end.





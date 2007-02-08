{
   File name: fChown.pas
   Date:      2003/07/03
   Author:    Martin Matusu <xmat@volny.cz>

   Copyright (C) 2003

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

{mate}

unit fChown;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes,  Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, fLngForm, uFileList;

type
  TfrmChown = class(TfrmLng)
    btnCancel: TBitBtn;
    btnSkip: TBitBtn;
    btnOK: TBitBtn;
    btnAll: TBitBtn;
    cbxUsers: TComboBox;
    cbxGroups: TComboBox;
    lblFileName: TLabel;
    lblOwner: TLabel;
    lblGroup: TLabel;
    procedure btnSkipClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);

  private
    { Private declarations }
    bPerm: Boolean;
    iCurrent:Integer;
    ffileList:TFileList;
  public
    { Public declarations }
    procedure LoadLng; override;
    procedure StoreData(FileList:TFileList);
    function FindNextSelected:Boolean;
    procedure ShowFile(iIndex:Integer);
    procedure ChangeOwner;
  end;

  procedure ShowChownForm(FileList:TFileList; const aPath:String);


implementation


uses
  uLng, Unix, BaseUnix, uUsersGroups;

procedure ShowChownForm(FileList:TFileList; const aPath:String);
begin
  with TfrmChown.Create(Application) do
  begin
    try
//      Path:=aPath;
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
end;

procedure TfrmChown.StoreData(FileList:TFileList);
begin
  fFileList:=FileList;
  iCurrent:=0;
end;

function TfrmChown.FindNextSelected:Boolean;
var
  i:Integer;
begin
  Application.ProcessMessages;
  for i:=iCurrent to FFileList.Count-1 do
  begin
    if FFileList.GetItem(i)^.bSelected then
    begin
      iCurrent:=i;
      Result:=True;
      Exit;
    end;
  end;
  Result:=False;
end;

procedure TfrmChown.ShowFile(iIndex:Integer);
var
  iMyUID: Cardinal;
begin
  iMyUID:=fpGetUID; //get user's UID
  with ffileList.GetItem(iIndex)^ do
  begin
    bPerm:=(iMyUID=iOwner);
    lblFileName.Caption:=sName;
    cbxUsers.Text:=sOwner;
    if(imyUID=0) then GetUsers(cbxUsers.Items); //huh, a ROOT :))
    cbxUsers.Enabled:=(imyUID=0);
    cbxGroups.Text:=sGroup;
    if(bPerm or (iMyUID=0)) then
      GetUsrGroups(iMyUID,cbxGroups.Items);
    cbxGroups.Enabled:=(bPerm or (iMyUID=0));
  end;
end;

procedure TfrmChown.btnSkipClick(Sender: TObject);
begin
 inherited;
  inc (iCurrent);
  if not FindNextSelected Then
    Close
  else
    ShowFile(iCurrent);
end;

procedure TfrmChown.LoadLng;
begin
// load strings
  Caption:=lngGetString(clngChownDlg);
  lblOwner.Caption:=lngGetString(clngChownOwner);
  lblGroup.Caption:=lngGetString(clngChownGroup);
  btnCancel.Caption:=lngGetString(clngbutCancel);
  btnSkip.Caption:=lngGetString(clngbutSkip);
  btnAll.Caption:=lngGetString(clngbutAll);
end;

procedure TfrmChown.ChangeOwner;
begin
  fpchown(PChar(ffileList.GetItem(iCurrent)^.sName),StrToUID(cbxUsers.Text),
               StrToGID(cbxGroups.Text));
end;

procedure TfrmChown.btnOKClick(Sender: TObject);
begin
  inherited;
  if (bPerm) then
    ChangeOwner;
  btnSkipClick(Self);
end;

procedure TfrmChown.btnAllClick(Sender: TObject);
begin
  inherited;
  repeat
    if(bPerm) then
      ChangeOwner;
    inc (iCurrent);
  until not FindNextSelected;
  Close;
end;

initialization
 {$I fChown.lrs}
end.
{/mate}

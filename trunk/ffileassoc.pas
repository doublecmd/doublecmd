{
    Double Commander
    -------------------------------------------------------------------------
    File associations configuration

    Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fFileAssoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Buttons;

type

  { TfrmFileAssoc }

  TfrmFileAssoc = class(TForm)
    btnAddAct: TButton;
    btnAddExt: TButton;
    btnAddNewType: TButton;
    btnDownAct: TButton;
    btnUpAct: TButton;
    btnRemoveAct: TButton;
    btnRemoveExt: TButton;
    btnRemoveType: TButton;
    btnRenameType: TButton;
    btnModifyAct: TButton;
    bpButtonPanel: TButtonPanel;
    edtIconFileName: TEdit;
    gbFileTypes: TGroupBox;
    gbIcon: TGroupBox;
    gbExts: TGroupBox;
    gbActions: TGroupBox;
    lbActions: TListBox;
    lbExts: TListBox;
    lbFileTypes: TListBox;
    sbtnIcon: TSpeedButton;
    procedure btnAddExtClick(Sender: TObject);
    procedure btnDownActClick(Sender: TObject);
    procedure btnUpActClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lbFileTypesSelectionChange(Sender: TObject; User: boolean);
    procedure sbtnIconClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 


implementation
uses uExts, uGlobs, uOSForms, uPixMapManager;

{ TfrmFileAssoc }

procedure TfrmFileAssoc.FormCreate(Sender: TObject);
var
  I, iCount : Integer;
  sName : String;
begin
  iCount := gExts.ExtList.Count - 1;
  for I := 0 to iCount do
    begin
      sName := TExtAction(gExts.ExtList.Items[I]).Name;
      if sName = '' then
        sName := TExtAction(gExts.ExtList.Items[I]).SectionName;
      lbFileTypes.Items.AddObject(sName, gExts.ExtList.Items[I]);
    end;
end;

procedure TfrmFileAssoc.lbFileTypesSelectionChange(Sender: TObject;
  User: boolean);
var
  ExtCommand : TExtAction;
  I, iCount : Integer;
begin
  with Sender as TListBox do
    begin
      ExtCommand := TExtAction(Items.Objects[ItemIndex]);
      lbExts.Items.Assign(ExtCommand.Extensions);
      lbActions.Items.Clear;
      iCount := ExtCommand.Actions.Count - 1;
      for I := 0 to iCount do
        begin
          lbActions.Items.AddObject(ExtCommand.Actions.Names[I], ExtCommand.Actions);
        end;
    end;
end;

procedure TfrmFileAssoc.sbtnIconClick(Sender: TObject);
var
  sFileName : String;
begin
  if ShowOpenIconDialog(Self, sFileName) then
    begin
      edtIconFileName.Text := sFileName;
      sbtnIcon.Glyph := LoadBitmapFromFile(sFileName, 32, sbtnIcon.Color)
    end;
end;

procedure TfrmFileAssoc.btnAddExtClick(Sender: TObject);
var
  sExt : String;
begin
  sExt := InputBox(Caption, 'Enter file extension:', '');
  if sExt <> '' then
    lbExts.Items.Add(sExt);
end;

procedure TfrmFileAssoc.btnDownActClick(Sender: TObject);
var
  I : Integer;
begin
  with lbActions do
  begin
    I := ItemIndex;
    if I = - 1 then exit;
    if (I < Items.Count - 1) and (I > -1) then
    begin
      Items.Move(I, I + 1);
      ItemIndex:= I + 1;
    end;
  end;

end;

procedure TfrmFileAssoc.btnUpActClick(Sender: TObject);
var
  I : Integer;
begin
  with lbActions do
  begin
    I := ItemIndex;
    if I = - 1 then exit;
    if I > 0 then
    begin
      Items.Move(I, I - 1);
      ItemIndex:= I - 1;
    end;
  end;

end;

procedure TfrmFileAssoc.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

initialization
  {$I ffileassoc.lrs}

end.


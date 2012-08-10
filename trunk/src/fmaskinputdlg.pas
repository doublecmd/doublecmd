{
   Double Commander
   -------------------------------------------------------------------------
   File mask input dialog

   Copyright (C) 2010-2012 Koblov Alexander (Alexx2000@mail.ru)

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

unit fMaskInputDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, StdCtrls, Buttons;

type

  { TfrmMaskInputDlg }

  TfrmMaskInputDlg = class(TForm)
    btnDefineTemplate: TBitBtn;
    lblPrompt: TLabel;
    lblSearchTemplate: TLabel;
    cmbMask: TComboBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lbxSearchTemplate: TListBox;
    procedure btnDefineTemplateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxSearchTemplateClick(Sender: TObject);
    procedure lbxSearchTemplateDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

function ShowMaskInputDlg(const sCaption, sPrompt: UTF8String; slValueList: TStringList; var sValue: UTF8String): Boolean;

implementation

{$R *.lfm}

uses
  fFindDlg, uGlobs, uSearchTemplate;

function ShowMaskInputDlg(const sCaption, sPrompt: UTF8String;
                                slValueList: TStringList; var sValue: UTF8String): Boolean;
var
  Index: Integer;
begin
  Result:= False;
  with TfrmMaskInputDlg.Create(Application) do
  try
    Caption:= sCaption;
    lblPrompt.Caption:= sPrompt;
    cmbMask.Items.Assign(slValueList);
    cmbMask.Text:= sValue;
    if IsMaskSearchTemplate(sValue) then
    begin
      Index:= lbxSearchTemplate.Items.IndexOf(PAnsiChar(sValue) + 1);
      if Index >= 0 then lbxSearchTemplate.ItemIndex:= Index;
    end;
    if ShowModal = mrOK then
      begin
        if not IsMaskSearchTemplate(cmbMask.Text) then
          if slValueList.IndexOf(cmbMask.Text) < 0 then
            slValueList.Add(cmbMask.Text);
        sValue:= cmbMask.Text;
        Result:= True;
      end;
  finally
    Free;
  end;
end;

{ TfrmMaskInputDlg }

procedure TfrmMaskInputDlg.lbxSearchTemplateClick(Sender: TObject);
begin
  if lbxSearchTemplate.ItemIndex < 0 then Exit;
  cmbMask.Text:= cTemplateSign + lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
end;

procedure TfrmMaskInputDlg.lbxSearchTemplateDblClick(Sender: TObject);
begin
  if lbxSearchTemplate.ItemIndex < 0 then Exit;
  cmbMask.Text:= cTemplateSign + lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
  Close;
  ModalResult:= mrOK;
end;

procedure TfrmMaskInputDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I:= 0 to gSearchTemplateList.Count - 1 do
    lbxSearchTemplate.Items.Add(gSearchTemplateList.Templates[I].TemplateName);
end;

procedure TfrmMaskInputDlg.btnDefineTemplateClick(Sender: TObject);
var
  sTemplateName: UTF8String;
begin
  if lbxSearchTemplate.ItemIndex >= 0 then
    sTemplateName:= lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
  if ShowDefineTemplateDlg(sTemplateName) then
  begin
    lbxSearchTemplate.ItemIndex:= lbxSearchTemplate.Items.Add(sTemplateName);
    cmbMask.Text:= cTemplateSign + sTemplateName;
  end;
end;

end.


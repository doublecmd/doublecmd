{
   Double Commander
   -------------------------------------------------------------------------
   File mask input dialog

   Copyright (C) 2010-2016 Alexander Koblov (alexx2000@mail.ru)

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
  { TMaskInputDlgStyle }
  TMaskInputDlgStyle = (midsLegacy, midsFull);

  { TfrmMaskInputDlg }
  TfrmMaskInputDlg = class(TForm)
    btnDefineTemplate: TBitBtn;
    chkIgnoreAccentsAndLigatures: TCheckBox;
    chkCaseSensitive: TCheckBox;
    lblPrompt: TLabel;
    lblSearchTemplate: TLabel;
    cmbMask: TComboBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lbxSearchTemplate: TListBox;
    lblAttributes: TLabel;
    edtAttrib: TEdit;
    btnAddAttribute: TButton;
    btnAttrsHelp: TButton;
    procedure btnDefineTemplateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxSearchTemplateClick(Sender: TObject);
    procedure lbxSearchTemplateDblClick(Sender: TObject);
    procedure btnAddAttributeClick(Sender: TObject);
    procedure btnAttrsHelpClick(Sender: TObject);
  private
    { private declarations }
    procedure OnAddAttribute(Sender: TObject);
  public
    { public declarations }
  end;

function ShowMaskInputDlg(const sCaption, sPrompt: string; slValueList: TStringList; var sValue: string): boolean;
function ShowExtendedMaskInputDlg(const sCaption, sPrompt: string; slValueList: TStringList; var sValue: string; AMaskInputDlgStyle: TMaskInputDlgStyle; var bCaseSensitive: boolean;
  var bIgnoreAccents: boolean; var sAttribute:string): boolean;

implementation

{$R *.lfm}

uses
  HelpIntfs, fAttributesEdit, fFindDlg, uGlobs, uSearchTemplate;

{ ShowMaskInputDlg }
function ShowMaskInputDlg(const sCaption, sPrompt: string; slValueList: TStringList; var sValue: string): boolean;
var
  dummybCaseSensitive: boolean = False;
  dummybIgnoreAccents: boolean = False;
  dummysAttribute: string = '';
begin
  Result := ShowExtendedMaskInputDlg(sCaption, sPrompt, slValueList, sValue, midsLegacy, dummybCaseSensitive, dummybIgnoreAccents, dummysAttribute);
end;

{ ShowExtendedMaskInputDlg }
function ShowExtendedMaskInputDlg(const sCaption, sPrompt: string; slValueList: TStringList; var sValue: string; AMaskInputDlgStyle: TMaskInputDlgStyle; var bCaseSensitive: boolean;
  var bIgnoreAccents: boolean; var sAttribute:string): boolean;
var
  Index, iCurrentPos: integer;
begin
  Result := False;
  with TfrmMaskInputDlg.Create(Application) do
    try
      Caption := sCaption;
      lblPrompt.Caption := sPrompt;
      cmbMask.Items.Assign(slValueList);
      cmbMask.Text := sValue;
      edtAttrib.Text := sAttribute;

      case AMaskInputDlgStyle of
        midsFull:
        begin
          chkCaseSensitive.Checked := bCaseSensitive;
          chkIgnoreAccentsAndLigatures.Checked := bIgnoreAccents;
        end;

        midsLegacy:
        begin
          chkIgnoreAccentsAndLigatures.Visible := False;
          chkCaseSensitive.Visible := False;
        end;
      end;

      // Don't show the attribute filter if we're in legacy request mode OR if user request to don't use it.
      if (AMaskInputDlgStyle=midsLegacy) OR (not gMarkShowWantedAttribute) then
      begin
        lblAttributes.Visible := False;
        btnAddAttribute.Visible := False;
        btnAttrsHelp.Visible := False;
        edtAttrib.Visible := False;
      end;

      if IsMaskSearchTemplate(sValue) then
      begin
        Index := lbxSearchTemplate.Items.IndexOf(PAnsiChar(sValue) + 1);
        if Index >= 0 then lbxSearchTemplate.ItemIndex := Index;
      end;

      if ShowModal = mrOk then
      begin
        if not IsMaskSearchTemplate(cmbMask.Text) then
        begin
          iCurrentPos := slValueList.IndexOf(cmbMask.Text);
          if iCurrentPos <> -1 then slValueList.Delete(iCurrentPos);
          if slValueList.Count = 0 then
            slValueList.Add(cmbMask.Text)
          else
            slValueList.Insert(0, cmbMask.Text);
        end;
        sValue := cmbMask.Text;
        bCaseSensitive := chkCaseSensitive.Checked;
        bIgnoreAccents := chkIgnoreAccentsAndLigatures.Checked;
        sAttribute := edtAttrib.Text;
        Result := True;
      end;
    finally
      Free;
    end;
end;

{ TfrmMaskInputDlg }

procedure TfrmMaskInputDlg.lbxSearchTemplateClick(Sender: TObject);
begin
  if lbxSearchTemplate.ItemIndex < 0 then Exit;
  cmbMask.Text := cTemplateSign + lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
end;

procedure TfrmMaskInputDlg.lbxSearchTemplateDblClick(Sender: TObject);
begin
  if lbxSearchTemplate.ItemIndex < 0 then Exit;
  cmbMask.Text := cTemplateSign + lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
  Close;
  ModalResult := mrOk;
end;

procedure TfrmMaskInputDlg.FormCreate(Sender: TObject);
var
  I: integer;
begin
  InitPropStorage(Self);
  for I := 0 to gSearchTemplateList.Count - 1 do
    lbxSearchTemplate.Items.Add(gSearchTemplateList.Templates[I].TemplateName);
end;

procedure TfrmMaskInputDlg.btnDefineTemplateClick(Sender: TObject);
var
  sTemplateName: string;
begin
  if lbxSearchTemplate.ItemIndex >= 0 then
    sTemplateName := lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
  if ShowDefineTemplateDlg(sTemplateName) then
  begin
    lbxSearchTemplate.ItemIndex := lbxSearchTemplate.Items.Add(sTemplateName);
    cmbMask.Text := cTemplateSign + sTemplateName;
  end;
end;

procedure TfrmMaskInputDlg.btnAddAttributeClick(Sender: TObject);
var
  FFrmAttributesEdit: TfrmAttributesEdit;
begin
  FFrmAttributesEdit := TfrmAttributesEdit.Create(Self);
  try
    FFrmAttributesEdit.OnOk := @OnAddAttribute;
    FFrmAttributesEdit.Reset;
    FFrmAttributesEdit.ShowModal;
  finally
    FFrmAttributesEdit.Free;
  end;
end;

procedure TfrmMaskInputDlg.btnAttrsHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', edtAttrib.HelpKeyword);
end;

procedure TfrmMaskInputDlg.OnAddAttribute(Sender: TObject);
var
  sAttr: String;
begin
  sAttr := edtAttrib.Text;
  if edtAttrib.SelStart > 0 then    
    Insert((Sender as TfrmAttributesEdit).AttrsAsText, sAttr, edtAttrib.SelStart + 1) // Insert at caret position.
  else
    sAttr := sAttr + (Sender as TfrmAttributesEdit).AttrsAsText;
  edtAttrib.Text := sAttr;
end;

end.

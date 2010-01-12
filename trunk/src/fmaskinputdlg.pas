unit fMaskInputDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfrmMaskInputDlg }

  TfrmMaskInputDlg = class(TForm)
    lblPrompt: TLabel;
    lblSearchTemplate: TLabel;
    cmbMask: TComboBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lbxSearchTemplate: TListBox;
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

uses
  uGlobs, uSearchTemplate;

function ShowMaskInputDlg(const sCaption, sPrompt: UTF8String;
                                slValueList: TStringList; var sValue: UTF8String): Boolean;
begin
  Result:= False;
  with TfrmMaskInputDlg.Create(Application) do
  try
    Caption:= sCaption;
    lblPrompt.Caption:= sPrompt;
    cmbMask.Items.Assign(slValueList);
    cmbMask.Text := sValue;
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
  cmbMask.Text:= cTemplateSign + lbxSearchTemplate.Items[lbxSearchTemplate.ItemIndex];
end;

procedure TfrmMaskInputDlg.lbxSearchTemplateDblClick(Sender: TObject);
begin
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

initialization
  {$I fmaskinputdlg.lrs}

end.


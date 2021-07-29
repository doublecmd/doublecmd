{
   Seksi Commander
   ----------------------------
   Find dialog for Viewer

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:


}

unit fFindView;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, uOSForms;

type

  { TfrmFindView }

  TfrmFindView = class(TModalForm)
    cbDataToFind: TComboBox;
    btnFind: TBitBtn;
    btnClose: TBitBtn;
    cbCaseSens: TCheckBox;
    cbBackwards: TCheckBox;
    chkHex: TCheckBox;
    cbRegExp: TCheckBox;
    procedure cbBackwardsChange(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure chkHexChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure cbDataToFindKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function GetTextSearchOptions: UIntPtr;
  public
    { Public declarations }
  end;


implementation

{$R *.lfm}

uses
  LCLProc, LCLType, uFindFiles, uDCUtils;

procedure TfrmFindView.FormShow(Sender: TObject);
begin
  if cbDataToFind.Text = EmptyStr then
    begin
      if cbDataToFind.Items.Count > 0 then
        cbDataToFind.Text:= cbDataToFind.Items[0];
    end;
  cbDataToFind.SelectAll;
end;

procedure TfrmFindView.chkHexChange(Sender: TObject);
begin
  if not chkHex.Checked then
    cbCaseSens.Checked:= Boolean(cbCaseSens.Tag)
  else begin
    cbCaseSens.Tag:= Integer(cbCaseSens.Checked);
    cbCaseSens.Checked:= True;
  end;
  cbCaseSens.Enabled:= not chkHex.Checked;
end;

procedure TfrmFindView.FormActivate(Sender: TObject);
begin
  cbDataToFind.SetFocus;
end;

procedure TfrmFindView.cbBackwardsChange(Sender: TObject);
begin
  if cbBackwards.Checked then cbRegExp.Checked:= False
end;

procedure TfrmFindView.cbRegExpChange(Sender: TObject);
begin
  if cbRegExp.Checked then cbBackwards.Checked:= False;
end;

procedure TfrmFindView.btnFindClick(Sender: TObject);
begin
  InsertFirstItem(cbDataToFind.Text, cbDataToFind, GetTextSearchOptions);
  ModalResult:= mrOk;
end;

procedure TfrmFindView.cbDataToFindKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Down) and (cbDataToFind.Items.Count > 0) then
    cbDataToFind.DroppedDown:= True;
  if Key = 13 then
  begin
    Key:= 0;
    btnFind.Click;
  end;
  if Key = 27 then
  begin
    Key:= 0;
    ModalResult:= mrCancel;
  end;
end;

function TfrmFindView.GetTextSearchOptions: UIntPtr;
var
  Options: TTextSearchOptions absolute Result;
begin
  Result:= 0;
  if cbCaseSens.Checked then
    Include(Options, tsoMatchCase);
  if cbRegExp.Checked then
    Include(Options, tsoRegExpr);
  if chkHex.Checked then
    Include(Options, tsoHex);
end;

end.

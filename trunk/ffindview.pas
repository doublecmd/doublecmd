{
Seksi Commander
----------------------------
Find dialog for Viewer

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:


}
{$mode objfpc}{$H+}
unit fFindView;

interface

uses
  LResources, LClType,
  SysUtils, Types, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, Buttons;

type
  TfrmFindView = class(TfrmLng)
    cbDataToFind: TComboBox;
    btnFind: TBitBtn;
    btnClose: TBitBtn;
    cbCaseSens: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure cbDataToFindKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadLng; override;
  end;


implementation

uses
  uLng;
  
procedure TfrmFindView.LoadLng;
begin
  Caption:=   lngGetString(clngViewFnd);
  btnFind.Caption:= lngGetString(clngbutFind);
  cbCaseSens.Caption:= lngGetString(clngViewFndCase);
  btnClose.Caption:= lngGetString(clngbutClose);
end;

procedure TfrmFindView.FormShow(Sender: TObject);
begin
  inherited;
  cbDataToFind.SelectAll;
end;

procedure TfrmFindView.btnFindClick(Sender: TObject);
begin
  inherited;
  cbDataToFind.Items.Add(cbDataToFind.Text);
  ModalResult:=mrOk;
end;

procedure TfrmFindView.cbDataToFindKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key=VK_Down) and (cbDataToFind.Items.Count>0) then
    cbDataToFind.DroppedDown:=True;
  writeln(Key);
  if Key=13 then
  begin
    Key:=0;
    btnFind.Click;
  end;
  if Key=27 then
  begin
    Key:=0;
    ModalResult:=mrCancel;
  end;

end;


initialization
 {$I fFindView.lrs}

end.

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
  LResources, LClType,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmFindView = class(TForm)
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
  end;


implementation

uses
  LCLProc, uDCUtils;

procedure TfrmFindView.FormShow(Sender: TObject);
begin
  if cbDataToFind.Text = EmptyStr then
    begin
      if cbDataToFind.Items.Count > 0 then
        cbDataToFind.Text:= cbDataToFind.Items[0];
    end;
  cbDataToFind.SelectAll;
end;

procedure TfrmFindView.btnFindClick(Sender: TObject);
begin
  InsertFirstItem(cbDataToFind.Text, cbDataToFind);
  ModalResult:= mrOk;
end;

procedure TfrmFindView.cbDataToFindKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = VK_Down) and (cbDataToFind.Items.Count > 0) then
    cbDataToFind.DroppedDown:= True;
  DebugLn(IntToStr(Key));
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

initialization
 {$I ffindview.lrs}

end.

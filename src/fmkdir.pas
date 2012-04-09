unit fMkDir;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls;

type

  { TfrmMkDir }

  TfrmMkDir = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    cbMkDir: TComboBox;
    lblMakeDir: TLabel;
    pnlButtons: TPanel;
    pnlBottom: TPanel;

    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
   function ShowMkDir(var sPath:string):Boolean;  // 21.05.2009 - перенес в public
  end;

var  frmMkDir: TfrmMkDir;  // 21.05.2009  - создаем из файла проекта

implementation

{$R *.lfm}

procedure TfrmMkDir.FormKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#27 then
  begin
    ModalResult:=mrCancel;
    Key := #0;
  end
  else if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

Function TfrmMkDir.ShowMkDir(var sPath:string):Boolean; var Index: Integer;
const
  MAX_LINES = 20;
begin
  ActiveControl := cbMkDir;
  if (sPath <> '..') then
    cbMkDir.Text := sPath
  else
    cbMkDir.Text := '';
  cbMkDir.SelectAll;
  Result := (ShowModal = mrOK);
  sPath := cbMkDir.Text;
  If Result then
   begin
     Index := cbMkDir.Items.IndexOf(cbMkDir.Text);
     if (Index = -1) then
       cbMkDir.Items.Insert(0, cbMkDir.Text)
     else
       cbMkDir.Items.Move(Index, 0);

     if (cbMkDir.Items.Count > MAX_LINES) then
       cbMkDir.Items.Delete(cbMkDir.Items.Count - 1);
   end;
end;

end.

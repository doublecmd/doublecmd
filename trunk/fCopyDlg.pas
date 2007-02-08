unit fCopyDlg;

interface

uses
  LResources,
  SysUtils, Types, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, Buttons;

type
  TfrmCopyDlg = class(TfrmLng)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtDst: TEdit;
    lblCopySrc: TLabel;
    cmbFileType: TComboBox;
    lblFileType: TLabel;
    procedure frmCopyDlgKeyPress(Sender: TObject; var Key: Char);
    procedure frmCopyDlgShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadLng; override;
  end;

var
  frmCopyDlg: TfrmCopyDlg;

implementation
uses
  uLng;

procedure TfrmCopyDlg.frmCopyDlgShow(Sender: TObject);
begin
  edtDst.SelectAll;
  edtDst.SetFocus;
end;

procedure TfrmCopyDlg.frmCopyDlgKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then
    ModalResult:=mrCancel;
  if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

procedure TfrmCopyDlg.LoadLng;
begin
  Caption:= lngGetString(clngDlgCp);
//  lblCopySrc.Caption:= lngGetString(clngDlgCpSrc); handle by MainForm
  lblFileType.Caption:= lngGetString(clngDlgCpType);
end;

initialization
 {$I fCopyDlg.lrs}
end.

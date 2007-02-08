unit fMkDir;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, fLngForm;

type
  TfrmMkDir = class(TfrmLng)
    edtMkDir: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblMakeDir: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    procedure LoadLng; override;
  end;

Function ShowMkDir(var sPath:string):Boolean;


implementation
uses
  uLng;


procedure TfrmMkDir.LoadLng;
begin
  Caption:= lngGetString(clngDlgMkDir);
  lblMakeDir.Caption:= lngGetString(clngDlgMkDirInput);
end;

procedure TfrmMkDir.FormKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#27 then
    ModalResult:=mrCancel;
  if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

Function ShowMkDir(var sPath:string):Boolean;
begin
  with TfrmMkDir.Create(Application) do
  try
    Result:=ShowModal=mrOK;
    sPath:=Trim(edtMkDir.Text);
{    if Result then
      MkDir(sPath+edtMkDir.Text);}
  finally
    Free;
  end;
end;

initialization
 {$I fMkDir.lrs}

end.

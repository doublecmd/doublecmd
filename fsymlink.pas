unit fSymLink;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, Buttons;

type
  TfrmSymLink = class(TfrmLng)
    lblNew: TLabel;
    lblDst: TLabel;
    edtNew: TEdit;
    edtDst: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure frmSymLinkKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }

  public
    procedure LoadLng; override;
  end;

procedure ShowSymLinkForm(const sNew, sDst:String);

implementation

uses
  uLng, uShowMsg, uOSUtils;

procedure ShowSymLinkForm(const sNew, sDst:String);
begin
  with TfrmSymLink.Create(Application) do
  begin
    try
      edtDst.Text:=sDst;
      edtNew.Text:=sNew;
      ShowModal;
    finally
      Free;
    end;
  end;
end;


procedure TfrmSymLink.LoadLng;
begin
  Caption:=lngGetString(clngSymLink);
  lblNew.Caption:=lngGetString(clngSymLinkDst);
  lblDst.Caption:=lngGetString(clngSymLinkNew);
end;

procedure TfrmSymLink.btnOKClick(Sender: TObject);
var
  sSrc,sDst:String;
begin
  inherited;
  sSrc:=edtNew.Text;
  sDst:=edtDst.Text;
  if CreateSymLink(sSrc, sDst) then
    Close
  else
  begin
    MsgError(lngGetString(clngSymErrCreate));
  end;
end;

procedure TfrmSymLink.frmSymLinkKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then
    ModalResult:=mrCancel;
  if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

initialization
 {$I fsymlink.lrs}

end.

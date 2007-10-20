unit fHardLink;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, Buttons;

type
  TfrmHardLink = class(TfrmLng)
    lblNew: TLabel;
    lblDst: TLabel;
    edtNew: TEdit;
    edtDst: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }

  public
    procedure LoadLng; override;
  end;

procedure ShowHardLinkForm(const sNew, sDst:String);

implementation


uses
  uLng, uShowMsg, uOSUtils;

procedure ShowHardLinkForm(const sNew, sDst:String);
begin
  with TfrmHardLink.Create(Application) do
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


procedure TfrmHardLink.LoadLng;
begin
  Caption:=lngGetString(clngHardLink);
  lblNew.Caption:=lngGetString(clngHardLinkDst);
  lblDst.Caption:=lngGetString(clngHardLinkNew);
end;

procedure TfrmHardLink.btnOKClick(Sender: TObject);
var
  sSrc,sDst:String;
begin
  inherited;
  sSrc:=edtNew.Text;
  sDst:=edtDst.Text;
  if CreateHardLink(sSrc, sDst) then
    Close
  else
  begin
    MsgError(lngGetString(clngHardErrCreate));
  end;
end;

initialization
 {$I fhardlink.lrs}
end.

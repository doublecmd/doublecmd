unit fSymLink;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmSymLink = class(TForm)
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
    { Public declarations }
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
    MsgError(rsSymErrCreate);
  end;
end;

initialization
 {$I fsymlink.lrs}

end.

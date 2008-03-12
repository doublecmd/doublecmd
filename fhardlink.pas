unit fHardLink;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmHardLink = class(TForm)
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

function ShowHardLinkForm(const sNew, sDst:String): Boolean;

implementation


uses
  uLng, uGlobs, uLog, uShowMsg, uOSUtils;

function ShowHardLinkForm(const sNew, sDst:String): Boolean;
begin
  with TfrmHardLink.Create(Application) do
  begin
    try
      edtDst.Text:=sDst;
      edtNew.Text:=sNew;
      Result:= (ShowModal = mrOK);
    finally
      Free;
    end;
  end;
end;

procedure TfrmHardLink.btnOKClick(Sender: TObject);
var
  sSrc,sDst:String;
begin
  inherited;
  sSrc:=edtNew.Text;
  sDst:=edtDst.Text;
  if CreateHardLink(sSrc, sDst) then
    begin
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
        logWrite(Format(rsMsgLogSuccess+rsMsgLogLink,[sSrc+' -> '+sDst]), lmtSuccess);

      Close;
    end
  else
    begin
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
        logWrite(Format(rsMsgLogError+rsMsgLogLink,[sSrc+' -> '+sDst]), lmtError);

      // Standart error modal dialog
      MsgError(rsHardErrCreate);
    end;
end;

initialization
 {$I fhardlink.lrs}
 
end.

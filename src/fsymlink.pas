unit fSymLink;

interface

uses
  LResources,
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons;

type

  { TfrmSymLink }

  TfrmSymLink = class(TForm)
    lblExistingFile: TLabel;
    lblLinkToCreate: TLabel;
    edtExistingFile: TEdit;
    edtLinkToCreate: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnCancelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure btnOKMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);

  private
    FCurrentPath: String;

  public
    constructor Create(TheOwner: TComponent;
                       CurrentPath: String); reintroduce;
  end;

function ShowSymLinkForm(const sExistingFile, sLinkToCreate, CurrentPath: String): Boolean;

implementation

uses
  FileUtil, uLng, uGlobs, uLog, uShowMsg, uOSUtils, uDCUtils;

function ShowSymLinkForm(const sExistingFile, sLinkToCreate, CurrentPath: String): Boolean;
begin
  with TfrmSymLink.Create(Application, CurrentPath) do
  begin
    try
      edtLinkToCreate.Text := sLinkToCreate;
      edtExistingFile.Text := sExistingFile;
      Result:= (ShowModal = mrOK);
    finally
      Free;
    end;
  end;
end;

constructor TfrmSymLink.Create(TheOwner: TComponent;
                               CurrentPath: String);
begin
  inherited Create(TheOwner);
  FCurrentPath := CurrentPath;
end;

procedure TfrmSymLink.btnOKClick(Sender: TObject);
var
  sSrc,sDst:String;
begin
  inherited;
  sSrc:=edtExistingFile.Text;
  sDst:=edtLinkToCreate.Text;

  if CompareFilenames(sSrc, sDst) = 0 then Exit;

  if GetPathType(sSrc) <> ptAbsolute then
    sSrc := FCurrentPath + sSrc;
  if GetPathType(sDst) <> ptAbsolute then
    sDst := FCurrentPath + sDst;

  if CreateSymLink(sSrc, sDst) then
    begin
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
        logWrite(Format(rsMsgLogSuccess+rsMsgLogSymLink,[sSrc+' -> '+sDst]), lmtSuccess);
    end
  else
    begin
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
        logWrite(Format(rsMsgLogError+rsMsgLogSymLink,[sSrc+' -> '+sDst]), lmtError);

      // Standart error modal dialog
      MsgError(rsSymErrCreate);
    end;
end;

procedure TfrmSymLink.btnCancelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ModalResult := btnCancel.ModalResult;
end;

procedure TfrmSymLink.btnOKMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ModalResult := btnOK.ModalResult;
  btnOKClick(Sender);
end;

procedure TfrmSymLink.FormShow(Sender: TObject);
begin
  edtLinkToCreate.SelectAll;
end;

initialization
 {$I fsymlink.lrs}

end.

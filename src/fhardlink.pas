unit fHardLink;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons;

type

  { TfrmHardLink }

  TfrmHardLink = class(TForm)
    lblExistingFile: TLabel;
    lblLinkToCreate: TLabel;
    edtExistingFile: TEdit;
    edtLinkToCreate: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FCurrentPath: String;

  public
    constructor Create(TheOwner: TComponent;
                       CurrentPath: String); reintroduce;
  end;

function ShowHardLinkForm(const sExistingFile, sLinkToCreate, CurrentPath: String): Boolean;

implementation

{$R *.lfm}

uses
  FileUtil, uLng, uGlobs, uLog, uShowMsg, uOSUtils, DCStrUtils;

function ShowHardLinkForm(const sExistingFile, sLinkToCreate, CurrentPath: String): Boolean;
begin
  with TfrmHardLink.Create(Application, CurrentPath) do
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

constructor TfrmHardLink.Create(TheOwner: TComponent;
                                CurrentPath: String);
begin
  inherited Create(TheOwner);
  FCurrentPath := CurrentPath;
end;

procedure TfrmHardLink.btnOKClick(Sender: TObject);
var
  sSrc,sDst:String;
begin
  sSrc:=edtExistingFile.Text;
  sDst:=edtLinkToCreate.Text;

  if CompareFilenames(sSrc, sDst) = 0 then Exit;

  sSrc := GetAbsoluteFileName(FCurrentPath, sSrc);
  sDst := GetAbsoluteFileName(FCurrentPath, sDst);

  if CreateHardLink(sSrc, sDst) then
    begin
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
        logWrite(Format(rsMsgLogSuccess+rsMsgLogLink,[sSrc+' -> '+sDst]), lmtSuccess);
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

procedure TfrmHardLink.FormShow(Sender: TObject);
begin
  edtLinkToCreate.SelectAll;
end;

end.
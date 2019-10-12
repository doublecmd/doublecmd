unit fSymLink;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons;

type

  { TfrmSymLink }

  TfrmSymLink = class(TForm)
    chkUseRelativePath: TCheckBox;
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

function ShowSymLinkForm(const sExistingFile, sLinkToCreate, CurrentPath: String): Boolean;

implementation

{$R *.lfm}

uses
  LazFileUtils, uLng, uGlobs, uLog, uShowMsg, DCStrUtils, DCOSUtils, uAdministrator;

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
  sSrc, sDst, Message: String;
begin
  sSrc:=edtExistingFile.Text;
  sDst:=edtLinkToCreate.Text;

  if CompareFilenames(sSrc, sDst) = 0 then Exit;

  sDst := GetAbsoluteFileName(FCurrentPath, sDst);

  if chkUseRelativePath.Checked then begin
    sSrc:= CreateRelativePath(sSrc, ExtractFileDir(sDst));
  end;

  ElevateAction:= dupIgnore;

  if CreateSymbolicLinkUAC(sSrc, sDst) then
    begin
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
        logWrite(Format(rsMsgLogSuccess+rsMsgLogSymLink,[sSrc+' -> '+sDst]), lmtSuccess);
    end
  else
    begin
      Message:= mbSysErrorMessage;
      // write log
      if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
        logWrite(Format(rsMsgLogError+rsMsgLogSymLink,[sSrc+' -> '+sDst]), lmtError);
      // Standart error modal dialog
      MsgError(rsSymErrCreate + LineEnding + LineEnding + Message);
    end;
end;

procedure TfrmSymLink.FormShow(Sender: TObject);
begin
  edtLinkToCreate.SelectAll;
end;

end.

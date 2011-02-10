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
    procedure btnCancelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure btnOKMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  FileUtil, uLng, uGlobs, uLog, uShowMsg, uOSUtils, uDCUtils;

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

  if GetPathType(sSrc) <> ptAbsolute then
    sSrc := FCurrentPath + sSrc;
  if GetPathType(sDst) <> ptAbsolute then
    sDst := FCurrentPath + sDst;

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

procedure TfrmHardLink.btnCancelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
    ModalResult := btnCancel.ModalResult;
{$ENDIF}
end;

procedure TfrmHardLink.btnOKMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
  begin
    ModalResult := btnOK.ModalResult;
    btnOKClick(Sender);
  end;
{$ENDIF}
end;

procedure TfrmHardLink.FormShow(Sender: TObject);
begin
  edtLinkToCreate.SelectAll;
end;

end.

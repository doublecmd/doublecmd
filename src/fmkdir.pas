unit fMkDir;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls;

type

  { TfrmMkDir }

  TfrmMkDir = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    cbMkDir: TComboBox;
    lblMakeDir: TLabel;
    pnlButtons: TPanel;
    pnlBottom: TPanel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public

  end;

function ShowMkDir(TheOwner: TComponent; var sPath: String): Boolean;

implementation

{$R *.lfm}

uses
  uGlobs;

procedure TfrmMkDir.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    ModalResult:= mrCancel;
    Key := #0;
  end
  else if Key = #13 then
  begin
    ModalResult:= mrOK;
    Key:= #0;
  end;
end;

function ShowMkDir(TheOwner: TComponent; var sPath: String): Boolean;
const
  MAX_LINES = 20;
var
  Index: Integer;
begin
  with TfrmMkDir .Create(TheOwner) do
  try
    ActiveControl := cbMkDir;
    cbMkDir.Items.Assign(glsCreateDirectoriesHistory);
    if (sPath <> '..') then
      cbMkDir.Text := sPath
    else begin
      cbMkDir.Text := '';
    end;
    cbMkDir.SelectAll;
    Result := (ShowModal = mrOK);
    if Result then
    begin
      sPath := TrimRight(cbMkDir.Text);
      sPath := StringReplace(sPath, ' ' + PathDelim, PathDelim, [rfReplaceAll]);
      glsCreateDirectoriesHistory.CaseSensitive := FileNameCaseSensitive;
      Index := glsCreateDirectoriesHistory.IndexOf(cbMkDir.Text);

      if (Index = -1) then
        glsCreateDirectoriesHistory.Insert(0, sPath)
      else
        glsCreateDirectoriesHistory.Move(Index, 0);

      if (glsCreateDirectoriesHistory.Count > MAX_LINES) then
        glsCreateDirectoriesHistory.Delete(glsCreateDirectoriesHistory.Count - 1);
    end;
  finally
    Free;
  end;
end;

end.

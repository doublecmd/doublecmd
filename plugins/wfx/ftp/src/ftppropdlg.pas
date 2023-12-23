unit FtpPropDlg;

{$mode delphi}
{$include calling.inc}

interface

uses
  SysUtils, Extension, FtpFunc;

function ShowPropertiesDlg(const AText: String): Boolean;

implementation

{$R ftppropdlg.lfm}

function DlgProc(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
var
  Data: PtrInt;
  Text: PString absolute Data;
begin
  Result:= 0;
  with gStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
      begin
        Data:= SendDlgMsg(pDlg, nil, DM_GETDLGDATA, 0, 0);
        Data:= PtrInt(PAnsiChar(Text^));
        SendDlgMsg(pDlg, 'seProperties', DM_SETTEXT, Data, 0);
      end;
    end;
  end; // with
end;

function ShowPropertiesDlg(const AText: String): Boolean;
var
  ResHandle: TFPResourceHandle = 0;
  ResGlobal: TFPResourceHGLOBAL = 0;
  ResData: Pointer = nil;
  ResSize: LongWord;
begin
  Result := False;
  try
    ResHandle := FindResource(HINSTANCE, PChar('TfrmFileProperties'), MAKEINTRESOURCE(10) {RT_RCDATA});
    if ResHandle <> 0 then
    begin
      ResGlobal := LoadResource(HINSTANCE, ResHandle);
      if ResGlobal <> 0 then
      begin
        ResData := LockResource(ResGlobal);
        ResSize := SizeofResource(HINSTANCE, ResHandle);

        with gStartupInfo do
        begin
          Result := (DialogBoxParam(ResData, ResSize, @DlgProc, DB_LRS, @AText, nil) > 0);
        end;
      end;
    end;

  finally
    if ResGlobal <> 0 then
    begin
      UnlockResource(ResGlobal);
      FreeResource(ResGlobal);
    end;
  end;
end;

end.


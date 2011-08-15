unit SmbAuthDlg;

{$mode objfpc}{$H+}

{$R smbauthdlg.lfm}

interface

uses
  SysUtils, Extension;

function ShowSmbAuthDlg: Boolean;

implementation

uses
  SmbFunc;

function DlgProc (pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; cdecl;
var
 Data: PtrInt;
 Text: UTF8String;
begin
  with ExtensionStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          Text:= PAnsiChar(SendDlgMsg(pDlg, 'lblMessage', DM_GETTEXT, 0, 0));
          Data:= PtrInt(PAnsiChar(Format(Text, [Message])));
          SendDlgMsg(pDlg, 'lblMessage', DM_SETTEXT, Data, 0);
          Data:= PtrInt(PAnsiChar(UserName));
          SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
          Data:= PtrInt(PAnsiChar(WorkGroup));
          SendDlgMsg(pDlg, 'edtDomain', DM_SETTEXT, Data, 0);
          Data:= PtrInt(PAnsiChar(Password));
          SendDlgMsg(pDlg, 'edtPassword', DM_SETTEXT, Data, 0);
        end;
      DN_CLICK:
        if DlgItemName = 'btnOK' then
          begin
            Data:= SendDlgMsg(pDlg, 'edtUserName', DM_GETTEXT, 0, 0);
            StrLCopy(UserName, PAnsiChar(Data), MAX_PATH);
            Data:= SendDlgMsg(pDlg, 'edtDomain', DM_GETTEXT, 0, 0);
            StrLCopy(WorkGroup, PAnsiChar(Data), MAX_PATH);
            Data:= SendDlgMsg(pDlg, 'edtPassword', DM_GETTEXT, 0, 0);
            StrLCopy(Password, PAnsiChar(Data), MAX_PATH);
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, ID_OK, 0);
          end
        else if DlgItemName = 'btnCancel' then
          begin
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, ID_CANCEL, 0);
          end;
    end;// case
  end; // with
end;

function ShowSmbAuthDlg: Boolean;
var
  ResHandle: TFPResourceHandle = 0;
  ResGlobal: TFPResourceHGLOBAL = 0;
  ResData: Pointer = nil;
  ResSize: LongWord;
begin
  Result := False;
  try
    ResHandle := FindResource(HINSTANCE, PChar('TDIALOGBOX'), MAKEINTRESOURCE(10) {RT_RCDATA});
    if ResHandle <> 0 then
    begin
      ResGlobal := LoadResource(HINSTANCE, ResHandle);
      if ResGlobal <> 0 then
      begin
        ResData := LockResource(ResGlobal);
        ResSize := SizeofResource(HINSTANCE, ResHandle);

        with ExtensionStartupInfo do
        begin
          Result := DialogBoxLRS(ResData, ResSize, @DlgProc);
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

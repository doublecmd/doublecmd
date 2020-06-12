unit RarConfDlg;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils;

procedure LoadConfig;
procedure CreateRarConfDlg;

var
  Args: String;
  Method: Integer;
  Recovery: Boolean;
  Encrypt: Boolean;
  Solid: Boolean;
{$IF DEFINED(MSWINDOWS)}
  WinRar: String = '%ProgramFiles%\WinRAR\WinRAR.exe';
{$ELSE}
  WinRar: String = '/usr/bin/rar';
{$ENDIF}

implementation

uses
  IniFiles, UnRARFunc, Extension, RarFunc;

{$R *.lfm}

procedure LoadConfig;
var
  gIni: TIniFile;
begin
  try
    gIni:= TIniFile.Create(IniFileName);
    try
      Args:= gIni.ReadString('unrar', 'Args', EmptyStr);
      WinRar:= gIni.ReadString('unrar', 'Path', WinRar);
      Method:= gIni.ReadInteger('unrar', 'Method', 3);
      Recovery:= gIni.ReadBool('unrar', 'Recovery', False);
      Encrypt:= gIni.ReadBool('unrar', 'Encrypt', False);
      Solid:= gIni.ReadBool('unrar', 'Solid', False);
    finally
      gIni.Free;
    end;
  except
  end;
end;

procedure SaveConfig;
var
  gIni: TIniFile;
begin
  try
    gIni:= TIniFile.Create(IniFileName);
    try
      gIni.WriteString('unrar', 'Args', Args);
      gIni.WriteString('unrar', 'Path', WinRar);
      gIni.WriteInteger('unrar', 'Method', Method);
      gIni.WriteBool('unrar', 'Recovery', Recovery);
      gIni.WriteBool('unrar', 'Encrypt', Encrypt);
      gIni.WriteBool('unrar', 'Solid', Solid);
    finally
      gIni.Free;
    end;
  except
  end;
end;

function DlgProc (pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
begin
  Result:= 0;
  with gStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          SendDlgMsg(pDlg, 'cmbMethod', DM_LISTSETITEMINDEX, Method, 0);
          SendDlgMsg(pDlg, 'chkRecovery', DM_SETCHECK, PtrInt(Recovery), 0);
          SendDlgMsg(pDlg, 'chkEncrypt', DM_SETCHECK, PtrInt(Encrypt), 0);
          SendDlgMsg(pDlg, 'chkSolid', DM_SETCHECK, PtrInt(Solid), 0);
          SendDlgMsg(pDlg, 'edtArgs', DM_SETTEXT, PtrInt(PAnsiChar(Args)), 0);
          SendDlgMsg(pDlg, 'fnePath', DM_SETTEXT, PtrInt(PAnsiChar(WinRar)), 0);
        end;
      DN_CLICK:
        if DlgItemName = 'btnSave' then
          begin
            Args:= PAnsiChar(SendDlgMsg(pDlg, 'edtArgs', DM_GETTEXT, 0, 0));
            WinRar:= PAnsiChar(SendDlgMsg(pDlg, 'fnePath', DM_GETTEXT, 0, 0));
            Method:= SendDlgMsg(pDlg, 'cmbMethod', DM_LISTGETITEMINDEX, 0, 0);
            Recovery:= Boolean(SendDlgMsg(pDlg, 'chkRecovery', DM_GETCHECK, 0, 0));
            Encrypt:= Boolean(SendDlgMsg(pDlg, 'chkEncrypt', DM_GETCHECK, 0, 0));
            Solid:= Boolean(SendDlgMsg(pDlg, 'chkSolid', DM_GETCHECK, 0, 0));
            SaveConfig;
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 1, 0);
          end
        else if DlgItemName = 'btnCancel' then
          SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 2, 0);
    end;// case
  end; // with
end;

procedure CreateRarConfDlg;
var
  ResHandle: TFPResourceHandle = 0;
  ResGlobal: TFPResourceHGLOBAL = 0;
  ResData: Pointer = nil;
  ResSize: LongWord;
begin
  try
    ResHandle := FindResource(HINSTANCE, PChar('TDIALOGBOX'), MAKEINTRESOURCE(10) {RT_RCDATA});
    if ResHandle <> 0 then
    begin
      ResGlobal := LoadResource(HINSTANCE, ResHandle);
      if ResGlobal <> 0 then
      begin
        ResData := LockResource(ResGlobal);
        ResSize := SizeofResource(HINSTANCE, ResHandle);

        with gStartupInfo do
        begin
          DialogBoxLRS(ResData, ResSize, @DlgProc);
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


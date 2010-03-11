{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit FtpConfDlg;

{$mode objfpc}{$H+}

{$R FtpConfDlg.lfm}

interface

uses
  SysUtils, DialogAPI;

function ShowFtpConfDlg: Boolean;
  
implementation

uses
  FtpFunc, FtpUtils;

function DlgProc (pDlg: PtrUInt; DlgItemName: PChar; Msg, wParam, lParam: PtrInt): PtrInt; stdcall;
var
 Data: PtrInt;
 wsText: WideString;
begin
  with gSetDlgProcInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          wsText:= gConnection.ConnectionName;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtName', DM_SETTEXT, Data, 0);
          wsText:= gConnection.Host;
          if gConnection.Port <> EmptyStr then
            wsText:= wsText + ':' + gConnection.Port;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtHost', DM_SETTEXT, Data, 0);
          wsText:= gConnection.UserName;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
          if gConnection.MasterPassword then
            begin
              SendDlgMsg(pDlg, 'chkMasterPassword', DM_SETCHECK, 1, 0);
              SendDlgMsg(pDlg, 'chkMasterPassword', DM_ENABLE, 0, 0);
              SendDlgMsg(pDlg, 'edtPassword', DM_SHOWITEM, 0, 0);
              SendDlgMsg(pDlg, 'btnChangePassword', DM_SHOWITEM, 1, 0);
            end
          else
            begin
              wsText:= gConnection.Password;
              Data:= PtrInt(PWideChar(wsText));
              SendDlgMsg(pDlg, 'edtPassword', DM_SETTEXT, Data, 0);
            end;
          wsText:= gConnection.Path;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtRemoteDir', DM_SETTEXT, Data, 0);
          wsText:= gConnection.InitCommands;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtInitCommands', DM_SETTEXT, Data, 0);
          Data:= PtrInt(gConnection.PassiveMode);
          SendDlgMsg(pDlg, 'chkPassiveMode', DM_SETCHECK, Data, 0);
        end;
      DN_CHANGE:
        if DlgItemName = 'chkMasterPassword' then
          begin
            Data:= SendDlgMsg(pDlg, 'chkMasterPassword', DM_GETCHECK, 0, 0);
            gConnection.MasterPassword:= Boolean(Data);
            if not gConnection.MasterPassword then
              DeletePassword(gConnection.ConnectionName);
          end
        else if DlgItemName = 'chkSendCommand' then
          begin
            SendDlgMsg(pDlg, 'cmbCommand', DM_ENABLE, wParam, 0);
            SendDlgMsg(pDlg, 'edtInterval', DM_ENABLE, wParam, 0);
          end;
      DN_CLICK:
        if DlgItemName = 'btnAnonymous' then
          begin
            wsText:= 'anonymous';
            Data:= PtrInt(PWideChar(wsText));
            SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
          end
        else if DlgItemName = 'btnChangePassword' then
          begin
            wsText:= ReadPassword(gConnection.ConnectionName);
            if wsText <> EmptyStr then
              begin
                Data:= PtrInt(PWideChar(wsText));
                SendDlgMsg(pDlg, 'edtPassword', DM_SETTEXT, Data, 0);
                SendDlgMsg(pDlg, 'edtPassword', DM_SHOWITEM, 1, 0);
                SendDlgMsg(pDlg, 'btnChangePassword', DM_SHOWITEM, 0, 0);
                SendDlgMsg(pDlg, 'chkMasterPassword', DM_ENABLE, 1, 0);
                gConnection.PasswordChanged:= True;
              end;
          end
        else if DlgItemName = 'btnOK' then
          begin
            Data:= SendDlgMsg(pDlg, 'edtName', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.ConnectionName:= wsText;
            Data:= SendDlgMsg(pDlg, 'edtHost', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.Host:= ExtractConnectionHost(wsText);
            gConnection.Port:= ExtractConnectionPort(wsText);
            Data:= SendDlgMsg(pDlg, 'edtUserName', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.UserName:= wsText;
            Data:= SendDlgMsg(pDlg, 'edtPassword', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.Password:= wsText;
            Data:= SendDlgMsg(pDlg, 'chkMasterPassword', DM_GETCHECK, 0, 0);
            gConnection.MasterPassword:= Boolean(Data);
            Data:= SendDlgMsg(pDlg, 'edtRemoteDir', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.Path:= wsText;
            Data:= SendDlgMsg(pDlg, 'edtInitCommands', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.InitCommands:= wsText;
            Data:= SendDlgMsg(pDlg, 'chkPassiveMode', DM_GETCHECK, 0, 0);
            gConnection.PassiveMode:= Boolean(Data);
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 1, 0);
          end
        else if DlgItemName = 'btnCancel' then
          begin
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 2, 0);
          end;
    end;// case
  end; // with
end;

function ShowFtpConfDlg: Boolean;
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

        with gSetDlgProcInfo do
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
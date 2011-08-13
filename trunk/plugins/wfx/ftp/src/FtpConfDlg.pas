{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009-2011  Koblov Alexander (Alexx2000@mail.ru)

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
{$include calling.inc}

{$R FtpConfDlg.lfm}

interface

uses
  SysUtils, Extension;

function ShowFtpConfDlg: Boolean;
  
implementation

uses
  FtpFunc, FtpUtils;

function DlgProc (pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
var
 Data: PtrInt;
 Text: UTF8String;
begin
  with gStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          Text:= gConnection.ConnectionName;
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtName', DM_SETTEXT, Data, 0);
          Text:= gConnection.Host;
          if gConnection.Port <> EmptyStr then
            Text:= Text + ':' + gConnection.Port;
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtHost', DM_SETTEXT, Data, 0);
          Text:= gConnection.UserName;
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
          if gConnection.MasterPassword then
            begin
              SendDlgMsg(pDlg, 'chkMasterPassword', DM_SETCHECK, 1, 0);
              SendDlgMsg(pDlg, 'chkMasterPassword', DM_ENABLE, 0, 0);
              //SendDlgMsg(pDlg, 'edtPassword', DM_SHOWITEM, 0, 0);
              SendDlgMsg(pDlg, 'btnChangePassword', DM_SHOWITEM, 1, 0);
            end
          else
            begin
              Text:= gConnection.Password;
              Data:= PtrInt(PAnsiChar(Text));
              SendDlgMsg(pDlg, 'edtPassword', DM_SETTEXT, Data, 0);
            end;
          Text:= gConnection.Path;
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtRemoteDir', DM_SETTEXT, Data, 0);
          Text:= gConnection.InitCommands;
          Data:= PtrInt(PAnsiChar(Text));
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
          end;
      DN_CLICK:
        if DlgItemName = 'btnAnonymous' then
          begin
            Text:= 'anonymous';
            Data:= PtrInt(PAnsiChar(Text));
            SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
          end
        else if DlgItemName = 'btnChangePassword' then
          begin
            Text:= ReadPassword(gConnection.ConnectionName);
            if Text <> EmptyStr then
              begin
                Data:= PtrInt(PAnsiChar(Text));
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
            Text:= PAnsiChar(Data);
            gConnection.ConnectionName:= StringReplace(Text, PathDelim, '_', [rfReplaceAll]);
            Data:= SendDlgMsg(pDlg, 'edtHost', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.Host:= ExtractConnectionHost(Text);
            gConnection.Port:= ExtractConnectionPort(Text);
            Data:= SendDlgMsg(pDlg, 'edtUserName', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.UserName:= Text;
            Data:= SendDlgMsg(pDlg, 'edtPassword', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.Password:= Text;
            Data:= SendDlgMsg(pDlg, 'chkMasterPassword', DM_GETCHECK, 0, 0);
            gConnection.MasterPassword:= Boolean(Data);
            Data:= SendDlgMsg(pDlg, 'edtRemoteDir', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.Path:= Text;
            Data:= SendDlgMsg(pDlg, 'edtInitCommands', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.InitCommands:= Text;
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

        with gStartupInfo do
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

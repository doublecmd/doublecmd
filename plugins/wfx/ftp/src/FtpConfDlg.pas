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

interface

uses
  SysUtils, DialogAPI;

function ShowFtpConfDlg: Boolean;
  
implementation

uses
  FtpFunc;

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
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtHost', DM_SETTEXT, Data, 0);
          wsText:= gConnection.UserName;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
          wsText:= gConnection.Path;
          Data:= PtrInt(PWideChar(wsText));
          SendDlgMsg(pDlg, 'edtRemoteDir', DM_SETTEXT, Data, 0);
        end;
      DN_CHANGE:
        if DlgItemName = 'chkSendCommand' then
          begin
            SendDlgMsg(pDlg, 'cmbCommand', DM_ENABLE, wParam, 0);
            SendDlgMsg(pDlg, 'edtInterval', DM_ENABLE, wParam, 0);
          end;
      DN_CLICK:
        if DlgItemName = 'btnOK' then
          begin
            Data:= SendDlgMsg(pDlg, 'edtName', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.ConnectionName:= wsText;
            Data:= SendDlgMsg(pDlg, 'edtHost', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.Host:= wsText;
            Data:= SendDlgMsg(pDlg, 'edtUserName', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.UserName:= wsText;
            Data:= SendDlgMsg(pDlg, 'edtRemoteDir', DM_GETTEXT, 0, 0);
            wsText:= PWideChar(Data);
            gConnection.Path:= wsText;
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 0, 0);
          end
        else if DlgItemName = 'btnCancel' then
          begin
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 0, 0);
          end;
    end;// case
  end; // with
end;

function ShowFtpConfDlg: Boolean;
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(gPluginDir) + 'FtpConfDlg.lfm';

  with gSetDlgProcInfo do
  begin
    Result:= DialogBoxEx(PWideChar(wFileName), @DlgProc);
  end;
end;

end.

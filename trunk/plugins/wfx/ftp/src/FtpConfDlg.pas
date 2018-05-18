{
   Double commander
   -------------------------------------------------------------------------
   Wfx plugin for working with File Transfer Protocol

   Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

unit FtpConfDlg;

{$mode objfpc}{$H+}
{$include calling.inc}

{$R FtpConfDlg.lfm}

interface

uses
  SysUtils, Extension, FtpFunc;

function ShowFtpConfDlg(Connection: TConnection): Boolean;
  
implementation

uses
  LazUTF8, DynLibs, FtpUtils, blcksock, ssl_openssl_lib, libssh, FtpProxy, TypInfo;

var
  ProxyIndex: Integer;
  gConnection: TConnection;

procedure ShowWarningSSL;
begin
  with gStartupInfo do
  begin
    MessageBox(PAnsiChar('OpenSSL library not found!' + LineEnding +
               'To use SSL connections, please install the OpenSSL ' +
               'libraries (' + DLLSSLName + ' and ' + DLLUtilName + ')!'),
               'OpenSSL', MB_OK or MB_ICONERROR
               );
  end;
end;

procedure ShowWarningSSH;
begin
  with gStartupInfo do
  begin
    MessageBox(PAnsiChar('LibSSH2 library not found!' + LineEnding +
               'To use SSH2 connections, please install the LibSSH2 ' +
               'library (' + LibSSHName + ')!'),
               'LibSSH2', MB_OK or MB_ICONERROR
               );
  end;
end;

procedure EnableControls(pDlg: PtrUInt);
begin
  with gStartupInfo do
  begin
    SendDlgMsg(pDlg, 'gbSSH', DM_ENABLE, PtrInt(gConnection.OpenSSH), 0);
    SendDlgMsg(pDlg, 'gbFTP', DM_ENABLE, PtrInt(not gConnection.OpenSSH), 0);
    if not gConnection.OpenSSH then
      SendDlgMsg(pDlg, 'chkOnlySCP', DM_SETCHECK, 0, 0)
    else begin
      SendDlgMsg(pDlg, 'chkShowHidden', DM_SETCHECK, 0, 0);
      SendDlgMsg(pDlg, 'chkPassiveMode', DM_SETCHECK, 0, 0);
      SendDlgMsg(pDlg, 'chkKeepAliveTransfer', DM_SETCHECK, 0, 0);
    end;
  end;
end;

function CreateProxyID: String;
var
  Guid: TGuid;
begin
  if CreateGUID(Guid) = 0 then
    Result := GUIDToString(Guid)
  else
    Result := IntToStr(Random(MaxInt));
end;

function GetProxyName(Proxy: TFtpProxy): String;
begin
  Result:= Proxy.Host;
  if Proxy.Port <> '' then Result+= ':' + Proxy.Port;
  Result+= ' (' + GetEnumName(TypeInfo(TProxyType), Integer(Proxy.ProxyType)) + ')';
end;

procedure LoadProxy(pDlg: PtrUInt);
var
  Data: PtrInt;
  Text: String;
  Proxy: TFtpProxy;
begin
  with gStartupInfo do
  begin
    if (ProxyIndex > 0) then
    begin
      Proxy:= TFtpProxy(SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETDATA, ProxyIndex, 0));
      SendDlgMsg(pDlg, 'rgProxyType', DM_LISTSETITEMINDEX, PtrInt(Proxy.ProxyType) - 1, 0);
      Text:= Proxy.Host;
      if Proxy.Port <> EmptyStr then Text+= ':' + Proxy.Port;
      Data:= PtrInt(PAnsiChar(Text));
      SendDlgMsg(pDlg, 'edtProxyHost', DM_SETTEXT, Data, 0);
      Data:= PtrInt(PAnsiChar(Proxy.User));
      SendDlgMsg(pDlg, 'edtProxyUser', DM_SETTEXT, Data, 0);
      Data:= PtrInt(PAnsiChar(Proxy.Password));
      SendDlgMsg(pDlg, 'edtProxyPassword', DM_SETTEXT, Data, 0);
    end
    else begin
      SendDlgMsg(pDlg, 'rgProxyType', DM_LISTSETITEMINDEX, 0, 0);
      SendDlgMsg(pDlg, 'edtProxyHost', DM_SETTEXT, 0, 0);
      SendDlgMsg(pDlg, 'edtProxyUser', DM_SETTEXT, 0, 0);
      SendDlgMsg(pDlg, 'edtProxyPassword', DM_SETTEXT, 0, 0);
    end;
    SendDlgMsg(pDlg, 'gbLogon', DM_ENABLE, ProxyIndex, 0);
    SendDlgMsg(pDlg, 'rgProxyType', DM_ENABLE, ProxyIndex, 0);
    SendDlgMsg(pDlg, 'btnDelete', DM_ENABLE, ProxyIndex, 0);
  end;
end;

procedure UpdateProxy(pDlg: PtrUInt);
var
  Data: PtrInt;
  Text: String;
  Proxy: TFtpProxy;
begin
  if (ProxyIndex > 0) then
  begin
    with gStartupInfo do
    begin
      Proxy:= TFtpProxy(SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETDATA, ProxyIndex, 0));
      Data:= SendDlgMsg(pDlg, 'rgProxyType', DM_LISTGETITEMINDEX, 0, 0);
      Proxy.ProxyType:= TProxyType(Data + 1);
      Text:= PAnsiChar(SendDlgMsg(pDlg, 'edtProxyHost', DM_GETTEXT, 0, 0));
      Proxy.Host:= ExtractConnectionHost(Text);
      Proxy.Port:= ExtractConnectionPort(Text);
      if Length(Text) > 0 then
      begin
        Text:= GetProxyName(Proxy);
        Data:= PtrInt(PAnsiChar(Text));
        SendDlgMsg(pDlg, 'cmbProxy', DM_LISTUPDATE, ProxyIndex, Data);
      end;
      Data:= SendDlgMsg(pDlg, 'edtProxyUser', DM_GETTEXT, 0, 0);
      Proxy.User:= PAnsiChar(Data);
      Data:= SendDlgMsg(pDlg, 'edtProxyPassword', DM_GETTEXT, 0, 0);
      Proxy.Password:= PAnsiChar(Data);
    end;
  end;
end;

procedure LoadProxyList(pDlg: PtrUInt);
var
  Data: PtrInt;
  Text: String;
  Index: Integer;
  Proxy: TFtpProxy;
begin
  ProxyIndex:= ProxyList.IndexOf(gConnection.Proxy) + 1;
  with gStartupInfo do
  begin
    Data:= PtrInt(PAnsiChar('(None)'));
    SendDlgMsg(pDlg, 'cmbProxy', DM_LISTADDSTR, Data, 0);
    for Index:= 0 to ProxyList.Count - 1 do
    begin
      Proxy:= TFtpProxy(ProxyList.Objects[Index]).Clone;
      Text:= GetProxyName(Proxy);
      Data:= PtrInt(PAnsiChar(Text));
      SendDlgMsg(pDlg, 'cmbProxy', DM_LISTADD, Data, PtrInt(Proxy));
    end;
    SendDlgMsg(pDlg, 'cmbProxy', DM_LISTSETITEMINDEX, ProxyIndex, 0);
  end;
  LoadProxy(pDlg);
end;

procedure SaveProxyList(pDlg: PtrUInt);
var
  Count: Integer;
  Index: Integer;
  Proxy: TFtpProxy;
begin
  with gStartupInfo do
  begin
    ProxyList.Clear;
    UpdateProxy(pDlg);
    Count:= SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETCOUNT, 0, 0);
    for Index:= 1 to Count - 1 do
    begin
      Proxy:= TFtpProxy(SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETDATA, Index, 0));
      ProxyList.AddObject(Proxy.ID, Proxy);
    end;
  end;
  if ProxyIndex > 0 then
    gConnection.Proxy:= TFtpProxy(ProxyList.Objects[ProxyIndex - 1]).ID
  else
    gConnection.Proxy:= EmptyStr;
end;

procedure FreeProxyList(pDlg: PtrUInt);
var
  Count: Integer;
  Index: Integer;
begin
  with gStartupInfo do
  begin
    Count:= SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETCOUNT, 0, 0);
    for Index:= 1 to Count - 1 do
    begin
      TFtpProxy(SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETDATA, Index, 0)).Free;
    end;
  end;
end;

function DlgProc (pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; dcpcall;
var
  Data: PtrInt;
  Text: String;
  Proxy: TFtpProxy;
begin
  Result:= 0;
  with gStartupInfo do
  begin
    case Msg of
      DN_INITDIALOG:
        begin
          Text:= gConnection.Encoding;
          Data:= PtrInt(PAnsiChar(Text));
          Data:= SendDlgMsg(pDlg, 'cmbEncoding', DM_LISTINDEXOF, 0, Data);
          if Data >= 0 then SendDlgMsg(pDlg, 'cmbEncoding', DM_LISTSETITEMINDEX, Data, 0);
          Text:= gConnection.ConnectionName;
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtName', DM_SETTEXT, Data, 0);
          Text:= gConnection.Host;
          if gConnection.FullSSL then
            Text:= 'ftps://' + Text;
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
          Text:= SysToUTF8(gConnection.Path);
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtRemoteDir', DM_SETTEXT, Data, 0);
          Text:= gConnection.InitCommands;
          Data:= PtrInt(PAnsiChar(Text));
          SendDlgMsg(pDlg, 'edtInitCommands', DM_SETTEXT, Data, 0);
          Data:= PtrInt(gConnection.PassiveMode);
          SendDlgMsg(pDlg, 'chkPassiveMode', DM_SETCHECK, Data, 0);
          Data:= PtrInt(gConnection.AutoTLS);
          SendDlgMsg(pDlg, 'chkAutoTLS', DM_SETCHECK, Data, 0);
          Data:= PtrInt(gConnection.OpenSSH);
          SendDlgMsg(pDlg, 'chkOpenSSH', DM_SETCHECK, Data, 0);
          Data:= PtrInt(gConnection.OnlySCP);
          SendDlgMsg(pDlg, 'chkOnlySCP', DM_SETCHECK, Data, 0);
          Data:= PtrInt(gConnection.ShowHiddenItems);
          SendDlgMsg(pDlg, 'chkShowHidden', DM_SETCHECK, Data, 0);
          Data:= PtrInt(gConnection.KeepAliveTransfer);
          SendDlgMsg(pDlg, 'chkKeepAliveTransfer', DM_SETCHECK, Data, 0);

          Data:= PtrInt(PAnsiChar(gConnection.PublicKey));
          SendDlgMsg(pDlg, 'fnePublicKey', DM_SETTEXT, Data, 0);
          Data:= PtrInt(PAnsiChar(gConnection.PrivateKey));
          SendDlgMsg(pDlg, 'fnePrivateKey', DM_SETTEXT, Data, 0);

          if SameText(gConnection.ConnectionName, cQuickConnection) then
          begin
            SendDlgMsg(pDlg, 'edtName', DM_ENABLE, 0, 0);
            SendDlgMsg(pDlg, 'chkMasterPassword', DM_SHOWITEM, 0, 0);
          end;

          EnableControls(pDlg);

          LoadProxyList(pDlg);
        end;
      DN_CHANGE:
        begin
        if DlgItemName = 'chkMasterPassword' then
          begin
            Data:= SendDlgMsg(pDlg, 'chkMasterPassword', DM_GETCHECK, 0, 0);
            gConnection.MasterPassword:= Boolean(Data);
            gConnection.PasswordChanged:= True;
          end
        else if DlgItemName = 'chkAutoTLS' then
          begin
            Data:= SendDlgMsg(pDlg, 'chkAutoTLS', DM_GETCHECK, 0, 0);
            gConnection.AutoTLS:= Boolean(Data);
            if gConnection.AutoTLS then
            begin
              gConnection.OpenSSH:= False;
              if not InitSSLInterface then
              begin
                ShowWarningSSL;
                gConnection.AutoTLS:= False;
                Data:= PtrInt(gConnection.AutoTLS);
                SendDlgMsg(pDlg, 'chkAutoTLS', DM_SETCHECK, Data, 0);
              end;
              SendDlgMsg(pDlg, 'chkOpenSSH', DM_SETCHECK, 0, 0);
            end;
            EnableControls(pDlg);
          end
        else if DlgItemName = 'chkOpenSSH' then
          begin
            Data:= SendDlgMsg(pDlg, 'chkOpenSSH', DM_GETCHECK, 0, 0);
            gConnection.OpenSSH:= Boolean(Data);
            if gConnection.OpenSSH then
            begin
              if libssh2 = NilHandle then
              begin
                ShowWarningSSH;
                gConnection.OpenSSH:= False;
                Data:= PtrInt(gConnection.OpenSSH);
                SendDlgMsg(pDlg, 'chkOpenSSH', DM_SETCHECK, Data, 0);
               end;
              SendDlgMsg(pDlg, 'chkAutoTLS', DM_SETCHECK, 0, 0);
            end;
            EnableControls(pDlg);
          end
        else if DlgItemName = 'cmbProxy' then
          begin
            UpdateProxy(pDlg);
            ProxyIndex:= SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETITEMINDEX, 0, 0);
            // Load current proxy settings
            LoadProxy(pDlg);
          end;
        end;
      DN_CLICK:
        if DlgItemName = 'btnAnonymous' then
          begin
            Text:= 'anonymous';
            Data:= PtrInt(PAnsiChar(Text));
            SendDlgMsg(pDlg, 'edtUserName', DM_SETTEXT, Data, 0);
            Text:= 'anonymous@example.org';
            Data:= PtrInt(PAnsiChar(Text));
            SendDlgMsg(pDlg, 'edtPassword', DM_SETTEXT, Data, 0);
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
        else if DlgItemName = 'btnAdd' then
        begin
          UpdateProxy(pDlg);
          Proxy:= TFtpProxy.Create;
          Proxy.ID:= CreateProxyID;
          Data:= PtrInt(PAnsiChar(Proxy.ID));
          ProxyIndex:= SendDlgMsg(pDlg, 'cmbProxy', DM_LISTADD, Data, PtrInt(Proxy));
          SendDlgMsg(pDlg, 'cmbProxy', DM_LISTSETITEMINDEX, ProxyIndex, 0);
          LoadProxy(pDlg);
        end
        else if DlgItemName = 'btnDelete' then
        begin
          Data:= SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETITEMINDEX, 0, 0);
          TFtpProxy(SendDlgMsg(pDlg, 'cmbProxy', DM_LISTGETDATA, Data, 0)).Free;
          SendDlgMsg(pDlg, 'cmbProxy', DM_LISTDELETE, Data, 0);
          ProxyIndex:= 0;
          SendDlgMsg(pDlg, 'cmbProxy', DM_LISTSETITEMINDEX, 0, 0);
          LoadProxy(pDlg);
        end
        else if DlgItemName = 'btnOK' then
          begin
            Data:= SendDlgMsg(pDlg, 'cmbEncoding', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.Encoding:= Text;
            Data:= SendDlgMsg(pDlg, 'edtName', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.ConnectionName:= RepairConnectionName(Text);
            Data:= SendDlgMsg(pDlg, 'edtHost', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            if (Length(Text) = 0) or (Length(gConnection.ConnectionName) = 0) then
            begin
              gStartupInfo.MessageBox('You MUST at least specify a connection and host name!',
                                      nil, MB_OK or MB_ICONERROR);
              Exit;
            end;
            gConnection.Host:= ExtractConnectionHost(Text);
            gConnection.Port:= ExtractConnectionPort(Text);
            gConnection.FullSSL:= ExtractConnectionProt(Text) = 'ftps';
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
            gConnection.Path:= UTF8ToSys(Text);
            Data:= SendDlgMsg(pDlg, 'edtInitCommands', DM_GETTEXT, 0, 0);
            Text:= PAnsiChar(Data);
            gConnection.InitCommands:= Text;
            Data:= SendDlgMsg(pDlg, 'chkPassiveMode', DM_GETCHECK, 0, 0);
            gConnection.PassiveMode:= Boolean(Data);
            Data:= SendDlgMsg(pDlg, 'chkAutoTLS', DM_GETCHECK, 0, 0);
            gConnection.AutoTLS:= Boolean(Data);
            Data:= SendDlgMsg(pDlg, 'chkOnlySCP', DM_GETCHECK, 0, 0);
            gConnection.OnlySCP:= Boolean(Data);
            Data:= SendDlgMsg(pDlg, 'chkShowHidden', DM_GETCHECK, 0, 0);
            gConnection.ShowHiddenItems:= Boolean(Data);
            Data:= SendDlgMsg(pDlg, 'chkKeepAliveTransfer', DM_GETCHECK, 0, 0);
            gConnection.KeepAliveTransfer:= Boolean(Data);

            Data:= SendDlgMsg(pDlg, 'fnePublicKey', DM_GETTEXT, 0, 0);
            gConnection.PublicKey:= PAnsiChar(Data);
            Data:= SendDlgMsg(pDlg, 'fnePrivateKey', DM_GETTEXT, 0, 0);
            gConnection.PrivateKey:= PAnsiChar(Data);

            if gConnection.FullSSL and (InitSSLInterface = False) then
            begin;
              ShowWarningSSL;
            end;

            SaveProxyList(pDlg);

            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 1, 0);
          end
        else if DlgItemName = 'btnCancel' then
          begin
            FreeProxyList(pDlg);
            // close dialog
            SendDlgMsg(pDlg, DlgItemName, DM_CLOSE, 2, 0);
          end;
    end;// case
  end; // with
end;

function ShowFtpConfDlg(Connection: TConnection): Boolean;
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
          gConnection := Connection;
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

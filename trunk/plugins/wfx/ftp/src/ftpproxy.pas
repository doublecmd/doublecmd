{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2018 Alexander Koblov (alexx2000@mail.ru)

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit FtpProxy;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, ftpsend;

type
  TProxyType = (
    PROXY_NONE = 0,
    PROXY_SOCKS4 = 1,
    PROXY_SOCKS5 = 2,
    PROXY_HTTP_CONNECT = 3
   );

  { TFtpProxy }

  TFtpProxy = class
  public
    ID: String;
    Host: String;
    Port: String;
    User: String;
    Password: String;
    ProxyType: TProxyType;
    function Clone: TFtpProxy;
  end;

var
  ProxyList: TStringList;

procedure LoadProxyList(IniFile: TIniFile);
procedure SaveProxyList(IniFile: TIniFile);
procedure SetProxy(FtpSend: TFTPSend; ProxyID: String);

implementation

uses
  FtpUtils, blcksock;

procedure LoadProxyList(IniFile: TIniFile);
var
  Proxy: TFtpProxy;
  I, Count: Integer;
  sIndex: AnsiString;
begin
  ProxyList.Clear;
  Count := IniFile.ReadInteger('FTP', 'ProxyCount', 0);
  for I := 1 to Count do
  begin
    sIndex := IntToStr(I);
    Proxy := TFtpProxy.Create;
    Proxy.ID := IniFile.ReadString('FTP', 'Proxy' + sIndex + 'ID', EmptyStr);
    Proxy.Host := IniFile.ReadString('FTP', 'Proxy' + sIndex + 'Host', EmptyStr);
    Proxy.Port := IniFile.ReadString('FTP', 'Proxy' + sIndex + 'Port', EmptyStr);
    Proxy.User := IniFile.ReadString('FTP', 'Proxy' + sIndex + 'User', EmptyStr);
    Proxy.Password := DecodeBase64(IniFile.ReadString('FTP', 'Proxy' + sIndex + 'Password', EmptyStr));
    Proxy.ProxyType := TProxyType(IniFile.ReadInteger('FTP', 'Proxy' + sIndex + 'Type', Integer(PROXY_NONE)));
    // Add proxy to proxy list
    ProxyList.AddObject(Proxy.ID, Proxy);
  end;
end;

procedure SaveProxyList(IniFile: TIniFile);
var
  Proxy: TFtpProxy;
  I, Count: Integer;
  sIndex: AnsiString;
begin
  Count:= ProxyList.Count;
  IniFile.WriteInteger('FTP', 'ProxyCount', Count);
  for I := 0 to Count - 1 do
  begin
    sIndex := IntToStr(I + 1);
    Proxy := TFtpProxy(ProxyList.Objects[I]);
    IniFile.WriteString('FTP', 'Proxy' + sIndex + 'ID', Proxy.ID);
    IniFile.WriteString('FTP', 'Proxy' + sIndex + 'Host', Proxy.Host);
    IniFile.WriteString('FTP', 'Proxy' + sIndex + 'Port', Proxy.Port);
    IniFile.WriteString('FTP', 'Proxy' + sIndex + 'User', Proxy.User);
    IniFile.WriteString('FTP', 'Proxy' + sIndex + 'Password', EncodeBase64(Proxy.Password));
    IniFile.WriteInteger('FTP', 'Proxy' + sIndex + 'Type', Integer(Proxy.ProxyType));
  end;
end;

procedure SetProxy(FtpSend: TFTPSend; ProxyID: String);
var
  Index: Integer;
  Proxy: TFtpProxy;
begin
  Index:= ProxyList.IndexOf(ProxyID);
  if (Index < 0) then
    begin
      FtpSend.Sock.HTTPTunnelIP:= EmptyStr;
      FtpSend.Sock.HTTPTunnelUser:= EmptyStr;
      FtpSend.Sock.HTTPTunnelPass:= EmptyStr;

      FtpSend.DSock.HTTPTunnelIP:= EmptyStr;
      FtpSend.DSock.HTTPTunnelUser:= EmptyStr;
      FtpSend.DSock.HTTPTunnelPass:= EmptyStr;

      FtpSend.Sock.SocksIP:= EmptyStr;
      FtpSend.Sock.SocksUsername:= EmptyStr;
      FtpSend.Sock.SocksPassword:= EmptyStr;

      FtpSend.DSock.SocksIP:= EmptyStr;
      FtpSend.DSock.SocksUsername:= EmptyStr;
      FtpSend.DSock.SocksPassword:= EmptyStr;
    end
  else begin
    Proxy:= TFtpProxy(ProxyList.Objects[Index]);
    case Proxy.ProxyType of
      PROXY_HTTP_CONNECT:
      begin
        FtpSend.Sock.HTTPTunnelIP:= Proxy.Host;
        FtpSend.Sock.HTTPTunnelUser:= Proxy.User;
        FtpSend.Sock.HTTPTunnelPass:= Proxy.Password;

        FtpSend.DSock.HTTPTunnelIP:= Proxy.Host;
        FtpSend.DSock.HTTPTunnelUser:= Proxy.User;
        FtpSend.DSock.HTTPTunnelPass:= Proxy.Password;

        if Length(Proxy.Port) > 0 then
        begin
          FtpSend.Sock.HTTPTunnelPort:= Proxy.Port;
          FtpSend.DSock.HTTPTunnelPort:= Proxy.Port;
        end;
      end;
      PROXY_SOCKS4,
      PROXY_SOCKS5:
      begin
        if Proxy.ProxyType = PROXY_SOCKS4 then
        begin
          FtpSend.Sock.SocksType:= ST_Socks4;
          FtpSend.DSock.SocksType:= ST_Socks4;
        end
        else begin
          FtpSend.Sock.SocksType:= ST_Socks5;
          FtpSend.DSock.SocksType:= ST_Socks5;
        end;

        FtpSend.Sock.SocksIP:= Proxy.Host;
        FtpSend.Sock.SocksResolver:= False;
        FtpSend.Sock.SocksUsername:= Proxy.User;
        FtpSend.Sock.SocksPassword:= Proxy.Password;

        FtpSend.DSock.SocksIP:= Proxy.Host;
        FtpSend.DSock.SocksResolver:= False;
        FtpSend.DSock.SocksUsername:= Proxy.User;
        FtpSend.DSock.SocksPassword:= Proxy.Password;

        if Length(Proxy.Port) > 0 then
        begin
          FtpSend.Sock.SocksPort:= Proxy.Port;
          FtpSend.DSock.SocksPort:= Proxy.Port;
        end;
      end;
    end;
  end;
end;

{ TFtpProxy }

function TFtpProxy.Clone: TFtpProxy;
begin
  Result:= TFtpProxy.Create;
  Result.ID:= ID;
  Result.Host:= Host;
  Result.Port:= Port;
  Result.User:= User;
  Result.Password:= Password;
  Result.ProxyType:= ProxyType;
end;

initialization
  ProxyList:= TStringList.Create;
  ProxyList.OwnsObjects:= True;

finalization
  ProxyList.Free;

end.


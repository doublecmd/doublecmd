{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009-2015 Alexander Koblov (alexx2000@mail.ru)

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

unit FtpAdv;

{$mode delphi}

interface

uses
  Classes, SysUtils, WfxPlugin, FtpSend;

type

  { EUserAbort }

  EUserAbort = class(Exception);

  { TFTPListRecEx }

  TFTPListRecEx = class(TFTPListRec)
  public
    procedure Assign(Value: TFTPListRec); override;
  end;

  { TFTPListEx }

  TFTPListEx = class(TFTPList)
  public
    procedure Assign(Value: TFTPList); override;
  end;

  { TProgressStream }

  TProgressStream = class(TFileStream)
  public
    DoneSize: Int64;
    FileSize: Int64;
    PluginNumber: Integer;
    ProgressProc: TProgressProc;
    RemoteName, LocalName: PAnsiChar;
  private
    procedure DoProgress(Result: Integer);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TConvertEncoding }

  TConvertEncoding = function(const S: String): String;

  { TFTPSendEx }

  TFTPSendEx = class(TFTPSend)
  private
    FUnicode: Boolean;
    FSetTime: Boolean;
  protected
    function Connect: Boolean; override;
    procedure DoStatus(Response: Boolean; const Value: string); override;
  public
    ClientToServer,
    ServerToClient: TConvertEncoding;
  public
    constructor Create; reintroduce;
    function Login: Boolean; override;
    function SetTime(const FileName: String; FileTime: TDateTime): Boolean;
    function StoreFile(const FileName: string; Restore: Boolean): Boolean; override;
    function RetrieveFile(const FileName: string; FileSize: Int64; Restore: Boolean): Boolean; overload;
    function NetworkError(): Boolean;
  end;

implementation

uses
  LazUTF8, FtpFunc;

function Dummy(const S: String): String;
begin
  Result:= S;
end;

{ TFTPListRecEx }

procedure TFTPListRecEx.Assign(Value: TFTPListRec);
begin
  inherited Assign(Value);
  Permission:= Value.Permission;
end;

{ TFTPListEx }

procedure TFTPListEx.Assign(Value: TFTPList);
var
  flr: TFTPListRecEx;
  n: integer;
begin
  Clear;
  for n := 0 to Value.Count - 1 do
  begin
    flr := TFTPListRecEx.Create;
    flr.Assign(Value[n]);
    Flist.Add(flr);
  end;
  Lines.Assign(Value.Lines);
  Masks.Assign(Value.Masks);
  UnparsedLines.Assign(Value.UnparsedLines);
end;

{ TProgressStream }

procedure TProgressStream.DoProgress(Result: Integer);
var
  Percent: Int64;
begin
  DoneSize += Result;
  Percent:= DoneSize * 100 div FileSize;
  if ProgressProc(PluginNumber, LocalName, RemoteName, Percent) = 1 then
    raise EUserAbort.Create(EmptyStr);
end;

function TProgressStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= inherited Read(Buffer, Count);
  if FileSize > 0 then DoProgress(Result);
end;

function TProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= inherited Write(Buffer, Count);
  if FileSize > 0 then DoProgress(Result);
end;

{ TFTPSendEx }

function TFTPSendEx.Connect: Boolean;
begin
  Result:= inherited Connect;
  if Result then LogProc(PluginNumber, MSGTYPE_CONNECT, nil);
end;

procedure TFTPSendEx.DoStatus(Response: Boolean; const Value: string);
begin
  LogProc(PluginNumber, msgtype_details, PAnsiChar(ServerToClient(Value)));
  if FSock.LastError <> 0 then begin
    LogProc(PluginNumber, msgtype_details, PAnsiChar('Network error: ' + FSock.LastErrorDesc));
  end;
end;

constructor TFTPSendEx.Create;
begin
  inherited Create;
  FDirectFile:= True;
  ClientToServer:= @Dummy;
  ServerToClient:= @Dummy;
end;

function TFTPSendEx.Login: Boolean;
var
  Index: Integer;
begin
  Result:= inherited Login;
  if Result then
  begin
    if (FTPCommand('FEAT') div 100) = 2 then
    begin
      for Index:= 0 to FFullResult.Count - 1 do
      begin
        if not FUnicode then FUnicode:= Pos('UTF8', FFullResult[Index]) > 0;
        if not FSetTime then FSetTime:= Pos('MFMT', FFullResult[Index]) > 0;
      end;
      if FUnicode then
      begin
        FTPCommand('OPTS UTF8 ON');
        ClientToServer:= @SysToUTF8;
        ServerToClient:= @UTF8ToSys;
      end;
    end;
  end;
end;

function TFTPSendEx.SetTime(const FileName: String; FileTime: TDateTime): Boolean;
var
  Time: String;
begin
  if not FSetTime then Exit(False);
  Time:= FormatDateTime('yyyymmddhhnnss', FileTime);
  Result:= FTPCommand('MFMT ' + Time + ' ' + FileName) = 213;
end;

function TFTPSendEx.StoreFile(const FileName: string; Restore: Boolean): Boolean;
var
  StorSize: Int64;
  RestoreAt: Int64 = 0;
  SendStream: TProgressStream;
begin
  Result := False;
  Restore := Restore and FCanResume;
  if Restore then
  begin
    RestoreAt := Self.FileSize(FileName);
    if RestoreAt < 0 then RestoreAt := 0;
  end;

  SendStream := TProgressStream.Create(FDirectFileName, fmOpenRead or fmShareDenyWrite);

  SendStream.PluginNumber:= PluginNumber;
  SendStream.ProgressProc:= ProgressProc;
  SendStream.LocalName:= PAnsiChar(FDirectFileName);
  SendStream.RemoteName:= PAnsiChar(ServerToClient(FileName));

  try
    if not DataSocket then Exit;
    FTPCommand('TYPE I');
    StorSize := SendStream.Size;
    if not FCanResume then RestoreAt := 0;
    if RestoreAt > StorSize then RestoreAt := 0;
    if (StorSize > 0) and (RestoreAt = StorSize) then
    begin
      Result := True;
      Exit;
    end;
    SendStream.FileSize := StorSize;
    SendStream.DoneSize := RestoreAt;
    FTPCommand('ALLO ' + IntToStr(StorSize - RestoreAt));
    if FCanResume then
    begin
      if (FTPCommand('REST ' + IntToStr(RestoreAt)) div 100) <> 3 then
        Exit;
    end;
    SendStream.Position := RestoreAt;
    if (FTPCommand('STOR ' + FileName) div 100) <> 1 then
      Exit;
    Result := DataWrite(SendStream);
  finally
    SendStream.Free;
  end;
end;

function TFTPSendEx.RetrieveFile(const FileName: string; FileSize: Int64; Restore: Boolean): Boolean;
var
  RetrStream: TProgressStream;
begin
  Result := False;
  if not DataSocket then Exit;
  Restore := Restore and FCanResume;

  if Restore and FileExists(FDirectFileName) then
    RetrStream := TProgressStream.Create(FDirectFileName, fmOpenWrite or fmShareExclusive)
  else begin
    RetrStream := TProgressStream.Create(FDirectFileName, fmCreate or fmShareDenyWrite)
  end;

  RetrStream.FileSize := FileSize;
  RetrStream.PluginNumber := PluginNumber;
  RetrStream.ProgressProc := ProgressProc;
  RetrStream.LocalName := PAnsiChar(FDirectFileName);
  RetrStream.RemoteName := PAnsiChar(ServerToClient(FileName));

  try
    FTPCommand('TYPE I');
    if Restore then
    begin
      RetrStream.DoneSize := RetrStream.Size;
      RetrStream.Position := RetrStream.DoneSize;
      if (FTPCommand('REST ' + IntToStr(RetrStream.DoneSize)) div 100) <> 3 then
        Exit;
    end;
    if (FTPCommand('RETR ' + FileName) div 100) <> 1 then
      Exit;
    Result := DataRead(RetrStream);
  finally
    RetrStream.Free;
  end;
end;

function TFTPSendEx.NetworkError: Boolean;
begin
  Result := FSock.CanRead(0);
end;

end.


{
   Double commander
   -------------------------------------------------------------------------
   Remote procedure call implementation (Unix)

   Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uClientServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Ssockets, uService;

type

  { TUnixServer }

  TUnixServer = class(Ssockets.TUnixServer)
  protected
    procedure Bind; override;
  end;

  { TPipeTransport }

  TPipeTransport = class(TBaseTransport)
  private
    FAddress : String;
    FConnection : TSocketStream;
  private
    procedure Connect;
  public
    procedure Disconnect; override;
    procedure WriteHandle(AHandle: THandle); override;
    function ReadHandle(var AHandle: THandle) : Int64; override;
    procedure WriteBuffer(const AData; const ALength : Int64); override;
    function ReadBuffer(var AData; const ALength : Int64) : Int64; override;
  public
    constructor Create(const Address : String);
    constructor Create(ASocket : TSocketStream);
    destructor Destroy; override;
  end;

  { TClientHandlerThread }

  TClientHandlerThread = class(TClientThread)
  public
    constructor Create(ASocket : TSocketStream; AOwner : TBaseService);
  end;
  
  { TServerListnerThread }

  TServerListnerThread = class(TServerThread)
  private
    FSocketObject : TUnixServer;
    procedure DoConnect(Sender: TObject; Data: TSocketStream);
  public
    destructor Destroy; override;
    procedure Execute; override;
  end;    

implementation

uses 
  BaseUnix, Unix, uLocalSockets, uDebug;

{ TUnixServer }

procedure TUnixServer.Bind;
begin
  inherited Bind;
  fpChmod(FileName, &0666);
end;

{ TPipeTransport }

procedure TPipeTransport.Connect;
begin
  if FConnection = nil then
  begin
    FConnection:= TUnixSocket.Create(SocketDirectory + FAddress);
    SetSocketClientProcessId(FConnection.Handle);
  end;
end;

procedure TPipeTransport.Disconnect;
begin
  FreeAndNil(FConnection);
end;

procedure TPipeTransport.WriteHandle(AHandle: THandle);
begin
  SendHandle(FConnection.Handle, AHandle);
end;

function TPipeTransport.ReadHandle(var AHandle: THandle): Int64;
begin
  AHandle:= RecvHandle(FConnection.Handle);
end;

procedure TPipeTransport.WriteBuffer(const AData; const ALength : Int64);
var
  P : PByte;
  C, Len : Integer;
begin
  Connect;
  P := PByte(@AData);
  Len := ALength;
  repeat
    C := FConnection.Write(P^,len);
    if (C < 0) then
       raise EInOutError.Create(SysErrorMessage(FConnection.LastError));
    if (C > 0) then
    begin
      Inc(P, C);
      Dec(Len, C);
    end;
  until (Len = 0);
end;

function TPipeTransport.ReadBuffer(var AData; const ALength : Int64) : Int64;
Var
  P : PByte;
  C : Integer;
  Len : Int64;
begin
  Len := ALength;
  P:= PByte(@AData);
  repeat
    C:= FConnection.Read(P^, Len);
    if (C <= 0) then
      raise EInOutError.Create(SysErrorMessage(FConnection.LastError));
    if (C > 0) then
    begin
      Inc(P, C);
      Dec(Len, C);
    end
  until (Len = 0);
  Result := ALength;
end;

constructor TPipeTransport.Create(const Address: String);
begin
  FAddress:= Address;
end;

constructor TPipeTransport.Create(ASocket: TSocketStream);
begin
  FConnection:= ASocket;
end;

destructor TPipeTransport.Destroy;
begin
  FreeAndNil(FConnection);
  inherited Destroy;
end;

{ TClientHandlerThread }

constructor TClientHandlerThread.Create(ASocket : TSocketStream; AOwner : TBaseService);
begin
  FOwner := AOwner;
  FreeOnTerminate := True;
  FTransport:= TPipeTransport.Create(ASocket);
  inherited Create(False);
end;

{ TServerListnerThread }

procedure TServerListnerThread.DoConnect(Sender: TObject; Data: TSocketStream);
begin
  if (FOwner.VerifyChild and not VerifyChild(Data.Handle)) or
     (FOwner.VerifyParent and not VerifyParent(Data.Handle)) then
  begin
    Data.Free;
    Exit;
  end;

  if not Terminated then
    TClientHandlerThread.Create(Data, FOwner)
  else
    Data.Free;
end;

destructor TServerListnerThread.Destroy;
begin
  DCDebug('TServerListnerThread.Destroy');
  FSocketObject.StopAccepting(True);
  inherited Destroy;
end;

procedure TServerListnerThread.Execute;
begin
  try
    FSocketObject:= TUnixServer.Create(SocketDirectory + FOwner.Name);
    try
      FSocketObject.Bind;
      FReadyEvent.SetEvent;
      FSocketObject.OnConnect:= @DoConnect;
      FSocketObject.StartAccepting;
    finally
      FreeAndNil(FSocketObject);
    end;
  except
     on e : Exception do
     begin
       Terminate;
     end;
  end;
end;

end.

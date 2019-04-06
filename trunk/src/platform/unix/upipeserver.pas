{
   Double Commander
   -------------------------------------------------------------------------
   Unix implementation of one-way IPC between 2 processes

   Copyright (C) 2015-2019 Alexander Koblov (alexx2000@mail.ru)

   Based on simpleipc.inc from Free Component Library.
   Copyright (c) 2005 by Michael Van Canneyt, member of
   the Free Pascal development team

   See the file COPYING.FPC.txt, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit uPipeServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, BaseUnix;

Type

  { TPipeServerComm }

  TPipeServerComm = Class(TIPCServerComm)
  Private
    FFileName: String;
    FStream: TFileStream;
  private
    procedure Handler(Sender: TObject);
  Public
    Constructor Create(AOWner : TSimpleIPCServer); override;
    Procedure StartServer; override;
    Procedure StopServer; override;
    Function  PeekMessage(TimeOut : Integer) : Boolean; override;
    Procedure ReadMessage ; override;
    Function GetInstanceID : String;override;
    Property FileName : String Read FFileName;
    Property Stream : TFileStream Read FStream;
  end;

implementation

uses
  uPollThread;

ResourceString
  SErrFailedToCreatePipe = 'Failed to create named pipe: %s';
  SErrFailedToRemovePipe = 'Failed to remove named pipe: %s';

type
  TUnixIPCServer = class(TSimpleIPCServer);

procedure TPipeServerComm.Handler(Sender: TObject);
begin
  TThread.Synchronize(nil, @TUnixIPCServer(Owner).ReadMessage);
end;

constructor TPipeServerComm.Create(AOWner: TSimpleIPCServer);
begin
  inherited Create(AOWner);
  FFileName:= Owner.ServerID;
  if not Owner.Global then
    FFileName:= FFileName + '-' + IntToStr(fpGetPID);
  if FFileName[1] <> '/' then
    FFileName:= GetTempDir(Owner.Global) + FFileName;
end;

procedure TPipeServerComm.StartServer;

const
  PrivateRights = S_IRUSR or S_IWUSR;
  GlobalRights  = PrivateRights or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Rights : Array [Boolean] of Integer = (PrivateRights,GlobalRights);

begin
  If not FileExists(FFileName) then
    If (fpmkFifo(FFileName,438)<>0) then
      DoError(SErrFailedToCreatePipe,[FFileName]);
  FStream:=TFileStream.Create(FFileName,fmOpenReadWrite+fmShareDenyNone,Rights[Owner.Global]);
  AddPoll(FStream.Handle, POLLIN, @Handler, False);
end;

procedure TPipeServerComm.StopServer;
begin
  RemovePoll(FStream.Handle);
  FreeAndNil(FStream);
  if Not DeleteFile(FFileName) then
    DoError(SErrFailedtoRemovePipe,[FFileName]);
end;

function TPipeServerComm.PeekMessage(TimeOut: Integer): Boolean;

Var
  FDS : TFDSet;

begin
  fpfd_zero(FDS);
  fpfd_set(FStream.Handle,FDS);
  Result:=fpSelect(FStream.Handle+1,@FDS,Nil,Nil,TimeOut)>0;
end;

procedure TPipeServerComm.ReadMessage;
var
{$IF (FPC_FULLVERSION < 030001)}
  M : TStream;
  Count : Integer;
{$ENDIF}
  Hdr : TMsgHeader;
begin
  FStream.ReadBuffer(Hdr,SizeOf(Hdr));
{$IF (FPC_FULLVERSION >= 030001)}
  PushMessage(Hdr,FStream);
{$ELSE}
  SetMsgType(Hdr.MsgType);
  Count:=Hdr.MsgLen;
  M:=MsgData;
  if count > 0 then
    begin
    M.Seek(0,soFrombeginning);
    M.CopyFrom(FStream,Count);
    end
  else
    M.Size := 0;
{$ENDIF}
end;

function TPipeServerComm.GetInstanceID: String;
begin
  Result:=IntToStr(fpGetPID);
end;

initialization
  DefaultIPCServerClass:= TPipeServerComm;

end.


unit uService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type

  TRPC_Commands = (
    RPC_Terminate,          //  = 0;
    RPC_FileOpen,           // = 1;
    RPC_FileCreate,         // = 2;
    RPC_DeleteFile,         // = 3;
    RPC_RenameFile,         // = 4;
    RPC_CreateDirectory,    // = 5;
    RPC_RemoveDirectory,    // = 6;
    RPC_CreateSymbolicLink, // = 7;
    RPC_CreateHardLink,     // = 8;
    RPC_FileExists,         // = 9;
    RPC_FileGetAttr,        // = 10;
    RPC_FileSetAttr,        // = 11;
    RPC_FileSetTime,        // = 12;
    RPC_DirectoryExists,    // = 13;
    RPC_FileSetReadOnly,    // = 14;
    RPC_FileCopyAttr,       // = 15;
    RPC_FindFirst,          // = 16;
    RPC_FindNext,           // = 17;
    RPC_FindClose,          // = 18;
    RPC_FileCopy,           // = 19;
    RPC_Execute,            // master = 1;
    RPC_DeleteToTrashFile
    );


  { TBaseTransport }

  TBaseTransport = class
  public
    procedure Disconnect; virtual; abstract;
    procedure WriteHandle(AHandle: THandle); virtual; abstract;
    function ReadHandle(var AHandle: THandle) : Int64; virtual; abstract;
    procedure WriteBuffer(const AData; const ALength : Int64); virtual; abstract;
    function ReadBuffer(var AData; const ALength : Int64) : Int64; virtual; abstract;
  end;

  { TBaseService }

  TBaseService = class
  protected
    FName: String;
    FEvent: TEvent;
    FProcessId: UInt32;
    FVerifyChild: Boolean;
    FVerifyParent: Boolean;
    FServerThread: TThread;
  protected
    procedure ProcessRequest(ATransport: TBaseTransport; const ACommand: TRPC_Commands; ARequest: TStream); virtual; abstract;
  public
    constructor Create(const AName: String); virtual;
    destructor Destroy; override;
    procedure Start;
  public
    ClientCount: Integer;
    property Name: String read FName;
    property Event: TEvent read FEvent;
    property ProcessId: UInt32 read FProcessId write FProcessId;
    property VerifyChild: Boolean read FVerifyChild write FVerifyChild;
    property VerifyParent: Boolean read FVerifyParent write FVerifyParent;
  end;

  { TClientThread }

  TClientThread = class(TThread)
  protected
    FOwner : TBaseService;
    FTransport: TBaseTransport;
  protected
    function ReadRequest(ARequest : TMemoryStream; out ACommand : TRPC_Commands): Integer;
    procedure SendResponse(AResponse : TMemoryStream);
  public
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TServerThread }

  TServerThread = class(TThread)
  protected
    FReadyEvent: TEvent;
    FOwner : TBaseService;
  public
    constructor Create(AOwner : TBaseService); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  uClientServer, uDebug;

{ TServerThread }

constructor TServerThread.Create(AOwner: TBaseService);
begin
  FOwner := AOwner;
  FReadyEvent:= TSimpleEvent.Create;
  inherited Create(False);
end;

destructor TServerThread.Destroy;
begin
  inherited Destroy;
  FReadyEvent.Free;
end;

{ TBaseService }

constructor TBaseService.Create(const AName: String);
begin
  FName:= AName;
  FEvent:= TEvent.Create(nil, False, False, '');
end;

destructor TBaseService.Destroy;
begin
  if (FServerThread <> nil) then
  begin
    FServerThread.Terminate;
    FServerThread.Free;
  end;
  FEvent.Free;
  inherited Destroy;
end;

procedure TBaseService.Start;
begin
  FServerThread:= TServerListnerThread.Create(Self);
  TServerThread(FServerThread).FReadyEvent.WaitFor(30000);
end;

{ TClientThread }

function TClientThread.ReadRequest(ARequest: TMemoryStream;
  out ACommand: TRPC_Commands): Integer;
var
  R: Int64;
  ALength : Int32 = 0;
begin
  // Read command
  R:= FTransport.ReadBuffer(ACommand, SizeOf(ACommand));
  if (R = 0) then Exit(0);

  // Read arguments size
  R:= FTransport.ReadBuffer(ALength, SizeOf(ALength));
  if (R = 0) then Exit(0);

  // Read arguments
  if (ALength > 0) then
  begin
    ARequest.Size:= ALength;
    Result:= FTransport.ReadBuffer(ARequest.Memory^, ALength);
  end;
end;

procedure TClientThread.SendResponse(AResponse: TMemoryStream);
begin
  FTransport.WriteBuffer(AResponse.Memory^, AResponse.Size);
end;

procedure TClientThread.Execute;
var
  ACommand : TRPC_Commands;// = 0;
  ARequest : TMemoryStream;
begin
  InterLockedIncrement(FOwner.ClientCount);

  while not Terminated do
  begin
    try
      ARequest:= TMemoryStream.Create;
      try
        if ReadRequest(ARequest, ACommand) >= SizeOf(Int32) then
        begin
          FOwner.ProcessRequest(FTransport, ACommand, ARequest);
        end;
      finally
        ARequest.Free;
      end;
    except
      on E: Exception do
      begin
        Terminate;
        DCDebug(E.Message);
      end;
    end;
  end;

  InterLockedDecrement(FOwner.ClientCount);
end;

destructor TClientThread.Destroy;
begin
  FTransport.Free;
  inherited Destroy;
  DCDebug('TClientThread.Destroy');
end;

end.


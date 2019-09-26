unit uService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type

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
    procedure ProcessRequest(ATransport: TBaseTransport; ACommand: Int32; ARequest: TStream); virtual; abstract;
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
    function ReadRequest(ARequest : TMemoryStream; var ACommand : LongInt): Integer;
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
  var ACommand: LongInt): Integer;
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
  ACommand : Int32 = 0;
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


unit un_process;

{$mode delphi}{$H+}

interface

uses
  Process, SysUtils, DCProcessUtf8;
 
type

  TOnReadLn = procedure (str: String) of object;
  TOnOperationProgress = procedure of object;
  
  { TExProcess }

  TExProcess = class
  protected
    FProcess: TProcess;
    FOutputLine: String;
    FStop: Boolean;
    FQueryString: String;
    FOnReadLn,
    FOnQueryString: TOnReadLn;
    FOnProcessExit: TOnOperationProgress;
    FOnOperationProgress: TOnOperationProgress;
    function _GetExitStatus(): Integer;
  public
    constructor Create(CommandLine: String = '');
    procedure Execute;
    procedure Stop;
    procedure SetCmdLine(CommandLine: String);
    destructor Destroy; override;

    property Process: TProcess read FProcess;
    property ExitStatus: Integer read _GetExitStatus;
    property QueryString: String read FQueryString write FQueryString;
    property OnReadLn: TOnReadLn read FOnReadLn write FOnReadLn;
    property OnQueryString: TOnReadLn read FOnQueryString write FOnQueryString;
    property OnProcessExit: TOnOperationProgress read FOnProcessExit write FOnProcessExit;
    property OnOperationProgress: TOnOperationProgress read FOnOperationProgress write FOnOperationProgress;
  end;

implementation

uses
  DCStrUtils;

const
  BufferSize = 3000;

{ TExProcess }

function TExProcess._GetExitStatus(): Integer;
begin
  Result:= FProcess.ExitStatus;
end;

constructor TExProcess.Create(CommandLine: String = '');
begin
  FOutputLine:= EmptyStr;
  FProcess:= TProcessUtf8.Create(nil);
  FProcess.CommandLine:= CommandLine;
  FProcess.Options:= [poUsePipes, poNoConsole, poNewProcessGroup];
end;

procedure TExProcess.Execute;
var
  P: Integer;
  S, OutputBuffer: String;
begin
  S:= EmptyStr;
  FProcess.Execute;
  try
    repeat
      if Assigned(FOnOperationProgress) then
        FOnOperationProgress();
      if FStop then Exit;
      // If no output yet
      if FProcess.Output.NumBytesAvailable = 0 then
      begin
        if not FProcess.Running then
          Break
        else
          begin
            Sleep(1);
            if Assigned(FOnQueryString) and Assigned(FProcess.Stderr) and (FProcess.Stderr.NumBytesAvailable > 0) then
            begin
              SetLength(OutputBuffer, BufferSize);
              // Waits for the process output
              SetLength(OutputBuffer, FProcess.Stderr.Read(OutputBuffer[1], Length(OutputBuffer)));
              if (Pos(FQueryString, OutputBuffer) > 0) then FOnQueryString(OutputBuffer);
              OutputBuffer:= EmptyStr;
            end;
            Continue;
          end
      end;
      SetLength(OutputBuffer, BufferSize);
      // Waits for the process output
      SetLength(OutputBuffer, FProcess.Output.Read(OutputBuffer[1], Length(OutputBuffer)));
      // Cut the incoming stream to lines:
      FOutputLine:= FOutputLine + OutputBuffer; // Add to the accumulator
      P:= 1;
      while GetNextLine(FOutputLine, S, P) do
      begin
        if FStop then Exit;
        // Return the line without the CR/LF characters
        if Assigned(FOnReadLn) then FOnReadLn(S);
        // Update progress
        if Assigned(FOnOperationProgress) then FOnOperationProgress();
      end;
      // Remove the processed lines from accumulator
      Delete(FOutputLine, 1, P - 1);
      // Check query string
      if Length(FOutputLine) > 0 then
      begin
        if Assigned(FOnQueryString) and (Pos(FQueryString, FOutputLine) <> 0) then
        begin
          FOnQueryString(FOutputLine);
          FOutputLine:= EmptyStr;
        end;
      end;
      // No more data, break
      if Length(OutputBuffer) = 0 then Break;
    until False;
    if FStop then Exit;
    if (Length(FOutputLine) <> 0) and Assigned(FOnReadLn) then
      FOnReadLn(FOutputLine);
    OutputBuffer:= EmptyStr;
  finally
    if Assigned(FOnProcessExit) then
      FOnProcessExit();
  end;
end;

procedure TExProcess.Stop;
begin
  FStop:= True;
  FProcess.Terminate(-1);
end;

procedure TExProcess.SetCmdLine(CommandLine: String);
begin
  FProcess.CommandLine:= CommandLine;
end;

destructor TExProcess.Destroy;
begin
  FreeAndNil(FProcess);
end;

end.

unit un_process;

{$mode delphi}{$H+}

interface

uses
  Process, SysUtils, Math;
 
type

  TOnReadLn = procedure (str: String) of object;
  TOnOperationProgress = procedure of object;
  
  { TExProcess }

  TExProcess = class
  protected
    FProcess: TProcess;
    FOutputLine: String;
    FStop: Boolean;
    FOnReadLn: TOnReadLn;
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
    property OnReadLn: TOnReadLn read FOnReadLn write FOnReadLn;
    property OnOperationProgress: TOnOperationProgress read FOnOperationProgress write FOnOperationProgress;
  end;

implementation

uses
  LCLProc;

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
  FProcess:= TProcess.Create(nil);
  FProcess.CommandLine:= CommandLine;
  FProcess.Options:= [poUsePipes, poNoConsole];
end;

procedure TExProcess.Execute;
var
  I, J: Integer;
  OutputBuffer: String;
begin
  try
    FProcess.Execute;
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
              Continue;
            end
        end;
      SetLength(OutputBuffer, BufferSize);
      // Waits for the process output
      SetLength(OutputBuffer, FProcess.output.Read(OutputBuffer[1], Length(OutputBuffer)));
      // Cut the incoming stream to lines:
      FOutputLine:= FOutputLine + OutputBuffer; // Add to the accumulator

      // Detect the line breaks and cut.
      repeat
        if Assigned(FOnOperationProgress) then
          FOnOperationProgress();
        if FStop then Exit;
        I:= Pos(#13, FOutputLine);
        J:= Pos(#10, FOutputLine);
        if I = 0 then I:= J;
        if J = 0 then J:= I;
        if J = 0 then Break; // There are no complete lines yet.
        if Assigned(FOnReadLn) then
          FOnReadLn(Copy(FOutputLine, 1, Min(I, J) - 1)); // Return the line without the CR/LF characters
        // Remove the line from accumulator
        FOutputLine:= Copy(FOutputLine, Max(I, J) + 1, Length(FOutputLine) - Max(I, J));
      until False;
      if (OutputBuffer = EmptyStr) then Break;
    until False;
    if FStop then Exit;
    if (FOutputLine <> EmptyStr) and Assigned(FOnReadLn) then
      FOnReadLn(FOutputLine);
    OutputBuffer:= EmptyStr;
    if Assigned(FOnReadLn) then
      FOnReadLn(OutputBuffer); // Empty line to notify DC about process finish
  finally
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

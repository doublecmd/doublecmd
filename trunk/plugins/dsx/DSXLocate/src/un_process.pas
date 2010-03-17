{$mode delphi}
{$longstrings on}

unit un_process;

interface

uses
  process, Math, SysUtils;

type

  TOnReadLn = procedure(str: String) of object;

  { TExProcess }

  TExProcess = class
  protected
    p: TProcess;
    s: String;
    FStop: Boolean;
    function _GetExitStatus(): Integer;
  public
    OnReadLn: TOnReadLn;
    constructor Create(commandline: String = '');
    procedure Execute;
    procedure Stop;
    procedure SetCmdLine(commandline: String);
    destructor Destroy; override;

    property ExitStatus: Integer read _GetExitStatus;
  end;

implementation

const
  buf_len = 3000;


{ TExProcess }

function TExProcess._GetExitStatus(): Integer;
begin
  Result := p.ExitStatus;
end;

constructor TExProcess.Create(commandline: String = '');
begin
  s := '';
  p := TProcess.Create(nil);
  p.CommandLine := commandline;
  p.Options := [poUsePipes, poNoConsole, poWaitOnExit];
end;

procedure TExProcess.Execute;
var
  buf: String;
  i, j: Integer;
begin
  try
    p.Execute;
    repeat
      if FStop then
        exit;
      SetLength(buf, buf_len);
      SetLength(buf, p.output.Read(buf[1], length(buf))); //waits for the process output
      // cut the incoming stream to lines:
      s := s + buf; //add to the accumulator

      repeat //detect the line breaks and cut.
        i := Pos(#13, s);
        j := Pos(#10, s);
        if i = 0 then
          i := j;
        if j = 0 then
          j := i;
        if j = 0 then
          Break; //there are no complete lines yet.
        if Assigned(OnReadLn) then
          OnReadLn(Copy(s, 1, min(i, j) - 1));
        //return the line without the CR/LF characters
        s := Copy(s, max(i, j) + 1, length(s) - max(i, j));
        //remove the line from accumulator
      until False;
    until buf = '';
    if s <> '' then
      if Assigned(OnReadLn) then
        OnReadLn(s);
    buf := '';
  except
    {$IFDEF UNIX}
    on e: Exception do
      Writeln('DSXLocate error: ', e.Message);
    {$ENDIF}
  end;

  if Assigned(OnReadLn) then
    OnReadLn(buf); //Empty line to notify DC about search process finish
end;

procedure TExProcess.Stop;
begin
  FStop := True;
end;

procedure TExProcess.SetCmdLine(commandline: String);
begin
  p.CommandLine := commandline;
end;

destructor TExProcess.Destroy;
begin
  FreeAndNil(p);
  inherited Destroy;
end;

end.


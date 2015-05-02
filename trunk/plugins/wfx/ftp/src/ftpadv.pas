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

implementation

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
  DoProgress(Result);
end;

function TProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= inherited Write(Buffer, Count);
  DoProgress(Result);
end;

end.


unit AbProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AbArcTyp, DCClassesUtf8;

type

  { TAbProgress }

  TAbProgress = object
    DoneSize: Int64;
    FileSize: Int64;
    OnProgress: TAbProgressEvent;
    procedure DoProgress(Result: Integer);
  end;

  { TAbProgressStream }

  TAbProgressStream = class(TStream)
  private
    FSource: TStream;
    FProgress: TAbProgress;
  public
    constructor Create(ASource : TStream; AEvent: TAbProgressEvent); reintroduce;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  { TAbProgressFileStream }

  TAbProgressFileStream = class(TFileStreamEx)
  private
    FProgress: TAbProgress;
  public
    constructor Create(const AFileName: String; Mode: LongWord; AEvent: TAbProgressEvent); reintroduce;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

implementation

uses
  AbExcept, DCOSUtils;

{ TAbProgress }

procedure TAbProgress.DoProgress(Result: Integer);
var
  Percent: Byte;
  Abort: Boolean = False;
begin
  if (FileSize > 0) then
  begin
    DoneSize += Result;
    Percent:= Byte(DoneSize * 100 div FileSize);

    OnProgress(Percent, Abort);

    if Abort then raise EAbUserAbort.Create;
  end;
end;

{ TAbProgressStream }

constructor TAbProgressStream.Create(ASource: TStream; AEvent: TAbProgressEvent);
begin
  FSource:= ASource;
  FProgress.OnProgress:= AEvent;
  FProgress.FileSize:= FSource.Size;
end;

function TAbProgressStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= FSource.Read(Buffer, Count);
  if Assigned(FProgress.OnProgress) then FProgress.DoProgress(Result);
end;

function TAbProgressStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= FSource.Write(Buffer, Count);
end;

function TAbProgressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result:= FSource.Seek(Offset, Origin);
end;

{ TAbProgressFileStream }

constructor TAbProgressFileStream.Create(const AFileName: String;
  Mode: LongWord; AEvent: TAbProgressEvent);
begin
  FProgress.OnProgress:= AEvent;
  inherited Create(AFileName, Mode);
  FProgress.FileSize:= FileGetSize(Handle);
end;

function TAbProgressFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= inherited Read(Buffer, Count);
  if Assigned(FProgress.OnProgress) then FProgress.DoProgress(Result);
end;

end.


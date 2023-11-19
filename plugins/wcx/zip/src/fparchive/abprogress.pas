unit AbProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AbArcTyp;

type

  { TAbProgress }

  TAbProgress = object
    DoneSize: Int64;
    FileSize: Int64;
    OnProgress: TAbProgressEvent;
    procedure DoProgress(Result: Integer);
  end;

  { TAbProgressReadStream }

  TAbProgressReadStream = class(TStream)
  private
    FSource: TStream;
    FProgress: TAbProgress;
  public
    constructor Create(ASource : TStream; AEvent: TAbProgressEvent); reintroduce;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  { TAbProgressWriteStream }

  TAbProgressWriteStream = class(TStream)
  private
    FTarget: TStream;
    FProgress: TAbProgress;
  public
    constructor Create(ATarget : TStream; ASize: Int64; AEvent: TAbProgressEvent); reintroduce;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

uses
  AbExcept;

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

{ TAbProgressReadStream }

constructor TAbProgressReadStream.Create(ASource: TStream; AEvent: TAbProgressEvent);
begin
  FSource:= ASource;
  FProgress.OnProgress:= AEvent;
  FProgress.FileSize:= FSource.Size;
end;

function TAbProgressReadStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= FSource.Read(Buffer, Count);
  if Assigned(FProgress.OnProgress) then FProgress.DoProgress(Result);
end;

function TAbProgressReadStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result:= FSource.Seek(Offset, Origin);
end;

{ TAbProgressWriteStream }

constructor TAbProgressWriteStream.Create(ATarget: TStream; ASize: Int64;
  AEvent: TAbProgressEvent);
begin
  FTarget:= ATarget;
  FProgress.FileSize:= ASize;
  FProgress.OnProgress:= AEvent;
end;

function TAbProgressWriteStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= FTarget.Write(Buffer, Count);
  if Assigned(FProgress.OnProgress) then FProgress.DoProgress(Result);
end;

function TAbProgressWriteStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result:= FTarget.Seek(Offset, Origin);
end;

end.


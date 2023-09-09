unit ZipCache;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fpTimer;

type

  { TPasswordCache }

  TPasswordCache = class
  private
    FTimer: TFPTimer;
    FArchiveSize: Int64;
    FArchiveName: String;
    FArchiveTime: Integer;
    FMutex: TCriticalSection;
    FArchivePassword: String;
    const FInterval: Cardinal = 120000;
  private
    procedure ResetTimer;
    procedure ZeroPassword;
    procedure TimerEvent(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function GetPassword(const Archive: String): String;
    procedure SetPassword(const Archive: String; const Password: String);
  end;

implementation

uses
  LazFileUtils;

{ TPasswordCache }

procedure TPasswordCache.ResetTimer;
begin
  if FTimer.Interval > FInterval then
    FTimer.Interval:= FTimer.Interval - 1
  else
    FTimer.Interval:= FTimer.Interval + 1;
end;

procedure TPasswordCache.ZeroPassword;
begin
  if (Length(FArchivePassword) > 0) then
  begin
    FillChar(FArchivePassword[1], Length(FArchivePassword), #0);
    SetLength(FArchivePassword, 0);
  end;
end;

procedure TPasswordCache.TimerEvent(Sender: TObject);
begin
  FMutex.Acquire;
  try
    ZeroPassword;
    FTimer.Enabled:= False;
  finally
    FMutex.Release;
  end;
end;

function TPasswordCache.GetPassword(const Archive: String): String;
begin
  FMutex.Acquire;
  try
    if (SameText(FArchiveName, Archive)) and
       (FArchiveSize = FileSizeUtf8(Archive)) and
       (FArchiveTime = FileAgeUtf8(Archive)) then
    begin
      ResetTimer;
      Result:= FArchivePassword
    end
    else begin
      FTimer.Enabled:= False;
      Result:= EmptyStr;
      ZeroPassword;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TPasswordCache.SetPassword(const Archive: String; const Password: String);
begin
  FMutex.Acquire;
  try
    if (Length(Password) = 0) then
      FArchiveName:= EmptyStr
    else begin
      FArchiveName:= Archive;
      FArchivePassword:= Password;
      FArchiveTime:= FileAgeUtf8(Archive);
      FArchiveSize:= FileSizeUtf8(Archive);
      FTimer.Enabled:= True;
      ResetTimer;
    end;
  finally
    FMutex.Release;
  end;
end;

constructor TPasswordCache.Create;
begin
  FTimer:= TFPTimer.Create(nil);
  FTimer.UseTimerThread:= True;
  FTimer.OnTimer:= @TimerEvent;
  FTimer.Interval:= FInterval;
  FMutex:= TCriticalSection.Create;
end;

destructor TPasswordCache.Destroy;
begin
  FTimer.Free;
  FMutex.Free;
  inherited Destroy;
end;

end.


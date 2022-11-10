{
   Double commander
   -------------------------------------------------------------------------
   This module contains classes with UTF8 file names support.

   Copyright (C) 2008-2022 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit DCClassesUtf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, RtlConsts, SysUtils, IniFiles;

type
  { TFileStreamEx class }

  TFileStreamEx = class(THandleStream)
  private
    FDirty: Int64;
    FAutoSync: Boolean;
    FDirtyLimit: Int64;
    procedure SetAutoSync(AValue: Boolean);
  protected
    FFileName: String;
    procedure Sync(AWritten: Int64);
    procedure SetSize64(const NewSize: Int64); override;
  public
    constructor Create(const AFileName: String; Mode: LongWord); virtual; overload;
    destructor Destroy; override;
    function Flush: Boolean;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    property DirtyLimit: Int64 read FDirtyLimit write FDirtyLimit;
    property AutoSync: Boolean read FAutoSync write SetAutoSync;
    property FileName: String read FFileName;
  end; 

  { TStringListEx }

  TStringListEx = class(TStringList)
  public
    function IndexOfValue(const Value: String): Integer;
    procedure LoadFromFile(const FileName: String); override;
    procedure SaveToFile(const FileName: String); override;
  end;   

  { TIniFileEx }

  TIniFileEx = class(TMemIniFile)
  private
    FReadOnly: Boolean;
  public
    constructor Create(const AFileName: String; Mode: Word); virtual;
    constructor Create(const AFileName: String; AEscapeLineFeeds : Boolean = False); override;
    procedure UpdateFile; override;
  public
    property ReadOnly: Boolean read FReadOnly;
  end;

implementation

uses
  DCOSUtils;

{ TFileStreamEx }

procedure TFileStreamEx.SetAutoSync(AValue: Boolean);
const
  DIRTY_LIMIT = 1024 * 1024;
begin
  FAutoSync:= AValue;
  if AValue and (FDirtyLimit = 0) then
  begin
    FDirtyLimit:= DIRTY_LIMIT;
  end;
end;

procedure TFileStreamEx.Sync(AWritten: Int64);
const
  TARGET_LATENCY_LOW  = 900;
  TARGET_LATENCY_HIGH = 1100;
  DIRTY_LIMIT_LOW  = 512 * 1024;
  DIRTY_LIMIT_HIGH = MaxLongInt + 1;
var
  T1, T2: QWord;
  Elapsed: Double;
  Slowdown: Double;
begin
  FDirty+= AWritten;

  if FDirty < FDirtyLimit then
    Exit;

  FDirty:= 0;
  T1:= GetTickCount64;

  if not FileFlushData(Handle) then
    Exit;

  T2:= GetTickCount64;

  Elapsed:= (T2 - T1);

  if (Elapsed > TARGET_LATENCY_HIGH) then
  begin
    if (FDirtyLimit > DIRTY_LIMIT_LOW) then
    begin
      Slowdown:= Elapsed / TARGET_LATENCY_HIGH;

      if (Slowdown > 2) then
          FDirtyLimit := Round(FDirtyLimit / Slowdown)
       else begin
          FDirtyLimit := Round(FDirtyLimit * 0.7);
      end;

      if (FDirtyLimit < DIRTY_LIMIT_LOW) then
        FDirtyLimit := DIRTY_LIMIT_LOW
      else begin
        FDirtyLimit := (FDirtyLimit div 4096 * 4096);
      end;
    end;
  end
  else if (Elapsed < TARGET_LATENCY_LOW) then
  begin
    if FDirtyLimit < DIRTY_LIMIT_HIGH then
    begin
      FDirtyLimit := Round(FDirtyLimit * 1.3);

      if (FDirtyLimit > DIRTY_LIMIT_HIGH) then
        FDirtyLimit := DIRTY_LIMIT_HIGH
      else begin
        FDirtyLimit := (FDirtyLimit div 4096 * 4096);
      end;
    end;
  end;
end;

procedure TFileStreamEx.SetSize64(const NewSize: Int64);
begin
  FileAllocate(Handle, NewSize);
end;

constructor TFileStreamEx.Create(const AFileName: String; Mode: LongWord);
var
  AHandle: System.THandle;
begin
  if (Mode and fmCreate) <> 0 then
    begin
      AHandle:= mbFileCreate(AFileName, Mode);
      if AHandle = feInvalidHandle then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(AHandle);
    end
  else
    begin 
      AHandle:= mbFileOpen(AFileName, Mode);
      if AHandle = feInvalidHandle then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(AHandle);
    end;
  FFileName:= AFileName;
end;

destructor TFileStreamEx.Destroy;
begin
  inherited Destroy;
  // Close handle after destroying the base object, because it may use Handle in Destroy.
  if Handle <> feInvalidHandle then FileClose(Handle);
end;

function TFileStreamEx.Flush: Boolean;
begin
  Result:= FileFlush(Handle);
end;

function TFileStreamEx.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result:= FileRead(Handle, Buffer, Count);
  if Result = -1 then
    raise EReadError.Create(mbSysErrorMessage(GetLastOSError));
end;

function TFileStreamEx.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result:= inherited Write(Buffer, Count);
  if FAutoSync and (Result > 0) then Sync(Result);
end;

{ TStringListEx }

function TStringListEx.IndexOfValue(const Value: String): Integer;
var
  iStart: LongInt;
  sTemp: String;
begin
  CheckSpecialChars;
  Result:= 0;
  while (Result < Count) do
    begin
    sTemp:= Strings[Result];
    iStart:= Pos(NameValueSeparator, sTemp) + 1;
    if (iStart > 0) and (DoCompareText(Value, Copy(sTemp, iStart, MaxInt)) = 0) then
      Exit;
    Inc(result);
    end;
  Result:= -1;
end;

procedure TStringListEx.LoadFromFile(const FileName: String);
var
  fsFileStream: TFileStreamEx;
begin
  fsFileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(fsFileStream);
  finally
    fsFileStream.Free;
  end;
end;

procedure TStringListEx.SaveToFile(const FileName: String);
var
  AMode: LongWord;
  fsFileStream: TFileStreamEx;
begin
  if not mbFileExists(FileName) then
    AMode:= fmCreate
  else begin
    AMode:= fmOpenWrite or fmShareDenyWrite;
  end;
  fsFileStream:= TFileStreamEx.Create(FileName, AMode);
  try
    SaveToStream(fsFileStream);
    if (AMode <> fmCreate) then fsFileStream.Size:= fsFileStream.Position;
  finally
    fsFileStream.Free;
  end;
end;

{ TIniFileEx }

constructor TIniFileEx.Create(const AFileName: String; Mode: Word);
var
  slLines : TStringListEx;
begin
  FReadOnly := ((Mode and $03) = fmOpenRead);

  inherited Create(EmptyStr);

  if ((Mode and $03) <> fmOpenWrite) then
  begin
    if mbFileExists(AFileName) then
    begin
      slLines := TStringListEx.Create;
      try
        slLines.LoadFromFile(AFileName);
        SetStrings(slLines);
      finally
        slLines.Free;
      end;
    end;
  end;
  Rename(AFileName, False);
end;

constructor TIniFileEx.Create(const AFileName: String; AEscapeLineFeeds: Boolean);
var
  Mode: Word;
begin
  if not mbFileExists(AFileName) then
    Mode := fmOpenWrite or fmShareDenyWrite
  else if mbFileAccess(AFileName, fmOpenReadWrite or fmShareDenyWrite) then
    Mode := fmOpenReadWrite or fmShareDenyWrite
  else begin
    Mode := fmOpenRead or fmShareDenyNone;
  end;
  Create(AFileName, Mode);
end;

procedure TIniFileEx.UpdateFile;
var
  slLines: TStringListEx;
begin
  if not FReadOnly then
  begin
    slLines := TStringListEx.Create;
    try
      GetStrings(slLines);
      slLines.SaveToFile(FileName);
      PBoolean(@Dirty)^:= False;
    finally
      slLines.Free;
    end;
  end;
end;

end.

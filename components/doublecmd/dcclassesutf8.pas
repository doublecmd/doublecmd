{
   Double commander
   -------------------------------------------------------------------------
   This module contains classes with UTF8 file names support.

   Copyright (C) 2008-2019 Alexander Koblov (alexx2000@mail.ru)

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
  protected
    FFileName: String;
    procedure SetSize64(const NewSize: Int64); override;
  public
    constructor Create(const AFileName: String; Mode: LongWord); virtual; overload;
    destructor Destroy; override;
    function Flush: Boolean;
    function Read(var Buffer; Count: LongInt): LongInt; override;
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
  fsFileStream: TFileStreamEx = nil;
begin
  try
    if mbFileExists(FileName) then
      begin
        fsFileStream:= TFileStreamEx.Create(FileName, fmOpenWrite or fmShareDenyWrite);
        fsFileStream.Position:= 0;
        fsFileStream.Size:= 0;
      end
    else
      fsFileStream:= TFileStreamEx.Create(FileName, fmCreate);

    SaveToStream(fsFileStream);
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

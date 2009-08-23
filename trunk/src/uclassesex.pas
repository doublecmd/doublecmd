{
   Double commander
   -------------------------------------------------------------------------
   This module contains classes with UTF8 file names support.

   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit uClassesEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, RtlConsts, SysUtils, IniFiles, IniPropStorage;

type
  { TFileStreamEx class }

  TFileStreamEx = class(THandleStream)
  private
    FHandle: THandle;
    FFileName: UTF8String;
  public
    constructor Create(const AFileName: UTF8String; Mode: Word);
    destructor Destroy; override;
    property FileName : UTF8String read FFileName;
  end; 

  { TStringListEx }

  TStringListEx = class(TStringList)
  public
    function IndexOfValue(const Value: String): Integer;
    procedure LoadFromFile(const FileName: String); override;
    procedure SaveToFile(const FileName: String); override;
  end;   
  
  { TIniFileEx }

  TIniFileEx = class(TIniFile)
  private
    FIniFileStream: TFileStreamEx;
  public
    constructor Create(const AFileName: String; Mode: Word);
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    destructor Destroy; override;
    procedure UpdateFile; override;
  end;

  { TIniPropStorageEx }

  TIniPropStorageEx = class(TCustomIniPropStorage)
  private
    FPercentSize: Integer;
  protected
    function IniFileClass: TIniFileClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Restore; override;
    property PercentSize: Integer read FPercentSize write FPercentSize;
  end;

implementation

uses Forms, uOSUtils;

{ TFileStreamEx}

constructor TFileStreamEx.Create(const AFileName: UTF8String; Mode: Word);
begin
  if Mode = fmCreate then
    begin
      FHandle:= mbFileCreate(AFileName);
      if FHandle = feInvalidHandle then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(FHandle);	  
    end
  else
    begin 
      FHandle:= mbFileOpen(AFileName, Mode);
      if FHandle = feInvalidHandle then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(FHandle);	  
    end;
  FFileName:= AFileName;
end;

destructor TFileStreamEx.Destroy;
begin
  inherited Destroy;
  // Close handle after destroying the base object, because it may use Handle in Destroy.
  if FHandle >= 0 then FileClose(FHandle);
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
  LoadFromStream(fsFileStream);
  fsFileStream.Free;
end;

procedure TStringListEx.SaveToFile(const FileName: String);
var
  fsFileStream: TFileStreamEx;
begin
  if mbFileExists(FileName) then
    begin
      fsFileStream:= TFileStreamEx.Create(FileName, fmOpenWrite or fmShareDenyNone);
      fsFileStream.Position:= 0;
      fsFileStream.Size:= 0;
    end
  else
    fsFileStream:= TFileStreamEx.Create(FileName, fmCreate);

  SaveToStream(fsFileStream);
  fsFileStream.Free;
end;

{ TIniFileEx }

constructor TIniFileEx.Create(const AFileName: String; Mode: Word);
begin
  if mbFileExists(AFileName) then
    FIniFileStream:= TFileStreamEx.Create(AFileName, Mode or fmShareDenyNone)
  else
    FIniFileStream:= TFileStreamEx.Create(AFileName, fmCreate);
  inherited Create(FIniFileStream);
end;

constructor TIniFileEx.Create(const AFileName: string; AEscapeLineFeeds: Boolean);
begin
  Create(AFileName, fmOpenReadWrite);
end;

procedure TIniFileEx.UpdateFile;
begin
  Stream.Position:=0;
  Stream.Size:= 0;
  inherited UpdateFile;
end; 

destructor TIniFileEx.Destroy;
begin
  inherited Destroy;
  // Destroy stream after destroying the base object, because it may use the stream in Destroy.
  FreeAndNil(FIniFileStream);
end;

{ TIniPropStorageEx }

function TIniPropStorageEx.IniFileClass: TIniFileClass;
begin
  Result:= TIniFileEx;
end;

constructor TIniPropStorageEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPercentSize:= 5;
end;

procedure TIniPropStorageEx.Restore;
var
  mLeft, mTop, // monitor left and top
  mWidth, mHeight, // monitor width and height
  pWidth, pHeight: Integer;
begin
  inherited Restore;
  if Self.Owner is TCustomForm then
    with Self.Owner as TCustomForm do
    begin
      mLeft:= Monitor.Left;
      mTop:= Monitor.Top;
      mWidth:= Monitor.Width;
      mHeight:= Monitor.Height;

      pWidth:= (mWidth * FPercentSize) div 100;
      pHeight:= (mHeight * FPercentSize) div 100;

      if (mWidth < Width) or (mHeight < Height) then
        begin
          Width:= mWidth - pWidth;
          Height:= mHeight - (pHeight * 2);
        end;

      if (Top > (mTop + mHeight - pHeight)) or (Top < mTop) then
        Top:= mTop + pHeight;
      if (Left > (mLeft + mWidth - pWidth)) or ((Left + Width - pWidth) < mLeft) then
        Left:= mLeft + pWidth;
    end;
end;

end.

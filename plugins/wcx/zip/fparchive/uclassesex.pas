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
  Classes, SysUtils;

type
  { TFileStreamEx class }

  TFileStreamEx = class(THandleStream)
  private
    FFileName: UTF8String;
  public
    constructor Create(const AFileName: UTF8String; Mode: Word);
    destructor Destroy; override;
    property FileName : UTF8String read FFileName;
  end; 

implementation

uses
  RtlConsts
  {$IF DEFINED(MSWINDOWS)}
  , Windows
  {$ELSEIF DEFINED(UNIX)}
  , BaseUnix
  {$ENDIF}
  ;

{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of DWORD = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);
{$ENDIF}

function mbFileOpen(const FileName: UTF8String; Mode: Integer): THandle;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= CreateFileW(PWChar(wFileName), AccessMode[Mode and 3],
                       ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
const
  AccessMode: array[0..2] of LongInt  = (
                O_RdOnly,
                O_WrOnly,
                O_RdWr);
begin
  Result:= fpOpen(FileName, AccessMode[Mode and 3]);
end;
{$ENDIF}

function mbFileCreate(const FileName: UTF8String): THandle;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= CreateFileW(PWChar(wFileName), GENERIC_READ or GENERIC_WRITE,
                       FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
begin
  Result:= fpOpen(FileName, O_Creat or O_RdWr or O_Trunc);
end;
{$ENDIF}

{ TFileStreamEx }

constructor TFileStreamEx.Create(const AFileName: UTF8String; Mode: Word);
var
  H: System.THandle;
begin
  if Mode = fmCreate then
    begin
      H:= mbFileCreate(AFileName);
      if H = feInvalidHandle then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(H);
    end
  else
    begin 
      H:= mbFileOpen(AFileName, Mode);
      if H = feInvalidHandle then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(H);
    end;
  FFileName:= AFileName;
end;

destructor TFileStreamEx.Destroy;
begin
  inherited Destroy;
  // Close handle after destroying the base object, because it may use Handle in Destroy.
  if Handle >= 0 then
    FileClose(Handle);
end;

end.

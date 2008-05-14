{
   Double commander
   -------------------------------------------------------------------------
   TFileStreamEx class with UTF8 file names support.

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

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

unit uFileStreamEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, RtlConsts, SysUtils;

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

implementation

uses uOSUtils;

{ TFileStreamEx}

constructor TFileStreamEx.Create(const AFileName: UTF8String; Mode: Word);
begin
  if Mode = fmCreate then
    begin
      FHandle:= mbFileCreate(AFileName);
      if FHandle < 0 then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(FHandle);	  
    end
  else
    begin 
      FHandle:= mbFileOpen(AFileName, Mode);
      if FHandle < 0 then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(FHandle);	  
    end;
  FFileName:= AFileName;
end;

destructor TFileStreamEx.Destroy;
begin
  if FHandle >= 0 then FileClose(FHandle);
  inherited Destroy;
end;

end.


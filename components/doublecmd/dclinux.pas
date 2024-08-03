{
  Double Commander
  -------------------------------------------------------------------------
  This unit contains Linux specific functions

  Copyright (C) 2023 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this program. If not, see <https://www.gnu.org/licenses/>
}

unit DCLinux;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, BaseUnix, Unix;

const
  FS_IOC_GETFLAGS = $80086601;
  FS_IOC_SETFLAGS = $40086602;
  (*
   * Inode flags (FS_IOC_GETFLAGS / FS_IOC_SETFLAGS)
  *)
  FS_SECRM_FL     = $00000001; //* Secure deletion */
  FS_UNRM_FL      = $00000002; //* Undelete */
  FS_COMPR_FL     = $00000004; //* Compress file */
  FS_SYNC_FL      = $00000008; //* Synchronous updates */
  FS_IMMUTABLE_FL = $00000010; //* Immutable file */
  FS_APPEND_FL    = $00000020; //* Writes to file may only append */
  FS_NODUMP_FL    = $00000040; //* Do not dump file */
  FS_NOATIME_FL   = $00000080; //* Do not update atime */

  FS_FL_USER_VISIBLE    = $0003DFFF; //* User visible flags */
  FS_FL_USER_MODIFIABLE = $000380FF; //* User modifiable flags */

type
  TFlagName = record
    Flag: UInt32;
    Name: AnsiChar;
  end;

const
  FlagsName: array[1..8] of TFlagName = (
    (Flag: FS_SECRM_FL;        Name: 's'),
    (Flag: FS_UNRM_FL;         Name: 'u'),
    (Flag: FS_SYNC_FL;         Name: 'S'),
    (Flag: FS_IMMUTABLE_FL;    Name: 'i'),
    (Flag: FS_APPEND_FL;       Name: 'a'),
    (Flag: FS_NODUMP_FL;       Name: 'd'),
    (Flag: FS_NOATIME_FL;      Name: 'A'),
    (Flag: FS_COMPR_FL;        Name: 'c')
  );

function FormatFileFlags(Flags: UInt32): String;
function FileGetFlags(Handle: THandle; out Flags: UInt32): Boolean;
function mbFileGetFlags(const FileName: String; out Flags: UInt32): Boolean;

function mbFileGetXattr(const FileName: String): TStringArray;
function mbFileCopyXattr(const Source, Target: String): Boolean;

implementation

uses
  InitC, DCUnix, DCConvertEncoding, DCOSUtils;

function lremovexattr(const path, name: PAnsiChar): cint; cdecl; external clib;
function llistxattr(const path: PAnsiChar; list: PAnsiChar; size: csize_t): ssize_t; cdecl; external clib;
function lgetxattr(const path, name: PAnsiChar; value: Pointer; size: csize_t): ssize_t; cdecl; external clib;
function lsetxattr(const path, name: PAnsiChar; const value: Pointer; size: csize_t; flags: cint): cint; cdecl; external clib;

function FormatFileFlags(Flags: UInt32): String;
var
  Index: Integer;
begin
  Result:=StringOfChar('-', Length(FlagsName));
  for Index:= 1 to High(FlagsName) do
  begin
    if Flags and FlagsName[Index].Flag <> 0 then
    begin
      Result[Index]:= FlagsName[Index].Name;
    end;
  end;
end;

function FileGetFlags(Handle: THandle; out Flags: UInt32): Boolean;
begin
  Result:= (FpIOCtl(Handle, FS_IOC_GETFLAGS, @Flags) >= 0);
end;

function mbFileGetFlags(const FileName: String; out Flags: UInt32): Boolean;
var
  Handle: THandle;
begin
  Handle:= mbFileOpen(FileName, fmOpenRead or fmShareDenyNone);
  Result:= Handle <> feInvalidHandle;
  if Result then
  begin
    Result:= (FpIOCtl(Handle, FS_IOC_GETFLAGS, @Flags) >= 0);
    FileClose(Handle);
  end;
end;

function mbFileGetXattr(const FileName: String): TStringArray;
var
  AList: String;
  ALength: ssize_t;
  AFileName: String;
begin
  SetLength(AList, MaxSmallint);
  Result:= Default(TStringArray);
  AFileName:= CeUtf8ToSys(FileName);
  ALength:= llistxattr(PAnsiChar(AFileName), Pointer(AList), Length(AList));
  if (ALength < 0) then
  begin
    if (fpgetCerrno <> ESysERANGE) then
    begin
      fpseterrno(fpgetCerrno);
      Exit;
    end
    else begin
      ALength:= llistxattr(PAnsiChar(AFileName), nil, 0);
      if ALength < 0 then
      begin
        fpseterrno(fpgetCerrno);
        Exit;
      end;
      SetLength(AList, ALength);
      ALength:= llistxattr(PAnsiChar(AFileName), Pointer(AList), ALength);
      if ALength < 0 then
      begin
        fpseterrno(fpgetCerrno);
        Exit;
      end;
    end;
  end;
  if (ALength > 0) then
  begin
    SetLength(AList, ALength - 1);
    Result:= AList.Split(#0);
  end;
end;

function mbFileCopyXattr(const Source, Target: String): Boolean;
var
  Value: String;
  Index: Integer;
  ALength: ssize_t;
  Names: TStringArray;
  ASource, ATarget: String;
begin
  Result:= True;
  ASource:= CeUtf8ToSys(Source);
  ATarget:= CeUtf8ToSys(Target);
  // Remove attributes from target
  Names:= mbFileGetXattr(Target);
  for Index:= 0 to High(Names) do
  begin
    lremovexattr(PAnsiChar(ATarget), PAnsiChar(Names[Index]));
  end;
  SetLength(Value, MaxSmallint);
  Names:= mbFileGetXattr(Source);
  for Index:= 0 to High(Names) do
  begin
    ALength:= lgetxattr(PAnsiChar(ASource), PAnsiChar(Names[Index]), Pointer(Value), Length(Value));
    if (ALength < 0) then
    begin
      if (fpgetCerrno <> ESysERANGE) then
      begin
        fpseterrno(fpgetCerrno);
        Exit(False);
      end
      else begin
        ALength:= lgetxattr(PAnsiChar(ASource), PAnsiChar(Names[Index]), nil, 0);
        if ALength < 0 then
        begin
          fpseterrno(fpgetCerrno);
          Exit(False);
        end;
        SetLength(Value, ALength);
        ALength:= lgetxattr(PAnsiChar(ASource), PAnsiChar(Names[Index]), Pointer(Value), Length(Value));
        if ALength < 0 then
        begin
          fpseterrno(fpgetCerrno);
          Exit(False);
        end;
      end;
    end;
    if (lsetxattr(PAnsiChar(ATarget), PAnsiChar(Names[Index]), Pointer(Value), ALength, 0) < 0) then
    begin
      fpseterrno(fpgetCerrno);
      Exit(fpgeterrno = ESysEOPNOTSUPP);
    end;
  end;
end;

end.

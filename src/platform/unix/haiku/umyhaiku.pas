{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific HAIKU functions.

   Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit uMyHaiku;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, BaseUnix, CTypes, DCBasicTypes
{$IF DEFINED(LCLQT5)}
  , Qt5
{$ELSEIF DEFINED(LCLQT6)}
  , Qt6
{$ENDIF}
  ;

const
  B_OS_NAME_LENGTH   = 32;
  B_FILE_NAME_LENGTH = 256;

  B_FS_IS_READONLY   = $00000001;
  B_FS_IS_REMOVABLE  = $00000002;
  B_FS_IS_PERSISTENT = $00000004;
  B_FS_IS_SHARED     = $00000008;

type
  Tfs_info = record
    dev: dev_t;
    root: ino_t;
    flags: cuint32;
    block_size: coff_t;
    io_size: coff_t;
    total_blocks: coff_t;
    free_blocks: coff_t;
    total_nodes: coff_t;
    free_nodes: coff_t;
    device_name: array[0..127] of AnsiChar;
    volume_name: array[0..Pred(B_FILE_NAME_LENGTH)] of AnsiChar;
    fsh_name: array[0..Pred(B_OS_NAME_LENGTH)] of AnsiChar;
  end;
  Pfs_info = ^Tfs_info;

function next_dev(pos: pcint32): dev_t; cdecl; external clib;
function fs_stat_dev(dev: dev_t; info: Pfs_info): cint; cdecl; external clib;

function OpenUrl(const URL: String): Boolean;

implementation

function OpenUrl(const URL: String): Boolean;
{$IF DEFINED(LCLQT5) or DEFINED(LCLQT6)}
var
  QUrl: QUrlH;
  AFileName: WideString;
{$ENDIF}
begin
  Result:= False;
{$IF DEFINED(LCLQT5) or DEFINED(LCLQT6)}
  QURl:= QUrl_Create;
  AFileName:= UTF8Decode(URL);
  QUrl_fromLocalFile(QUrl, @AFileName);
  Result:= QDesktopServices_openUrl(QUrl);
  QUrl_Destroy(QUrl);
{$ENDIF}
end;

end.

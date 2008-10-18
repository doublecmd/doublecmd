{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains specific WINDOWS functions.

    Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

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

unit uMyWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  drive_root: AnsiString = ':\';

function DriveReady(const Drv: Char): Boolean;
function mbGetVolumeLabel(const Drv: UTF8String; const VolReal: Boolean): UTF8String;
function mbSetVolumeLabel(sRootPathName, sVolumeName: UTF8String): Boolean;
procedure mbWaitLabelChange(const Drv: Char; const Str: UTF8String);
procedure mbCloseCD(const Drive: UTF8String);

implementation

uses
  LCLProc, ShellAPI, MMSystem;

function DisplayName(const wsDrv: WideString): WideString;
var
  SFI: TSHFileInfoW;
begin
  FillChar(SFI, SizeOf(SFI), 0);
  SHGetFileInfoW(PWChar(wsDrv), 0, SFI, SizeOf(SFI), SHGFI_DISPLAYNAME);
  Result:= SFI.szDisplayName;

  if Pos('(', Result) <> 0 then
    SetLength(Result, Pos('(', Result) - 2);
end;

(* Drive ready *)

function DriveReady(const Drv: Char): Boolean;
var
  NotUsed: DWORD;
begin
  Result:= GetVolumeInformation(PChar(Drv + drive_root), nil, 0, nil,
    NotUsed, NotUsed, nil, 0);
end;

(* Disk label *)

function mbGetVolumeLabel(const Drv: UTF8String; const VolReal: Boolean): UTF8String;
var
  WinVer: Byte;
  DriveType, NotUsed: DWORD;
  Buf: array [0..MAX_PATH - 1] of WideChar;
  wsDrv,
  wsResult: WideString;
begin
  Result:= '';
  wsDrv:= UTF8Decode(Drv);
  WinVer:= LOBYTE(LOWORD(GetVersion));
  DriveType:= GetDriveTypeW(PWChar(wsDrv));

  if (WinVer <= 4) and (DriveType <> DRIVE_REMOVABLE) or VolReal then
    begin // Win9x, Me, NT <= 4.0
      Buf[0]:= #0;
      GetVolumeInformationW(PWChar(wsDrv), Buf, DWORD(SizeOf(Buf)), nil,
                            NotUsed, NotUsed, nil, 0);
      wsResult:= Buf;

      if VolReal and (WinVer >= 5) and (Result <> '') and
         (DriveType <> DRIVE_REMOVABLE) then // Win2k, XP and higher
        wsResult:= DisplayName(wsDrv)
      else if (Result = '') and (not VolReal) then
        wsResult:= '<none>';
    end
  else
    wsResult:= DisplayName(wsDrv);
  Result:= UTF8Encode(wsResult);
end;

(* Wait for change disk label *)

function mbSetVolumeLabel(sRootPathName, sVolumeName: UTF8String): Boolean;
var
  wsRootPathName,
  wsVolumeName: WideString;
begin
  wsRootPathName:= UTF8Decode(sRootPathName);
  wsVolumeName:= UTF8Decode(sVolumeName);
  Result:= SetVolumeLabelW(PWChar(wsRootPathName), PWChar(wsVolumeName));
end;

procedure mbWaitLabelChange(const Drv: Char; const Str: UTF8String);
var
  st1, st2: string;
begin
  if mbGetVolumeLabel(Drv, True) = '' then
    Exit;
  st1:= TrimLeft(Str);
  st2:= st1;
  while st1 = st2 do
    st2:= mbGetVolumeLabel(Drv, FALSE);
end;

(* Close CD/DVD *)

procedure mbCloseCD(const Drive: UTF8String);
var
  OpenParms: MCI_OPEN_PARMS;
begin
  FillChar(OpenParms, SizeOf(OpenParms), 0);
  OpenParms.lpstrDeviceType:= 'CDAudio';
  OpenParms.lpstrElementName:= PChar(Drive + ':');
  mciSendCommand(0, MCI_OPEN, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, Longint(@OpenParms));
  mciSendCommand(OpenParms.wDeviceID, MCI_SET, MCI_SET_DOOR_CLOSED, 0);
  mciSendCommand(OpenParms.wDeviceID, MCI_CLOSE, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, Longint(@OpenParms));
end;

end.


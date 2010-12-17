{
    Double Commander
    -------------------------------------------------------------------------
    Some functions for working with trash

    Copyright (C) 2009-2010  Koblov Alexander (Alexx2000@mail.ru)

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

unit uTrash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

// 30.04.2009 - this function move files and folders to trash can.
function mbDeleteToTrash(const FileName: UTF8String): Boolean;
// 14.05.2009 - this funсtion checks trash availability.
function mbCheckTrash(sPath: UTF8String): Boolean;

implementation

uses
  uOSUtils, uDCUtils, uFileProcs
  {$IF DEFINED(MSWINDOWS)}
  , Windows, ShellApi, Win32Int, InterfaceBase, uMyWindows
  {$ELSEIF DEFINED(UNIX)}
  , BaseUnix, Unix, uMyUnix
    {$IFDEF DARWIN}
    , MacOSAll
    {$ENDIF}
  {$ENDIF};

function mbDeleteToTrash(const FileName: UTF8String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  wFileName: WideString;
  FileOp: TSHFileOpStructW;
begin
  wFileName:= UTF8Decode(FileName);
  wFileName:= wFileName + #0;
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.Wnd := TWin32Widgetset(Widgetset).AppHandle;
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PWideChar(wFileName);
  // Move without question
  FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT or FOF_NOCONFIRMATION;
  Result := (SHFileOperationW(@FileOp) = 0) and (not FileOp.fAnyOperationsAborted);
end;
{$ELSEIF DEFINED(DARWIN)}
var
  theSourceFSRef,
  theTargetFSRef: FSRef;
begin
  Result:= False;
  if (FSPathMakeRefWithOptions(PAnsiChar(FileName), kFSPathMakeRefDoNotFollowLeafSymlink, theSourceFSRef, nil) = noErr) then
  begin
    Result:= (FSMoveObjectToTrashSync(theSourceFSRef, theTargetFSRef, kFSFileOperationDefaultOptions) = noErr);
  end;
end;
{$ELSEIF DEFINED(UNIX)}
// This implementation is based on FreeDesktop.org "Trash Specification"
// (http://www.freedesktop.org/wiki/Specifications/trash-spec)
const
  trashFolder = '.Trash';
  trashFiles = 'files';
  trashInfo = 'info';
  trashExt =  '.trashinfo';
var
  sUserID: AnsiString;
  sTopDir, sFileName,
  sTemp, sNow, sHomeDir,
  sTrashInfoFile,
  sTrashDataFile: UTF8String;
  dtNow: TDateTime;
  st1, st2: Stat;

  function CreateTrashInfoFile: Boolean;
  var
    hFile: THandle;
  begin
    Result:= False;
    hFile:= FileCreate(sTrashInfoFile);
    if hFile <> feInvalidHandle then
    begin
      sTemp:= '[Trash Info]' + LineEnding;
      FileWrite(hFile, PChar(sTemp)[0], Length(sTemp));
      sTemp:= 'Path=' + FileName + LineEnding;
      FileWrite(hFile, PChar(sTemp)[0], Length(sTemp));
      sTemp:= 'DeletionDate=' + FormatDateTime('YYYY-MM-DD', dtNow);
      sTemp:= sTemp + 'T' + FormatDateTime('hh:nn:ss', dtNow) + LineEnding;
      FileWrite(hFile, PChar(sTemp)[0], Length(sTemp));
      FileClose(hFile);
      Result:= True;
    end;
  end;

  function TrashFile: Boolean;
  begin
    Result:= False;
    if CreateTrashInfoFile then
    begin
      Result:= (fpRename(FileName, sTrashDataFile) >= 0);
      if not Result then DeleteFile(sTrashInfoFile);
    end;
  end;

begin
  Result:= False;
  dtNow:= Now;
  sNow:= IntToStr(DateTimeToFileDate(dtNow));
  sFileName:= ExtractOnlyFileName(FileName) + '_' + sNow + ExtractFileExt(FileName);

  // Get user home directory
  sHomeDir:= GetEnvironmentVariable('HOME');
  // Get “top directory” for file
  sTopDir:= FindMountPointPath(FileName);
  // Check if file in home directory
  if (fpLStat(PChar(sHomeDir), st1) >= 0)
     and (fpLStat(PChar(sTopDir), st2) >= 0)
     and (st1.st_dev = st2.st_dev) then
  begin
    // Get $XDG_DATA_HOME directory
    sTemp:= GetEnvironmentVariable('XDG_DATA_HOME');
    if (Length(sTemp) = 0) then
      sTemp:= sHomeDir + '/.local/share/Trash'
    else
      sTemp:= IncludeTrailingPathDelimiter(sTemp) + 'Trash';
    // Create destination directories if needed
    if (ForceDirectories(sTemp + PathDelim + trashFiles) and ForceDirectories(sTemp + PathDelim + trashInfo)) then
    begin
      sTrashInfoFile:= sTemp + PathDelim + trashInfo + PathDelim + sFileName + trashExt;
      sTrashDataFile:= sTemp + PathDelim + trashFiles + PathDelim + sFileName;
      Result:= TrashFile;
      Exit;
    end;
  end;
  sUserID:= IntToStr(fpGetUID);
  // Try to use "$topdir/.Trash/$uid" directory
  sTemp:= sTopDir + trashFolder;
  if (fpLStat(PChar(sTemp), st1) >= 0)
     and fpS_ISDIR(st1.st_mode) and not fpS_ISLNK(st1.st_mode) then
    begin
      sTemp:= sTemp + PathDelim + sUserID;
      // Create destination directories if needed
      if (ForceDirectories(sTemp + PathDelim + trashFiles) and ForceDirectories(sTemp + PathDelim + trashInfo)) then
      begin
        sTrashInfoFile:= sTemp + PathDelim + trashInfo + PathDelim + sFileName + trashExt;
        sTrashDataFile:= sTemp + PathDelim + trashFiles + PathDelim + sFileName;
        Result:= TrashFile;
        Exit;
      end;
    end;
  // Try to use "$topdir/.Trash-$uid" directory
  sTemp:= sTopDir + trashFolder + '-' + sUserID;
  if ((fpLStat(PChar(sTemp), st1) >= 0) and fpS_ISDIR(st1.st_mode)
     and not fpS_ISLNK(st1.st_mode)) or CreateDir(sTemp) then
    begin
      // Create destination directories if needed
      if (ForceDirectories(sTemp + PathDelim + trashFiles) and ForceDirectories(sTemp + PathDelim + trashInfo)) then
      begin
        sTrashInfoFile:= sTemp + PathDelim + trashInfo + PathDelim + sFileName + trashExt;
        sTrashDataFile:= sTemp + PathDelim + trashFiles + PathDelim + sFileName;
        Result:= TrashFile;
        Exit;
      end;
    end;
end;
{$ELSE}
begin
  Result:= False;
end;
{$ENDIF}

function mbCheckTrash(sPath: UTF8String): Boolean;
{$IF DEFINED(MSWINDOWS)}
const
  wsRoot: WideString = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\BitBucket\';
var
  Key: HKEY;
  Value: DWORD;
  ValueSize: LongInt;
  VolumeName: WideString;
begin
  Result:= False;
  if not mbDirectoryExists(sPath) then Exit;
  ValueSize:= SizeOf(DWORD);
  // Windows Vista/Seven
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
    begin
      VolumeName:= GetMountPointVolumeName(UTF8Decode(ExtractFileDrive(sPath)));
      VolumeName:= 'Volume' + PathDelim + ExtractVolumeGUID(VolumeName);
      if RegOpenKeyExW(HKEY_CURRENT_USER, PWideChar(wsRoot + VolumeName), 0, KEY_READ, Key) = ERROR_SUCCESS then
        begin
          if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
            Value:= 0; // delete to trash by default
          Result:= (Value = 0);
          RegCloseKey(Key);
        end;
    end
  // Windows 2000/XP
  else if RegOpenKeyExW(HKEY_LOCAL_MACHINE, PWideChar(wsRoot), 0, KEY_READ, Key) = ERROR_SUCCESS then
    begin
      if RegQueryValueExW(Key, 'UseGlobalSettings', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
        Value:= 1; // use global settings by default
      if (Value = 1) then
        begin
          if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
            Value:= 0; // delete to trash by default
          Result:= (Value = 0);
          RegCloseKey(Key);
        end
      else
        begin
          RegCloseKey(Key);
          if RegOpenKeyExW(HKEY_LOCAL_MACHINE, PWideChar(wsRoot + sPath[1]), 0, KEY_READ, Key) = ERROR_SUCCESS then
            begin
              if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
                Value:= 0; // delete to trash by default
              Result:= (Value = 0);
              RegCloseKey(Key);
            end;
        end;
    end;
end;
{$ELSE}
begin
  Result := True;
end;
{$ENDIF}

end.


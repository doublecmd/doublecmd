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
  uOSUtils,
  {$IF DEFINED(MSWINDOWS)}
  Windows, ShellApi, Win32Int, InterfaceBase, uMyWindows
  {$ELSEIF DEFINED(UNIX)}
  BaseUnix, uMyUnix, uDCUtils, FileUtil
    {$IFDEF DARWIN}
    , MacOSAll, DynLibs, StrUtils
    {$ELSE}
    , uFileProcs
    {$ENDIF}
  {$ENDIF};

function mbDeleteToTrash(const FileName: UTF8String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  wsFileName: WideString;
  FileOp: TSHFileOpStructW;
  dwFileAttributes: LongWord;
begin
  wsFileName:= UTF8Decode(FileName);
  // Windows before Vista don't move symlink into recycle bin
  // correctly, so we return False in that case
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 6) then
  begin
    dwFileAttributes:= GetFileAttributesW(PWideChar(wsFileName));
    if FPS_ISLNK(dwFileAttributes) and FPS_ISDIR(dwFileAttributes) then
      Exit(False);
  end;
  wsFileName:= wsFileName + #0;
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.Wnd := TWin32Widgetset(Widgetset).AppHandle;
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PWideChar(wsFileName);
  // Move without question
  FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT or FOF_NOCONFIRMATION;
  Result := (SHFileOperationW(@FileOp) = 0) and (not FileOp.fAnyOperationsAborted);
end;
{$ELSEIF DEFINED(DARWIN)}
type
  TFSMoveObjectToTrashSync = function( const (*var*) source: FSRef; var target: FSRef; options: OptionBits ): OSStatus; stdcall;
var
  FSMoveObjectToTrash: TFSMoveObjectToTrashSync;
  CarbonLib: THandle;
  theSourceFSRef,
  theTargetFSRef: FSRef;

  newFileName: String;
  fullFileName: String;
  trashDir: String;
  dirList: array[0..100] of PChar;
begin
  Result := False;

  CarbonLib := LoadLibrary('Carbon.framework/Versions/A/Carbon');
  if CarbonLib <> NilHandle then
  begin
    try
      FSMoveObjectToTrash := TFSMoveObjectToTrashSync(GetProcedureAddress(CarbonLib, 'FSMoveObjectToTrashSync'));
      if Assigned(FSMoveObjectToTrash) then
      begin
        if (FSPathMakeRefWithOptions(PAnsiChar(FileName), kFSPathMakeRefDoNotFollowLeafSymlink, theSourceFSRef, nil) = noErr) then
        begin
          Result:= (FSMoveObjectToTrash(theSourceFSRef, theTargetFSRef, kFSFileOperationDefaultOptions) = noErr);
          Exit;
        end;
      end; { if }

    finally
      UnloadLibrary(CarbonLib);
    end; { try - finally }
  end; { if }

{
  MacOSX 10.4 and below compatibility mode:
  - If file is in base drive, it gets moved to $HOME/.Trash
  - If file is in some other local drive, it gets moved to /Volumes/.Trashes/uid/
  - If file is in network, it can't be moved to trash
  Trash folders are automatically created by OS at login and deleted if empty at logout.
  If a file with same name exists in trash folder, time is appended to filename
}
  fullFileName := ExpandFileName(FileName);
  if AnsiStartsStr('/Volumes/', fullFileName) then
  begin
    // file is not located at base drive
    FillChar(dirList, SizeOf(dirList), 0);
    SysUtils.GetDirs(fullFileName, dirList);
    trashDir := Format('/Volumes/%s/.Trashes/%d/', [StrPas(dirList[1]), FpGetuid]);
  end { if }
  else
    trashDir := GetHomeDir + '/.Trash/';

  // check if trash folder exists (e.g. network drives don't have one)
  if not mbDirectoryExists(trashDir) then
  begin
    Result := false;
    Exit;
  end; { if }

  newFileName := trashDir + ExtractFileName(FileName);
  if mbFileSystemEntryExists(newFileName) then
    newFileName := Format('%s %s', [newFileName, FormatDateTime('hh-nn-ss', Time)]);

  Result := mbRenameFile(FileName, newFileName);
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
    hFile:= mbFileCreate(sTrashInfoFile);
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
      Result:= (fpRename(UTF8ToSys(FileName), sTrashDataFile) >= 0);
      if not Result then mbDeleteFile(sTrashInfoFile);
    end;
  end;

begin
  Result:= False;
  dtNow:= Now;
  sNow:= IntToStr(Trunc(dtNow * 86400000)); // The time in milliseconds
  sFileName:= ExtractOnlyFileName(FileName) + '_' + sNow + ExtractFileExt(FileName);
  // Get user home directory
  sHomeDir:= GetHomeDir;
  // Check if file in home directory
  if (fpLStat(PChar(UTF8ToSys(sHomeDir)), st1) >= 0)
     and (fpLStat(PChar(UTF8ToSys(FileName)), st2) >= 0)
     and (st1.st_dev = st2.st_dev) then
  begin
    // Get $XDG_DATA_HOME directory
    sTemp:= mbGetEnvironmentVariable('XDG_DATA_HOME');
    if (Length(sTemp) = 0) then
      sTemp:= sHomeDir + '/.local/share/Trash'
    else
      sTemp:= IncludeTrailingPathDelimiter(sTemp) + 'Trash';
    // Create destination directories if needed
    if (mbForceDirectory(sTemp + PathDelim + trashFiles) and mbForceDirectory(sTemp + PathDelim + trashInfo)) then
    begin
      sTrashInfoFile:= sTemp + PathDelim + trashInfo + PathDelim + sFileName + trashExt;
      sTrashDataFile:= sTemp + PathDelim + trashFiles + PathDelim + sFileName;
      Result:= TrashFile;
      Exit;
    end;
  end;
  sUserID:= IntToStr(fpGetUID);
  // Get “top directory” for file
  sTopDir:= FindMountPointPath(FileName);
  // Try to use "$topdir/.Trash/$uid" directory
  sTemp:= sTopDir + trashFolder;
  if (fpLStat(PChar(UTF8ToSys(sTemp)), st1) >= 0)
     and fpS_ISDIR(st1.st_mode) and not fpS_ISLNK(st1.st_mode) then
    begin
      sTemp:= sTemp + PathDelim + sUserID;
      // Create destination directories if needed
      if mbForceDirectory(sTemp + PathDelim + trashFiles) and mbForceDirectory(sTemp + PathDelim + trashInfo) then
      begin
        sTrashInfoFile:= sTemp + PathDelim + trashInfo + PathDelim + sFileName + trashExt;
        sTrashDataFile:= sTemp + PathDelim + trashFiles + PathDelim + sFileName;
        Result:= TrashFile;
        Exit;
      end;
    end;
  // Try to use "$topdir/.Trash-$uid" directory
  sTemp:= sTopDir + trashFolder + '-' + sUserID;
  if ((fpLStat(PChar(UTF8ToSys(sTemp)), st1) >= 0) and fpS_ISDIR(st1.st_mode)
     and not fpS_ISLNK(st1.st_mode)) or mbCreateDir(sTemp) then
    begin
      // Create destination directories if needed
      if mbForceDirectory(sTemp + PathDelim + trashFiles) and mbForceDirectory(sTemp + PathDelim + trashInfo) then
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


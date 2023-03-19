{
    Double Commander
    -------------------------------------------------------------------------
    Some functions for working with trash

    Copyright (C) 2009-2022 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uTrash;

{$mode objfpc}{$H+}

{$IF DEFINED(DARWIN)}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  LazUtf8, Classes, SysUtils;

// this function move files and folders to trash can.
function mbDeleteToTrash(const FileName: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
{ en
  this funсtion checks trash availability for Windows.
}
function mbCheckTrash(const {%H-}sPath: String): Boolean;
{$ENDIF}

var
  FileTrashUtf8: function(const FileName: String): Boolean;

implementation

uses
  DCOSUtils, DCStrUtils,
  {$IF DEFINED(MSWINDOWS)}
  Windows, ShellApi, DCConvertEncoding, uMyWindows
  {$ELSEIF DEFINED(UNIX)}
  BaseUnix, FileUtil
    {$IF DEFINED(DARWIN)}
    , MacOSAll, DynLibs, CocoaAll, uMyDarwin, uSysFolders
    {$ELSEIF DEFINED(HAIKU)}
    , DCHaiku, DCConvertEncoding
    {$ELSE}
    , DCConvertEncoding, DCUnix, uXdg, uElevation, uProcessInfo, uURIHandling
    {$ENDIF}
  {$ENDIF}
  ;

{$IF DEFINED(DARWIN)}
type
  NSFileManager = objcclass external (NSObject)
  public
    class function defaultManager: NSFileManager; message 'defaultManager';
    function trashItemAtURL_resultingItemURL_error(url: NSURL; outResultingURL: NSURLPtr; error: NSErrorPtr): Boolean; message 'trashItemAtURL:resultingItemURL:error:';
  end;

var
  CoreLib: TLibHandle;
  FSMoveObjectToTrash: function( const (*var*) source: FSRef; var target: FSRef; options: OptionBits ): OSStatus; stdcall;
{$ENDIF}

function mbDeleteToTrash(const FileName: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  wsFileName: WideString;
  FileOp: TSHFileOpStructW;
  dwFileAttributes: LongWord;
  vFuncRes: Integer;
begin
  // Windows cannot move file with space at the end into
  // recycle bin correctly, so we return False in this case
  if StrEnds(FileName, ' ') then
    Exit(False);

  wsFileName:= CeUtf8ToUtf16(FileName);
  // Windows before Vista cannot move symlink into
  // recycle bin correctly, so we return False in this case
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 6) then
  begin
    dwFileAttributes:= GetFileAttributesW(PWideChar(wsFileName));
    if FPS_ISLNK(dwFileAttributes) and FPS_ISDIR(dwFileAttributes) then
      Exit(False);
  end;

  wsFileName:= wsFileName + #0;
  FillChar({%H-}FileOp, SizeOf(FileOp), 0);
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PWideChar(wsFileName);
  // Move without question
  FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT or FOF_NOCONFIRMATION;
  vFuncRes := SHFileOperationW(@FileOp);
  if vFuncRes = $78 then {DE_ACCESSDENIEDSRC: Security settings denied access to the source}
    SetLastOSError(ERROR_ACCESS_DENIED);
  Result := (vFuncRes = 0) and (not FileOp.fAnyOperationsAborted);
end;
{$ELSEIF DEFINED(DARWIN)}
var
  theSourceFSRef,
  theTargetFSRef: FSRef;
  newFileName: String;
  trashDir: String;
begin
  // Mac OS X >= 10.8
  if (NSAppKitVersionNumber >= 1187) then
  begin
    Result:= NSFileManager.defaultManager.trashItemAtURL_resultingItemURL_error(NSURL(NSURL.fileURLWithPath(StringToNSString(FileName))), nil, nil);
    Exit;
  end;

  // Mac OS X >= 10.5
  if Assigned(FSMoveObjectToTrash) then
  begin
    if (FSPathMakeRefWithOptions(PAnsiChar(FileName), kFSPathMakeRefDoNotFollowLeafSymlink, theSourceFSRef, nil) = noErr) then
    begin
      Result:= (FSMoveObjectToTrash(theSourceFSRef, theTargetFSRef, kFSFileOperationDefaultOptions) = noErr);
      Exit;
    end;
  end; { if }

 {
  MacOSX 10.4 and below compatibility mode:
  - If file is in base drive, it gets moved to $HOME/.Trash/
  - If file is in some other local drive, it gets moved to /Volumes/$(Volume)/.Trashes/$UID/
  - If file is in network, it can't be moved to trash
  Trash folders are automatically created by OS at login and deleted if empty at logout.
  If a file with same name exists in trash folder, time is appended to filename
}
  trashDir := FindMountPointPath(FileName);
  if (trashDir = PathDelim) then
    trashDir := GetHomeDir + '/.Trash'
  else begin
    // file is not located at base drive
    trashDir += '.Trashes/' + IntToStr(fpGetUID);
  end; { if }

  // check if trash folder exists (e.g. network drives don't have one)
  if not mbDirectoryExists(trashDir) then
  begin
    Result := false;
    Exit;
  end; { if }

  newFileName := trashDir + PathDelim + ExtractFileName(FileName);
  if mbFileSystemEntryExists(newFileName) then
    newFileName := Format('%s %s', [newFileName, FormatDateTime('hh-nn-ss', Time)]);

  Result := mbRenameFile(FileName, newFileName);
end;
{$ELSEIF DEFINED(HAIKU)}
const
  kAttrOriginalPath = '_trk/original_path';
var
  dev: dev_t;
  ATrash: String;
  AHandle: THandle;
  AFileName, ATrashFile: String;
begin
  AFileName:= CeUtf8ToSys(FileName);
  dev:= dev_for_path(PAnsiChar(AFileName));
  if not mbFindDirectory(B_TRASH_DIRECTORY, dev, True, ATrash) then
    Exit(False);
  if IsInPath(ATrash, FileName, True, True) then
    Exit(False);
  ATrash:= IncludeTrailingBackslash(ATrash);
  ATrashFile:= ATrash + ExtractOnlyFileName(FileName);
  ATrashFile:= GetTempName(ATrashFile, ExtractOnlyFileExt(FileName));

  if fpRename(FileName, ATrashFile) < 0 then
    Exit(False);

  Result:= mbFileWriteXattr(ATrashFile, kAttrOriginalPath, AFileName);
  if not Result then begin
    fpRename(ATrashFile, FileName);
  end;
end;
{$ELSEIF DEFINED(UNIX)}
// This implementation is based on FreeDesktop.org "Trash Specification"
// (http://www.freedesktop.org/wiki/Specifications/trash-spec)

  function GetHomeEx: String;
  var
    UserID: TUid;
    UserInfo: PPasswordRecord;
  begin
    if (WorkerService = nil) then
      UserID:= fpGetUID
    else begin
      UserID:= GetProcessUserId(StrToInt(ParamStr(2)));
    end;
    UserInfo:= getpwuid(UserID);
    if (UserInfo <> nil) and (UserInfo^.pw_dir <> '') then
    begin
      Result:= CeSysToUtf8(UserInfo^.pw_dir);
    end
    else begin
      Result:= GetEnvironmentVariable('HOME');
    end;
    Result:= ExcludeBackPathDelimiter(Result);
  end;

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
  sTrashDataFile: String;
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
      sTemp:= 'Path=' + URIEncode(FileName) + LineEnding;
      FileWrite(hFile, PChar(sTemp)[0], Length(sTemp));
      sTemp:= 'DeletionDate=' + FormatDateTime('YYYY-MM-DD', dtNow);
      sTemp:= sTemp + 'T' + FormatDateTime('hh:nn:ss', dtNow) + LineEnding;
      FileWrite(hFile, PChar(sTemp)[0], Length(sTemp));
      FileClose(hFile);
      Result:= True;
    end;
  end;

  function TrashFile: Boolean;
  var vLastError: Integer;
  begin
    Result:= False;
    if CreateTrashInfoFile then
    begin
      Result:= (fpRename(UTF8ToSys(FileName), sTrashDataFile) >= 0);
      if not Result then
        begin
          vLastError := GetLastOSError;
          mbDeleteFile(sTrashInfoFile);
          SetLastOSError(vLastError);
        end
     end;
  end;

var sGetUserDataDir: string;
begin
  Result:= False;
  dtNow:= Now;
  sNow:= IntToStr(Trunc(dtNow * 86400000)); // The time in milliseconds
  sFileName:= ExtractOnlyFileName(FileName) + '_' + sNow + ExtractFileExt(FileName);
  // Get user home directory
  sHomeDir:= GetHomeEx;
  // Check if file in home directory
  // If it's a file, stat the parent directory instead for correct behavior on OverlayFS,
  // it shouldn't make any difference in other cases
  if (fpStat(UTF8ToSys(sHomeDir), {%H-}st1) >= 0)
     and (fpLStat(UTF8ToSys(FileName), {%H-}st2) >= 0)
     and (fpS_ISDIR(st2.st_mode) or (fpStat(UTF8ToSys(ExtractFileDir(FileName)), st2) >= 0))
     and (st1.st_dev = st2.st_dev) then
  begin
    // Get trash directory in $XDG_DATA_HOME
    if WorkerService = nil then
      sGetUserDataDir := IncludeTrailingPathDelimiter(GetUserDataDir)
    else
      sGetUserDataDir := sHomeDir + '/.local/share/';
    sTemp:= sGetUserDataDir + 'Trash';
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
  // Get “top directory” for file
  sTopDir:= FindMountPointPath(FileName);
  // Try to use "$topdir/.Trash/$uid" directory
  sTemp:= sTopDir + trashFolder;
  if (fpLStat(UTF8ToSys(sTemp), st1) >= 0)
     and fpS_ISDIR(st1.st_mode) and not fpS_ISLNK(st1.st_mode) then
    begin
      sTemp:= sTemp + PathDelim + sUserID;
      // Create destination directories if needed
      if ForceDirectories(sTemp + PathDelim + trashFiles) and ForceDirectories(sTemp + PathDelim + trashInfo) then
      begin
        sTrashInfoFile:= sTemp + PathDelim + trashInfo + PathDelim + sFileName + trashExt;
        sTrashDataFile:= sTemp + PathDelim + trashFiles + PathDelim + sFileName;
        Result:= TrashFile;
        Exit;
      end;
    end;
  // Try to use "$topdir/.Trash-$uid" directory
  sTemp:= sTopDir + trashFolder + '-' + sUserID;
  if ((fpLStat(UTF8ToSys(sTemp), st1) >= 0) and fpS_ISDIR(st1.st_mode)
     and not fpS_ISLNK(st1.st_mode)) or mbCreateDir(sTemp) then
    begin
      // Create destination directories if needed
      if ForceDirectories(sTemp + PathDelim + trashFiles) and ForceDirectories(sTemp + PathDelim + trashInfo) then
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

{$IF DEFINED(MSWINDOWS)}
function mbCheckTrash(const sPath: String): Boolean;
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
  // Windows Vista or newer
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
    begin
      VolumeName:= GetMountPointVolumeName(CeUtf8ToUtf16(ExtractFileDrive(sPath)));
      if (VolumeName <> EmptyWideStr) and
         (RegOpenKeyExW(HKEY_CURRENT_USER, PWideChar(wsRoot + 'Volume' + PathDelim +
           ExtractVolumeGUID(VolumeName)), 0, KEY_READ, {%H-}Key) = ERROR_SUCCESS) then
        begin
          if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) = ERROR_SUCCESS then
            Result := (Value = 0);
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
          if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) = ERROR_SUCCESS then
            Result:= (Value = 0); // delete not to trash by default
          RegCloseKey(Key);
        end
      else
        begin
          RegCloseKey(Key);
          if RegOpenKeyExW(HKEY_LOCAL_MACHINE, PWideChar(wsRoot + sPath[1]), 0, KEY_READ, {%H-}Key) = ERROR_SUCCESS then
            begin
              if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) = ERROR_SUCCESS then
                Result:= (Value = 0); // delete not to trash by default
              RegCloseKey(Key);
            end;
        end;
    end;
end;
{$ENDIF}

initialization
  FileTrashUtf8:= @mbDeleteToTrash;
{$IF DEFINED(DARWIN)}
  CoreLib := LoadLibrary('CoreServices.framework/CoreServices');
  if CoreLib <> NilHandle then begin
    Pointer(FSMoveObjectToTrash) := GetProcedureAddress(CoreLib, 'FSMoveObjectToTrashSync');
  end;
{$ENDIF}

end.

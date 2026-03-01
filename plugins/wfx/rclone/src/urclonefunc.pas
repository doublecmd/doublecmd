{
   Double Commander
   -------------------------------------------------------------------------
   WFX plugin for working with rclone remotes

   Copyright (C) 2026 Miklos Mukka Szel <contact@miklos-szel.com>

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

unit uRcloneFunc;

{$mode delphi}{$H+}
{$include calling.inc}

interface

uses
  SysUtils, Classes, WfxPlugin, Extension;

{ WFX API Functions }
function FsInitW(PluginNr: Integer; pProgressProc: TProgressProcW;
  pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer; dcpcall;

function FsFindFirstW(Path: PWideChar; var FindData: TWin32FindDataW): THandle; dcpcall;
function FsFindNextW(Hdl: THandle; var FindData: TWin32FindDataW): BOOL; dcpcall;
function FsFindClose(Hdl: THandle): Integer; dcpcall;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: Integer): Integer; dcpcall;

function FsDeleteFileW(RemoteName: PWideChar): BOOL; dcpcall;
function FsRemoveDirW(RemoteName: PWideChar): BOOL; dcpcall;
function FsMkDirW(RemoteDir: PWideChar): BOOL; dcpcall;

function FsRenMovFileW(OldName, NewName: PWideChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; dcpcall;
function FsDisconnectW(DisconnectRoot: PWideChar): BOOL; dcpcall;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); dcpcall;
procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); dcpcall;
function FsGetBackgroundFlags: Integer; dcpcall;
function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: Integer; TheIcon: PWfxIcon): Integer; dcpcall;

{ Extension API }
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;

var
  gStartupInfo: TExtensionStartupInfo;
  gProgressProc: TProgressProcW;
  gLogProc: TLogProcW;
  gRequestProc: TRequestProcW;
  gPluginNr: Integer;

implementation

uses
  uRcloneUtil, uRcloneCli, uRcloneJson;

type
  TListRec = record
    Path: UnicodeString;
    Index: Integer;
    RemoteList: TStringList;   // For root listing
    FileList: TRcloneFileList; // For directory listing
  end;
  PListRec = ^TListRec;

var
  gRcloneCli: TRcloneCli;

function ProgressCallback(SourceName, TargetName: PWideChar;
  PercentDone: Integer): Integer;
begin
  Result := 0;
  if Assigned(gProgressProc) then
    Result := gProgressProc(gPluginNr, SourceName, TargetName, PercentDone);
end;

procedure FillFindData(var FindData: TWin32FindDataW; const RcloneFile: TRcloneFile);
var
  FileTime: TFileTime;
  SizeRec: TInt64Rec;
begin
  FillChar(FindData, SizeOf(FindData), 0);

  // Copy filename
  StrPLCopy(FindData.cFileName, RcloneFile.Name, MAX_PATH - 1);

  // Set attributes
  if RcloneFile.IsDir then
    FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
  else
    FindData.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;

  // Set file size
  SizeRec.Value := RcloneFile.Size;
  FindData.nFileSizeLow := SizeRec.Low;
  FindData.nFileSizeHigh := SizeRec.High;

  // Set modification time
  FileTime := StrToFileTime(RcloneFile.ModTime);
  FindData.ftLastWriteTime := FileTime;
  FindData.ftCreationTime := FileTime;
  FindData.ftLastAccessTime := FileTime;
end;

procedure FillRemoteFindData(var FindData: TWin32FindDataW;
  const RemoteName: UnicodeString);
begin
  FillChar(FindData, SizeOf(FindData), 0);

  // Copy remote name
  StrPLCopy(FindData.cFileName, RemoteName, MAX_PATH - 1);

  // Remotes appear as volumes/directories
  FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;

  // Set invalid size and time (not applicable for remotes)
  FindData.nFileSizeLow := $FFFFFFFE;
  FindData.nFileSizeHigh := $FFFFFFFF;
  FindData.ftLastWriteTime.dwLowDateTime := $FFFFFFFE;
  FindData.ftLastWriteTime.dwHighDateTime := $FFFFFFFF;
end;

{ WFX API Implementation }

function FsInitW(PluginNr: Integer; pProgressProc: TProgressProcW;
  pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer; dcpcall;
begin
  gPluginNr := PluginNr;
  gProgressProc := pProgressProc;
  gLogProc := pLogProc;
  gRequestProc := pRequestProc;

  // Initialize rclone CLI wrapper
  if not Assigned(gRcloneCli) then
    gRcloneCli := TRcloneCli.Create;

  gRcloneCli.Initialize(PluginNr, @ProgressCallback);

  Result := 0;
end;

function FsFindFirstW(Path: PWideChar; var FindData: TWin32FindDataW): THandle; dcpcall;
var
  ListRec: PListRec;
  PathStr: UnicodeString;
  RemoteName, RemotePath: UnicodeString;
  RclonePath: AnsiString;
begin
  Result := wfxInvalidHandle;

  New(ListRec);
  ListRec^.Index := 0;
  ListRec^.Path := Path;
  ListRec^.RemoteList := nil;
  ListRec^.FileList := nil;

  PathStr := Path;

  // Root path - list all remotes
  if IsRootPath(PathStr) then
  begin
    ListRec^.RemoteList := gRcloneCli.ListRemotes;
    if ListRec^.RemoteList.Count > 0 then
    begin
      FillRemoteFindData(FindData, UTF8ToWide(ListRec^.RemoteList[0]));
      ListRec^.Index := 1;
      Result := THandle(ListRec);
    end
    else
    begin
      Dispose(ListRec);
    end;
  end
  else
  begin
    // Directory listing
    RemoteName := ExtractRemoteName(PathStr);
    RemotePath := ExtractRemotePath(PathStr);
    RclonePath := WideToUTF8(BuildRclonePath(RemoteName, RemotePath));

    ListRec^.FileList := gRcloneCli.ListDirectory(RclonePath);

    if (ListRec^.FileList <> nil) and (ListRec^.FileList.Count > 0) then
    begin
      FillFindData(FindData, ListRec^.FileList[0]);
      ListRec^.Index := 1;
      Result := THandle(ListRec);
    end
    else
    begin
      // Empty directory or error - return invalid handle
      // but first check if we got any data at all
      if ListRec^.FileList <> nil then
        ListRec^.FileList.Free;
      Dispose(ListRec);
    end;
  end;
end;

function FsFindNextW(Hdl: THandle; var FindData: TWin32FindDataW): BOOL; dcpcall;
var
  ListRec: PListRec;
begin
  Result := False;
  ListRec := PListRec(Hdl);

  if ListRec = nil then
    Exit;

  // Root listing (remotes)
  if ListRec^.RemoteList <> nil then
  begin
    if ListRec^.Index < ListRec^.RemoteList.Count then
    begin
      FillRemoteFindData(FindData, UTF8ToWide(ListRec^.RemoteList[ListRec^.Index]));
      Inc(ListRec^.Index);
      Result := True;
    end;
  end
  // Directory listing
  else if ListRec^.FileList <> nil then
  begin
    if ListRec^.Index < ListRec^.FileList.Count then
    begin
      FillFindData(FindData, ListRec^.FileList[ListRec^.Index]);
      Inc(ListRec^.Index);
      Result := True;
    end;
  end;
end;

function FsFindClose(Hdl: THandle): Integer; dcpcall;
var
  ListRec: PListRec;
begin
  Result := 0;
  ListRec := PListRec(Hdl);

  if ListRec <> nil then
  begin
    if ListRec^.RemoteList <> nil then
      ListRec^.RemoteList.Free;
    if ListRec^.FileList <> nil then
      ListRec^.FileList.Free;
    Dispose(ListRec);
  end;
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
var
  RemoteStr, LocalStr: UnicodeString;
  Remote, Path: UnicodeString;
  RclonePath: AnsiString;
begin
  RemoteStr := RemoteName;
  LocalStr := LocalName;

  // Check if file exists and handle overwrite
  if ((CopyFlags and FS_COPYFLAGS_OVERWRITE) = 0) and FileExists(WideToUTF8(LocalStr)) then
  begin
    Result := FS_FILE_EXISTS;
    Exit;
  end;

  // Build rclone path
  Remote := ExtractRemoteName(RemoteStr);
  Path := ExtractRemotePath(RemoteStr);
  RclonePath := WideToUTF8(BuildRclonePath(Remote, Path));

  // Report initial progress
  if Assigned(gProgressProc) then
    gProgressProc(gPluginNr, RemoteName, LocalName, 0);

  // Perform the copy
  Result := gRcloneCli.CopyToLocal(RclonePath, WideToUTF8(LocalStr));

  // Report completion
  if (Result = FS_FILE_OK) and Assigned(gProgressProc) then
    gProgressProc(gPluginNr, RemoteName, LocalName, 100);
end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: Integer): Integer; dcpcall;
var
  RemoteStr, LocalStr: UnicodeString;
  Remote, Path: UnicodeString;
  RclonePath: AnsiString;
begin
  RemoteStr := RemoteName;
  LocalStr := LocalName;

  // Build rclone path
  Remote := ExtractRemoteName(RemoteStr);
  Path := ExtractRemotePath(RemoteStr);
  RclonePath := WideToUTF8(BuildRclonePath(Remote, Path));

  // Report initial progress
  if Assigned(gProgressProc) then
    gProgressProc(gPluginNr, LocalName, RemoteName, 0);

  // Perform the copy
  Result := gRcloneCli.CopyToRemote(WideToUTF8(LocalStr), RclonePath);

  // Report completion
  if (Result = FS_FILE_OK) and Assigned(gProgressProc) then
    gProgressProc(gPluginNr, LocalName, RemoteName, 100);
end;

function FsDeleteFileW(RemoteName: PWideChar): BOOL; dcpcall;
var
  RemoteStr: UnicodeString;
  Remote, Path: UnicodeString;
  RclonePath: AnsiString;
begin
  RemoteStr := RemoteName;

  Remote := ExtractRemoteName(RemoteStr);
  Path := ExtractRemotePath(RemoteStr);
  RclonePath := WideToUTF8(BuildRclonePath(Remote, Path));

  Result := gRcloneCli.DeleteFile(RclonePath);
end;

function FsRemoveDirW(RemoteName: PWideChar): BOOL; dcpcall;
var
  RemoteStr: UnicodeString;
  Remote, Path: UnicodeString;
  RclonePath: AnsiString;
begin
  RemoteStr := RemoteName;

  Remote := ExtractRemoteName(RemoteStr);
  Path := ExtractRemotePath(RemoteStr);
  RclonePath := WideToUTF8(BuildRclonePath(Remote, Path));

  Result := gRcloneCli.DeleteDir(RclonePath);
end;

function FsMkDirW(RemoteDir: PWideChar): BOOL; dcpcall;
var
  RemoteStr: UnicodeString;
  Remote, Path: UnicodeString;
  RclonePath: AnsiString;
begin
  RemoteStr := RemoteDir;

  Remote := ExtractRemoteName(RemoteStr);
  Path := ExtractRemotePath(RemoteStr);
  RclonePath := WideToUTF8(BuildRclonePath(Remote, Path));

  Result := gRcloneCli.MakeDir(RclonePath);
end;

function FsRenMovFileW(OldName, NewName: PWideChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
var
  OldStr, NewStr: UnicodeString;
  OldRemote, OldPath: UnicodeString;
  NewRemote, NewPath: UnicodeString;
  OldRclonePath, NewRclonePath: AnsiString;
begin
  Result := FS_FILE_NOTSUPPORTED;

  if not Move then
  begin
    // Copy operation - not directly supported, would need copyto
    Result := FS_FILE_NOTSUPPORTED;
    Exit;
  end;

  OldStr := OldName;
  NewStr := NewName;

  OldRemote := ExtractRemoteName(OldStr);
  OldPath := ExtractRemotePath(OldStr);
  NewRemote := ExtractRemoteName(NewStr);
  NewPath := ExtractRemotePath(NewStr);

  // Both paths must be on the same remote
  if OldRemote <> NewRemote then
  begin
    Result := FS_FILE_NOTSUPPORTED;
    Exit;
  end;

  OldRclonePath := WideToUTF8(BuildRclonePath(OldRemote, OldPath));
  NewRclonePath := WideToUTF8(BuildRclonePath(NewRemote, NewPath));

  // Report progress
  if Assigned(gProgressProc) then
    gProgressProc(gPluginNr, OldName, NewName, 0);

  if gRcloneCli.MoveFile(OldRclonePath, NewRclonePath) then
  begin
    Result := FS_FILE_OK;
    if Assigned(gProgressProc) then
      gProgressProc(gPluginNr, OldName, NewName, 100);
  end
  else
    Result := FS_FILE_WRITEERROR;
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; dcpcall;
var
  RemoteStr, VerbStr: UnicodeString;
begin
  Result := FS_EXEC_OK;

  RemoteStr := RemoteName;
  VerbStr := Verb;

  if VerbStr = 'open' then
  begin
    // When opening a remote at root level, navigate into it
    if IsRootPath(ExtractFilePath(RemoteStr)) then
      Result := FS_EXEC_SYMLINK
    else
      Result := FS_EXEC_YOURSELF;
  end
  else if VerbStr = 'properties' then
  begin
    // Could show properties dialog here
    Result := FS_EXEC_OK;
  end
  else
    Result := FS_EXEC_OK;
end;

function FsDisconnectW(DisconnectRoot: PWideChar): BOOL; dcpcall;
begin
  // rclone doesn't maintain persistent connections
  // Just log the disconnect
  if Assigned(gLogProc) then
    gLogProc(gPluginNr, msgtype_disconnect, PWideChar('DISCONNECT ' + UnicodeString(DisconnectRoot)));

  Result := True;
end;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); dcpcall;
begin
  StrPLCopy(DefRootName, 'rclone', MaxLen);
end;

procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); dcpcall;
begin
  // Could load configuration from ini file here
end;

function FsGetBackgroundFlags: Integer; dcpcall;
begin
  // Support background downloads and uploads
  Result := BG_DOWNLOAD or BG_UPLOAD;
end;

function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: Integer; TheIcon: PWfxIcon): Integer; dcpcall;
var
  RemoteStr: UnicodeString;
  IconPath: AnsiString;
begin
  Result := FS_ICON_USEDEFAULT;
  RemoteStr := RemoteName;

  // Check if at root level - show rclone icon for all remotes
  if (ExtractFileDir(RemoteStr) = PathDelim) then
  begin
    IconPath := gStartupInfo.PluginDir + 'rclone.ico';

    if FileExists(IconPath) then
    begin
      Result := FS_ICON_EXTRACTED;
      TheIcon^.Format := FS_ICON_FORMAT_FILE;
      StrPLCopy(RemoteName, UTF8ToWide(IconPath), MAX_PATH - 1);
    end;
  end;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;
begin
  gStartupInfo := StartupInfo^;
end;

initialization
  gRcloneCli := nil;

finalization
  if Assigned(gRcloneCli) then
    FreeAndNil(gRcloneCli);

end.

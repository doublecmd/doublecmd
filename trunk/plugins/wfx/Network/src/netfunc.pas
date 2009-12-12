unit NetFunc;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Windows, ShellApi, SysUtils, Classes, NetUtils, WfxPlugin;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; stdcall;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; stdcall;
function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): BOOL; stdcall;
function FsFindClose(Hdl: THandle): Integer; stdcall;

function FsExecuteFile(MainWin: HWND; RemoteName, Verb: PAnsiChar): Integer; stdcall;
function FsRenMovFile(OldName, NewName: PAnsiChar; Move, OverWrite: BOOL; RemoteInfo: PRemoteInfo): Integer; stdcall;
function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer; RemoteInfo: PRemoteInfo): Integer; stdcall;
function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: Integer): Integer; stdcall;
function FsDeleteFile(RemoteName: PAnsiChar): BOOL; stdcall;

function FsMkDir(RemoteDir: PAnsiChar): BOOL; stdcall;
function FsRemoveDir(RemoteName: PAnsiChar): BOOL; stdcall;
function FsSetAttr(RemoteName: PAnsiChar; NewAttr: Integer): BOOL; stdcall;
function FsSetTime(RemoteName: PAnsiChar; CreationTime, LastAccessTime, LastWriteTime: PFileTime): BOOL; stdcall;

function FsLinksToLocalFiles: BOOL; stdcall;
function FsGetLocalName(RemoteName: PAnsiChar; MaxLen: Integer): BOOL; stdcall;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;

implementation

var
  ProgressProc: TProgressProc;
  LogProc: TLogProc;
  RequestProc: TRequestProc;
  PluginNumber: Integer;

type
  TListRec = record
    List:   TList;
    Index:  Integer;
    Handle: THandle;
  end;
  PListRec = ^TListRec;

type
  TProgressInfo = record
    SourceName,
    TargetName: PAnsiChar;
  end;
  PProgressInfo = ^TProgressInfo;

{$IFDEF FPC}
const
  kernel32 = 'kernel32.dll';

function MoveFileWithProgress(lpExistingFileName, lpNewFileName: PAnsiChar; lpProgressRoutine: Pointer; lpData: Pointer; dwFlags: DWORD): BOOL; stdcall; external kernel32 Name 'MoveFileWithProgressA';
function CopyFileEx(lpExistingFileName, lpNewFileName: PAnsiChar; lpProgressRoutine: Pointer; lpData: Pointer; pbCancel: PBool; dwCopyFlags: DWORD): BOOL; stdcall; external kernel32 Name 'CopyFileExA';
{$ELSE}
const
  AllowDirectorySeparators: set of char = ['\', '/'];

Function GetDirs(Var DirName: String; Var Dirs: Array of PAnsiChar): Longint;
Var
  I: Longint;
begin
  I := 1;
  Result := -1;
  While I <= Length(DirName) do
  begin
    If (DirName[i] in AllowDirectorySeparators) and
      { avoid error in case last char=pathdelim }
      (length(dirname) > i) then
    begin
      DirName[i] := #0;
      Inc(Result);
      Dirs[Result] := @DirName[I + 1];
    end;
    Inc(I);
  end;
  If Result > -1 then
    Inc(Result);
end;

{$ENDIF}

function ExcludeTrailingPathDelimiters(Path: String): String;
var
  iLength: Integer;
begin
  if Path[1] = PathDelim then
    if (Length(Path) > 1) and (Path[2] = PathDelim) then
      Delete(Path, 1, 2)
    else
      Delete(Path, 1, 1);
  iLength := Length(Path);
  if Path[iLength] = PathDelim then
    Delete(Path, iLength, 1);
  Result := Path;
end;

function ExtractNetworkFileName(const FileName: String): String;
var
  I, iCount: Integer;
begin
  Result := FileName;
  for I := 0 to 2 do
  begin
    iCount := Pos(PathDelim, Result);
    System.Delete(Result, 1, iCount);
  end;
  Result := '\\' + Result;
end;

function FsGetLastError: LongInt;
var
  err: Integer;
begin
  err := GetLastError();
  case (err) of
    ERROR_FILE_NOT_FOUND,
    ERROR_PATH_NOT_FOUND,
    ERROR_TOO_MANY_OPEN_FILES,
    ERROR_ACCESS_DENIED:
      Result := FS_FILE_NOTFOUND;
    ERROR_FILE_EXISTS:
      Result := FS_FILE_EXISTS;
    ERROR_REQUEST_ABORTED:
      Result := FS_FILE_USERABORT;
  else
    Result := FS_FILE_READERROR;
  end; // case
end;

function CopyProgressRoutine(TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: Int64; dwStreamNumber, dwCallbackReason: DWORD; hSourceFile, hDestinationFile: THandle; lpData: Pointer): DWORD; stdcall;
var
  Percent: Integer;
  ProgressInfo: PProgressInfo;
begin
  if TotalFileSize = 0 then
    Percent := 100
  else
    Percent := TotalBytesTransferred * 100 div TotalFileSize;
  ProgressInfo := PProgressInfo(lpData);
  Result := ProgressProc(PluginNumber, ProgressInfo^.SourceName, ProgressInfo^.TargetName, Percent);
end;

function FsInit(PluginNr: Integer; pProgressProc: tProgressProc; pLogProc: tLogProc; pRequestProc: tRequestProc): Integer; stdcall;
begin
  ProgressProc := pProgressProc;
  LogProc := pLogProc;
  RequestProc := pRequestProc;
  PluginNumber := PluginNr;
  Result := 0;
end;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; stdcall;
var
  ListRec: PListRec;
  I, iCount: Integer;
  bFound: Boolean;
  sDirName: AnsiString;
  Dirs: Array[0..127] of PAnsiChar;
  NetRes: TNetRes;
begin
  New(ListRec);
  ListRec^.List:= nil;
  ListRec^.Index := 0;
  ListRec^.Handle := INVALID_HANDLE_VALUE;
  FillChar(FindData, SizeOf(FindData), #0);
  try
    // root directory
    if Path = PathDelim then
    begin
      ListRec^.List := GetContainerList(nil); // get list of networks
      FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
      StrPCopy(FindData.cFileName, PNetRes(ListRec^.List[0])^.RemoteName);
    end
    else
    begin
      bFound := False;
      sDirName := StrPas(Path);

      iCount := GetDirs(sDirName, Dirs);
      if iCount = 0 then
        begin
          Result:= wfxInvalidHandle;
          Exit;
        end;
      // open shared folder
      if iCount > 3 then
      begin
        sDirName := ExtractNetworkFileName(Path) + PathDelim + '*';
        ListRec^.Handle := FindFirstFile(PAnsiChar(sDirName), FindData);
        Result := THandle(ListRec);
        Exit;
      end;

      sDirName := Dirs[0];
      ListRec^.List := GetContainerList(nil); // get list of networks
      for I := 0 to ListRec^.List.Count - 1 do
      begin
        // open subnet
        if PNetRes(ListRec^.List[i])^.RemoteName = sDirName then
        begin
          NetRes := PNetRes(ListRec^.List[I])^;
          FreeNetResList(ListRec^.List);
          ListRec^.List := GetContainerList(@NetRes);
          bFound := True;
          Break;
        end;
      end;
      // open domen
      if bFound and Assigned(ListRec^.List) and (iCount > 1) then
      begin
        bFound := False;
        sDirName := Dirs[1];
        for I := 0 to ListRec^.List.Count - 1 do
        begin
          if PNetRes(ListRec^.List[i])^.RemoteName = sDirName then
          begin
            NetRes := PNetRes(ListRec^.List[I])^;
            FreeNetResList(ListRec^.List);
            ListRec^.List := GetContainerList(@NetRes);
            bFound := True;
            Break;
          end;
        end;
        // open computer
        if bFound and Assigned(ListRec^.List) and (iCount = 3) then
        begin
          bFound := False;
          sDirName := Dirs[2];
          for I := 0 to ListRec^.List.Count - 1 do
          begin
            if PNetRes(ListRec^.List[i])^.RemoteName = '\\' + sDirName then
            begin
              NetRes := PNetRes(ListRec^.List[I])^;
              FreeNetResList(ListRec^.List);
              ListRec^.List := GetContainerList(@NetRes);
              bFound := True;
              Break;
            end;
          end; // for

          if not Assigned(ListRec^.List) then
          begin
            Result := wfxInvalidHandle;
            Exit;
          end;
        end; // openPC
      end;   // open Domen

      FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
      StrPCopy(FindData.cFileName, ExtractFileName(ExcludeTrailingPathDelimiters(PNetRes(ListRec^.List[0])^.RemoteName)));
    end; // PathDelim

    Result:= THandle(ListRec);
  except
    Result:= wfxInvalidHandle;
  end;
end;

function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): bool; stdcall;
var
  ListRec: PListRec;
begin
  Result := False;
  ListRec := PListRec(Pointer(Hdl));

  FillChar(FindData, SizeOf(FindData), #0);
  if ListRec^.Handle <> INVALID_HANDLE_VALUE then
    Result := FindNextFile(ListRec^.Handle, FindData)
  else if Assigned(ListRec^.List) and (ListRec^.Index < ListRec^.List.Count - 1) then
  begin
    Inc(ListRec^.Index);
    // if PNetRes(ListRec^.List[ListRec^.Index])^.dwDisplayType  = RESOURCEDISPLAYTYPE_DOMAIN then
    FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
    //else
    //FindData.dwFileAttributes:= 0;

    StrPCopy(FindData.cFileName, ExtractFileName(ExcludeTrailingPathDelimiters(PNetRes(ListRec^.List[ListRec^.Index])^.RemoteName)));

    Result := True;
  end;
end;

function FsFindClose(Hdl: THandle): Integer; stdcall;
var
  ListRec: PListRec;
begin
  ListRec := PListRec(Pointer(Hdl));
  if ListRec^.Handle <> INVALID_HANDLE_VALUE then
    Result := Integer(Windows.FindClose(ListRec^.Handle));
  if Assigned(ListRec^.List) then
    FreeNetResList(ListRec^.List);
  Dispose(ListRec);
end;

function FsExecuteFile(MainWin: HWND; RemoteName, Verb: PAnsiChar): Integer; stdcall;
var
  sFileName: AnsiString;
begin
  Result := FS_EXEC_ERROR;
  sFileName:= ExtractNetworkFileName(RemoteName);
  if sFileName = EmptyStr then Exit;
  RemoteName:= PAnsiChar(sFileName);
  if ShellExecute(MainWin, Verb, RemoteName, nil, nil, SW_SHOW) > 32 then
    Result:= FS_EXEC_OK;
end;

function FsRenMovFile(OldName, NewName: PAnsiChar; Move, OverWrite: BOOL;
                      RemoteInfo: PRemoteInfo): Integer; stdcall;
var
  err: Integer;
  ok: Boolean;
  sOldName,
  sNewName: AnsiString;
  ProgressInfo: TProgressInfo;
begin
  sOldName:= ExtractNetworkFileName(OldName);
  sNewName:= ExtractNetworkFileName(NewName);
  if (sOldName = EmptyStr) or (sNewName = EmptyStr) then
  begin
    Result := FS_FILE_NOTFOUND;
    Exit;
  end;
  OldName := PAnsiChar(sOldName);
  NewName := PAnsiChar(sNewName);
  err := ProgressProc(PluginNumber, OldName, NewName, 0);
  if (err = 1) then
    Result := FS_FILE_USERABORT;

  ProgressInfo.SourceName := OldName;
  ProgressInfo.TargetName := NewName;

  if OverWrite then
    DeleteFile(NewName);
  if Move then
    ok := MoveFileWithProgress(OldName, NewName, @CopyProgressRoutine, @ProgressInfo, MOVEFILE_COPY_ALLOWED)
  else
    ok := CopyFileEx(OldName, NewName, @CopyProgressRoutine, @ProgressInfo, nil, 0);

  if ok then
    begin
      ProgressProc(PluginNumber, OldName, NewName, 100);
      Result := FS_FILE_OK;
    end
  else
    begin
      Result:= FsGetLastError;
    end;
end;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer; RemoteInfo: PRemoteInfo): Integer; stdcall;
var
  err: Integer;
  ok, OverWrite, Resume, Move: Boolean;
  sRemoteName: String;
  ProgressInfo: TProgressInfo;
begin
  OverWrite := (CopyFlags and FS_COPYFLAGS_OVERWRITE) = 1;
  Resume := (CopyFlags and FS_COPYFLAGS_RESUME) = 1;
  Move := (CopyFlags and FS_COPYFLAGS_MOVE) = 1;

  if (Resume) then
    Result := FS_FILE_NOTSUPPORTED;
  sRemoteName := ExtractNetworkFileName(RemoteName);
  if sRemoteName = EmptyStr then
  begin
    Result := FS_FILE_NOTFOUND;
    Exit;
  end;
  RemoteName := PAnsiChar(sRemoteName);
  err := ProgressProc(PluginNumber, RemoteName, LocalName, 0);
  if (err = 1) then
    Result := FS_FILE_USERABORT;

  ProgressInfo.SourceName := RemoteName;
  ProgressInfo.TargetName := LocalName;

  if OverWrite then
    DeleteFile(LocalName);
  if Move then
    ok := MoveFileWithProgress(RemoteName, LocalName, @CopyProgressRoutine, @ProgressInfo, MOVEFILE_COPY_ALLOWED)
  else
    ok := CopyFileEx(RemoteName, LocalName, @CopyProgressRoutine, @ProgressInfo, nil, 0);

  if ok then
    begin
      ProgressProc(PluginNumber, RemoteName, LocalName, 100);
      Result := FS_FILE_OK;
    end
  else
    begin
      Result:= FsGetLastError;
    end;
end;

function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: Integer): Integer; stdcall;
var
  err: Integer;
  ok, OverWrite, Resume, Move: Boolean;
  sRemoteName: String;
  ProgressInfo: TProgressInfo;
begin
  OverWrite := (CopyFlags and FS_COPYFLAGS_OVERWRITE) = 1;
  Resume := (CopyFlags and FS_COPYFLAGS_RESUME) = 1;
  Move := (CopyFlags and FS_COPYFLAGS_MOVE) = 1;

  if (Resume) then
    Result := FS_FILE_NOTSUPPORTED;
  sRemoteName := ExtractNetworkFileName(RemoteName);
  if sRemoteName = EmptyStr then
  begin
    Result := FS_FILE_NOTFOUND;
    Exit;
  end;
  RemoteName := PAnsiChar(sRemoteName);
  err := ProgressProc(PluginNumber, LocalName, RemoteName, 0);
  if (err = 1) then
    Result := FS_FILE_USERABORT;

  ProgressInfo.SourceName := LocalName;
  ProgressInfo.TargetName := RemoteName;

  if OverWrite then
    DeleteFile(RemoteName);
  if Move then
    ok := MoveFileWithProgress(LocalName, RemoteName, @CopyProgressRoutine, @ProgressInfo, MOVEFILE_COPY_ALLOWED)
  else
    ok := CopyFileEx(LocalName, RemoteName, @CopyProgressRoutine, @ProgressInfo, nil, 0);

  if ok then
    begin
      ProgressProc(PluginNumber, LocalName, RemoteName, 100);
      Result := FS_FILE_OK;
    end
  else
    begin
      Result:= FsGetLastError;
    end;
end;

function FsDeleteFile(RemoteName: PAnsiChar): BOOL; stdcall;
var
  sFileName: String;
begin
  sFileName := ExtractNetworkFileName(RemoteName);
  Result := DeleteFile(PAnsiChar(sFileName));
end;

function FsMkDir(RemoteDir: PAnsiChar): BOOL; stdcall;
var
  sDirName: String;
begin
  sDirName := ExtractNetworkFileName(RemoteDir);
  Result := CreateDirectory(PAnsiChar(sDirName), nil);
end;

function FsRemoveDir(RemoteName: PAnsiChar): BOOL; stdcall;
var
  sDirName: String;
begin
  sDirName := ExtractNetworkFileName(RemoteName);
  Result := RemoveDirectory(PAnsiChar(sDirName));
end;

function FsSetAttr(RemoteName: PAnsiChar; NewAttr: Integer): BOOL; stdcall;
var
  sFileName: String;
begin
  sFileName := ExtractNetworkFileName(RemoteName);
  Result := SetFileAttributes(PAnsiChar(sFileName), NewAttr);
end;

function FsSetTime(RemoteName: PAnsiChar; CreationTime, LastAccessTime,
                   LastWriteTime: PFileTime): BOOL; stdcall;
var
  sFileName: String;
  hFile: THandle;
begin
  sFileName := ExtractNetworkFileName(RemoteName);
  hFile:= CreateFile(PAnsiChar(sFileName),
                      GENERIC_WRITE,          // Open for writing
                      0,                      // Do not share
                      nil,                   // No security
                      OPEN_EXISTING,          // Existing file only
                      FILE_ATTRIBUTE_NORMAL,  // Normal file
                      0);
  if hFile = feInvalidHandle then Exit(False);

  Result := SetFileTime(hFile, CreationTime, LastAccessTime, LastWriteTime);
  CloseHandle(hFile);
end;

function FsLinksToLocalFiles: BOOL; stdcall;
begin
  Result:= True;
end;

function FsGetLocalName(RemoteName: PAnsiChar; MaxLen: Integer): BOOL; stdcall;
var
  sFileName: String;
begin
  Result:= False;
  sFileName := ExtractNetworkFileName(RemoteName);
  if sFileName = EmptyStr then Exit;
  StrPLCopy(RemoteName, sFileName, MaxLen);
  Result:= True;
end;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrPLCopy(DefRootName, 'Network', MaxLen);
end;

end.


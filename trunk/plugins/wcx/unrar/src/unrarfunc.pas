{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for unpacking RAR archives
   This is simple wrapper for unrar.dll or libunrar.so

   Copyright (C) 2008-2011  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit UnRARFunc;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  WcxPlugin, Extension;

const
  {$IF DEFINED(MSWINDOWS)}
  // libunrar must be built with sizeof(wchar_t) = 2 (default on Windows)
  _unrar = 'unrar.dll';
  {$ELSEIF DEFINED(DARWIN)}
  // libunrar must be built with sizeof(wchar_t) = 4 (default on Unix)
  _unrar = 'libunrar.dylib';
  {$ELSEIF DEFINED(UNIX)}
  // libunrar must be built with sizeof(wchar_t) = 4 (default on Unix)
  _unrar = 'libunrar.so';
  {$ENDIF}

const
  // Unrar callback messages.
  UCM_CHANGEVOLUME    =  0;
  UCM_PROCESSDATA     =  1;
  UCM_NEEDPASSWORD    =  2;

  // Main header flags.
  MHD_VOLUME         = $0001;
  MHD_COMMENT        = $0002;
  MHD_LOCK           = $0004;
  MHD_SOLID          = $0008;
  MHD_PACK_COMMENT   = $0010;
  MHD_NEWNUMBERING   = $0010;
  MHD_AV             = $0020;  // (archive signed)
  MHD_PROTECT        = $0040;
  MHD_PASSWORD       = $0080;
  MHD_FIRSTVOLUME    = $0100;
  MHD_ENCRYPTVER     = $0200;

type

{$IFDEF UNIX}
  TRarUnicodeChar = UCS4Char;
  TRarUnicodeString = UCS4String;
{$ENDIF}

{$IFDEF WINDOWS}
  TRarUnicodeChar = WideChar;         // assuming 2 byte WideChar
  TRarUnicodeString = WideString;
{$ENDIF}

  PRarUnicodeChar = ^TRarUnicodeChar;
  TRarUnicodeArray = packed array [0..1023] of TRarUnicodeChar;

  RARHeaderData = packed record
    ArcName: packed array[0..259] of Char;
    FileName: packed array[0..259] of Char; // a zero terminated string of the file name in OEM (DOS) encoding.
    Flags: LongWord;
    PackSize: LongWord;
    UnpSize: LongWord;
    HostOS: LongWord;
    FileCRC: LongWord;
    FileTime: LongWord;
    UnpVer: LongWord;
    Method: LongWord;
    FileAttr: LongWord;
    CmtBuf: PChar;
    CmtBufSize: LongWord;
    CmtSize: LongWord;
    CmtState: LongWord;
  end;

  RARHeaderDataEx = packed record
    ArcName: packed array [0..1023] of Char;
    ArcNameW: TRarUnicodeArray;
    FileName: packed array [0..1023] of Char;
    FileNameW: TRarUnicodeArray;
    Flags: LongWord;
    PackSize: LongWord;
    PackSizeHigh: LongWord;
    UnpSize: LongWord;
    UnpSizeHigh: LongWord;
    HostOS: LongWord;
    FileCRC: LongWord;
    FileTime: LongWord;
    UnpVer: LongWord;
    Method: LongWord;
    FileAttr: LongWord;
    CmtBuf: PChar;
    CmtBufSize: LongWord;
    CmtSize: LongWord;
    CmtState: LongWord;
    Reserved: packed array [0..1023] of LongWord;
  end;

  RAROpenArchiveData = packed record
    ArcName: PChar;
    OpenMode: LongWord;
    OpenResult: LongWord;
    CmtBuf: PChar;
    CmtBufSize: LongWord;
    CmtSize: LongWord;
    CmtState: LongWord;
  end;

  RAROpenArchiveDataEx = packed record
    ArcName: PChar;
    ArcNameW: PRarUnicodeChar;
    OpenMode: LongWord;
    OpenResult: LongWord;
    CmtBuf: PChar;
    CmtBufSize: LongWord;
    CmtSize: LongWord;
    CmtState: LongWord;
    Flags: LongWord;
    Reserved: packed array [0..31] of LongWord;
  end;

  {$IFDEF MSWINDOWS}{$CALLING STDCALL}{$ELSE}{$CALLING CDECL}{$ENDIF}

  TUnrarCallback = function(Msg: LongWord; UserData, P1, P2: PtrInt) : Integer;
  TUnrarChangeVolProc = function(ArcName: PChar; Mode: Integer): Integer;
  TUnrarProcessDataProc = function(BufAddr: Pointer; BufSize: Integer): Integer;

  TRAROpenArchive = function(var ArchiveData: RAROpenArchiveData) : TArcHandle;
  TRAROpenArchiveEx = function(var ArchiveData: RAROpenArchiveDataEx) : TArcHandle;
  TRARCloseArchive = function(hArcData: TArcHandle) : Integer;
  TRARReadHeader = function(hArcData: TArcHandle; var HeaderData: RARHeaderData) : Integer;
  TRARReadHeaderEx = function (hArcData: TArcHandle; var HeaderData: RARHeaderDataEx) : Integer;
  TRARProcessFile = function(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PAnsiChar) : Integer;
  TRARProcessFileW = function(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PRarUnicodeChar) : Integer;
  TRARSetCallback = procedure(hArcData: TArcHandle; UnrarCallback: TUnrarCallback; UserData: PtrInt);
  TRARSetChangeVolProc = procedure(hArcData: TArcHandle; ChangeVolProc: TUnrarChangeVolProc);
  TRARSetProcessDataProc = procedure(hArcData: TArcHandle; ProcessDataProc: TUnrarProcessDataProc);
  TRARSetPassword = procedure(hArcData: TArcHandle; Password: PChar);
  TRARGetDllVersion = function: Integer;

  {$CALLING DEFAULT}

var
  RAROpenArchive : TRAROpenArchive = nil;
  RAROpenArchiveEx : TRAROpenArchiveEx = nil;
  RARCloseArchive : TRARCloseArchive = nil;
  RARReadHeader : TRARReadHeader = nil;
  RARReadHeaderEx : TRARReadHeaderEx = nil;
  RARProcessFile : TRARProcessFile = nil;
  RARProcessFileW : TRARProcessFileW = nil;
  RARSetCallback : TRARSetCallback = nil;
  RARSetChangeVolProc : TRARSetChangeVolProc = nil;
  RARSetProcessDataProc : TRARSetProcessDataProc = nil;
  RARSetPassword : TRARSetPassword = nil;
  RARGetDllVersion : TRARGetDllVersion = nil;

  ModuleHandle : THandle = 0;

function OpenArchive(var ArchiveData: TOpenArchiveData) : TArcHandle;dcpcall;
function OpenArchiveW(var ArchiveData: tOpenArchiveDataW) : TArcHandle;dcpcall;
function ReadHeader(hArcData: TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall;
function ReadHeaderEx(hArcData: TArcHandle; var HeaderData: THeaderDataEx) : Integer;dcpcall;
function ReadHeaderExW(hArcData: TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall;
function ProcessFile(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar) : Integer;dcpcall;
function ProcessFileW(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PWideChar) : Integer;dcpcall;
function CloseArchive(hArcData: TArcHandle): Integer;dcpcall;
procedure SetChangeVolProc(hArcData : TArcHandle; pChangeVolProc : TChangeVolProc);dcpcall;
procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall;
procedure SetProcessDataProc(hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall;
procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall;
function GetPackerCaps : Integer; dcpcall;
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;

implementation

uses
  DynLibs,
  DCBasicTypes, DCDateTimeUtils, DCConvertEncoding, DCFileAttributes;

type
  // From libunrar (dll.hpp)
  RarHostSystem = (
    HOST_MSDOS  = 0,
    HOST_OS2    = 1,
    HOST_WIN32  = 2,
    HOST_UNIX   = 3,
    HOST_MACOS  = 4,
    HOST_BEOS   = 5,
    HOST_MAX
  );

var
  ChangeVolProc : TChangeVolProc = nil;
  ChangeVolProcW : TChangeVolProcW = nil;
  ProcessDataProc : TProcessDataProc = nil;
  ProcessDataProcW : TProcessDataProcW = nil;

  // These variables store currently processed file name.
  // They cannot be dynamic strings, because if they are created from the
  // main thread of the calling program and then they're freed from another
  // thread of the calling program there's a crash.
  // It is because currently the library can only be statically linked with RTL
  // and so it doesn't know about the main program's multithreading.
  ProcessedFileName:  array [0..1023] of Char;
  ProcessedFileNameW: array [0..1023] of WideChar;
  ProcessedFileHostOS: RarHostSystem;
  ExtensionStartupInfo: TExtensionStartupInfo;

procedure StringToArrayA(src: AnsiString;
                         pDst: PAnsiChar;
                         MaxDstLength: Integer);
begin
  if Length(src) < MaxDstLength then
    MaxDstLength := Length(src)
  else
    MaxDstLength := MaxDstLength - 1; // for ending #0

  if Length(src) > 0 then
    Move(src[1], pDst^, SizeOf(AnsiChar) * MaxDstLength);
  pDst[MaxDstLength] := AnsiChar(0);
end;

procedure StringToArrayW(src: WideString;
                         pDst: PWideChar;
                         MaxDstLength: Integer);
begin
  if Length(src) < MaxDstLength then
    MaxDstLength := Length(src)
  else
    MaxDstLength := MaxDstLength - 1; // for ending #0

  if Length(src) > 0 then
    Move(src[1], pDst^, SizeOf(WideChar) * MaxDstLength);
  pDst[MaxDstLength] := WideChar(0);
end;

function RarUnicodeStringToWideString(src: TRarUnicodeString): WideString;
begin
{$IFDEF UNIX}
  Result := UCS4StringToWideString(src);
{$ELSE}
  Result := src;
{$ENDIF}
end;

function WideStringToRarUnicodeString(src: WideString): TRarUnicodeString;
begin
{$IFDEF UNIX}
  Result := WideStringToUCS4String(src);
{$ELSE}
  Result := src;
{$ENDIF}
end;

function GetSystemSpecificFileName(HostOS: RarHostSystem; FileName: AnsiString) : AnsiString;
begin
  Result:= FileName;
  if HostOS in [HOST_MSDOS, HOST_WIN32] then
  begin
    Result:= OEMToSys(Result);
  end;
  {$IFDEF MSWINDOWS}
  if HostOS in [HOST_UNIX, HOST_MACOS] then
  begin
    Result:= UTF8ToAnsi(Result);
  end;
  {$ENDIF}
end;

function SetSystemSpecificFileName(HostOS: RarHostSystem; FileName: AnsiString) : AnsiString;
begin
  Result:= FileName;
{$IFDEF MSWINDOWS}
  if HostOS in [HOST_MSDOS, HOST_WIN32] then
  begin
    Result:= SysToOEM(Result);
  end;
  if HostOS in [HOST_UNIX, HOST_MACOS] then
  begin
    Result:= SysToOEM(Result);
  end;
{$ENDIF}
end;

function GetSystemSpecificFileTime(HostOS: RarHostSystem; FileTime: LongInt) : LongInt;
var
  DateTime: TDateTime;
begin
  Result := FileTime;

{$IFDEF MSWINDOWS}
  if (HostOS = HOST_UNIX) then
  begin
    DateTime := UnixFileTimeToDateTime(Result);
    Result   := LongInt(DateTimeToDosFileTime(DateTime));
  end;
{$ENDIF}

{$IFDEF UNIX}
  if HostOS in [HOST_MSDOS, HOST_WIN32] then
  begin
    DateTime := DosFileTimeToDateTime(TDosFileTime(Result));
    Result   := LongInt(DateTimeToUnixFileTime(DateTime));
  end;
{$ENDIF}
end;

function GetSystemSpecificAttributes(HostOS: RarHostSystem; Attrs: LongInt): LongInt;
begin
  Result := Attrs;

{$IFDEF MSWINDOWS}
  if (HostOS = HOST_UNIX) or
     // Ugly hack: $1FFFF is max value of attributes on Windows
     (Result > $1FFFF) then
  begin
    Result := LongInt(UnixToWinFileAttr(TFileAttrs(Attrs)));
  end;
{$ENDIF}
{$IFDEF UNIX}
  if HostOS in [HOST_MSDOS, HOST_WIN32] then
    Result := LongInt(WinToUnixFileAttr(TFileAttrs(Result)));
{$ENDIF}
end;

function UnrarCallback(Msg: LongWord; UserData, P1, P2: PtrInt) : Integer; dcpcall;
begin
  Result := 0;
  case Msg of
  UCM_CHANGEVOLUME:
    begin
      if Assigned(ChangeVolProcW) then
      begin
        if ChangeVolProcW(PWideChar(UTF8Decode(AnsiToUtf8(PChar(P1)))), LongInt(P2)) = 0 then
          Result := -1;
      end
      else if Assigned(ChangeVolProc) then
      begin
        if ChangeVolProc(PChar(P1), LongInt(P2)) = 0 then
          Result := -1;
      end
    end;
  UCM_PROCESSDATA:
    begin
      // P1 - pointer to data buffer        (first param of ProcessDataProc)
      // P2 - number of bytes in the buffer (second param of ProcessDataProc)
      if Assigned(ProcessDataProcW) then
      begin
        if ProcessDataProcW(PWideChar(ProcessedFileNameW), LongInt(P2)) = 0 then
          Result := -1;
      end
      else if Assigned(ProcessDataProc) then
      begin
        if ProcessDataProc(PAnsiChar(ProcessedFileName), LongInt(P2)) = 0 then
          Result := -1;
      end;
    end;
  UCM_NEEDPASSWORD:
    begin
      // DLL needs a password to process archive. This message must be
      // processed if you wish to be able to handle encrypted archives.
      // Return zero or a positive value to continue process or -1
      // to cancel the archive operation.
      // P1 - contains the address pointing to the buffer for
      // a password in single byte encoding. You need to copy a password
      // here.
      // P2 - contains the size of password buffer.
      if not ExtensionStartupInfo.InputBox('Unrar', 'Please enter the password:', True, PAnsiChar(P1), P2) then
        Result := -1;
    end;
  end;
end;

function OpenArchive(var ArchiveData: TOpenArchiveData) : TArcHandle;dcpcall;
var
  RarArchiveData: RAROpenArchiveData;
begin
  if Assigned(RAROpenArchive) then
  begin
    RarArchiveData.ArcName    := ArchiveData.ArcName;
    RarArchiveData.OpenMode   := ArchiveData.OpenMode;
    RarArchiveData.CmtBuf     := ArchiveData.CmtBuf;
    RarArchiveData.CmtBufSize := ArchiveData.CmtBufSize;

    Result := RAROpenArchive(RarArchiveData);
    if Result <> 0 then
    begin
      ArchiveData.OpenResult := RarArchiveData.OpenResult;
      ArchiveData.CmtSize    := RarArchiveData.CmtSize;
      ArchiveData.CmtState   := RarArchiveData.CmtState;

      RARSetCallback(Result, @UnrarCallback, 0);
    end;
  end
  else
  begin
    ArchiveData.OpenResult := E_EOPEN;
    Result := 0;
  end;
end;

function OpenArchiveW(var ArchiveData: tOpenArchiveDataW) : TArcHandle;dcpcall;
var
  RarArchiveData: RAROpenArchiveDataEx;
  RarArcName: TRarUnicodeString;
begin
  if Assigned(RAROpenArchiveEx) then
  begin
    RarArcName := WideStringToRarUnicodeString(ArchiveData.ArcName);

    RarArchiveData.ArcName    := nil;
    RarArchiveData.ArcNameW   := PRarUnicodeChar(RarArcName);
    RarArchiveData.OpenMode   := ArchiveData.OpenMode;
    RarArchiveData.CmtBuf     := nil;
    RarArchiveData.CmtBufSize := 0;

    Result := RAROpenArchiveEx(RarArchiveData);
    if Result <> 0 then
    begin
      ArchiveData.OpenResult := RarArchiveData.OpenResult;
      ArchiveData.CmtSize    := RarArchiveData.CmtSize;
      ArchiveData.CmtState   := RarArchiveData.CmtState;

      RARSetCallback(Result, @UnrarCallback, 0);
    end;
  end
  else
  begin
    ArchiveData.OpenResult := E_EOPEN;
    Result := 0;
  end;
end;

function ReadHeader(hArcData: TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall;
var
  RarHeader: RARHeaderData;
begin
  if Assigned(RARReadHeader) then
    begin
      FillChar(RarHeader, SizeOf(RarHeader), 0);

      RarHeader.CmtBuf      := HeaderData.CmtBuf;
      RarHeader.CmtBufSize  := HeaderData.CmtBufSize;

      Result := RARReadHeader(hArcData, RarHeader);

{$PUSH}
{$Q-}
{$R-}
      HeaderData.ArcName    := RarHeader.ArcName;

      StringToArrayA(
                     GetSystemSpecificFileName(RarHostSystem(RarHeader.HostOS),
                                               AnsiString(RarHeader.FileName)),
                     @HeaderData.FileName, SizeOf(HeaderData.FileName)
                     );

      HeaderData.Flags      := RarHeader.Flags;
      HeaderData.PackSize   := RarHeader.PackSize;
      HeaderData.UnpSize    := RarHeader.UnpSize;
      HeaderData.HostOS     := RarHeader.HostOS;
      HeaderData.FileCRC    := RarHeader.FileCRC;
      HeaderData.FileTime   := RarHeader.FileTime;
      HeaderData.UnpVer     := RarHeader.UnpVer;
      HeaderData.Method     := RarHeader.Method;
      HeaderData.FileAttr   := RarHeader.FileAttr;
      HeaderData.CmtSize    := RarHeader.CmtSize;
      HeaderData.CmtState   := RarHeader.CmtState;

      HeaderData.FileAttr :=
          GetSystemSpecificAttributes(RarHostSystem(HeaderData.HostOS),
                                      HeaderData.FileAttr);
      HeaderData.FileTime :=
          GetSystemSpecificFileTime(RarHostSystem(HeaderData.HostOS),
                                    HeaderData.FileTime);
{$POP}
      Move(HeaderData.FileName, ProcessedFileName, SizeOf(HeaderData.FileName));
      ProcessedFileNameW := '';
      ProcessedFileHostOS:= RarHostSystem(HeaderData.HostOS);
    end
  else
    Result := E_EREAD;
end;

function ReadHeaderEx(hArcData: TArcHandle; var HeaderData: THeaderDataEx) : Integer;dcpcall;
var
  RarHeader: RARHeaderDataEx;
begin
  if Assigned(RARReadHeaderEx) then
    begin
      FillChar(RarHeader, SizeOf(RarHeader), 0);

      RarHeader.CmtBuf      := HeaderData.CmtBuf;
      RarHeader.CmtBufSize  := HeaderData.CmtBufSize;

      Result := RARReadHeaderEx(hArcData, RarHeader);

{$PUSH}
{$Q-}
{$R-}
      HeaderData.ArcName      := RarHeader.ArcName;

      StringToArrayA(
                     GetSystemSpecificFileName(RarHostSystem(RarHeader.HostOS),
                                               AnsiString(RarHeader.FileName)),
                     @HeaderData.FileName, SizeOf(HeaderData.FileName)
                     );

      HeaderData.Flags        := RarHeader.Flags;
      HeaderData.PackSize     := RarHeader.PackSize;
      HeaderData.PackSizeHigh := RarHeader.PackSizeHigh;
      HeaderData.UnpSize      := RarHeader.UnpSize;
      HeaderData.UnpSizeHigh  := RarHeader.UnpSizeHigh;
      HeaderData.HostOS       := RarHeader.HostOS;
      HeaderData.FileCRC      := RarHeader.FileCRC;
      HeaderData.FileTime     := RarHeader.FileTime;
      HeaderData.UnpVer       := RarHeader.UnpVer;
      HeaderData.Method       := RarHeader.Method;
      HeaderData.FileAttr     := RarHeader.FileAttr;
      HeaderData.CmtSize      := RarHeader.CmtSize;
      HeaderData.CmtState     := RarHeader.CmtState;

      HeaderData.FileAttr :=
          GetSystemSpecificAttributes(RarHostSystem(HeaderData.HostOS),
                                      HeaderData.FileAttr);
      HeaderData.FileTime :=
          GetSystemSpecificFileTime(RarHostSystem(HeaderData.HostOS),
                                    HeaderData.FileTime);
{$POP}
      ProcessedFileName := HeaderData.FileName;
      ProcessedFileNameW := '';
      ProcessedFileHostOS:= RarHostSystem(HeaderData.HostOS);
    end
  else
    Result := E_EREAD;
end;

function ReadHeaderExW(hArcData: TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall;
var
  RarHeader: RARHeaderDataEx;
begin
  if Assigned(RARReadHeaderEx) then
    begin
      FillChar(RarHeader, SizeOf(RarHeader), 0);

      RarHeader.CmtBuf      := HeaderData.CmtBuf;
      RarHeader.CmtBufSize  := HeaderData.CmtBufSize;

      Result := RARReadHeaderEx(hArcData, RarHeader);

{$PUSH}
{$Q-}
{$R-}
      StringToArrayW(
          RarUnicodeStringToWideString(TRarUnicodeString(RarHeader.ArcNameW)),
          @HeaderData.ArcName, SizeOf(HeaderData.ArcName));

      StringToArrayW(
          RarUnicodeStringToWideString(TRarUnicodeString(RarHeader.FileNameW)),
          @HeaderData.FileName, SizeOf(HeaderData.FileName));

      HeaderData.Flags        := RarHeader.Flags;
      HeaderData.PackSize     := RarHeader.PackSize;
      HeaderData.PackSizeHigh := RarHeader.PackSizeHigh;
      HeaderData.UnpSize      := RarHeader.UnpSize;
      HeaderData.UnpSizeHigh  := RarHeader.UnpSizeHigh;
      HeaderData.HostOS       := RarHeader.HostOS;
      HeaderData.FileCRC      := RarHeader.FileCRC;
      HeaderData.FileTime     := RarHeader.FileTime;
      HeaderData.UnpVer       := RarHeader.UnpVer;
      HeaderData.Method       := RarHeader.Method;
      HeaderData.FileAttr     := RarHeader.FileAttr;
      HeaderData.CmtSize      := RarHeader.CmtSize;
      HeaderData.CmtState     := RarHeader.CmtState;

      HeaderData.FileAttr :=
          GetSystemSpecificAttributes(RarHostSystem(HeaderData.HostOS),
                                      HeaderData.FileAttr);
      HeaderData.FileTime :=
          GetSystemSpecificFileTime(RarHostSystem(HeaderData.HostOS),
                                    HeaderData.FileTime);
{$POP}
      ProcessedFileName := RarHeader.FileName;
      ProcessedFileNameW := HeaderData.FileName;
    end
  else
    Result := E_EREAD;
end;

function ProcessFile(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PChar) : Integer;dcpcall;
var
  pacDestPath: PAnsiChar = nil;
  pacDestName: PAnsiChar = nil;
  SysSpecDestPath, SysSpecDestName: AnsiString;
begin
  if Assigned(RARProcessFile) then
    begin
      // Both DestPath and DestName must be in OEM encoding
      // if HostOS is MS DOS or MS Windows and archive is open under MS Windows.
      if DestPath <> nil then
        begin
          SysSpecDestPath:= SetSystemSpecificFileName(ProcessedFileHostOS, DestPath);
          pacDestPath := PAnsiChar(SysSpecDestPath);
        end;
      if DestName <> nil then
        begin
          SysSpecDestName:= SetSystemSpecificFileName(ProcessedFileHostOS, DestName);
          pacDestName := PAnsiChar(SysSpecDestName);
        end;
      Result := RARProcessFile(hArcData, Operation, pacDestPath, pacDestName);
    end
  else
    Result := E_EREAD;
end;

function ProcessFileW(hArcData: TArcHandle; Operation: Integer; DestPath, DestName: PWideChar) : Integer;dcpcall;
var
  pwcDestPath: PRarUnicodeChar = nil;
  pwcDestName: PRarUnicodeChar = nil;
  SysSpecDestPath, SysSpecDestName: TRarUnicodeString;
begin
  if Assigned(RARProcessFileW) then
    begin
      if DestPath <> nil then
        begin
          SysSpecDestPath:= WideStringToRarUnicodeString(DestPath);
          pwcDestPath := PRarUnicodeChar(SysSpecDestPath);
        end;
      if DestName <> nil then
        begin
          SysSpecDestName:= WideStringToRarUnicodeString(DestName);
          pwcDestName := PRarUnicodeChar(SysSpecDestName);
        end;
      Result := RARProcessFileW(hArcData, Operation, pwcDestPath, pwcDestName);
    end
  else
    Result := E_EREAD;
end;

function CloseArchive(hArcData: TArcHandle) : Integer;dcpcall;
begin
  if Assigned(RARCloseArchive) then
    Result := RARCloseArchive(hArcData)
  else
    Result := E_ECLOSE;
end;

procedure SetChangeVolProc(hArcData : TArcHandle; pChangeVolProc : TChangeVolProc);dcpcall;
begin
  ChangeVolProc := pChangeVolProc;
end;

procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall;
begin
  ChangeVolProcW := pChangeVolProc;
end;

procedure SetProcessDataProc(hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall;
begin
  ProcessDataProc := pProcessDataProc;
end;

procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall;
begin
  ProcessDataProcW := pProcessDataProc;
end;

function GetPackerCaps: Integer; dcpcall;
begin
  Result := PK_CAPS_MULTIPLE or PK_CAPS_BY_CONTENT;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;
begin
  ExtensionStartupInfo := StartupInfo^;
end;

finalization
  if ModuleHandle <> 0 then
    UnloadLibrary(ModuleHandle);

end.


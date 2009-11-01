{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for unpacking RAR archives
   This is simple wrapper for unrar.dll or libunrar.so

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

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

interface

uses 
  WcxPlugin;

const
  {$IFDEF MSWINDOWS}
  _unrar = 'unrar.dll';
  {$ELSE UNIX}
  _unrar = 'libunrar.so';
  {$ENDIF}

const
  UCM_CHANGEVOLUME    =  0;
  UCM_PROCESSDATA     =  1;
  UCM_NEEDPASSWORD    =  2;

type
  RARHeaderData = packed record
    ArcName: packed array[0..259] of Char;
    FileName: packed array[0..259] of Char;
    Flags: LongInt;
    PackSize: LongInt;
    UnpSize: LongInt;
    HostOS: LongInt;
    FileCRC: LongInt;
    FileTime: LongInt;
    UnpVer: LongInt;
    Method: LongInt;
    FileAttr: LongInt;
    CmtBuf: PChar;
    CmtBufSize: LongInt;
    CmtSize: LongInt;
    CmtState: LongInt;
  end;

  RARHeaderDataEx = packed record
    ArcName: packed array [0..1023] of Char;
    ArcNameW: packed array [0..1023] of WideChar;
    FileName: packed array [0..1023] of Char;
    FileNameW: packed array [0..1023] of WideChar;
    Flags: LongInt;
    PackSize: LongInt;
    PackSizeHigh: LongInt;
    UnpSize: LongInt;
    UnpSizeHigh: LongInt;
    HostOS: LongInt;
    FileCRC: LongInt;
    FileTime: LongInt;
    UnpVer: LongInt;
    Method: LongInt;
    FileAttr: LongInt;
    CmtBuf: PChar;
    CmtBufSize: LongInt;
    CmtSize: LongInt;
    CmtState: LongInt;
    Reserved: packed array [0..1023] of LongInt;
  end;

  RAROpenArchiveData = packed record
    ArcName: PChar;
    OpenMode: LongInt;
    OpenResult: LongInt;
    CmtBuf: PChar;
    CmtBufSize: LongInt;
    CmtSize: LongInt;
    CmtState: LongInt;
  end;

  RAROpenArchiveDataEx = packed record
    ArcName: PChar;
    ArcNameW: PWideChar;
    OpenMode: LongInt;
    OpenResult: LongInt;
    CmtBuf: PChar;
    CmtBufSize: LongInt;
    CmtSize: LongInt;
    CmtState: LongInt;
    Flags: LongInt;
    Reserved: packed array [0..31] of LongInt;
  end;

  TUnrarCallback = function (Msg: LongInt; UserData, P1, P2: Integer) : Integer;{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  TRAROpenArchive = function(var ArchiveData: RAROpenArchiveData) : THandle;stdcall;
  TRAROpenArchiveEx = function(var ArchiveData: RAROpenArchiveDataEx) : THandle;stdcall;
  TRARCloseArchive = function(hArcData: THandle) : Integer;stdcall;
  TRARReadHeader = function(hArcData: THandle; var HeaderData: RARHeaderData) : Integer;stdcall;
  TRARReadHeaderEx = function (hArcData: THandle; var HeaderData: RARHeaderDataEx) : Integer;stdcall;
  TRARProcessFile = function(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar) : Integer;stdcall;
  TRARSetCallback = procedure(hArcData: THandle; UnrarCallback: TUnrarCallback; UserData: Integer);stdcall;
  TRARSetPassword = procedure(hArcData: THandle; Password: PChar);stdcall;

var
  RAROpenArchive : TRAROpenArchive = nil;
  RARReadHeader : TRARReadHeader = nil;
  RARProcessFile : TRARProcessFile = nil;
  RARCloseArchive : TRARCloseArchive = nil;
  RARSetCallback : TRARSetCallback = nil;

  ModuleHandle : THandle = 0;

function OpenArchive(var ArchiveData: RAROpenArchiveData) : THandle;stdcall;
function ReadHeader(hArcData: THandle; var HeaderData: RARHeaderData) : Integer;stdcall;
function ProcessFile(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar) : Integer;stdcall;
function CloseArchive(hArcData: THandle): Integer;stdcall;
procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc : TChangeVolProc);stdcall;
procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc : TProcessDataProc);stdcall;

implementation

uses
  DynLibs;

var
  ChangeVolProc : TChangeVolProc;
  ProcessDataProc : TProcessDataProc;
  gHeaderData : RARHeaderData;

function UnrarCallback(Msg: LongInt; UserData, P1, P2: Integer) : Integer;{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := 0;
  case Msg of
  UCM_CHANGEVOLUME:
    begin
      if ChangeVolProc(PChar(P1), P2) = 0 then
        Result := -1;
    end;
  UCM_PROCESSDATA:
    begin
      if ProcessDataProc(gHeaderData.FileName, P2) = 0 then
        Result := -1;
    end;
  end;
end;

function OpenArchive(var ArchiveData: RAROpenArchiveData) : THandle;stdcall;
begin
  if Assigned(RAROpenArchive) then
  begin
    Result := RAROpenArchive(ArchiveData);
    if Result <> 0 then
      RARSetCallback(Result, UnrarCallback, 0);
  end
  else
  begin
    ArchiveData.OpenResult := E_EOPEN;
    Result := 0;
  end;
end;

function ReadHeader(hArcData: THandle; var HeaderData: RARHeaderData) : Integer;stdcall;
begin
  if Assigned(RARReadHeader) then
    begin
      Result := RARReadHeader(hArcData, HeaderData);
      gHeaderData := HeaderData;
    end
  else
    Result := E_EOPEN;
end;

function ProcessFile(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar) : Integer;stdcall;
begin
  if Assigned(RARProcessFile) then
    Result := RARProcessFile(hArcData, Operation, DestPath, DestName)
  else
    Result := E_EOPEN;
end;

function CloseArchive(hArcData: THandle) : Integer;stdcall;
begin
  if Assigned(RARCloseArchive) then
    Result := RARCloseArchive(hArcData)
  else
    Result := E_EOPEN;
end;

procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc : TChangeVolProc);stdcall;
begin
  ChangeVolProc := pChangeVolProc;
end;

procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc : TProcessDataProc);stdcall;
begin
  ProcessDataProc := pProcessDataProc;
end;

finalization
  if ModuleHandle <> 0 then
    UnloadLibrary(ModuleHandle);

end.


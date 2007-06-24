unit uWCXprototypes;

{$mode objfpc}{$H+}

interface

uses
  {Classes, SysUtils,} uWCXhead;
type
{mandatory}
  TOpenArchive = function (var ArchiveData : tOpenArchiveData): THandle;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
  TReadHeader = function (hArcData: THandle; var HeaderData : THeaderData): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
  TProcessFile = function  (hArcData: THandle;  Operation: integer;  DestPath: pchar;  DestName: pchar): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
  TCloseArchive = function (hArcData: THandle): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
{Optional}
   TPackFiles = function (PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TDeleteFiles = function (PackedFile: pchar;  DeleteList: pchar): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TGetPackerCaps = function () : integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TConfigurePacker = procedure (Parent: THandle;  DllInstance: THandle);{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TSetChangeVolProc = procedure (hArcData: THandle;  pChangeVolProc1: tChangeVolProc);{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TSetProcessDataProc = procedure (hArcData: THandle;  pProcessDataProc: TProcessDataProc);{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TStartMemPack = function (Options: integer;  FileName: pchar): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TPackToMem = function (hMemPack: integer;  BufIn: pchar;  InLen: integer;  Taken: pinteger;  BufOut: pchar;  OutLen: integer;  Written: pinteger;  SeekBy: integer): integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
   TDoneMemPack = function (hMemPack: integer): integer;stdcall;
   TCanYouHandleThisFile = function (FileName: pchar): boolean;stdcall;
   TPackSetDefaultParams = procedure (dps: pPackDefaultParamStruct);stdcall;

implementation

end.


unit uWCXprototypes;

{$mode objfpc}{$H+}

interface

uses
  {Classes, SysUtils,} uWCXhead;
type
{mandatory}
  TOpenArchive = function (var ArchiveData : tOpenArchiveData): THandle;stdcall;
  TReadHeader = function (hArcData: THandle; var HeaderData : THeaderData): integer;stdcall;
  TProcessFile = function  (hArcData: THandle;  Operation: integer;  DestPath: pchar;  DestName: pchar): integer;stdcall;
  TCloseArchive = function (hArcData: THandle): integer;stdcall;
{Optional}
   TPackFiles = function (PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): integer;stdcall;
   TDeleteFiles = function (PackedFile: pchar;  DeleteList: pchar): integer;stdcall;
   TGetPackerCaps = function () : integer;stdcall;
   TConfigurePacker = procedure (Parent: THandle;  DllInstance: THandle);stdcall;
   TSetChangeVolProc = procedure (hArcData: THandle;  pChangeVolProc1: tChangeVolProc);stdcall;
   TSetProcessDataProc = procedure (hArcData: THandle;  pProcessDataProc: TProcessDataProc);stdcall;
   TStartMemPack = function (Options: integer;  FileName: pchar): integer;stdcall;
   TPackToMem = function (hMemPack: integer;  BufIn: pchar;  InLen: integer;  Taken: pinteger;  BufOut: pchar;  OutLen: integer;  Written: pinteger;  SeekBy: integer): integer;stdcall;
   TDoneMemPack = function (hMemPack: integer): integer;stdcall;
   TCanYouHandleThisFile = function (FileName: pchar): boolean;stdcall;
   TPackSetDefaultParams = procedure (dps: pPackDefaultParamStruct);stdcall;
   TReadHeaderEx = function (hArcData: THandle; var HeaderDataEx : THeaderDataEx): integer;stdcall;

implementation

end.


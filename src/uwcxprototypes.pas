unit uWCXprototypes;

{$mode objfpc}{$H+}

interface

uses
  LCLType, WcxPlugin;
type
{ Mandatory }
  TOpenArchive = function (var ArchiveData : tOpenArchiveData): TArcHandle;stdcall;
  TReadHeader = function (hArcData: TArcHandle; var HeaderData : THeaderData): integer;stdcall;
  TProcessFile = function  (hArcData: TArcHandle;  Operation: integer;  DestPath: pchar;  DestName: pchar): integer;stdcall;
  TCloseArchive = function (hArcData: TArcHandle): integer;stdcall;
{ Optional }
  TPackFiles = function (PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): integer;stdcall;
  TDeleteFiles = function (PackedFile: pchar;  DeleteList: pchar): integer;stdcall;
  TGetPackerCaps = function () : integer;stdcall;
  TConfigurePacker = procedure (Parent: HWND;  DllInstance: THandle);stdcall;
  TSetChangeVolProc = procedure (hArcData: TArcHandle;  ChangeVolProc: tChangeVolProc);stdcall;
  TSetProcessDataProc = procedure (hArcData: TArcHandle;  ProcessDataProc: TProcessDataProc);stdcall;
  TStartMemPack = function (Options: integer;  FileName: pchar): integer;stdcall;
  TPackToMem = function (hMemPack: integer;  BufIn: pchar;  InLen: integer;  Taken: pinteger;  BufOut: pchar;  OutLen: integer;  Written: pinteger;  SeekBy: integer): integer;stdcall;
  TDoneMemPack = function (hMemPack: integer): integer;stdcall;
  TCanYouHandleThisFile = function (FileName: pchar): boolean;stdcall;
  TPackSetDefaultParams = procedure (dps: pPackDefaultParamStruct);stdcall;
  TReadHeaderEx = function (hArcData: TArcHandle; var HeaderDataEx : THeaderDataEx): integer;stdcall;
  TPkSetCryptCallback = procedure (PkCryptProc: TPkCryptProc; CryptoNr, Flags: Integer); stdcall;
{ Unicode }
  TOpenArchiveW = function (var ArchiveData : tOpenArchiveDataW): TArcHandle;stdcall;
  TReadHeaderExW = function (hArcData: TArcHandle; var HeaderDataExW : THeaderDataExW): integer;stdcall;
  TProcessFileW = function  (hArcData: TArcHandle;  Operation: integer; DestPath, DestName: PWideChar): integer;stdcall;
  TSetChangeVolProcW = procedure (hArcData: TArcHandle; ChangeVolProc: tChangeVolProcW);stdcall;
  TSetProcessDataProcW = procedure (hArcData: TArcHandle; ProcessDataProc: TProcessDataProcW);stdcall;
  TPackFilesW = function (PackedFile, SubPath, SrcPath, AddList: PWideChar; Flags: integer): integer;stdcall;
  TDeleteFilesW = function (PackedFile, DeleteList: PWideChar): integer;stdcall;
  TStartMemPackW = function (Options: integer;  FileName: PWideChar): integer;stdcall;
  TCanYouHandleThisFileW = function (FileName: PWideChar): boolean;stdcall;
  TPkSetCryptCallbackW = procedure (PkCryptProc: TPkCryptProcW; CryptoNr, Flags: Integer); stdcall;

implementation

end.

unit uWCXprototypes;

{$mode objfpc}{$H+}

interface

uses
  LCLType, WcxPlugin;
type
{ Mandatory }
  TOpenArchive = function (var ArchiveData : tOpenArchiveData): TArcHandle;stdcall;
  TReadHeader = function (hArcData: TArcHandle; var HeaderData : THeaderData): Integer;stdcall;
  TProcessFile = function  (hArcData: TArcHandle;  Operation: Integer;  DestPath: PAnsiChar;  DestName: PAnsiChar): Integer;stdcall;
  TCloseArchive = function (hArcData: TArcHandle): Integer;stdcall;
{ Optional }
  TPackFiles = function (PackedFile: PAnsiChar;  SubPath: PAnsiChar;  SrcPath: PAnsiChar;  AddList: PAnsiChar;  Flags: Integer): Integer;stdcall;
  TDeleteFiles = function (PackedFile: PAnsiChar;  DeleteList: PAnsiChar): Integer;stdcall;
  TGetPackerCaps = function () : Integer;stdcall;
  TConfigurePacker = procedure (Parent: HWND;  DllInstance: THandle);stdcall;
  TSetChangeVolProc = procedure (hArcData: TArcHandle;  ChangeVolProc: tChangeVolProc);stdcall;
  TSetProcessDataProc = procedure (hArcData: TArcHandle;  ProcessDataProc: TProcessDataProc);stdcall;
  TStartMemPack = function (Options: Integer;  FileName: PAnsiChar): TArcHandle;stdcall;
  TPackToMem = function (hMemPack: TArcHandle;  BufIn: PByte;  InLen: Integer;  Taken: pInteger;  BufOut: PByte;  OutLen: Integer;  Written: pInteger;  SeekBy: pInteger): Integer;stdcall;
  TDoneMemPack = function (hMemPack: TArcHandle): Integer;stdcall;
  TCanYouHandleThisFile = function (FileName: PAnsiChar): boolean;stdcall;
  TPackSetDefaultParams = procedure (dps: pPackDefaultParamStruct);stdcall;
  TReadHeaderEx = function (hArcData: TArcHandle; var HeaderDataEx : THeaderDataEx): Integer;stdcall;
  TPkSetCryptCallback = procedure (PkCryptProc: TPkCryptProc; CryptoNr, Flags: Integer); stdcall;
  TGetBackgroundFlags = function(): Integer; stdcall;
{ Unicode }
  TOpenArchiveW = function (var ArchiveData : tOpenArchiveDataW): TArcHandle;stdcall;
  TReadHeaderExW = function (hArcData: TArcHandle; var HeaderDataExW : THeaderDataExW): Integer;stdcall;
  TProcessFileW = function  (hArcData: TArcHandle;  Operation: Integer; DestPath, DestName: PWideChar): Integer;stdcall;
  TSetChangeVolProcW = procedure (hArcData: TArcHandle; ChangeVolProc: tChangeVolProcW);stdcall;
  TSetProcessDataProcW = procedure (hArcData: TArcHandle; ProcessDataProc: TProcessDataProcW);stdcall;
  TPackFilesW = function (PackedFile, SubPath, SrcPath, AddList: PWideChar; Flags: Integer): Integer;stdcall;
  TDeleteFilesW = function (PackedFile, DeleteList: PWideChar): Integer;stdcall;
  TStartMemPackW = function (Options: Integer;  FileName: PWideChar): TArcHandle;stdcall;
  TCanYouHandleThisFileW = function (FileName: PWideChar): boolean;stdcall;
  TPkSetCryptCallbackW = procedure (PkCryptProc: TPkCryptProcW; CryptoNr, Flags: Integer); stdcall;

implementation

end.


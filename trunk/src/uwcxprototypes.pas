unit uWCXprototypes;

{$mode objfpc}{$H+}

interface

uses
  LCLType, WcxPlugin;

{$IFDEF MSWINDOWS}{$CALLING STDCALL}{$ELSE}{$CALLING CDECL}{$ENDIF}

type
{ Mandatory }
  TOpenArchive = function (var ArchiveData : tOpenArchiveData): TArcHandle;
  TReadHeader = function (hArcData: TArcHandle; var HeaderData : THeaderData): Integer;
  TProcessFile = function  (hArcData: TArcHandle;  Operation: Integer;  DestPath: PAnsiChar;  DestName: PAnsiChar): Integer;
  TCloseArchive = function (hArcData: TArcHandle): Integer;
{ Optional }
  TPackFiles = function (PackedFile: PAnsiChar;  SubPath: PAnsiChar;  SrcPath: PAnsiChar;  AddList: PAnsiChar;  Flags: Integer): Integer;
  TDeleteFiles = function (PackedFile: PAnsiChar;  DeleteList: PAnsiChar): Integer;
  TGetPackerCaps = function () : Integer;
  TConfigurePacker = procedure (Parent: HWND;  DllInstance: THandle);
  TSetChangeVolProc = procedure (hArcData: TArcHandle;  ChangeVolProc: tChangeVolProc);
  TSetProcessDataProc = procedure (hArcData: TArcHandle;  ProcessDataProc: TProcessDataProc);
  TStartMemPack = function (Options: Integer;  FileName: PAnsiChar): TArcHandle;
  TPackToMem = function (hMemPack: TArcHandle;  BufIn: PByte;  InLen: Integer;  Taken: pInteger;  BufOut: PByte;  OutLen: Integer;  Written: pInteger;  SeekBy: pInteger): Integer;
  TDoneMemPack = function (hMemPack: TArcHandle): Integer;
  TCanYouHandleThisFile = function (FileName: PAnsiChar): boolean;
  TPackSetDefaultParams = procedure (dps: pPackDefaultParamStruct);
  TReadHeaderEx = function (hArcData: TArcHandle; var HeaderDataEx : THeaderDataEx): Integer;
  TPkSetCryptCallback = procedure (PkCryptProc: TPkCryptProc; CryptoNr, Flags: Integer);
  TGetBackgroundFlags = function(): Integer;
{ Unicode }
  TOpenArchiveW = function (var ArchiveData : tOpenArchiveDataW): TArcHandle;
  TReadHeaderExW = function (hArcData: TArcHandle; var HeaderDataExW : THeaderDataExW): Integer;
  TProcessFileW = function  (hArcData: TArcHandle;  Operation: Integer; DestPath, DestName: PWideChar): Integer;
  TSetChangeVolProcW = procedure (hArcData: TArcHandle; ChangeVolProc: tChangeVolProcW);
  TSetProcessDataProcW = procedure (hArcData: TArcHandle; ProcessDataProc: TProcessDataProcW);
  TPackFilesW = function (PackedFile, SubPath, SrcPath, AddList: PWideChar; Flags: Integer): Integer;
  TDeleteFilesW = function (PackedFile, DeleteList: PWideChar): Integer;
  TStartMemPackW = function (Options: Integer;  FileName: PWideChar): TArcHandle;
  TCanYouHandleThisFileW = function (FileName: PWideChar): boolean;
  TPkSetCryptCallbackW = procedure (PkCryptProc: TPkCryptProcW; CryptoNr, Flags: Integer);

{$CALLING DEFAULT}

implementation

end.


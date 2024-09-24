{
   Double commander
   -------------------------------------------------------------------------
   Archive File support - class for manage WCX plugins (Version 2.20)

   Copyright (C) 2006-2024 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit uWCXmodule;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  LCLType, Classes, Dialogs, DCClassesUtf8, dynlibs, SysUtils, uExtension,
  uWCXprototypes, WcxPlugin, Extension, DCBasicTypes, DCXmlConfig, uClassesEx;

Type
  TWCXOperation = (OP_EXTRACT, OP_PACK, OP_DELETE);

  { EWcxModuleException }

  EWcxModuleException = class(EOSError)
  public
    constructor Create(AErrorCode: Integer);
  end;

  { TWCXHeaderData }

  { Handles THeaderData and THeaderDataEx }
  TWCXHeader = class(TObjectEx)
  private
    FileTime: LongInt;
    FDateTime: TDateTime;
    FNanoTime: TWinFileTime;
  private
    function GetDateTime: TDateTime;
    function PCharLToUTF8(CharString: PChar; MaxSize: Integer): String;
  public
    ArcName: String;
    FileName: String;
    Flags,
    HostOS,
    FileCRC,
    UnpVer,
    Method: Longint;
    FileAttr: TFileAttrs;
    PackSize,
    UnpSize: Int64;
    Cmt: String;
    CmtState: Longint;

    function Clone: TWCXHeader; override;

    constructor Create(const Data: PHeaderData); overload;
    constructor Create(const Data: PHeaderDataEx); overload;
    constructor Create(const Data: PHeaderDataExW); overload;
    constructor Create; overload; // allows creating empty record

    property DateTime: TDateTime read GetDateTime write FDateTime;
  end;

  
  { TWcxModule }

  TWcxModule = class(TDcxModule)
  private
    FModuleName: String;
    FBackgroundFlags: Integer;

  public
    // module's functions
    { Mandatory }
    OpenArchive : TOpenArchive;
    ReadHeader : TReadHeader;
    ProcessFile : TProcessFile;
    CloseArchive : TCloseArchive;
    { Optional }
    ReadHeaderEx : TReadHeaderEx;
    PackFiles : TPackFiles;
    DeleteFiles : TDeleteFiles;
    GetPackerCaps : TGetPackerCaps;
    ConfigurePacker : TConfigurePacker;
    SetChangeVolProc : TSetChangeVolProc;
    SetProcessDataProc : TSetProcessDataProc;
    StartMemPack : TStartMemPack;
    PackToMem : TPackToMem;
    DoneMemPack : TDoneMemPack;
    CanYouHandleThisFile : TCanYouHandleThisFile;
    PackSetDefaultParams : TPackSetDefaultParams;
    PkSetCryptCallback : TPkSetCryptCallback;
    GetBackgroundFlags: TGetBackgroundFlags;
    { Unicode }
    OpenArchiveW: TOpenArchiveW;
    ReadHeaderExW: TReadHeaderExW;
    ProcessFileW: TProcessFileW;
    SetChangeVolProcW: TSetChangeVolProcW;
    SetProcessDataProcW:TSetProcessDataProcW;
    PackFilesW: TPackFilesW;
    DeleteFilesW: TDeleteFilesW;
    StartMemPackW: TStartMemPackW;
    CanYouHandleThisFileW: TCanYouHandleThisFileW;
    PkSetCryptCallbackW : TPkSetCryptCallbackW;
    { Extension API }
    ExtensionInitialize: TExtensionInitializeProc;
    ExtensionFinalize:   TExtensionFinalizeProc;

  private
    function LoadModule(const sName:String):Boolean; {Load WCX plugin}
    procedure UnloadModule;                          {UnLoad WCX plugin}

  public
    constructor Create;
    destructor Destroy; override;

    { Reads WCX header using ReadHeaderEx if available or ReadHeader. }
    function ReadWCXHeader(hArcData: TArcHandle;
                           out HeaderData: TWCXHeader): Integer;

    function OpenArchiveHandle(FileName: String; anOpenMode: Longint; out OpenResult: Longint): TArcHandle;
    function WcxProcessFile(hArcData: TArcHandle; Operation: LongInt; DestPath, DestName: String): LongInt;
    function WcxPackFiles(PackedFile, SubPath, SrcPath, AddList: String; Flags: LongInt): LongInt;
    function WcxDeleteFiles(PackedFile, DeleteList: String): LongInt;
    function WcxCanYouHandleThisFile(FileName: String): Boolean;
    function WcxStartMemPack(Options: LongInt;  FileName: String): TArcHandle;
    procedure WcxSetChangeVolProc(hArcData: TArcHandle); overload;
    procedure WcxSetChangeVolProc(hArcData: TArcHandle; ChangeVolProcA: TChangeVolProc; ChangeVolProcW: TChangeVolProcW); overload;
    procedure WcxSetProcessDataProc(hArcData: TArcHandle; ProcessDataProcA: TProcessDataProc; ProcessDataProcW: TProcessDataProcW);
    procedure WcxSetCryptCallback(CryptoNr, Flags: Integer; PkCryptProcA: TPkCryptProc; PkCryptProcW: TPkCryptProcW);

    procedure VFSConfigure(Parent: HWND);
    function GetPluginCapabilities: Integer;

    function IsLoaded: Boolean;

    property ModuleName: String read FModuleName;
    property BackgroundFlags: Integer read FBackgroundFlags write FBackgroundFlags;
  end;

  { TWCXModuleList }

  TWCXModuleList = class(TStringList)
  private
    FModuleList: TStringListEx;
  private
    function GetAEnabled(Index: Integer): Boolean;
    function GetAExt(Index: Integer): String;
    function GetAFileName(Index: Integer): String;
    function GetAFlags(Index: Integer): PtrInt;
    procedure SetAEnabled(Index: Integer; const AValue: Boolean);
    procedure SetAFileName(Index: Integer; const AValue: String);
    procedure SetAFlags(Index: Integer; const AValue: PtrInt);
    procedure SetExt(Index: Integer; const AValue: String);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  public
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode);
    function ComputeSignature(seed: dword): dword;
    function Add(Ext: String; Flags: PtrInt; FileName: String): Integer; reintroduce;
    function FindFirstEnabledByName(Name: String): Integer;
    function Find(const aFileName, aExt: String): Integer; overload;
    function LoadModule(const FileName: String): TWcxModule;

    property FileName[Index: Integer]: String read GetAFileName write SetAFileName;
    property Flags[Index: Integer]: PtrInt read GetAFlags write SetAFlags;
    property Ext[Index: Integer]: String read GetAExt write SetExt;
    property Enabled[Index: Integer]: Boolean read GetAEnabled write SetAEnabled;
  end;

  function GetErrorMsg(iErrorMsg : Integer): String;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  SysConst, LazUTF8, FileUtil,

  //DC
  uDCUtils, uComponentsSignature, uGlobsPaths, uLng, uOSUtils, DCOSUtils, uOSForms,
  DCDateTimeUtils, DCConvertEncoding, fDialogBox, uDebug, uShowMsg, uLog, uGlobs;

const
  WcxIniFileName = 'wcx.ini';

function ChangeVolProc(var ArcName : String; Mode: LongInt): LongInt;
begin
  Result:= 1;
  case Mode of
  PK_VOL_ASK:
    begin
      if not ShowInputQuery('Double Commander', rsMsgSelLocNextVol, ArcName) then
        Result := 0; // Abort operation
    end;
  PK_VOL_NOTIFY:
    if log_arc_op in gLogOptions then
      LogWrite(rsMsgNextVolUnpack + #32 + ArcName);
  end;
end;

function ChangeVolProcA(ArcName : PAnsiChar; Mode: LongInt): LongInt; dcpcall;
var
  sArcName: String;
begin
  sArcName:= CeSysToUtf8(StrPas(ArcName));
  Result:= ChangeVolProc(sArcName, Mode);
  if (Mode = PK_VOL_ASK) and (Result <> 0) then
    StrPLCopy(ArcName, CeUtf8ToSys(sArcName), MAX_PATH);
end;

function ChangeVolProcW(ArcName : PWideChar; Mode: LongInt): LongInt; dcpcall;
var
  sArcName: String;
begin
  sArcName:= UTF16ToUTF8(UnicodeString(ArcName));
  Result:= ChangeVolProc(sArcName, Mode);
  if (Mode = PK_VOL_ASK) and (Result <> 0) then
    StrPLCopy(ArcName, CeUtf8ToUtf16(sArcName), MAX_PATH);
end;

{ EWcxModuleException }

constructor EWcxModuleException.Create(AErrorCode: Integer);
begin
  ErrorCode:= AErrorCode;
  inherited Create(GetErrorMsg(ErrorCode));
end;

constructor TWcxModule.Create;
begin
  FModuleHandle := 0;
end;

destructor TWcxModule.Destroy;
begin
  if IsLoaded then
  begin
    if Assigned(ExtensionFinalize) then
      ExtensionFinalize(nil);
    //------------------------------------------------------
    UnloadModule;
  end;
  inherited Destroy;
end;

function TWcxModule.OpenArchiveHandle(FileName: String; anOpenMode: Longint; out OpenResult: Longint): TArcHandle;
var
  ArcFile: tOpenArchiveData;
  ArcFileW: tOpenArchiveDataW;
  AnsiFileName: AnsiString;
  WideFileName: WideString;
begin
  if (anOpenMode >= PK_OM_LIST) and (anOpenMode <= PK_OM_EXTRACT) then
  begin
    if Assigned(OpenArchiveW) then
      begin
        FillChar(ArcFileW, SizeOf(ArcFileW), #0);
        WideFileName := CeUtf8ToUtf16(FileName);
        ArcFileW.ArcName := PWideChar(WideFileName); // Pointer to local variable.
        ArcFileW.OpenMode := anOpenMode;
        Result := OpenArchiveW(ArcFileW);
        if Result = 0 then
          OpenResult := ArcFileW.OpenResult
        else
          OpenResult := E_SUCCESS;
      end
    else if Assigned(OpenArchive) then
      begin
        FillChar(ArcFile, SizeOf(ArcFile), #0);
        AnsiFileName := mbFileNameToSysEnc(FileName);
        ArcFile.ArcName := PAnsiChar(AnsiFileName); // Pointer to local variable.
        ArcFile.OpenMode := anOpenMode;
        Result := OpenArchive(ArcFile);
        if Result = 0 then
          OpenResult := ArcFile.OpenResult
        else
          OpenResult := E_SUCCESS;
      end;
  end
  else
    raise Exception.Create('Invalid WCX open mode');
end;

function TWcxModule.WcxProcessFile(hArcData: TArcHandle; Operation: LongInt;
  DestPath, DestName: String): LongInt;
begin
  if Assigned(ProcessFileW) then
    begin
      if DestPath = EmptyStr then
        Result:= ProcessFileW(hArcData, Operation, nil, PWideChar(CeUtf8ToUtf16(DestName)))
      else
        Result:= ProcessFileW(hArcData, Operation, PWideChar(CeUtf8ToUtf16(DestPath)), PWideChar(CeUtf8ToUtf16(DestName)));
    end
  else if Assigned(ProcessFile) then
    begin
      if DestPath = EmptyStr then
        Result:= ProcessFile(hArcData, Operation, nil, PAnsiChar(CeUtf8ToSys(DestName)))
      else
        Result:= ProcessFile(hArcData, Operation, PAnsiChar(CeUtf8ToSys(DestPath)), PAnsiChar(CeUtf8ToSys(DestName)));
    end;
end;

function TWcxModule.WcxPackFiles(PackedFile, SubPath, SrcPath,
  AddList: String; Flags: LongInt): LongInt;
begin
  if Assigned(PackFilesW) then
    begin
      if SubPath = EmptyStr then
        Result:= PackFilesW(PWideChar(CeUtf8ToUtf16(PackedFile)), nil,
                            PWideChar(CeUtf8ToUtf16(SrcPath)), PWideChar(CeUtf8ToUtf16(AddList)), Flags)
      else
        Result:= PackFilesW(PWideChar(CeUtf8ToUtf16(PackedFile)), PWideChar(CeUtf8ToUtf16(SubPath)),
                            PWideChar(CeUtf8ToUtf16(SrcPath)), PWideChar(CeUtf8ToUtf16(AddList)), Flags);
    end
  else if Assigned(PackFiles) then
    begin
      if SubPath = EmptyStr then
        Result:= PackFiles(PAnsiChar(CeUtf8ToSys(PackedFile)), nil,
                           PAnsiChar(CeUtf8ToSys(SrcPath)), PAnsiChar(CeUtf8ToSys(AddList)), Flags)
      else
        Result:= PackFiles(PAnsiChar(CeUtf8ToSys(PackedFile)), PAnsiChar(CeUtf8ToSys(SubPath)),
                           PAnsiChar(CeUtf8ToSys(SrcPath)), PAnsiChar(CeUtf8ToSys(AddList)), Flags);
    end;
end;

function TWcxModule.WcxDeleteFiles(PackedFile, DeleteList: String): LongInt;
begin
  if Assigned(DeleteFilesW) then
    Result:= DeleteFilesW(PWideChar(CeUtf8ToUtf16(PackedFile)), PWideChar(CeUtf8ToUtf16(DeleteList)))
  else if Assigned(DeleteFiles) then
    Result:= DeleteFiles(PAnsiChar(CeUtf8ToSys(PackedFile)), PAnsiChar(CeUtf8ToSys(DeleteList)));
end;

function TWcxModule.WcxCanYouHandleThisFile(FileName: String): Boolean;
begin
  if Assigned(CanYouHandleThisFileW) then
    Result:= CanYouHandleThisFileW(PWideChar(CeUtf8ToUtf16(FileName)))
  else if Assigned(CanYouHandleThisFile) then
    Result:= CanYouHandleThisFile(PAnsiChar(CeUtf8ToSys(FileName)))
  else
    Result:= True;
end;

function TWcxModule.WcxStartMemPack(Options: LongInt; FileName: String): TArcHandle;
begin
  if Assigned(StartMemPackW) then
    Result:= StartMemPackW(Options, PWideChar(CeUtf8ToUtf16(FileName)))
  else if Assigned(StartMemPack) then
    Result:= StartMemPack(Options, PAnsiChar(CeUtf8ToSys(FileName)));
end;

procedure TWcxModule.WcxSetChangeVolProc(hArcData: TArcHandle);
begin
  WcxSetChangeVolProc(hArcData, @ChangeVolProcA, @ChangeVolProcW);
end;

procedure TWcxModule.WcxSetChangeVolProc(hArcData: TArcHandle;
  ChangeVolProcA: TChangeVolProc; ChangeVolProcW: TChangeVolProcW);
begin
  if Assigned(SetChangeVolProcW) then
    SetChangeVolProcW(hArcData, ChangeVolProcW);
  if Assigned(SetChangeVolProc) then
    SetChangeVolProc(hArcData, ChangeVolProcA);
end;

procedure TWcxModule.WcxSetProcessDataProc(hArcData: TArcHandle;
  ProcessDataProcA: TProcessDataProc; ProcessDataProcW: TProcessDataProcW);
begin
  if Assigned(SetProcessDataProcW) then
    SetProcessDataProcW(hArcData, ProcessDataProcW);
  if Assigned(SetProcessDataProc) then
    SetProcessDataProc(hArcData, ProcessDataProcA);
end;

procedure TWcxModule.WcxSetCryptCallback(CryptoNr, Flags: Integer;
  PkCryptProcA: TPkCryptProc; PkCryptProcW: TPkCryptProcW);
begin
  if Assigned(PkSetCryptCallbackW) then
    PkSetCryptCallbackW(PkCryptProcW, CryptoNr, Flags);
  if Assigned(PkSetCryptCallback) then
    PkSetCryptCallback(PkCryptProcA, CryptoNr, Flags);
end;

function TWcxModule.LoadModule(const sName:String):Boolean;
var
  StartupInfo: TExtensionStartupInfo;
  PackDefaultParamStruct : TPackDefaultParamStruct;
begin
  FModuleName := ExtractFileName(sName);
  FModulePath := mbExpandFileName(sName);
  FModuleHandle := mbLoadLibrary(FModulePath);
  if FModuleHandle = 0 then Exit(False);

  DCDebug('WCX module loaded ' + sName + ' at ' + hexStr(Pointer(FModuleHandle)));

  // Mandatory functions
  OpenArchive:= TOpenArchive(GetProcAddress(FModuleHandle,'OpenArchive'));
  ReadHeader:= TReadHeader(GetProcAddress(FModuleHandle,'ReadHeader'));
  ReadHeaderEx:= TReadHeaderEx(GetProcAddress(FModuleHandle,'ReadHeaderEx'));
  ProcessFile:= TProcessFile(GetProcAddress(FModuleHandle,'ProcessFile'));
  CloseArchive:= TCloseArchive(GetProcAddress(FModuleHandle,'CloseArchive'));
  // Unicode
  OpenArchiveW:= TOpenArchiveW(GetProcAddress(FModuleHandle,'OpenArchiveW'));
  ReadHeaderExW:= TReadHeaderExW(GetProcAddress(FModuleHandle,'ReadHeaderExW'));
  ProcessFileW:= TProcessFileW(GetProcAddress(FModuleHandle,'ProcessFileW'));

  Result:= (OpenArchive <> nil) and (ReadHeader <> nil) and (ProcessFile <> nil);
  if (Result = False) then
  begin
    OpenArchive:= nil;
    ReadHeader:= nil;
    ProcessFile:= nil;
    Result:= (OpenArchiveW <> nil) and (ReadHeaderExW <> nil) and (ProcessFileW <> nil);
  end;

  if (Result = False) or (CloseArchive = nil) then
  begin
    OpenArchiveW:= nil;
    ReadHeaderExW:= nil;
    ProcessFileW:= nil;
    CloseArchive:= nil;
    Exit(False);
  end;

  // Optional functions
  PackFiles:= TPackFiles(GetProcAddress(FModuleHandle,'PackFiles'));
  DeleteFiles:= TDeleteFiles(GetProcAddress(FModuleHandle,'DeleteFiles'));
  GetPackerCaps:= TGetPackerCaps(GetProcAddress(FModuleHandle,'GetPackerCaps'));
  ConfigurePacker:= TConfigurePacker(GetProcAddress(FModuleHandle,'ConfigurePacker'));
  SetChangeVolProc:= TSetChangeVolProc(GetProcAddress(FModuleHandle,'SetChangeVolProc'));
  SetProcessDataProc:= TSetProcessDataProc(GetProcAddress(FModuleHandle,'SetProcessDataProc'));
  StartMemPack:= TStartMemPack(GetProcAddress(FModuleHandle,'StartMemPack'));
  PackToMem:= TPackToMem(GetProcAddress(FModuleHandle,'PackToMem'));
  DoneMemPack:= TDoneMemPack(GetProcAddress(FModuleHandle,'DoneMemPack'));
  CanYouHandleThisFile:= TCanYouHandleThisFile(GetProcAddress(FModuleHandle,'CanYouHandleThisFile'));
  PackSetDefaultParams:= TPackSetDefaultParams(GetProcAddress(FModuleHandle,'PackSetDefaultParams'));
  PkSetCryptCallback:= TPkSetCryptCallback(GetProcAddress(FModuleHandle,'PkSetCryptCallback'));
  GetBackgroundFlags:= TGetBackgroundFlags(GetProcAddress(FModuleHandle,'GetBackgroundFlags'));
  // Unicode
  SetChangeVolProcW:= TSetChangeVolProcW(GetProcAddress(FModuleHandle,'SetChangeVolProcW'));
  SetProcessDataProcW:= TSetProcessDataProcW(GetProcAddress(FModuleHandle,'SetProcessDataProcW'));
  PackFilesW:= TPackFilesW(GetProcAddress(FModuleHandle,'PackFilesW'));
  DeleteFilesW:= TDeleteFilesW(GetProcAddress(FModuleHandle,'DeleteFilesW'));
  StartMemPackW:= TStartMemPackW(GetProcAddress(FModuleHandle,'StartMemPackW'));
  CanYouHandleThisFileW:= TCanYouHandleThisFileW(GetProcAddress(FModuleHandle,'CanYouHandleThisFileW'));
  PkSetCryptCallbackW:= TPkSetCryptCallbackW(GetProcAddress(FModuleHandle,'PkSetCryptCallbackW'));
  // Extension API
  ExtensionInitialize:= TExtensionInitializeProc(GetProcAddress(FModuleHandle,'ExtensionInitialize'));
  ExtensionFinalize:= TExtensionFinalizeProc(GetProcAddress(FModuleHandle,'ExtensionFinalize'));

  if Assigned(PackSetDefaultParams) then
    begin
      with PackDefaultParamStruct do
        begin
          Size := SizeOf(PackDefaultParamStruct);
          PluginInterfaceVersionLow := 22;
          PluginInterfaceVersionHi := 2;
          DefaultIniName := mbFileNameToSysEnc(gpCfgDir + WcxIniFileName);
        end;
      PackSetDefaultParams(@PackDefaultParamStruct);
    end;

  if not Assigned(GetBackgroundFlags) then
    FBackgroundFlags:= 0
  else
    FBackgroundFlags:= GetBackgroundFlags();

  // Extension API
  if Assigned(ExtensionInitialize) then
  begin
    InitializeExtension(@StartupInfo);

    ExtensionInitialize(@StartupInfo);
  end;
end;

procedure TWcxModule.UnloadModule;
begin
  if FModuleHandle <> NilHandle then
  begin
    FreeLibrary(FModuleHandle);
    FModuleHandle := NilHandle;
  end;
  // Mandatory
  OpenArchive:= nil;
  ReadHeader:= nil;
  ReadHeaderEx:= nil;
  ProcessFile:= nil;
  CloseArchive:= nil;
  // Optional
  PackFiles:= nil;
  DeleteFiles:= nil;
  GetPackerCaps:= nil;
  ConfigurePacker:= nil;
  SetChangeVolProc:= nil;
  SetProcessDataProc:= nil;
  StartMemPack:= nil;
  PackToMem:= nil;
  DoneMemPack:= nil;
  CanYouHandleThisFile:= nil;
  PackSetDefaultParams:= nil;
  PkSetCryptCallback:= nil;
  GetBackgroundFlags:= nil;
  // Unicode
  OpenArchiveW:= nil;
  ReadHeaderExW:= nil;
  ProcessFileW:= nil;
  SetChangeVolProcW:= nil;
  SetProcessDataProcW:= nil;
  PackFilesW:= nil;
  DeleteFilesW:= nil;
  StartMemPackW:= nil;
  CanYouHandleThisFileW:= nil;
  PkSetCryptCallbackW:= nil;
  // Extension API
  ExtensionInitialize:= nil;
  ExtensionFinalize:= nil;
end;

function GetErrorMsg(iErrorMsg : Integer): String;
begin
  case iErrorMsg of
    E_END_ARCHIVE    :   Result := rsMsgErrEndArchive;
    E_NO_MEMORY      :   Result := rsMsgErrNoMemory;
    E_BAD_DATA       :   Result := rsMsgErrBadData;
    E_BAD_ARCHIVE    :   Result := rsMsgErrBadArchive;
    E_UNKNOWN_FORMAT :   Result := rsMsgErrUnknownFormat;
    E_EOPEN          :   Result := rsMsgErrEOpen;
    E_ECREATE        :   Result := rsMsgErrECreate;
    E_ECLOSE         :   Result := rsMsgErrEClose;
    E_EREAD          :   Result := rsMsgErrERead;
    E_EWRITE         :   Result := rsMsgErrEWrite;
    E_SMALL_BUF      :   Result := rsMsgErrSmallBuf;
    E_EABORTED       :   Result := rsMsgErrEAborted;
    E_NO_FILES       :   Result := rsMsgErrNoFiles;
    E_TOO_MANY_FILES :   Result := rsMsgErrTooManyFiles;
    E_NOT_SUPPORTED  :   Result := rsMsgErrNotSupported;
    else                 Result := Format(SUnknownErrorCode, [iErrorMsg]);
  end;
end;

procedure TWcxModule.VFSConfigure(Parent: HWND);
begin
  if Assigned(ConfigurePacker) then
    ConfigurePacker(GetWindowHandle(Parent), FModuleHandle);
end;

function TWcxModule.ReadWCXHeader(hArcData: TArcHandle;
                                  out HeaderData: TWCXHeader): Integer;
var
  ArcHeader : THeaderData;
  ArcHeaderEx : THeaderDataEx;
  ArcHeaderExW : THeaderDataExW;
begin
  HeaderData := nil;

  if Assigned(ReadHeaderExW) then
  begin
    FillChar(ArcHeaderExW, SizeOf(ArcHeaderExW), #0);
    Result := ReadHeaderExW(hArcData, ArcHeaderExW);
    if Result = E_SUCCESS then
    begin
      HeaderData := TWCXHeader.Create(PHeaderDataExW(@ArcHeaderExW));
    end;
  end
  else if Assigned(ReadHeaderEx) then
  begin
    FillChar(ArcHeaderEx, SizeOf(ArcHeaderEx), #0);
    Result := ReadHeaderEx(hArcData, ArcHeaderEx);
    if Result = E_SUCCESS then
    begin
      HeaderData := TWCXHeader.Create(PHeaderDataEx(@ArcHeaderEx));
    end;
  end
  else if Assigned(ReadHeader) then
  begin
    FillChar(ArcHeader, SizeOf(ArcHeader), #0);
    Result := ReadHeader(hArcData, ArcHeader);
    if Result = E_SUCCESS then
    begin
      HeaderData := TWCXHeader.Create(PHeaderData(@ArcHeader));
    end;
  end
  else
  begin
    Result := E_NOT_SUPPORTED;
  end;
end;

function TWcxModule.GetPluginCapabilities: Integer;
begin
  if Assigned(GetPackerCaps) then
    Result := GetPackerCaps()
  else
    Result := 0;
end;

function TWcxModule.IsLoaded: Boolean;
begin
  Result := (FModuleHandle <> NilHandle);
end;

{ TWCXModuleList }

function TWCXModuleList.GetAEnabled(Index: Integer): Boolean;
begin
  Result:= Boolean(PtrInt(Objects[Index]));
end;

function TWCXModuleList.GetAExt(Index: Integer): String;
begin
  Result:= Names[Index];
end;

function TWCXModuleList.GetAFileName(Index: Integer): String;
var
  sCurrPlugin: String;
  iPosComma : Integer;
begin
  sCurrPlugin:= ValueFromIndex[Index];
  iPosComma:= Pos(',', sCurrPlugin);
    //get file name
  Result:= Copy(sCurrPlugin, iPosComma + 1, Length(sCurrPlugin) - iPosComma);
end;

function TWCXModuleList.GetAFlags(Index: Integer): PtrInt;
var
  sCurrPlugin: String;
  iPosComma : Integer;
begin
  sCurrPlugin:= ValueFromIndex[Index];
  iPosComma:= Pos(',', sCurrPlugin);
  // get packer flags
  Result:= StrToInt(Copy(sCurrPlugin, 1, iPosComma-1));
end;

procedure TWCXModuleList.SetAEnabled(Index: Integer; const AValue: Boolean);
begin
  Objects[Index]:= TObject(PtrInt(AValue));
end;

procedure TWCXModuleList.SetAFileName(Index: Integer; const AValue: String);
begin
  ValueFromIndex[Index]:= IntToStr(GetAFlags(Index)) + #44 + AValue;
end;

procedure TWCXModuleList.SetAFlags(Index: Integer; const AValue: PtrInt);
begin
  ValueFromIndex[Index]:= IntToStr(AValue) + #44 + GetAFileName(Index);
end;

procedure TWCXModuleList.SetExt(Index: Integer; const AValue: String);
var
  sValue : String;
begin
  sValue:= ValueFromIndex[Index];
  Self[Index]:= AValue + '=' + sValue;
end;

constructor TWCXModuleList.Create;
begin
  FModuleList:= TStringListEx.Create;
  FModuleList.Sorted:= True;
end;

destructor TWCXModuleList.Destroy;
var
  I: Integer;
begin
  for I:= 0 to FModuleList.Count - 1 do
  begin
    TWcxModule(FModuleList.Objects[I]).Free;
  end;
  FreeAndNil(FModuleList);
  inherited Destroy;
end;

procedure TWCXModuleList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  AExt, APath: String;
  AFlags: Integer;
begin
  Clear;

  ANode := ANode.FindNode('WcxPlugins');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('WcxPlugin') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'ArchiveExt', AExt) and
           AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          AFlags := AConfig.GetValue(ANode, 'Flags', 0);
          I := Add(AExt, AFlags, APath);
          Enabled[I] := AConfig.GetAttr(ANode, 'Enabled', True);
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWCXModuleList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'WcxPlugins', True);
  AConfig.ClearNode(ANode);
  for I := 0 to Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'WcxPlugin');
      AConfig.SetAttr(SubNode, 'Enabled', Enabled[I]);
      AConfig.AddValue(SubNode, 'ArchiveExt', Ext[I]);
      AConfig.AddValue(SubNode, 'Path', FileName[I]);
      AConfig.AddValue(SubNode, 'Flags', Flags[I]);
    end;
end;

{ TWCXModuleList.ComputeSignature }
function TWCXModuleList.ComputeSignature(seed: dword): dword;
var
  iIndex: integer;
begin
  result := seed;
  for iIndex := 0 to pred(Count) do
  begin
    result := ComputeSignatureBoolean(result, Enabled[iIndex]);
    result := ComputeSignatureString(result, Ext[iIndex]);
    result := ComputeSignatureString(result, FileName[iIndex]);
    result := ComputeSignaturePtrInt(result, Flags[iIndex]);
  end;
end;

{ TWCXModuleList.Add }
function TWCXModuleList.Add(Ext: String; Flags: PtrInt; FileName: String): Integer;
begin
  Result:= AddObject(Ext + '=' + IntToStr(Flags) + #44 + FileName, TObject(True));
end;

function TWCXModuleList.FindFirstEnabledByName(Name: String): Integer;
begin
  Result:=0;
  while Result < Count do
  begin
    if Enabled[Result] and (DoCompareText(Names[Result], Name) = 0) then
       Exit
    else
      Result := Result + 1;
  end;
  if Result=Count then Result:=-1;
end;

function TWCXModuleList.Find(const aFileName, aExt: String): Integer;
begin
  Result:=0;
  while Result < Count do
  begin
    if (DoCompareText(Ext[Result], aExt) = 0) and (DoCompareText(FileName[Result], aFileName) = 0) then
       Exit
    else
      Result := Result + 1;
  end;
  if Result=Count then Result:=-1;
end;

function TWCXModuleList.LoadModule(const FileName: String): TWcxModule;
var
  Index: Integer;
begin
  if FModuleList.Find(FileName, Index) then
    Result := TWcxModule(FModuleList.Objects[Index])
  else begin
    Result := TWcxModule.Create;
    if not Result.LoadModule(FileName) then
      FreeAndNil(Result)
    else begin
      FModuleList.AddObject(FileName, Result);
    end;
  end;
end;

{ TWCXHeader }

constructor TWCXHeader.Create(const Data: PHeaderData);
begin
  ArcName  := PCharLToUTF8(Data^.ArcName, SizeOf(Data^.ArcName));
  FileName := PCharLToUTF8(Data^.FileName, SizeOf(Data^.FileName));
  Flags    := Data^.Flags;
  HostOS   := Data^.HostOS;
  FileCRC  := Data^.FileCRC;
  FileTime := Data^.FileTime;
  UnpVer   := Data^.UnpVer;
  Method   := Data^.Method;
  FileAttr := TFileAttrs(Data^.FileAttr);
  PackSize := Data^.PackSize;
  UnpSize  := Data^.UnpSize;
  if Assigned(Data^.CmtBuf) then
    Cmt := PCharLToUTF8(Data^.CmtBuf, Data^.CmtSize);
  CmtState := Data^.CmtState;
end;

constructor TWCXHeader.Create(const Data: PHeaderDataEx);

  function Combine64(High, Low: LongWord): Int64;
  begin
    Result := Int64(High) shl (SizeOf(Int64) shl 2);
    Result := Result + Int64(Low);
  end;

begin
  ArcName  := PCharLToUTF8(Data^.ArcName, SizeOf(Data^.ArcName));
  FileName := PCharLToUTF8(Data^.FileName, SizeOf(Data^.FileName));
  Flags    := Data^.Flags;
  HostOS   := Data^.HostOS;
  FileCRC  := Data^.FileCRC;
  FileTime := Data^.FileTime;
  UnpVer   := Data^.UnpVer;
  Method   := Data^.Method;
  FileAttr := TFileAttrs(Data^.FileAttr);
  PackSize := Combine64(Data^.PackSizeHigh, Data^.PackSize);
  UnpSize  := Combine64(Data^.UnpSizeHigh, Data^.UnpSize);
  if Assigned(Data^.CmtBuf) then
    Cmt := PCharLToUTF8(Data^.CmtBuf, Data^.CmtSize);
  CmtState := Data^.CmtState;
end;

constructor TWCXHeader.Create(const Data: PHeaderDataExW);

  function Combine64(High, Low: LongWord): Int64;
  begin
    Result := Int64(High) shl (SizeOf(Int64) shl 2);
    Result := Result + Int64(Low);
  end;

begin
  ArcName  := UTF16ToUTF8(UnicodeString(Data^.ArcName));
  FileName := UTF16ToUTF8(UnicodeString(Data^.FileName));
  Flags    := Data^.Flags;
  HostOS   := Data^.HostOS;
  FileCRC  := Data^.FileCRC;
  FileTime := Data^.FileTime;
  UnpVer   := Data^.UnpVer;
  Method   := Data^.Method;
  FileAttr := TFileAttrs(Data^.FileAttr);
  PackSize := Combine64(Data^.PackSizeHigh, Data^.PackSize);
  UnpSize  := Combine64(Data^.UnpSizeHigh, Data^.UnpSize);
  if Assigned(Data^.CmtBuf) then
    Cmt := PCharLToUTF8(Data^.CmtBuf, Data^.CmtSize);
  CmtState := Data^.CmtState;
  FNanoTime:= Data^.MfileTime;
end;

constructor TWCXHeader.Create;
begin
end;

function TWCXHeader.GetDateTime: TDateTime;
begin
  if FDateTime <> 0 then
    Result:= FDateTime
  else begin
    if (FNanoTime > 0) then
      FDateTime:= WinFileTimeToDateTime(FNanoTime)
    else begin
      if (FileTime = 0) then
        FDateTime:= DATE_TIME_NULL
      else begin
        FDateTime:= WcxFileTimeToDateTime(FileTime);
      end;
    end;
    Result:= FDateTime;
  end;
end;

function TWCXHeader.PCharLToUTF8(CharString: PChar; MaxSize: Integer): String;
var
  NameLength: Integer;
  TempString: AnsiString;
begin
  NameLength := strlen(CharString);
  if NameLength > MaxSize then
    NameLength := MaxSize;

  SetString(TempString, CharString, NameLength);
  Result := CeSysToUtf8(TempString);
end;

function TWCXHeader.Clone: TWCXHeader;
begin
  Result:= TWCXHeader.Create;

  Result.ArcName:= ArcName;
  Result.FileName:= FileName;
  Result.Flags:= Flags;
  Result.HostOS:= HostOS;
  Result.FileCRC:= FileCRC;
  Result.FileTime:= FileTime;
  Result.UnpVer:= UnpVer;
  Result.Method:= Method;
  Result.FileAttr:=FileAttr;
  Result.PackSize:= PackSize;
  Result.UnpSize:= UnpSize;
  Result.Cmt:= Cmt;
  Result.CmtState:= CmtState;
  Result.FNanoTime:= FNanoTime;
  Result.FDateTime:= FDateTime;
end;

end.

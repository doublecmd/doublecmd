{
   Double commander
   -------------------------------------------------------------------------
   Archive File support - class for manage WCX plugins (Version 2.10)

   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

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

interface

uses
  uWCXprototypes, uWCXhead, uFileList, dynlibs, Classes, uVFSModule,
  uVFSTypes, fFileOpDlg, Dialogs, DialogAPI, uClassesEx,
  StringHashList, uOSUtils;

Type
  TWCXOperation = (OP_EXTRACT, OP_PACK, OP_DELETE);

  { TWCXHeaderData }

  { Handles THeaderData and THeaderDataEx }
  TWCXHeader = class
  private
    function PCharLToUTF8(CharString: PChar; MaxSize: Integer): UTF8String;

  public
    ArcName: UTF8String;
    FileName: UTF8String;
    Flags,
    HostOS,
    FileCRC,
    FileTime,
    UnpVer,
    Method: Longint;
    FileAttr: TFileAttrs;
    PackSize,
    UnpSize: Int64;
    Cmt: UTF8String;
    CmtState: Longint;

    constructor Create(const Data: PHeaderData); overload;
    constructor Create(const Data: PHeaderDataEx); overload;
    constructor Create; overload; // allows creating empty record
  end;

  
  { TWCXModule }

  TWCXModule = class (TVFSModule)
  private
    FPackerCaps : Integer;

  protected
    FModuleHandle: TLibHandle;  // Handle to .DLL or .so
    FArchiveName: String;

  public
    // module's functions
  //**mandatory:
    OpenArchive : TOpenArchive;
    ReadHeader : TReadHeader;
    ProcessFile : TProcessFile;
    CloseArchive : TCloseArchive;
  //**optional:
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
    // Dialog API
    SetDlgProc: TSetDlgProc;

    constructor Create;
    destructor Destroy; override;

    { Reads WCX header using ReadHeaderEx if available or ReadHeader. }
    function ReadWCXHeader(hArcData: TArcHandle;
                           out HeaderData: TWCXHeader): Integer;

    function OpenArchiveHandle(FileName: String; anOpenMode: Longint; out OpenResult: Longint): TArcHandle;

    function LoadModule(const sName:String):Boolean;override; {Load WCX plugin}
    procedure UnloadModule;override;                          {UnLoad WCX plugin}

    function VFSInit(Data: PtrInt):Boolean;override;
    procedure VFSDestroy;override;
    function VFSCaps : TVFSCaps;override;

    function VFSConfigure(Parent: THandle):Boolean;override;

    function VFSRun(const sName:String):Boolean;override;

    function VFSMisc : PtrUInt;override;
  end;

  { TWCXModuleList }

  TWCXModuleList = class(TStringList)
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
    procedure Load(Ini: TIniFileEx);
    procedure Save(Ini: TIniFileEx);
    function Add(Ext: String; Flags: PtrInt; FileName: String): Integer; reintroduce;
    function FindFirstEnabledByName(Name: String): Integer;

    property FileName[Index: Integer]: String read GetAFileName write SetAFileName;
    property Flags[Index: Integer]: PtrInt read GetAFlags write SetAFlags;
    property Ext[Index: Integer]: String read GetAExt write SetExt;
    property Enabled[Index: Integer]: Boolean read GetAEnabled write SetAEnabled;
  end;

  function GetErrorMsg(iErrorMsg : Integer): String;

implementation

uses Forms, SysUtils, uGlobs, LCLProc, uDCUtils,
     uLng, Controls, fPackInfoDlg, fDialogBox, uGlobsPaths, FileUtil;

const
  WcxIniFileName = 'wcx.ini';

constructor TWCXModule.Create;
begin
  FModuleHandle := 0;
end;

destructor TWCXModule.Destroy;
begin
  UnloadModule;
end;

function TWCXModule.OpenArchiveHandle(FileName: String; anOpenMode: Longint; out OpenResult: Longint): TArcHandle;
var
  ArcFile: tOpenArchiveData;
begin
  if (anOpenMode >= PK_OM_LIST) and (anOpenMode <= PK_OM_EXTRACT) then
  begin
    FillChar(ArcFile, SizeOf(ArcFile), #0);
    ArcFile.ArcName := PAnsiChar(UTF8ToSys(FileName));
    ArcFile.OpenMode := anOpenMode;
    Result := OpenArchive(ArcFile);
    if Result = 0 then
      OpenResult := ArcFile.OpenResult
    else
      OpenResult := E_SUCCESS;
  end
  else
    raise Exception.Create('Invalid WCX open mode');
end;

function TWCXModule.LoadModule(const sName:String):Boolean;
var
  PackDefaultParamStruct : TPackDefaultParamStruct;
  SetDlgProcInfo: TSetDlgProcInfo;
  sPluginDir: WideString;
  sPluginConfDir: WideString;
begin
  FModuleHandle := mbLoadLibrary(sName);
  debugln('loaded ' + sName + ' at ' + hexStr(Pointer(FModuleHandle)));
  if FModuleHandle = 0 then
    Exit(False);

  // mandatory functions
  OpenArchive:= TOpenArchive(GetProcAddress(FModuleHandle,'OpenArchive'));
  ReadHeader:= TReadHeader(GetProcAddress(FModuleHandle,'ReadHeader'));
  ReadHeaderEx:= TReadHeaderEx(GetProcAddress(FModuleHandle,'ReadHeaderEx'));
  ProcessFile:= TProcessFile(GetProcAddress(FModuleHandle,'ProcessFile'));
  CloseArchive:= TCloseArchive(GetProcAddress(FModuleHandle,'CloseArchive'));

  if (OpenArchive = nil) or (ReadHeader = nil) or
     (ProcessFile = nil) or (CloseArchive = nil) then
    begin
      OpenArchive := nil;
      ReadHeader:= nil;
      ProcessFile := nil;
      CloseArchive := nil;
      Result := False;
      Exit;
    end;

  // optional functions
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
  // Dialog API function
  SetDlgProc:= TSetDlgProc(GetProcAddress(FModuleHandle,'SetDlgProc'));

  if Assigned(PackSetDefaultParams) then
    begin
      with PackDefaultParamStruct do
        begin
          Size := SizeOf(PackDefaultParamStruct);
          PluginInterfaceVersionLow := 10;
          PluginInterfaceVersionHi := 2;
          DefaultIniName := gpIniDir + WcxIniFileName;
        end;
      PackSetDefaultParams(@PackDefaultParamStruct);
    end;

  // Dialog API
  if Assigned(SetDlgProc) then
    begin
      sPluginDir := UTF8Decode(ExtractFilePath(sName));
      sPluginConfDir := UTF8Decode(gpIniDir);

      with SetDlgProcInfo do
      begin
        PluginDir:= PWideChar(sPluginDir);
        PluginConfDir:= PWideChar(sPluginConfDir);
        InputBox:= @fDialogBox.InputBox;
        MessageBox:= @fDialogBox.MessageBox;
        DialogBox:= @fDialogBox.DialogBox;
        DialogBoxEx:= @fDialogBox.DialogBoxEx;
        SendDlgMsg:= @fDialogBox.SendDlgMsg;
      end;

      SetDlgProc(SetDlgProcInfo);
    end;

  Result := True;
end;

procedure TWCXModule.UnloadModule;
begin
  if FModuleHandle <> 0 then
  begin
    FreeLibrary(FModuleHandle);
    FModuleHandle := 0;
  end;

  OpenArchive:= nil;
  ReadHeader:= nil;
  ReadHeaderEx:= nil;
  ProcessFile:= nil;
  CloseArchive:= nil;
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
  SetDlgProc:= nil;
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
    else                 Result := IntToStr(iErrorMsg);
  end;
end;

function TWCXModule.VFSInit(Data: PtrInt): Boolean;
begin
  FPackerCaps:= Data;
end;

procedure TWCXModule.VFSDestroy;
begin
  UnloadModule;
end;

function TWCXModule.VFSCaps: TVFSCaps;
begin
  Result := [];
  Include(Result, VFS_CAPS_COPYOUT);
  if Assigned(PackFiles) then
    Include(Result, VFS_CAPS_COPYIN);
  if Boolean(FPackerCaps and PK_CAPS_DELETE) and Assigned(DeleteFiles) then
    Include(Result, VFS_CAPS_DELETE);
end;

function TWCXModule.VFSConfigure(Parent: THandle): Boolean;
begin
  if Assigned(ConfigurePacker) then
    ConfigurePacker(Parent, FModuleHandle);
end;

function GetFileList(var fl:TFileList; Operation: TWCXOperation) : String;
var
  I        : Integer;
  FileName : String;
begin
  Result := '';

  for I := 0 to fl.Count - 1 do
    begin
      // Filenames must be relative to archive root and shouldn't start with path delimiter.
      FileName := ExcludeFrontPathDelimiter(fl.GetItem(I)^.sName);

      // Special treatment of directories.
      if FPS_ISDIR(fl.GetItem(I)^.iMode) then
      begin
        case Operation of
          OP_PACK:
              FileName := IncludeTrailingPathDelimiter(FileName);

          OP_DELETE:
              FileName := IncludeTrailingPathDelimiter(FileName) + '*.*';
        end;
      end;

      Result := Result + FileName + #0;
    end;

  Result := Result + #0;
end;

function TWCXModule.ReadWCXHeader(hArcData: TArcHandle;
                                  out HeaderData: TWCXHeader): Integer;
var
  ArcHeader : THeaderData;
  ArcHeaderEx : THeaderDataEx;
begin
  HeaderData := nil;

  if Assigned(ReadHeaderEx) then
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

function TWCXModule.VFSRun(const sName: String): Boolean;
var
  iCount, I: Integer;
  Header: TWCXHeader;
begin
{
  iCount := FArcFileList.Count - 1;
  for I := 0 to  iCount do
   begin
     Header := TWCXHeader(FArcFileList.Items[I]);
     if PathDelim + Header.FileName = FFolder + sName then
     begin
       Result := ShowPackInfoDlg(Self, Header);
       Exit;
     end;
   end;
   Result:= False;
}
end;

function TWCXModule.VFSMisc: PtrUInt;
begin
  if Assigned(GetPackerCaps) then
    Result := GetPackerCaps()
  else
    Result := 0;
end;

{ TWCXModuleList }

function TWCXModuleList.GetAEnabled(Index: Integer): Boolean;
begin
  Result:= Boolean(Objects[Index]);
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

procedure TWCXModuleList.Load(Ini: TIniFileEx);
var
  I: Integer;
  sCurrPlugin,
  sValue: String;
begin
  Ini.ReadSectionRaw('PackerPlugins', Self);
  for I:= 0 to Count - 1 do
    if Pos('#', Names[I]) = 0 then
      begin
        Enabled[I]:= True;
      end
    else
      begin
        sCurrPlugin:= Names[I];
        sValue:= ValueFromIndex[I];
        Self[I]:= Copy(sCurrPlugin, 2, Length(sCurrPlugin) - 1) + '=' + sValue;
        Enabled[I]:= False;
      end;
end;

procedure TWCXModuleList.Save(Ini: TIniFileEx);
var
 I: Integer;
begin
  Ini.EraseSection('PackerPlugins');
  for I := 0 to Count - 1 do
    begin
      if Boolean(Objects[I]) then
        begin
          Ini.WriteString('PackerPlugins', Names[I], ValueFromIndex[I])
        end
      else
        begin
          Ini.WriteString('PackerPlugins', '#' + Names[I], ValueFromIndex[I]);
        end;
    end;
end;

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

  function Combine64(High, Low: Longint): Int64;
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

constructor TWCXHeader.Create;
begin
end;

function TWCXHeader.PCharLToUTF8(CharString: PChar; MaxSize: Integer): UTF8String;
var
  NameLength: Integer;
  TempString: AnsiString;
begin
  NameLength := strlen(CharString);
  if NameLength > MaxSize then
    NameLength := MaxSize;

  SetString(TempString, CharString, NameLength);
  Result := SysToUTF8(TempString);
end;

end.

unit WfxPlugin;    { Plugin definitions version 2.0 }

interface

uses
  SysUtils {$IFDEF MSWINDOWS}, Windows{$ENDIF};

{ ids for FsGetFile }

const FS_FILE_OK=0;

      FS_FILE_EXISTS=1;

      FS_FILE_NOTFOUND=2;

      FS_FILE_READERROR=3;

      FS_FILE_WRITEERROR=4;

      FS_FILE_USERABORT=5;

      FS_FILE_NOTSUPPORTED=6;

      FS_FILE_EXISTSRESUMEALLOWED=7;

      FS_EXEC_OK=0;

      FS_EXEC_ERROR=1;

      FS_EXEC_YOURSELF=-1;

      FS_EXEC_SYMLINK=-2;

      FS_COPYFLAGS_OVERWRITE=1;

      FS_COPYFLAGS_RESUME=2;

      FS_COPYFLAGS_MOVE=4;

      FS_COPYFLAGS_EXISTS_SAMECASE=8;

      FS_COPYFLAGS_EXISTS_DIFFERENTCASE=16;



{ flags for tRequestProc }

const

  RT_Other=0;

  RT_UserName=1;

  RT_Password=2;

  RT_Account=3;

  RT_UserNameFirewall=4;

  RT_PasswordFirewall=5;

  RT_TargetDir=6;

  RT_URL=7;

  RT_MsgOK=8;

  RT_MsgYesNo=9;

  RT_MsgOKCancel=10;

{ flags for tLogProc }

const msgtype_connect=1;

      msgtype_disconnect=2;

      msgtype_details=3;

      msgtype_transfercomplete=4;

      msgtype_connectcomplete=5;

      msgtype_importanterror=6;

      msgtype_operationcomplete=7;

{ flags for FsStatusInfo }

const FS_STATUS_START=0;

      FS_STATUS_END=1;

      FS_STATUS_OP_LIST=1;

      FS_STATUS_OP_GET_SINGLE=2;

      FS_STATUS_OP_GET_MULTI=3;

      FS_STATUS_OP_PUT_SINGLE=4;

      FS_STATUS_OP_PUT_MULTI=5;

      FS_STATUS_OP_RENMOV_SINGLE=6;

      FS_STATUS_OP_RENMOV_MULTI=7;

      FS_STATUS_OP_DELETE=8;

      FS_STATUS_OP_ATTRIB=9;

      FS_STATUS_OP_MKDIR=10;

      FS_STATUS_OP_EXEC=11;

      FS_STATUS_OP_CALCSIZE=12;

      FS_STATUS_OP_SEARCH=13;

      FS_STATUS_OP_SEARCH_TEXT=14;

      FS_STATUS_OP_SYNC_SEARCH=15;

      FS_STATUS_OP_SYNC_GET=16;

      FS_STATUS_OP_SYNC_PUT=17;

      FS_STATUS_OP_SYNC_DELETE=18;

{Flags for FsExtractCustomIcon}

const FS_ICONFLAG_SMALL=1;

      FS_ICONFLAG_BACKGROUND=2;

      FS_ICON_USEDEFAULT=0;

      FS_ICON_EXTRACTED=1;

      FS_ICON_EXTRACTED_DESTROY=2;

      FS_ICON_DELAYED=3;

const FS_BITMAP_NONE=0;

      FS_BITMAP_EXTRACTED=1;

      FS_BITMAP_EXTRACT_YOURSELF=2;

      FS_BITMAP_EXTRACT_YOURSELF_ANDDELETE=3;

      FS_BITMAP_CACHE=256;

{Flags for crypto callback function}

      FS_CRYPT_SAVE_PASSWORD=1;

      FS_CRYPT_LOAD_PASSWORD=2;

      FS_CRYPT_LOAD_PASSWORD_NO_UI=3; {Load password only if master password has already been entered!}

      FS_CRYPT_COPY_PASSWORD=4;

      FS_CRYPT_MOVE_PASSWORD=5;

      FS_CRYPT_DELETE_PASSWORD=6;

      FS_CRYPTOPT_MASTERPASS_SET=1;   {The user already has a master password defined}

type
  { Unsigned integer with pointer size }
  THandle = {$IFDEF CPU64}QWord{$ELSE}LongWord{$ENDIF};

const
  wfxInvalidHandle: THandle = THandle(-1);

{ Some Windows specific stuff }

const
  MAXDWORD = DWORD($FFFFFFFF);
  FILE_ATTRIBUTE_NORMAL = 128;
  FILE_ATTRIBUTE_DIRECTORY = 16;
  FILE_ATTRIBUTE_REPARSE_POINT = $0400;
  FILE_ATTRIBUTE_UNIX_MODE = $80000000;
  
type
  TInt64Rec = packed record
    case Boolean of
      True : (Value : Int64);
      False : (Low, High : DWORD);
  end;

  BOOL = LongBool;
  HBITMAP = THandle;
  HICON = THandle;
  HWND = THandle;

type
{$IFDEF MSWINDOWS}
  FILETIME = Windows.FILETIME;
{$ELSE}
  FILETIME = packed record
    dwLowDateTime : DWORD;
    dwHighDateTime : DWORD;
  end;
{$ENDIF}
  TFileTime = FILETIME; // for compatibility with all plugins
  PFileTime = ^FILETIME;
  TWfxFileTime = FILETIME;
  PWfxFileTime = ^FILETIME;

{$IFDEF MSWINDOWS}
  WIN32_FIND_DATAA = Windows.WIN32_FIND_DATA;
{$ELSE}
  WIN32_FIND_DATAA = packed record
    dwFileAttributes : DWORD;
    ftCreationTime : TFILETIME;
    ftLastAccessTime : TFILETIME;
    ftLastWriteTime : TFILETIME;
    nFileSizeHigh : DWORD;
    nFileSizeLow : DWORD;
    dwReserved0 : DWORD;
    dwReserved1 : DWORD;
    cFileName : array[0..(MAX_PATH)-1] of CHAR;
    cAlternateFileName : array[0..13] of CHAR;
  end;
{$ENDIF}
  TWin32FindData = WIN32_FIND_DATAA;

{$IFDEF MSWINDOWS}
  WIN32_FIND_DATAW = Windows.WIN32_FIND_DATAW;
{$ELSE}
  WIN32_FIND_DATAW = packed record
    dwFileAttributes : DWORD;
    ftCreationTime : TFILETIME;
    ftLastAccessTime : TFILETIME;
    ftLastWriteTime : TFILETIME;
    nFileSizeHigh : DWORD;
    nFileSizeLow : DWORD;
    dwReserved0 : DWORD;
    dwReserved1 : DWORD;
    cFileName : array[0..(MAX_PATH)-1] of WCHAR;
    cAlternateFileName : array[0..13] of WCHAR;
  end;
{$ENDIF}
  TWin32FindDataW = WIN32_FIND_DATAW;

type

  tRemoteInfo=record

    SizeLow,SizeHigh:longint;

    LastWriteTime:TFileTime;

    Attr:longint;

  end;

  pRemoteInfo=^tRemoteInfo;

type

  tFsDefaultParamStruct=record

    size,

    PluginInterfaceVersionLow,

    PluginInterfaceVersionHi:longint;

    DefaultIniName:array[0..MAX_PATH-1] of char;

  end;

  pFsDefaultParamStruct=^tFsDefaultParamStruct;

{ callback functions }

type

  TProgressProc=function(PluginNr:integer;SourceName,

    TargetName:pchar;PercentDone:integer):integer; stdcall;
  
  TProgressProcW=function(PluginNr:integer;SourceName,

    TargetName:pwidechar;PercentDone:integer):integer; stdcall;  

  TLogProc=procedure(PluginNr,MsgType:integer;LogString:pchar); stdcall;
  
  TLogProcW=procedure(PluginNr,MsgType:integer;LogString:pwidechar); stdcall;

  TRequestProc=function(PluginNr,RequestType:integer;CustomTitle,CustomText,

    ReturnedText:pchar;maxlen:integer):bool; stdcall;
  
  TRequestProcW=function(PluginNr,RequestType:integer;CustomTitle,CustomText,

    ReturnedText:pwidechar;maxlen:integer):bool; stdcall;

  TCryptProc=function(PluginNr,CryptoNumber:integer;mode:integer;ConnectionName,

    Password:pchar;maxlen:integer):integer; stdcall;
    
  TCryptProcW=function(PluginNr,CryptoNumber:integer;mode:integer;ConnectionName,

    Password:pwidechar;maxlen:integer):integer; stdcall;

{ Function prototypes - the callback functions MUST be implemented exactly like this! }

{

function FsInit(PluginNr:integer;pProgressProc:tProgressProc;pLogProc:tLogProc;

                pRequestProc:tRequestProc):integer; stdcall;

function FsInitW(PluginNr:integer;pProgressProcW:tProgressProcW;pLogProcW:tLogProcW;

                pRequestProcW:tRequestProcW):integer; stdcall;

procedure FsSetCryptCallback(CryptProc:TCryptProc;CryptoNr,Flags:integer); stdcall;

procedure FsSetCryptCallbackW(CryptProcW:TCryptProcW;CryptoNr,Flags:integer); stdcall;

function FsFindFirst(path :pchar;var FindData:tWIN32FINDDATA):thandle; stdcall;

function FsFindFirstW(path :pwidechar;var FindData:tWIN32FINDDATAW):thandle; stdcall;

function FsFindNext(Hdl:thandle;var FindData:tWIN32FINDDATA):bool; stdcall;

function FsFindNextW(Hdl:thandle;var FindDataW:tWIN32FINDDATAW):bool; stdcall;

function FsFindClose(Hdl:thandle):integer; stdcall;

function FsMkDir(RemoteDir:pchar):bool; stdcall;

function FsMkDirW(RemoteDir:pwidechar):bool; stdcall;

function FsExecuteFile(MainWin:thandle;RemoteName,Verb:pchar):integer; stdcall;

function FsExecuteFileW(MainWin:thandle;RemoteName,Verb:pwidechar):integer; stdcall;

function FsRenMovFile(OldName,NewName:pchar;Move,OverWrite:bool;

  RemoteInfo:pRemoteInfo):integer; stdcall;

function FsRenMovFileW(OldName,NewName:pwidechar;Move,OverWrite:bool;

  RemoteInfo:pRemoteInfo):integer; stdcall;

function FsGetFile(RemoteName,LocalName:pchar;CopyFlags:integer;

  RemoteInfo:pRemoteInfo):integer; stdcall;

function FsGetFileW(RemoteName,LocalName:pwidechar;CopyFlags:integer;

  RemoteInfo:pRemoteInfo):integer; stdcall;

function FsPutFile(LocalName,RemoteName:pchar;CopyFlags:integer):integer; stdcall;

function FsPutFileW(LocalName,RemoteName:pwidechar;CopyFlags:integer):integer; stdcall;

function FsDeleteFile(RemoteName:pchar):bool; stdcall;

function FsDeleteFileW(RemoteName:pwidechar):bool; stdcall;

function FsRemoveDir(RemoteName:pchar):bool; stdcall;

function FsRemoveDirW(RemoteName:pwidechar):bool; stdcall;

function FsDisconnect(DisconnectRoot:pchar):bool; stdcall;

function FsDisconnectW(DisconnectRoot:pwidechar):bool; stdcall;

function FsSetAttr(RemoteName:pchar;NewAttr:integer):bool; stdcall;

function FsSetAttrW(RemoteName:pwidechar;NewAttr:integer):bool; stdcall;

function FsSetTime(RemoteName:pchar;CreationTime,LastAccessTime,

  LastWriteTime:PFileTime):bool; stdcall;

function FsSetTimeW(RemoteName:pwidechar;CreationTime,LastAccessTime,

  LastWriteTime:PFileTime):bool; stdcall;

procedure FsStatusInfo(RemoteDir:pchar;InfoStartEnd,InfoOperation:integer); stdcall;

procedure FsStatusInfoW(RemoteDir:pwidechar;InfoStartEnd,InfoOperation:integer); stdcall;

procedure FsGetDefRootName(DefRootName:pchar;maxlen:integer); stdcall;

function FsExtractCustomIcon(RemoteName:pchar;ExtractFlags:integer;

  var TheIcon:hicon):integer; stdcall;

function FsExtractCustomIconW(RemoteName:pwidechar;ExtractFlags:integer;

  var TheIcon:hicon):integer; stdcall;

procedure FsSetDefaultParams(dps:pFsDefaultParamStruct); stdcall;

function FsGetPreviewBitmap(RemoteName:pchar;width,height:integer,

  var ReturnedBitmap:hbitmap):integer; stdcall;

function FsGetPreviewBitmapW(RemoteName:pwidechar;width,height:integer,

  var ReturnedBitmap:hbitmap):integer; stdcall;

function FsLinksToLocalFiles:bool; stdcall;

function FsGetLocalName(RemoteName:pchar;maxlen:integer):bool; stdcall;

function FsGetLocalNameW(RemoteName:pwidechar;maxlen:integer):bool; stdcall;

}

{****************************** content plugin part *****************************}

const ft_nomorefields=0;

      ft_numeric_32=1;

      ft_numeric_64=2;

      ft_numeric_floating=3;

      ft_date=4;

      ft_time=5;

      ft_boolean=6;

      ft_multiplechoice=7;

      ft_string=8;

      ft_fulltext=9;

      ft_datetime=10;
      
      ft_stringw=11;

// for ContentGetValue

      ft_nosuchfield=-1;

      ft_fileerror=-2;

      ft_fieldempty=-3;

      ft_ondemand=-4;

      ft_delayed=0;

// for ContentSetValue

      ft_setsuccess=0;

      setflags_first_attribute=1;     {First attribute of this file}

      setflags_last_attribute=2;

      setflags_only_date=4;

      CONTENT_DELAYIFSLOW=1;  // ContentGetValue called in foreground

type tContentDefaultParamStruct=record

      size,

      PluginInterfaceVersionLow,

      PluginInterfaceVersionHi:longint;

      DefaultIniName:array[0..MAX_PATH-1] of char;

    end;

    pContentDefaultParamStruct=^tContentDefaultParamStruct;

type tdateformat=record

       wYear,wMonth,wDay:word;

     end;

     pdateformat=^tdateformat;

type ttimeformat=record

       wHour,wMinute,wSecond:word;

     end;

     ptimeformat=^ttimeformat;

{ Function prototypes: }

{

procedure FsContentGetDetectString(DetectString:pchar;maxlen:integer); stdcall;

function FsContentGetSupportedField(FieldIndex:integer;FieldName:pchar;

  Units:pchar;maxlen:integer):integer; stdcall;

function FsContentGetValue(FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte;

  maxlen,flags:integer):integer; stdcall;

function FsContentGetValueW(FileName:pwidechar;FieldIndex,UnitIndex:integer;FieldValue:pbyte;

  maxlen,flags:integer):integer; stdcall;

procedure FsContentSetDefaultParams(dps:pContentDefaultParamStruct); stdcall;

procedure FsContentStopGetValue(FileName:pchar); stdcall;

procedure FsContentStopGetValueW(FileName:pwidechar); stdcall;

function FsContentGetDefaultSortOrder(FieldIndex:integer):integer; stdcall;

function FsContentGetSupportedFieldFlags(FieldIndex:integer):integer; stdcall;

function FsContentSetValue(FileName:pchar;FieldIndex,UnitIndex,FieldType:integer;

  FieldValue:pbyte;flags:integer):integer; stdcall;

function FsContentSetValueW(FileName:pwidechar;FieldIndex,UnitIndex,FieldType:integer;

  FieldValue:pbyte;flags:integer):integer; stdcall;

function FsContentGetDefaultView(ViewContents,ViewHeaders,ViewWidths,

  ViewOptions:pchar;maxlen:integer):bool; stdcall;

function FsContentGetDefaultViewW(ViewContents,ViewHeaders,ViewWidths,

  ViewOptions:pwidechar;maxlen:integer):bool; stdcall;

}

{ Network API }

const
  FS_NM_ACTION_ADD     = 0;

  FS_NM_ACTION_EDIT    = 1;

  FS_NM_ACTION_DELETE  = 2;

{ Function prototypes: }

{

procedure FsNetworkGetSupportedProtocols(Protocols: PAnsiChar; MaxLen: LongInt); stdcall;

procedure FsNetworkGetSupportedProtocolsW(Protocols: PWideChar; MaxLen: LongInt); stdcall;

function FsNetworkGetConnection(Index: LongInt; Connection: PAnsiChar; MaxLen: LongInt): LongBool; stdcall;

function FsNetworkGetConnectionW(Index: LongInt; Connection: PWideChar; MaxLen: LongInt): LongBool; stdcall;

function FsNetworkManageConnection(Connection: PAnsiChar; Action: LongInt; MaxLen: LongInt): LongBool; stdcall;

function FsNetworkManageConnectionW(Connection: PWideChar; Action: LongInt; MaxLen: LongInt): LongBool; stdcall;

function FsNetworkOpenConnection(Connection: PAnsiChar; RemotePath: PAnsiChar; MaxLen: LongInt): LongBool; stdcall;

function FsNetworkOpenConnectionW(Connection: PWideChar; RemotePath: PWideChar; MaxLen: LongInt): LongBool; stdcall;

}

implementation

end.

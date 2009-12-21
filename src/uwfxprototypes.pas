unit uWFXprototypes;

{$mode objfpc}{$H+}

interface

uses
  WfxPlugin, uTypes;

type
{File system plugins API (version 2.0) for TC}
{R}//Realized
{U}//Unrealized
{Mandatory}
{R}  TFsInit=function(PluginNr:Integer; pProgressProc:tProgressProc; pLogProc:tlogProc; pRequestProc:tRequestProc):integer;stdcall;
{R}  TFsFindFirst=function(path :pchar;var FindData:tWIN32FINDDATA):thandle;stdcall;
{R}  TFsFindNext=function(Hdl:thandle;var FindData:tWIN32FINDDATA):boolean;stdcall;
{R}  TFsFindClose=function(Hdl:thandle):integer;stdcall;
{Optional}
{R}  TFsSetCryptCallback = procedure(pCryptProc:TCryptProc;CryptoNr,Flags:integer); stdcall;
{R}  TFsMkDir = function(RemoteDir:pchar):boolean; stdcall;
{R}  TFsGetFile = function(RemoteName,LocalName:pchar;CopyFlags:integer; RemoteInfo:pRemoteInfo):integer; stdcall;
{R}  TFsPutFile=function(LocalName,RemoteName:pchar;CopyFlags:integer):integer; stdcall;
{R}  TFsDeleteFile=function(RemoteName:pchar):boolean; stdcall;
{R}  TFsRemoveDir=function(RemoteName:pchar):boolean; stdcall;
{R}  TFsStatusInfo = procedure(RemoteDir:pchar;InfoStartEnd,InfoOperation:integer); stdcall;
{R}  TFsSetDefaultParams = procedure (dps:pFsDefaultParamStruct); stdcall;
{R}  TFsExecuteFile=Function(MainWin:thandle;RemoteName,Verb:pchar):integer; stdcall;
{R}  TFsGetDefRootName=procedure (DefRootName:pchar;maxlen:integer); stdcall;
//------------------------------------------------------
{U}  TFsSetAttr=function (RemoteName:pchar;NewAttr:integer):boolean; stdcall;
{U}  TFsSetTime=Function(RemoteName:pchar;CreationTime,LastAccessTime,LastWriteTime:PWinFileTime):boolean; stdcall;
{U}  TFsExtractCustomIcon=function(RemoteName:pchar;ExtractFlags:integer;var TheIcon:hicon):integer; stdcall;
{U}  TFsRenMovFile= function(OldName,NewName:pchar;  Move, OverWrite:boolean; ri:pRemoteInfo):Integer; stdcall;
{U}  TFsDisconnect = function (DisconnectRoot:pchar):boolean; stdcall;
{U}  TFsGetPreviewBitmap = function ( RemoteName:pchar; width,height:integer; ReturnedBitmap:HBITMAP):integer; stdcall;
{U}  TFsLinksToLocalFiles = function:boolean; stdcall;
{U}  TFsGetLocalName = function (RemoteName:pchar;maxlen:integer):boolean; stdcall;
//------------------------------------------------------
{R}  TFsContentPluginUnloading = procedure; stdcall;
{U}  TFsContentGetDetectString = procedure (DetectString:pchar;maxlen:integer); stdcall;
{U}  TFsContentGetSupportedField = function (FieldIndex:integer;FieldName:pchar;  Units:pchar;maxlen:integer):integer; stdcall;
{U}  TFsContentGetValue = function (FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte; maxlen,flags:integer):integer; stdcall;
{U}  TFsContentSetDefaultParams =  procedure (dps:pContentDefaultParamStruct); stdcall;
{U}  TFsContentStopGetValue = procedure (FileName:pchar); stdcall;
{U}  TFsContentGetDefaultSortOrder = function (FieldIndex:integer):integer; stdcall;
{U}  TFsContentGetSupportedFieldFlags = function (FieldIndex:integer):integer; stdcall;
{U}  TFsContentSetValue = function (FileName:pchar;FieldIndex,UnitIndex,FieldType:integer; FieldValue:pbyte;flags:integer):integer; stdcall;
{U}  TFsContentGetDefaultView = function (ViewContents,ViewHeaders,ViewWidths, ViewOptions:pchar;maxlen:integer):boolean; stdcall;
{ Unicode }
     TFsInitW = function(PluginNr:integer;pProgressProcW:tProgressProcW;pLogProcW:tLogProcW; pRequestProcW:tRequestProcW):integer; stdcall;
     TFsFindFirstW = function(path :pwidechar;var FindData:tWIN32FINDDATAW):thandle; stdcall;
     TFsFindNextW = function(Hdl:thandle;var FindDataW:tWIN32FINDDATAW):bool; stdcall;
//------------------------------------------------------
     TFsSetCryptCallbackW = procedure(CryptProcW:TCryptProcW;CryptoNr,Flags:integer); stdcall;
     TFsMkDirW = function(RemoteDir:pwidechar):bool; stdcall;
     TFsExecuteFileW = function(MainWin:thandle;RemoteName,Verb:pwidechar):integer; stdcall;
     TFsRenMovFileW = function(OldName,NewName:pwidechar;Move,OverWrite:bool; RemoteInfo:pRemoteInfo):integer; stdcall;
     TFsGetFileW = function(RemoteName,LocalName:pwidechar;CopyFlags:integer; RemoteInfo:pRemoteInfo):integer; stdcall;
     TFsPutFileW = function(LocalName,RemoteName:pwidechar;CopyFlags:integer):integer; stdcall;
     TFsDeleteFileW = function(RemoteName:pwidechar):bool; stdcall;
     TFsRemoveDirW = function(RemoteName:pwidechar):bool; stdcall;
     TFsDisconnectW = function(DisconnectRoot:pwidechar):bool; stdcall;
     TFsSetAttrW = function(RemoteName:pwidechar;NewAttr:integer):bool; stdcall;
     TFsSetTimeW = function(RemoteName:pwidechar;CreationTime,LastAccessTime, LastWriteTime:PWinFileTime):bool; stdcall;
     TFsStatusInfoW = procedure(RemoteDir:pwidechar;InfoStartEnd,InfoOperation:integer); stdcall;
     TFsExtractCustomIconW = function(RemoteName:pwidechar;ExtractFlags:integer; var TheIcon:hicon):integer; stdcall;
     TFsGetPreviewBitmapW = function(RemoteName:pwidechar;width,height:integer; var ReturnedBitmap:hbitmap):integer; stdcall;
     TFsGetLocalNameW = function(RemoteName:pwidechar;maxlen:integer):bool; stdcall;
//------------------------------------------------------
     TFsContentGetValueW = function(FileName:pwidechar;FieldIndex,UnitIndex:integer;FieldValue:pbyte; maxlen,flags:integer):integer; stdcall;
     TFsContentStopGetValueW = procedure(FileName:pwidechar); stdcall;
     TFsContentSetValueW = function(FileName:pwidechar;FieldIndex,UnitIndex,FieldType:integer; FieldValue:pbyte;flags:integer):integer; stdcall;
     TFsContentGetDefaultViewW = function(ViewContents,ViewHeaders,ViewWidths, ViewOptions:pwidechar;maxlen:integer):bool; stdcall;
//------------------------------------------------------

implementation

end.


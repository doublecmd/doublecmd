unit uWFXprototypes;

{$mode objfpc}{$H+}

interface

uses
  WfxPlugin;

{$IFDEF MSWINDOWS}{$CALLING STDCALL}{$ELSE}{$CALLING CDECL}{$ENDIF}

type
{ File system plugins API (version 2.0) for TC }
{R}  //Realized
{U}  //Unrealized
{ Mandatory }
{R}  TFsInit=function(PluginNr:Integer; pProgressProc:tProgressProc; pLogProc:tlogProc; pRequestProc:tRequestProc):integer;
{R}  TFsFindFirst=function(path :pchar;var FindData:tWIN32FINDDATA):thandle;
{R}  TFsFindNext=function(Hdl:thandle;var FindData:tWIN32FINDDATA):boolean;
{R}  TFsFindClose=function(Hdl:thandle):integer;
{ Optional }
{R}  TFsSetCryptCallback = procedure(pCryptProc:TCryptProc;CryptoNr,Flags:integer);
{R}  TFsMkDir = function(RemoteDir:pchar):boolean;
{R}  TFsGetFile = function(RemoteName,LocalName:pchar;CopyFlags:integer; RemoteInfo:pRemoteInfo):integer;
{R}  TFsPutFile=function(LocalName,RemoteName:pchar;CopyFlags:integer):integer;
{R}  TFsDeleteFile=function(RemoteName:pchar):boolean;
{R}  TFsRemoveDir=function(RemoteName:pchar):boolean;
{R}  TFsStatusInfo = procedure(RemoteDir:pchar;InfoStartEnd,InfoOperation:integer);
{R}  TFsSetDefaultParams = procedure (dps:pFsDefaultParamStruct);
{R}  TFsExecuteFile=Function(MainWin:HWND;RemoteName,Verb:pchar):integer;
{R}  TFsGetDefRootName=procedure (DefRootName:pchar;maxlen:integer);
//------------------------------------------------------
{R}  TFsSetAttr=function (RemoteName:pchar;NewAttr:integer):boolean;
{R}  TFsSetTime=Function(RemoteName:pchar;CreationTime,LastAccessTime,LastWriteTime:PWfxFileTime):boolean;
{U}  TFsExtractCustomIcon=function(RemoteName:pchar;ExtractFlags:integer;var TheIcon:hicon):integer;
{R}  TFsRenMovFile= function(OldName,NewName:pchar;  Move, OverWrite:boolean; ri:pRemoteInfo):Integer;
{U}  TFsDisconnect = function (DisconnectRoot:pchar):boolean;
{U}  TFsGetPreviewBitmap = function ( RemoteName:pchar; width,height:integer; ReturnedBitmap:HBITMAP):integer;
{R}  TFsLinksToLocalFiles = function:boolean;
{R}  TFsGetLocalName = function (RemoteName:pchar;maxlen:integer):boolean;
//------------------------------------------------------
     TFsGetBackgroundFlags = function: integer;
//------------------------------------------------------
{R}  TFsContentPluginUnloading = procedure;
{U}  TFsContentGetDetectString = procedure (DetectString:pchar;maxlen:integer);
{U}  TFsContentGetSupportedField = function (FieldIndex:integer;FieldName:pchar;  Units:pchar;maxlen:integer):integer;
{U}  TFsContentGetValue = function (FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte; maxlen,flags:integer):integer;
{U}  TFsContentSetDefaultParams =  procedure (dps:pContentDefaultParamStruct);
{U}  TFsContentStopGetValue = procedure (FileName:pchar);
{U}  TFsContentGetDefaultSortOrder = function (FieldIndex:integer):integer;
{U}  TFsContentGetSupportedFieldFlags = function (FieldIndex:integer):integer;
{U}  TFsContentSetValue = function (FileName:pchar;FieldIndex,UnitIndex,FieldType:integer; FieldValue:pbyte;flags:integer):integer;
{U}  TFsContentGetDefaultView = function (ViewContents,ViewHeaders,ViewWidths, ViewOptions:pchar;maxlen:integer):boolean;
{ Unicode }
     TFsInitW = function(PluginNr:integer;pProgressProcW:tProgressProcW;pLogProcW:tLogProcW; pRequestProcW:tRequestProcW):integer;
     TFsFindFirstW = function(path :pwidechar;var FindData:tWIN32FINDDATAW):thandle;
     TFsFindNextW = function(Hdl:thandle;var FindDataW:tWIN32FINDDATAW):bool;
//------------------------------------------------------
     TFsSetCryptCallbackW = procedure(CryptProcW:TCryptProcW;CryptoNr,Flags:integer);
     TFsMkDirW = function(RemoteDir:pwidechar):bool;
     TFsExecuteFileW = function(MainWin:HWND;RemoteName,Verb:pwidechar):integer;
     TFsRenMovFileW = function(OldName,NewName:pwidechar;Move,OverWrite:bool; RemoteInfo:pRemoteInfo):integer;
     TFsGetFileW = function(RemoteName,LocalName:pwidechar;CopyFlags:integer; RemoteInfo:pRemoteInfo):integer;
     TFsPutFileW = function(LocalName,RemoteName:pwidechar;CopyFlags:integer):integer;
     TFsDeleteFileW = function(RemoteName:pwidechar):bool;
     TFsRemoveDirW = function(RemoteName:pwidechar):bool;
     TFsDisconnectW = function(DisconnectRoot:pwidechar):bool;
     TFsSetAttrW = function(RemoteName:pwidechar;NewAttr:integer):bool;
     TFsSetTimeW = function(RemoteName:pwidechar;CreationTime,LastAccessTime, LastWriteTime:PWfxFileTime):bool;
     TFsStatusInfoW = procedure(RemoteDir:pwidechar;InfoStartEnd,InfoOperation:integer);
     TFsExtractCustomIconW = function(RemoteName:pwidechar;ExtractFlags:integer; var TheIcon:hicon):integer;
     TFsGetPreviewBitmapW = function(RemoteName:pwidechar;width,height:integer; var ReturnedBitmap:hbitmap):integer;
     TFsGetLocalNameW = function(RemoteName:pwidechar;maxlen:integer):bool;
//------------------------------------------------------
     TFsContentGetValueW = function(FileName:pwidechar;FieldIndex,UnitIndex:integer;FieldValue:pbyte; maxlen,flags:integer):integer;
     TFsContentStopGetValueW = procedure(FileName:pwidechar);
     TFsContentSetValueW = function(FileName:pwidechar;FieldIndex,UnitIndex,FieldType:integer; FieldValue:pbyte;flags:integer):integer;
     TFsContentGetDefaultViewW = function(ViewContents,ViewHeaders,ViewWidths, ViewOptions:pwidechar;maxlen:integer):bool;
//------------------------------------------------------

{$CALLING DEFAULT}

implementation

end.


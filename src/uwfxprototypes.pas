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
{R}  TFsFindFirst=function(path :pansichar;var FindData:tWIN32FINDDATA):thandle;
{R}  TFsFindNext=function(Hdl:thandle;var FindData:tWIN32FINDDATA):bool;
{R}  TFsFindClose=function(Hdl:thandle):integer;
{ Optional }
{R}  TFsSetCryptCallback = procedure(pCryptProc:TCryptProc;CryptoNr,Flags:integer);
{R}  TFsMkDir = function(RemoteDir:pansichar):bool;
{R}  TFsGetFile = function(RemoteName,LocalName:pansichar;CopyFlags:integer; RemoteInfo:pRemoteInfo):integer;
{R}  TFsPutFile=function(LocalName,RemoteName:pansichar;CopyFlags:integer):integer;
{R}  TFsDeleteFile=function(RemoteName:pansichar):bool;
{R}  TFsRemoveDir=function(RemoteName:pansichar):bool;
{R}  TFsStatusInfo = procedure(RemoteDir:pansichar;InfoStartEnd,InfoOperation:integer);
{R}  TFsSetDefaultParams = procedure (dps:pFsDefaultParamStruct);
{R}  TFsExecuteFile=Function(MainWin:HWND;RemoteName,Verb:pansichar):integer;
{R}  TFsGetDefRootName=procedure (DefRootName:pansichar;maxlen:integer);
//------------------------------------------------------
{R}  TFsSetAttr=function (RemoteName:pansichar;NewAttr:integer):bool;
{R}  TFsSetTime=Function(RemoteName:pansichar;CreationTime,LastAccessTime,LastWriteTime:PWfxFileTime):bool;
{U}  TFsExtractCustomIcon=function(RemoteName:pansichar;ExtractFlags:integer;var TheIcon:hicon):integer;
{R}  TFsRenMovFile= function(OldName,NewName:pansichar;  Move, OverWrite:bool; ri:pRemoteInfo):Integer;
{U}  TFsDisconnect = function (DisconnectRoot:pansichar):bool;
{U}  TFsGetPreviewBitmap = function ( RemoteName:pansichar; width,height:integer; ReturnedBitmap:HBITMAP):integer;
{R}  TFsLinksToLocalFiles = function:bool;
{R}  TFsGetLocalName = function (RemoteName:pansichar;maxlen:integer):bool;
//------------------------------------------------------
     TFsGetBackgroundFlags = function: integer;
//------------------------------------------------------
{R}  TFsContentPluginUnloading = procedure;
{U}  TFsContentGetDetectString = procedure (DetectString:pansichar;maxlen:integer);
{U}  TFsContentGetSupportedField = function (FieldIndex:integer;FieldName:pansichar;  Units:pansichar;maxlen:integer):integer;
{U}  TFsContentGetValue = function (FileName:pansichar;FieldIndex,UnitIndex:integer;FieldValue:pbyte; maxlen,flags:integer):integer;
{U}  TFsContentSetDefaultParams =  procedure (dps:pContentDefaultParamStruct);
{U}  TFsContentStopGetValue = procedure (FileName:pansichar);
{U}  TFsContentGetDefaultSortOrder = function (FieldIndex:integer):integer;
{U}  TFsContentGetSupportedFieldFlags = function (FieldIndex:integer):integer;
{U}  TFsContentSetValue = function (FileName:pansichar;FieldIndex,UnitIndex,FieldType:integer; FieldValue:pbyte;flags:integer):integer;
{U}  TFsContentGetDefaultView = function (ViewContents,ViewHeaders,ViewWidths, ViewOptions:pansichar;maxlen:integer):bool;
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


unit uWFXprototypes;

{$mode objfpc}{$H+}

interface

uses
  {Classes, SysUtils,} ufsplugin, uOSUtils;

type
{File system plugins API (version 1.5) for TC}
{R}//Realized
{U}//Unrealized
{Mandatory}
{R}  TFsInit=function(PluginNr:Integer; pProgressProc:tProgressProc; pLogProc:tlogProc; pRequestProc:tRequestProc):integer;stdcall;
{R}  TFsFindFirst=function(path :pchar;var FindData:tWIN32FINDDATA):thandle;stdcall;
{R}  TFsFindNext=function(Hdl:thandle;var FindData:tWIN32FINDDATA):boolean;stdcall;
{R}  TFsFindClose=function(Hdl:thandle):integer;stdcall;
{Optional}
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
{U}  TFsSetTime=Function(RemoteName:pchar;CreationTime,LastAccessTime,LastWriteTime:PFileTime):boolean; stdcall;
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
//------------------------------------------------------

implementation

end.


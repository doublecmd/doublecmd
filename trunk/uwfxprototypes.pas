unit uWFXprototypes;

{$mode objfpc}{$H+}

interface

uses
  {Classes, SysUtils,} ufsplugin;

type
{Mandatory}
  TFsInit=function(PluginNr:Integer; pProgressProc:tProgressProc; pLogProc:tlogProc; pRequestProc:tRequestProc):integer;stdcall;
  TFsFindFirst=function(path :pchar;var FindData:tWIN32FINDDATA):thandle;stdcall;
  TFsFindNext=function(Hdl:thandle;var FindData:tWIN32FINDDATA):longbool;stdcall;
  TFsFindClose=function(Hdl:thandle):integer;stdcall;
{Optional}
  TFsMkDir = function(RemoteDir:pchar):longbool; stdcall;
  TFsGetFile = function(RemoteName,LocalName:pchar;CopyFlags:integer;
                        RemoteInfo:pRemoteInfo):integer; stdcall;
  TFsPutFile=function(LocalName,RemoteName:pchar;CopyFlags:integer):integer; stdcall;
  TFsDeleteFile=function(RemoteName:pchar):longbool; stdcall;
  TFsRemoveDir=function(RemoteName:pchar):longbool; stdcall;

  TFsExtractCustomIcon=function(RemoteName:pchar;ExtractFlags:integer;var TheIcon:hicon):integer; stdcall;
  TFsExecuteFile=Function(MainWin:thandle;RemoteName,Verb:pchar):integer; stdcall;
  TFsGetDefRootName=procedure (DefRootName:pchar;maxlen:integer); stdcall;
  TFsSetAttr=function (RemoteName:pchar;NewAttr:integer):longbool; stdcall;
  TFsSetTime=Function(RemoteName:pchar;CreationTime,LastAccessTime,LastWriteTime:PFileTime):longbool; stdcall;
  TFsStatusInfo = procedure(RemoteDir:pchar;InfoStartEnd,InfoOperation:integer); stdcall;


implementation

end.


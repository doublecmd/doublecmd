library Sample;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
{$IFDEF MSWINDOWS}
   Windows,
{$ENDIF}
{$IFDEF unix}
   baseunix,
{$ENDIF}
  Classes,
  WfxPlugin,
  SysUtils;

{$E wfx}
{$R *.res}

var
  gPluginNr: integer;
  gProgressProc: tProgressProc;
  gLogProc: tLogProc;
  gRequestProc: tRequestProc;

function FsInit(PluginNr:integer;pProgressProc:tProgressProc;pLogProc:tLogProc;
                pRequestProc:tRequestProc):integer; dcpcall;
begin
  gPluginNr:= PluginNr;
  gProgressProc:= pProgressProc;
  gLogProc:= pLogProc;
  gRequestProc:= pRequestProc;
  Result:= 0;
end;

function FsFindFirst(path :pchar;var FindData:tWIN32FINDDATA):thandle; dcpcall;
begin
  FillChar(FindData, SizeOf(FindData), 0);
  FindData.dwFileAttributes :=0; //0 - обычный файл без каких-либо атрибутов
  StrPCopy(FindData.cFileName,'Hello, world.txt'); //имя файла
  Result:= 1985; //функция нормально отработала}
end;

function FsFindNext(Hdl:thandle;var FindData:tWIN32FINDDATA): BOOL; dcpcall;
begin
//  gRequestProc(gPluginNr, RT_URL, nil, nil, nil, 0);
  Result:= False;
end;

function FsFindClose(Hdl:thandle):integer; dcpcall;
begin
  Result:= 0;
end;

function FsRenMovFile(OldName,NewName:pchar;Move,OverWrite:bool;
  RemoteInfo:pRemoteInfo):integer; dcpcall;
begin
  gRequestProc(gPluginNr, RT_MsgOK, OldName, NewName, nil, 0);
  Result:= FS_FILE_OK;
end;

function FsExecuteFile(MainWin:thandle;RemoteName,Verb:pchar):integer; dcpcall;
begin
  gRequestProc(gPluginNr, RT_MsgOK, RemoteName, Verb, nil, 0);
  Result:= FS_EXEC_OK;
end;

exports
      // mandatory
      FsInit,
      FsFindFirst,
      FsFindNext,
      FsFindClose,
      // optional
      FsRenMovFile,
      FsExecuteFile;

begin
end.

library Sample;

{$mode objfpc}{$H+}

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
//{$R *.res}

function FsInit(PluginNr:integer;pProgressProc:tProgressProc;pLogProc:tLogProc;
                pRequestProc:tRequestProc):integer; stdcall;
begin
result:=0;
end;

function FsFindFirst(path :pchar;var FindData:tWIN32FINDDATA):thandle; stdcall;
begin
FindData.dwFileAttributes :=0; //0 - обычный файл без каких-либо атрибутов
StrPCopy(FindData.cFileName,'Hello, world.txt'); //имя файла
result:=0; //функция нормально отработала}
end;

function FsFindNext(Hdl:thandle;var FindData:tWIN32FINDDATA):boolean; stdcall;
begin
 Result:=false;
end;

function FsFindClose(Hdl:thandle):integer; stdcall;
begin
result:=0;
end;


exports
      FsInit,
      FsFindFirst,
      FsFindNext,
      FsFindClose;

begin
end.

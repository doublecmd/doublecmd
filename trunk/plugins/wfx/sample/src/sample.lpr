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

var
  gPluginNr: integer;
  gProgressProc: tProgressProc;
  gLogProc: tLogProc;
  gRequestProc: tRequestProc;

function FsInit(PluginNr:integer;pProgressProc:tProgressProc;pLogProc:tLogProc;
                pRequestProc:tRequestProc):integer; stdcall;
begin
  gPluginNr:= PluginNr;
  gProgressProc:= pProgressProc;
  gLogProc:= pLogProc;
  gRequestProc:= pRequestProc;
  Result:= 0;
end;

function FsFindFirst(path :pchar;var FindData:tWIN32FINDDATA):thandle; stdcall;
begin
  FillChar(FindData, SizeOf(FindData), 0);
  FindData.dwFileAttributes :=0; //0 - обычный файл без каких-либо атрибутов
  StrPCopy(FindData.cFileName,'Hello, world.txt'); //имя файла
  Result:= 1985; //функция нормально отработала}
end;

function FsFindNext(Hdl:thandle;var FindData:tWIN32FINDDATA): BOOL; stdcall;
begin
//  gRequestProc(gPluginNr, RT_URL, nil, nil, nil, 0);
  Result:= False;
end;

function FsFindClose(Hdl:thandle):integer; stdcall;
begin
  Result:= 0;
end;


exports
      FsInit,
      FsFindFirst,
      FsFindNext,
      FsFindClose;

begin
end.

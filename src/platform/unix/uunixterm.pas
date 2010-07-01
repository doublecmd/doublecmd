{
   Double Commander
   -------------------------------------------------------------------------
   Terminal emulator implementation.

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

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

unit uUnixTerm;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, BaseUnix,
   ExtCtrls, LCLProc, cwstring,
   LCLType, Graphics, TermInfo, termio, uTerminal, uOSUtils;

{$IF DEFINED(LINUX)}
{$LINKLIB util} // under Linux forkpty is situated in libutil.a library
{$ENDIF}

const clib = 'c';
      C_stdin   = 0;
      C_stdout  = 1;
      C_stderr  = 2;


const
  {c_cc characters}
  CDISABLE = 255;

           //key // xterm default bindings
  CINTR    = 003; // ^C
  CQUIT    = 034; // ^\
  CERASE   = 177; // ^?
  CKILL    = 025; // ^U
  CEOF     = 004; // ^D
  CSTART   = 021; // ^Q
  CSTOP    = 023; // ^S
  CSUSP    = 032; // ^Z
  CREPRINT = 022; // ^R
  CWERASE  = 027; // ^W
  CLNEXT   = 026; // ^V
  CDISCARD = 017; // ^O

  //disabled
  CTIME    = 0;
  CMIN     = 1;
  CSWTC    = CDISABLE;
  CEOL     = CDISABLE;
  CEOL2    = CDISABLE;


   
const CSIList=
                '@'+ // Вставить N  пустых символов.
                'A'+ //Переместить курсор вверх на N рядов.
                'B'+ //Переместить курсор вниз на N рядов.
                'C'+ //Переместить курсор вправо на N столбцов.
                'D'+ //Переместить курсор влево на N столбцов.
                'E'+ //Переместить курсор вниз на N рядов, в столбец #1.
                'F'+ //Переместить курсор вверх на N рядов, в столбец #1.
                'G'+ //Переместить курсор в указанный столбец текущего ряда.
                'H'+ //Переместить курсор в указанный ряд и столбец (начало в 1,1).
                'J'+ //"Очистить" экран (по умолчанию от курсора до конца экрана). ESC [ 1 J: "очистить" от начала столбца до курсора. ESC [ 2 J: "очистить" весь экран.
                'K'+  //"Очистить" строку (по умолчанию от курсора до ее конца). ESC [ 1 K: "очистить" от начала строки до курсора. ESC [ 2 K: "очистить" всю строку.
                'L'+  //Вставить N пустых строк.
                'M'+ //Удалить N строк.
                'P'+ //Удалить (со смещением в строке) N символов в текущей строке.
                'X'+ //"Очистить" (без смещения в строке) N символов в текущей строке.
                'a'+ //Переместить курсор вправо на N столбцов.
                'c'+ //Ответить ESC [ ? 6 c: `Я являюсь VT102'.
                'd'+ //Переместить курсор в указанный ряд текущего столбца.
                'e'+ //Переместить курсор вниз на N рядов.
                'f'+ //Переместить курсор в указанный ряд и столбец.
                'g'+ //Без параметров: "очистить" текущую позицию табуляции. ESC [ 3 g: удалить все позиции табуляции.
                'h'+ //Режим установки
                'l'+ //Режим сброса
                'm'+  //Установка атрибутов
                'n'+ //Отчет о статусе
                'q'+ //Установить режимы работы индикаторов на клавиатуре. ESC [ 0 q: выключить все индикаторы ESC [ 1 q: включить индикатор "Scroll Lock" ESC [ 2 q: включить индикатор "Num Lock" ESC [ 3 q: включить индикатор "Caps Lock"
                'r'+  //Установить область прокрутки; параметрами будут верхний и нижний ряды.
                's'+ //Сохранить местоположение курсора.
                'u'; //Восстановить местоположение курсора.
              //'`'+ //Переместить курсор в указанный столбец текущего ряда.



const
   NCCS = 32;

type

   Pwinsize = ^winsize;
   winsize = record
        ws_row : word;
        ws_col : word;
        ws_xpixel : word;
        ws_ypixel : word;
     end;


   __pid_t = longint;

   Pcc_t = ^cc_t;
   cc_t = char;

   Pspeed_t = ^speed_t;
   speed_t = dword;

   Ptcflag_t = ^tcflag_t;
   tcflag_t = dword;

   Ptermios = ^termios;
   termios = record
        c_iflag : tcflag_t;
        c_oflag : tcflag_t;
        c_cflag : tcflag_t;
        c_lflag : tcflag_t;
        c_line : cc_t;
        c_cc : array[0..(NCCS)-1] of cc_t;
        c_ispeed : speed_t;
        c_ospeed : speed_t;
     end;

type
   { TUnixTerm }

   TUnixTerm = class(TTerminal)
   private
    FCols,Frows:integer;
    //---------------------
    //---------------------
   public
     //---------------------
     constructor Create;
     destructor Destroy; override;
{    \\---------------------}
      function Read_Pty(var str:UTF8String; const timeout: longint=10): longint; override; // Read info from pty
      function Fork_pty(const rows,cols:integer; const cmd:UTF8string; const params:UTF8string=''):System.THandle; override; //Create new pty and start cmd
      function Write_pty(const str:UTF8string):boolean; override; //write str to pty
      //---------------------
      function SendBreak_pty():boolean; override; // ^C
      function SendSignal_pty(Sig:Cint):boolean; override;
      function SetScreenSize(aCols,aRows:integer):boolean; override;
      function SetCurrentDir(const NewDir: UTF8String): Boolean; override;
      //---------------------
      function KillShell:LongInt; override;
      function CSI_GetTaskId(const buf:UTF8string):integer; override; //get index of sequence in CSILast list
   end;
   
     { TConThread }

  TUnixConThread = class(TConsoleThread)
   private
     procedure AddSymbol;
     procedure CSIProc(NCode, Param: integer; ExParam: integer=0);
     procedure CSI_CaretTo(Y, X: integer);
     procedure CSI_Colors(const Param: integer);
     procedure WriteS(const s: UTF8String);
   protected
     procedure Execute; override;
   public
     constructor Create;
     destructor Destroy; override;
  end;

function forkpty(__amaster:Plongint; __name:Pchar; __termp:Ptermios; __winp:Pwinsize):longint;cdecl;external clib name 'forkpty';
function setenv(__name:Pchar; __value:Pchar; __replace:longint):longint;cdecl;external clib name 'setenv';
function execl(__path:Pchar; __arg:Pchar):longint;cdecl;varargs;external clib name 'execl';

implementation

{ TUnixConThread }

procedure TUnixConThread.WriteS(const s:UTF8String);
begin
if not assigned(FOut) then exit;
//Form1.CmdBox1.StopRead;
FOut.Write(s);
//Form1.CmdBox1.StartRead(clWhite,clBlack,'',clWhite,clBlack);
end;


procedure TUnixConThread.CSI_Colors(const Param:integer);
begin
if not assigned(FOut) then exit;
with FOut do
begin
  case Param of
0: TextColors(clWhite,clBlack);//	сбросить все атрибуты в их значения по умолчанию
1: ;//	установить жирный шрифт
2: ;//	установить более яркий (имитированное цветом на цветном дисплее)
4: ;//	установить подчеркивание (имитированное цветом на цветном дисплее);
        //цвета, используемые для имитации затемнения или подчеркивания, устанавливаются
	//при помощи ESC ] ...
5: ;//	включить мерцание
7: ;//	включить режим инвертированного видео
10: ;//	сбросить выбранное распределение, флаги управления экраном
         //и переключить метафлаг
11: ;//	выбрать null-распределение, установить флаг управления экраном,
	//сбросить переключатель метафлага.
12: ;//	выбрать null-распределение, установить флаг управления экраном,
       //включить переключатель метафлага. Переключение метафлага
       //задает переключение старшего бита в байте
       //до его трансформации согласно таблице распределения.
21: ;//	включить режим нормальной интенсивности (несовместимо с ECMA-48)
22: ;//	выключить режим нормальной интенсивности
24: ;//	выключить подчеркивание
25: ;//	выключить мерцание
27: ;//	выключить инвертированное видео
30:TextColor(clGray) ;//	установить черный цвет символов
31:TextColor(clRed) ;//	        установить красный цвет символов
32:TextColor($0024F947) ;//	установить зеленый цвет символов
33:TextColor($003A85CF) ;//	установить коричневый цвет символов
34:TextColor(clBlue) ;//	установить синий цвет символов
35:TextColor(clPurple) ;//	установить сиреневый цвет символов
36:TextColor(clSkyBlue) ;//	установить голубой цвет символов
37:TextColor(clWhite);//	установить белый цвет символов
38:TextColor(clWhite);//	включить подчеркивание, установить цвет символов по умолчанию
39:TextColor(clWhite) ;//	выключить подчеркивание, установить цвет символов по умолчанию
40:TextBackground(clBlack) ;//	установить черный цвет фона
41:TextBackground(clRed) ;//	установить красный цвет фона
42:TextBackground(clGreen) ;//	установить зеленый цвет фона
43:TextBackground(clRed) ;//	установить коричневый цвет фона
44:TextBackground(clBlue) ;//	установить синий цвет фона
45:TextBackground(clPurple) ;//	установить сиреневый цвет фона
46:TextBackground(clSkyBlue) ;//	установить голубой цвет фона
47:TextBackground(clWhite) ;//	установить белый цвет фона
49:TextBackground(clBlack) ;//	установить цвет фона по умолчанию
  end;
end;
end;

procedure TUnixConThread.CSI_CaretTo(Y,X:integer); //хз x y или y x. Надо проверить.
begin
debugln('  Y: '+inttostr(Y)+'  X: '+inttostr(X));
//Fout.OutY:=Y;
//Fout.OutX:=X;
end;

procedure TUnixConThread.CSIProc(NCode, Param:integer; ExParam:integer=0);
begin
  //debugln('Code:'+Inttostr(NCode)+'  Param: '+inttostr(Param));
  case NCode of
  9:CSI_CaretTo(Param,ExParam);
  24:CSI_Colors(Param);
  end;
end;

constructor TUnixConThread.Create;
begin
  inherited Create(true);
  System.InitCriticalSection(FLock);
  Fterm:=TUnixTerm.Create;
  FRowsCount:=50;
  FColsCount:=100;
end;

destructor TUnixConThread.Destroy;
begin
  FreeAndNil(fTerm);
  System.DoneCriticalSection(FLock);
  inherited Destroy;
end;


procedure TUnixConThread.Execute;
begin
  FShell:=GetShell;
  if length(FShell)=0 then FShell:='/bin/bash';
  if Assigned(fterm) then  Fterm.Fork_pty(FRowsCount,FColsCount,FShell);
  while true do
    begin
     if Assigned(fterm) then
       begin
         if Fterm.Read_Pty(fbuf,0)>0 then
           Synchronize(@AddSymbol)
         else
           Sleep(1);
       end else break;
    end;
end;

//------------------------------------------------------

procedure TUnixConThread.AddSymbol;
var SeqCode,SeqPrm,i,x:integer;
    es,s:UTF8string;
    esnow,CSINow:boolean;
begin

s:='';
es:='';
esnow:=false;
CSInow:=false;

  for i:=1 to length(fbuf) do
    begin
    //разбор
    //------------------------------------------------------
     if esnow then
       begin
         //------------------------------------------------------
         if CSINow then
           begin
            //Пытаемся определить управляющий символ CSI последовательности
             SeqCode:=(fTerm.CSI_GetTaskId(es));
              if SeqCode>0 then
                begin
                  //разбор управляющей последовательности.
                   //------------------------------------------------------
                    WriteS(s);
                    s:='';
                    delete(es,1,1);
                    delete(es,length(es),1);
                    x:=pos(';',es);
                    while x>0 do
                    begin
                      if tryStrToInt(copy(es,1,x-1),SeqPrm) then
                        begin
                          CSIProc(SeqCode,SeqPrm);
                          delete(es,1,x);
                          x:=pos(';',es);
                        end
                      else
                        begin
                          WriteS(copy(es,1,x-1));
                          delete(es,1,x);
                          x:=pos(';',es);
                        end;
                    end;
                    if es<>'' then
                    begin
                      if tryStrToInt(es,SeqPrm) then
                       CSIProc(SeqCode,SeqPrm)
                      else
                        WriteS(es);
                    end;
                   //------------------------------------------------------

                  es:='';
                  esnow:=false;
                  CSINow:=False;
                end
              else
              es:=es+fbuf[i];
           end
         else
           es:=es+fbuf[i];
           //------------------------------------------------------
         end;

    //Начало управляющей последовательности
    if (fbuf[i]=#155) or ((fbuf[i]=#27)) then
      begin
       esnow:=true;
       //Начало CSI последовательности
       if (i<length(fbuf)) and (fbuf[i+1]='[') then
         CSINow:=true;
      end;


    //выбор чарсета. Неактуально....
    if fbuf[i]='%' then
      continue;

    //удалить последний символ
    if fbuf[i]=#8 then
      s:=copy(s,1,length(s)-1);

    //simple text or control symbols (Nither CSI nor escape sequenses)
    if (esnow=false) and ((fbuf[i]>#31) or (fbuf[i]=#13) or (fbuf[i]=#10)) then
     begin
       if fbuf[i]=#10 then
         begin
          if s<>'' then
            if Assigned(FOut) then
            FOut.Write(s+#10);
            s:='';

            continue;
         end;
       if (fbuf[i]=#13) then
         if Assigned(FOut) then FOut.Write(#13);

       s:=s+fbuf[i];
     end;


    //------------------------------------------------------
    end;


  if s<>'' then
    begin
     if Assigned(FOut) then FOut.Write(s);
    end;

end;
//------------------------------------------------------



{ TUnixTerm }

function TUnixTerm.CSI_GetTaskId(const buf:UTF8string):integer;
var Rez,L,R,M:integer;
begin
 result:=0;
 if buf='' then exit;

 if buf[length(buf)]='`' then
 begin
   result:=length(CSIList)+1;
   exit;
 end;


//бинарный поиск
L:=0;
R:=Length(CSIList);
 while (L<=R) do
  begin
    M:=(L+R) div 2;
    Rez:=CompareChar(CSIList[M],buf[length(buf)],1);
    if Rez=0 then
      begin
        Result:=M;
        exit;
      end
    else
     if Rez<0 then
      L:=M+1
     else
      R:=M-1;
  end;
  
 result:=0;
end;

function TUnixTerm.Fork_pty(const rows, cols: integer; const cmd:UTF8string; const params:UTF8string=''): System.THandle;
var ws:TWinSize;
    ChildPid:THandle;
begin
  FCols:=cols;
  Frows:=rows;
  ws.ws_row:=rows;
  ws.ws_col:=cols;
  ws.ws_xpixel:=0;
  ws.ws_ypixel:=0;
  ChildPid:=forkpty(@Fpty,nil,nil,@ws);

  if ChildPid<0 then
  begin
    Result:=-1;
    Exit;
  end;

  if ChildPid=0 then
  begin
    //Child
    setenv('TERM', 'linux', 1);
    execl(pchar(cmd), pchar(params), nil);
    
    //если execl не сработал и новый процесс не подменил форкнутый, то ошибка
    fpWrite(C_stderr, pchar('execl() failed. Command: '+ cmd),length('execl() failed. Command: '+ cmd));
    exit(127);  // error exec'ing
  end;
FChildPid:=ChildPid;
Result:=ChildPid;
end;


function TUnixTerm.Read_Pty(var str:UTF8String; const timeout:longint=10):longint;
var ifs:TFdSet;
    BytesRead:longint;
    buf:array [0..512] of char;
begin

  Result:=0;
  if Fpty<0 then exit;
  //check if pty has new info for us
  fpFD_ZERO(ifs);
  fpFD_SET(Fpty,ifs);
  if FPSelect(fpty+1,@ifs,nil,nil,timeout)<=0 then exit;
  bytesread := fpread(fpty, buf, 512);

  result:=bytesread;
  str:='';
  if bytesread <= 0 then exit;
  str:=copy(buf,0,BytesRead);
end;

function TUnixTerm.Write_pty(const str: UTF8string): boolean;
var BytesWritten:TSize; i:integer;
begin
  i:=1;
  result:=true;
  while i<=length(str) do
  begin
   BytesWritten:=fpwrite(Fpty,str[i],length(str[i]));
    result:=result and (BytesWritten>0);
   i:=i+1;
  end;
end;

function TUnixTerm.SendBreak_pty(): boolean;
begin
   result:=SendSignal_pty(CINTR);
end;

function TUnixTerm.SendSignal_pty(Sig: Cint): boolean;
var BytesWritten:TSize;
begin
  BytesWritten:=fpwrite(Fpty,Sig,sizeof(sig));
  Result := (BytesWritten>0);
end;

function TUnixTerm.SetScreenSize(aCols, aRows: integer): boolean;
var ws:TWinSize;
begin
  ws.ws_row:=aRows;
  ws.ws_col:=aCols;
  ws.ws_xpixel:=0;
  ws.ws_ypixel:=0;

  if FpIOCtl(Fpty,TIOCSWINSZ,@ws)=0 then
    begin
      Result:=true;
      FCols:=aCols;
      Frows:=aRows;
    end
  else Result:=false;
end;

function TUnixTerm.SetCurrentDir(const NewDir: UTF8String): Boolean;
begin
  Result:= Write_pty(' cd "' + NewDir + '"' + #13);
end;

function TUnixTerm.KillShell: LongInt;
begin
  //FchildPid must be >0 in other case all processes in this group will be killed
  if FChildPid>0 then
    result:=fpkill(FChildPid,SIGKILL)
  else
    result:=-1;
end;

constructor TUnixTerm.Create;
var tio:termio.termios;
begin
 TCGetAttr(Fpty,tio);
 tio.c_iflag:=BRKINT or IGNPAR or ICRNL or IXON;
 tio.c_oflag:=OPOST or ONLCR;
 tio.c_cflag:=CS8 or CREAD;
 tio.c_lflag:=ISIG or ICANON or IEXTEN or ECHO or ECHOE or ECHOK or ECHOKE or ECHOCTL;

 tio.c_cc[VINTR]:=CINTR;
 tio.c_cc[VQUIT]:=CQUIT;
 tio.c_cc[VERASE]:=CERASE;
 tio.c_cc[VKILL]:=CKILL;
 tio.c_cc[VSTART]:=CSTART;
 tio.c_cc[VSTOP]:=CSTOP;
 tio.c_cc[VSUSP]:=CSUSP;
 tio.c_cc[VREPRINT]:=CREPRINT;
 tio.c_cc[VDISCARD]:=CDISCARD;
 tio.c_cc[VWERASE]:=CWERASE;
 tio.c_cc[VLNEXT]:=CLNEXT;
 tio.c_cc[VEOF]:=CEOF;
 tio.c_cc[VEOL]:=CEOL;
 tio.c_cc[VEOL2]:=CEOL2;
{$IF NOT DEFINED(DARWIN)}
 tio.c_cc[VSWTC]:=CSWTC;
{$ENDIF}
 tio.c_cc[VMIN]:=CMIN;
 tio.c_cc[VTIME]:=CTIME;

 TCSetAttr(Fpty,TCSANOW,tio);
end;

destructor TUnixTerm.Destroy;
begin
  KillShell;
  inherited Destroy;
end;

end.
{//  thr.Terminal.Write_pty(#27+'[21~'); //F10
//  thr.Terminal.Write_pty(#27+'[D'); //Left
//  thr.Terminal.Write_pty(#27+'[3~'); //delete
}

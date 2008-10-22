unit uterm;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, BaseUnix, unix, errors, //libc,
   ExtCtrls,LCLProc,LCLType,termio,uCmdBox,Graphics,cwstring;

{$L libutil.a}
const clib = 'c';
      C_stdin   = 0;
      C_stdout  = 1;
      C_stderr  = 2;
  
     CSIList=
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
   { Tterm }

   Tterm = class
   private
    FChildPid:THandle;
    Fpty:Longint;

    //---------------------
    //---------------------
   public
     //---------------------
     constructor Create;
     destructor Destroy; override;
{    \\---------------------}
      function Read_Pty(var str:UTF8String; const timeout: longint=10): longint; // Read info from pty
      function Fork_pty(const rows,cols:integer; const cmd:UTF8string; const params:UTF8string=''):THandle; //Create new pty and start cmd
      function Write_pty(const str:UTF8string):boolean; //write str to pty
      function KillShell:LongInt;
      function CSI_GetTaskId(const buf:UTF8string):integer; //get index of sequence in CSILast list
{    //---------------------}
     property ShellPid:THandle read FChildPid;
     property PtyPid:LongInt read Fpty;
     
   end;

  { TConThread }

  TConThread=class (TThread)
   private
     FLock: System.TRTLCriticalSection;
     fTerm: Tterm;
     fbuf:  UTF8String;
     FRowsCount,
     FColsCount:integer;
     procedure AddSymbol;
     procedure CSI_Colors(const Param: integer);
     procedure WriteS(const s: UTF8String);
   protected
     procedure Execute; override;
   public
     CmdBox:TCmdBox;
     constructor Create;
     destructor Destroy; override;
     property Terminal:TTerm read fterm;
     property RowsCount:integer read FRowsCount write FRowsCount;
     property ColsCount:integer read FColsCount write FColsCount;
  end;


function forkpty(__amaster:Plongint; __name:Pchar; __termp:Ptermios; __winp:Pwinsize):longint;cdecl;external clib name 'forkpty';
function setenv(__name:Pchar; __value:Pchar; __replace:longint):longint;cdecl;external clib name 'setenv';
function execl(__path:Pchar; __arg:Pchar):longint;cdecl;varargs;external clib name 'execl';

implementation

{ TConThread }

procedure TConThread.WriteS(const s:UTF8String);
begin
//Form1.CmdBox1.StopRead;
CmdBox.Write(s);
//Form1.CmdBox1.StartRead(clWhite,clBlack,'',clWhite,clBlack);
//Form1.CmdBox1.Refresh;
end;

procedure TConThread.CSI_Colors(const Param:integer);
begin
with CmdBox do
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
31:TextColor(clRed) ;//	установить красный цвет символов
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

//------------------------------------------------------
var bufer:UTF8string;

procedure TConThread.AddSymbol;
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
if fbuf[i]=#13 then
   begin
     { Form1.CmdBox1.StopRead;
      Form1.CmdBox1. Writeln('');
      Form1.CmdBox1.StartRead(clWhite,clBlack,'',clWhite,clBlack);}
//      WriteS(#13);
   end;


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
                  //разбор следует проводить на этой стороне,
                  //ввиду большой вариабельности параметров
                  //---------------------
                  //Form1.escseqlist.Items.Add(es); //debug only

                   case SeqCode of
                   //------------------------------------------------------

                   //Установка атрибутов
                   24:begin
                        WriteS(s);
                        s:='';
                        delete(es,1,1);
                        delete(es,length(es),1);
                        x:=pos(';',es);
                        while x>0 do
                        begin
                          if TryStrToInt(copy(es,1,x-1),SeqPrm) then
                            begin
                             //SeqPrm:=StrToInt(copy(es,1,x-1));
                              CSI_Colors(SeqPrm);
                              delete(es,1,x);
                              x:=pos(';',es);
                            end
                          else
                            begin
                             WriteS(copy(es,1,x));
                             delete(es,1,x);
                             x:=pos(';',es);
                            end;
                        end;
                        if es<>'' then
                        begin
                         if TryStrToInt(es,SeqPrm) then
                       //  SeqPrm:=StrToInt(es);
                         CSI_Colors(SeqPrm)
                         else
                           writes(es);
                        end;
                      end;

                   end; //case
                   //------------------------------------------------------


                  es:='';
                  esnow:=false;
                  CSINow:=False;
                end
              else
              es:=es+fbuf[i];
           end
//          esnow:=false;
//          es:='';
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
//       es:=es+buf[i];
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
         //Form1.CmdBox1.StopRead;
         if s<>'' then
           CmdBox.Writeln(s);
           s:='';
           continue;
         //form1.CmdBox1.StartRead(clWhite,clBlack,'',clWhite,clBlack);
         end else

       s:=s+fbuf[i];
     end;


    //------------------------------------------------------
    end;
if s<>'' then
  begin
   CmdBox.Write(s);
   s:='';
//   Form1.CmdBox1.CaretX:=10;
  end;



//debug
//for i:=1 to length(fbuf) do
// form1.escnotcsi.Items.Add(fbuf[i]+' = '+inttostr(ord(fbuf[i])));
end;
//------------------------------------------------------

procedure TConThread.Execute;
var x:TUTF8char;
begin
  if Assigned(fterm) then  Fterm.Fork_pty(FRowsCount,FColsCount,'/bin/zsh');
  while true do
    begin
     if Assigned(fterm) then
       begin
         if Fterm.Read_Pty(fbuf,0)>0 then
          Synchronize(@AddSymbol);
       end else break;
    end;
end;

constructor TConThread.Create;
begin
  inherited Create(true);
  System.InitCriticalSection(FLock);
  Fterm:=Tterm.Create;
  FRowsCount:=50;
  FColsCount:=100;
end;

destructor TConThread.Destroy;
begin
  FreeAndNil(fTerm);
  System.DoneCriticalSection(FLock);
  inherited Destroy;
end;


{ Tterm }

function Tterm.CSI_GetTaskId(const buf:UTF8string):integer;
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

function Tterm.Fork_pty(const rows, cols: integer; const cmd:UTF8string; const params:UTF8string=''): THandle;
var ws:TWinSize;
    ChildPid:THandle;
begin

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
    execl(Pchar(cmd), Pchar(params),nil);
    
    //если execl не сработал и новый процесс не подменил форкнутый, то ошибка
    fpWrite(C_stderr, pchar('execl() failed. Command: '+ cmd),length('execl() failed. Command: '+ cmd));
    exit(127);  // error exec'ing
  end;
FChildPid:=ChildPid;
Result:=ChildPid;
end;


function Tterm.Read_Pty(var str:UTF8String; const timeout:longint=10):longint;
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

function Tterm.Write_pty(const str: UTF8string): boolean;
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

function Tterm.KillShell: LongInt;
begin
  //FchildPid must be >0 in other case all processes in this group will be killed
  if FChildPid>0 then
    result:=fpkill(FChildPid,SIGKILL)
  else
    result:=-1;
end;

constructor Tterm.Create;
begin

end;

destructor Tterm.Destroy;
begin
  KillShell;

  inherited Destroy;
end;

end.


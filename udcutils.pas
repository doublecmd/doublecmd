{
Double Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : Alexander Koblov (Alexx2000@mail.ru)

Several useful functions

contributors:
Radek Cervinka  <radek.cervinka@centrum.cz>

Part of this code got from http://www.delphirus.com.ru

}

unit uDCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

Function cnvFormatFileSize(iSize:Int64):String;
Function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                           MaxLen: Integer): String;
function FileMaskEquate(F, M: string): boolean;
procedure DivFileName(const sFileName:String; var n,e:String);

implementation
uses
   uGlobs;

Function cnvFormatFileSize(iSize:Int64):String;
var
  d:double;
begin
//   writeln(iSize);
  if gShortFileSizeFormat then
  begin
  // TODo  Giga
    if iSize div (1024*1024) >0 then
    begin
//      writeln('Div:',Trunc(iSize*10 /(1024*1024))/10);
      Result:=FloatToStrF((iSize*10 div (1024*1024))/10, ffFixed, 15, 1)+'M'
    end
    else
    if iSize div 1024 >0 then
    begin
      Result:=FloatToStrF((iSize*10 div 1024)/10, ffFixed, 15, 1)+'K'
    end
    else
      Result:=IntToStr(iSize);
  end
  else
  begin
    d:=iSize;
    Result:=Format('%8.0n',[d]);

  end;
end;
   
{=========================================================}
Function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                                      MaxLen: Integer): String;
{=========================================================}
// "C:\Program Files\Delphi\DDropTargetDemo\main.pas"
// "C:\Program Files\..\main.pas"
Var
  sl: TStringList;
  sHelp, sFile,
  sFirst: String;
  iPos: Integer;

Begin
  sHelp := PathToMince;
  iPos := Pos(PathDelim, sHelp);
  If iPos = 0 Then
  Begin
    Result := PathToMince;
  End
  Else
  Begin
    sl := TStringList.Create;
    // Decode string
    While iPos <> 0 Do
    Begin
      sl.Add(Copy(sHelp, 1, (iPos - 1)));
      sHelp := Copy(sHelp, (iPos + 1), Length(sHelp));
      iPos := Pos(PathDelim, sHelp);
    End;
    If sHelp <> '' Then
    Begin
      sl.Add(sHelp);
    End;
    // Encode string
    sFirst := sl[0];
    sFile := sl[sl.Count - 1];
    sl.Delete(sl.Count - 1);
    Result := '';
    MaxLen := MaxLen - Canvas.TextWidth('XXX');
    if (sl.Count <> 0) and (Canvas.TextWidth(Result + sl[0] + PathDelim + sFile) < MaxLen) then
      begin
        While (sl.Count <> 0) and (Canvas.TextWidth(Result + sl[0] + PathDelim + sFile) < MaxLen) Do
          Begin
            Result := Result + sl[0] + PathDelim;
            sl.Delete(0);
          End;
        If sl.Count = 0 Then
          Begin
            Result := Result + sFile;
          End
        Else
          Begin
            Result := Result + '..' + PathDelim + sFile;
          End;
      end
    else
      If sl.Count = 0 Then
          Begin
            Result := sFirst + PathDelim;
          End
        Else
          Begin
            Result := sFirst + PathDelim + '..' + PathDelim + sFile;
          End;
    sl.Free;
  End;
  //WriteLN('PathX ' , Result);
  if Canvas.TextWidth(Result) > MaxLen + Canvas.TextWidth('XXX') then
       begin
         while Canvas.TextWidth(Result) > MaxLen do
           begin
             Delete(Result, Length(Result), 1);
           end;
         Result := Copy(Result, 1, Length(Result) - 3) + '...';
       end;
End;

procedure DivFileName(const sFileName:String; var n,e:String);
var
  i:Integer;
begin
  for i:= length(sFileName) downto 1 do
    if sFileName[i]='.' then
    begin
//      if i>1 then // hidden files??
      e:=Copy(sFileName,i,Length(sFileName)-i+1);
      n:=Copy(sFileName,1,i-1);
      Exit;
    end;
  e:='';
  n:=sFileName;
end;


{ **** UBPFD *********** by delphibase.endimus.com ****
>> Проверка на соответствие имени файла заданной маске

Зависимости: sysutils
Автор:       Dmitry Raduzhan
Copyright:   Dmitry Raduzhan
Дата:        24 марта 2004 г.
***************************************************** }

function FileMaskEquate(F, M: string): boolean;
var
  Fl, Ml: byte; // length of file name and mask
  Fp, Mp: byte; // pointers
begin
  F := UpperCase(F);
  M := UpperCase(M);
  result := true;
  Fl := length(F);
  Ml := length(M);
  Fp := 1;
  Mp := 1;
  while Mp <= Ml do
  begin // wildcard
    case M[Mp] of //
      '?':
        begin // if one any char
          inc(Mp); // next char of mask
          inc(Fp); // next char of file name
        end; //
      '*':
        begin // if any chars
          if Mp = Ml then
            exit; // if last char in mask then exit
          if M[Mp + 1] = F[Fp] then
          begin // if next char in mask equate char in
            Inc(Mp); // file name then next char in mask and
          end
          else
          begin // else
            if Fp = Fl then
            begin // if last char in file name then
              result := false; // function return false
              exit; //
            end; // else, if not previous, then
            inc(Fp); // next char in file name
          end; //
        end; //
    else
      begin // other char in mask
        if M[Mp] <> F[Fp] then
        begin // if char in mask not equate char in
          result := false; // file name then function return
          exit; // false
        end; // else
        inc(Fp); // next char of mask
        inc(Mp); // next char of file name
      end //
    end;
  end;
end;


end.


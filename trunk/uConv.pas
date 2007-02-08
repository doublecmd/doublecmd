{
Seksi Commander
----------------------------
Variate conversion utils

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

}
unit uConv;

interface

Function cnvFormatFileSize(iSize:Int64):String;

implementation
uses
  uGlobs, SysUtils;

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

end.

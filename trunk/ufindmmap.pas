{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

implementind memory searching with case
and mmap file to memory

contributors:
  Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)
}


unit uFindMmap;

{$mode objfpc}{$H+}

interface

function PosMem(pAdr:PChar; iLength:Integer; const sData:String; bCase:Boolean):Pointer;
function FindMmap(const sFileName:String; const sFindData:String; bCase:Boolean):Boolean;

implementation
uses
  uOSUtils;

function PosMem(pAdr:PChar; iLength:Integer; const sData:String; bCase:Boolean):Pointer;
var
  xIndex:Integer;

function sPos2(pAdr:PChar; const sData:String):Boolean;
var
  i:Integer;
begin
  Result:=False;
  for i:=1 to length(sData) do
  begin
    case bCase of
     False:if UpCase(pAdr^)<>UpCase(sData[i]) then Exit;
     True: if pAdr^<>sData[i] then Exit;
    end;
    inc(pAdr);
  end;
  Result:=True;
end;

begin
  Result:=pointer(-1);
  for xIndex:=0 to iLength-length(sData) do
  begin
    if sPos2(pAdr,sData) then
    begin
      Result:=pAdr;
      Exit;
    end;
    inc(pAdr);
  end;
end;

function FindMmap(const sFileName, sFindData:String; bCase:Boolean):Boolean;
var
  fmr : TFileMapRec;
begin
  Result:=False;

  try
    if MapFile(sFileName, fmr) then
      Result:= PosMem(fmr.MappedFile, fmr.FileSize, sFindData, bCase)<>Pointer(-1);
  finally
    UnMapFile(fmr);
  end;
end;

end.

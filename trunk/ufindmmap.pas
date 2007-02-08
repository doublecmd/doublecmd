{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

implementind memory searching with case
and mmap file to memory

contributors:

}


unit uFindMmap;

{$mode objfpc}{$H+}

interface

function PosMem(pAdr:PChar; iLength:Integer; const sData:String; bCase:Boolean):Pointer;
function FindMmap(const sFileName:String; const sFindData:String; bCase:Boolean):Boolean;

implementation
uses
  Libc;
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
  fd:Integer;
  pmmap:Pointer;
  fs:Integer;
  stat:_stat64;
begin

  Result:=False;
  pmmap:=nil;
  fs:=0;
  fd:=Libc.Open(PChar(sFileName), O_RDONLY);
  if fd=-1 then Exit;
  try
    if fstat64(fd, stat) <> 0 then Exit;
    fs := stat.st_size;
    pmmap:=mmap(nil,fs,PROT_READ, MAP_PRIVATE,fd,0 );
    if Integer(Pmmap)=-1 then Exit;

    Result:= PosMem(pmmap,fs,sFindData,bCase)<>Pointer(-1);
  finally
    Libc.__close(fd);
    if assigned(pmmap) then
      munmap(pmmap,fs);
  end;
end;



end.

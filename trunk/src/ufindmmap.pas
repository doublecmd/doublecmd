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

type
  TAbortFunction = function: Boolean of object;

function PosMem(pAdr:PChar; iLength:Integer; const sData:String; bCase:Boolean):Pointer;

{en
   Searches a file for a string using memory mapping.

   @param(sFileName   File to search in.)
   @param(sFindData   String to search for.)
   @param(bCase       If @true the search is case-sensitive.)
   @param(Abort       This function is called repeatedly during searching.
                      If it returns @true the search is aborted.)

   @returns(-1 in case of error
     @br     0 if the string wasn't found
     @br     1 if the string was found)
}
function FindMmap(const sFileName:String; const sFindData:String; bCase:Boolean;
                  Abort: TAbortFunction):Integer;

implementation
uses
  uOSUtils;

function PosMem(pAdr:PChar; iLength:Integer; const sData:String; bCase:Boolean):Pointer;
var
  xIndex:Integer;
  DataLength: Integer;

  function sPos2(pAdr:PChar):Boolean;
  var
    i:Integer;
  begin
    Result:=False;
    for i:=1 to DataLength do
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
  DataLength := Length(sData);
  for xIndex:=0 to iLength - DataLength do
  begin
    if sPos2(pAdr) then
    begin
      Result:=pAdr;
      Exit;
    end;
    inc(pAdr);
  end;
end;

function FindMmap(const sFileName, sFindData:String; bCase:Boolean;
                  Abort: TAbortFunction):Integer;

  function PosMem(pAdr:PChar; iLength:Integer):Pointer;
  var
    xIndex:Integer;
    DataLength: Integer;

    function sPos(pAdr:PChar):Boolean;
    var
      i:Integer;
    begin
      Result:=False;
      for i:=1 to DataLength do
      begin
        case bCase of
         False:if UpCase(pAdr^)<>UpCase(sFindData[i]) then Exit;
         True: if pAdr^<>sFindData[i] then Exit;
        end;
        inc(pAdr);
      end;
      Result:=True;
    end;

  begin
    Result:=pointer(-1);
    DataLength := Length(sFindData);

    for xIndex:=0 to iLength - DataLength do
    begin
      if sPos(pAdr) then
      begin
        Result:=pAdr;
        Exit;
      end;
      inc(pAdr);

      if Abort() then
        Exit;
    end;
  end;

var
  fmr : TFileMapRec;
begin
  Result := -1;

  if MapFile(sFileName, fmr) then
  begin
    try
      begin
        if PosMem(fmr.MappedFile, fmr.FileSize) <> Pointer(-1) then
          Result := 1
        else
          Result := 0;
      end;
    finally
      UnMapFile(fmr);
    end;
  end;
end;

end.

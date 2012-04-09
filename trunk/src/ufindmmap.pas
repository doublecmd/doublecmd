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

{en
   Searches data in memory for a string.

   @param(pDataAddr           Pointer to the beginning of the data buffer.)
   @param(iDataLength         Length of the data buffer in bytes.)
   @param(iStartPos           Position in the buffer from which to begin search.)
   @param(sSearchText         Text that is searched for in the data buffer.)
   @param(bCaseSensitive      If @true the search is case sensitive.)
   @param(bSearchBackwards    If @true the search is done in iStartPos..0.
                              If @false the search is done in iStartPos..(iLength-1).)
   @returns(If the string was not found it returns -1.
            If the string was found it returns pointer to the data buffer
            where the searched text begins.)

}
function PosMem(pDataAddr: PChar; iDataLength, iStartPos: PtrInt; const sSearchText: String;
                bCaseSensitive: Boolean; bSearchBackwards: Boolean): Pointer;

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
  DCOSUtils;

function PosMem(pDataAddr: PChar; iDataLength, iStartPos: PtrInt; const sSearchText: String;
                bCaseSensitive: Boolean; bSearchBackwards: Boolean): Pointer;
var
  SearchTextLength: Integer;

  function sPos2(pAdr: PChar):Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to SearchTextLength do
    begin
      case bCaseSensitive of
       False: if UpCase(pAdr^) <> UpCase(sSearchText[i]) then Exit; // Only for Ansi
       True : if pAdr^ <> sSearchText[i] then Exit;
      end;
      Inc(pAdr);
    end;
    Result:=True;
  end;

var
  pCurrentAddr, pEndAddr: PAnsiChar;
begin
  Result := Pointer(-1);

  SearchTextLength := Length(sSearchText);
  if (SearchTextLength <= 0) or (iDataLength <= 0) then
    Exit;

  pCurrentAddr := pDataAddr + iStartPos;
  pEndAddr := pDataAddr + iDataLength - SearchTextLength;

  if bSearchBackwards and (pCurrentAddr > pEndAddr) then
    // Move to the first possible position for searching backwards.
    pCurrentAddr := pEndAddr;

  if (pEndAddr < pDataAddr) or (pCurrentAddr < pDataAddr) or (pCurrentAddr > pEndAddr) then
    Exit;

  while True do
  begin
    if (pCurrentAddr > pEndAddr) or (pCurrentAddr < pDataAddr) then
      Exit;

    if sPos2(pCurrentAddr) then
    begin
      Result := pCurrentAddr;
      Exit;
    end;

    case bSearchBackwards of
      False: Inc(pCurrentAddr);
      True : Dec(pCurrentAddr);
    end;
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
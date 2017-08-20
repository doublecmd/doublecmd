{
implementing memory searching with case (any single-byte encoding)
and mmap file to memory
based on ufindmmap.pas by radek.cervinka@centrum.cz
}

unit uFindByrMr;

{$mode objfpc}{$H+}

interface

type
  TAbortFunction = function: Boolean of object;
  TRecodeTable = array[0..255] of byte;

function PosMemBoyerMur(pAdr: PChar; iLength: Integer; const sFindData: String;
                        RecodeTable: TRecodeTable): Integer;
{en
   Searches a file for a string using memory mapping.

   @param(sFileName   File to search in.)
   @param(sFindData   String to search for.)
   @param(RecodeTable table for case-insensitive compare)
   @param(Abort       This function is called repeatedly during searching.
                      If it returns @true the search is aborted.)

   @returns(-1 in case of error
     @br     0 if the string wasn't found
     @br     1 if the string was found)
}
function FindMmapBM(const sFileName: String; const sFindData: String;
                    RecodeTable: TRecodeTable; Abort: TAbortFunction):Integer;

{en
   Initializes table for recode from different encodings.

   @param(Encoding            Name of encoding.)
   @param(bCaseSensitive      If @true the search is case sensitive.)
   @returns(TRecodeTable array to use in FindMmap)

}
function InitRecodeTable(Encoding: String; bCaseSensitive: Boolean): TRecodeTable;

implementation

uses
  DCOSUtils, LConvEncoding, LazUTF8, uConvEncoding;

type
  TIntArray = array of Integer;

function InitRecodeTable(Encoding:string; bCaseSensitive: Boolean): TRecodeTable;
var i:byte;
    c:string;
begin
  for i:=0 to 255 do
   begin
     if bCaseSensitive then
       Result[i]:=i
     else
     begin
       c:=ConvertEncoding(chr(i), Encoding, EncodingUTF8);
       c:=UTF8UpperCase(c);
       c:=ConvertEncoding(c, EncodingUTF8, Encoding);
       if length(c)>0 then Result[i]:=ord(c[1]);
     end;
   end;
end;

function PosMemBoyerMur(pAdr: PChar; iLength: Integer; const sFindData: String;
                        RecodeTable: TRecodeTable):Integer;

  function prefixFunc(s:string):TIntArray;
  var k,i:Integer;
  begin
    SetLength(Result, Length(s)+1);
    Result[0] := 0;
    Result[1] := 0;
    k := 0;
    for i := 2 to Length(s) do
    begin
      while (k > 0) and (s[k+1] <> s[i]) do
        k := Result[k];
      if s[k+1] = s[i] then Inc(k);
      Result[i] := k;
    end;
  end;

var StopTable:array[0..255] of byte;
    prefTable,pf1,pf2:TIntArray;
    i,j,len:Integer;
    curPos,curCharPos:Integer;
    encStr,rvrsStr:string;
    curChar:byte;
begin
  Result:=-1;
  len:=Length(sFindData);
  encStr:='';
  for i:=1 to len do
    encStr:=encStr+chr(RecodeTable[ord(sFindData[i])]);
  rvrsStr:='';
  for i:=len downto 1 do
    rvrsStr:=rvrsStr+encStr[i];
  for i:=0 to 255 do
    StopTable[i]:=0;
  for i:=len-1 downto 1 do
    if StopTable[ord(encStr[i])]=0 then
      StopTable[ord(encStr[i])]:=i;
  //Calc prefix table
  pf1:=prefixFunc(encStr);
  pf2:=prefixFunc(rvrsStr);
  setLength(prefTable,len+1);
  for j:=0 to len do
    prefTable[j]:= len - pf1[len];
  for i:=1 to len do
  begin
    j:= len - pf2[i];
    if i - pf2[i] < prefTable[j] then
      prefTable[j]:= i - pf2[i];
  end;

  curPos:=0;
  while curPos<=iLength-len do
  begin
    curCharPos:=len;
    curChar:=RecodeTable[ord((pAdr+curPos+curCharPos-1)^)];
    while (curCharPos>0) do
    begin
      if (curChar<>byte(encStr[curCharPos])) then break;
      dec(curCharPos);
      if curCharPos>0 then
        curChar:=RecodeTable[ord((pAdr+curPos+curCharPos-1)^)];
    end;
    if curCharPos=0 then
    begin//found
      Result:=curPos;
      exit;
    end
    else
    begin//shift
      if curCharPos=len then
        curPos:=curPos+len-StopTable[curChar]
      else
        curPos:=curPos+prefTable[curCharPos];
    end
  end;
end;

function FindMmapBM(const sFileName, sFindData: String; RecodeTable: TRecodeTable;
                    Abort: TAbortFunction):Integer;
var
  fmr : TFileMapRec;
begin
  Result := -1;

  if MapFile(sFileName, fmr) then
  begin
    try
      begin
        if PosMemBoyerMur(fmr.MappedFile, fmr.FileSize, sFindData, RecodeTable) <> -1 then
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

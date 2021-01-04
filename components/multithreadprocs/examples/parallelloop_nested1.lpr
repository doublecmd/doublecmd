{ Example for a parallel loop with MTProcs.

  Copyright (C) 2017 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
program parallelloop_nested1;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Classes, SysUtils, Math, MTProcs;


function FindBestParallel(aList: TList; aValue: Pointer): integer;
var
  BlockSize: PtrInt;
  Results: array of integer;

  procedure InParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i, StartIndex, EndIndex: PtrInt;
  begin
    Results[Index]:=-1;
    StartIndex:=Index*BlockSize;
    EndIndex:=Min(StartIndex+BlockSize,aList.Count);
    //if MainThreadID=GetCurrentThreadId then
    //  writeln('FindBestParallel Index=',Index,' StartIndex=',StartIndex,' EndIndex=',EndIndex);
    for i:=StartIndex to EndIndex-1 do begin
      if aList[i]=aValue then  // imagine here an expensive compare function
        Results[Index]:=i;
    end;
  end;

var
  Index: integer;
  BlockCount: PtrInt;
begin
  ProcThreadPool.CalcBlockSize(aList.Count,BlockCount,BlockSize);
  SetLength(Results,BlockCount);
  //writeln('FindBestParallel BlockCount=',BlockCount,' List.Count=',aList.Count,' BlockSize=',BlockSize);
  ProcThreadPool.DoParallelNested(@InParallel,0,BlockCount-1);
  // collect results
  Result:=-1;
  for Index:=0 to BlockCount-1 do
    if Results[Index]>=0 then
      Result:=Results[Index];
end;

function FindBestSingleThreaded(List: TList; Value: Pointer): integer;
var
  i: integer;
begin
  Result:=-1;
  i:=0;
  while i<List.Count do begin
    if List[i]=Value then  // imagine here an expensive compare function
      Result:=i;
    inc(i);
  end;
end;

var
  List: TList;
  i: Integer;
begin
  List:=TList.Create;
  for i:=0 to 100000000 do
    List.Add(Pointer(i));
  writeln('searching ...');
  i:=FindBestParallel(List,Pointer(List.Count-2));
  writeln('parallel search i=',i);
  i:=FindBestSingleThreaded(List,Pointer(List.Count-2));
  writeln('linear search i=',i);
end.


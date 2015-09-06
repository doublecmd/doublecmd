{ Example for a parallel loop with MTProcs.

  Copyright (C) 2009 Mattias Gaertner mattias@freepascal.org

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
program ParallelLoop1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Classes, SysUtils, MTProcs;

type
  TFindBestData = record
    List: TList;
    Value: Pointer;
    BlockCount: integer;
    Results: array of integer;
  end;
  PFindBestData = ^TFindBestData;

procedure FindBestParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
  i: integer;
begin
  with PFindBestData(Data)^ do begin
    Results[Index]:=-1;
    i:=Index;
    while i<List.Count-1 do begin
      if List[i]=Value then  // hier wuerde die teure Vergleichsoperation stehen
        Results[Index]:=i;
      inc(i,BlockCount);
    end;
  end;
end;

function FindBest(aList: TList; aValue: Pointer): integer;
var
  Index: integer;
  Data: TFindBestData;
begin
  with Data do begin
    List:=aList;
    Value:=aValue;
    BlockCount:=ProcThreadPool.MaxThreadCount;
    SetLength(Results,BlockCount);
    ProcThreadPool.DoParallel(@FindBestParallel,0,BlockCount-1,@Data);
    // Ergebnisse zusammenfassen
    Result:=-1;
    for Index:=0 to BlockCount-1 do
      if Results[Index]>=0 then
        Result:=Results[Index];
  end;
end;

function FindBest1(List: TList; Value: Pointer): integer;
var
  i: integer;
begin
  Result:=-1;
  i:=0;
  while i<List.Count do begin
    if List[i]=Value then // hier wuerde die teure Vergleichsoperation stehen
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
  i:=FindBest(List,Pointer(9999));
  //i:=FindBest1(List,Pointer(9999));
  writeln('i=',i);
end.


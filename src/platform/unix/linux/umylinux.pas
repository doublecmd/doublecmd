{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific LINUX functions.

   Copyright (C) 2023 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uMyLinux;

{$mode delphi}

interface

uses
  SysUtils;

function GetFreeMem(out MemFree, MemTotal: Int64): Boolean;

implementation

uses
  DCStrUtils;

function Convert(const S: String; Index: Integer): Int64;
var
  V: String;
  K: Integer;
begin
  K:= 1;
  V:= Trim(Copy(S, Index, MaxInt));
  while (V[K] in ['0'..'9']) do Inc(K);
  Result:= StrToInt64Def(Copy(V, 1, K - 1), 0);
  V:= Trim(Copy(V, K, MaxInt));
  if Length(V) > 0 then
  begin
    case LowerCase(V[1]) of
      'k': Result:= Result * 1024;
      'm': Result:= Result * 1024 * 1024;
      'g': Result:= Result * 1024 * 1024 * 1024;
    end;
  end;
end;

function GetFreeMem(out MemFree, MemTotal: Int64): Boolean;
var
  S: String;
  F: TextFile;
  Count: Integer = 0;
  MemAvailable: Int64;
begin
  try
    AssignFile(F, '/proc/meminfo');
    try
      Reset(F);
      repeat
        ReadLn(F, S);
        if StrBegins(S, 'MemTotal:') then
        begin
          Inc(Count);
          MemTotal:= Convert(S, 10);
        end
        else if StrBegins(S, 'MemFree:') then
        begin
          Inc(Count);
          MemFree:= Convert(S, 9);
        end
        else if StrBegins(S, 'MemAvailable:') then
        begin
          Inc(Count);
          MemAvailable:= Convert(S, 14);
        end;
      until EOF(F) or (Count = 3);
      if MemAvailable < MemTotal then
      begin
        MemFree:= MemAvailable;
      end;
      Result:= (Count = 3);
    finally
      System.Close(F);
    end;
  except
    Result:= False;
  end;
end;

end.

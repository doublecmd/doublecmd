{
    Filesystem traffic generator
    -------------------------------------------------------------------------
    Creates, modifies, removes files, quickly and in large quantities.

    Useful for testing how a program behaves when there's a lot of traffic
    happening on the file system.

    Copyright (C) 2010-2012  Przemysław Nagay (cobines@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

program fsgenerator;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Windows;

var
  Path: UTF8String;
  fs: TFileStream;
  filenames: TStringList;
  nr: Integer;
  buffer: array[0..16383] of byte;

procedure GenNames;
var
  i, j: Integer;
  name: String;
begin
  for i := 0 to Random(1000) do
  begin
    name := '';
    for j := 0 to random(100) do
      name := name + chr(random(ord('z') - ord('a')) + ord('a'));
    filenames.Add(name);
  end;
end;

function RandomName: String;
begin
  Result := Path + filenames[Random(Filenames.Count)];
end;

procedure Create(name: String);
begin
  fs := TFileStream.Create(name, fmCreate);
  fs.Write(buffer, random(sizeof(buffer)));
  fs.Free;
end;

procedure Modify(name: String);
var
  P: Int64;
  count: Int64;
  Mode: Word;
  size: int64;
begin
  if not FileExists(name) then
    mode := fmCreate
  else
    mode := fmOpenReadWrite;

  fs := TFileStream.Create(Name, mode);
  if mode = fmCreate then
  begin
    fs.Write(buffer, random(sizeof(buffer)));
    fs.Seek(0, soBeginning);
  end;

  size := fs.size;
  p := random(size);
  fs.Seek(p, soBeginning);
  count := min(sizeof(buffer),random(size-p));
  fs.Write(buffer, count);
  //writeln('writing ',count, ' p=',p,' size=',size);
  fs.Free;
end;

procedure Delete(name: String);
begin
  if FileExists(Name) then
    Sysutils.DeleteFile(Name);
end;

begin
  if Paramcount = 0 then
  begin
    WriteLn('File system traffic generator.');
    WriteLn('Creates, modifies, removes files, quickly and in large quantities.');
    Writeln;
    WriteLn('Usage:');
    WriteLn(ExtractFileName(ParamStr(0)) + ' <destination_path>');
    Exit;
  end;

  FileNames := TStringList.Create;
  GenNames;
  Path := IncludeTrailingPathDelimiter(ParamStr(1));
  ForceDirectories(Path);

  WriteLn('Starting changing ', Path);
  while True do
  begin
    case Random(6) of
      0: Sleep(10);
      1: Modify(RandomName);
      2: Create(RandomName);
      3: Modify(RandomName);
      4: Delete(RandomName);
      5: Modify(RandomName);
    end;

    Sleep(10);

    if (GetKeyState(VK_SPACE) < 0) or
       (GetKeyState(VK_SHIFT) < 0) or
       (GetKeyState(VK_ESCAPE) < 0) then
      Break;
  end;

  WriteLn('Finished changing');
  Filenames.Free;
end.


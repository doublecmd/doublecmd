{
    This file is part of the chelinfo library.

    Copyright (c) 2008 by Anton Rzheshevski

    Dwarf LineInfo Extractor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    2008, Anton Rzheshevski aka Cheb:
    Like dr. Frankenshtein I sewn this library together
    from the dead meat of the the FPC RTL modules
    lineinfo.pp and lnfodwrf.pp.
    These (as of Jan. 2008 / FPC 2.2.0) both didn't work
    and had several limitations (e.g. inability to be used
    from a DLL)

    }

{note: DON'T FORGET to compile your program with the -gw key
  Lazarus: you must type it in
    Project -> Compiler Options -> Other -> User parameters
}

{$mode delphi}
{$longstrings on}
{$ifndef unix}
  {$apptype console}
{$endif}

//extracts the line info in the dwarf format from the executable


program extractdwrflnfo;

uses
  SysUtils, Classes, un_xtrctdwrflnfo, zstream;

var
  _dwarf: pointer;
  DwarfSize, CompressedDwarfSize: QWord;
  base_addr: QWord;
  f: TFileStream;
  CS: TCompressionStream;
  dllname, iname: ansistring;
begin

  if Paramcount = 0 then
  begin
    WriteLn('Usage: ' + ExtractFileName(GetModuleName(0)) + ' <executable name>');
    exit;
  end;

  dllname:= ParamStr(1);

  WriteLn('Extracting Dwarf line info from ', dllname);
  try
   iname:= DlnNameByExename(dllname);

   if ExtractDwarfLineInfo(dllname, _dwarf, DwarfSize, base_addr) then begin

      f:= TFileStream.Create(iname , fmCreate);
      CS:= TCompressionStream.Create(clMax, f);
      CS.Write(dwarfsize, sizeof(dwarfsize));  // 8 bytes (QWORD)
      CS.Write(base_addr, sizeof(base_addr));  // 8 bytes (QWORD)
      CS.Write(_dwarf^, dwarfsize);
      CS.Free;
      CompressedDwarfSize := f.Size;
      f.free;

      WriteLn('Ok, saved ', CompressedDwarfSize, ' bytes to ', iname);
    end
    else begin
      if FileExists(iname) then DeleteFile(iname);
      WriteLn('Error: ' + ExtractDwarfLineInfoError);
    end;
  except
    WriteLn((ExceptObject as Exception).Message);
  end;
end.


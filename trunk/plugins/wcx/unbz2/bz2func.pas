{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for extract *.bz2 archives


   Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)

   based on:
     pasbzip.pas from FPC sources

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}


unit bz2func;

interface
uses uWCXhead;

{Mandatory functions}
function OpenArchive (var ArchiveData : tOpenArchiveData) : THandle;stdcall;
function ReadHeader (hArcData : THandle; var HeaderData : THeaderData) : Integer;stdcall;
function ProcessFile (hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;stdcall;
function CloseArchive (hArcData : THandle) : Integer;stdcall;
procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc : TChangeVolProc);stdcall;
procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc : TProcessDataProc);stdcall;
{Optional functions}


implementation
uses bzip2, SysUtils, objects;

var
  sArcName : String;
  Count : Integer = 0;
  ProcessDataProc : TProcessDataProc;


function ExtractOnlyFileName(const FileName: string): string;
var
 iDotIndex,
 I: longint;
begin
  (* Find a dot index *)
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['.', '/', '\', ':']) do Dec(I);
  if (I > 0) and (FileName[I] = '.') then
     iDotIndex := I
  else
     iDotIndex := MaxInt;
  (* Find file name index *)
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
  Result := Copy(FileName, I + 1, iDotIndex - I - 1);
end;

  
function OpenArchive (var ArchiveData : tOpenArchiveData) : THandle;
begin
  if FileExists(ArchiveData.ArcName) then
    begin
      sArcName := ArchiveData.ArcName;
      Count := 0;
      Result := 1985;
    end
  else
    Result := E_EOPEN;
end;

function ReadHeader (hArcData : THandle; var HeaderData : THeaderData) : Integer;
var
  sr : TSearchRec;
begin
  if Count > 0 then
    begin
      Result := E_END_ARCHIVE;
      exit;
    end;

  with HeaderData do
    begin
      FindFirst(sArcName, faAnyFile, sr);
      FileName := ExtractOnlyFileName(sArcName);
      PackSize := sr.Size;
      UnpSize  := sr.Size; // we don't know real file size
      FileTime := sr.Time;
      FileAttr := sr.Attr;
      FindClose(sr);
    end;
  Result := 0;
end;

function ProcessFile (hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;
var
  infile,outfile:Tbufstream;
  decoder:Tbzip2_decode_stream;
  a:array[1..4096] of byte;
  i,readsize:cardinal;
  sOutputFileName : String;
  iLastPos : LongInt;
begin

  case Operation of
  PK_TEST:
    begin

    end;


  PK_EXTRACT:
    begin

      sOutputFileName := DestPath;
      if sOutputFileName <> '' then
        sOutputFileName := sOutputFileName + ExtractOnlyFileName(sArcName)
      else
        sOutputFileName := DestName;
      {  // Debug
      assign(output, DestPath + 'unbz2.log');
      rewrite(output);
      }
      begin
        infile.init(sArcName,stopenread,4096);
        outfile.init(sOutputFileName,stcreate,4096);
        decoder.init(@infile);
        {  // Debug
        if decoder.status<>stok then
          writeln(output, 'Fout: ',decoder.status,' ',decoder.errorinfo);
        }
        iLastPos := infile.Position;
        repeat
          readsize:=4096;
          decoder.read(a,readsize);
          dec(readsize,decoder.short);
          outfile.write(a,readsize);
          if Assigned(ProcessDataProc) and (infile.Position <> iLastPos) then
            begin
              ProcessDataProc(PChar(sOutputFileName), infile.Position - iLastPos);
              iLastPos := infile.Position;
            end;
        until decoder.status<>0;
        {  // Debug
        if decoder.status<>stok then
          writeln(output, 'Fout: ',decoder.status,' ',decoder.errorinfo);
        }
        decoder.done;
        infile.done;
        outfile.done;
      end;
      {  // Debug
      close(output);
      }
    end;

  PK_SKIP:
    begin

    end;
  end; {case}

  Count := Count + 1;
  Result :=0;


end;

function CloseArchive (hArcData : THandle) : Integer;
begin
  Result := 0;
end;

procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc : TChangeVolProc);
begin
end;

procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc : TProcessDataProc);
begin
  if Assigned(pProcessDataProc) then
    ProcessDataProc := pProcessDataProc
  else
    ProcessDataProc := nil;
end;


end.

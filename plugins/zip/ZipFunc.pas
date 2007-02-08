{
   File name: zipfunc.pas

   Author:    Koblov Alexander (Alexx2000@mail.ru)

   Plugin functions

   Copyright (C) 2006

   contributors:



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


unit ZipFunc;

interface
uses wcxhead;

{Mandatory functions}
function OpenArchive (var ArchiveData : tOpenArchiveData) : THandle;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function ReadHeader (hArcData : THandle; var HeaderData : THeaderData) : Integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function ProcessFile (hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function CloseArchive (hArcData : THandle) : Integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc1 : PChangeVolProc);{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc1 : PProcessDataProc);{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
{Optional functions}
function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function GetPackerCaps : Integer;{$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};


implementation
uses AbZipKit, SysUtils, Classes;
var
Arc : TAbZipKit;

function OpenArchive (var ArchiveData : tOpenArchiveData) : THandle;
begin
if not Assigned(Arc) Then
Arc := TAbZipKit.Create(nil);
//MessageBox(0,ArchiveData.ArcName,'OpenArchive',16);
Arc.OpenArchive(ArchiveData.ArcName);
Arc.Tag :=0;
//MessageBox(0,'OpenArchive','OpenArchive',16);
Result :=Cardinal(Arc);

end;

function ReadHeader (hArcData : THandle; var HeaderData : THeaderData) : Integer;
var
I, Size : Integer;
Year, Month, Day,
Hour, Min, Sec, MSec: Word;
begin

if Arc.Tag > Arc.Count - 1 then
begin
Result := E_END_ARCHIVE;
exit;
end;


with HeaderData do
begin
//MessageBox(0,PChar(Arc.Items[Arc.Tag].FileName),'',16);

StrPCopy(FileName, Arc.Items[Arc.Tag].FileName);
PackSize := Arc.Items[Arc.Tag].CompressedSize;
UnpSize := Arc.Items[Arc.Tag].UncompressedSize;
FileCRC := Arc.Items[Arc.Tag].CRC32;
{File date/time}
DecodeDate(Arc.Items[Arc.Tag].LastModTimeAsDateTime, Year, Month, Day);
DecodeTime(Arc.Items[Arc.Tag].LastModTimeAsDateTime, Hour, Min, Sec, MSec);
FileTime := (Year - 1980) shl 25 or (Month shl 21) or (Day shl 16) or (Hour shl 11) or (Min shl 5) or (Sec div 2);
FileAttr := Arc.Items[Arc.Tag].ExternalFileAttributes;

end;
Result := 0;

end;

function ProcessFile (hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;
begin
case Operation of
PK_TEST:
        begin
        Arc.TagItems('*.*');
        Arc.TestTaggedItems;
        end;

PK_EXTRACT:
           begin
           Arc.BaseDirectory := ExtractFilePath(DestName);//DestPath;
           Arc.ExtractAt(Arc.Tag, DestName);
           end;

PK_SKIP:
        begin

        end;
end; {case}

Arc.Tag := Arc.Tag + 1;
Result :=0;


end;

function CloseArchive (hArcData : THandle) : Integer;
begin
Arc.CloseArchive;
FreeAndNil(Arc);
Result := 0;
end;

procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc1 : PChangeVolProc);
begin
end;

procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc1 : PProcessDataProc);
begin
end;

{Optional functions}

function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;
begin
try
if not Assigned(Arc) Then
Arc := TAbZipKit.Create(nil);
Arc.OpenArchive(PackedFile);
Arc.DeleteFiles(DeleteList);
Arc.CloseArchive;
FreeAndNil(Arc);
Result := 0;
except
Result := E_BAD_DATA;
end;
end;

function GetPackerCaps : Integer;
begin
Result := PK_CAPS_DELETE or PK_CAPS_MODIFY or PK_CAPS_MULTIPLE;
end;


end.

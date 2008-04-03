{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.tar, *.tgz archives


   Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)


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
uses uWCXhead, AbZipKit, AbArcTyp;

type
  TAbZipKitEx = class (TAbZipKit)
  private
    FProcessDataProc : TProcessDataProc;
    procedure AbArchiveItemProgressEvent(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
                                         var Abort : Boolean);
    procedure AbArchiveProgressEvent (Sender : TObject; Progress : Byte; var Abort : Boolean);
  end;

{Mandatory functions}
function OpenArchive (var ArchiveData : tOpenArchiveData) : THandle;stdcall;
function ReadHeader (hArcData : THandle; var HeaderData : THeaderData) : Integer;stdcall;
function ProcessFile (hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;stdcall;
function CloseArchive (hArcData : THandle) : Integer;stdcall;
procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc1 : PChangeVolProc);stdcall;
procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc1 : TProcessDataProc);stdcall;
{Optional functions}
function PackFiles(PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): Integer;stdcall;
function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;stdcall;
function GetPackerCaps : Integer;stdcall;


implementation
uses AbUtils, AbExcept, SysUtils, Classes;//, windows;

var
  ProcessDataProc : TProcessDataProc;

{$IFNDEF FPC} // for compiling under Delphi
Const
  DirSeparators : set of char = ['/','\'];

Procedure DoDirSeparators (Var FileName : String);

VAr I : longint;

begin
  For I:=1 to Length(FileName) do
    If FileName[I] in DirSeparators then
      FileName[i]:=PathDelim;
end;
{$ENDIF}

function ExtractOnlyFileName(const FileName: string): string;
var
 iDotIndex,
 I: longint;
 sExt : String;
begin
  (* Find a dot index *)
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['.', '/', '\', ':']) do Dec(I);
  if (I > 0) and (FileName[I] = '.') then
     begin
       iDotIndex := I;
       sExt := Copy(FileName, I, MaxInt);
     end
  else
    begin
     iDotIndex := MaxInt;
     sExt := '';
    end;
  (* Find file name index *)
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
  Result := Copy(FileName, I + 1, iDotIndex - I - 1);
  if sExt = '.tgz' then
    Result := Result + '.tar';
end;


{
  Create file list like "filename1;filename2;filename3"
  from file list like "filename1#0filename2#0filename3#0#0"
}

function MakeFileList(FileList : PChar) : String;
var
  I : Integer;
  CurrentChar : Char;
begin
    I := 0;
    while True do
    begin
      CurrentChar :=  (FileList + I)^;
      if CurrentChar = #0 then
        CurrentChar := ';';

      if ((FileList + I)^ = #0) and ((FileList + I + 1)^ = #0) then
        break;
      Result := Result +  CurrentChar;
      I := I + 1;
    end;
   //WriteLN('MakeFileList = ' + Result);
end;

function OpenArchive (var ArchiveData : tOpenArchiveData) : THandle;
var
  Arc : TAbZipKitEx;
begin
  Result := 0;
  Arc := TAbZipKitEx.Create(nil);
  //MessageBox(0,ArchiveData.ArcName,'OpenArchive',16);
  Arc.OnArchiveItemProgress := Arc.AbArchiveItemProgressEvent;
  Arc.OnArchiveProgress := Arc.AbArchiveProgressEvent;

  try
    Arc.TarAutoHandle:=true;
    Arc.OpenArchive(ArchiveData.ArcName);
    Arc.Tag := 0;
    //MessageBox(0,'OpenArchive','OpenArchive',16);
    Result :=Cardinal(Arc);
  except
    on EAbUnhandledType do ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
  end;

  if (Result = 0) and Assigned(Arc) then
    Arc.Free;
end;

function ReadHeader (hArcData : THandle; var HeaderData : THeaderData) : Integer;
var
  Arc : TAbZipKitEx;
  I, Size : Integer;
  Year, Month, Day,
  Hour, Min, Sec, MSec: Word;
  sFileName : String;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  if Arc.Tag > Arc.Count - 1 then
    begin
      Result := E_END_ARCHIVE;
      Exit;
    end;


  with HeaderData do
    begin
      //MessageBox(0,PChar(Arc.Items[Arc.Tag].FileName),'',16);

      sFileName := Arc.Items[Arc.Tag].FileName;
      
      if (Arc.ArchiveType in [atGzip, atGzippedTar]) and (sFileName = 'unknown') then
         sFileName := ExtractOnlyFileName(Arc.FileName);
         
      DoDirSeparators(sFileName);
      sFileName := ExcludeTrailingPathDelimiter(sFileName);

      StrPCopy(FileName, sFileName);

      PackSize := Arc.Items[Arc.Tag].CompressedSize;
      UnpSize := Arc.Items[Arc.Tag].UncompressedSize;
      FileCRC := Arc.Items[Arc.Tag].CRC32;
      {File date/time}
      try
        DecodeDate(Arc.Items[Arc.Tag].LastModTimeAsDateTime, Year, Month, Day);
        DecodeTime(Arc.Items[Arc.Tag].LastModTimeAsDateTime, Hour, Min, Sec, MSec);
        FileTime := (Year - 1980) shl 25 or (Month shl 21) or (Day shl 16) or (Hour shl 11) or (Min shl 5) or (Sec div 2);
      except
        FileTime := FileAge(Arc.FileName);
      end;
      FileAttr := Arc.Items[Arc.Tag].ExternalFileAttributes;

    end;
  Result := 0;

end;

function ProcessFile (hArcData : THandle; Operation : Integer; DestPath, DestName : PChar) : Integer;
var
  Arc : TAbZipKitEx;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
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
  Result := 0;
end;

function CloseArchive (hArcData : THandle) : Integer;
var
 Arc : TAbZipKitEx;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  Arc.CloseArchive;
  FreeAndNil(Arc);
  Result := 0;
end;

procedure SetChangeVolProc (hArcData : THandle; pChangeVolProc1 : PChangeVolProc);
begin
end;

procedure SetProcessDataProc (hArcData : THandle; pProcessDataProc1 : TProcessDataProc);
var
 Arc : TAbZipKitEx;
begin
  if (hArcData <> 0) then  // if archive is open
   begin
     Arc := TAbZipKitEx(Pointer(hArcData));
     if Assigned(pProcessDataProc1) then
       Arc.FProcessDataProc := pProcessDataProc1
     else
       Arc.FProcessDataProc := nil;
   end
  else  // if archive is close
     if Assigned(pProcessDataProc1) then
       ProcessDataProc := pProcessDataProc1
     else
       ProcessDataProc := nil;
end;

{Optional functions}

function PackFiles(PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): integer;
var
 Arc : TAbZipKitEx;
begin
  try
    Arc := TAbZipKitEx.Create(nil);
    Arc.FProcessDataProc := ProcessDataProc;
    Arc.OnArchiveItemProgress := Arc.AbArchiveItemProgressEvent;
    Arc.OnArchiveProgress := Arc.AbArchiveProgressEvent;

    Arc.OpenArchive(PackedFile);
    Arc.BaseDirectory := SrcPath;
    
    Arc.AddFiles(MakeFileList(AddList), faAnyFile);
    Arc.Save;
    Arc.CloseArchive;
    FreeAndNil(Arc);
    Result := 0;
  except
    Result := E_BAD_DATA;
  end;
end;

function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;
var
 Arc : TAbZipKitEx;
begin
  try
    Arc := TAbZipKitEx.Create(nil);
    Arc.FProcessDataProc := ProcessDataProc;
    Arc.OnArchiveItemProgress := Arc.AbArchiveItemProgressEvent;
    Arc.OnArchiveProgress := Arc.AbArchiveProgressEvent;
    
    Arc.OpenArchive(PackedFile);
    Arc.DeleteFiles(MakeFileList(DeleteList));
    Arc.Save;
    Arc.CloseArchive;
    FreeAndNil(Arc);
    Result := 0;
  except
    Result := E_BAD_DATA;
  end;
end;

function GetPackerCaps : Integer;
begin
  Result := PK_CAPS_NEW or PK_CAPS_DELETE or PK_CAPS_MODIFY or PK_CAPS_MULTIPLE;
end;


{ TAbZipKitEx }

procedure TAbZipKitEx.AbArchiveItemProgressEvent(Sender: TObject;
  Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
begin
  try
    if Assigned(FProcessDataProc) then
      Abort := (FProcessDataProc(PChar(Item.FileName), -(Progress)) = 0);
  except
    Abort := True;
  end;

end;

procedure TAbZipKitEx.AbArchiveProgressEvent(Sender: TObject;
  Progress: Byte; var Abort: Boolean);
begin
 try
    if Assigned(FProcessDataProc) then
      Abort := (FProcessDataProc(nil, -(Progress + 1000)) = 0);
  except
    Abort := True;
  end;
end;

end.

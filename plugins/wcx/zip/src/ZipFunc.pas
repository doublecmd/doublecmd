{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.tar, *.tgz archives

   Copyright (C) 2007-2009  Koblov Alexander (Alexx2000@mail.ru)

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
uses uWCXhead, AbZipKit, AbArcTyp, AbZipTyp, DialogAPI, IniFiles,
     AbExcept, AbUtils;

type
  TAbZipKitEx = class (TAbZipKit)
  private
    FProcessDataProc : TProcessDataProc;
    procedure AbArchiveItemProgressEvent(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
                                         var Abort : Boolean);
    procedure AbArchiveProgressEvent (Sender : TObject; Progress : Byte; var Abort : Boolean);

    procedure AbProcessItemFailureEvent(Sender: TObject; Item: TAbArchiveItem; ProcessType: TAbProcessType;
                                        ErrorClass: TAbErrorClass; ErrorCode: Integer);
  end;

{Mandatory functions}
function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;stdcall;
function ReadHeader (hArcData : TArcHandle; var HeaderData : THeaderData) : Integer;stdcall;
function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;stdcall;
function CloseArchive (hArcData : TArcHandle) : Integer;stdcall;
procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc1 : PChangeVolProc);stdcall;
procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc1 : TProcessDataProc);stdcall;
{Optional functions}
function PackFiles(PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): Integer;stdcall;
function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;stdcall;
function GetPackerCaps : Integer;stdcall;
procedure ConfigurePacker (Parent: THandle;  DllInstance: THandle);stdcall;
{Dialog API function}
procedure SetDlgProc(var SetDlgProcInfo: TSetDlgProcInfo);stdcall;

var
  gProcessDataProc : TProcessDataProc;
  gSetDlgProcInfo: TSetDlgProcInfo;
  gCompressionMethodToUse : TAbZipSupportedMethod;
  gDeflationOption : TAbZipDeflationOption;
  gIni: TIniFile;
  
implementation
uses SysUtils, Classes, ZipConfDlg
{$IFDEF MSWINDOWS}
, Windows
{$ENDIF}
;

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

procedure TAbZipKitEx.AbProcessItemFailureEvent(Sender: TObject;
                           Item: TAbArchiveItem; ProcessType: TAbProcessType;
                           ErrorClass: TAbErrorClass; ErrorCode: Integer);
var
  Msg: String;
begin
//ProcessType:(ptAdd, ptDelete, ptExtract, ptFreshen, ptMove, ptReplace, ptFoundUnhandled);

  Msg := 'Error while processing: ' + Item.FileName;

{$IFDEF MSWINDOWS}
  // This is supposedly thread-safe.
  MessageBox(0, PCHAR(msg), 'Error', MB_OK or MB_ICONERROR);
{$ENDIF}
end;

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
      CurrentChar := AbPathSep;

    if ((FileList + I)^ = #0) and ((FileList + I + 1)^ = #0) then
      break;
    Result := Result +  CurrentChar;
    I := I + 1;
  end;
end;

function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;
var
  Arc : TAbZipKitEx;
begin
  Result := 0;
  Arc := TAbZipKitEx.Create(nil);
  //MessageBox(0,ArchiveData.ArcName,'OpenArchive',16);
  Arc.OnArchiveItemProgress := Arc.AbArchiveItemProgressEvent;
  Arc.OnArchiveProgress := Arc.AbArchiveProgressEvent;
  Arc.OnProcessItemFailure := Arc.AbProcessItemFailureEvent;

  try
    Arc.TarAutoHandle:=true;
    Arc.OpenArchive(ArchiveData.ArcName);
    Arc.Tag := 0;
    //MessageBox(0,'OpenArchive','OpenArchive',16);
    Result := TArcHandle(Arc);
  except
    on EAbUnhandledType do ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
  end;

  if (Result = 0) and Assigned(Arc) then
    Arc.Free;
end;

function ReadHeader (hArcData : TArcHandle; var HeaderData : THeaderData) : Integer;
var
  Arc : TAbZipKitEx;
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

      StrPLCopy(FileName, sFileName, SizeOf(FileName) - 1);

      PackSize := Arc.Items[Arc.Tag].CompressedSize;
      UnpSize  := Arc.Items[Arc.Tag].UncompressedSize;
      FileCRC  := Arc.Items[Arc.Tag].CRC32;
      FileTime := Arc.Items[Arc.Tag].SystemSpecificLastModFileTime;
      FileAttr := Arc.Items[Arc.Tag].SystemSpecificAttributes;
    end;
  Result := 0;

end;

function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;
var
  Arc : TAbZipKitEx;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));

  try
    Result := E_SUCCESS;

    case Operation of
    PK_TEST:
      begin
        Arc.TagItems('*.*');
        Arc.TestTaggedItems;
      end;

    PK_EXTRACT:
      begin
        Arc.BaseDirectory := ExtractFilePath(DestName);
        Arc.ExtractAt(Arc.Tag, DestName);

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProc) then
        begin
          if Arc.FProcessDataProc(PChar(Arc.Items[Arc.Tag].FileName),
                                  Arc.Items[Arc.Tag].UncompressedSize) = 0
          then
            Result := E_EABORTED;
        end;
      end;

    PK_SKIP:
      begin

      end;
    end; {case}

  except
    on EAbUserAbort do
      Result := E_EABORTED;
    else
      Result := E_BAD_DATA;
  end;

  Arc.Tag := Arc.Tag + 1;
end;

function CloseArchive (hArcData : TArcHandle) : Integer;
var
 Arc : TAbZipKitEx;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  Arc.CloseArchive;
  FreeAndNil(Arc);
  Result := 0;
end;

procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc1 : PChangeVolProc);
begin
end;

procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc1 : TProcessDataProc);
var
 Arc : TAbZipKitEx;
begin
  if (hArcData <> INVALID_HANDLE_VALUE) then  // if archive is open
   begin
     Arc := TAbZipKitEx(Pointer(hArcData));
     if Assigned(pProcessDataProc1) then
       Arc.FProcessDataProc := pProcessDataProc1
     else
       Arc.FProcessDataProc := nil;
   end
  else  // if archive is close
     if Assigned(pProcessDataProc1) then
       gProcessDataProc := pProcessDataProc1
     else
       gProcessDataProc := nil;
end;

{Optional functions}

function PackFiles(PackedFile: pchar;  SubPath: pchar;  SrcPath: pchar;  AddList: pchar;  Flags: integer): integer;
var
 Arc : TAbZipKitEx;
begin
  try
    try
      Arc := TAbZipKitEx.Create(nil);
      Arc.AutoSave := False;
      Arc.CompressionMethodToUse:= gCompressionMethodToUse;
      Arc.DeflationOption:= gDeflationOption;
      Arc.FProcessDataProc := gProcessDataProc;
      Arc.OnProcessItemFailure := Arc.AbProcessItemFailureEvent;

      Arc.TarAutoHandle:=True;
      Arc.OpenArchive(PackedFile);

      Arc.OnArchiveItemProgress := Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := Arc.AbArchiveProgressEvent;

      Arc.BaseDirectory := SrcPath;
      Arc.AddEntries(MakeFileList(AddList), SubPath);

      Arc.Save;
      Arc.CloseArchive;

      Result := E_SUCCESS;
    except
      on EAbUserAbort do
        Result := E_EABORTED;
      on EAbFileNotFound do
        Result := E_EOPEN;
      else
        begin
          Result := E_BAD_DATA;
        end;
    end;

  finally
    FreeAndNil(Arc);
  end;
end;

function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;

  function StrEndsWith(S : String; SearchPhrase : String) : Boolean;
  begin
    Result := (RightStr(S, Length(SearchPhrase)) = SearchPhrase);
  end;

var
 Arc : TAbZipKitEx;
 pFileName : PChar;
 FileName : String;
begin
  try
    try
      Arc := TAbZipKitEx.Create(nil);
      Arc.FProcessDataProc := gProcessDataProc;
      Arc.OnProcessItemFailure := Arc.AbProcessItemFailureEvent;

      Arc.TarAutoHandle:=True;
      Arc.OpenArchive(PackedFile);

      // Set this after opening archive, to get only progress of deleting.
      Arc.OnArchiveItemProgress := Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := Arc.AbArchiveProgressEvent;

      // Parse file list.
      pFileName := DeleteList;
      while pFileName^ <> #0 do
      begin
        FileName := pFileName;    // Convert PChar to String (up to first #0).

        // If ends with '.../*.*' or '.../' then delete directory.
        if StrEndsWith(FileName, PathDelim + '*.*') or
           StrEndsWith(FileName, PathDelim)
        then
          Arc.DeleteDirectoriesRecursively(ExtractFilePath(FileName))
        else
          Arc.DeleteFiles(FileName);

        pFileName := pFileName + Length(FileName) + 1; // move after filename and ending #0
        if pFileName^ = #0 then
          Break;  // end of list
      end;

      Arc.Save;
      Arc.CloseArchive;
      Result := E_SUCCESS;
    except
      on EAbUserAbort do
        Result := E_EABORTED;
      else
        Result := E_BAD_DATA;
    end;
  finally
    FreeAndNil(Arc);
  end;
end;

function GetPackerCaps : Integer;
begin
  Result := PK_CAPS_NEW      or PK_CAPS_DELETE  or PK_CAPS_MODIFY
         or PK_CAPS_MULTIPLE or PK_CAPS_OPTIONS or PK_CAPS_BY_CONTENT;
  //     or PK_CAPS_MEMPACK  or PK_CAPS_ENCRYPT
end;

procedure ConfigurePacker(Parent: THandle; DllInstance: THandle);
begin
  CreateZipConfDlg;
end;

procedure SetDlgProc(var SetDlgProcInfo: TSetDlgProcInfo);
begin
  gSetDlgProcInfo:= SetDlgProcInfo;
  // load configuration from ini file
  gIni:= TIniFile.Create(gSetDlgProcInfo.PluginConfDir + 'zip.ini');
  LoadConfig;
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

finalization
  gIni.Free;
end.


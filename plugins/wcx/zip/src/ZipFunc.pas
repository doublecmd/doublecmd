{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.tar, *.tgz archives

   Copyright (C) 2007-2012  Koblov Alexander (Alexx2000@mail.ru)

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

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses 
  Classes,
  WcxPlugin, AbArcTyp, AbZipTyp, Extension,
  AbExcept, AbUtils, AbConst, ZipApp;

type

  { TAbZipKitEx }

  TAbZipKitEx = class (TAbZipKit)
  private
    FOperationResult: LongInt;
    FProcessDataProc  : TProcessDataProc;
    FProcessDataProcW : TProcessDataProcW;
    procedure AbArchiveItemProgressEvent(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
                                         var Abort : Boolean);
    procedure AbArchiveProgressEvent (Sender : TObject; Progress : Byte; var Abort : Boolean);
    procedure AbNeedPasswordEvent(Sender : TObject; var NewPassword : AnsiString);
    procedure AbProcessItemFailureEvent(Sender: TObject; Item: TAbArchiveItem; ProcessType: TAbProcessType;
                                        ErrorClass: TAbErrorClass; ErrorCode: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{Mandatory functions}
function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;dcpcall;
function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle;dcpcall;
function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall;
function ReadHeaderEx(hArcData : TArcHandle; var HeaderData: THeaderDataEx) : Integer;dcpcall;
function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall;
function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;dcpcall;
function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer;dcpcall;
function CloseArchive (hArcData : TArcHandle) : Integer;dcpcall;
procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc);dcpcall;
procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall;
procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall;
procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall;
{Optional functions}
function PackFiles(PackedFile: PChar;  SubPath: PChar;  SrcPath: PChar;  AddList: PChar;  Flags: Integer): Integer;dcpcall;
function PackFilesW(PackedFile: PWideChar;  SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall;
function DeleteFiles(PackedFile, DeleteList : PChar) : Integer;dcpcall;
function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall;
function GetPackerCaps : Integer;dcpcall;
procedure ConfigurePacker (Parent: HWND;  DllInstance: THandle);dcpcall;
function CanYouHandleThisFile(FileName: PAnsiChar): Boolean; dcpcall;
function CanYouHandleThisFileW(FileName: PWideChar): Boolean; dcpcall;
{Extension API}
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;

const
  IniFileName = 'zip.ini';

var
  gProcessDataProc : TProcessDataProc;
  gProcessDataProcW : TProcessDataProcW;
  gStartupInfo: TExtensionStartupInfo;
  gCompressionMethodToUse : TAbZipSupportedMethod;
  gDeflationOption : TAbZipDeflationOption;
  
implementation

uses
  SysUtils, ZipConfDlg, IniFiles, AbBrowse, DCConvertEncoding;

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

(*
function MakeFileList(FileList : PAnsiChar) : UTF8String;
var
  FileName: AnsiString;
begin
  Result := '';
  while True do
  begin
    FileName := AnsiString(FileList);
    Result := Result + FileName;
    if (FileList + Length(FileName) + 1)^ = #0 then
      Break;
    Result := Result + AbPathSep;
    Inc(FileList, Length(FileName) + 1);
  end;
end;

function MakeFileListW(FileList : PWideChar) : UTF8String;
var
  FileName: WideString;
begin
  Result := '';
  while True do
  begin
    FileName := WideString(FileList);
    Result := Result + UTF8Encode(FileName);
    if (FileList + Length(FileName) + 1)^ = #0 then
      Break;
    Result := Result + AbPathSep;
    Inc(FileList, Length(FileName) + 1);
  end;
end;
*)

procedure StringToArrayW(src: WideString;
                         pDst: PWideChar;
                         MaxDstLength: Integer);
begin
  if Length(src) < MaxDstLength then
    MaxDstLength := Length(src)
  else
    MaxDstLength := MaxDstLength - 1; // for ending #0

  if Length(src) > 0 then
    Move(src[1], pDst^, SizeOf(WideChar) * MaxDstLength);
  pDst[MaxDstLength] := WideChar(0);
end;

function StrEndsWith(S : String; SearchPhrase : String) : Boolean;
begin
  Result := (RightStr(S, Length(SearchPhrase)) = SearchPhrase);
end;

// -- Exported functions ------------------------------------------------------

function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;dcpcall;
var
  Arc : TAbZipKitEx;
begin
  Result := 0;
  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
    Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
    Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;

    Arc.TarAutoHandle := True;
    Arc.OpenArchive(ArchiveData.ArcName);
    Arc.Tag := 0;
    Result := TArcHandle(Arc);
  except
    on EAbUnhandledType do
      ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
    on EAbFileNotFound do
      ArchiveData.OpenResult := E_EOPEN;
    on EAbUnhandledType do
      ArchiveData.OpenResult := E_NOT_SUPPORTED;
    on EFCreateError do
      ArchiveData.OpenResult := E_ECREATE;
    on EFOpenError do
      ArchiveData.OpenResult := E_EOPEN;
    on EReadError do
      ArchiveData.OpenResult := E_EREAD;
    on EWriteError do
      ArchiveData.OpenResult := E_EWRITE;
    else
      ;
  end;

  if (Result = 0) and Assigned(Arc) then
    Arc.Free;
end;

function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle;dcpcall;
var
  Arc : TAbZipKitEx;
begin
  Result := 0;
  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
    Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
    Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;

    Arc.TarAutoHandle := True;
    Arc.OpenArchive(UTF8Encode(WideString(ArchiveData.ArcName)));
    Arc.Tag := 0;
    Result := TArcHandle(Arc);
  except
    on EAbUnhandledType do
      ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
    on EAbFileNotFound do
      ArchiveData.OpenResult := E_EOPEN;
    on EAbUnhandledType do
      ArchiveData.OpenResult := E_NOT_SUPPORTED;
    on EFCreateError do
      ArchiveData.OpenResult := E_ECREATE;
    on EFOpenError do
      ArchiveData.OpenResult := E_EOPEN;
    on EReadError do
      ArchiveData.OpenResult := E_EREAD;
    on EWriteError do
      ArchiveData.OpenResult := E_EWRITE;
    else
      ;
  end;

  if (Result = 0) and Assigned(Arc) then
    Arc.Free;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  sFileName : String;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  if Arc.Tag > Arc.Count - 1 then
    Exit(E_END_ARCHIVE);

  sFileName := Arc.Items[Arc.Tag].FileName;

  if (Arc.ArchiveType in [atGzip, atGzippedTar]) and (sFileName = 'unknown') then
     sFileName := ExtractOnlyFileName(Arc.FileName);

  DoDirSeparators(sFileName);
  sFileName := ExcludeTrailingPathDelimiter(sFileName);

  StrPLCopy(HeaderData.FileName, sFileName, SizeOf(HeaderData.FileName) - 1);

  with Arc.Items[Arc.Tag] do
    begin
      HeaderData.PackSize     := CompressedSize;
      HeaderData.UnpSize      := UncompressedSize;
      HeaderData.FileCRC      := CRC32;
 //     HeaderData.FileTime     := SystemSpecificLastModFileTime;
      HeaderData.FileAttr     := NativeFileAttributes;
    end;

  Result := E_SUCCESS;
end;

function ReadHeaderEx(hArcData : TArcHandle; var HeaderData: THeaderDataEx) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  sFileName : String;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  if Arc.Tag > Arc.Count - 1 then
    Exit(E_END_ARCHIVE);

  sFileName := Arc.Items[Arc.Tag].FileName;

  if (Arc.ArchiveType in [atGzip, atGzippedTar]) and (sFileName = 'unknown') then
     sFileName := ExtractOnlyFileName(Arc.FileName);

  DoDirSeparators(sFileName);
  sFileName := ExcludeTrailingPathDelimiter(sFileName);

  StrPLCopy(HeaderData.FileName, sFileName, SizeOf(HeaderData.FileName) - 1);

  with Arc.Items[Arc.Tag] do
    begin
      HeaderData.PackSize     := Lo(CompressedSize);
      HeaderData.PackSizeHigh := Hi(CompressedSize);
      HeaderData.UnpSize      := Lo(UncompressedSize);
      HeaderData.UnpSizeHigh  := Hi(UncompressedSize);
      HeaderData.FileCRC      := CRC32;
//      HeaderData.FileTime     := SystemSpecificLastModFileTime;
      HeaderData.FileAttr     := NativeFileAttributes;
    end;

  Result := E_SUCCESS;
end;

function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  sFileName : String;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  if Arc.Tag > Arc.Count - 1 then
    Exit(E_END_ARCHIVE);

  sFileName := Arc.Items[Arc.Tag].FileName;

  if (Arc.ArchiveType in [atGzip, atGzippedTar]) and (sFileName = 'unknown') then
     sFileName := ExtractOnlyFileName(Arc.FileName);

  DoDirSeparators(sFileName);
  sFileName := ExcludeTrailingPathDelimiter(sFileName);

  StringToArrayW(UTF8Decode(sFileName), @HeaderData.FileName, SizeOf(HeaderData.FileName));

  with Arc.Items[Arc.Tag] do
    begin
      HeaderData.PackSize     := Lo(CompressedSize);
      HeaderData.PackSizeHigh := Hi(CompressedSize);
      HeaderData.UnpSize      := Lo(UncompressedSize);
      HeaderData.UnpSizeHigh  := Hi(UncompressedSize);
      HeaderData.FileCRC      := CRC32;
//      HeaderData.FileTime     := SystemSpecificLastModFileTime;
      HeaderData.FileAttr     := NativeFileAttributes;
    end;

  Result := E_SUCCESS;
end;

function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));

  try
    Arc.FOperationResult := E_SUCCESS;

    case Operation of
    PK_TEST:
      begin
        Arc.TestItemAt(Arc.Tag);

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProc) then
        begin
          if Arc.FProcessDataProc(PChar(Arc.Items[Arc.Tag].FileName),
                                  Arc.Items[Arc.Tag].UncompressedSize) = 0
          then
            Arc.FOperationResult := E_EABORTED;
        end;
      end;

    PK_EXTRACT:
      begin
        if (DestPath <> nil) and (DestPath[0] <> #0) then
          Arc.BaseDirectory := DestPath
        else
          Arc.BaseDirectory := ExtractFilePath(DestName);
        Arc.ExtractAt(Arc.Tag, DestName);

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProc) then
        begin
          if Arc.FProcessDataProc(PChar(Arc.Items[Arc.Tag].FileName),
                                  Arc.Items[Arc.Tag].UncompressedSize) = 0
          then
            Arc.FOperationResult := E_EABORTED;
        end;
      end;

    PK_SKIP:
      begin

      end;
    end; {case}

  except
    on EAbUserAbort do
      Arc.FOperationResult := E_EABORTED;
    else
      Arc.FOperationResult := E_BAD_DATA;
  end;

  Result:= Arc.FOperationResult;
  Arc.Tag := Arc.Tag + 1;
end;

function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  DestNameUtf8: UTF8String;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));

  try
    Arc.FOperationResult := E_SUCCESS;

    case Operation of
    PK_TEST:
      begin
        Arc.TestItemAt(Arc.Tag);

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProcW) then
        begin
          if Arc.FProcessDataProcW(PWideChar(UTF8Decode(Arc.Items[Arc.Tag].FileName)),
                                   Arc.Items[Arc.Tag].UncompressedSize) = 0
          then
            Arc.FOperationResult := E_EABORTED;
        end;
      end;

    PK_EXTRACT:
      begin
        DestNameUtf8 := UTF8Encode(WideString(DestName));
        if (DestPath <> nil) and (DestPath[0] <> #0) then
          Arc.BaseDirectory := DestPath
        else
          Arc.BaseDirectory := ExtractFilePath(DestNameUtf8);
        Arc.ExtractAt(Arc.Tag, DestNameUtf8);

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProcW) then
        begin
          if Arc.FProcessDataProcW(PWideChar(UTF8Decode(Arc.Items[Arc.Tag].FileName)),
                                   Arc.Items[Arc.Tag].UncompressedSize) = 0
          then
            Arc.FOperationResult := E_EABORTED;
        end;
      end;

    PK_SKIP:
      begin

      end;
    end; {case}

  except
    on EAbUserAbort do
      Arc.FOperationResult := E_EABORTED;
    else
      Arc.FOperationResult := E_BAD_DATA;
  end;

  Result:= Arc.FOperationResult;
  Arc.Tag := Arc.Tag + 1;
end;

function CloseArchive (hArcData : TArcHandle) : Integer;dcpcall;
var
 Arc : TAbZipKitEx;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  Arc.CloseArchive;
  FreeAndNil(Arc);
  Result := E_SUCCESS;
end;

procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc);dcpcall;
begin
end;

procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall;
begin
end;

procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall;
var
 Arc : TAbZipKitEx;
begin
  if (hArcData <> wcxInvalidHandle) then  // if archive is open
   begin
     Arc := TAbZipKitEx(Pointer(hArcData));
     Arc.FProcessDataProc := pProcessDataProc;
   end
  else  // if archive is close
    gProcessDataProc := pProcessDataProc;
end;

procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall;
var
 Arc : TAbZipKitEx;
begin
  if (hArcData <> wcxInvalidHandle) then  // if archive is open
   begin
     Arc := TAbZipKitEx(Pointer(hArcData));
     Arc.FProcessDataProcW := pProcessDataProc;
   end
  else  // if archive is close
    gProcessDataProcW := pProcessDataProc;
end;

{Optional functions}

function PackFiles(PackedFile: PChar;  SubPath: PChar;  SrcPath: PChar;  AddList: PChar;  Flags: Integer): Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  FilePath: AnsiString;
  FileName: AnsiString;
  sPassword: AnsiString;
begin
  try
    try
      Arc := TAbZipKitEx.Create(nil);
      Arc.AutoSave := False;
      Arc.CompressionMethodToUse:= gCompressionMethodToUse;
      Arc.DeflationOption:= gDeflationOption;
      Arc.FProcessDataProc := gProcessDataProc;
      Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
      Arc.StoreOptions := Arc.StoreOptions + [soReplace];

      if ((Flags and PK_PACK_ENCRYPT) <> 0) and
         (LowerCase(ExtractFileExt(PackedFile)) = '.zip') then // only zip supports encryption
        begin
          Arc.AbNeedPasswordEvent(Arc, sPassword);
          Arc.Password:= sPassword;
        end;

      Arc.TarAutoHandle:=True;
      Arc.OpenArchive(PackedFile);

      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;

      Arc.BaseDirectory := SrcPath;

      FilePath:= AnsiString(SubPath);
      while True do
      begin
        FileName := AnsiString(AddList);
        Arc.Archive.AddEntry(FileName, FilePath);
        if (AddList + Length(FileName) + 1)^ = #0 then
          Break;
        Inc(AddList, Length(FileName) + 1);
      end;

      Arc.Save;
      Arc.CloseArchive;

      Result := E_SUCCESS;
    except
      on EAbUserAbort do
        Result := E_EABORTED;
      on EAbFileNotFound do
        Result := E_EOPEN;
      on EAbUnhandledType do
        Result := E_NOT_SUPPORTED;
      on EFCreateError do
        Result := E_ECREATE;
      on EFOpenError do
        Result := E_EOPEN;
      on EReadError do
        Result := E_EREAD;
      on EWriteError do
        Result := E_EWRITE;
      else
        begin
          Result := E_BAD_DATA;
        end;
    end;

  finally
    FreeAndNil(Arc);
  end;
end;

function PackFilesW(PackedFile: PWideChar;  SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  FilePath: UTF8String;
  FileName: WideString;
  sPassword: AnsiString;
  sPackedFile: UTF8String;
begin
  try
    try
      Arc := TAbZipKitEx.Create(nil);
      Arc.AutoSave := False;
      Arc.CompressionMethodToUse:= gCompressionMethodToUse;
      Arc.DeflationOption:= gDeflationOption;
      Arc.FProcessDataProcW := gProcessDataProcW;
      Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
      Arc.StoreOptions := Arc.StoreOptions + [soReplace];

      sPackedFile := UTF8Encode(WideString(PackedFile));

      if ((Flags and PK_PACK_ENCRYPT) <> 0) and
         (LowerCase(ExtractFileExt(sPackedFile)) = '.zip') then // only zip supports encryption
        begin
          Arc.AbNeedPasswordEvent(Arc, sPassword);
          Arc.Password:= sPassword;
        end;

      Arc.TarAutoHandle:=True;
      Arc.OpenArchive(sPackedFile);

      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;

      Arc.BaseDirectory := UTF8Encode(WideString(SrcPath));

      FilePath:= UTF8Encode(WideString(SubPath));
      while True do
      begin
        FileName := WideString(AddList);
        Arc.Archive.AddEntry(UTF8Encode(FileName), FilePath);
        if (AddList + Length(FileName) + 1)^ = #0 then
          Break;
        Inc(AddList, Length(FileName) + 1);
      end;

      Arc.Save;
      Arc.CloseArchive;

      Result := E_SUCCESS;
    except
      on EAbUserAbort do
        Result := E_EABORTED;
      on EAbFileNotFound do
        Result := E_EOPEN;
      on EAbUnhandledType do
        Result := E_NOT_SUPPORTED;
      on EFCreateError do
        Result := E_ECREATE;
      on EFOpenError do
        Result := E_EOPEN;
      on EReadError do
        Result := E_EREAD;
      on EWriteError do
        Result := E_EWRITE;
      else
        begin
          Result := E_BAD_DATA;
        end;
    end;

  finally
    FreeAndNil(Arc);
  end;
end;

function DeleteFiles (PackedFile, DeleteList : PChar) : Integer;dcpcall;
var
 Arc : TAbZipKitEx;
 pFileName : PChar;
 FileName : String;
begin
  try
    try
      Arc := TAbZipKitEx.Create(nil);
      Arc.FProcessDataProc := gProcessDataProc;
      Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
      Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;

      Arc.TarAutoHandle:=True;
      Arc.OpenArchive(PackedFile);

      // Set this after opening archive, to get only progress of deleting.
      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;

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
      on EAbFileNotFound do
        Result := E_EOPEN;
      on EAbUnhandledType do
        Result := E_NOT_SUPPORTED;
      on EFCreateError do
        Result := E_ECREATE;
      on EFOpenError do
        Result := E_EOPEN;
      on EReadError do
        Result := E_EREAD;
      on EWriteError do
        Result := E_EWRITE;
      else
        Result := E_BAD_DATA;
    end;
  finally
    FreeAndNil(Arc);
  end;
end;

function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall;
var
 Arc : TAbZipKitEx;
 pFileName : PWideChar;
 FileName : WideString;
 FileNameUTF8 : UTF8String;
begin
  try
    try
      Arc := TAbZipKitEx.Create(nil);
      Arc.FProcessDataProcW := gProcessDataProcW;
      Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
      Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;

      Arc.TarAutoHandle:=True;
      Arc.OpenArchive(UTF8Encode(WideString(PackedFile)));

      // Set this after opening archive, to get only progress of deleting.
      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;

      // Parse file list.
      pFileName := DeleteList;
      while pFileName^ <> #0 do
      begin
        FileName := pFileName;    // Convert PWideChar to WideString (up to first #0).

        FileNameUTF8 := UTF8Encode(FileName);

        // If ends with '.../*.*' or '.../' then delete directory.
        if StrEndsWith(FileNameUTF8, PathDelim + '*.*') or
           StrEndsWith(FileNameUTF8, PathDelim)
        then
          Arc.DeleteDirectoriesRecursively(ExtractFilePath(FileNameUTF8))
        else
          Arc.DeleteFiles(FileNameUTF8);

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
      on EAbFileNotFound do
        Result := E_EOPEN;
      on EAbUnhandledType do
        Result := E_NOT_SUPPORTED;
      on EFCreateError do
        Result := E_ECREATE;
      on EFOpenError do
        Result := E_EOPEN;
      on EReadError do
        Result := E_EREAD;
      on EWriteError do
        Result := E_EWRITE;
      else
        Result := E_BAD_DATA;
    end;
  finally
    FreeAndNil(Arc);
  end;
end;

function GetPackerCaps : Integer;dcpcall;
begin
  Result := PK_CAPS_NEW      or PK_CAPS_DELETE  or PK_CAPS_MODIFY
         or PK_CAPS_MULTIPLE or PK_CAPS_OPTIONS or PK_CAPS_BY_CONTENT
         or PK_CAPS_ENCRYPT;
  //     or PK_CAPS_MEMPACK
end;

procedure ConfigurePacker(Parent: HWND; DllInstance: THandle);dcpcall;
begin
  CreateZipConfDlg;
end;

function CanYouHandleThisFile(FileName: PAnsiChar): Boolean; dcpcall;
begin
  try
    Result:= (AbDetermineArcType(SysToUtf8(StrPas(FileName)), atUnknown) <> atUnknown);
  except
    Result := False;
  end;
end;

function CanYouHandleThisFileW(FileName: PWideChar): Boolean; dcpcall;
begin
  try
    Result:= (AbDetermineArcType(UTF8Encode(WideString(FileName)), atUnknown) <> atUnknown);
  except
    Result := False;
  end;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;
var
  gIni: TIniFile;
begin
  gStartupInfo:= StartupInfo^;

  // load configuration from ini file
  gIni:= TIniFile.Create(gStartupInfo.PluginConfDir + IniFileName);
  try
    LoadConfig;
  finally
    gIni.Free;
  end;
end;


{ TAbZipKitEx }

constructor TAbZipKitEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOperationResult := E_SUCCESS;
  FProcessDataProc  := nil;
  FProcessDataProcW := nil;
end;

procedure TAbZipKitEx.AbProcessItemFailureEvent(Sender: TObject;
                           Item: TAbArchiveItem; ProcessType: TAbProcessType;
                           ErrorClass: TAbErrorClass; ErrorCode: Integer);
begin
  case ErrorCode of
  AbUserAbort:
    FOperationResult:= E_EABORTED;
  AbZipBadCRC:
    FOperationResult:= E_BAD_ARCHIVE;
  AbFileNotFound:
    FOperationResult:= E_NO_FILES;
  AbReadError:
    FOperationResult:= E_EREAD;
  else
    FOperationResult:= E_BAD_DATA;
  end;
end;

procedure TAbZipKitEx.AbArchiveItemProgressEvent(Sender: TObject;
  Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
begin
  try
    if Assigned(FProcessDataProcW) then
      begin
        if Assigned(Item) then
          Abort := (FProcessDataProcW(PWideChar(UTF8Decode(Item.FileName)), -(Progress)) = 0)
        else
          Abort := (FProcessDataProcW(nil, -(Progress)) = 0);
      end
    else if Assigned(FProcessDataProc) then
      begin
        if Assigned(Item) then
          Abort := (FProcessDataProc(PAnsiChar(Item.FileName), -(Progress)) = 0)
        else
          Abort := (FProcessDataProc(nil, -(Progress)) = 0);
      end;
  except
    Abort := True;
  end;
end;

procedure TAbZipKitEx.AbArchiveProgressEvent(Sender: TObject;
  Progress: Byte; var Abort: Boolean);
begin
  try
    if Assigned(FProcessDataProcW) then
      Abort := (FProcessDataProcW(nil, -(Progress + 1000)) = 0)
    else if Assigned(FProcessDataProc) then
      Abort := (FProcessDataProc(nil, -(Progress + 1000)) = 0);
  except
    Abort := True;
  end;
end;

procedure TAbZipKitEx.AbNeedPasswordEvent(Sender: TObject;
                                          var NewPassword: AnsiString);
var
  aNewPassword: array[0..MAX_PATH-1] of AnsiChar;
  Result: Boolean;
begin
  aNewPassword := Copy(NewPassword, 1, MAX_PATH);
  Result:= gStartupInfo.InputBox('Zip', 'Please enter the password:', True, PAnsiChar(aNewPassword), MAX_PATH);
  if Result then
    begin
      NewPassword := aNewPassword;
    end
  else
    begin
      raise EAbUserAbort.Create;
    end;
end;

end.


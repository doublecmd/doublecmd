{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.tar, *.tgz archives

   Copyright (C) 2007-2014 Alexander Koblov (alexx2000@mail.ru)

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
function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall;
function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;dcpcall;
function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer;dcpcall;
function CloseArchive (hArcData : TArcHandle) : Integer;dcpcall;
procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc);dcpcall;
procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall;
procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall;
procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall;
{Optional functions}
function PackFilesW(PackedFile: PWideChar;  SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall;
function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall;
function GetPackerCaps : Integer;dcpcall;
procedure ConfigurePacker (Parent: HWND;  DllInstance: THandle);dcpcall;
function CanYouHandleThisFileW(FileName: PWideChar): Boolean; dcpcall;
{Extension API}
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;

const
  IniFileName = 'zip.ini';

var
  gStartupInfo: TExtensionStartupInfo;
  gCompressionMethodToUse : TAbZipSupportedMethod;
  gDeflationOption : TAbZipDeflationOption;
  gTarAutoHandle : Boolean;
  
implementation

uses
  SysUtils, LazUTF8, ZipConfDlg, AbBrowse, DCConvertEncoding;

threadvar
  gProcessDataProcW : TProcessDataProcW;

procedure StringToArrayW(src: UnicodeString;
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

function GetArchiveError(const E : Exception): Integer;
begin
  if E is EAbUserAbort then
    Result := E_EABORTED
  else if E is EAbFileNotFound then
    Result := E_EOPEN
  else if E is EAbUnhandledType then
    Result := E_UNKNOWN_FORMAT
  else if E is EFCreateError then
    Result := E_ECREATE
  else if E is EFOpenError then
    Result := E_EOPEN
  else if E is EReadError then
    Result := E_EREAD
  else if E is EWriteError then
    Result := E_EWRITE
  else
    Result := E_UNKNOWN;
end;

// -- Exported functions ------------------------------------------------------

function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;dcpcall;
begin
  Result := 0;
  ArchiveData.OpenResult := E_NOT_SUPPORTED;
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

    Arc.TarAutoHandle := gTarAutoHandle;
    Arc.OpenArchive(UTF16ToUTF8(UnicodeString(ArchiveData.ArcName)));
    Arc.Tag := 0;
    Result := TArcHandle(Arc);
  except
    on E: Exception do
    begin
      Arc.Free;
      ArchiveData.OpenResult := GetArchiveError(E);
      if (ArchiveData.OpenResult = E_UNKNOWN) then
      begin
        ArchiveData.OpenResult := E_HANDLED;
        gStartupInfo.MessageBox(PAnsiChar(E.Message), nil, MB_OK or MB_ICONERROR);
      end;
    end;
  end;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall;
begin
  Result := E_NOT_SUPPORTED;
end;

function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  sFileName : String;
begin
  Arc := TAbZipKitEx(Pointer(hArcData));
  if Arc.Tag > Arc.Count - 1 then
    Exit(E_END_ARCHIVE);

  sFileName := Arc.GetFileName(Arc.Tag);

  StringToArrayW(UTF8Decode(sFileName), @HeaderData.FileName, SizeOf(HeaderData.FileName));

  with Arc.Items[Arc.Tag] do
    begin
      HeaderData.PackSize     := Lo(CompressedSize);
      HeaderData.PackSizeHigh := Hi(CompressedSize);
      HeaderData.UnpSize      := Lo(UncompressedSize);
      HeaderData.UnpSizeHigh  := Hi(UncompressedSize);
      HeaderData.FileCRC      := CRC32;
      HeaderData.FileTime     := NativeLastModFileTime;
      HeaderData.FileAttr     := NativeFileAttributes;

      if IsEncrypted then begin
        HeaderData.Flags      := RHDF_ENCRYPTED;
      end;

      if IsDirectory then begin
        HeaderData.FileAttr   := HeaderData.FileAttr or faFolder;
      end;
    end;

  Result := E_SUCCESS;
end;

function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;dcpcall;
begin
  Result := E_NOT_SUPPORTED;
end;

function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  DestNameUtf8: String;
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
        DestNameUtf8 := UTF16ToUTF8(UnicodeString(DestName));
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
    on E: Exception do
    begin
      Arc.FOperationResult := GetArchiveError(E);
      if (Operation = PK_TEST) and (Arc.FOperationResult = E_UNKNOWN) then
      begin
        DestNameUtf8:= E.Message + LineEnding + LineEnding + Arc.Items[Arc.Tag].FileName;
        if gStartupInfo.MessageBox(PAnsiChar(DestNameUtf8), nil, MB_OKCANCEL or MB_ICONERROR) = ID_OK then
          Arc.FOperationResult:= E_HANDLED
        else begin
          Arc.FOperationResult:= E_EABORTED;
        end;
      end;
    end;
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
begin
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

function PackFilesW(PackedFile: PWideChar;  SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall;
var
  Arc : TAbZipKitEx;
  FilePath: String;
  FileName: UnicodeString;
  sPassword: AnsiString;
  sPackedFile: String;
begin
  if (Flags and PK_PACK_MOVE_FILES) <> 0 then begin
    Exit(E_NOT_SUPPORTED);
  end;

  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.AutoSave := False;
    Arc.TarAutoHandle:= True;
    Arc.CompressionMethodToUse:= gCompressionMethodToUse;
    Arc.DeflationOption:= gDeflationOption;
    Arc.FProcessDataProcW := gProcessDataProcW;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
    Arc.StoreOptions := Arc.StoreOptions + [soReplace];

    sPackedFile := UTF16ToUTF8(UnicodeString(PackedFile));

    try
      if ((Flags and PK_PACK_ENCRYPT) <> 0) and
         (LowerCase(ExtractFileExt(sPackedFile)) = '.zip') then // only zip supports encryption
      begin
        Arc.AbNeedPasswordEvent(Arc, sPassword);
        Arc.Password:= sPassword;
      end;

      Arc.OpenArchive(sPackedFile);

      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;

      Arc.BaseDirectory := UTF16ToUTF8(UnicodeString(SrcPath));

      FilePath:= UTF16ToUTF8(UnicodeString(SubPath));
      while True do
      begin
        FileName := UnicodeString(AddList);
        Arc.Archive.AddEntry(UTF16ToUTF8(FileName), FilePath);
        if (AddList + Length(FileName) + 1)^ = #0 then
          Break;
        Inc(AddList, Length(FileName) + 1);
      end;

      Arc.Save;
      Arc.CloseArchive;
    except
      on E: Exception do
        Arc.FOperationResult := GetArchiveError(E);
    end;
  finally
    Result := Arc.FOperationResult;
    FreeAndNil(Arc);
  end;
end;

function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall;
var
 Arc : TAbZipKitEx;
 pFileName : PWideChar;
 FileName : UnicodeString;
 FileNameUTF8 : String;
begin
  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.TarAutoHandle:= True;
    Arc.FProcessDataProcW := gProcessDataProcW;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
    Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;

    try
      Arc.OpenArchive(UTF16ToUTF8(UnicodeString(PackedFile)));

      // Set this after opening archive, to get only progress of deleting.
      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;

      // Parse file list.
      pFileName := DeleteList;
      while pFileName^ <> #0 do
      begin
        FileName := pFileName;    // Convert PWideChar to UnicodeString (up to first #0).

        FileNameUTF8 := UTF16ToUTF8(FileName);

        // If ends with '.../*.*' or '.../' then delete directory.
        if StrEndsWith(FileNameUTF8, PathDelim + '*.*') or
           StrEndsWith(FileNameUTF8, PathDelim)
        then
          Arc.DeleteDirectoriesRecursively(ExtractFilePath(FileNameUTF8))
        else
          Arc.DeleteFile(FileNameUTF8);

        pFileName := pFileName + Length(FileName) + 1; // move after filename and ending #0
        if pFileName^ = #0 then
          Break;  // end of list
      end;

      Arc.Save;
      Arc.CloseArchive;
    except
      on E: Exception do
        Arc.FOperationResult := GetArchiveError(E);
    end;
  finally
    Result := Arc.FOperationResult;
    FreeAndNil(Arc);
  end;
end;

function GetPackerCaps : Integer;dcpcall;
begin
  Result := PK_CAPS_NEW      or PK_CAPS_DELETE  or PK_CAPS_MODIFY
         or PK_CAPS_MULTIPLE or PK_CAPS_OPTIONS or PK_CAPS_BY_CONTENT
         or PK_CAPS_ENCRYPT;
end;

procedure ConfigurePacker(Parent: HWND; DllInstance: THandle);dcpcall;
begin
  CreateZipConfDlg;
end;

function CanYouHandleThisFileW(FileName: PWideChar): Boolean; dcpcall;
begin
  try
    Result:= (AbDetermineArcType(UTF16ToUTF8(UnicodeString(FileName)), atUnknown) <> atUnknown);
  except
    Result := False;
  end;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;
begin
  gStartupInfo:= StartupInfo^;
  // Load configuration from ini file
  LoadConfig;
end;

{ TAbZipKitEx }

constructor TAbZipKitEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOperationResult := E_SUCCESS;
  FProcessDataProcW := nil;

  TempDirectory := GetTempDir;
end;

procedure TAbZipKitEx.AbProcessItemFailureEvent(Sender: TObject;
                           Item: TAbArchiveItem; ProcessType: TAbProcessType;
                           ErrorClass: TAbErrorClass; ErrorCode: Integer);
var
  Message: AnsiString;
begin
  // Unknown error
  FOperationResult:= E_UNKNOWN;
  // Check error class
  case ErrorClass of
  ecFileOpenError:
    begin
      ErrorClass:= ecAbbrevia;
      ErrorCode:= AbFCIFileOpenError;
      FOperationResult:= E_EOPEN;
    end;
  ecFileCreateError:
    begin
      ErrorClass:= ecAbbrevia;
      ErrorCode:= AbFCICreateError;
      FOperationResult:= E_ECREATE;
    end;
  ecAbbrevia:
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
      end;
    end;
  // Has exception message? Show it!
  else if Assigned(ExceptObject) and (ExceptObject is Exception) then
    begin
      Message := Exception(ExceptObject).Message;
      if Assigned(Item) then Message += LineEnding + LineEnding + Item.FileName;
      if gStartupInfo.MessageBox(PAnsiChar(Message), nil, MB_OKCANCEL or MB_ICONERROR) = ID_OK then
        FOperationResult:= E_HANDLED
      else begin
        raise EAbUserAbort.Create;
      end;
    end;
  end;
  // Show abbrevia specific errors
  if (ErrorClass = ecAbbrevia) and (ProcessType in [ptAdd, ptFreshen, ptReplace]) then
  begin
    Message:= AbStrRes(ErrorCode) + LineEnding + LineEnding + Item.FileName;
    if gStartupInfo.MessageBox(PAnsiChar(Message), nil, MB_OKCANCEL or MB_ICONERROR) = ID_OK then
      FOperationResult:= E_HANDLED
    else begin
      raise EAbUserAbort.Create;
    end;
  end;
end;

procedure TAbZipKitEx.AbArchiveItemProgressEvent(Sender: TObject;
  Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
begin
  try
    if Assigned(FProcessDataProcW) then
    begin
      if Assigned(Item) then
        Abort := (FProcessDataProcW(PWideChar(UTF8Decode(Item.FileName)), -(Progress + 1000)) = 0)
      else
        Abort := (FProcessDataProcW(nil, -(Progress + 1000)) = 0);
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
      Abort := (FProcessDataProcW(nil, -(Progress)) = 0);
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
    NewPassword := aNewPassword
  else
    raise EAbUserAbort.Create;
end;

end.


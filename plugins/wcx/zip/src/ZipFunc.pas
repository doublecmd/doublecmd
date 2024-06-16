{
   Double commander
   -------------------------------------------------------------------------
   WCX plugin for working with *.zip, *.gz, *.tar, *.tgz archives

   Copyright (C) 2007-2024 Alexander Koblov (alexx2000@mail.ru)

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
    FItemProgress: Byte;
    FItem: TAbArchiveItem;
    FNeedPassword: Boolean;
    FOperationResult: LongInt;
    FChangeVolProcW: TChangeVolProcW;
    FProcessDataProcW : TProcessDataProcW;
    procedure AbOneItemProgressEvent(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
                                     var Abort : Boolean);
    procedure AbArchiveItemProgressEvent(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
                                         var Abort : Boolean);
    procedure AbArchiveProgressEvent (Sender : TObject; Progress : Byte; var Abort : Boolean);
    procedure AbNeedPasswordEvent(Sender : TObject; var NewPassword : AnsiString);
    procedure AbProcessItemFailureEvent(Sender: TObject; Item: TAbArchiveItem; ProcessType: TAbProcessType;
                                        ErrorClass: TAbErrorClass; ErrorCode: Integer);
    procedure AbRequestImageEvent(Sender : TObject; ImageNumber : Integer;
                                  var ImageName : String; var Abort : Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{Mandatory functions}
function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;dcpcall; export;
function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle;dcpcall; export;
function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall; export;
function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall; export;
function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;dcpcall; export;
function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer;dcpcall; export;
function CloseArchive (hArcData : TArcHandle) : Integer;dcpcall; export;
procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc);dcpcall; export;
procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall; export;
procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall; export;
procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall; export;
{Optional functions}
function PackFilesW(PackedFile: PWideChar;  SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall; export;
function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall; export;
function GetPackerCaps : Integer;dcpcall; export;
function GetBackgroundFlags: Integer; dcpcall; export;
procedure ConfigurePacker (Parent: HWND;  DllInstance: THandle);dcpcall; export;
function CanYouHandleThisFileW(FileName: PWideChar): Boolean; dcpcall; export;
{Extension API}
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall; export;

const
  IniFileName = 'zip.ini';

var
  gStartupInfo: TExtensionStartupInfo;
  gTarAutoHandle : Boolean;
  
implementation

uses
  SysUtils, LazUTF8, ZipConfDlg, AbBrowse, DCConvertEncoding, DCOSUtils, ZipOpt,
  ZipLng, ZipCache, DCDateTimeUtils;

var
  PasswordCache: TPasswordCache;

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

function CheckError(E: Exception): Integer;
begin
  Result:= GetArchiveError(E);
  if (Result = E_UNKNOWN) then
  begin
    Result := E_HANDLED;
    gStartupInfo.MessageBox(PAnsiChar(E.Message), nil, MB_OK or MB_ICONERROR);
  end;
end;

procedure CheckError(Arc: TAbZipKitEx; E: Exception; const FileName: String);
var
  AMessage: String;
begin
  if (Arc.FOperationResult = E_UNKNOWN) then
  begin
    AMessage:= E.Message + LineEnding + LineEnding + FileName;
    if gStartupInfo.MessageBox(PAnsiChar(AMessage), nil, MB_OKCANCEL or MB_ICONERROR) = ID_OK then
      Arc.FOperationResult:= E_HANDLED
    else begin
      Arc.FOperationResult:= E_EABORTED;
    end;
  end;
end;

// -- Exported functions ------------------------------------------------------

function OpenArchive (var ArchiveData : tOpenArchiveData) : TArcHandle;dcpcall; export;
begin
  Result := 0;
  ArchiveData.OpenResult := E_NOT_SUPPORTED;
end;

function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle;dcpcall; export;
var
  Arc : TAbZipKitEx absolute Result;
begin
  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
    Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;
    Arc.OnRequestImage:= @Arc.AbRequestImageEvent;

    case ArchiveData.OpenMode of
      PK_OM_LIST: Arc.OpenMode := opList;
      PK_OM_EXTRACT: Arc.OpenMode := opExtract;
    end;
    Arc.TarAutoHandle := gTarAutoHandle;
    Arc.OpenArchive(UTF16ToUTF8(UnicodeString(ArchiveData.ArcName)));

    if Arc.ArchiveType in [atGzip, atBzip2, atXz, atLzma, atZstd] then
      Arc.OnArchiveItemProgress := @Arc.AbOneItemProgressEvent
    else begin
      Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
    end;

    Arc.Password := PasswordCache.GetPassword(Arc.FileName);
    Arc.Tag := 0;
  except
    on E: Exception do
    begin
      FreeAndNil(Arc);
      ArchiveData.OpenResult := CheckError(E);
    end;
  end;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData: THeaderData) : Integer;dcpcall; export;
begin
  Result := E_NOT_SUPPORTED;
end;

function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer;dcpcall; export;
var
  sFileName : String;
  Item : TAbArchiveItem;
  Arc : TAbZipKitEx absolute hArcData;
begin
  if Arc.ZipArchive.StreamMode then
  try
    if not Arc.ZipArchive.StreamFindNext(Item) then
      Exit(E_END_ARCHIVE);
  except
    on E: Exception do Exit(CheckError(E));
  end
  else begin
    if Arc.Tag > Arc.Count - 1 then
      Exit(E_END_ARCHIVE);
    Item:= Arc.Items[Arc.Tag];
  end;

  sFileName := Arc.GetFileName(Item);

  StringToArrayW(CeUtf8ToUtf16(sFileName), @HeaderData.FileName, SizeOf(HeaderData.FileName));

  with Item do
    begin
      HeaderData.PackSize     := Lo(CompressedSize);
      HeaderData.PackSizeHigh := Hi(CompressedSize);
      HeaderData.UnpSize      := Lo(UncompressedSize);
      HeaderData.UnpSizeHigh  := Hi(UncompressedSize);
      HeaderData.FileCRC      := CRC32;
      HeaderData.FileTime     := NativeLastModFileTime;
      HeaderData.FileAttr     := NativeFileAttributes;
      HeaderData.MfileTime    := DateTimeToWinFileTime(LastModTimeAsDateTime);

      if IsEncrypted then begin
        HeaderData.Flags      := RHDF_ENCRYPTED;
      end;

      if IsDirectory then begin
        HeaderData.FileAttr   := HeaderData.FileAttr or faFolder;
      end;
    end;

  Result := E_SUCCESS;
end;

function ProcessFile (hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PChar) : Integer;dcpcall; export;
begin
  Result := E_NOT_SUPPORTED;
end;

function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer;dcpcall; export;
var
  Abort: Boolean;
  DestNameUtf8: String;
  Arc : TAbZipKitEx absolute hArcData;
begin
  try
    Arc.FOperationResult := E_SUCCESS;

    case Operation of
    PK_TEST:
      begin
        Arc.TestItemAt(Arc.Tag);

        if (Arc.FNeedPassword) and (Arc.FOperationResult = E_SUCCESS) and Arc.Items[Arc.Tag].IsEncrypted then
        begin
          Arc.FNeedPassword:= False;
          PasswordCache.SetPassword(Arc.FileName, Arc.Password);
        end;

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProcW) then
        begin
          Abort := False;
          Arc.OnArchiveItemProgress(Arc, Arc.Items[Arc.Tag], 100, Abort);
          if Abort then Arc.FOperationResult := E_EABORTED;
        end;
      end;

    PK_EXTRACT:
      begin
        DestNameUtf8 := UTF16ToUTF8(UnicodeString(DestName));
        if (DestPath <> nil) and (DestPath[0] <> #0) then
          Arc.BaseDirectory := DestPath
        else begin
          Arc.BaseDirectory := ExtractFilePath(DestNameUtf8);
        end;
        repeat
          Arc.FOperationResult := E_SUCCESS;
          Arc.ExtractAt(Arc.Tag, DestNameUtf8);
        until (Arc.FOperationResult <> maxLongint);

        if (Arc.FNeedPassword) and (Arc.FOperationResult = E_SUCCESS) and Arc.Items[Arc.Tag].IsEncrypted then
        begin
          Arc.FNeedPassword:= False;
          PasswordCache.SetPassword(Arc.FileName, Arc.Password);
        end;

        // Show progress and ask if aborting.
        if Assigned(Arc.FProcessDataProcW) then
        begin
          Abort := False;
          Arc.OnArchiveItemProgress(Arc, Arc.Items[Arc.Tag], 100, Abort);
          if Abort then Arc.FOperationResult := E_EABORTED;
        end;
      end;

    PK_SKIP:
      begin

      end;
    end; {case}

    if Arc.ZipArchive.StreamMode then
    begin
      Arc.ZipArchive.StreamSeekNext(Operation <> PK_SKIP);
    end

  except
    on E: Exception do
    begin
      Arc.FOperationResult := GetArchiveError(E);
      if (Operation = PK_TEST) then CheckError(Arc, E, Arc.Items[Arc.Tag].FileName);
    end;
  end;

  Result:= Arc.FOperationResult;
  Arc.Tag := Arc.Tag + 1;
end;

function CloseArchive (hArcData : TArcHandle) : Integer;dcpcall; export;
var
  Arc : TAbZipKitEx absolute hArcData;
begin
  Arc.CloseArchive;
  FreeAndNil(Arc);
  Result := E_SUCCESS;
end;

procedure SetChangeVolProc (hArcData : TArcHandle; pChangeVolProc : PChangeVolProc);dcpcall; export;
begin
end;

procedure SetChangeVolProcW(hArcData : TArcHandle; pChangeVolProc : TChangeVolProcW);dcpcall; export;
var
 Arc : TAbZipKitEx absolute hArcData;
begin
  if (hArcData <> wcxInvalidHandle) then
  begin
    Arc.FChangeVolProcW := pChangeVolProc;
  end;
end;

procedure SetProcessDataProc (hArcData : TArcHandle; pProcessDataProc : TProcessDataProc);dcpcall; export;
begin
end;

procedure SetProcessDataProcW(hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW);dcpcall; export;
var
  Arc : TAbZipKitEx absolute hArcData;
begin
  if (hArcData <> wcxInvalidHandle) then  // if archive is open
    Arc.FProcessDataProcW := pProcessDataProc
  else begin  // if archive is close
    gProcessDataProcW := pProcessDataProc;
  end;
end;

{Optional functions}

function PackFilesW(PackedFile: PWideChar;  SubPath: PWideChar;  SrcPath: PWideChar;  AddList: PWideChar;  Flags: Integer): Integer;dcpcall; export;
var
  FileExt: String;
  FilePath: String;
  Arc : TAbZipKitEx;
  FileName: UnicodeString;
  sPassword: AnsiString;
  sPackedFile: String;
  ArchiveFormat: TArchiveFormat;
begin
  if (Flags and PK_PACK_MOVE_FILES) <> 0 then begin
    Exit(E_NOT_SUPPORTED);
  end;

  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.AutoSave := False;
    Arc.OpenMode := opModify;
    Arc.TarAutoHandle:= True;
    Arc.FProcessDataProcW := gProcessDataProcW;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;

    sPackedFile := UTF16ToUTF8(UnicodeString(PackedFile));

    try
      FileExt:= LowerCase(ExtractFileExt(sPackedFile));

      if ((Flags and PK_PACK_ENCRYPT) <> 0) then
      begin
        // Only zip/zipx supports encryption
        if (FileExt = '.zip') or (FileExt = '.zipx') then
        begin
          sPassword:= EmptyStr;
          Arc.AbNeedPasswordEvent(Arc, sPassword);
          Arc.Password:= sPassword;
        end;
      end;

      Arc.OpenArchive(sPackedFile);

      ArchiveFormat:= ARCHIVE_FORMAT[Arc.ArchiveType];

      if (ArchiveFormat = afZip) then
      begin
         if (FileExt = '.zipx') then
           ArchiveFormat:= afZipx
         else begin
           case PluginConfig[ArchiveFormat].Level of
             1: Arc.DeflationOption:= doSuperFast;
             3: Arc.DeflationOption:= doFast;
             6: Arc.DeflationOption:= doNormal;
             9: Arc.DeflationOption:= doMaximum;
           end;
           case PluginConfig[ArchiveFormat].Method of
              PtrInt(cmStored): Arc.CompressionMethodToUse:= smStored;
              PtrInt(cmDeflated): Arc.CompressionMethodToUse:= smDeflated;
              PtrInt(cmEnhancedDeflated): Arc.CompressionMethodToUse:= smBestMethod;
           end;
         end;
      end;
      Arc.ZipArchive.CompressionLevel:= PluginConfig[ArchiveFormat].Level;
      Arc.ZipArchive.CompressionMethod:= PluginConfig[ArchiveFormat].Method;

      Arc.OnArchiveProgress := @Arc.AbArchiveProgressEvent;
      Arc.StoreOptions := Arc.StoreOptions + [soReplace];

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

      if Arc.ArchiveType in [atGzip, atBzip2, atXz, atLzma, atZstd] then
      begin
        with Arc.Archive.ItemList[0] do
        begin
          UncompressedSize := mbFileSize(DiskFileName);
        end;
        Arc.OnArchiveItemProgress := @Arc.AbOneItemProgressEvent
      end
      else begin
        Arc.OnArchiveItemProgress := @Arc.AbArchiveItemProgressEvent;
      end;

      Arc.Save;
      Arc.CloseArchive;
    except
      on E: Exception do
      begin
        Arc.FOperationResult := GetArchiveError(E);
        CheckError(Arc, E, sPackedFile);
      end;
    end;
  finally
    Result := Arc.FOperationResult;
    FreeAndNil(Arc);
  end;
end;

function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall; export;
var
 Arc : TAbZipKitEx;
 FileNameUTF8 : String;
 pFileName : PWideChar;
 FileName : UnicodeString;
 ArchiveFormat: TArchiveFormat;
begin
  Arc := TAbZipKitEx.Create(nil);
  try
    Arc.OpenMode := opModify;
    Arc.TarAutoHandle:= True;
    Arc.FProcessDataProcW := gProcessDataProcW;
    Arc.OnProcessItemFailure := @Arc.AbProcessItemFailureEvent;
    Arc.OnNeedPassword:= @Arc.AbNeedPasswordEvent;

    try
      Arc.OpenArchive(UTF16ToUTF8(UnicodeString(PackedFile)));

      ArchiveFormat:= ARCHIVE_FORMAT[Arc.ArchiveType];

      Arc.ZipArchive.CompressionLevel:= PluginConfig[ArchiveFormat].Level;

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
      begin
        Arc.FOperationResult := GetArchiveError(E);
        CheckError(Arc, E, Arc.FileName);
      end;
    end;
  finally
    Result := Arc.FOperationResult;
    FreeAndNil(Arc);
  end;
end;

function GetPackerCaps : Integer;dcpcall; export;
begin
  Result := PK_CAPS_NEW      or PK_CAPS_DELETE  or PK_CAPS_MODIFY
         or PK_CAPS_MULTIPLE or PK_CAPS_OPTIONS or PK_CAPS_BY_CONTENT
         or PK_CAPS_ENCRYPT;
end;

function GetBackgroundFlags: Integer; dcpcall; export;
begin
  Result:= BACKGROUND_UNPACK or BACKGROUND_PACK;
end;

procedure ConfigurePacker(Parent: HWND; DllInstance: THandle);dcpcall; export;
begin
  CreateZipConfDlg;
end;

function CanYouHandleThisFileW(FileName: PWideChar): Boolean; dcpcall; export;
begin
  try
    Result:= (AbDetermineArcType(UTF16ToUTF8(UnicodeString(FileName)), atUnknown) <> atUnknown);
  except
    Result := False;
  end;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall; export;
begin
  gStartupInfo:= StartupInfo^;
  // Load configuration from ini file
  LoadConfiguration;
  TranslateResourceStrings;
  // Create password cache object
  PasswordCache:= TPasswordCache.Create;
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
  if (ErrorClass = ecAbbrevia) then
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
  end
  // Has exception message? Show it!
  else if Assigned(ExceptObject) and (ExceptObject is Exception) then
  begin
    Message := Exception(ExceptObject).Message;
    if Assigned(Item) then Message += LineEnding + LineEnding + Item.FileName;
    if (ProcessType = ptExtract) then
    begin
      case gStartupInfo.MessageBox(PAnsiChar(Message), nil, MB_ABORTRETRYIGNORE or MB_ICONERROR) of
      ID_RETRY:
        FOperationResult:= maxLongint;
      ID_IGNORE:
        FOperationResult:= E_HANDLED;
      ID_ABORT:
        raise EAbUserAbort.Create;
      end;
    end
    else begin
      if gStartupInfo.MessageBox(PAnsiChar(Message), nil, MB_OKCANCEL or MB_ICONERROR) = ID_OK then
        FOperationResult:= E_HANDLED
      else begin
        raise EAbUserAbort.Create;
      end;
    end;
  end
  // Check error class
  else case ErrorClass of
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

procedure TAbZipKitEx.AbRequestImageEvent(Sender: TObject;
  ImageNumber: Integer; var ImageName: String; var Abort: Boolean);
var
  AVolume: array[0..MAX_PATH] of WideChar;
begin
  if (not mbFileExists(ImageName)) and Assigned(FChangeVolProcW) then
  begin
    StrPCopy(AVolume, CeUtf8ToUtf16(ImageName));
    Abort := (FChangeVolProcW(AVolume, PK_VOL_ASK) = 0);
    if not Abort then ImageName:= CeUtf16ToUtf8(UnicodeString(AVolume));
  end;
end;

procedure TAbZipKitEx.AbOneItemProgressEvent(Sender: TObject;
  Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
var
  ASize: Int64;
begin
  if Assigned(FProcessDataProcW) then
  begin
    ASize := Item.UncompressedSize;
    if ASize = 0 then
      ASize := -Progress
    else if FItemProgress = Progress then
      ASize := 0
    else begin
      ASize := (Int64(Progress) - Int64(FItemProgress)) * ASize div 100;
      if ASize > High(Int32) then ASize := -Progress;
      FItemProgress := Progress;
    end;
    Abort := (FProcessDataProcW(PWideChar(CeUtf8ToUtf16(Item.FileName)), ASize) = 0);
  end;
end;

procedure TAbZipKitEx.AbArchiveItemProgressEvent(Sender: TObject;
  Item: TAbArchiveItem; Progress: Byte; var Abort: Boolean);
var
  ASize: Int64;
begin
  if Assigned(FProcessDataProcW) then
  begin
    if (Item = nil) then
      Abort := (FProcessDataProcW(nil, -(Progress + 1000)) = 0)
    else begin
      if Item.IsDirectory then
        ASize:= 0
      else if Item.UncompressedSize = 0 then
        ASize:= -(Progress + 1000)
      else begin
        if FItem <> Item then
        begin
          FItem := Item;
          FItemProgress := 0;
        end;
        if FItemProgress = Progress then
          ASize := 0
        else begin
          ASize := Item.UncompressedSize;
          ASize := (Int64(Progress) - Int64(FItemProgress)) * ASize div 100;
          if ASize > High(Int32) then ASize := -(Progress + 1000);
          FItemProgress := Progress;
        end;
      end;
      Abort := (FProcessDataProcW(PWideChar(CeUtf8ToUtf16(Item.FileName)), ASize) = 0)
    end;
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
  else begin
    raise EAbUserAbort.Create;
  end;
  FNeedPassword:= True;
end;

end.


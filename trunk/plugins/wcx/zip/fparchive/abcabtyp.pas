(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbCabTyp.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Cabinet Archive                             *}
{* Based on info from the FCI/FDI Library Description,   *}
{* included in the Microsoft Cabinet SDK                 *}
{*********************************************************}

{$I AbDefine.inc}

unit AbCabTyp;

interface

uses
{$ifndef LINUX}
  Windows,
{$else}
  Libc,  
{$endif}
  SysUtils, Classes, AbFciFdi, AbArcTyp,
  AbUtils;

type
  TAbCabItem = class(TAbArchiveItem)
  protected {private}
    FPartialFile : Boolean;
    FRawFileName : AnsiString;
  public
    property PartialFile : Boolean
      read  FPartialFile
      write FPartialFile;
    property RawFileName : AnsiString
      read FRawFileName
      write FRawFileName;
  end;

type
  TAbCabCompressionType = (ctNone, ctMSZIP, ctLZX);
  TAbCabinetMode = (cmRead, cmWrite);
  TAbCabStatus = (csFile, csFolder, csCabinet);


const
  faExtractAndExecute  = $040;
  faUTF8Name = $080;
  AbDefCabSpanningThreshold  = 0;
  AbDefFolderThreshold = 0;
  AbDefCompressionType = ctMSZIP;
  AbDefReserveHeaderSize = 0;
  AbDefReserveFolderSize = 0;
  AbDefReserveDataSize = 0;
  AbDefLZXWindowSize = 18;

  CompressionTypeMap : array[TAbCabCompressionType] of Word = (0, 1, 4611);

type
  TAbCabArchive = class(TAbArchive)
  protected {private}
    {internal variables}
    FCabName         : AnsiString;
    FCabPath         : AnsiString;
    FFCICabInfo      : FCICabInfo;
    FFCIContext      : HFCI;
    FFDIContext      : HFDI;
    FFDICabInfo      : FDICabInfo;
    FErrors          : CabErrorRecord;
    FItemInProgress  : TAbCabItem;
    FItemStream      : TStream;
    FIIPName         : string;
    FItemProgress    : DWord;
    FNextCabinet     : string;
    FNextDisk        : string;
    FTempFileID      : Integer;

    {property variables}
    FCurrentCab         : Word;
    FCabSize           : Longint;
    FCompressionType   : TAbCabCompressionType;
    FFileCount         : Word;
    FFolderThreshold   : LongWord;
    FFolderCount       : Word;
    FHasPrev           : Boolean;
    FHasNext           : Boolean;
    FSetID             : Word;

    {internal methods}
    procedure CloseCabFile;
    procedure CreateCabFile;
    function  CreateItem(const SourceFileName   : string;
                         const ArchiveDirectory : string): TAbArchiveItem;
      override;
    procedure DoCabItemProcessed;
    procedure DoCabItemProgress(BytesCompressed : DWord;
      var Abort : Boolean);
    procedure DoGetNextCabinet(CabIndex : Integer; var CabName : string;
                               var Abort : Boolean);
    procedure ExtractItemAt(Index : Integer; const NewName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; OutStream : TStream);
      override;
    function  GetItem(ItemIndex : Integer) : TAbCabItem;
    procedure LoadArchive;
      override;
    procedure OpenCabFile;
    procedure PutItem( Index : Integer; Value : TAbCabItem );
    procedure SaveArchive;
      override;
    procedure SetFolderThreshold(Value : LongWord);
    procedure SetSetID(Value : Word);
    procedure SetSpanningThreshold(Value : Int64);
      override;
    procedure TestItemAt(Index : Integer);
      override;

  public {methods}
    constructor Create(const FileName : string; Mode : Word);
      override;
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string);
      override;
    destructor Destroy;
      override;
    procedure Add(aItem : TAbArchiveItem);
      override;
    procedure NewCabinet;
    procedure NewFolder;

  public {properties}
    property CurrentCab : Word
      read  FCurrentCab;
    property CabSize : Longint
      read  FCabSize;
    property CompressionType : TAbCabCompressionType
      read  FCompressionType
      write FCompressionType;
    property FolderThreshold : LongWord
      read  FFolderThreshold
      write SetFolderThreshold;
    property FolderCount : Word
      read  FFolderCount;
    property HasPrev : Boolean
      read  FHasPrev;
    property HasNext : Boolean
      read  FHasNext;
    property Items[Index : Integer] : TAbCabItem
      read  GetItem
      write PutItem; default;
    property ItemProgress : DWord
      read  FItemProgress
      write FItemProgress;
    property SetID : Word
      read  FSetID
      write SetSetID;
  end;

function VerifyCab(const Fn : string) : TAbArchiveType; overload;
function VerifyCab(Strm : TStream) : TAbArchiveType; overload;

implementation

uses
  AbConst, AbExcept, uClassesEx;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

type
  PWord    = ^Word;
  PInteger = ^Integer;

{ == FDI/FCI Callback Functions - cdecl calling convention ================= }
function FXI_GetMem(uBytes : Integer) : Pointer;
  cdecl;
  {allocate memory}
begin
  Result := nil;
  if (uBytes > 0) then
    GetMem(Result, uBytes);
end;
{ -------------------------------------------------------------------------- }
procedure FXI_FreeMem(lpBuffer : Pointer);
  cdecl;
  {free memory}
begin
  Dispose(lpBuffer);
end;


{ == FCI Callback Functions - cdecl calling convention ===================== }
function FCI_FileOpen(lpPathName: PAnsiChar; Flag, Mode: Integer;
  PError: PInteger; Archive: TAbCabArchive) : HFILE;
  cdecl;
  {open a file}
begin
  Result := _lcreat(lpPathName, 0);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileOpenError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileRead(hFile: HFILE; lpBuffer: Pointer;
  uBytes: UINT; PError: PInteger; Archive: TAbCabArchive) : HFILE;
  cdecl;
  {read from a file}
begin
  Result := _lread(hFile, lpBuffer, uBytes);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileReadError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileWrite(hFile: HFILE; lpBuffer: Pointer;
  uBytes: UINT; PError: PInteger; Archive: TAbCabArchive) : HFILE;
  cdecl;
  {write to a file}
begin
  Result := _lwrite(hFile, lpBuffer, uBytes);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileWriteError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileClose(hFile: HFILE; PError: PInteger;
  Archive: TAbCabArchive) : HFILE;
  cdecl;
  {close a file}
begin
  Result := _lclose(hFile);
  if (Result = HFILE_ERROR) then
    raise EAbFCIFileCloseError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileSeek(hFile: HFILE; Offset: Longint;
  Origin: Integer; PError: PInteger; Archive: TAbCabArchive) : Longint;
  cdecl;
  {reposition file pointer}
begin
  Result := _llseek(hFile, Offset, Origin);
  if (Result = -1) then
    raise EAbFCIFileSeekError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileDelete(lpFilename: PAnsiChar;  PError: PInteger;
  Archive: TAbCabArchive) : Boolean;
  cdecl;
  {delete a file}
begin
  Result := mbDeleteFile(string(lpFilename));
  if not Result then
    raise EAbFCIFileDeleteError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_GetNextCab(lpCCab: PFCICabInfo; PrevCab: Longint;
  Archive: TAbCabArchive) : Boolean;
  cdecl;
  {get next cabinet filename}
var
  CabName : string;
  Abort : Boolean;
begin
  Abort := False;
  with lpCCab^ do begin
    CabName := string(szCab);                                            {!!.02}
    {obtain next cabinet.  Make index zero-based}
    Archive.DoGetNextCabinet(Pred(iCab), CabName, Abort);
    if not Abort then
      StrPLCopy(szCab, AnsiString(CabName), Length(szCab));              {!!.02}
  end;
  Result := not Abort;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileDest(PCCab: PFCICabInfo; PFilename: PAnsiChar; cbFile: Longint;
  Continuation: Boolean; Archive: TAbCabArchive) : Integer;
  cdecl;
  {currently not used}
begin
  Result := 0;
end;
{ -------------------------------------------------------------------------- }
function FCI_GetOpenInfo(lpPathname: Pointer; PDate, PTime, PAttribs : PWord;
  PError: PInteger; Archive: TAbCabArchive) : Integer;
  cdecl;
  {open a file and return date/attributes}
var
  AttrEx: TAbAttrExRec;
  I: Integer;
  RawName: RawByteString;
begin
  Result := mbFileOpen(string(lpPathname), fmOpenRead or fmShareDenyNone);
  if (Result = -1) then
    raise EAbFCIFileOpenError.Create;
  AbFileGetAttrEx(string(lpPathname), AttrEx);
  PAttribs^ := AttrEx.Attr;
  PDate^ := AttrEx.Time shr 16;
  PTime^ := AttrEx.Time and $0FFFF;
  Archive.ItemProgress := 0;
  Archive.FItemInProgress.UncompressedSize := AttrEx.Size;
  RawName := Archive.FItemInProgress.RawFileName;
  for I := 1 to Length(RawName) do
    if Ord(RawName[I]) > 127 then
      PAttribs^ := PAttribs^ or faUTF8Name;
end;
{ -------------------------------------------------------------------------- }
function FCI_Status(Status: Word; cb1, cb2: DWord;
                    Archive: TAbCabArchive) : Longint; cdecl;
  {keep archive informed}
var
  Abort : Boolean;
begin
  Result := 0;
  if (Status = Word(csCabinet)) then begin
    Archive.DoSave;
    Archive.FCabSize := cb2;
    Result := cb2;
  end else if (Status = Word(csFolder)) then
    Archive.FCabSize := Archive.FCabSize + Longint(cb2)
  else if (Status = Word(csFile)) then begin
    Archive.DoCabItemProgress(cb2, Abort);
    Result := Longint(Abort);
  end;
end;
{ -------------------------------------------------------------------------- }
function FCI_GetTempFile(lpTempName: PAnsiChar; TempNameSize: Integer;
                         Archive: TAbCabArchive) : Integer; cdecl;
  {obtain temporary filename}
var
  TempPath : array[0..255] of AnsiChar;
begin
  Archive.FTempFileID := Archive.FTempFileID + 1;
  if (Archive.TempDirectory <> '') then
    StrPLCopy(TempPath, AnsiString(Archive.TempDirectory), Length(TempPath)){!!.02}
  else
    GetTempPathA(255, TempPath);                                         {!!.02}
  GetTempFileNameA(TempPath, 'VMS', Archive.FTempFileID, lpTempName);    {!!.02}
  Result := 1;
end;

{ == FDI Callback Functions - cdecl calling convention ===================== }
function FDI_FileOpen(lpPathName: PAnsiChar; Flag, Mode: Integer) : Integer;
  cdecl;
  {open a file}
var
  Handle : THandle;
begin
  Handle := mbFileOpen(string(lpPathName), fmOpenRead or fmShareDenyWrite);
  if Handle <> -1 then
    Result := Integer(THandleStream.Create(Handle))
  else
    Result := -1;
end;
{ -------------------------------------------------------------------------- }
function FDI_FileRead(hFile: HFILE; lpBuffer: Pointer; uBytes: UINT) : UINT;
  cdecl;
  {read from a file}
begin
  Result := TStream(hFile).Read(lpBuffer^, uBytes);
end;
{ -------------------------------------------------------------------------- }
function FDI_FileWrite(hFile: HFILE; lpBuffer: Pointer; uBytes: UINT) : UINT;
  cdecl;
  {write to a file}
begin
  Result := TStream(hFile).Write(lpBuffer^, uBytes);
end;
{ -------------------------------------------------------------------------- }
procedure FDI_FileClose(hFile : HFILE);
  cdecl;
  {close a file}
begin
  TStream(hFile).Free;
end;
{ -------------------------------------------------------------------------- }
function FDI_FileSeek(hFile : HFILE; Offset : Longint; Origin : Integer) : Longint;
  cdecl;
  {reposition file pointer}
begin
  Result := TStream(hFile).Seek(Offset, Origin);
end;
{ -------------------------------------------------------------------------- }
function FDI_EnumerateFiles(fdint : FDINOTIFICATIONTYPE;
  pfdin : PFDINotification) : Integer;
  cdecl;
  {Enumerate the files and build the archive file list}
var
  Item : TAbCabItem;
  Archive : TAbCabArchive;
begin
  Result := 1;
  Archive := pfdin^.pv;
  with Archive do case fdint of
    FDINT_Cabinet_Info :
      begin
        FSetID := pfdin^.setID;
        FCurrentCab := pfdin^.iCabinet;
        FNextCabinet := string(pfdin^.psz1);
        FNextDisk    := string(pfdin^.psz2);
        Result := 0;
      end;
    FDINT_Copy_File, FDINT_Partial_File :
      begin
        Item := TAbCabItem.Create;
        with Item do begin
          RawFileName := AnsiString(pfdin^.psz1);
          if (pfdin^.attribs and faUTF8Name) = faUTF8Name then
            Filename := UTF8ToString(RawFileName)
          else
            Filename := string(RawFileName);
          UnCompressedSize := pfdin^.cb;
          LastModFileDate := pfdin^.date;
          LastModFileTime := pfdin^.time;
          ExternalFileAttributes := pfdin^.attribs;
          IsEncrypted := False;  {encryption not implemented at this time}
          PartialFile := (fdint = FDINT_Partial_File);
        end;
        FItemList.Add(Item);
        Result := 0;
      end;
  end;
end;
{ -------------------------------------------------------------------------- }
function FDI_ExtractFiles(fdint : FDINOTIFICATIONTYPE;
  pfdin : PFDINotification) : Integer;
  cdecl;
  {extract file from cabinet}
var
  Archive : TAbCabArchive;
  NextCabName : string;
begin
  Result := 0;
  Archive := pfdin^.pv;
  case fdint of
    FDINT_Copy_File :
      begin
        if (AnsiString(pfdin^.psz1) = Archive.FItemInProgress.RawFileName) then
          if Archive.FIIPName <> '' then
            Result := Integer(TFileStream.Create(Archive.FIIPName, fmCreate))
          else
            Result := Integer(Archive.FItemStream)
        else
          Result := 0;
      end;
    FDINT_Next_Cabinet :
      begin
        Result := 1;
        NextCabName := string(pfdin^.psz3) + string(pfdin^.psz1);
      end;
    FDINT_Close_File_Info :
      begin
        if Archive.FIIPName <> '' then begin
          FileSetDate(TFileStream(pfdin^.hf).Handle,
            Longint(pfdin^.date) shl 16 + pfdin^.time);
          TFileStream(pfdin^.hf).Free;
          // [ 880505 ]  Need to Set Attributes after File is closed {!!.05}
          AbFileSetAttr(Archive.FIIPName, pfdin^.attribs);
        end;
        Archive.DoCabItemProcessed;
      end;
  end;
end;


{ == TAbCabArchive ========================================================= }
function VerifyCab(const Fn : string) : TAbArchiveType;
var
  Stream : TFileStream;
begin
  Stream := TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
  try
    Result := VerifyCab(Stream);
  finally
    Stream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
function VerifyCab(Strm : TStream) : TAbArchiveType; overload;
var
  Context : HFDI;
  Info : FDICabInfo;
  Errors : CabErrorRecord;
begin
  Result := atUnknown;
  Context := FDICreate(@FXI_GetMem, @FXI_FreeMem, @FDI_FileOpen,
    @FDI_FileRead, @FDI_FileWrite, @FDI_FileClose, @FDI_FileSeek, cpuDefault,
    @Errors);
  if Context = nil then
    Exit;
  try
    if FDIIsCabinet(Context, Integer(Strm), @Info) then
      Result := atCab;
  finally
    FDIDestroy(Context);
  end;
end;


{ == TAbCabArchive ========================================================= }
constructor TAbCabArchive.Create(const FileName : string; Mode : Word );
begin
  {Mode is used to identify which interface to use: }
  {  fmOpenWrite - FCI, fmOpenRead - FDI}
  inherited CreateInit;
  FMode := Mode and fmOpenWrite;
  FArchiveName := FileName;
  FCabName := AnsiString(ExtractFileName(FileName));
  FCabPath := AnsiString(ExtractFilePath(FileName));
  SpanningThreshold := AbDefCabSpanningThreshold;
  FFolderThreshold := AbDefFolderThreshold;
  FItemInProgress := nil;
  FItemProgress := 0;
end;
{ -------------------------------------------------------------------------- }
constructor TAbCabArchive.CreateFromStream(aStream : TStream;
  const aArchiveName : string);
begin
  raise EAbCabException.Create('TAbCabArchive does not support CreateFromStream');
end;
{ -------------------------------------------------------------------------- }
destructor TAbCabArchive.Destroy;
begin
  CloseCabFile;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.Add(aItem : TAbArchiveItem);
  {add a file to the cabinet}
var
  Confirm, ItemAdded : Boolean;
  Item : TAbCabItem;
begin
  ItemAdded := False;
  try
    CheckValid;
    if (FMode <> fmOpenWrite) then begin
      DoProcessItemFailure(aItem, ptAdd, ecCabError, 0);
      Exit;
    end;
    if FItemList.IsActiveDupe(aItem.FileName) then begin
      DoProcessItemFailure(aItem, ptAdd, ecAbbrevia, AbDuplicateName);
      Exit;
    end;
    DoConfirmProcessItem(aItem, ptAdd, Confirm);
    if not Confirm then
      Exit;
    Item := TAbCabItem(aItem);
    FItemInProgress := Item;
    Item.Action := aaAdd;

    Item.RawFileName := UTF8Encode(Item.FileName);
    if not FCIAddFile(FFCIContext, Pointer(Item.DiskFileName),
       PAnsiChar(Item.RawFileName), False, @FCI_GetNextCab, @FCI_Status,
       @FCI_GetOpenInfo, CompressionTypeMap[FCompressionType]) then
      raise EAbFCIAddFileError.Create;
    FItemList.Add(Item);
    ItemAdded := True;

    FIsDirty := True;
  finally
    if not ItemAdded then
      aItem.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.CloseCabFile;
  {Make sure the Cabinet DLL is shut down}
var
  Abort : Boolean;
begin
  if (FFDIContext <> nil) then begin
    FDIDestroy(FFDIContext);
    FFDIContext := nil;
  end;
  if (FFCIContext <> nil) then begin
    FCIFlushCabinet(FFCIContext, False, @FCI_GetNextCab, @FCI_Status);
    FCIDestroy(FFCIContext);
    FFCIContext := nil;
  end;
  DoArchiveProgress(0, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.CreateCabFile;
  {create a new cabinet}
begin
    {set cabinet parameters}
  with FFCICabInfo do begin
    if (SpanningThreshold > 0) then
      cb := SpanningThreshold
    else
      cb := AbDefCabSpanningThreshold;
    if (FolderThreshold > 0) then
      cbFolderThresh := FolderThreshold
    else
      cbFolderThresh  := AbDefFolderThreshold;
    cbReserveCFHeader := AbDefReserveHeaderSize;
    cbReserveCFFolder := AbDefReserveFolderSize;
    cbReserveCFData   := AbDefReserveDataSize;
    iCab              := 1;
    iDisk             := 0;
    fFailOnIncompressible := 0;
    setID             := SetID;
    StrPCopy(szDisk, '');
    StrPLCopy(szCab, FCabName, Length(szCab));
    StrPLCopy(szCabPath, FCabPath, Length(szCabPath));
  end;

    {obtain an FCI context}
  FFCIContext := FCICreate(@FErrors, @FCI_FileDest, @FXI_GetMem, @FXI_FreeMem,
    @FCI_FileOpen, @FCI_FileRead, @FCI_FileWrite, @FCI_FileClose, @FCI_FileSeek,
    @FCI_FileDelete, @FCI_GetTempFile, @FFCICabInfo, Self);
  if (FFCIContext = nil) then
    if FErrors.ErrorPresent then begin
      CloseCabFile;
      raise EAbFCICreateError.Create;
    end;
end;
{ -------------------------------------------------------------------------- }
function TAbCabArchive.CreateItem(const SourceFileName   : string;
                                  const ArchiveDirectory : string): TAbArchiveItem;
  {create a new item for the file list}
var
  FullSourceFileName, FullArchiveFileName : string;
begin
  Result := TAbCabItem.Create;
  with TAbCabItem(Result) do begin
    CompressedSize := 0;

    MakeFullNames(SourceFileName, ArchiveDirectory,
                  FullSourceFileName, FullArchiveFileName);

    Result.FileName := FullArchiveFileName;
    Result.DiskFileName := FullSourceFileName;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoCabItemProcessed;
  {allow messages to be processed}
begin
//  Application.ProcessMessages;                                       {!!.04}
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoCabItemProgress(BytesCompressed : DWord;
  var Abort : Boolean);
  {fire OnCabItemProgress event}
var
  Progress : Byte;
begin
  Abort := False;
  if Assigned(FOnArchiveItemProgress) then begin
    Inc(FItemProgress, BytesCompressed);
    Progress := AbPercentage(FItemProgress,
      FItemInProgress.UnCompressedSize);
    FOnArchiveItemProgress(Self, FItemInProgress, Progress, Abort);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoGetNextCabinet(CabIndex : Integer;
  var CabName : string; var Abort : Boolean);
  {fire OnRequestImage event}
begin
  Abort := False;
  if Assigned(FOnRequestImage) then
    FOnRequestImage(Self, CabIndex, CabName, Abort)
  else
    AbIncFilename(CabName, CabIndex);
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.ExtractItemAt(Index : Integer; const NewName : string);
  {extract a file from the cabinet}
begin
  FItemInProgress := GetItem(Index);
  FIIPName := NewName;
  try
    if not FDICopy(FFDIContext, PAnsiChar(FCabName), PAnsiChar(FCabPath), 0,
                   @FDI_ExtractFiles, nil, Self) then
      DoProcessItemFailure(FItemInProgress, ptExtract, ecCabError, FErrors.ErrorCode);
  finally
    FIIPName := '';
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.ExtractItemToStreamAt(Index : Integer; OutStream : TStream);
begin
  {not implemented for cabinet archives}
  FItemInProgress := GetItem(Index);
  FItemStream := OutStream;
  try
    if not FDICopy(FFDIContext, PAnsiChar(FCabName), PAnsiChar(FCabPath), 0,
                   @FDI_ExtractFiles, nil, Self) then
      DoProcessItemFailure(FItemInProgress, ptExtract, ecCabError, FErrors.ErrorCode);
  finally
    FItemStream := nil;
  end;
end;
{----------------------------------------------------------------------------}
function TAbCabArchive.GetItem(ItemIndex : Integer) : TAbCabItem;
  {fetch an item from the file list}
begin
  Result := TAbCabItem(FItemList.Items[ItemIndex]);
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.LoadArchive;
  {Open existing cabinet or create a new one}
begin
  if (FMode = fmOpenRead) then begin
    FFDIContext := FDICreate(@FXI_GetMem, @FXI_FreeMem, @FDI_FileOpen,
    @FDI_FileRead, @FDI_FileWrite, @FDI_FileClose, @FDI_FileSeek,
    cpuDefault, @FErrors);
    if (FFDIContext = nil) then
      raise EAbFDICreateError.Create;
    OpenCabFile;
  end else
    CreateCabFile;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.NewCabinet;
  {flush current cabinet and start a new one}
begin
  if not FCIFlushCabinet(FFCIContext, True, @FCI_GetNextCab, @FCI_Status) then
    raise EAbFCIFlushCabinetError.Create;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.NewFolder;
  {flush current folder and start a new one}
begin
  if not FCIFlushFolder(FFCIContext, @FCI_GetNextCab, @FCI_Status) then
    raise EAbFCIFlushFolderError.Create;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.OpenCabFile;
  {Open an existing cabinet}
var
  Abort : Boolean;
  Stream : TFileStream;
begin
    {verify that the archive can be opened and is a cabinet}
  Stream := TFileStream.Create(FArchiveName, fmOpenRead or fmShareDenyNone);
  try
    if not FDIIsCabinet(FFDIContext, Integer(Stream), @FFDICabInfo) then begin
      CloseCabFile;
      raise EAbInvalidCabFile.Create;
    end;
  finally
    Stream.Free;
  end;

    {store information about the cabinet}
  FCabSize := FFDICabInfo.cbCabinet;
  FFolderCount := FFDICabInfo.cFolders;
  FFileCount := FFDICabInfo.cFiles;
  FCurrentCab := FFDICabInfo.iCabinet;
  FHasPrev := FFDICabInfo.hasPrev;
  FHasNext := FFDICabInfo.hasNext;

    {Enumerate the files and build the file list}
  if not FDICopy(FFDIContext, PAnsiChar(FCabName), PAnsiChar(FCabPath), 0,
    @FDI_EnumerateFiles, nil, Self) then begin
    CloseCabFile;
    raise EAbFDICopyError.Create;
  end;
  DoArchiveProgress(100, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.PutItem( Index : Integer; Value : TAbCabItem );
  {replace an existing item in the file list}
begin
  FItemList.Items[Index] := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SaveArchive;
begin
  { No-op;  file is flushed in destructor }
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetFolderThreshold(Value : LongWord);
  {set maximum compression boundary}
begin
  if (Value > 0) then
    FFolderThreshold := Value
  else
    FFolderThreshold := AbDefFolderThreshold;
  FFCICabInfo.cbFolderThresh := FFolderThreshold;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetSetID(Value : Word);
  {set cabinet SetID}
begin
  FSetID := Value;
  FFCICabInfo.SetID := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetSpanningThreshold(Value : Int64);
  {set maximum cabinet size}
begin
  if (Value > 0) then
    FSpanningThreshold := Value
  else
    FSpanningThreshold := AbDefCabSpanningThreshold;
  FFCICabInfo.cb := FSpanningThreshold;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.TestItemAt(Index : Integer);
begin
  {not implemented for cabinet archives}
end;

end.

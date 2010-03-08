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
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbArcTyp.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: TABArchive, TABArchiveItem classes          *}
{*********************************************************}

{$I AbDefine.inc}

unit AbArcTyp;

interface

uses
  Classes,
  AbUtils,
  SysUtils,
  Types;

{ ===== TAbArchiveItem ====================================================== }
type
  TAbArchiveItem = class(TObject)
  private
    NextItem          : TAbArchiveItem;
    FAction           : TAbArchiveAction;
    FCompressedSize   : Int64;
    FCRC32            : Longint;
    FDiskFileName     : string;
    FExternalFileAttributes : LongWord;
    FFileName         : string;
    FIsDirectory      : Boolean;
    FIsEncrypted      : Boolean;
    FLastModFileTime  : Word;
    FLastModFileDate  : Word;
    FTagged           : Boolean;
    FUncompressedSize : Int64;

  protected {property methods}
    function GetCompressedSize : Int64; virtual;
    function GetCRC32 : Longint; virtual;
    function GetDiskPath : string;
    function GetExternalFileAttributes : LongWord; virtual;
    function GetFileName : string; virtual;
    function GetIsDirectory: Boolean;
    function GetIsEncrypted : Boolean; virtual;
    function GetLastModDateTime : TDateTime; virtual;
    function GetLastModFileDate : Word; virtual;
    function GetLastModFileTime : Word; virtual;
    { This depends on in what format the attributes are stored in the archive,
      to which system they refer (MS-DOS, Unix, etc.) and what system
      we're running on (compile time). }
    function GetSystemSpecificAttributes : LongWord; virtual;
    { This depends on in what format the date/time is stored in the archive
      (Unix, MS-DOS, ...) and what system we're running on (compile time).
      Returns MS-DOS local time on Windows, Unix UTC time on Unix. }
    function GetSystemSpecificLastModFileTime : Longint; virtual;
    function GetStoredPath : string;
    function GetUncompressedSize : Int64; virtual;
    procedure SetCompressedSize(const Value : Int64); virtual;
    procedure SetCRC32(const Value : Longint); virtual;
    procedure SetExternalFileAttributes( Value : LongWord ); virtual;
    procedure SetFileName(const Value : string); virtual;
    procedure SetIsDirectory(const Value: Boolean);
    procedure SetIsEncrypted(Value : Boolean); virtual;
    procedure SetLastModDateTime(const Value : TDateTime); virtual;
    procedure SetLastModFileDate(const Value : Word); virtual;
    procedure SetLastModFileTime(const Value : Word); virtual;
    { Attempts to translate attributes across systems. }
    procedure SetSystemSpecificAttributes(Value : LongWord); virtual;
    { Expects MS-DOS local time on Windows, Unix UTC time on Unix. }
    procedure SetSystemSpecificLastModFileTime(const Value: Longint); virtual;
    procedure SetUncompressedSize(const Value : Int64); virtual;

  public {methods}
    constructor Create; virtual;
    destructor Destroy; override;
    function MatchesDiskName(const FileMask : string) : Boolean;
    function MatchesStoredName(const FileMask : string; Recursive : Boolean = False) : Boolean;
    function MatchesStoredNameEx(const FileMask : string; Recursive : Boolean = False) : Boolean;
    function MatchesPath(const Path : String; Recursive : Boolean = False) : Boolean;
    function MatchesPathEx(const Paths : String; Recursive : Boolean = False) : Boolean;


  public {properties}
    property Action : TAbArchiveAction
      read FAction
      write FAction;
    property CompressedSize : Int64
      read GetCompressedSize
      write SetCompressedSize;
    property CRC32 : Longint
      read GetCRC32
      write SetCRC32;
    property DiskFileName : string
      read FDiskFileName
      write FDiskFileName;
    property DiskPath : string
      read GetDiskPath;
    property ExternalFileAttributes : LongWord
      read GetExternalFileAttributes
      write SetExternalFileAttributes;
    property FileName : string
      read GetFileName
      write SetFileName;
    property IsDirectory: Boolean
      read GetIsDirectory
      write SetIsDirectory;
    property IsEncrypted : Boolean
      read GetIsEncrypted
      write SetIsEncrypted;
    property LastModDateTime : TDateTime
      read GetLastModDateTime
      write SetLastModDateTime;
    property LastModFileDate : Word
      read GetLastModFileDate
      write SetLastModFileDate;
    property LastModFileTime : Word
      read GetLastModFileTime
      write SetLastModFileTime;
    property SystemSpecificAttributes : LongWord
      read GetSystemSpecificAttributes
      write SetSystemSpecificAttributes;
    property SystemSpecificLastModFileTime : Longint
      read GetSystemSpecificLastModFileTime
      write SetSystemSpecificLastModFileTime;
    property StoredPath : string
      read GetStoredPath;
    property Tagged : Boolean
      read FTagged
      write FTagged;
    property UncompressedSize : Int64
      read GetUncompressedSize
      write SetUncompressedSize;
  end;


{ ===== TAbArchiveList ====================================================== }
type
  TAbArchiveList = class
  protected {private}
    FList     : TList;
    FOwnsItems: Boolean;
    HashTable : array[0..1020] of TAbArchiveItem;
  protected {methods}
    function GenerateHash(const S : string) : LongInt;
    function GetCount : Integer;
    function Get(Index : Integer) : TAbArchiveItem;
    procedure Put(Index : Integer; Item : TAbArchiveItem);
  public {methods}
    constructor Create(AOwnsItems: Boolean);
    destructor Destroy; override;
    function Add(Item : Pointer): Integer;
    procedure Clear;
    procedure Delete(Index : Integer);
    function Find(const FN : string) : Integer;
    function IsActiveDupe(const FN : string) : Boolean;
  public {properties}
    property Count : Integer
      read GetCount;
    property Items[Index : Integer] : TAbArchiveItem
      read Get
      write Put; default;
  end;


{ ===== TAbArchive specific types =========================================== }
type
  TAbStoreOption =
    (soStripDrive, soStripPath, soRemoveDots, soRecurse, soFreshen, soReplace);
  TAbStoreOptions =
    set of TAbStoreOption;

  TAbExtractOption =
    (eoCreateDirs, eoRestorePath);
  TAbExtractOptions =
    set of TAbExtractOption;

  TAbArchiveStatus =
    (asInvalid, asIdle, asBusy);

  TAbArchiveEvent =
    procedure(Sender : TObject) of object;
  TAbArchiveConfirmEvent =
    procedure (Sender : TObject; var Confirm : Boolean) of object;
  TAbArchiveProgressEvent =
    procedure(Sender : TObject; Progress : Byte; var Abort : Boolean) of object;
  TAbArchiveItemEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem) of object;
  TAbArchiveItemConfirmEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      ProcessType : TAbProcessType; var Confirm : Boolean) of object;
  TAbConfirmOverwriteEvent =
    procedure(var Name : string; var Confirm : Boolean) of object;
  TAbArchiveItemFailureEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
      ErrorCode : Integer) of object;
  TAbArchiveItemExtractEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      const NewName : string) of object;
  TAbArchiveItemExtractToStreamEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream) of object;
  TAbArchiveItemTestEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem) of object;
  TAbArchiveItemInsertEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream) of object;
  TAbArchiveItemInsertFromStreamEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem;
      OutStream, InStream : TStream) of object;
  TAbArchiveItemProgressEvent =
    procedure(Sender : TObject; Item : TAbArchiveItem; Progress : Byte;
      var Abort : Boolean) of object;
  TAbProgressEvent =
    procedure(Progress : Byte; var Abort : Boolean) of object;
  TAbRequestDiskEvent =
    procedure(Sender : TObject; var Abort : Boolean) of object;
  TAbRequestImageEvent =
    procedure(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean) of object;
  TAbRequestNthDiskEvent =
    procedure(Sender : TObject; DiskNumber : Byte; var Abort : Boolean) of object;


type
  TAbArchiveStreamHelper = class
  protected
    FStream : TStream;
  public
    constructor Create(AStream : TStream);
    procedure ExtractItemData(AStream : TStream); virtual; abstract;
    function FindFirstItem : Boolean; virtual; abstract;
    function FindNextItem : Boolean; virtual; abstract;
    procedure ReadHeader; virtual; abstract;
    procedure ReadTail; virtual; abstract;
    function SeekItem(Index : Integer): Boolean; virtual; abstract;
    procedure WriteArchiveHeader; virtual; abstract;
    procedure WriteArchiveItem(AStream : TStream); virtual; abstract;
    procedure WriteArchiveTail; virtual; abstract;
    function GetItemCount : Integer; virtual; abstract;
  end;


{ ===== TAbArchive ========================================================== }
type
  TAbArchive = class(TObject)
  public
    FStream         : TStream;
    FStatus         : TAbArchiveStatus;

  protected {property variables}    //These break Encapsulation
    FArchiveName    : string;
    FAutoSave       : Boolean;
    FBaseDirectory  : string;
    FCurrentItem    : TAbArchiveItem;
    FDOSMode        : Boolean;
    FExtractOptions : TAbExtractOptions;
    FImageNumber    : Word;
    FInStream       : TStream;
    FIsDirty        : Boolean;
    FSpanningThreshold      : Int64;
    FItemList       : TAbArchiveList;
    FLogFile        : string;
    FLogging        : Boolean;
    FLogStream      : TFileStream;
    FMode           : Word;
    FOwnsStream     : Boolean;
    FSpanned        : Boolean;
    FStoreOptions   : TAbStoreOptions;
    FTempDir        : string;

  protected {event variables}
    FOnProcessItemFailure  : TAbArchiveItemFailureEvent;
    FOnArchiveProgress     : TAbArchiveProgressEvent;
    FOnArchiveSaveProgress : TAbArchiveProgressEvent;                  {!!.04}
    FOnArchiveItemProgress : TAbArchiveItemProgressEvent;
    FOnConfirmProcessItem  : TAbArchiveItemConfirmEvent;
    FOnConfirmOverwrite    : TAbConfirmOverwriteEvent;
    FOnConfirmSave         : TAbArchiveConfirmEvent;
    FOnLoad                : TAbArchiveEvent;
    FOnProgress            : TAbProgressEvent;
    FOnRequestImage        : TAbRequestImageEvent;
    FOnSave                : TAbArchiveEvent;

  protected {methods}
    constructor CreateInit;
    procedure CheckValid;
    function  ConfirmPath(Item : TAbArchiveItem; const NewName : string;
      out UseName : string) : Boolean;
    procedure FreshenAt(Index : Integer);
    function  FreshenRequired(Item : TAbArchiveItem) : Boolean;
    procedure GetFreshenTarget(Item : TAbArchiveItem);
    function  GetItemCount : Integer;
    procedure MakeLogEntry(const FN: string; LT : TAbLogType);
    procedure MakeFullNames(const SourceFileName: String;
                            const ArchiveDirectory: String;
                            out   FullSourceFileName: String;
                            out   FullArchiveFileName: String);
    procedure ReplaceAt(Index : Integer);
    procedure SaveIfNeeded(aItem : TAbArchiveItem);
    procedure SetBaseDirectory(Value : string);
    procedure SetLogFile(const Value : string);
    procedure SetLogging(Value : Boolean);

   	class function SupportsEmptyFolder: Boolean; virtual;

  protected {abstract methods}
    function CreateItem(const SourceFileName : string;
                        const ArchiveDirectory : string): TAbArchiveItem;
    {SourceFileName   - full or relative path to a file/dir on some file system
                        If full path, BaseDirectory is used to determine relative path}
    {ArchiveDirectory - path to a directory in the archive the file/dir will be in}
    {Example:
      FBaseDirectory      = /dir
      SourceFileName      = /dir/subdir/file
      ArchiveDirectory    = files/storage  (or files/storage/)
      -> name in archive  = files/storage/subdir/file}
      virtual; abstract;
    procedure ExtractItemAt(Index : Integer; const UseName : string);
      virtual; abstract;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
      virtual; abstract;
    procedure LoadArchive;
      virtual; abstract;
    procedure SaveArchive;
      virtual; abstract;
    procedure TestItemAt(Index : Integer);
      virtual; abstract;

  protected {virtual methods}
    procedure DoProcessItemFailure(Item : TAbArchiveItem;
      ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
      ErrorCode : Integer);
      virtual;
    procedure DoArchiveSaveProgress(Progress : Byte; var Abort : Boolean); {!!.04}
      virtual;                                                             {!!.04}
    procedure DoArchiveProgress(Progress : Byte; var Abort : Boolean);
      virtual;
    procedure DoArchiveItemProgress(Item : TAbArchiveItem; Progress : Byte;
      var Abort : Boolean);
      virtual;
    procedure DoConfirmOverwrite(var FileName : string; var Confirm : Boolean);
      virtual;
    procedure DoConfirmProcessItem(Item : TAbArchiveItem;
      const ProcessType : TAbProcessType; var Confirm : Boolean);
      virtual;
    procedure DoConfirmSave(var Confirm : Boolean);
      virtual;

    procedure DoLoad;
      virtual;
    procedure DoProgress(Progress : Byte; var Abort : Boolean);
      virtual;
    procedure DoSave;
      virtual;
    function FixName(const Value : string) : string;
      virtual;
    function GetSpanningThreshold : Int64;
      virtual;
    procedure SetSpanningThreshold( Value : Int64 );
      virtual;

  protected {properties and events}
    property InStream : TStream
      read FInStream;

  public {methods}
    constructor Create(const FileName : string; Mode : Word);
      virtual;
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string);
      virtual;
    destructor  Destroy;
      override;
    procedure Add(aItem : TAbArchiveItem);
      virtual;
    procedure AddEntry(const Path : String; const ArchiveDirectory : String);
    procedure AddEntries(const Paths : String; const ArchiveDirectory : String);
    procedure AddFiles(const FileMask : string; SearchAttr : Integer);
    procedure AddFilesEx(const FileMask, ExclusionMask : string;
      SearchAttr : Integer);
    procedure AddFromStream(const NewName : string; aStream : TStream);
    procedure ClearTags;
    procedure Delete(aItem : TAbArchiveItem);
    procedure DeleteAt(Index : Integer);
    procedure DeleteFiles(const FileMask : string; Recursive : Boolean = False);
    procedure DeleteFilesEx(const FileMask, ExclusionMask : string;
      Recursive : Boolean = False);
    procedure DeleteDirectoriesRecursively(const Paths : string);
    procedure DeleteTaggedItems;
    procedure Extract(aItem : TAbArchiveItem; const NewName : string);
    procedure ExtractAt(Index : Integer; const NewName : string);
    procedure ExtractFiles(const FileMask : string);
    procedure ExtractFilesEx(const FileMask, ExclusionMask : string);
    procedure ExtractTaggedItems;
    procedure ExtractToStream(const aFileName : string; aStream : TStream);
    function  FindFile(const aFileName : string): Integer;
    function  FindItem(aItem : TAbArchiveItem): Integer;
    procedure Freshen(aItem : TAbArchiveItem);
    procedure FreshenFiles(const FileMask : string);
    procedure FreshenFilesEx(const FileMask, ExclusionMask : string);
    procedure FreshenTaggedItems;
    procedure Load; virtual;
    procedure Move(aItem : TAbArchiveItem; const NewStoredPath : string);
      virtual;
    procedure Replace(aItem : TAbArchiveItem);
    procedure Save;
      virtual;
    procedure TagItems(const FileMask : string);
    procedure TestTaggedItems;
    procedure UnTagItems(const FileMask : string);


    procedure DoDeflateProgress(aPercentDone : integer);
      virtual;
    procedure DoInflateProgress(aPercentDone : integer);
      virtual;
    procedure DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean); virtual;
  public {properties}
    property OnProgress : TAbProgressEvent
      read FOnProgress write FOnProgress;
    property ArchiveName : string
      read FArchiveName;
    property AutoSave : Boolean
      read FAutoSave
      write FAutoSave;
    property BaseDirectory : string
      read FBaseDirectory
      write SetBaseDirectory;
    property Count : Integer
      read GetItemCount;
    property DOSMode : Boolean
      read FDOSMode
      write FDOSMode;
    property ExtractOptions : TAbExtractOptions
      read FExtractOptions
      write FExtractOptions;
    property IsDirty : Boolean
      read FIsDirty
      write FIsDirty;
    property ItemList : TAbArchiveList
      read FItemList;
    property LogFile : string
      read FLogFile
      write SetLogFile;
    property Logging : Boolean
      read FLogging
      write SetLogging;
    property Mode : Word
      read FMode;
    property Spanned : Boolean
      read FSpanned;
    property SpanningThreshold : Int64
      read  GetSpanningThreshold
      write SetSpanningThreshold;
    property Status : TAbArchiveStatus
      read FStatus;
    property StoreOptions : TAbStoreOptions
      read FStoreOptions
      write FStoreOptions;
    property TempDirectory : string
      read FTempDir
      write FTempDir;

  public {events}
    property OnProcessItemFailure : TAbArchiveItemFailureEvent
      read FOnProcessItemFailure
      write FOnProcessItemFailure;
    property OnArchiveProgress : TAbArchiveProgressEvent
      read FOnArchiveProgress
      write FOnArchiveProgress;
    property OnArchiveSaveProgress : TAbArchiveProgressEvent           {!!.04}
      read FOnArchiveSaveProgress                                      {!!.04}
      write FOnArchiveSaveProgress;                                    {!!.04}
    property OnArchiveItemProgress : TAbArchiveItemProgressEvent
      read FOnArchiveItemProgress
      write FOnArchiveItemProgress;
    property OnConfirmProcessItem : TAbArchiveItemConfirmEvent
      read FOnConfirmProcessItem
      write FOnConfirmProcessItem;
    property OnConfirmOverwrite : TAbConfirmOverwriteEvent
      read FOnConfirmOverwrite
      write FOnConfirmOverwrite;
    property OnConfirmSave : TAbArchiveConfirmEvent
      read FOnConfirmSave
      write FOnConfirmSave;
    property OnLoad : TAbArchiveEvent
      read FOnLoad
      write FOnLoad;
    property OnRequestImage : TAbRequestImageEvent
      read FOnRequestImage
      write FOnRequestImage;
    property OnSave : TAbArchiveEvent
      read FOnSave
      write FOnSave;
  end;


{ ===== TAbExtraField ======================================================= }
type
  PAbExtraSubField = ^TAbExtraSubField;
  TAbExtraSubField = packed record
    ID : Word;
    Len : Word;
    Data : record end;
  end;

  TAbExtraField = class
  private {fields}
    FBuffer : TByteDynArray;
  private {methods}
    procedure DeleteField(aSubField : PAbExtraSubField);
    function FindField(aID : Word; out aSubField : PAbExtraSubField) : Boolean;
    function FindNext(var aCurField : PAbExtraSubField) : Boolean;
    function GetCount : Integer;
    function GetID(aIndex : Integer): Word;
    procedure SetBuffer(const aValue : TByteDynArray);
  protected {methods}
    procedure Changed; virtual;
  public {methods}
    procedure Clear;
    procedure Delete(aID : Word);
    function Get(aID : Word; out aData : Pointer; out aDataSize : Word) : Boolean;
    procedure LoadFromStream(aStream : TStream; aSize : Word);
    procedure Put(aID : Word; const aData; aDataSize : Word);
  public {properties}
    property Count : Integer
      read GetCount;
    property Buffer : TByteDynArray
      read FBuffer
      write SetBuffer;
    property IDs[aIndex : Integer]: Word
      read GetID;
  end;


const
  AbDefAutoSave = False;
  AbDefExtractOptions = [eoCreateDirs];
  AbDefStoreOptions = [soStripDrive, soRemoveDots];
  AbBufferSize = 32768;
  AbLastDisk = -1;
  AbLastImage = -1;

implementation

{.$R ABRES.R32}

uses
  RTLConsts,
  AbExcept,
  AbDfBase,
  AbConst,
  AbResString,
  uClassesEx;


{ TAbArchiveItem implementation ============================================ }
{ TAbArchiveItem }
constructor TAbArchiveItem.Create;
begin
  inherited Create;
  FCompressedSize := 0;
  FUncompressedSize := 0;
  FFileName := '';
  FAction := aaNone;
  FLastModFileTime := 0;
  FLastModFileDate := 0;
  FCRC32 := 0;
end;
{ -------------------------------------------------------------------------- }
destructor TAbArchiveItem.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetCompressedSize : Int64;
begin
  Result := FCompressedSize;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetCRC32 : LongInt;
begin
  Result := FCRC32;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetDiskPath : string;
begin
  Result := ExtractFilePath(DiskFileName);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetExternalFileAttributes : LongWord;
begin
  Result := FExternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetFileName : string;
begin
  Result := FFileName;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetIsDirectory: Boolean;
begin
  Result := FIsDirectory;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetIsEncrypted : Boolean;
begin
  Result := FIsEncrypted;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetLastModDateTime : TDateTime;
var
  FileTime : Longint;
begin
  LongRec(FileTime).Hi := LastModFileDate;
  LongRec(FileTime).Lo := LastModFileTime;

  Result := FileDateToDateTime(FileTime);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetLastModFileTime : Word;
begin
  Result := FLastModFileTime;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetLastModFileDate : Word;
begin
  Result := FLastModFileDate;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetSystemSpecificAttributes : LongWord;
begin
  Result := GetExternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetSystemSpecificLastModFileTime : Longint;
begin
  LongRec(Result).Hi := LastModFileDate;
  LongRec(Result).Lo := LastModFileTime;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetStoredPath : string;
begin
  Result := ExtractFilePath(DiskFileName);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.GetUnCompressedSize : Int64;
begin
  Result := FUnCompressedSize;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesDiskName(const FileMask : string) : Boolean;
var
  DiskName, Mask : string;
begin
  DiskName := DiskFileName;
  AbUnfixName(DiskName);
  Mask := FileMask;
  AbUnfixName(Mask);
  Result := AbFileMatch(DiskName, Mask, False);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesStoredName(const FileMask : string;
                                          Recursive : Boolean) : Boolean;
var
  Value : string;
  Drive, Dir, Name : string;
begin
  Value := FileMask;
  AbUnfixName(Value);
  AbParseFileName(Value, Drive, Dir, Name);
  Value := Dir + Name;
  Name := FileName;
  AbUnfixName(Name);
  if IsDirectory then
    Name := ExcludeTrailingPathDelimiter(Name);
  Result := AbFileMatch(Name, Value, Recursive);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesStoredNameEx(const FileMask : string;
                                            Recursive : Boolean) : Boolean;
var
  Position: Integer;
  Path: String;
begin
  Result := True;

  Position := 1;
  while True do
  begin
    Path := AbExtractEntry(FileMask, Position);
    if Path = '' then Break;
    if MatchesStoredName(Trim(Path), Recursive) then Exit;
  end;

  Result := False;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesPath(const Path : String; Recursive : Boolean = False) : Boolean;
var
  Value : string;
  Drive, Dir, Name : string;
begin
  Value := Path;
  if (Value <> '') and (RightStr(Value, 1) <> AbPathDelim) then
    Value := Value + AbPathDelim;
  AbUnfixName(Value);
  AbParseFileName(Path, Drive, Dir, Name);
  Value := Dir + Name;
  Name := FileName;
  AbUnfixName(Name);
  Result := AbDirMatch(Name, Value, Recursive);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveItem.MatchesPathEx(const Paths : String; Recursive : Boolean = False) : Boolean;
var
  Position: Integer;
  Path: String;
begin
  Result := True;

  Position := 1;
  while True do
  begin
    Path := AbExtractEntry(Paths, Position);
    if Path = '' then Break;
    if MatchesPath(Path, Recursive) then Exit;
  end;

  Result := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetCompressedSize(const Value : Int64);
begin
  FCompressedSize := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetCRC32(const Value : LongInt);
begin
  FCRC32 := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetExternalFileAttributes( Value : LongWord );
begin
  FExternalFileAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetFileName(const Value : string);
begin
  FFileName := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetIsDirectory(const Value: Boolean);
begin
  FIsDirectory := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetIsEncrypted(Value : Boolean);
begin
  FIsEncrypted := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetLastModDateTime(const Value : TDateTime);
var
  FileTime : Integer;
begin
  FileTime := DateTimeToFileDate(Value);

  LastModFileTime := LongRec(FileTime).Lo;
  LastModFileDate := LongRec(FileTime).Hi;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetLastModFileDate(const Value : Word);
begin
  FLastModFileDate := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetLastModFileTime(const Value : Word);
begin
  FLastModFileTime := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetSystemSpecificAttributes(Value : LongWord);
begin
  SetExternalFileAttributes(Value);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetSystemSpecificLastModFileTime(const Value: Longint);
begin
  LastModFileDate := LongRec(Value).Hi;
  LastModFileTime := LongRec(Value).Lo;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveItem.SetUnCompressedSize(const Value : Int64);
begin
  if Value > High(LongWord) then
    raise EAbFileTooLarge.Create;
  FUnCompressedSize := Value;
end;
{ -------------------------------------------------------------------------- }

{ TAbArchiveList implementation ============================================ }

{ TAbArchiveList }
constructor TAbArchiveList.Create(AOwnsItems: Boolean);
begin
  inherited Create;
  FList := TList.Create;
  FOwnsItems := AOwnsItems;
end;
{ -------------------------------------------------------------------------- }
destructor TAbArchiveList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveList.Add(Item : Pointer) : Integer;
var
  H : LongInt;
begin
  if FOwnsItems then begin
    H := GenerateHash(TAbArchiveItem(Item).FileName);
    TAbArchiveItem(Item).NextItem := HashTable[H];
    HashTable[H] := TAbArchiveItem(Item);
  end;
  Result := FList.Add(Item);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.Clear;
var
  i : Integer;
begin
  if FOwnsItems then
    for i := 0 to Count - 1 do
      TObject(FList[i]).Free;
  FList.Clear;
  FillChar(HashTable, SizeOf(HashTable), #0);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.Delete(Index: Integer);
var
  Look : TAbArchiveItem;
  Last : Pointer;
  FN : string;
begin
  if FOwnsItems then begin
    FN := TAbArchiveItem(FList[Index]).FileName;
    Last := @HashTable[GenerateHash(FN)];
    Look := TAbArchiveItem(Last^);
    while Look <> nil do begin
      if CompareText(Look.FileName, FN) = 0 then begin
        Move(Look.NextItem, Last^, 4);
        Break;
      end;
      Last := @Look.NextItem;
      Look := TAbArchiveItem(Last^);
    end;
    TObject(FList[Index]).Free;
  end;
  FList.Delete(Index);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveList.Find(const FN : string) : Integer;
var
  Look : TAbArchiveItem;
  I : Integer;
begin
  if FOwnsItems then begin
    Look := HashTable[GenerateHash(FN)];
    while Look <> nil do begin
      if CompareText(Look.FileName, FN) = 0 then begin
        Result := FList.IndexOf(Look);
        Exit;
      end;
      Look := Look.NextItem;
    end;
  end
  else begin
    for I := 0 to FList.Count - 1 do
      if CompareText(Items[I].FileName, FN) = 0 then begin
        Result := I;
        Exit;
      end;
  end;
  Result := -1;
end;
{ -------------------------------------------------------------------------- }
{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
function TAbArchiveList.GenerateHash(const S : string) : LongInt;
var
  G : LongInt;
  I : Integer;
  U : string;
begin
  Result := 0;
  U := AnsiUpperCase(S);
  for I := 1 to Length(U) do begin
    Result := (Result shl 4) + Ord(U[I]);
    G := LongInt(Result and $F0000000);
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
  Result := Result mod 1021;
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}
{ -------------------------------------------------------------------------- }
function TAbArchiveList.Get(Index : Integer): TAbArchiveItem;
begin
  Result := TAbArchiveItem(FList[Index]);
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveList.GetCount : Integer;
begin
  Result := FList.Count;
end;
{ -------------------------------------------------------------------------- }
function TAbArchiveList.IsActiveDupe(const FN : string) : Boolean;
var
  Look : TAbArchiveItem;
  I : Integer;
begin
  if FOwnsItems then begin
    Look := HashTable[GenerateHash(FN)];
    while Look <> nil do begin
      if (CompareText(Look.FileName, FN) = 0) and
         (Look.Action <> aaDelete) then begin
        Result := True;
        Exit;
      end;
      Look := Look.NextItem;
    end;
  end
  else begin
    for I := 0 to Count - 1 do
      if (CompareText(Items[I].FileName, FN) = 0) and
         (Items[I].Action <> aaDelete) then begin
        Result := True;
        Exit;
      end;
  end;
  Result := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchiveList.Put(Index : Integer; Item : TAbArchiveItem);
var
  H : LongInt;
  Look : TAbArchiveItem;
  Last : Pointer;
  FN : string;
begin
  if FOwnsItems then begin
    FN := TAbArchiveItem(FList[Index]).FileName;
    Last := @HashTable[GenerateHash(FN)];
    Look := TAbArchiveItem(Last^);
    { Delete old index }
    while Look <> nil do begin
      if CompareText(Look.FileName, FN) = 0 then begin
        Move(Look.NextItem, Last^, 4);
        Break;
      end;
      Last := @Look.NextItem;
      Look := TAbArchiveItem(Last^);
    end;
    { Free old instance }
    TObject(FList[Index]).Free;
    { Add new index }
    H := GenerateHash(TAbArchiveItem(Item).FileName);
    TAbArchiveItem(Item).NextItem := HashTable[H];
    HashTable[H] := TAbArchiveItem(Item);
  end;
  { Replace pointer }
  FList[Index] := Item;
end;


{ TAbArchive implementation ================================================ }
{ TAbArchive }
constructor TAbArchive.CreateInit;
begin
  inherited Create;
  FIsDirty := False;
  FAutoSave := False;
  FItemList := TAbArchiveList.Create(True);
  StoreOptions := [];
  ExtractOptions := [];
  FStatus := asIdle;
  FOnProgress := DoProgress;
  BaseDirectory := ExtractFilePath(ParamStr(0));
end;
{ -------------------------------------------------------------------------- }
constructor TAbArchive.Create(const FileName : string; Mode : Word);
  {create an archive by opening a filestream on filename with the given mode}
begin
  FOwnsStream := True;
  CreateFromStream(TFileStreamEx.Create(FileName, Mode), FileName);
  FMode := Mode;
end;
{ -------------------------------------------------------------------------- }
constructor TAbArchive.CreateFromStream(aStream : TStream; const aArchiveName : string);
  {create an archive based on an existing stream}
begin
  CreateInit;
  FArchiveName := aArchiveName;
  FStream := aStream;
end;
{ -------------------------------------------------------------------------- }
destructor TAbArchive.Destroy;
begin
  FItemList.Free;
  FItemList := nil;
  if FOwnsStream then begin
   if Assigned(FStream) then {!!.05 avoid A/V if Nil (Only occurs if exception is raised)}
     FStream.Free;
    FStream := nil;
  end;
  if Assigned(FLogStream) then begin
    FLogStream.Free;
    FLogStream := nil;
  end;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Add(aItem : TAbArchiveItem);
var
  Confirm, ItemAdded : Boolean;
begin
  ItemAdded := False;
  try
    CheckValid;
    if FItemList.IsActiveDupe(aItem.FileName) then begin
      if (soFreshen in StoreOptions) then
        Freshen(aItem)
      else if (soReplace in StoreOptions) then
        Replace(aItem)
      else
        DoProcessItemFailure(aItem, ptAdd, ecAbbrevia, AbDuplicateName);
      Exit;
    end;
    DoConfirmProcessItem(aItem, ptAdd, Confirm);
    if not Confirm then
      Exit;
    aItem.Action := aaAdd;
    FItemList.Add(aItem);
    ItemAdded := True;
    FIsDirty := True;
    if AutoSave then
      Save;
  finally
    if not ItemAdded then
      aItem.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddEntry(const Path : String; const ArchiveDirectory : String);
var
  Item : TAbArchiveItem;
  FullSourceFileName, FullArchiveFileName : String;
begin
  MakeFullNames(Path, ArchiveDirectory, FullSourceFileName, FullArchiveFileName);

  if (FullSourceFileName <> FArchiveName) then begin
    Item := CreateItem(Path, ArchiveDirectory);
    Add(Item);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddEntries(const Paths : String; const ArchiveDirectory : String);
var
  Position: Integer;
  Path: String;
begin
  Position := 1;
  while True do
  begin
    Path := AbExtractEntry(Paths, Position);
    if Path = '' then Break;
    AddEntry(Path, ArchiveDirectory);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddFiles(const FileMask : string; SearchAttr : Integer);
  {Add files to the archive where the disk filespec matches}
begin
  AddFilesEx(FileMask, '', SearchAttr);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddFilesEx(const FileMask, ExclusionMask : string;
  SearchAttr : Integer);
  {Add files matching Filemask except those matching ExclusionMask}
var
  PathType : TAbPathType;
  SaveDir : string;
  Mask : string;
  MaskF : string;

  procedure CreateItems(Recursing : Boolean);
  var
    i : Integer;
    Files : TStrings;
    FilterList : TStringList;
  begin
    FilterList := TStringList.Create;
    try
      if (MaskF <> '') then
        // Exclude directories here, mask should only exclude files.
        AbFindFilesEx(MaskF, SearchAttr and not faDirectory, FilterList, Recursing);

        Files := TStringList.Create;
        try

          AbFindFilesEx(Mask, SearchAttr, Files, Recursing);
          if (Files.Count > 0) then begin
            for i := 0 to pred(Files.Count) do
              if FilterList.IndexOf(Files[i]) < 0 then
                AddEntry(Files[i], Files[i]);
            FIsDirty := true;
          end;
        finally
          Files.Free;
        end;

    finally
      FilterList.Free;
    end;
  end;

begin
  CheckValid;
  PathType := AbGetPathType(FileMask);

  Mask := FileMask;
  AbUnfixName(Mask);
  MaskF := ExclusionMask;
  AbUnfixName(MaskF);

  case PathType of
    ptNone, ptRelative :
      begin
        GetDir(0, SaveDir);
        if BaseDirectory <> '' then
          ChDir(BaseDirectory);
        try
          CreateItems(soRecurse in StoreOptions);
        finally
          if BaseDirectory <> '' then
            ChDir(SaveDir);
        end;
      end;
    ptAbsolute :
      begin
        CreateItems(soRecurse in StoreOptions);
      end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.AddFromStream(const NewName : string; aStream : TStream);
  {Add an item to the archive directly from a TStream descendant}
var
  Confirm : Boolean;
  Item : TAbArchiveItem;
  PT : TAbProcessType;                                               
begin
  Item := CreateItem('', NewName);
  CheckValid;

  PT := ptAdd;
  if FItemList.IsActiveDupe(NewName) then begin
    if ((soFreshen in StoreOptions) or (soReplace in StoreOptions)) then begin
      Item.Free;
      Item := FItemList[FItemList.Find(NewName)];
      PT := ptReplace;
    end else begin
      DoProcessItemFailure(Item, ptAdd, ecAbbrevia, AbDuplicateName);
      Item.Free;
      Exit;
    end;
  end;
  DoConfirmProcessItem(Item, PT, Confirm);

  if not Confirm then
    Exit;

  FInStream := aStream;
  Item.Action := aaStreamAdd;
  if (PT = ptAdd) then
    FItemList.Add(Item);
  FIsDirty := True;
  Save;
  FInStream := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.CheckValid;
begin
  if Status = asInvalid then
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ClearTags;
  {Clear all tags from the archive}
var
  i : Integer;
begin
  if Count > 0 then
    for i := 0 to pred(Count) do
      TAbArchiveItem(FItemList[i]).Tagged := False;
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.ConfirmPath(Item : TAbArchiveItem; const NewName : string;
  out UseName : string) : Boolean;
var
  Path : string;
begin
  if (NewName = '') then begin
    UseName := Item.FileName;
    if Item.IsDirectory then
      UseName := ExcludeTrailingPathDelimiter(UseName);
    AbUnfixName(UseName);
    if (not (eoRestorePath in ExtractOptions)) then
      UseName := ExtractFileName(UseName);
  end
  else
    UseName := NewName;
  if (AbGetPathType(UseName) <> ptAbsolute) then
    UseName := AbAddBackSlash(BaseDirectory) + UseName;

  Path := ExtractFileDir(UseName);
  if (Path <> '') and not AbDirectoryExists(Path) then
    if (eoCreateDirs in ExtractOptions) then
      AbCreateDirectory(Path)
    else
      raise EAbNoSuchDirectory.Create;

  Result := True;
  if FileExists(UseName) then
    DoConfirmOverwrite(UseName, Result);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Delete(aItem : TAbArchiveItem);
  {delete an item from the archive}
var
  Index : Integer;
begin
  CheckValid;
  Index := FindItem(aItem);
  if Index <> -1 then
    DeleteAt(Index);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteAt(Index : Integer);
  {delete the item at the index from the archive}
var
  Confirm : Boolean;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);
  DoConfirmProcessItem(FItemList[Index], ptDelete, Confirm);
  if not Confirm then
    Exit;

  TAbArchiveItem(FItemList[Index]).Action := aaDelete;
  FIsDirty := True;
  if AutoSave then
    Save;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteFiles(const FileMask : string; Recursive : Boolean);
  {delete all files from the archive that match the file mask}
  {if Recursive=False only files from matching directory are deleted}
begin
  DeleteFilesEx(FileMask, '', Recursive);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteFilesEx(const FileMask, ExclusionMask : string;
                                   Recursive : Boolean);
  {Delete files matching Filemask except those matching ExclusionMask}
  {if Recursive=False only files from matching directory are deleted}
var
  i : Integer;
begin
  CheckValid;
  if Count > 0 then begin
    for i := pred(Count) downto 0 do begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesStoredNameEx(FileMask, Recursive) then
          if not MatchesStoredNameEx(ExclusionMask, Recursive) then
            DeleteAt(i);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteDirectoriesRecursively(const Paths : string);
  {Delete directory entry and all file and directory entries matching the same path recursively}
var
  i : Integer;
begin
  CheckValid;
  if Count > 0 then begin
    for i := pred(Count) downto 0 do begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesPathEx(Paths, True) then
          DeleteAt(i);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DeleteTaggedItems;
  {delete all tagged items from the archive}
var
  i : Integer;
begin
  CheckValid;
  if Count > 0 then begin
    for i := pred(Count) downto 0 do begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
          DeleteAt(i);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoProcessItemFailure(Item : TAbArchiveItem;
  ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
  ErrorCode : Integer);
begin
  if Assigned(FOnProcessItemFailure) then
    FOnProcessItemFailure(Self, Item, ProcessType, ErrorClass, ErrorCode);
end;
{ -------------------------------------------------------------------------- }
{!!.04 - Added }
procedure TAbArchive.DoArchiveSaveProgress(Progress : Byte; var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FOnArchiveSaveProgress) then
    FOnArchiveSaveProgress(Self, Progress, Abort);
end;
{!!.04 - Added end }
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoArchiveProgress(Progress : Byte; var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FOnArchiveProgress) then
    FOnArchiveProgress(Self, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoArchiveItemProgress(Item : TAbArchiveItem;
  Progress : Byte; var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FOnArchiveItemProgress) then
    FOnArchiveItemProgress(Self, Item, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoConfirmOverwrite(var FileName : string; var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmOverwrite) then
    FOnConfirmOverwrite(FileName, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoConfirmProcessItem(Item : TAbArchiveItem;
  const ProcessType : TAbProcessType; var Confirm : Boolean);
const
  ProcessTypeToLogType : array[TAbProcessType] of TAbLogType =
    (ltAdd, ltDelete, ltExtract, ltFreshen, ltMove, ltReplace, ltFoundUnhandled);
begin
  Confirm := True;
  if Assigned(FOnConfirmProcessItem) then
    FOnConfirmProcessItem(Self, Item, ProcessType, Confirm);
  if (Confirm and FLogging) then
    MakeLogEntry(Item.Filename, ProcessTypeToLogType[ProcessType]);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoConfirmSave(var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmSave) then
    FOnConfirmSave(Self, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoDeflateProgress(aPercentDone: integer);
var
  Abort : Boolean;
begin
  DoProgress(aPercentDone, Abort);
  if Abort then
    raise EAbAbortProgress.Create(AbUserAbortS);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoInflateProgress(aPercentDone: integer);
var
  Abort : Boolean;
begin
  DoProgress(aPercentDone, Abort);
  if Abort then
    raise EAbAbortProgress.Create(AbUserAbortS);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoLoad;
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoProgress(Progress : Byte; var Abort : Boolean);
begin
  Abort := False;                                                      
  DoArchiveItemProgress(FCurrentItem, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoSave;
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Extract(aItem : TAbArchiveItem; const NewName : string);
  {extract an item from the archive}
var
  Index : Integer;
begin
  CheckValid;
  Index := FindItem(aItem);
  if Index <> -1 then
    ExtractAt(Index, NewName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractAt(Index : Integer; const NewName : string);
  {extract an item from the archive at Index}
var
  Confirm : Boolean;
  ErrorClass : TAbErrorClass;
  ErrorCode : Integer;
  UseName : string;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);
  DoConfirmProcessItem(FItemList[Index], ptExtract, Confirm);
  if not Confirm then
    Exit;

  if not ConfirmPath(FItemList[Index], NewName, UseName) then
    Exit;

  try
    FCurrentItem := FItemList[Index];
    ExtractItemAt(Index, UseName);
  except
    on E : Exception do begin
      AbConvertException(E, ErrorClass, ErrorCode);
      DoProcessItemFailure(FItemList[Index], ptExtract, ErrorClass, ErrorCode);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractToStream(const aFileName : string;
                                     aStream : TStream);
  {extract an item from the archive at Index directly to a stream}
var
  Confirm : Boolean;
  ErrorClass : TAbErrorClass;
  ErrorCode : Integer;
  Index : Integer;
begin
  CheckValid;
  Index := FindFile(aFileName);
  if (Index = -1) then
    Exit;

  SaveIfNeeded(FItemList[Index]);

  DoConfirmProcessItem(FItemList[Index], ptExtract, Confirm);
  if not Confirm then
    Exit;
  FCurrentItem := FItemList[Index];
  try
    ExtractItemToStreamAt(Index, aStream);
  except
    on E : Exception do begin
      AbConvertException(E, ErrorClass, ErrorCode);
      DoProcessItemFailure(FItemList[Index], ptExtract, ErrorClass, ErrorCode);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractFiles(const FileMask : string);
  {extract all files from the archive that match the mask}
begin
  ExtractFilesEx(FileMask, '');
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractFilesEx(const FileMask, ExclusionMask : string);
  {Extract files matching Filemask except those matching ExclusionMask}
var
  i : Integer;
  Abort : Boolean;
begin
  CheckValid;
  if Count > 0 then begin
    for i := 0 to pred(Count) do begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesStoredNameEx(FileMask) and
           not MatchesStoredNameEx(ExclusionMask) and
           ((eoCreateDirs in ExtractOptions) or not IsDirectory) then
          ExtractAt(i, '');
      DoArchiveProgress(AbPercentage(succ(i), Count), Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;
    DoArchiveProgress(100, Abort);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ExtractTaggedItems;
  {extract all tagged items from the archive}
var
  i : Integer;
  Abort : Boolean;
begin
  CheckValid;
  if Count > 0 then begin
    for i := 0 to pred(Count) do begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
          ExtractAt(i, '');
      DoArchiveProgress(AbPercentage(succ(i), Count), Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;
    DoArchiveProgress(100, Abort);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.TestTaggedItems;
  {test all tagged items in the archive}
var
  i : Integer;
  Abort : Boolean;                                                       
begin
  CheckValid;
  if Count > 0 then begin
    for i := 0 to pred(Count) do begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then begin
          FCurrentItem := FItemList[i];                              
          TestItemAt(i);
        end;
      DoArchiveProgress(AbPercentage(succ(i), Count), Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;
    DoArchiveProgress(100, Abort);                                       
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.FindFile(const aFileName : string): Integer;
  {find the index of the specified file}
begin
  Result := FItemList.Find(aFileName);
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.FindItem(aItem : TAbArchiveItem): Integer;
  {find the index of the specified item}
begin
  Result := FItemList.Find(aItem.FileName);
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.FixName(const Value : string) : string;
var
  lValue: string;
begin
  lValue := Value;
  {$IFDEF MSWINDOWS}
  if DOSMode then begin
    {Add the base directory to the filename before converting }
    {the file spec to the short filespec format. }
    if BaseDirectory <> '' then begin
      {Does the filename contain a drive or a leading backslash? }
      if not ((Pos(':', lValue) = 2) or (Pos(AbPathDelim, lValue) = 1)) then
        {If not, add the BaseDirectory to the filename.}
        lValue := AbAddBackSlash(BaseDirectory) + lValue;                {!!.04}
    end;
    lValue := AbGetShortFileSpec(lValue);
  end;
  {$ENDIF}

  {strip drive stuff}
  if soStripDrive in StoreOptions then
    AbStripDrive(lValue);

  {check for a leading backslash}
  if lValue[1] = AbPathDelim then
    System.Delete(lValue, 1, 1);

  if soStripPath in StoreOptions then begin
    lValue := ExtractFileName(lValue);
  end;

  if soRemoveDots in StoreOptions then
    AbStripDots(lValue);

  Result := lValue;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Freshen(aItem : TAbArchiveItem);
  {freshen the item}
var
  Index : Integer;
//  temp : String;
begin
  CheckValid;
  Index := FindItem(aItem);

  if Index <> -1 then
   begin
    // [ 892830 ] freshing file it doesn't set the correct Item.DiskFileName
    if AbGetPathType(aItem.DiskFileName) = ptAbsolute then   {!!.05}
     begin
       FItemList[Index].DiskFileName := aItem.DiskFileName;  {!!.05}
//       FItemList[Index].FileName     := aItem.DiskFileName;  {!!.05}
     end;
    FreshenAt(Index);
   end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenAt(Index : Integer);
  {freshen item at index}
var
  Confirm : Boolean;
  FR : Boolean;
  ErrorClass : TAbErrorClass;
  ErrorCode : Integer;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);

  GetFreshenTarget(FItemList[Index]);
  FR := False;
  try
    FR := FreshenRequired(FItemList[Index]);
  except
    on E : Exception do begin
      AbConvertException(E, ErrorClass, ErrorCode);
      DoProcessItemFailure(FItemList[Index], ptFreshen, ErrorClass, ErrorCode);
    end;
  end;
  if not FR then
    Exit;
  DoConfirmProcessItem(FItemList[Index], ptFreshen, Confirm);
  if not Confirm then
    Exit;

  TAbArchiveItem(FItemList[Index]).Action := aaFreshen;
  FIsDirty := True;
  if AutoSave then
    Save;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenFiles(const FileMask : string);
  {freshen all items that match the file mask}
begin
  FreshenFilesEx(FileMask, '');
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenFilesEx(const FileMask, ExclusionMask : string);
  {freshen all items that match the file mask}
var
  i : Integer;
begin
  CheckValid;
  if Count > 0 then begin
    for i := pred(Count) downto 0 do begin
      with TAbArchiveItem(FItemList[i]) do
        if MatchesStoredNameEx(FileMask) then
          if not MatchesStoredNameEx(ExclusionMask) then                 
            FreshenAt(i);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.FreshenRequired(Item : TAbArchiveItem) : Boolean;
var
  FS : TFileStream;
  DateTime : LongInt;
  FileTime : Word;
  FileDate : Word;
  Matched : Boolean;
  SaveDir : string;
begin
  GetDir(0, SaveDir);
  if BaseDirectory <> '' then
    ChDir(BaseDirectory);
  try
    FS := TFileStream.Create(Item.DiskFileName,
                              fmOpenRead or fmShareDenyWrite);
    try
      DateTime := FileGetDate(FS.Handle);
      FileTime := LongRec(DateTime).Lo;
      FileDate := LongRec(DateTime).Hi;
      Matched := (Item.LastModFileDate = FileDate) and
                 (Item.LastModFileTime = FileTime) and
                 (Item.UncompressedSize = FS.Size);
      Result := not Matched;
    finally
      FS.Free;
    end;
  finally
    if BaseDirectory <> '' then
      ChDir(SaveDir);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.FreshenTaggedItems;
  {freshen all tagged items}
var
  i : Integer;
begin
  CheckValid;
  if Count > 0 then begin
    for i := pred(Count) downto 0 do begin
      with TAbArchiveItem(FItemList[i]) do
        if Tagged then
          FreshenAt(i);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.GetFreshenTarget(Item : TAbArchiveItem);
var
  PathType : TAbPathType;
  Files : TStrings;
  SaveDir : string;
  DName : string;
begin
  PathType := AbGetPathType(Item.DiskFileName);
  if (soRecurse in StoreOptions) and (PathType = ptNone) then begin
    GetDir(0, SaveDir);
    if BaseDirectory <> '' then
      ChDir(BaseDirectory);
    try
      Files := TStringList.Create;
      try
        // even if archive supports empty folder we don't have to
        // freshen it because there is no data, although, the timestamp
        // can be update since the folder was added
        AbFindFiles(Item.FileName, faAnyFile and not faDirectory, Files,
                     True);
        if Files.Count > 0 then begin
          DName := AbAddBackSlash(BaseDirectory) + Files[0];           {!!.04}
          AbUnfixName(DName);
          Item.DiskFileName := DName;
        end
        else
          Item.DiskFileName := '';
      finally
        Files.Free;
      end;
    finally
      if BaseDirectory <> '' then
        ChDir(SaveDir);
    end;
  end
  else begin
    if (BaseDirectory <> '') then
      DName := AbAddBackSlash(BaseDirectory) + Item.FileName           {!!.04}
    else
      if AbGetPathType(Item.DiskFileName) = ptAbsolute then
        DName := Item.DiskFileName
      else
        DName := Item.FileName;
    AbUnfixName(DName);
    Item.DiskFileName := DName;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.GetSpanningThreshold : Int64;
begin
  Result := FSpanningThreshold;
end;
{ -------------------------------------------------------------------------- }
function TAbArchive.GetItemCount : Integer;
begin
  if Assigned(FItemList) then
    Result := FItemList.Count
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Load;
  {load the archive}
begin
  try
    LoadArchive;
    FStatus := asIdle;
  finally
    DoLoad;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.MakeLogEntry(const FN: string; LT : TAbLogType);
const
  LogTypeRes : array[TAbLogType] of string =
    (AbLtAddS, AbLtDeleteS, AbLtExtractS, AbLtFreshenS, AbLtMoveS, AbLtReplaceS,
     AbLtStartS, AbUnhandledEntityS);
var
  Buf : string;
begin
  if Assigned(FLogStream) then begin
    Buf := FN + LogTypeRes[LT] + DateTimeToStr(Now) + sLineBreak;
    FLogStream.Write(Buf[1], Length(Buf) * SizeOf(Char));
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.MakeFullNames(const SourceFileName: String;
                                   const ArchiveDirectory: String;
                                   out   FullSourceFileName: String;
                                   out   FullArchiveFileName: String);
var
  PathType : TAbPathType;
  RelativeSourceFileName: String;
begin
  PathType := AbGetPathType(SourceFileName);
  case PathType of
    ptNone, ptRelative :
      begin
        if FBaseDirectory <> '' then
          FullSourceFileName := AbAddBackSlash(FBaseDirectory) + SourceFileName
        else
          FullSourceFileName := SourceFileName;

        RelativeSourceFileName := SourceFileName;
      end;
    ptAbsolute :
      begin
        FullSourceFileName := SourceFileName;

        if FBaseDirectory <> '' then
          RelativeSourceFileName := ExtractRelativepath(AbAddBackSlash(FBaseDirectory),
                                                        SourceFileName)
        else
          RelativeSourceFileName := ExtractFileName(SourceFileName);
      end;
  end;

  if ArchiveDirectory <> '' then
    FullArchiveFileName := AbAddBackSlash(ArchiveDirectory) + RelativeSourceFileName
  else
    FullArchiveFileName := RelativeSourceFileName;

  FullArchiveFileName := FixName(FullArchiveFileName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Move(aItem : TAbArchiveItem; const NewStoredPath : string);
var
  Confirm : Boolean;
  Found : Boolean;
  i : Integer;
  FixedPath: string;
begin
  CheckValid;
  FixedPath := FixName(NewStoredPath);
  Found := False;
  if Count > 0 then
    for i := 0 to pred(Count) do
      if (ItemList[i] <> aItem) and SameText(FixedPath, ItemList[i].FileName) and
         (ItemList[i].Action <> aaDelete) then begin
        Found := True;
        Break;
      end;
  if Found then begin
    DoProcessItemFailure(aItem, ptMove, ecAbbrevia, AbDuplicateName);
    {even if something gets done in the AddItemFailure, we don't
     want to continue...}
    Exit;
  end;

  SaveIfNeeded(aItem);

  DoConfirmProcessItem(aItem, ptMove, Confirm);
  if not Confirm then
    Exit;

  with aItem do begin
    FileName := FixedPath;
    Action := aaMove;
  end;
  FIsDirty := True;
  if AutoSave then
    Save;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Replace(aItem : TAbArchiveItem);
  {replace the item}
var
  Index : Integer;
begin
  CheckValid;
  Index := FindItem(aItem);
  if Index <> -1 then
    ReplaceAt(Index);
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.ReplaceAt(Index : Integer);
  {replace item at Index}
var
  Confirm : Boolean;
begin
  CheckValid;
  SaveIfNeeded(FItemList[Index]);

  GetFreshenTarget(FItemList[Index]);
  DoConfirmProcessItem(FItemList[Index], ptReplace, Confirm);
  if not Confirm then
    Exit;

  TAbArchiveItem(FItemList[Index]).Action := aaReplace;
  FIsDirty := True;
  if AutoSave then
    Save;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.Save;
  {save the archive}
var
  Confirm : Boolean;
begin
  if FIsDirty and (Status <> asInvalid) then
  begin
    DoConfirmSave(Confirm);
    if not Confirm then
      Exit;

    SaveArchive;
    FIsDirty := False;
    DoSave;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.SaveIfNeeded(aItem : TAbArchiveItem);
begin
  if (aItem.Action <> aaNone) then
    Save;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetBaseDirectory(Value : string);
begin
  if (Value <> '') then
    if Value[Length(Value)] = AbPathDelim then
      if (Length(Value) > 1) and (Value[Length(Value) - 1] <> ':') then
        System.Delete(Value, Length(Value), 1);
  if (Length(Value) = 0) or AbDirectoryExists(Value) then
    FBaseDirectory := Value
  else
    raise EAbNoSuchDirectory.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetSpanningThreshold( Value : Int64 );
begin
  FSpanningThreshold := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetLogFile(const Value : string);
begin
  FLogFile := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.SetLogging(Value : Boolean);
begin
  FLogging := Value;
  if Assigned(FLogStream) then begin
    FLogStream.Free;
    FLogStream := nil;
  end;
  if FLogging and (FLogFile <> '') then begin
    try
      FLogStream := TFileStream.Create(FLogFile, fmCreate or fmOpenWrite);
      MakeLogEntry(FArchiveName, ltStart);
    except
      raise EAbException.Create(AbLogCreateErrorS);                      {!!.02}
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.TagItems(const FileMask : string);
  {tag all items that match the mask}
var
  i : Integer;
begin
  if Count > 0 then
    for i := 0 to pred(Count) do
      with TAbArchiveItem(FItemList[i]) do begin
        if MatchesStoredNameEx(FileMask) then                            
          Tagged := True;
      end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.UnTagItems(const FileMask : string);
  {clear tags for all items that match the mask}
var
  i : Integer;
begin
  if Count > 0 then
    for i := 0 to pred(Count) do
      with TAbArchiveItem(FItemList[i]) do begin
        if MatchesStoredNameEx(FileMask) then
          Tagged := False;
      end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  raise EAbSpanningNotSupported.Create;
end;
{ -------------------------------------------------------------------------- }
class function TAbArchive.SupportsEmptyFolder: Boolean;
begin
	Result := False;
end;
{ -------------------------------------------------------------------------- }

{ TAbExtraField implementation ============================================= }
procedure TAbExtraField.Changed;
begin
  // No-op
end;
{ -------------------------------------------------------------------------- }
procedure TAbExtraField.Clear;
begin
  FBuffer := nil;
  Changed;
end;
{ -------------------------------------------------------------------------- }
procedure TAbExtraField.Delete(aID : Word);
var
  SubField : PAbExtraSubField;
begin
  if FindField(aID, SubField) then begin
    DeleteField(SubField);
    Changed;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbExtraField.DeleteField(aSubField : PAbExtraSubField);
var
  Len, Offset : Integer;
begin
  Len := SizeOf(TAbExtraSubField) + aSubField.Len;
  Offset := Pointer(aSubField) - Pointer(FBuffer);
  if Offset + Len < Length(FBuffer) then
    Move(FBuffer[Offset + Len], aSubField^, Length(FBuffer) - Offset - Len);
  SetLength(FBuffer, Length(FBuffer) - Len);
end;
{ -------------------------------------------------------------------------- }
function TAbExtraField.FindField(aID : Word;
  out aSubField : PAbExtraSubField) : Boolean;
begin
  Result := False;
  aSubField := nil;
  while FindNext(aSubField) do
    if aSubField.ID = aID then begin
      Result := True;
      Break;
    end;
end;
{ -------------------------------------------------------------------------- }
function TAbExtraField.FindNext(var aCurField : PAbExtraSubField) : Boolean;
var
  BytesLeft : Integer;
begin
  if aCurField = nil then begin
    aCurField := PAbExtraSubField(FBuffer);
    BytesLeft := Length(FBuffer);
  end
  else begin
    BytesLeft := Length(FBuffer) -
      (Pointer(aCurField) - Pointer(FBuffer)) -
      SizeOf(TAbExtraSubField) - aCurField.Len;
    Inc(Pointer(aCurField), aCurField.Len + SizeOf(TAbExtraSubField));
  end;
  Result := (BytesLeft >= SizeOf(TAbExtraSubField));
  if Result and (BytesLeft < SizeOf(TAbExtraSubField) + aCurField.Len) then
    aCurField.Len := BytesLeft - SizeOf(TAbExtraSubField);
end;
{ -------------------------------------------------------------------------- }
function TAbExtraField.Get(aID : Word; out aData : Pointer;
  out aDataSize : Word) : Boolean;
var
  SubField : PAbExtraSubField;
begin
  Result := FindField(aID, SubField);
  if Result then begin
    aData := @SubField.Data;
    aDataSize := SubField.Len;
  end
  else begin
    aData := nil;
    aDataSize := 0;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbExtraField.GetCount : Integer;
var
  SubField : PAbExtraSubField;
begin
  Result := 0;
  SubField := nil;
  while FindNext(SubField) do
    Inc(Result);
end;
{ -------------------------------------------------------------------------- }
function TAbExtraField.GetID(aIndex : Integer): Word;
var
  i: Integer;
  SubField : PAbExtraSubField;
begin
  i := 0;
  SubField := nil;
  while FindNext(SubField) do
    if i = aIndex then begin
      Result := SubField.ID;
      Exit;
    end
    else
      Inc(i);
  raise EListError.CreateFmt(SListIndexError, [aIndex]);
end;
{ -------------------------------------------------------------------------- }
procedure TAbExtraField.LoadFromStream(aStream : TStream; aSize : Word);
begin
  SetLength(FBuffer, aSize);
  if aSize > 0 then
    aStream.ReadBuffer( FBuffer[0], aSize);
end;
{ -------------------------------------------------------------------------- }
procedure TAbExtraField.Put(aID : Word; const aData; aDataSize : Word);
var
  Offset : Cardinal;
  SubField : PAbExtraSubField;
begin
  if FindField(aID, SubField) then begin
    if SubField.Len = aDataSize then begin
      Move(aData, SubField.Data, aDataSize);
      Changed;
      Exit;
    end
    else DeleteField(SubField);
  end;
  Offset := Length(FBuffer);
  SetLength(FBuffer, Length(FBuffer) + SizeOf(TAbExtraSubField) + aDataSize);
  SubField := PAbExtraSubField(@FBuffer[Offset]);
  SubField.ID := aID;
  SubField.Len := aDataSize;
  Move(aData, SubField.Data, aDataSize);
  Changed;
end;
{ -------------------------------------------------------------------------- }
procedure TAbExtraField.SetBuffer(const aValue : TByteDynArray);
begin
  SetLength(FBuffer, Length(aValue));
  if Length(FBuffer) > 0 then
    Move(aValue[0], FBuffer[0], Length(FBuffer));
  Changed;
end;
{ -------------------------------------------------------------------------- }

{ ========================================================================== }
{ TAbArchiveStreamHelper }

constructor TAbArchiveStreamHelper.Create(AStream: TStream);
begin
  if Assigned(AStream) then
    FStream := AStream
  else
    raise Exception.Create('nil stream');
end;

end.

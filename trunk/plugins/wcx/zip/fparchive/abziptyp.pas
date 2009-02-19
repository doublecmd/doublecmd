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
{* ABBREVIA: AbZipTyp.pas 3.04                           *}
{*********************************************************}
{* ABBREVIA: PKZip types                                 *}
{* Based on information from Appnote.txt, shipped with   *}
{* PKWare's PKZip for Windows 2.5                        *}
{*********************************************************}

{$I AbDefine.inc}

unit AbZipTyp;

interface

uses
  Types, //!!  MVC for MMAX_PATH
  Classes, AbArcTyp, AbUtils, AbSpanSt;

const
  { note  #$50 = 'P', #$4B = 'K'}
  Ab_ZipVersion = 20;
  Ab_ZipLocalFileHeaderSignature            : Longint = $04034B50;
  Ab_ZipCentralDirectoryFileHeaderSignature : Longint = $02014B50;
  Ab_ZipCentralDirectoryTailSignature       : Longint = $06054B50;
  Ab_ZipSpannedSetSignature                 : Longint = $08074B50;
  Ab_ZipPossiblySpannedSignature            : Longint = $30304B50;
  Ab_GeneralZipSignature                    : Word    = $4B50;       {!!.02}

  Ab_WindowsExeSignature                    : Word    = $5A4D;       {!!.02}
  Ab_LinuxExeSigWord1                       : Word    = $457F;       {!!.02}
  Ab_LinuxExeSigWord2                       : Word    = $464C;       {!!.02}

  Ab_iWindowSize            = $8000;  {Inflate window size}
  Ab_iMaxCodeLen            = 16;     {Maximum bit length of any code}
  Ab_iMaxCodes              = 288;    {Maximum number of codes in any set}
  Size32K                   = 32768;
  AbDefZipSpanningThreshold = 0;
  AbDefPasswordRetries      = 3;
  AbFileIsEncryptedFlag     = $0001;
  AbHasDataDescriptorFlag   = $0008;

var
  Ab_ZipEndCentralDirectorySignature : Longint = $06054B50;

type
  PAbByteArray4K = ^TAbByteArray4K;
  TAbByteArray4K = array[1..4096] of Byte;
  PAbByteArray8K = ^TAbByteArray8K;
  TAbByteArray8K = array[0..8192] of Byte;
  PAbIntArray8K  = ^TAbIntArray8K;
  TAbIntArray8K  = array[0..8192] of SmallInt;

  PAbWordArray   = ^TAbWordArray;
  TAbWordArray   = array[0..65535 div SizeOf(Word)-1] of Word;
  PAbByteArray   = ^TAbByteArray;
  TAbByteArray   = array[0..65535-1] of Byte;
  PAbSmallIntArray = ^TAbSmallIntArray;
  TAbSmallIntArray = array[0..65535 div SizeOf(SmallInt)-1] of SmallInt;

  PAbIntegerArray = ^TAbIntegerArray;
  TAbIntegerArray = array[0..65535 div sizeof(integer)-1] of integer;

  PAbiSlide = ^TAbiSlide;
  TAbiSlide = array[0..Ab_iWindowSize] of Byte;

  PPAbHuft           = ^PAbHuft;
  PAbHuft            = ^TAbHuft;
  TAbHuft             = packed record
    ExtraBits      : Byte;   {Number of extra bits}
    NumBits        : Byte;   {Number of bits in this code or subcode}
    Filler : Word;
    case Byte of
      0: (N        : Word);  {Literal, length base, or distance base}
      1: (NextLevel: PAbHuft); {Pointer to next level of table}
  end;

  TAbFollower =                      {used to expand reduced files}
    packed record
      Size : Byte;                {size of follower set}
      FSet : array[0..31] of Byte; {follower set}
    end;
  PAbFollowerSets = ^TAbFollowerSets;
  TAbFollowerSets = array[0..255] of TAbFollower;


  PAbSfEntry = ^TAbSfEntry;
  TAbSfEntry =                       {entry in a Shannon-Fano tree}
    packed record
      case Byte of
        0 : (Code : Word; Value, BitLength : Byte);
        1 : (L : Longint);
    end;
  PAbSfTree = ^TAbSfTree;
  TAbSfTree =
    packed record                        {a Shannon-Fano tree}
      Entries : SmallInt;
      MaxLength : SmallInt;
      Entry : array[0..256] of TAbSfEntry;
    end;

  PAbWord = ^Word;

  TAbFCData = packed record
    case Byte of
      0 : (Freq : Word);  {frequency count}
      1 : (Code : Word);  {bit string}
  end;

  TAbDLData = packed record
    case Byte of
      0 : (Dad : Word);  {father node in Huffman tree}
      1 : (Len : Word);  {length of bit string}
  end;

  {Data structure describing a single value and its code string}
  TAbCTData = packed record
    FC : TAbFCData;
    Filler : word;
    DL : TAbDLData;
  end;
  PAbCTDataArray = ^TAbCTDataArray;
  TAbCTDataArray = array[0..65535 div SizeOf(TAbCTData) - 1] of TAbCTData;

  TAbTreeDescription = packed record
    DynamicTree : PAbCTDataArray;  {the dynamic tree}
    StaticTree  : PAbCTDataArray;  {corresponding static tree or NULL}
    ExtraBits   : PAbWordArray;    {extra bits for each code or NULL}
    ExtraBase   : SmallInt;         {base index for ExtraBits}
    MaxElements : SmallInt;         {max number of elements in the tree}
    MaxLength   : SmallInt;         {max bit length for the codes}
    MaxCode     : SmallInt;         {largest code with non zero frequency}
  end;


type
  TAbFileType =
    (Binary, Ascii, Unknown);

  TAbZipCompressionMethod =
    (cmStored, cmShrunk, cmReduced1, cmReduced2, cmReduced3,
     cmReduced4, cmImploded, cmTokenized, cmDeflated,
     cmEnhancedDeflated, cmDCLImploded, cmBestMethod);

  TAbZipSupportedMethod =
    (smStored, smDeflated, smBestMethod);

  TAbZipHostOS =
    (hosDOS, hosAmiga, hosVAX, hosUnix, hosVMCMS, hosAtari,
     hosOS2, hosMacintosh, hosZSystem, hosCPM, hosNTFS);

  {for method 6 - imploding}
  TAbZipDictionarySize =
    (dsInvalid, ds4K, ds8K);

  {for method 8 - deflating}
  TAbZipDeflationOption =
    (doInvalid, doNormal, doMaximum, doFast, doSuperFast );

type
  TAbNeedPasswordEvent = procedure(Sender : TObject;
                                   var NewPassword : string) of object;

const
  AbDefCompressionMethodToUse = smBestMethod;
  AbDefDeflationOption = doNormal;


type
  TAbZipDataDescriptor = class( TObject )
  protected {private}
    FCRC32            : Longint;
    FCompressedSize   : Longint;
    FUncompressedSize : Longint;
  public {methods}
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property CRC32 : Longint
      read FCRC32 write FCRC32;
    property CompressedSize : Longint
      read FCompressedSize write FCompressedSize;
    property UncompressedSize : Longint
      read FUncompressedSize write FUncompressedSize;
  end;

type
{ TAbZipFileHeader interface =============================================== }
  {ancestor class for ZipLocalFileHeader and DirectoryFileHeader}
  TAbZipFileHeader = class( TObject )
  protected {private}
    FValidSignature : Longint;
    FSignature : Longint;
    FVersionNeededToExtract : Word;
    FGeneralPurposeBitFlag : Word;
    FCompressionMethod : Word;
    FLastModFileTime : Word;
    FLastModFileDate : Word;
    FCRC32 : Longint;
    FCompressedSize : Longint;
    FUncompressedSize : Longint;
    FFileNameLength : Word;
    FExtraFieldLength : Word;
    FFileName : PChar;
    FExtraField : PChar;
    FIsValid : Boolean;
  protected {methods}
    function GetCompressionMethod : TAbZipCompressionMethod;
    function GetCompressionRatio : Double;
    function GetDataDescriptor : Boolean;
    function GetDeflationOption : TAbZipDeflationOption;
    function GetDictionarySize : TAbZipDictionarySize;
    function GetEncrypted : Boolean;
    function GetShannonFanoTreeCount : Byte;
    function GetValid : Boolean;
    procedure SetCompressionMethod( Value : TAbZipCompressionMethod );
    procedure SetExtraField( Value : PChar );
    procedure SetFileName( Value : PChar );
  public {methods}
    constructor Create;
    destructor Destroy; override;
  public {properties}
    property Signature : Longint
      read FSignature write FSignature;
    property VersionNeededToExtract : Word
      read FVersionNeededToExtract write FVersionNeededToExtract;
    property GeneralPurposeBitFlag : Word
      read FGeneralPurposeBitFlag write FGeneralPurposeBitFlag;
    property CompressionMethod : TAbZipCompressionMethod
      read GetCompressionMethod write SetCompressionMethod;
    property LastModFileTime : Word
      read FLastModFileTime write FLastModFileTime;
    property LastModFileDate : Word
      read FLastModFileDate write FLastModFileDate;
    property CRC32 : Longint
      read FCRC32 write FCRC32;
    property CompressedSize : Longint
      read FCompressedSize write FCompressedSize;
    property UncompressedSize : Longint
      read FUncompressedSize write FUncompressedSize;
    property FileNameLength : Word
      read FFileNameLength write FFileNameLength;
    property ExtraFieldLength : Word
      read FExtraFieldLength write FExtraFieldLength;
    property FileName : PChar
      read FFileName write SetFileName;
    property ExtraField : PChar
      read FExtraField write SetExtraField;

    property CompressionRatio : Double
      read GetCompressionRatio;
    property DeflationOption : TAbZipDeflationOption
      read GetDeflationOption;
    property DictionarySize : TAbZipDictionarySize
      read GetDictionarySize;
    property HasDataDescriptor : Boolean
      read GetDataDescriptor;
    property IsValid : Boolean
      read GetValid;
    property IsEncrypted : Boolean
      read GetEncrypted;
    property ShannonFanoTreeCount : Byte
      read GetShannonFanoTreeCount;
  end;

{ TAbZipLocalFileHeader interface ========================================== }
  TAbZipLocalFileHeader = class( TAbZipFileHeader )
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  end;

{ TAbZipDirectoryFileHeader interface ====================================== }
  TAbZipDirectoryFileHeader = class( TAbZipFileHeader )
  protected {private}
    FVersionMadeBy          : Word;
    FFileCommentLength      : Word;
    FDiskNumberStart        : Word;
    FInternalFileAttributes : Word;
    FExternalFileAttributes : Longint;
    FRelativeOffset         : Longint;
    FFileComment            : PChar;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property VersionMadeBy : Word
      read FVersionMadeBy write FVersionMadeBy;
    property FileCommentLength : Word
      read FFileCommentLength write FFileCommentLength;
    property DiskNumberStart : Word
      read FDiskNumberStart write FDiskNumberStart;
    property InternalFileAttributes : Word
      read FInternalFileAttributes write FInternalFileAttributes;
    property ExternalFileAttributes : Longint
      read FExternalFileAttributes write FExternalFileAttributes;
    property RelativeOffset : Longint
      read FRelativeOffset write FRelativeOffset;
    property FileComment : PChar
      read FFileComment write FFileComment;
  end;

{ TAbZipDirectoryFileFooter interface ====================================== }
  TAbZipDirectoryFileFooter = class( TObject )
  protected {private}
    FValidSignature       : Longint;
    FSignature            : Longint;
    FDiskNumber           : Word;
    FStartDiskNumber      : Word;
    FEntriesOnDisk        : Word;
    FTotalEntries         : Word;
    FDirectorySize        : Longint;
    FDirectoryOffset      : Longint;
    FZipfileCommentLength : Word;
    FZipFileComment       : PChar;
    function GetValid : Boolean;
  public {methods}
    constructor Create;
    destructor Destroy;
      override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property Signature : Longint
      read FSignature write FSignature;
    property DiskNumber : Word
      read FDiskNumber write FDiskNumber;
    property EntriesOnDisk : Word
      read FEntriesOnDisk write FEntriesOnDisk;
    property TotalEntries : Word
      read FTotalEntries write FTotalEntries;
    property DirectorySize : Longint
      read FDirectorySize write FDirectorySize;
    property DirectoryOffset : Longint
      read FDirectoryOffset write FDirectoryOffset;
    property StartDiskNumber : Word
      read FStartDiskNumber write FStartDiskNumber;
    property ZipfileCommentLength : Word
      read FZipfileCommentLength write FZipfileCommentLength;
    property ZipfileComment : PChar
      read FZipfileComment write FZipfileComment;
    property IsValid : Boolean
      read GetValid;
  end;

{ TAbZipItem interface ===================================================== }
  TAbZipItem = class( TAbArchiveItem )
  protected {private}
    FItemInfo : TAbZipDirectoryFileHeader;
    FDecoder : TObject;

  protected {methods}
    function GetCompressionMethod : TAbZipCompressionMethod;
    function GetCompressionRatio : Double;
    function GetDeflationOption : TAbZipDeflationOption;
    function GetDictionarySize : TAbZipDictionarySize;
    function GetDiskNumberStart : Word;
    function GetExtraField : string;
    function GetFileComment : string;
    function GetGeneralPurposeBitFlag : Word;
    function GetInternalFileAttributes : Word;
    function GetRelativeOffset : Longint;
    function GetShannonFanoTreeCount : Byte;
    function GetVersionMadeBy : Word;
    function GetVersionNeededToExtract : Word;
    procedure SaveCDHToStream( Stream : TStream );
    procedure SaveDDToStream( Stream : TStream );
    procedure SaveLFHToStream( Stream : TStream );
    procedure SetCompressionMethod( Value : TAbZipCompressionMethod );
    procedure SetDiskNumberStart( Value : Word );
    procedure SetFileComment( Value : string );
    procedure SetExtraField( Value : string );
    procedure SetGeneralPurposeBitFlag( Value : Word );
    procedure SetInternalFileAttributes( Value : Word );
    procedure SetRelativeOffset( Value : Longint );
    procedure SetVersionMadeBy( Value : Word );
    procedure SetVersionNeededToExtract( Value : Word );

  protected {redefined property methods}
    function  GetCompressedSize : Longint; override;
    function  GetCRC32 : Longint; override;
    function  GetExternalFileAttributes : Longint; override;
    function  GetFileName : string; override;
    function  GetIsEncrypted : Boolean; override;
    function  GetLastModFileDate : Word; override;
    function  GetLastModFileTime : Word; override;
    function  GetUncompressedSize : Longint; override;
    procedure SetCompressedSize( const Value : Longint ); override;
    procedure SetCRC32( const Value : Longint ); override;
    procedure SetExternalFileAttributes( Value : Longint ); override;
    procedure SetFileName( Value : string ); override;
    procedure SetLastModFileDate(const Value : Word ); override;
    procedure SetLastModFileTime(const Value : Word ); override;
    procedure SetUncompressedSize( const Value : Longint ); override;

  public {methods}
    constructor Create;
    destructor  Destroy; override;
    procedure LoadFromStream( Stream : TStream );

  public {properties}
    property CompressionMethod : TAbZipCompressionMethod
      read GetCompressionMethod
      write SetCompressionMethod;
    property CompressionRatio : Double
      read GetCompressionRatio;
    property DeflationOption : TAbZipDeflationOption
      read GetDeflationOption;
    property DictionarySize : TAbZipDictionarySize
      read GetDictionarySize;
    property DiskNumberStart : Word
      read GetDiskNumberStart
      write SetDiskNumberStart;
    property ExtraField : string
      read GetExtraField
      write SetExtraField;
    property FileComment : string
      read GetFileComment
      write SetFileComment;
    property InternalFileAttributes : Word
      read GetInternalFileAttributes
      write SetInternalFileAttributes;
    property GeneralPurposeBitFlag : Word
      read GetGeneralPurposeBitFlag
      write SetGeneralPurposeBitFlag;
    property RelativeOffset : Longint
      read GetRelativeOffset
      write SetRelativeOffset;
    property ShannonFanoTreeCount : Byte
      read GetShannonFanoTreeCount;
    property VersionMadeBy : Word
      read GetVersionMadeBy
      write SetVersionMadeBy;
    property VersionNeededToExtract : Word
      read GetVersionNeededToExtract
      write SetVersionNeededToExtract;
  end;

{ TAbZipArchive interface ================================================== }
  TAbZipArchive = class( TAbArchive )
  protected {private}
    FCompressionMethodToUse : TAbZipSupportedMethod;
    FCurrentDisk            : Word;
    FDeflationOption        : TAbZipDeflationOption;
    FDriveIsRemovable       : Boolean;
    FInfo                   : TAbZipDirectoryFileFooter;
    FIsExecutable           : Boolean;
    FPassword               : string;
    FPasswordRetries        : Byte;
    FStubSize               : Longint;
    FAutoGen                : Boolean;                               {!!.02}

    FExtractHelper          : TAbArchiveItemExtractEvent;
    FExtractToStreamHelper  : TAbArchiveItemExtractToStreamEvent;
    FTestHelper             : TAbArchiveItemTestEvent;
    FInsertHelper           : TAbArchiveItemInsertEvent;
    FInsertFromStreamHelper : TAbArchiveItemInsertFromStreamEvent;
    FOnNeedPassword         : TAbNeedPasswordEvent;
    FOnRequestLastDisk      : TAbRequestDiskEvent;
    FOnRequestNthDisk       : TAbRequestNthDiskEvent;
    FOnRequestBlankDisk     : TAbRequestDiskEvent;

  protected {methods}

    function CreateItem(const FileSpec : string): TAbArchiveItem; override;
    procedure DoExtractHelper(Index : Integer; NewName : string);
    procedure DoExtractToStreamHelper(Index : Integer; aStream : TStream);
    procedure DoTestHelper(Index : Integer);
    procedure DoInsertHelper(Index : Integer; OutStream : TStream);
    procedure DoInsertFromStreamHelper(Index : Integer; OutStream : TStream);
    procedure DoRequestNextImage(ImageNumber : Integer; var Stream : TStream;
      var Abort : Boolean );
    function FindCDTail : Longint;
    function GetItem( Index : Integer ) : TAbZipItem;                    
    function GetZipFileComment : string;
    procedure PutItem( Index : Integer; Value : TAbZipItem );
    procedure DoRequestLastDisk( var Abort : Boolean );
      virtual;
    procedure DoRequestNthDisk( DiskNumber : Byte; var Abort : Boolean );
      virtual;
    procedure DoRequestBlankDisk( var Abort : Boolean );
      virtual;
    procedure ExtractItemAt(Index : Integer; const NewName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
      override;
    procedure TestItemAt(Index : Integer);
      override;
    function FixName( Value : string ) : string;
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure SetZipFileComment( Value : string );

  protected {properties}
    property IsExecutable : Boolean
      read FIsExecutable write FIsExecutable;

  public {protected}
    procedure DoRequestNthImage(ImageNumber : Integer; var Stream : TStream;
      var Abort : Boolean );
    procedure DoSpanningMediaRequest(Sender: TObject; ImageNumber: Integer;
      var ImageName: string; var Abort: Boolean); override;
    procedure DoRequestImage(Mode : TAbSpanMode; ImageNumber: Integer;   {!!.01}
      var ImageName: string; var Abort: Boolean);                        {!!.01}

  public {methods}
    constructor Create( FileName : string; Mode : Word );
      override;
    constructor CreateFromStream( aStream : TStream; ArchiveName : string );
    destructor Destroy;
      override;

  public {properties}
    property CompressionMethodToUse : TAbZipSupportedMethod
      read FCompressionMethodToUse
      write FCompressionMethodToUse;
    property CurrentDisk : Word
      read FCurrentDisk
      write FCurrentDisk;
    property DeflationOption : TAbZipDeflationOption
      read FDeflationOption
      write FDeflationOption;
    property DriveIsRemovable : Boolean
      read FDriveIsRemovable;
    property ExtractHelper : TAbArchiveItemExtractEvent
      read FExtractHelper
      write FExtractHelper;
    property ExtractToStreamHelper : TAbArchiveItemExtractToStreamEvent
      read FExtractToStreamHelper
      write FExtractToStreamHelper;
    property TestHelper : TAbArchiveItemTestEvent
      read FTestHelper
      write FTestHelper;
    property InsertHelper : TAbArchiveItemInsertEvent
      read FInsertHelper
      write FInsertHelper;
    property InsertFromStreamHelper : TAbArchiveItemInsertFromStreamEvent
      read FInsertFromStreamHelper
      write FInsertFromStreamHelper;
    property Password : string
      read FPassword
      write FPassword;
    property PasswordRetries : Byte
      read FPasswordRetries
      write FPasswordRetries
      default AbDefPasswordRetries;
    property StubSize : Longint
      read FStubSize;
    property ZipFileComment : string
      read GetZipFileComment
      write SetZipFileComment;

    property Items[Index : Integer] : TAbZipItem                      {!!.03}
      read GetItem                                                    {!!.03}
      write PutItem; default;                                         {!!.03}

{!!!}    procedure SaveArchive2;


  public {events}
    property OnNeedPassword : TAbNeedPasswordEvent
      read FOnNeedPassword write FOnNeedPassword;
    property OnRequestLastDisk : TAbRequestDiskEvent
      read FOnRequestLastDisk write FOnRequestLastDisk;
    property OnRequestNthDisk : TAbRequestNthDiskEvent
      read FOnRequestNthDisk write FOnRequestNthDisk;
    property OnRequestBlankDisk : TAbRequestDiskEvent
      read FOnRequestBlankDisk write FOnRequestBlankDisk;
  end;

{============================================================================}
procedure MakeSelfExtracting( StubStream, ZipStream,
  SelfExtractingStream : TStream );
    {-takes an executable stub, and a .zip format stream, and creates
     a SelfExtracting stream.  The stub should create a TAbZipArchive
     passing itself as the file, using a read-only open mode.  It should
     then perform operations as needed - like ExtractFiles( '*.*' ).
     This routine updates the RelativeOffset of each item in the archive}

function FindCentralDirectoryTail(aStream : TStream) : Longint;

function VerifyZip(Strm : TStream) : TAbArchiveType;

function VerifySelfExtracting(Strm : TStream) : TAbArchiveType;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
//  Dialogs,                                                           {!!.04}
  {$ENDIF}
  {$IFDEF LINUX}
//  Libc,
  {$IFNDEF NoQt}
  {$IFDEF UsingCLX}
  QControls,
  QDialogs,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  AbConst,
  AbExcept,
  AbVMStrm,
  SysUtils;

function VerifyZip(Strm : TStream) : TAbArchiveType;
{ determine if stream appears to be in PkZip format }
var
  Footer       : TAbZipDirectoryFileFooter;
  Sig          : LongInt;                                                {!!.01}
  TailPosition : LongInt;
  StartPos     : LongInt;
begin
  StartPos := Strm.Position;
  Result := atUnknown;

  Strm.Position := 0;                                                {!!.02}
  Strm.Read(Sig, SizeOf(LongInt));                                   {!!.02}
  if (Sig = Ab_ZipSpannedSetSignature) or                            {!!.02}
     (Sig = Ab_ZipPossiblySpannedSignature) then                     {!!.02}
    Result := atSpannedZip                                           {!!.02}
  else begin                                                         {!!.02}

    { attempt to find Central Directory Tail }
    TailPosition := FindCentralDirectoryTail( Strm );
    if TailPosition <> -1 then begin
      { check Central Directory Signature }
      Footer := TAbZipDirectoryFileFooter.Create;
      try
        Footer.LoadFromStream(Strm);
        if Footer.FSignature = AB_ZipCentralDirectoryTailSignature then
          Result := atZip;
      finally
        Footer.Free;
      end;
    end
(* {!!.02}
  else begin  { may be a span }                                          {!!.01}
    Strm.Seek(0, soFromBeginning);                                       {!!.01}
    Strm.Read(Sig, SizeOf(LongInt));                                     {!!.01}
    if (Sig = Ab_ZipSpannedSetSignature)                                 {!!.01}
      or (Sig = Ab_ZipPossiblySpannedSignature)                          {!!.01}
    then                                                                 {!!.01}
      Result := atSpannedZip;                                            {!!.01}
*) {!!.02}
  end;                                                                   {!!.01}
  Strm.Position := StartPos;
end;

function VerifySelfExtracting(Strm : TStream) : TAbArchiveType;
{ determine if stream appears to be an executable with appended PkZip data }
var
  FileSignature : LongInt;
  StartPos      : LongInt;
  IsWinExe, IsLinuxExe : Boolean;                                        {!!.01}
begin
  StartPos := Strm.Position;
  { verify presence of executable stub }
  {check file type of stub stream}
  Strm.Position := 0;
  Strm.Read( FileSignature, sizeof( FileSignature ) );

  Result := atSelfExtZip;

{!!.01 -- re-written Executable Type Detection to allow use of non-native stubs }
  IsLinuxExe := False;
  IsWinExe := LongRec(FileSignature).Lo = Ab_WindowsExeSignature;        {!!.02}
  if not IsWinExe then begin
    IsLinuxExe := FileSignature = Ab_LinuxExeSigWord1; { check 1st sig }
    if IsLinuxExe then begin
      Strm.Read(FileSignature, SizeOf(FileSignature)); { check 2nd sig }
      IsLinuxExe := FileSignature = Ab_LinuxExeSigWord2;
    end;
  end;

  if not (IsWinExe or IsLinuxExe) then
    Result := atUnknown;

{!!.01 -- end re-written }
  { Check for central directory tail }
  if VerifyZip(Strm) <> atZip then
    Result := atUnknown;

  Strm.Position := StartPos;
end;

{============================================================================}
function FindCentralDirectoryTail(aStream : TStream) : Longint;
{ search end of aStream looking for ZIP Central Directory structure
  returns position in stream if found (otherwise returns -1),
  leaves stream positioned at start of structure or at original
  position if not found }
const
  StartBufSize = 512;
  CMaxBufSize = 64 * 1024;                                               {!!.01}
var
  StartPos  : longint;
  TailRec : packed record
    trSig : longint;
    trMid : array [0..15] of byte;
    trLen : word;
  end;
  Buffer    : PAnsiChar;
  Offset    : longint;
  TestPos   : PAnsiChar;
  Done      : boolean;
  BytesRead : longint;
  BufSize   : integer;
  MaxBufSize: Integer;                                                   {!!.01}
  CommentLen: integer;
  SpanState : Boolean;                                                   {!!.01}
begin
  { if spanning stream, don't want stream to read past beginning of current span}
  MaxBufSize := CMaxBufSize;                                             {!!.01}
  SpanState := False;                                                    {!!.01}
  if aStream is TAbSpanStream then begin                                 {!!.01}
    if (TAbSpanStream(aStream).Size > 0) and                             {!!.01}
      (TAbSpanStream(aStream).Size < CMaxBufSize) then                   {!!.01}
      MaxBufSize := TAbSpanStream(aStream).Size;                         {!!.01}
    SpanState := TAbSpanStream(aStream).IgnoreSpanning;                  {!!.01}
    TAbSpanStream(aStream).IgnoreSpanning := True;                       {!!.01}
  end;                                                                   {!!.01}

  {save the starting position}
  StartPos := aStream.Seek(0, soFromCurrent);

  {start off with the majority case: no zip file comment, so the
   central directory tail is the last thing in the stream and it's a
   fixed size and doesn't indicate a zip file comment}
  Result := aStream.Seek(-sizeof(TailRec), soFromEnd);
  if (Result >= 0) then begin
    aStream.ReadBuffer(TailRec, sizeof(TailRec));
    if (TailRec.trSig = Ab_ZipEndCentralDirectorySignature) and
       (TailRec.trLen = 0) then begin
      aStream.Seek(Result, soFromBeginning);
      Exit;
    end;
  end;

  {the zip stream seems to have a comment, or it has null padding
   bytes from some flaky program, or it's not even a zip formatted
   stream; we need to search for the tail signature}

  {get a buffer}
  BufSize := StartBufSize;
  GetMem(Buffer, BufSize);
  try

    {start out searching backwards}
    Offset := -BufSize;

    {while there is still data to search ...}
    Done := false;
    while not Done do begin

      {seek to the search position}
      Result := aStream.Seek(Offset, soFromEnd);
      if (Result <= 0) then begin                                        {!!.01}
        Result := aStream.Seek(0, soFromBeginning);
        Done := true;
      end;

      {read a buffer full}
      BytesRead := aStream.Read(Buffer^, BufSize);

      {search backwards through the buffer looking for the signature}
      TestPos := Buffer + BytesRead - sizeof(TailRec);
      while (TestPos <> Buffer) and
            (PLongint(TestPos)^ <> Ab_ZipEndCentralDirectorySignature) do
        dec(TestPos);

      {if we found the signature...}
      if (PLongint(TestPos)^ = Ab_ZipEndCentralDirectorySignature) then begin

        {get the tail record at this position}
        Move(TestPos^, TailRec, sizeof(TailRec));

        {if it's as valid a tail as we can check here...}
        CommentLen := -Offset - (TestPos - Buffer + sizeof(TailRec));
        if (TailRec.trLen <= CommentLen) then begin

          {calculate its position and exit}
          Result := Result + (TestPos - Buffer);
          aStream.Seek(Result, soFromBeginning);
          Exit;
        end;
      end;

      {otherwise move back one step, doubling the buffer}
      if (BufSize < MaxBufSize) then begin                               {!!.01}
{        write('+');}                                                    {!!.01}
        FreeMem(Buffer);
        BufSize := BufSize * 2;
        if BufSize > MaxBufSize then                                     {!!.01}
          BufSize := MaxBufSize;                                         {!!.01}
        GetMem(Buffer, BufSize);
      end;
{      else}                                                             {!!.01}
{        write('.');}                                                    {!!.01}
      dec(Offset, BufSize - SizeOf(longint));
    end;

    {if we reach this point, the CD tail is not present}
    Result := -1;
    aStream.Seek(StartPos, soFromBeginning);
  finally
    FreeMem(Buffer);
  end;

  { put SpanStream back the way it was }
  if aStream is TAbSpanStream then                                       {!!.01}
    TAbSpanStream(aStream).IgnoreSpanning := SpanState;                  {!!.01}
end;
{============================================================================}

{ TAbZipDataDescriptor implementation ====================================== }
procedure TAbZipDataDescriptor.LoadFromStream( Stream : TStream );
begin
  Stream.Read( FCRC32, sizeof( FCRC32 ) );
  Stream.Read( FCompressedSize, sizeof( FCompressedSize ) );
  Stream.Read( FUncompressedSize, sizeof( FUncompressedSize ) );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDataDescriptor.SaveToStream( Stream : TStream );
begin
  {!!.01 -- rewritten}
  Stream.Write( FCRC32, sizeof( FCRC32 ) );
  Stream.Write( FCompressedSize, sizeof( FCompressedSize ) );
  Stream.Write( FUncompressedSize, sizeof( FUncompressedSize ) );
  {!!.01 -- end rewritten}
end;
{ -------------------------------------------------------------------------- }

{ TAbZipFileHeader implementation ========================================== }
constructor TAbZipFileHeader.Create;
begin
  inherited Create;
  FValidSignature := $0;
  FFileName := nil;
  FExtraField := nil;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipFileHeader.Destroy;
begin
  if Assigned( FFileName ) then
    StrDispose( FFileName );
  if Assigned( FExtraField ) then
    StrDispose( FExtraField );
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetCompressionMethod : TAbZipCompressionMethod;
begin
  Result := TAbZipCompressionMethod( FCompressionMethod );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDataDescriptor : Boolean;
begin
  Result := ( CompressionMethod = cmDeflated ) and
            ( ( FGeneralPurposeBitFlag and AbHasDataDescriptorFlag ) <> 0 );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetCompressionRatio : Double;
var
  CompSize : Longint;
begin
  {adjust for encrypted headers - ensures we never get negative compression
  ratios for stored, encrypted files - no guarantees about negative
  compression ratios in other cases}
  if isEncrypted then
    CompSize := CompressedSize - 12
  else
    CompSize := CompressedSize;
  if UncompressedSize > 0 then
    Result := 100.0 * ( 1 - ( ( 1.0 * CompSize ) / UncompressedSize ) )
  else
    Result := 0.0;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDeflationOption : TAbZipDeflationOption;
begin
  if CompressionMethod = cmDeflated then
    if ( ( FGeneralPurposeBitFlag and $02 ) <> 0 ) then
      if ( ( FGeneralPurposeBitFlag and $04 ) <> 0 ) then
        Result := doSuperFast
      else
        Result := doMaximum
    else
      if ( ( FGeneralPurposeBitFlag and $04 ) <> 0 ) then
        Result := doFast
      else
        Result := doNormal
  else
    Result := doInvalid;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDictionarySize : TAbZipDictionarySize;
begin
  if CompressionMethod = cmImploded  then
    if ( ( FGeneralPurposeBitFlag and $02 ) <> 0 ) then
      Result := ds8K
    else
      Result := ds4K
  else
    Result := dsInvalid;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetEncrypted : Boolean;
begin
  {bit 0 of the GeneralPurposeBitFlag}
  Result := ( ( FGeneralPurposeBitFlag and AbFileIsEncryptedFlag ) <> 0 );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetShannonFanoTreeCount : Byte;
begin
  if CompressionMethod = cmImploded then
    if ( ( FGeneralPurposeBitFlag and $04 ) <> 0 ) then
      Result := 3
    else
      Result := 2
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetValid : Boolean;
begin
  Result := ( FValidSignature = FSignature );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetCompressionMethod( Value :
                                               TAbZipCompressionMethod );
begin
  FCompressionMethod := Ord( Value );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetExtraField( Value : PChar );
begin
  if Assigned( FExtraField ) then
    StrDispose( FExtraField );
  FExtraField := nil;
  FExtraFieldLength := StrLen( Value );

  if FExtraFieldLength > 0 then begin
    FExtraField := StrAlloc( succ( FExtraFieldLength ) );
    StrCopy( FExtraField, Value );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetFileName( Value : PChar );
begin
  if Assigned( FFileName ) then
    StrDispose( FFileName );
  FFileName := nil;
  FFileNameLength := StrLen( Value );
  FFileName := StrAlloc( succ( FFileNameLength ) );
  StrCopy( FFileName, Value );
end;
{ -------------------------------------------------------------------------- }

{ TAbZipLocalFileHeader implementation ===================================== }
constructor TAbZipLocalFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipLocalFileHeaderSignature;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipLocalFileHeader.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.LoadFromStream( Stream : TStream );
begin
  with Stream do begin
    Read( FSignature, sizeof( FSignature ) );
    Read( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
    Read( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
    Read( FCompressionMethod, sizeof( FCompressionMethod ) );
    Read( FLastModFileTime, sizeof( FLastModFileTime ) );
    Read( FLastModFileDate, sizeof( FLastModFileDate ) );
    Read( FCRC32, sizeof( FCRC32 ) );
    Read( FCompressedSize, sizeof( FCompressedSize ) );
    Read( FUncompressedSize, sizeof( FUncompressedSize ) );
    Read( FFileNameLength, sizeof( FFileNameLength ) );
    Read( FExtraFieldLength, sizeof( FExtraFieldLength ) );

    FFileName := StrAlloc( succ( FFileNameLength ) );
    Read( FFileName^, FFileNameLength );
    FFileName[FFileNameLength] := #0;

    if FExtraFieldLength > 0 then begin
      FExtraField := StrAlloc( succ( FExtraFieldLength ) );
      Read( FExtraField^, FExtraFieldLength );
      FExtraField[FExtraFieldLength] := #0;
    end;
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.SaveToStream( Stream : TStream );
begin
  with Stream do begin
    {write the valid signature from the constant}
    Write( FValidSignature, sizeof( FValidSignature ) );
    Write( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
    Write( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
    Write( FCompressionMethod, sizeof( FCompressionMethod ) );
    Write( FLastModFileTime, sizeof( FLastModFileTime ) );
    Write( FLastModFileDate, sizeof( FLastModFileDate ) );
    Write( FCRC32, sizeof( FCRC32 ) );
    Write( FCompressedSize, sizeof( FCompressedSize ) );
    Write( FUncompressedSize, sizeof( FUncompressedSize ) );
    Write( FFileNameLength, sizeof( FFileNameLength ) );
    Write( FExtraFieldLength, sizeof( FExtraFieldLength ) );

    Write( FFileName^, FFileNameLength );
    Write( FExtraField^, FExtraFieldLength );
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileHeader implementation ================================= }
constructor TAbZipDirectoryFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipCentralDirectoryFileHeaderSignature;
  FileComment := nil;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileHeader.Destroy;
begin
  if Assigned( FFileComment ) then
    StrDispose( FileComment );
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.LoadFromStream( Stream : TStream );
begin
  with Stream do begin
    Read( FSignature, sizeof( FSignature ) );
    Read( FVersionMadeBy, sizeof( FVersionMadeBy ) );
    Read( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
    Read( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
    Read( FCompressionMethod, sizeof( FCompressionMethod ) );
    Read( FLastModFileTime, sizeof( FLastModFileTime ) );
    Read( FLastModFileDate, sizeof( FLastModFileDate ) );
    Read( FCRC32, sizeof( FCRC32 ) );
    Read( FCompressedSize, sizeof( FCompressedSize ) );
    Read( FUncompressedSize, sizeof( FUncompressedSize ) );
    Read( FFileNameLength, sizeof( FFileNameLength ) );
    Read( FExtraFieldLength, sizeof( FExtraFieldLength ) );
    Read( FFileCommentLength, sizeof( FFileCommentLength ) );
    Read( FDiskNumberStart, sizeof( FDiskNumberStart ) );
    Read( FInternalFileAttributes, sizeof( FInternalFileAttributes ) );
    Read( FExternalFileAttributes, sizeof( FExternalFileAttributes ) );
    Read( FRelativeOffset, sizeof( FRelativeOffset ) );

    FFileName := StrAlloc( succ( FFileNameLength ) );
    Read( FFileName^, FFileNameLength );
    FFileName[FFileNameLength] := #0;

    if FExtraFieldLength > 0 then begin
      FExtraField := StrAlloc( succ( FExtraFieldLength ) );
      Read( FExtraField^, FExtraFieldLength );
      FExtraField[FExtraFieldLength] := #0;
    end;

    if FFileCommentLength > 0 then begin
      FFileComment := StrAlloc( succ( FFileCommentLength ) );
      Read( FFileComment^, FFileCommentLength );
      FFileComment[FFileCommentLength] := #0;
    end;
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.SaveToStream( Stream : TStream );
begin
  with Stream do begin
    {write the valid signature from the constant}
    Write( FValidSignature, sizeof( FValidSignature ) );
    Write( FVersionMadeBy, sizeof( FVersionMadeBy ) );
    Write( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
    Write( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
    Write( FCompressionMethod, sizeof( FCompressionMethod ) );
    Write( FLastModFileTime, sizeof( FLastModFileTime ) );
    Write( FLastModFileDate, sizeof( FLastModFileDate ) );
    Write( FCRC32, sizeof( FCRC32 ) );
    Write( FCompressedSize, sizeof( FCompressedSize ) );
    Write( FUncompressedSize, sizeof( FUncompressedSize ) );
    Write( FFileNameLength, sizeof( FFileNameLength ) );
    Write( FExtraFieldLength, sizeof( FExtraFieldLength ) );
    Write( FFileCommentLength, sizeof( FFileCommentLength ) );
    Write( FDiskNumberStart, sizeof( FDiskNumberStart ) );
    Write( FInternalFileAttributes, sizeof( FInternalFileAttributes ) );
    Write( FExternalFileAttributes, sizeof( FExternalFileAttributes ) );
    Write( FRelativeOffset, sizeof( FRelativeOffset ) );
    Write( FFileName^, FFileNameLength );
    Write( FExtraField^, FExtraFieldLength );
    Write( FFileComment^, FFileCommentLength );
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileFooter implementation ================================= }
constructor TAbZipDirectoryFileFooter.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipEndCentralDirectorySignature;
  FZipfileComment := nil;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileFooter.Destroy;
begin
  if Assigned( FZipfileComment ) then
    StrDispose( FZipfileComment );
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipDirectoryFileFooter.GetValid : Boolean;
begin
  Result := (FSignature = FValidSignature);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.LoadFromStream( Stream : TStream );
begin
  with Stream do begin
    Read( FSignature, sizeof( FSignature ) );
    Read( FDiskNumber, sizeof( FDiskNumber ) );
    Read( FStartDiskNumber, sizeof( FStartDiskNumber ) );
    Read( FEntriesOnDisk, sizeof( FEntriesOnDisk ) );
    Read( FTotalEntries, sizeof( FTotalEntries ) );
    Read( FDirectorySize, sizeof( FDirectorySize ) );
    Read( FDirectoryOffset, sizeof( FDirectoryOffset ) );
    Read( FZipfileCommentLength, sizeof( FZipfileCommentLength ) );

    if FZipfileCommentLength > 0 then begin
      FZipfileComment := StrAlloc( succ( FZipfileCommentLength ) );
      Read( FZipfileComment^, FZipfileCommentLength );
      FZipfileComment[FZipfileCommentLength] := #0;
    end;
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.SaveToStream( Stream : TStream );
begin
  with Stream do begin
    Write( FValidSignature, sizeof( FValidSignature ) );
    Write( FDiskNumber, sizeof( FDiskNumber ) );
    Write( FStartDiskNumber, sizeof( FStartDiskNumber ) );
    Write( FEntriesOnDisk, sizeof( FEntriesOnDisk ) );
    Write( FTotalEntries, sizeof( FTotalEntries ) );
    Write( FDirectorySize, sizeof( FDirectorySize ) );
    Write( FDirectoryOffset, sizeof( FDirectoryOffset ) );
    Write( FZipfileCommentLength, sizeof( FZipfileCommentLength ) );
    Write( FZipfileComment^, FZipfileCommentLength );
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipItem implementation ================================================ }
constructor TAbZipItem.Create;
begin
  inherited Create;
  FItemInfo := TAbZipDirectoryFileHeader.Create;
  FDecoder := nil;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipItem.Destroy;
begin
  FItemInfo.Free;
  FItemInfo := nil;
  FDecoder.Free;
  FDecoder := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressedSize : Longint;
begin
  Result := FItemInfo.CompressedSize;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressionMethod : TAbZipCompressionMethod;
begin
  Result := FItemInfo.CompressionMethod;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressionRatio : Double;
begin
  Result := FItemInfo.CompressionRatio;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCRC32 : Longint;
begin
  Result := FItemInfo.CRC32;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDeflationOption : TAbZipDeflationOption;
begin
  Result := FItemInfo.DeflationOption;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDictionarySize : TAbZipDictionarySize;
begin
  Result := FItemInfo.DictionarySize;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetGeneralPurposeBitFlag : Word;
begin
  Result := FItemInfo.GeneralPurposeBitFlag;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDiskNumberStart : Word;
begin
  Result := FItemInfo.DiskNumberStart;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExternalFileAttributes : Longint;
begin
  Result := FItemInfo.ExternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExtraField : string;
begin
  if Assigned( FItemInfo ) and ( FItemInfo.ExtraField <> nil ) then
    Result := StrPas( FItemInfo.ExtraField )
  else
    Result := '';
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetFileComment : string;
begin
  if Assigned( FItemInfo ) and ( FItemInfo.FileComment <> nil ) then
    Result := StrPas( FItemInfo.FileComment )
  else
    Result := '';
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetFileName : string;
var
  Buff : array [0..MAX_PATH] of Char;
begin
  if Assigned( FItemInfo ) and ( FItemInfo.FileName <> nil ) then begin
    StrCopy(Buff, FItemInfo.FileName);
{!!.03 - Added }
{$IFDEF Linux}
 { do nothing to Buff }
{$ELSE}
    if AreFileApisANSI then begin
      OEMToAnsi(Buff, Buff);
    end;
{$ENDIF}
{!!.03 - End Added }
    Result := StrPas(Buff)
  end else
    Result := '';
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetInternalFileAttributes : Word;
begin
  Result := FItemInfo.InternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetIsEncrypted : Boolean;
begin
  Result := FItemInfo.IsEncrypted;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetLastModFileDate : Word;
begin
  Result := FItemInfo.LastModFileDate;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetLastModFileTime : Word;
begin
  Result := FItemInfo.LastModFileTime;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetRelativeOffset : Longint;
begin
  Result := FItemInfo.RelativeOffset;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetShannonFanoTreeCount : Byte;
begin
  Result := FItemInfo.ShannonFanoTreeCount;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetUncompressedSize : Longint;
begin
  Result := FItemInfo.UncompressedSize;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetVersionMadeBy : Word;
begin
  Result := FItemInfo.VersionMadeBy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetVersionNeededToExtract : Word;
begin
  Result := FItemInfo.VersionNeededToExtract;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.LoadFromStream( Stream : TStream );
begin
  FItemInfo.LoadFromStream( Stream );
  if FItemInfo.FileName <> nil then
    FFileName := StrPas( FItemInfo.FileName )
  else
    FFileName := '';
  LastModFileTime := FItemInfo.LastModFileTime;
  LastModFileDate := FItemInfo.LastModFileDate;
  DiskFileName := FileName;
  AbUnfixName( FDiskFileName );
  Action := aaNone;
  Tagged := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveLFHToStream( Stream : TStream );
var
  LFH : TAbZipLocalFileHeader;
  Temp : PChar;
begin
  LFH := TAbZipLocalFileHeader.Create;
  try
    LFH.VersionNeededToExtract := VersionNeededToExtract;
    LFH.GeneralPurposeBitFlag := GeneralPurposeBitFlag;
    LFH.CompressionMethod := CompressionMethod;
    LFH.LastModFileTime := LastModFileTime;
    LFH.LastModFileDate := LastModFileDate;
    LFH.CRC32 := CRC32;
    LFH.CompressedSize := CompressedSize;
    LFH.UncompressedSize := UncompressedSize;
    if Length( FileName ) > 0 then begin
      Temp := StrAlloc( succ( Length( FileName ) ) );
      try
        StrPCopy( Temp, FileName );
        LFH.FileName := Temp;
      finally
        StrDispose( Temp );
      end;
    end;
    if Length( ExtraField ) > 0 then begin
      Temp := StrAlloc( succ( Length( ExtraField ) ) );
      try
        StrPCopy( Temp, ExtraField );
        LFH.ExtraField := Temp;
      finally
        StrDispose( Temp );
      end;
    end;
    LFH.SaveToStream( Stream );
  finally
    LFH.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveCDHToStream( Stream : TStream );
  {-Save a ZipCentralDirectorHeader entry to Stream}
begin
  FItemInfo.SaveToStream( Stream );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveDDToStream( Stream : TStream );
var
  DD : TAbZipDataDescriptor;
begin
  DD := TAbZipDataDescriptor.Create;
  try
    DD.CRC32 := CRC32;
    DD.CompressedSize := CompressedSize;
    DD.UncompressedSize := UncompressedSize;
    DD.SaveToStream( Stream );
  finally
    DD.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCompressedSize( const Value : Longint );
begin
  FItemInfo.CompressedSize:= Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCompressionMethod( Value : TAbZipCompressionMethod );
begin
  FItemInfo.CompressionMethod := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCRC32( const Value : Longint );
begin
  FItemInfo.CRC32 := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetDiskNumberStart( Value : Word );
begin
  FItemInfo.DiskNumberStart := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetExternalFileAttributes( Value : Longint );
begin
  FItemInfo.ExternalFileAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetExtraField( Value : string );
begin
  FItemInfo.ExtraFieldLength := Length( Value );
  if Assigned( FItemInfo.FExtraField ) then
    StrDispose( FItemInfo.FExtraField );
  FItemInfo.FExtraField := nil;
  if Length( Value ) > 0 then begin
    FItemInfo.FExtraField := StrAlloc( succ( FItemInfo.FExtraFieldLength ) );
    StrPCopy( FItemInfo.FExtraField, Value );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileComment( Value : string );
begin
  FItemInfo.FileCommentLength := Length( Value );
  if Assigned( FItemInfo.FFileComment ) then
    StrDispose( FItemInfo.FFileComment );
  FItemInfo.FFileComment := nil;

  if Length( Value ) > 0 then begin
    FItemInfo.FFileComment := StrAlloc( succ( FItemInfo.FFileCommentLength ) );
    StrPCopy( FItemInfo.FFileComment, Value );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileName( Value : string );
begin
  FFileName := Value;
  FItemInfo.FileNameLength := Length( Value );
  if Assigned( FItemInfo.FFileName ) then
    StrDispose( FItemInfo.FFileName );
  FItemInfo.FFileName := nil;
  if Length( Value ) > 0 then begin
    FItemInfo.FFileName := StrAlloc( succ( FItemInfo.FFileNameLength ) );
    StrPCopy( FItemInfo.FFileName, Value );
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetGeneralPurposeBitFlag( Value : Word );
begin
  FItemInfo.GeneralPurposeBitFlag := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetInternalFileAttributes( Value : Word );
begin
  FItemInfo.InternalFileAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetLastModFileDate( const Value : Word );
begin
  FItemInfo.LastModFileDate := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetLastModFileTime( const Value : Word );
begin
  FItemInfo.LastModFileTime := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetRelativeOffset( Value : Longint );
begin
  FItemInfo.RelativeOffset := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetUncompressedSize( const Value : Longint );
begin
  inherited SetUncompressedSize( Value );
  FItemInfo.UncompressedSize:= Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetVersionMadeBy( Value : Word );
begin
  FItemInfo.VersionMadeBy := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetVersionNeededToExtract( Value : Word );
begin
  FItemInfo.VersionNeededToExtract := Value;
end;
{ -------------------------------------------------------------------------- }


{ TAbZipArchive implementation ============================================= }
constructor TAbZipArchive.Create( FileName : string; Mode : Word );
begin
  inherited Create( FileName, Mode );
  FCompressionMethodToUse := smBestMethod;
  FInfo := TAbZipDirectoryFileFooter.Create;
  StoreOptions := StoreOptions + [soStripDrive];
  FDeflationOption := doNormal;
  FPasswordRetries := AbDefPasswordRetries;
  FTempDir := '';
  SpanningThreshold := AbDefZipSpanningThreshold;
  FCurrentDisk := Word(-1); {!!}
end;
{ -------------------------------------------------------------------------- }
constructor TAbZipArchive.CreateFromStream( aStream : TStream;
                                            ArchiveName : string );
begin
  inherited CreateFromStream( aStream, ArchiveName );
  FInfo := TAbZipDirectoryFileFooter.Create;
  FPasswordRetries := AbDefPasswordRetries;                            
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipArchive.Destroy;
begin
  FInfo.Free;
  FInfo := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.CreateItem( const FileSpec : string ): TAbArchiveItem;
var
  Buff : array [0..MAX_PATH] of Char;
begin
  Result := TAbZipItem.Create;
  with TAbZipItem( Result ) do begin
    CompressionMethod := cmDeflated;
    GeneralPurposeBitFlag := 0;
    CompressedSize := 0;
    CRC32 := 0;
    ExtraField := '';
    StrPCopy(Buff, ExpandFileName(FileSpec));
{!!.03 - Added }
{$IFDEF Linux}
 { do nothing to Buff }
{$ELSE}
    if AreFileApisANSI then begin
      AnsiToOEM(Buff, Buff);
    end;
{$ENDIF}
{!!.03 - End Added }
    DiskFileName := StrPas(Buff);
    StrPCopy(Buff, FixName(FileSpec));
{!!.03 - Added }
{$IFDEF Linux}
 { do nothing to Buff }
{$ELSE}
    if AreFileApisANSI then begin
      AnsiToOEM(Buff, Buff);
    end;
{$ENDIF}
{!!.03 - End Added }
    FileName := StrPas(Buff);
    RelativeOffset := 0;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractHelper(Index : Integer; NewName : string);
begin
  if Assigned(FExtractHelper) then
    FExtractHelper(Self, ItemList[Index], NewName)
  else
    raise EAbZipNoExtraction.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractToStreamHelper(Index : Integer;
                                                aStream : TStream);
begin
  if Assigned(FExtractToStreamHelper) then
    FExtractToStreamHelper(Self, ItemList[Index], aStream)
  else
    raise EAbZipNoExtraction.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoTestHelper(Index : Integer);
begin
  if Assigned(FTestHelper) then
    FTestHelper(Self, ItemList[Index])
  else
    raise EAbZipNoExtraction.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoInsertHelper(Index : Integer; OutStream : TStream);
begin
  if Assigned(FInsertHelper) then
    FInsertHelper(Self, ItemList[Index], OutStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoInsertFromStreamHelper(Index : Integer;
  OutStream : TStream);
begin
  if Assigned(FInsertFromStreamHelper) then
    FInsertFromStreamHelper(Self, ItemList[Index], OutStream, InStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestLastDisk( var Abort : Boolean );
var
  pMessage : string;
  pCaption : string;
begin
  Abort := False;
  if Assigned( FOnRequestLastDisk ) then
    FOnRequestLastDisk( Self, Abort )
  else begin
    pMessage := AbStrRes(AbLastDiskRequest);
    pCaption := AbStrRes(AbDiskRequest);
{$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox( 0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL ) = IDCANCEL;
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt}
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF}
{$ENDIF}
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNthDisk( DiskNumber : Byte;
                                          var Abort : Boolean );
var
  pMessage : string;
  pCaption : string;
  FMessage : string;
begin
  Abort := False;
  if Assigned( FOnRequestNthDisk ) then
    FOnRequestNthDisk( Self, DiskNumber, Abort )
  else begin
    pMessage := AbStrRes(AbDiskNumRequest);
    FMessage := Format(pMessage, [DiskNumber] );
    pMessage := FMessage;
    pCaption := AbStrRes(AbDiskRequest);
{$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox( 0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL ) = IDCANCEL;                      
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt }
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF }
{$ENDIF}
  end;

//  if not Abort and (FStream is TAbSpanStream) then                       {!!.01}
//    TAbSpanStream(FStream).SpanNumber := DiskNumber;                     {!!.01}

end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestBlankDisk( var Abort : Boolean );
var
  pMessage : string;
  pCaption : string;
begin
  Abort := False;
  FSpanned := True;

  if Assigned( FOnRequestBlankDisk ) then
    FOnRequestBlankDisk( Self, Abort )
  else begin
    pMessage := AbStrRes(AbBlankDisk);
    pCaption := AbStrRes(AbDiskRequest);
{$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox( 0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL ) = IDCANCEL;
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt}
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF}
{$ENDIF}
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNthImage(ImageNumber : Integer;
  var Stream : TStream; var Abort : Boolean);
var
  ImageName : string;
  i : Integer;
  Found : Boolean;
  MediaType : TAbMediaType;
begin
  Abort := False;
  ImageName := FArchiveName;

  {--spanned disk set--}
  MediaType := mtLocal;
  if FDriveIsRemovable then begin
{$IFDEF LINUX}
    raise EAbException.Create('Floppy Spanning not supported on Linux'); {!!.01}
{$ENDIF}
    MediaType := mtRemoveable;

    if (ImageNumber > AbLastDisk) then
      DoRequestNthDisk(Succ(ImageNumber), Abort);
    if Abort then
      raise EAbUserAbort.Create;

    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);

    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;

{!!.04 - changed}
//    if FindCentralDirectoryTail(Stream) = -1 {not found} then
    if (CurrentDisk = Word(-1))
      and (FindCentralDirectoryTail(Stream) = -1) {not found} then
{!!.04 - changed end}
      DoRequestLastDisk(Abort);
    if Abort then
      raise EAbUserAbort.Create;

    Exit;
  end;

  {--spanned image set--}
  { first check if the current image contains the CDT }                  {!!.03}
  if FindCentralDirectoryTail(Stream) > -1 {not found} then begin        {!!.03}
    Exit;                                                                {!!.03}
  end;                                                                   {!!.03}

  {if OnRequestImage assigned, then fire event}
  if Assigned(FOnRequestImage) then begin

    FOnRequestImage(Self, ImageNumber, ImageName, Abort);
    if Abort then
      raise EAbUserAbort.Create;
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
//    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Exit;
  end;

  {if not last image requested, then simply auto-generate image name}
  if (ImageNumber > AbLastImage) then begin
    if (ImageNumber = 0) then
      ImageName := FArchiveName
    else
      AbIncFilename(ImageName, ImageNumber);
    if not FileExists(ImageName) then
      raise EAbFileNotFound.Create;
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
//    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Exit;
  end;




  {search for last image, assuming images were auto-generated}
  Stream.Free;                                                         {!!.04}
  FAutoGen := True;                                                  {!!.02}
  for i := 1 to 99 do begin
    AbIncFilename(ImageName, i);
    if not FileExists(ImageName) then
      raise EAbFileNotFound.Create;
//    Stream.Free;                                                     {!!.04}
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
//    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Found := (FindCentralDirectoryTail(Stream) > -1);
    if Found then
      Break
    else
      Stream.Free;
  end;

  CurrentDisk := ImageNumber;                                            {!!.01}
  if not Found then
    raise EAbFileNotFound.Create
end;
{ -------------------------------------------------------------------------- }
{!!.01 -- Added}
procedure TAbZipArchive.DoRequestImage(Mode : TAbSpanMode; ImageNumber : Integer;
  var ImageName : string ; var Abort : Boolean);
var
  pMessage : string;
  pCaption : string;
begin
  if Assigned(FOnRequestImage) then
    FOnRequestImage(self, ImageNumber, ImageName, Abort)
  else if FAutoGen then                                              {!!.02}
    AbIncFilename(ImageName, ImageNumber)                            {!!.02}
  else if Mode = smReading then begin

    pMessage := Format(AbStrRes(AbImageNumRequest), [ImageNumber]);
    pCaption := AbStrRes(AbImageRequest);
{$IFDEF MSWINDOWS}
{!!.04}
//    Abort := not InputQuery(pCaption, pMessage, ImageName);
    Abort := Windows.MessageBox( 0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL ) = IDCANCEL;
{!!.04}

{$ENDIF}
{$IFDEF LINUX}
{$IFDEF NoQt}
    WriteLn(pMessage);
{$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
{$ENDIF}
{$ENDIF}
  end;
end;
{ -------------------------------------------------------------------------- }
{!!.01 -- End Added}
procedure TAbZipArchive.DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
  var ImageName : string; var Abort : Boolean);
var
  SpanStrm : TAbSpanStream;
begin
  SpanStrm := Sender as TAbSpanStream;
  if SpanStrm.SpanMode = smWriting then begin
    if SpanStrm.MediaType = mtRemoveable then begin
      DoRequestBlankDisk(Abort);
      AbWriteVolumeLabel(Format('PKBACK# %3.3d',                         {!!.01}
        [Pred(ImageNumber)]), AbDrive(FArchiveName));                    {!!.01}
    end
    else begin
      DoRequestImage(SpanStrm.SpanMode, ImageNumber, ImageName, Abort)   {!!.01}
    end;
  end
  else begin  { SpanMode = smReading }
    if SpanStrm.MediaType = mtRemoveable then begin
      DoRequestNthDisk(ImageNumber, Abort)
    end
    else begin
      DoRequestImage(SpanStrm.SpanMode, ImageNumber, ImageName, Abort);  {!!.01}
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNextImage(ImageNumber : Integer;
  var Stream : TStream; var Abort : Boolean);
var
  ImageName : string;
  MediaType : TAbMediaType;
begin
  Abort := False;
  Stream.Free;
  Stream := nil;
  ImageName := FArchiveName;

  {if drive is removable then request a blank disk}
  if FDriveIsRemovable then begin
    DoRequestBlankDisk(Abort);
    MediaType := mtRemoveable;

  {otherwise we need the next image file name}
  end else begin
    MediaType := mtLocal;
    if Assigned(FOnRequestImage) then
      FOnRequestImage(Self, ImageNumber, ImageName, Abort)
    else {auto-generate the name}
      AbIncFilename(ImageName, ImageNumber);
  end;
  if Abort then
    raise EAbUserAbort.Create
  else begin
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmCreate, MediaType, SpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
//    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.ExtractItemAt(Index : Integer; const NewName : string);
begin
  DoExtractHelper(Index, NewName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.ExtractItemToStreamAt(Index : Integer;
                                              aStream : TStream);
begin
  DoExtractToStreamHelper(Index, aStream);
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.FindCDTail : Longint;
begin
  Result := FindCentralDirectoryTail( FStream );
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.FixName( Value : string ) : string;
  {-changes backslashes to forward slashes}
var
  i : SmallInt;
begin
  {$IFDEF MSWINDOWS}
  if DOSMode then begin
    {Add the base directory to the filename before converting }
    {the file spec to the short filespec format. }
    if BaseDirectory <> '' then begin
      {Does the filename contain a drive or a leading backslash? }
      if not ((Pos(':', Value) = 2) or (Pos(AbPathDelim, Value) = 1)) then
        {If not, add the BaseDirectory to the filename.}
        Value := AbAddBackSlash(BaseDirectory) + Value;                {!!.04}
    end;
    Value := AbGetShortFileSpec( Value );
  end;
  {$ENDIF MSWINDOWS}

  {Zip files Always strip the drive path}
  StoreOptions := StoreOptions + [soStripDrive];

  {strip drive stuff}
  if soStripDrive in StoreOptions then
    AbStripDrive( Value );

  {check for a leading backslash}
  if Value[1] = AbPathDelim then
    System.Delete( Value, 1, 1 );

  if soStripPath in StoreOptions then begin
    Value := ExtractFileName( Value );
  end;

  if soRemoveDots in StoreOptions then
    AbStripDots( Value );

  for i := 1 to Length( Value ) do
    if Value[i] = '\' then
      Value[i] := '/';
  Result := Value;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetItem( Index : Integer ) : TAbZipItem;
begin
  Result := TAbZipItem(FItemList.Items[Index]);
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetZipFileComment : string;
begin
  Result := StrPas( FInfo.ZipFileComment );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.LoadArchive;
var
  Abort : Boolean;
  TailPosition : Longint;
  Item : TAbZipItem;
  i : Integer;
  Progress : Byte;
  FileSignature : DWord;                                             {!!.02}
  IsZip : Boolean;
  Lowest : Longint;
begin
  Lowest := MaxLongint;
  Abort := False;
  FAutoGen := False;                                                 {!!.02}
  if FStream.Size = 0 then
    Exit;

  {Get signature info}
  FStream.Position := 0;
  FStream.Read( FileSignature, sizeof( FileSignature ) );

  IsZip := (FileSignature and $0000FFFF) = Ab_GeneralZipSignature;   {!!.02}

{$IFDEF MSWINDOWS}
  IsExecutable := FileSignature = Ab_WindowsExeSignature;
{$ENDIF}
{$IFDEF LINUX}
  if FileSignature = Ab_LinuxExeSigWord1 then begin
    FStream.Read( FileSignature, sizeof( FileSignature ) );
    IsExecutable := FileSignature = Ab_LinuxExeSigWord2;
  end;
{$ENDIF}


  {Check for spanning}
  if (FStream is TAbSpanStream) then
    FDriveIsRemovable := AbDriveIsRemovable(ArchiveName);


  { try to locate central directory tail }
  if (FileSignature = DWord(Ab_ZipSpannedSetSignature)) or           {!!.02}
     (FileSignature = DWord(Ab_ZipPossiblySpannedSignature)) then    {!!.02}
    TailPosition := -1                                               {!!.02}
  else                                                               {!!.02}
    TailPosition := FindCDTail;

  if (TailPosition = -1) then begin { not found so need different image }
    if IsZip then begin
      while (not Abort) and (TailPosition = -1) do begin
        DoRequestNthImage(AbLastDisk, FStream, Abort);
        TailPosition := FindCDTail;
      end;
      if Abort then
        Exit
      else
        FSpanned := True;
    end else begin
      FStatus := asInvalid;
      raise EAbZipInvalid.Create;
    end;
  end;

  { load the ZipDirectoryFileFooter }
  FInfo.LoadFromStream(FStream);
  CurrentDisk := FInfo.DiskNumber;
  { set spanning flag if current disk is not the first one }
  if (FInfo.DiskNumber > 0) then
    FSpanned := True;

  { build Items list from central directory records }
  i := 0;
  FStream.Seek(FInfo.DirectoryOffset, soFromBeginning);
  {  while not((FStream.Position = TailPosition) and }                   {!!.01}
  {    (CurrentDisk = FInfo.DiskNumber)) do begin }                      {!!.01}
  while (FStream.Position < TailPosition) or                             {!!.01}
    (CurrentDisk <> FInfo.DiskNumber) do begin                           {!!.01}
    if Spanned then begin
      {see if we've got to switch disks}
      if (CurrentDisk < FInfo.DiskNumber) and (FInfo.EntriesOnDisk = i) or
        (FStream.Size = FStream.Position) then begin
        CurrentDisk := CurrentDisk + 1;
        DoRequestNthImage(CurrentDisk, FStream, Abort);
        if Abort then
          Exit;
      end;
    end;

    { create new Item }
    Item := TAbZipItem.Create;
    Item.LoadFromStream(FStream);
    if IsExecutable then
      if (Item.RelativeOffset < Lowest) then
        Lowest := Item.RelativeOffset;
    FItemList.Add(Item);
    ItemList[pred(Count)].Action := aaNone;
    Inc(i);
    Progress := (i * 100) div FInfo.TotalEntries;
    DoArchiveProgress( Progress, Abort );
    if Abort then begin
      FStatus := asInvalid;
      raise EAbUserAbort.Create;
    end;
  end;

  DoArchiveProgress(100, Abort);
  if IsExecutable then
    FStubSize := Lowest;
  FIsDirty := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.PutItem( Index : Integer; Value : TAbZipItem );
begin
  FItemList.Items[Index] := Value;
end;
{ -------------------------------------------------------------------------- }

type
  TAbItemDataRec = record
    StreamOffset : LongInt;
  end;

{ -------------------------------------------------------------------------- }
const
  AB_SIZE_LOCAL_HEADER = 30;
  AB_SIZE_DATA_DESC    = 16;
  AB_SIZE_CD_HEADER    = 46;
  AB_SIZE_CD_TAIL      = 22;

procedure TAbZipArchive.SaveArchive;
var
  CompressedDataStream  : TAbVirtualMemoryStream;
//  CDStream : TAbVirtualMemoryStream;
  ArchiveSize : LongInt;

{ Zip file save logic, taking unsplittable structures into account}

procedure IncrementSize(Size : LongInt; CurrItem : TAbZipItem);
begin
  { track size of Archive }
  ArchiveSize := ArchiveSize +
    Size                 + { size of compressed data }
    AB_SIZE_LOCAL_HEADER + { local header size }
    AB_SIZE_CD_HEADER +
    (2 * Length(CurrItem.FileName)) +
    (2 * Length(CurrItem.ExtraField));

  if (CurrItem.CompressionMethod = cmDeflated) and
    ((CurrItem.GeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0)
  then
    ArchiveSize := ArchiveSize + AB_SIZE_DATA_DESC;
end;

procedure BuildData;
{ build zip structures and compress the data }
var
  CurrItem : TAbZipItem;
  i : Integer;
  Size : LongInt;
  TempStream : TAbVirtualMemoryStream;
begin
  ArchiveSize := 0;

  { for each item in original item list }
  for i := 0 to pred( Count ) do begin
    CurrItem := (ItemList[i] as TAbZipItem);
    FCurrentItem := ItemList[i];              { archive needs to know this for event handling }

    { handle item according to item disposition }
    case CurrItem.Action of
      aaNone, aaMove: begin
      {just copy the compressed file data to temporary stream }
        { find the current item's data }
        FStream.Position := CurrItem.RelativeOffset;

        { save compressed data size }
        Size := CurrItem.CompressedSize;
        CompressedDataStream.Write(Size, SizeOf(LongInt));

        { save compressed data }
        CompressedDataStream.CopyFrom(FStream, Size);

        { track the size }
        IncrementSize(Size, CurrItem);
      end;

      aaDelete: begin
        {doing nothing omits file from new stream}
      end;

      aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
        { build zip structures for item }
          // build local header
          // build data descriptor
          // build CD header

        { compress data to temp stream }
        TempStream := TAbVirtualMemoryStream.Create;
        try
          TempStream.SwapFileDirectory := CompressedDataStream.SwapFileDirectory;

          if (CurrItem.Action = aaStreamAdd) then
            DoInsertFromStreamHelper(i, TempStream)
          else
            DoInsertHelper(i, TempStream);

          if CurrItem.CompressedSize > 0 then begin
            TempStream.Seek(0, soFromBeginning);
            { save compressed data offset }
            Size := TempStream.Size;
            CompressedDataStream.Write(Size, SizeOf(LongInt));

            { save compressed data }
            CompressedDataStream.CopyFrom(TempStream, Size);

            IncrementSize(Size, CurrItem);
          end;

          { update zip structures for item }
          // fixup local header
          // fixup data descriptor
          // fixup CD header

        finally
          TempStream.Free;
        end;
      end;

    end; { case }

    { !!! progress event }

  end; { for i }

  CompressedDataStream.Seek(0, soFromBeginning);
end;

function Spanning : Boolean;
begin
//  Result := False; {!!!}
// if writing to removeable media or spanning threshold is set
// and there's no room for the archive in the current span size,
// then we're spanning
  Result :=
  ((FSpanningThreshold > 0) and (ArchiveSize > FSpanningThreshold))
    or (AbDriveIsRemovable(FArchiveName) and ((FStream as TAbSpanStream).FreeSpace < ArchiveSize));
end;

procedure GetNextMedia;
begin
  { get next media }
  TAbSpanStream(FStream).GotoNext;

  { set volume label }
  if AbDriveIsRemovable(ArchiveName) then
    AbSetSpanVolumeLabel(AbDrive(FArchiveName), Succ((FStream as TAbSpanStream).SpanNumber));
end;

procedure SaveData;
{ save compressed data }
var
  CurrItem : TAbZipItem;
  i : Integer;
  Size : LongInt;
  WorkStream : TMemoryStream;
  MediaType  : TAbMediaType;
begin
  WorkStream := TMemoryStream.Create;
  try

  { need new stream for writing }
  MediaType := TAbSpanStream(FStream).MediaType;
  FStream.Free;
  FStream := TAbSpanstream.Create(ArchiveName, fmOpenWrite or fmShareDenyWrite,
    MediaType, FSpanningThreshold);

  if Spanning then begin
    { write spanned signature }
    FStream.Write(Ab_ZipSpannedSetSignature, SizeOf(Ab_ZipSpannedSetSignature));

    { set volume label }
    if AbDriveIsRemovable(ArchiveName) then
      AbSetSpanVolumeLabel(AbDrive(FArchiveName), Succ((FStream as TAbSpanStream).SpanNumber));
  end; { if Spanning }

  { for each item in saved item list }
  for i := 0 to pred(Count) do begin
    CurrItem := (ItemList[i] as TAbZipItem);
    FCurrentItem := ItemList[i];              { archive needs to know this for event handling }

    if not (CurrItem.Action = aaDelete) then begin

      { if not room on current media for item's local header }
      if (FStream as TAbSpanStream).FreeSpace < AB_SIZE_LOCAL_HEADER then
        GetNextMedia;

      { fixup CD header } {!!!}
      CurrItem.DiskNumberStart := (FStream as TAbSpanStream).SpanNumber;
      CurrItem.RelativeOffset := FStream.Position;

      { write local header }{ intermediate stream needed for speed }
      WorkStream.Size := 0;
      CurrItem.SaveLFHToStream(WorkStream);
      WorkStream.Seek(0, soFromBeginning);
      FStream.CopyFrom(WorkStream, WorkStream.Size);

      { write compressed data }
      if CurrItem.CompressedSize > 0 then begin
        CompressedDataStream.Read(Size, SizeOf(LongInt));
        if Size > 0 then
          FStream.CopyFrom(CompressedDataStream, Size);
      end;

      { write data descriptor }{ intermediate stream needed for speed }
      if (CurrItem.CompressionMethod = cmDeflated) and
        ((CurrItem.GeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0) { data descriptor flag set }
      then begin
        WorkStream.Size := 0;
        CurrItem.SaveDDToStream(WorkStream);
        WorkStream.Seek(0, soFromBeginning);
        FStream.CopyFrom(WorkStream, WorkStream.Size);
      end;

    end; { if }

    { !!! progress event }

  end; { for i }

  finally
    WorkStream.Free;
  end;
end; { SaveData }

procedure SaveCDT;
{ save Central Directory }
var
  CurrItem : TAbZipItem;
  i : Integer;
  EntryCt : Integer;
  DirSize : LongInt;
  WorkStream : TMemoryStream;
begin
  WorkStream := TMemoryStream.Create;
  try

  { offset of Start of Central Directory }
  FInfo.FDirectoryOffset := FStream.Position;

  {!!!}
  FInfo.StartDiskNumber := (FStream as TAbSpanStream).SpanNumber;
  DirSize := 0;
  FInfo.TotalEntries := Count;
  EntryCt := 0;

  { for each item in saved item list }
  for i := 0 to pred( Count ) do begin
    CurrItem := (ItemList[i] as TAbZipItem);
    FCurrentItem := ItemList[i];              { archive needs to know this for event handling }

    if not (CurrItem.Action = aaDelete) then begin
      { if not room on current media for item's CD header }
      if (FStream as TAbSpanStream).FreeSpace < AB_SIZE_CD_HEADER then begin
        GetNextMedia;
        EntryCt := 0;
      end;

      { write CD header and increment CD size }{ intermediate stream needed for speed }
      WorkStream.Size := 0;
      CurrItem.SaveCDHToStream(WorkStream);
      WorkStream.Seek(0, soFromBeginning);
      Inc(DirSize, WorkStream.Size);
      FStream.CopyFrom(WorkStream, WorkStream.Size);

      { count entry }
      Inc(EntryCt);
    end; { if }

    { !!! progress event }
  end; { for i }

  { build CD Tail }
  FInfo.EntriesOnDisk := EntryCt;
  FInfo.DirectorySize := DirSize; //FStream.Position - FInfo.FDirectoryOffset; {!!!}

  { if not room on current media for CD tail }
  if (FStream as TAbSpanStream).FreeSpace < AB_SIZE_CD_TAIL then
    GetNextMedia;

  FInfo.DiskNumber := (FStream as TAbSpanStream).SpanNumber { + 1};
  { write CD tail }{ intermediate stream needed for speed }
  WorkStream.Size := 0;
  FInfo.SaveToStream(WorkStream);
  WorkStream.Seek(0, soFromBeginning);
  FStream.CopyFrom(WorkStream, WorkStream.Size);

  finally
    WorkStream.Free;
  end;
end; { SaveCDT }

procedure Initialize;
begin
  FStream.Position := 0;

  CompressedDataStream := TAbVirtualMemoryStream.Create;
  CompressedDataStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

//  CDStream := TAbVirtualMemoryStream.Create;
//  CDStream.SwapFileDirectory := CompressedDataStream.SwapFileDirectory;
end;

procedure Cleanup;
begin
  CompressedDataStream.Free;
end;

procedure CopyStub;
begin
  {copy the executable stub over to the output}
  if IsExecutable then
//    NewStream.CopyFrom( FStream, StubSize )
end;

procedure UpdateItemsList;
var
  i : Integer;
begin
  {update Items list}
  for i := pred( Count ) downto 0 do begin
    if FItemList[i].Action = aaDelete then
      FItemList.Delete( i )
    else if FItemList[i].Action <> aaFailed then
      FItemList[i].Action := aaNone;
  end;
end;

function Validate : Boolean;
var
  i : Integer;
begin
  {shouldn't be trying to overwrite an existing spanned archive}
  Result := True;
  if Spanned then begin
    for i := 0 to Pred(Count) do
      if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    FIsDirty := False;
    Result := False;
  end;
end;

begin { SaveArchive }
  if Validate then begin
    Initialize;
    try
      CopyStub;
      BuildData;
      SaveData;
      SaveCDT;
      UpdateItemsList;
    finally
      Cleanup;
    end;
  end else
    raise EAbZipSpanOverwrite.Create;
end;  { SaveArchive }



{ -------------------------------------------------------------------------- }



procedure TAbZipArchive.SaveArchive2;
  {builds a new archive and copies it to FStream}
var
  Abort              : Boolean;
  {BlockSize         : Longint;}                                         {!!.01}
  {TotalBytesWritten : Longint;}                                         {!!.01}
  CDHStream          : TMemoryStream;
  HasDataDescriptor  : Boolean;
  i                  : Integer;
  LFH                : TAbZipLocalFileHeader;
  NewStream          : TAbVirtualMemoryStream;
  WorkingStream      : TAbVirtualMemoryStream;
  CurrItem           : TAbZipItem;
  SCurrentImage      : Word;
  SCurrentOffset     : Longint;
  SSpanningThreshold : Longint;
  CanSpan            : Boolean;
  MediaType          : TAbMediaType;                                     {!!.01}
begin
  {shouldn't be trying to overwrite an existing spanned archive}
  if Spanned then begin
    for i := 0 to Pred(Count) do
      if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    FIsDirty := False;
    raise EAbZipSpanOverwrite.Create;
  end;

{!!.01 -- Modified to take better advantage of TAbSpanStream features }
  if FStream is TAbSpanStream then begin
    if TAbSpanStream(FStream).SpanMode = smWriting then begin
      MediaType := TAbSpanStream(FStream).MediaType;
      FStream.Free;
      FStream := TAbSpanstream.Create(ArchiveName, fmOpenRead or fmShareDenyWrite,
        MediaType, FSpanningThreshold);
      TAbSpanStream(FStream).OnRequestImage := DoSpanningMediaRequest;
      TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
    end;
  end;
{!!.01 -- End Modified (see more below)}

  {can span if new archive and drive is removable, or
   can span if SpanningThreshold > 0 and drive is fixed}
  FDriveIsRemovable := AbDriveIsRemovable(ArchiveName);
  SSpanningThreshold := AbGetDriveFreeSpace(ArchiveName);
  if FDriveIsRemovable then
    CanSpan := (FStream.Size = 0)
  else begin
    CanSpan := (SpanningThreshold > 0);
    if CanSpan then
      SSpanningThreshold := SpanningThreshold
  end;
  if (SSpanningThreshold <= 0) then
    SSpanningThreshold := MaxLongint;

  {init new zip archive stream}
  NewStream := TAbVirtualMemoryStream.Create;
  try {NewStream}
    NewStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

    {copy the executable stub over to the output}
    if IsExecutable then
      NewStream.CopyFrom( FStream, StubSize )
    else if CanSpan then
      NewStream.Write(Ab_ZipPossiblySpannedSignature,
        SizeOf(Ab_ZipPossiblySpannedSignature));

    {init central directory stream}
    CDHStream := TMemoryStream.Create;
    try {CDHStream}
      FInfo.EntriesOnDisk := 0;
      FInfo.TotalEntries := 0;
      SCurrentImage := 0;
      SCurrentOffset := NewStream.Position;

      FStream.Position := 0;                                             {!!.01}

      {build new zip archive from existing archive}
      for i := 0 to pred( Count ) do begin
        CurrItem := (ItemList[i] as TAbZipItem);
        FCurrentItem := ItemList[i];

        case CurrItem.Action of
          aaNone, aaMove: begin
            {just copy the file to new stream, and add CDH record}
            FStream.Position := CurrItem.RelativeOffset;
            CurrItem.DiskNumberStart := SCurrentImage;
            CurrItem.RelativeOffset := SCurrentOffset;
            {toss old local file header}
            LFH := TAbZipLocalFileHeader.Create;
            try {LFH}
              LFH.LoadFromStream( FStream );
            finally {LFH}
              LFH.Free;
            end; {LFH}
            {write out new local file header and append compressed data}

            CurrItem.SaveLFHToStream( NewStream );
            if (CurrItem.CompressedSize > 0) then
              NewStream.CopyFrom(FStream, CurrItem.CompressedSize);
            FInfo.EntriesOnDisk := FInfo.EntriesOnDisk + 1;
            FInfo.TotalEntries := FInfo.TotalEntries + 1;
            CurrItem.SaveCDHToStream( CDHStream );
          end;

          aaDelete: begin
            {doing nothing omits file from new stream}
          end;

          aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
            {compress the file, add it new stream, and add CDH record}
            CurrItem.DiskNumberStart := SCurrentImage;
            CurrItem.RelativeOffset := SCurrentOffset;
            WorkingStream := TAbVirtualMemoryStream.Create;
            try {WorkingStream}

              WorkingStream.SwapFileDirectory := NewStream.SwapFileDirectory;
              if (CurrItem.Action = aaStreamAdd) then
                DoInsertFromStreamHelper(i, WorkingStream)
              else
                DoInsertHelper(i, WorkingStream);
              CurrItem.SaveLFHToStream(NewStream);
              NewStream.CopyFrom(WorkingStream, 0);
              if CurrItem.IsEncrypted then                               {!!.01}
                CurrItem.SaveDDToStream(NewStream);                      {!!.01}

              FInfo.EntriesOnDisk := FInfo.EntriesOnDisk + 1;
              FInfo.TotalEntries := FInfo.TotalEntries + 1;
              CurrItem.SaveCDHToStream(CDHStream);
            except
              CurrItem.Action := aaDelete;
              DoProcessItemFailure(CurrItem, ptAdd, ecFileOpenError, 0);
            end;
            WorkingStream.Free;
          end;
        end; { case }

        {Now add the data descriptor record to new stream}
        HasDataDescriptor := (CurrItem.CompressionMethod = cmDeflated) and
          ((CurrItem.GeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0);
        if (CurrItem.Action <> aaDelete) and HasDataDescriptor then
          CurrItem.SaveDDToStream(NewStream);
        DoArchiveProgress(AbPercentage(9 * succ( i ), 10 * Count), Abort);
        if Abort then
          raise EabUserAbort.Create;
        if (SSpanningThreshold > 0) then 
          SCurrentImage := NewStream.Position div SSpanningThreshold;
        SCurrentOffset := NewStream.Position - (SCurrentImage * SSpanningThreshold);
      end;

      {append the central directory}
      FInfo.StartDiskNumber := SCurrentImage;
      FInfo.DirectoryOffset := SCurrentOffset;
      CDHStream.Position := 0;
      NewStream.CopyFrom( CDHStream, CDHStream.Size );

      { we're not sure if the CDH may span disks }
      if (SpanningThreshold > 0) then
        SCurrentImage := NewStream.Position div SSpanningThreshold;

      {append the central directory footer}
      FInfo.DirectorySize := CDHStream.Size;
      FInfo.DiskNumber := SCurrentImage;
      FInfo.SaveToStream(NewStream);
    finally {CDHStream}
      CDHStream.Free;
    end; {CDHStream}

    {check for spanning}
    FSpanned := (SCurrentImage > 0) and                                  {!!.03}
      (NewStream.Size > SSpanningThreshold) and CanSpan;                 {!!.03}
    if Spanned then begin
      NewStream.Position := 0;
      NewStream.Write(Ab_ZipSpannedSetSignature, SizeOf(Ab_ZipSpannedSetSignature));
    end;

    {copy new stream to FStream}
    NewStream.Position := 0;
    if (FStream is TMemoryStream) then
      TMemoryStream(FStream).LoadFromStream(NewStream)
    else begin
      { need new stream to write }
      FStream.Free;

{!!.01 -- Modified to take better advantage of TAbSpanStream features }
      if Spanned and AbDriveIsRemovable(FArchiveName) then begin         {!!.03}
        {reset image number }
        SCurrentImage := 0;
        AbWriteVolumeLabel(Format('PKBACK# %3.3d',
          [Succ(SCurrentImage)]), AbDrive(FArchiveName));
        FStream := TAbSpanStream.Create(FArchiveName,
          fmOpenWrite or fmShareDenyWrite, mtRemoveable,
           FSpanningThreshold)
        {!!! write spanning signature here?}
      end
      else
        FStream := TAbSpanStream.Create(FArchiveName,
          fmOpenWrite or fmShareDenyWrite, mtLocal,
            FSpanningThreshold);

      try
        TAbSpanStream(FStream).OnRequestImage :=
          DoSpanningMediaRequest;
        TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
//        TAbSpanStream(FStream).SpanNumber := SCurrentImage;              {!!.01}

        FStream.Size := 0;
        { copy temporary archive to the stream }
        if FStream.Position <> 0 then
          FStream.Position := 0;
        TAbSpanStream(FStream).ArchiveTotalSize := NewStream.Size;     {!!.04}
        FStream.CopyFrom(NewStream, NewStream.Size);
      except
        raise EAbBadStream.Create;
      end;
{!!.01 -- End Modified }
    end;

    {update Items list}
    for i := pred( Count ) downto 0 do begin
      if FItemList[i].Action = aaDelete then
        FItemList.Delete( i )
      else if FItemList[i].Action <> aaFailed then
        FItemList[i].Action := aaNone;
    end;

    DoArchiveSaveProgress( 100, Abort );                               {!!.04}
    DoArchiveProgress( 100, Abort );
  finally {NewStream}
    NewStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.SetZipFileComment( Value : string );
begin
  FInfo.ZipFileCommentLength := Length( Value );
  if Assigned( FInfo.FZipFileComment ) then
    StrDispose( FInfo.FZipFileComment );
  FInfo.FZipFileComment := nil;
  if Length( Value ) > 0 then begin
    FInfo.FZipFileComment := StrAlloc( succ( FInfo.FZipFileCommentLength ) );
    StrPCopy( FInfo.FZipFileComment, Value );
    FIsDirty := True;
  end                                                                    {!!.02}
  else begin  { if Value = '' then clear the ZIP Comment }               {!!.02}
    FInfo.FZipFileCommentLength := 0;                                    {!!.02}
    FInfo.FZipFileComment := nil;                                        {!!.02}
    FIsDirty := True;                                                    {!!.02}
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.TestItemAt(Index : Integer);
begin
  DoTestHelper(Index);
end;
{ -------------------------------------------------------------------------- }
procedure MakeSelfExtracting( StubStream, ZipStream,
                              SelfExtractingStream : TStream );
  {-takes an executable stub, and a .zip format stream, and creates
   a SelfExtracting stream.  The stub should create a TAbZipArchive
   passing itself as the file, using a read-only open mode.  It should
   then perform operations as needed - like ExtractFiles( '*.*' ).
   This routine updates the RelativeOffset of each item in the archive}
var
  DirectoryStart : Longint;
  FileSignature : Word;
  StubSize : Longint;
  TailPosition : Longint;
  ZDFF : TAbZipDirectoryFileFooter;
  ZipItem : TAbZipItem;
  IsWinExe, IsLinuxExe : Boolean;                                        {!!.01}
begin
  {check file type of stub stream}
  StubStream.Position := 0;
  StubStream.Read(FileSignature, SizeOf(FileSignature));

{!!.01 -- re-written executable Type Detection to allow use of non-native stubs }
  IsLinuxExe := False;
  IsWinExe := FileSignature = Ab_WindowsExeSignature;
  if not IsWinExe then begin
    IsLinuxExe := FileSignature = Ab_LinuxExeSigWord1; { check 1st sig }
    if IsLinuxExe then begin
      StubStream.Read(FileSignature, SizeOf(FileSignature)); { check 2nd sig }
      IsLinuxExe := FileSignature = Ab_LinuxExeSigWord2;
    end;
  end;

  if not (IsWinExe or IsLinuxExe) then
    raise EAbZipInvalidStub.Create;
{!!.01 -- End Re-written}

  StubStream.Position := 0;
  StubSize := StubStream.Size;

  ZipStream.Position := 0;
  ZipStream.Read( FileSignature, sizeof( FileSignature ) );
  if FileSignature <> $4B50 then
    raise EAbZipInvalid.Create;
  ZipStream.Position := 0;

  {copy the stub into the selfex stream}
  SelfExtractingStream.Position := 0;
  SelfExtractingStream.CopyFrom( StubStream, 0 );

  TailPosition := FindCentralDirectoryTail( ZipStream );
  if TailPosition = -1 then
    raise EAbZipInvalid.Create;
  {load the ZipDirectoryFileFooter}
  ZDFF := TAbZipDirectoryFileFooter.Create;
  try
    ZDFF.LoadFromStream( ZipStream );
    DirectoryStart := ZDFF.DirectoryOffset;
  finally
    ZDFF.Free;
  end;
  {copy everything up to the CDH into the SelfExtractingStream}
  ZipStream.Position := 0;
  SelfExtractingStream.CopyFrom( ZipStream, DirectoryStart );
  ZipStream.Position := DirectoryStart;
  repeat
    ZipItem := TAbZipItem.Create;
    try
      ZipItem.LoadFromStream( ZipStream );
      ZipItem.RelativeOffset := ZipItem.RelativeOffset + StubSize;
      {save the modified entry into the Self Extracting Stream}
      ZipItem.SaveCDHToStream( SelfExtractingStream );
    finally
      ZipItem.Free;
    end;
  until ZipStream.Position = TailPosition;

  {save the CDH Footer.}
  ZDFF := TAbZipDirectoryFileFooter.Create;
  try
    ZDFF.LoadFromStream( ZipStream );
    ZDFF.DirectoryOffset := ZDFF.DirectoryOffset + StubSize;
    ZDFF.SaveToStream( SelfExtractingStream );
  finally
    ZDFF.Free;
  end;
end;

end.





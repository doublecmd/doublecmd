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
{* ABBREVIA: AbZipTyp.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: PKZip types                                 *}
{* Based on information from Appnote.txt, shipped with   *}
{* PKWare's PKZip for Windows 2.5                        *}
{*********************************************************}

{$I AbDefine.inc}

unit AbZipTyp;

interface

uses
  Classes, AbArcTyp, AbUtils, AbSpanSt, uClassesEx;

const
  { note  #$50 = 'P', #$4B = 'K'}
  Ab_ZipVersion = 21;
  Ab_ZipLocalFileHeaderSignature            : Longint = $04034B50;
  Ab_ZipCentralDirectoryFileHeaderSignature : Longint = $02014B50;
  Ab_ZipCentralDirectoryTailSignature       : Longint = $06054B50;
  Ab_ZipSpannedSetSignature                 : Longint = $08074B50;
  Ab_ZipPossiblySpannedSignature            : Longint = $30304B50;
  Ab_GeneralZipSignature                    : Word    = $4B50;       {!!.02}

  Ab_ArchiveExtraDataRecord                 : Longint = $08064B50;
  Ab_DigitalSignature                       : Longint = $05054B50;
  Ab_Zip64EndCetralDirectory                : Longint = $06064B50;
  Ab_Zip64EndCetralDirectoryLocator         : Longint = $07064B50;
  Ab_ZipEndCentralDirectorySignature        : Longint = $06054B50;

  Ab_WindowsExeSignature                    : Word    = $5A4D;       {!!.02}
  Ab_LinuxExeSigWord1                       : Word    = $457F;       {!!.02}
  Ab_LinuxExeSigWord2                       : Word    = $464C;       {!!.02}

  AbDefZipSpanningThreshold = 0;
  AbDefPasswordRetries      = 3;
  AbFileIsEncryptedFlag     = $0001;
  AbHasDataDescriptorFlag   = $0008;
  AbLanguageEncodingFlag    = $0800;

  Ab_InfoZipUnicodePathSubfieldID           : Word    = $7075;
  Ab_XceedUnicodePathSubfieldID             : Word    = $554E;
  Ab_XceedUnicodePathSignature              : LongWord= $5843554E;

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

  PInfoZipUnicodePathRec = ^TInfoZipUnicodePathRec;
  TInfoZipUnicodePathRec = packed record
    Version: Byte;
    NameCRC32: LongInt;
    UnicodeName: array[0..0] of AnsiChar;
  end;

  PXceedUnicodePathRec = ^TXceedUnicodePathRec;
  TXceedUnicodePathRec = packed record
    Signature: LongWord;
    Length: Integer;
    UnicodeName: array[0..0] of WideChar;
  end;

type
  TAbZipCompressionMethod =
    (cmStored, cmShrunk, cmReduced1, cmReduced2, cmReduced3,
     cmReduced4, cmImploded, cmTokenized, cmDeflated,
     cmEnhancedDeflated, cmDCLImploded, cmBestMethod);

  TAbZipSupportedMethod =
    (smStored, smDeflated, smBestMethod);

  TAbZipHostOS =
    (hosMSDOS, hosAmiga, hosVAX, hosUnix, hosVMCMS, hosAtari,
     hosOS2, hosMacintosh, hosZSystem, hosCPM, hosNTFS, hosMVS);
     // above list is from APPNOTE.TXT Zip specification.
     // 7-Zip defines NTFS as value 11
     // also, see AB_BZIP_OS_ID_ constants in AbBZipTyp
     // and   TAbGzFileSystem in AbGzTyp

  {for method 6 - imploding}
  TAbZipDictionarySize =
    (dsInvalid, ds4K, ds8K);

  {for method 8 - deflating}
  TAbZipDeflationOption =
    (doInvalid, doNormal, doMaximum, doFast, doSuperFast );

type
  TAbNeedPasswordEvent = procedure(Sender : TObject;
                                   var NewPassword : AnsiString) of object;

const
  AbDefCompressionMethodToUse = smBestMethod;
  AbDefDeflationOption = doNormal;


type
  TAbZipDataDescriptor = class( TObject )
  protected {private}
    FCRC32            : Longint;
    FCompressedSize   : LongWord;
    FUncompressedSize : LongWord;
  public {methods}
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property CRC32 : Longint
      read FCRC32 write FCRC32;
    property CompressedSize : LongWord
      read FCompressedSize write FCompressedSize;
    property UncompressedSize : LongWord
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
    FLastModFileTime : Word;         // This is stored
    FLastModFileDate : Word;         // in local time zone
    FCRC32 : Longint;
    FCompressedSize : LongWord;
    FUncompressedSize : LongWord;
    FFileName : AnsiString;
    FExtraField : TAbExtraField;
    FIsValid : Boolean;
  protected {methods}
    function GetCompressionMethod : TAbZipCompressionMethod;
    function GetCompressionRatio : Double;
    function GetDataDescriptor : Boolean;
    function GetDeflationOption : TAbZipDeflationOption;
    function GetDictionarySize : TAbZipDictionarySize;
    function GetEncrypted : Boolean;
    function GetIsUTF8 : Boolean;
    function GetShannonFanoTreeCount : Byte;
    function GetValid : Boolean;
    procedure SetCompressionMethod( Value : TAbZipCompressionMethod );
    procedure SetIsUTF8( Value : Boolean );
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
    property CompressedSize : LongWord
      read FCompressedSize write FCompressedSize;
    property UncompressedSize : LongWord
      read FUncompressedSize write FUncompressedSize;
    property FileName : AnsiString
      read FFileName write FFileName;
    property ExtraField : TAbExtraField
      read FExtraField;

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
    property IsUTF8 : Boolean
      read GetIsUTF8 write SetIsUTF8;
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
    FDiskNumberStart        : Word;
    FInternalFileAttributes : Word;
    FExternalFileAttributes : LongWord;
    FRelativeOffset         : LongWord;
    FFileComment            : AnsiString;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property VersionMadeBy : Word
      read FVersionMadeBy write FVersionMadeBy;
    property DiskNumberStart : Word
      read FDiskNumberStart write FDiskNumberStart;
    property InternalFileAttributes : Word
      read FInternalFileAttributes write FInternalFileAttributes;
    property ExternalFileAttributes : LongWord
      read FExternalFileAttributes write FExternalFileAttributes;
    property RelativeOffset : LongWord
      read FRelativeOffset write FRelativeOffset;
    property FileComment : AnsiString
      read FFileComment write FFileComment;
  end;

{ TAbZipDirectoryFileFooter interface ====================================== }
  TAbZipDirectoryFileFooter = class( TObject )
  private
    function GetIsZip64: Boolean;
  protected {private}
    FValidSignature       : Longint;
    FSignature            : Longint;
    FDiskNumber           : Word;
    FStartDiskNumber      : Word;
    FEntriesOnDisk        : Word;
    FTotalEntries         : Word;
    FDirectorySize        : LongWord;
    FDirectoryOffset      : LongWord;
    FZipfileComment       : AnsiString;
    function GetValid : Boolean;
  public {methods}
    constructor Create;
    destructor Destroy;
      override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
    function Verify( Stream : TStream ): Boolean;
  public {properties}
    property Signature : Longint
      read FSignature write FSignature;
    property DiskNumber : Word
      read FDiskNumber write FDiskNumber;
    property EntriesOnDisk : Word
      read FEntriesOnDisk write FEntriesOnDisk;
    property TotalEntries : Word
      read FTotalEntries write FTotalEntries;
    property DirectorySize : LongWord
      read FDirectorySize write FDirectorySize;
    property DirectoryOffset : LongWord
      read FDirectoryOffset write FDirectoryOffset;
    property StartDiskNumber : Word
      read FStartDiskNumber write FStartDiskNumber;
    property ZipfileComment : AnsiString
      read FZipfileComment write FZipfileComment;
    property IsValid : Boolean
      read GetValid;
    property IsZip64: Boolean
      read GetIsZip64;
  end;

  {
   		Zip64 end of central directory record
        zip64 end of central dir
        signature                       4 bytes  (0x06064b50)
        size of zip64 end of central
        directory record                8 bytes
        version made by                 2 bytes
        version needed to extract       2 bytes
        number of this disk             4 bytes
        number of the disk with the
        start of the central directory  4 bytes
        total number of entries in the
        central directory on this disk  8 bytes
        total number of entries in the
        central directory               8 bytes
        size of the central directory   8 bytes
        offset of start of central
        directory with respect to
        the starting disk number        8 bytes
        zip64 extensible data sector    (variable size)
}


{ TAbZipItem interface ===================================================== }
  TAbZipItem = class( TAbArchiveItem )
  protected {private}
    FItemInfo : TAbZipDirectoryFileHeader;

  protected {methods}
    function GetCompressionMethod : TAbZipCompressionMethod;
    function GetCompressionRatio : Double;
    function GetDeflationOption : TAbZipDeflationOption;
    function GetDictionarySize : TAbZipDictionarySize;
    function GetDiskNumberStart : Word;
    function GetExtraField : TAbExtraField;
    function GetFileComment : AnsiString;
    function GetGeneralPurposeBitFlag : Word;
    function GetInternalFileAttributes : Word;
    function GetRawFileName : AnsiString;
    function GetRelativeOffset : Int64;
    function GetShannonFanoTreeCount : Byte;
    function GetVersionMadeBy : Word;
    function GetVersionNeededToExtract : Word;
    procedure SaveCDHToStream( Stream : TStream );
    procedure SaveDDToStream( Stream : TStream );
    procedure SaveLFHToStream( Stream : TStream );
    procedure SetCompressionMethod( Value : TAbZipCompressionMethod );
    procedure SetDiskNumberStart( Value : Word );
    procedure SetFileComment(const Value : AnsiString );
    procedure SetGeneralPurposeBitFlag( Value : Word );
    procedure SetInternalFileAttributes( Value : Word );
    procedure SetRelativeOffset( Value : Int64 );
    procedure SetVersionMadeBy( Value : Word );
    procedure SetVersionNeededToExtract( Value : Word );

  protected {redefined property methods}
    function  GetCompressedSize : Int64; override;
    function  GetCRC32 : Longint; override;
    function  GetExternalFileAttributes : LongWord; override;
    function  GetIsEncrypted : Boolean; override;
    function  GetLastModDateTime : TDateTime; override;
    function  GetLastModFileDate : Word; override;
    function  GetLastModFileTime : Word; override;
    function  GetSystemSpecificAttributes: LongWord; override;
    function  GetSystemSpecificLastModFileTime: Longint; override;
    function  GetUncompressedSize : Int64; override;
    procedure SetCompressedSize( const Value : Int64 ); override;
    procedure SetCRC32( const Value : Longint ); override;
    procedure SetExternalFileAttributes( Value : LongWord ); override;
    procedure SetFileName(const Value : string ); override;
    procedure SetLastModDateTime(const Value : TDateTime); override;
    procedure SetLastModFileDate(const Value : Word ); override;
    procedure SetLastModFileTime(const Value : Word ); override;
    procedure SetSystemSpecificAttributes(Value: LongWord); override;
    procedure SetSystemSpecificLastModFileTime(const Value: Longint); override;
    procedure SetUncompressedSize( const Value : Int64 ); override;

  public {methods}
    constructor Create; override;
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
    property ExtraField : TAbExtraField
      read GetExtraField;
    property FileComment : AnsiString
      read GetFileComment
      write SetFileComment;
    property InternalFileAttributes : Word
      read GetInternalFileAttributes
      write SetInternalFileAttributes;
    property GeneralPurposeBitFlag : Word
      read GetGeneralPurposeBitFlag
      write SetGeneralPurposeBitFlag;
    property RawFileName : AnsiString
      read GetRawFileName;
    property RelativeOffset : Int64
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
    FPassword               : AnsiString;
    FPasswordRetries        : Byte;
    FStubSize               : LongWord;
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
    class function SupportsEmptyFolder: Boolean; override;

  protected {methods}    
    function CreateItem(const SourceFileName   : string;
                        const ArchiveDirectory : string): TAbArchiveItem; override;
    procedure DoExtractHelper(Index : Integer; const NewName : string);
    procedure DoExtractToStreamHelper(Index : Integer; aStream : TStream);
    procedure DoTestHelper(Index : Integer);
    procedure DoInsertHelper(Index : Integer; OutStream : TStream);
    procedure DoInsertFromStreamHelper(Index : Integer; OutStream : TStream);
    procedure DoRequestNextImage(ImageNumber : Integer; var Stream : TStream;
      var Abort : Boolean );
    function FindCDTail : Int64;
    function GetItem( Index : Integer ) : TAbZipItem;
    function GetZipfileComment : AnsiString;
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
    function FixName(const Value : string ) : string;
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure SetZipfileComment(const Value : AnsiString );

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
    constructor Create(const FileName : string; Mode : Word );
      override;
    constructor CreateFromStream( aStream : TStream; const ArchiveName : string );
      override;
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
    property Password : AnsiString
      read FPassword
      write FPassword;
    property PasswordRetries : Byte
      read FPasswordRetries
      write FPasswordRetries
      default AbDefPasswordRetries;
    property StubSize : LongWord
      read FStubSize;
    property ZipfileComment : AnsiString
      read GetZipfileComment
      write SetZipfileComment;

    property Items[Index : Integer] : TAbZipItem                      {!!.03}
      read GetItem                                                    {!!.03}
      write PutItem; default;                                         {!!.03}

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

function FindCentralDirectoryTail(aStream : TStream) : Int64;
function FindZip64CentralDirLocator(aStream: TStream; centralDirectoryPos: Int64): Int64;

function VerifyZip(Strm : TStream) : TAbArchiveType;

function VerifySelfExtracting(Strm : TStream) : TAbArchiveType;

function MakeVersionMadeBy : Word;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  {$IFNDEF NoQt}
  {$IFDEF UsingCLX}
  QControls,
  QDialogs,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  AbResString,
  AbExcept,
  AbVMStrm,
  SysUtils,
  osConvEncoding;

function MakeVersionMadeBy : Word;
begin
  Result := Ab_ZipVersion;
{$IFDEF MSWINDOWS}
  if (GetVersion and $FF) > $05 then
    Result := Result + (10 shl 8) // Windows/NTFS
  else
    Result := Result + (0 shl 8)  // MS-DOS/FAT
{$ENDIF}
{$IFDEF LINUX}
  Result := Result + (3 shl 8)
{$ENDIF}
end;

function VerifyZip(Strm : TStream) : TAbArchiveType;
{ determine if stream appears to be in PkZip format }
var
  Footer       : TAbZipDirectoryFileFooter;
  ZipSig       : Word;
  Sig          : LongInt;                                                {!!.01}
  TailPosition : int64;
  StartPos     : int64;
begin
  StartPos := Strm.Position;
  Result := atUnknown;
  try
    Strm.Position := 0;
    if (Strm.Read(ZipSig, SizeOf(ZipSig)) = SizeOf(ZipSig)) and
       (ZipSig = Ab_GeneralZipSignature) then
    begin
      Strm.Position := 0;                                                {!!.02}
      Strm.Read(Sig, SizeOf(LongInt));                                   {!!.02}
      if (Sig = Ab_ZipSpannedSetSignature) then                          {!!.02}
        Result := atSpannedZip                                           {!!.02}
      else begin                                                         {!!.02}

        { attempt to find Central Directory Tail }
        TailPosition := FindCentralDirectoryTail( Strm );
        if TailPosition <> -1 then begin
          { check Central Directory Signature }
          Footer := TAbZipDirectoryFileFooter.Create;
          try
            if Footer.Verify(Strm) then
              Result := atZip;
          finally
            Footer.Free;
          end;
        end
    (* {!!.02}
      else begin  { may be a span }                                          {!!.01}
        Strm.Seek(0, soBeginning);                                           {!!.01}
        Strm.Read(Sig, SizeOf(LongInt));                                     {!!.01}
        if (Sig = Ab_ZipSpannedSetSignature)                                 {!!.01}
          or (Sig = Ab_ZipPossiblySpannedSignature)                          {!!.01}
        then                                                                 {!!.01}
          Result := atSpannedZip;                                            {!!.01}
    *) {!!.02}
      end;                                                                   {!!.01}
    end;
  except
    on EFilerError do
      Result := atUnknown;
  end;
  Strm.Position := StartPos;
end;

function VerifySelfExtracting(Strm : TStream) : TAbArchiveType;
{ determine if stream appears to be an executable with appended PkZip data }
var
  FileSignature : LongInt;
  StartPos      : Int64;
  IsWinExe, IsLinuxExe : Boolean;                                        {!!.01}
begin
  StartPos := Strm.Position;
  try
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
  except
    on EFilerError do
      Result := atUnknown;
  end;
  Strm.Position := StartPos;
end;

{============================================================================}
function FindZip64CentralDirLocator(aStream: TStream; centralDirectoryPos: Int64): Int64;
const
  minimumBlockSize: Integer = $14;
  maximumVariableData: Integer = $1000;
type
  DataRec = packed record
    signature: Longint;
    Data: array[0..15] of Byte;
  end;
  PDataRec = ^DataRec;
var
	Found: Boolean;
	i, BytesRead: Integer;
  Position, EndPosition: Int64;
  Buffer: array[0..4095] of Byte;

  function ToInt(value: Longint): integer;
  var
    Bytes: array[0..3] of byte absolute value;
  begin
    Result := (bytes[0]) or (bytes[1] shl 8)
      or (bytes[2] shl 16) or (bytes[3] shl 24);
  end;

begin
	Found := False;
	Position := centralDirectoryPos;
  EndPosition := Position - maximumVariableData;

  if Position < 0 then
    Position := 0;

  aStream.Seek(EndPosition, soBeginning);
  BytesRead := aStream.Read(Buffer, SizeOf(buffer));

	for i := BytesRead - SizeOf(DataRec) downto 0 do begin
    if (ToInt(PDataRec(@Buffer[i])^.signature) = Ab_Zip64EndCetralDirectoryLocator) then begin
      Position := EndPosition - (BytesRead - i);
      aStream.Seek(Position, soBeginning);
      Found := True;
      Break;
    end;
  end;

  if Found then 
    Result := Position
  else
    Result := -1;
end;
{============================================================================}
function FindCentralDirectoryTail(aStream : TStream) : Int64;
{ search end of aStream looking for ZIP Central Directory structure
  returns position in stream if found (otherwise returns -1),
  leaves stream positioned at start of structure or at original
  position if not found }
const
  StartBufSize = 512;
  CMaxBufSize = 64 * 1024;                                               {!!.01}
var
  StartPos  : Int64;
  TailRec : packed record
    trSig : longint;
    trMid : array [0..15] of byte;
    trLen : word;
  end;
  Buffer    : PAnsiChar;
  Offset    : Int64;
  TestPos   : PAnsiChar;
  Done      : boolean;
  BytesRead : Int64;
  BufSize   : Int64;
  MaxBufSize: Int64;                                                   {!!.01}
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
  StartPos := aStream.Seek(0, soCurrent);

  {start off with the majority case: no zip file comment, so the
   central directory tail is the last thing in the stream and it's a
   fixed size and doesn't indicate a zip file comment}
  Result := aStream.Seek(-sizeof(TailRec), soEnd);
  if (Result >= 0) then begin
    aStream.ReadBuffer(TailRec, sizeof(TailRec));
    if (TailRec.trSig = Ab_ZipEndCentralDirectorySignature) and
       (TailRec.trLen = 0) then begin
      aStream.Seek(Result, soBeginning);
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
      Result := aStream.Seek(Offset, soEnd);
      if (Result <= 0) then begin                                        {!!.01}
        Result := aStream.Seek(0, soBeginning);
        Done := true;
      end;

      {read a buffer full}
      BytesRead := aStream.Read(Buffer^, BufSize);

      if BytesRead < sizeOf(TailRec) then begin
        Result := -1;
        Exit;
      end;

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
          aStream.Seek(Result, soBeginning);
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
      dec(Offset, BufSize - SizeOf(TailRec));
    end;

    {if we reach this point, the CD tail is not present}
    Result := -1;
    aStream.Seek(StartPos, soBeginning);
  finally
    FreeMem(Buffer);
  end;

  { put SpanStream back the way it was }
  if aStream is TAbSpanStream then                                       {!!.01}
    TAbSpanStream(aStream).IgnoreSpanning := SpanState;                  {!!.01}
end;
{============================================================================}
procedure MakeSelfExtracting( StubStream, ZipStream,
                              SelfExtractingStream : TStream );
  {-takes an executable stub, and a .zip format stream, and creates
   a SelfExtracting stream.  The stub should create a TAbZipArchive
   passing itself as the file, using a read-only open mode.  It should
   then perform operations as needed - like ExtractFiles( '*.*' ).
   This routine updates the RelativeOffset of each item in the archive}
var
  DirectoryStart : Int64;
  FileSignature : Word;
  StubSize : LongWord;
  TailPosition : Int64;
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
  SelfExtractingStream.CopyFrom( StubStream, 0 ); // copy whole StubStream

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
  if DirectoryStart > 0 then
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
{============================================================================}
{$IFDEF MSWINDOWS}
function IsOEM(const aValue: RawByteString): Boolean;
const
  // Byte values of alpha-numeric characters in OEM and ANSI codepages.
  // Excludes NBSP, ordinal indicators, exponents, the florin symbol, and, for
  // ANSI codepages matched to certain OEM ones, the micro character.
  //
  // US (OEM 437, ANSI 1252)
  Oem437AnsiChars =
    [138, 140, 142, 154, 156, 158, 159, 181, 192..214, 216..246, 248..255];
  Oem437OemChars =
    [128..154, 160..165, 224..235, 237, 238];
  // Arabic (OEM 720, ANSI 1256)
  Oem720AnsiChars =
    [129, 138, 140..144, 152, 154, 156, 159, 170, 181, 192..214, 216..239, 244,
     249, 251, 252, 255];
  Oem720OemChars =
    [130, 131, 133, 135..140, 147, 149..155, 157..173, 224..239];
  // Greek (OEM 737, ANSI 1253)
  Oem737AnsiChars =
    [162, 181, 184..186, 188, 190..209, 211..254];
  Oem737OemChars =
    [128..175, 224..240, 244, 245];
  // Baltic Rim (OEM 775, ANSI 1257)
  Oem775AnsiChars =
    [168, 170, 175, 184, 186, 191..214, 216..246, 248..254];
  Oem775OemChars =
    [128..149, 151..155, 157, 160..165, 173, 181..184, 189, 190, 198, 199,
     207..216, 224..238];
  // Western European (OEM 850, ANSI 1252)
  Oem850AnsiChars =
    [138, 140, 142, 154, 156, 158, 159, 192..214, 216..246, 248..255];
  Oem850OemChars =
    [128..155, 157, 160..165, 181..183, 198, 199, 208..216, 222, 224..237];
  // Central & Eastern European (OEM 852, ANSI 1250)
  Oem852AnsiChars =
    [138, 140..143, 154, 156..159, 163, 165, 170, 175, 179, 185, 186, 188,
     190..214, 216..246, 248..254];
  Oem852OemChars =
    [128..157, 159..169, 171..173, 181..184, 189, 190, 198, 199, 208..216, 221,
     222, 224..238, 251..253];
  // Cyrillic (OEM 855, ANSI 1251)
  Oem855AnsiChars =
    [128, 129, 131, 138, 140..144, 154, 156..159, 161..163, 165, 168, 170, 175,
     178..180, 184, 186, 188..255];
  Oem855OemChars =
    [128..173, 181..184, 189, 190, 198, 199, 208..216, 221, 222, 224..238,
     241..252];
  // Turkish (OEM 857, ANSI 1254)
  Oem857AnsiChars =
    [138, 140, 154, 156, 159, 192..214, 216..246, 248..255];
  Oem857OemChars =
    [128..155, 157..167, 181..183, 198, 199, 210..212, 214..216, 222, 224..230,
     233..237];
  // Hebrew (OEM 862, ANSI 1255)
  Oem862AnsiChars =
    [181, 212..214, 224..250];
  Oem862OemChars =
    [128..154, 160..165, 224..235, 237, 238];
  // Cyrillic CIS (OEM 866, ANSI 1251)
  Oem866AnsiChars =
    [128, 129, 131, 138, 140..144, 154, 156..159, 161..163, 165, 168, 170, 175,
     178..181, 184, 186, 188..255];
  Oem866OemChars =
    [128..175, 224..247];
var
  AnsiChars, OemChars: set of Byte;
  IsANSI: Boolean;
  i: Integer;
begin
  case GetOEMCP of
    437:
    begin
      AnsiChars := Oem437AnsiChars;
      OemChars := Oem437OemChars;
    end;
    720:
    begin
      AnsiChars := Oem720AnsiChars;
      OemChars := Oem720OemChars;
    end;
    737:
    begin
      AnsiChars := Oem737AnsiChars;
      OemChars := Oem737OemChars;
    end;
    775:
    begin
      AnsiChars := Oem775AnsiChars;
      OemChars := Oem775OemChars;
    end;
    850:
    begin
      AnsiChars := Oem850AnsiChars;
      OemChars := Oem850OemChars;
    end;
    852:
    begin
      AnsiChars := Oem852AnsiChars;
      OemChars := Oem852OemChars;
    end;
    855:
    begin
      AnsiChars := Oem855AnsiChars;
      OemChars := Oem855OemChars;
    end;
    857:
    begin
      AnsiChars := Oem857AnsiChars;
      OemChars := Oem857OemChars;
    end;
    862:
    begin
      AnsiChars := Oem862AnsiChars;
      OemChars := Oem862OemChars;
    end;
    866:
    begin
      AnsiChars := Oem866AnsiChars;
      OemChars := Oem866OemChars;
    end;
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  IsANSI := True;
  Result := True;
  for i := 0 to Length(aValue) do
    if Ord(aValue[i]) >= $80 then
    begin
      if IsANSI then
        IsANSI := Ord(aValue[i]) in AnsiChars;
      if Result then
        Result := Ord(aValue[i]) in OemChars;
      if not IsANSI and not Result then
        Break
    end;
  if IsANSI then
    Result := False;
end;
{============================================================================}
function TryEncode(const aValue: UnicodeString; aCodePage: UINT; aAllowBestFit: Boolean;
  out aResult: AnsiString): Boolean;
const
  WC_NO_BEST_FIT_CHARS = $00000400;
  Flags: array[Boolean] of DWORD = (WC_NO_BEST_FIT_CHARS, 0);
var
  UsedDefault: BOOL;
begin
  if not aAllowBestFit and not CheckWin32Version(4, 1) then
    Result := False
  else begin
    SetLength(aResult, WideCharToMultiByte(aCodePage, Flags[aAllowBestFit],
      PWideChar(aValue), Length(aValue), nil, 0, nil, @UsedDefault));
    SetLength(aResult, WideCharToMultiByte(aCodePage, Flags[aAllowBestFit],
      PWideChar(aValue), Length(aValue), PAnsiChar(aResult),
      Length(aResult), nil, @UsedDefault));
    Result := not UsedDefault;
  end;
end;
{$ENDIF MSWINDOWS}
{============================================================================}
{ TAbZipDataDescriptor implementation ====================================== }
procedure TAbZipDataDescriptor.LoadFromStream( Stream : TStream );
begin
  Stream.Read( FCRC32, sizeof(FCRC32) );
  if FCRC32 = Ab_ZipSpannedSetSignature then
  	Stream.Read( FCRC32, sizeof( FCRC32 ) );
  Stream.Read( FCompressedSize, sizeof( FCompressedSize ) );
  Stream.Read( FUncompressedSize, sizeof( FUncompressedSize ) );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDataDescriptor.SaveToStream( Stream : TStream );
begin
  {!!.01 -- rewritten}
  {      Although not originally assigned a signature, the value 
      0x08074b50 has commonly been adopted as a signature value 
      for the data descriptor record.  Implementers should be 
      aware that ZIP files may be encountered with or without this 
      signature marking data descriptors and should account for
      either case when reading ZIP files to ensure compatibility.
      When writing ZIP files, it is recommended to include the
      signature value marking the data descriptor record.  When
      the signature is used, the fields currently defined for
      the data descriptor record will immediately follow the
      signature.}
  Stream.Write( Ab_ZipSpannedSetSignature, sizeof( Ab_ZipSpannedSetSignature ) );
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
  FExtraField := TAbExtraField.Create;
  FValidSignature := $0;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipFileHeader.Destroy;
begin
  if Assigned(FExtraField) then
    FreeAndNil(FExtraField);
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
  CompSize : Int64;
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
function TAbZipFileHeader.GetIsUTF8 : Boolean;
begin
  Result := ( ( GeneralPurposeBitFlag and AbLanguageEncodingFlag ) <> 0 );
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
procedure TAbZipFileHeader.SetIsUTF8( Value : Boolean );
begin
  if Value then
    GeneralPurposeBitFlag := GeneralPurposeBitFlag or AbLanguageEncodingFlag
  else
    GeneralPurposeBitFlag := GeneralPurposeBitFlag and not AbLanguageEncodingFlag;
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
var
  ExtraFieldLength, FileNameLength : Word;
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
    Read( FileNameLength, sizeof( FileNameLength ) );
    Read( ExtraFieldLength, sizeof( ExtraFieldLength ) );

    SetLength( FFileName, FileNameLength );
    if FileNameLength > 0 then
      Read( FFileName[1], FileNameLength );

    FExtraField.LoadFromStream( Stream, ExtraFieldLength );
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.SaveToStream( Stream : TStream );
var
  ExtraFieldLength, FileNameLength: Word;
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
    FileNameLength := Word( Length( FFileName ) );
    Write( FileNameLength, sizeof( FileNameLength ) );
    ExtraFieldLength := Length(FExtraField.Buffer);
    Write( ExtraFieldLength, sizeof( ExtraFieldLength ) );
    if FileNameLength > 0 then
      Write( FFileName[1], FileNameLength );
    if ExtraFieldLength > 0 then
      Write(FExtraField.Buffer[0], ExtraFieldLength);
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileHeader implementation ================================= }
constructor TAbZipDirectoryFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipCentralDirectoryFileHeaderSignature;
  FVersionMadeBy:= MakeVersionMadeBy;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileHeader.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.LoadFromStream( Stream : TStream );
var
  ExtraFieldLength, FileCommentLength, FileNameLength : Word;
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
    Read( FileNameLength, sizeof( FileNameLength ) );
    Read( ExtraFieldLength, sizeof( ExtraFieldLength ) );
    Read( FileCommentLength, sizeof( FileCommentLength ) );
    Read( FDiskNumberStart, sizeof( FDiskNumberStart ) );
    Read( FInternalFileAttributes, sizeof( FInternalFileAttributes ) );
    Read( FExternalFileAttributes, sizeof( FExternalFileAttributes ) );
    Read( FRelativeOffset, sizeof( FRelativeOffset ) );

    SetLength( FFileName, FileNameLength );
    if FileNameLength > 0 then
      Read( FFileName[1], FileNameLength );

    FExtraField.LoadFromStream( Stream, ExtraFieldLength );

    SetLength( FFileComment, FileCommentLength );
    if FileCommentLength > 0 then
      Read( FFileComment[1], FileCommentLength );
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.SaveToStream( Stream : TStream );
var
  ExtraFieldLength, FileCommentLength, FileNameLength : Word;
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
    FileNameLength := Word( Length( FFileName ) );
    Write( FileNameLength, sizeof( FileNameLength ) );
    ExtraFieldLength := Length(FExtraField.Buffer);
    Write( ExtraFieldLength, sizeof( ExtraFieldLength ) );
    FileCommentLength := Word( Length( FFileComment ) );
    Write( FileCommentLength, sizeof( FileCommentLength ) );
    Write( FDiskNumberStart, sizeof( FDiskNumberStart ) );
    Write( FInternalFileAttributes, sizeof( FInternalFileAttributes ) );
    Write( FExternalFileAttributes, sizeof( FExternalFileAttributes ) );
    Write( FRelativeOffset, sizeof( FRelativeOffset ) );
    if FileNameLength > 0 then
      Write( FFileName[1], FileNameLength );
    if ExtraFieldLength > 0 then
      Write( FExtraField.Buffer[0], ExtraFieldLength );
    if FileCommentLength > 0 then
      Write( FFileComment[1], FileCommentLength );
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileFooter implementation ================================= }
constructor TAbZipDirectoryFileFooter.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipEndCentralDirectorySignature;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileFooter.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipDirectoryFileFooter.GetIsZip64: Boolean;
begin
  Result := (DiskNumber = $FFFF) or
            (StartDiskNumber = $FFFF) or
            (EntriesOnDisk = $FFFF) or
            (TotalEntries = $FFFF) or
            (DirectorySize = LongWord(-1)) or
            (DirectoryOffset = LongWord(-1));
end;
{ -------------------------------------------------------------------------- }
function TAbZipDirectoryFileFooter.GetValid : Boolean;
begin
  Result := (FSignature = FValidSignature);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.LoadFromStream( Stream : TStream );
var
  ZipfileCommentLength : Word;
begin
  with Stream do begin
    Read( FSignature, sizeof( FSignature ) );
    Read( FDiskNumber, sizeof( FDiskNumber ) );
    Read( FStartDiskNumber, sizeof( FStartDiskNumber ) );
    Read( FEntriesOnDisk, sizeof( FEntriesOnDisk ) );
    Read( FTotalEntries, sizeof( FTotalEntries ) );
    Read( FDirectorySize, sizeof( FDirectorySize ) );
    Read( FDirectoryOffset, sizeof( FDirectoryOffset ) );
    Read( ZipfileCommentLength, sizeof( ZipfileCommentLength ) );

    SetLength( FZipfileComment, ZipfileCommentLength );
    if ZipfileCommentLength > 0 then
      Read( FZipfileComment[1], ZipfileCommentLength );
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.SaveToStream( Stream : TStream );
var
  ZipfileCommentLength : Word;
begin
  with Stream do begin
    Write( FValidSignature, sizeof( FValidSignature ) );
    Write( FDiskNumber, sizeof( FDiskNumber ) );
    Write( FStartDiskNumber, sizeof( FStartDiskNumber ) );
    Write( FEntriesOnDisk, sizeof( FEntriesOnDisk ) );
    Write( FTotalEntries, sizeof( FTotalEntries ) );
    Write( FDirectorySize, sizeof( FDirectorySize ) );
    Write( FDirectoryOffset, sizeof( FDirectoryOffset ) );
    ZipfileCommentLength := Length( FZipfileComment );
    Write( ZipfileCommentLength, sizeof( ZipfileCommentLength ) );
    if ZipfileCommentLength > 0 then
      Write( FZipfileComment[1], ZipfileCommentLength );
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbZipDirectoryFileFooter.Verify( Stream : TStream ): Boolean;
var
  ZipfileCommentLength : Word;
begin
  with Stream do
  begin
    Result :=
      ( Read( FSignature, sizeof( FSignature ) ) = sizeof( FSignature ) ) and
      ( IsValid ) and
      ( Read( FDiskNumber, sizeof( FDiskNumber ) ) = sizeof( FDiskNumber ) ) and
      ( Read( FStartDiskNumber, sizeof( FStartDiskNumber ) ) = sizeof( FStartDiskNumber ) ) and
      ( Read( FEntriesOnDisk, sizeof( FEntriesOnDisk ) ) = sizeof( FEntriesOnDisk ) ) and
      ( Read( FTotalEntries, sizeof( FTotalEntries ) ) = sizeof( FTotalEntries ) ) and
      ( Read( FDirectorySize, sizeof( FDirectorySize ) ) = sizeof( FDirectorySize ) ) and
      ( Read( FDirectoryOffset, sizeof( FDirectoryOffset ) ) = sizeof( FDirectoryOffset ) ) and
      ( Read( ZipfileCommentLength, sizeof( ZipfileCommentLength ) ) = sizeof( ZipfileCommentLength ) ) and
      // Only comment left in the stream.
      ( ZipfileCommentLength = Size - Position );
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipItem implementation ================================================ }
constructor TAbZipItem.Create;
begin
  inherited Create;
  FItemInfo := TAbZipDirectoryFileHeader.Create;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipItem.Destroy;
begin
  FItemInfo.Free;
  FItemInfo := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressedSize : Int64;
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
function TAbZipItem.GetExternalFileAttributes : LongWord;
begin
  Result := FItemInfo.ExternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExtraField : TAbExtraField;
begin
  Result := FItemInfo.ExtraField;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetFileComment : AnsiString;
begin
  Result := FItemInfo.FileComment;
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
function TAbZipItem.GetLastModDateTime : TDateTime;
var
  FileTime: Longint;
begin
  LongRec(FileTime).Hi := LastModFileDate;
  LongRec(FileTime).Lo := LastModFileTime;

  // ZIP stores MS-DOS local time
  Result := AbDosFileTimeToDateTime(FileTime);
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
function TAbZipItem.GetSystemSpecificAttributes: LongWord;
var
  SystemCode: TAbZipHostOs;
begin
  Result := GetExternalFileAttributes;

  SystemCode := TAbZipHostOs(Byte(VersionMadeBy shr 8));

{$IFDEF MSWINDOWS}
  if (SystemCode = hosUnix) or
     // Ugly hack: $1FFFF is max value of attributes on Windows
     (Result > $1FFFF) then
  begin
    // Zip stores Unix attributes in high 16 bits.
    Result := AbUnix2DosFileAttributes(Result shr 16);
  end;
{$ENDIF}
{$IFDEF UNIX}
  if (SystemCode = hosMSDOS) or (SystemCode = hosNTFS) or (SystemCode = hosMVS) then
    Result := AbDOS2UnixFileAttributes(Result)
  else
    // Zip stores Unix attributes in high 16 bits.
    Result := Result shr 16;
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetRawFileName : AnsiString;
begin
  Result := FItemInfo.FileName;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetSystemSpecificLastModFileTime: Longint;
{$IFDEF UNIX}
var
  DateTime: TDateTime;
{$ENDIF}
begin
  // Zip stores MS-DOS date/time.
  LongRec(Result).Hi := LastModFileDate;
  LongRec(Result).Lo := LastModFileTime;

{$IFDEF UNIX}
  DateTime := AbDosFileTimeToDateTime(Result);
  Result   := AbDateTimeToUnixFileTime(DateTime);
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetRelativeOffset : Int64;
begin
  Result := FItemInfo.RelativeOffset;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetShannonFanoTreeCount : Byte;
begin
  Result := FItemInfo.ShannonFanoTreeCount;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetUncompressedSize : Int64;
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
var
  FieldSize: Word;
  InfoZipField: PInfoZipUnicodePathRec;
  UnicodeName: UnicodeString;
  UTF8Name: UTF8String;
  XceedField: PXceedUnicodePathRec;
  tempFileName: string;
  SystemCode: TAbZipHostOs;
begin
  FItemInfo.LoadFromStream( Stream );

  if FItemInfo.IsUTF8 or (AbDetectCharSet(FItemInfo.FileName) = csUTF8) then
    inherited SetFileName(FItemInfo.FileName)
  else if FItemInfo.ExtraField.Get(Ab_InfoZipUnicodePathSubfieldID, Pointer(InfoZipField), FieldSize) and
     (FieldSize > SizeOf(TInfoZipUnicodePathRec)) and
     (InfoZipField.Version = 1) and
     (InfoZipField.NameCRC32 = AbCRC32Of(FItemInfo.FileName)) then begin
    SetString(UTF8Name, InfoZipField.UnicodeName,
      FieldSize - SizeOf(TInfoZipUnicodePathRec) + 1);
    inherited SetFileName(UTF8Name);
  end
  else if FItemInfo.ExtraField.Get(Ab_XceedUnicodePathSubfieldID, Pointer(XceedField), FieldSize) and
     (FieldSize > SizeOf(TXceedUnicodePathRec)) and
     (XceedField.Signature = Ab_XceedUnicodePathSignature) and
     (XceedField.Length * SizeOf(WideChar) = FieldSize - SizeOf(TXceedUnicodePathRec) + SizeOf(WideChar)) then begin
    SetString(UnicodeName, XceedField.UnicodeName, XceedField.Length);
    inherited SetFileName(UTF8Encode(UnicodeName));
  end
  else
  begin
    SystemCode := TAbZipHostOs(Byte(VersionMadeBy shr 8));
    {$IF DEFINED(MSWINDOWS)}
    if (GetACP <> GetOEMCP) and ((SystemCode = hosMSDOS) or IsOEM(FItemInfo.FileName)) then
      inherited SetFileName(AnsiToUtf8(AbStrOemToAnsi(FItemInfo.FileName)))
    else if (SystemCode = hosNTFS) or (SystemCode = hosMVS) then
      inherited SetFileName(AnsiToUtf8(FItemInfo.FileName))
    else
    {$ELSEIF DEFINED(LINUX)}
    if (SystemCode = hosMSDOS) then
      inherited SetFileName(OEMToSys(FItemInfo.FileName))
    else if (SystemCode = hosNTFS) or (SystemCode = hosMVS) then
      inherited SetFileName(AnsiToSys(FItemInfo.FileName))
    else
    {$ENDIF}
      inherited SetFileName(FItemInfo.FileName);
  end;

  IsDirectory := ((FItemInfo.ExternalFileAttributes and faDirectory) <> 0) or
    ((FileName <> '') and CharInSet(Filename[Length(Filename)], ['\','/']));

  LastModFileTime := FItemInfo.LastModFileTime;
  LastModFileDate := FItemInfo.LastModFileDate;
  tempFileName := FileName;
  AbUnfixName( tempFileName );
  DiskFileName := tempFileName;
  Action := aaNone;
  Tagged := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveLFHToStream( Stream : TStream );
var
  LFH : TAbZipLocalFileHeader;
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
    LFH.FileName := RawFileName;
    LFH.ExtraField.Buffer := ExtraField.Buffer;
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
procedure TAbZipItem.SetCompressedSize( const Value : Int64 );
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
procedure TAbZipItem.SetExternalFileAttributes( Value : LongWord );
begin
  FItemInfo.ExternalFileAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileComment(const Value : AnsiString );
begin
  FItemInfo.FileComment := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileName(const Value : string );
var
  {$IFDEF MSWINDOWS}
  AnsiName : AnsiString;
  {$ENDIF}
  UTF8Name : UTF8String;
  FieldSize : Word;
  I : Integer;
  InfoZipField : PInfoZipUnicodePathRec;
  UseExtraField: Boolean;
begin
  inherited SetFileName(Value);
  {$IFDEF MSWINDOWS}
  FItemInfo.IsUTF8 := False;
  VersionMadeBy := Low(VersionMadeBy);
  if TryEncode(UTF8Decode(Value), CP_OEMCP, False, AnsiName) then
    {no-op}
  else if (GetACP <> GetOEMCP) and TryEncode(UTF8Decode(Value), CP_ACP, False, AnsiName) then
    VersionMadeBy := VersionMadeBy or $0B00
(*
  else if TryEncode(UTF8Decode(Value), CP_OEMCP, True, AnsiName) then
    {no-op}
  else if (GetACP <> GetOEMCP) and TryEncode(UTF8Decode(Value), CP_ACP, True, AnsiName) then
    VersionMadeBy := VersionMadeBy or $0B00
*)
  else
    FItemInfo.IsUTF8 := True;
  if FItemInfo.IsUTF8 then
    FItemInfo.FileName := Value
  else
    FItemInfo.FileName := AnsiName;
  {$ENDIF}
  {$IFDEF LINUX}
  FItemInfo.FileName := Value;
  FItemInfo.IsUTF8 := AbSysCharSetIsUTF8;
  {$ENDIF}

  UseExtraField := False;
  if not FItemInfo.IsUTF8 then
    for i := 1 to Length(Value) do begin
      if Ord(Value[i]) > 127 then begin
        UseExtraField := True;
        Break;
      end;
    end;

  if UseExtraField then begin
    UTF8Name := Value;
    FieldSize := SizeOf(TInfoZipUnicodePathRec) + Length(UTF8Name) - 1;
    GetMem(InfoZipField, FieldSize);
    try
      InfoZipField.Version := 1;
      InfoZipField.NameCRC32 := AbCRC32Of(FItemInfo.FileName);
      Move(UTF8Name[1], InfoZipField.UnicodeName, Length(UTF8Name));
      FItemInfo.ExtraField.Put(Ab_InfoZipUnicodePathSubfieldID, InfoZipField^, FieldSize);
    finally
      FreeMem(InfoZipField);
    end;
  end
  else
    FItemInfo.ExtraField.Delete(Ab_InfoZipUnicodePathSubfieldID);
  FItemInfo.ExtraField.Delete(Ab_XceedUnicodePathSubfieldID);
end;
{$IFDEF OPTIMIZATIONS_ON}{$O+}{$ENDIF}
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
procedure TAbZipItem.SetLastModDateTime(const Value : TDateTime);
var
  FileTime : Integer;
begin
  // ZIP stores MS-DOS local time
  FileTime := AbDateTimeToDosFileTime(Value);

  LastModFileDate := LongRec(FileTime).Hi;
  LastModFileTime := LongRec(FileTime).Lo;
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
procedure TAbZipItem.SetSystemSpecificAttributes(Value: LongWord);
begin
{$IFDEF UNIX}
  // Zip stores Unix attributes in high 16 bits.
  Value := Value shl 16;
{$ENDIF}
  SetExternalFileAttributes(Value);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetSystemSpecificLastModFileTime(const Value: Longint);
var
{$IFDEF UNIX}
  DateTime: TDateTime;
{$ENDIF}
  DosFileTime: Longint;
begin
  // Zip stores MS-DOS date/time.
{$IFDEF UNIX}
  DateTime    := AbUnixFileTimeToDateTime(Value);
  DosFileTime := AbDateTimeToDosFileTime(DateTime);
{$ELSE}
  DosFileTime := Value;
{$ENDIF}

  LastModFileDate := LongRec(DosFileTime).Hi;
  LastModFileTime := LongRec(DosFileTime).Lo;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetRelativeOffset( Value : Int64 );
begin
  FItemInfo.RelativeOffset := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetUncompressedSize( const Value : Int64 );
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
constructor TAbZipArchive.Create(const FileName : string; Mode : Word );
var
  Stream: TAbSpanStream;
begin
  FOwnsStream := True;
  if AbDriveIsRemovable(FileName) then
    Stream := TAbSpanStream.Create(FileName, Mode, mtRemoveable, FSpanningThreshold)
  else
    Stream := TAbSpanStream.Create(FileName, Mode, mtLocal, FSpanningThreshold);
  Stream.OnRequestImage := DoSpanningMediaRequest;
  Stream.OnArchiveProgress := DoArchiveSaveProgress;
  CreateFromStream(Stream, FileName);
  FMode := Mode;
end;
{ -------------------------------------------------------------------------- }
constructor TAbZipArchive.CreateFromStream( aStream : TStream;
                                      const ArchiveName : string );
begin
  inherited CreateFromStream( aStream, ArchiveName );
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
destructor TAbZipArchive.Destroy;
begin
  FInfo.Free;
  FInfo := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.CreateItem(const SourceFileName   : string;
                                  const ArchiveDirectory : string): TAbArchiveItem;
var
  FullSourceFileName, FullArchiveFileName: string;
begin
  Result := TAbZipItem.Create;
  with TAbZipItem( Result ) do begin
    CompressionMethod := cmDeflated;
    GeneralPurposeBitFlag := 0;
    CompressedSize := 0;
    CRC32 := 0;
    RelativeOffset := 0;

    MakeFullNames(SourceFileName, ArchiveDirectory,
                  FullSourceFileName, FullArchiveFileName);

    if AbDirectoryExists(FullSourceFileName) then begin
      IsDirectory := True;
      FullSourceFileName := IncludeTrailingPathDelimiter(FullSourceFileName);
    end;

    Result.FileName     := FullArchiveFileName;
    Result.DiskFileName := FullSourceFileName;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractHelper(Index : Integer; const NewName : string);
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
    pMessage := AbLastDiskRequestS;
    pCaption := AbDiskRequestS;
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
    pMessage := AbDiskNumRequestS;
    FMessage := Format(pMessage, [DiskNumber] );
    pMessage := FMessage;
    pCaption := AbDiskRequestS;
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
    pMessage := AbBlankDiskS;
    pCaption := AbDiskRequestS;
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
// Removed {!!.05 if ImageNumber is looking for another disk it will never be opened.
//  if FindCentralDirectoryTail(Stream) > -1 {not found} then begin        {!!.03}
//    Exit;                                                                {!!.03}
//  end;                                                                   {!!.03}

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
//    if (ImageNumber = 0) then
//      ImageName := FArchiveName
//    else
      AbIncFilename(ImageName, ImageNumber);
    if not mbFileExists(ImageName) then
      raise EAbFileNotFound.Create;
    Stream.Free;
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;   {!!.05 [ 714944 ]}
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
//    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Exit;
  end;


  {search for last image, assuming images were auto-generated}
  FAutoGen := True;                                                  {!!.02}
  for i := 1 to 99 do begin
    AbIncFilename(ImageName, i);
    if not mbFileExists(ImageName) then
      raise EAbFileNotFound.Create;
    // 885670 (Moved Stream to avoid file corruption)
    if Assigned(Stream) then
       Stream.Free;                                                     {!!.04}
    Stream := TAbSpanStream.Create(ImageName, fmOpenRead, MediaType, FSpanningThreshold);
    TAbSpanStream(Stream).OnRequestImage := DoSpanningMediaRequest;
    TAbSpanStream(Stream).OnArchiveProgress := DoArchiveSaveProgress;  {!!.04}
//    TAbSpanStream(Stream).SpanNumber := ImageNumber;                     {!!.01}
    Found := (FindCentralDirectoryTail(Stream) > -1);
    if Found then
      Break
    else
      FreeAndNil(Stream);
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
  else if FAutoGen then
   begin
    AbIncFilename(ImageName, ImageNumber);                            {!!.02}
    // if we are reading and the file does not exist
    // then we must be at last file in archive, change to .ZIP extention
    // as the last file is there.
    if (Mode = smReading) and Not mbFileExists(ImageName) then         {!!.05}
      ImageName := ChangeFileExt(ImageName,'.ZIP');
   end
  else if Mode = smReading then begin

    pMessage := Format(AbImageNumRequestS, [ImageNumber]);
    pCaption := AbImageRequestS;
{$IFDEF MSWINDOWS}
{!!.04}
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
      AbWriteVolumeLabel(Format(AB_SPAN_VOL_LABEL,                       {!!.01}
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
function TAbZipArchive.FindCDTail : Int64;
begin
  Result := FindCentralDirectoryTail( FStream );
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.FixName(const Value : string ) : string;
  {-changes backslashes to forward slashes}
var
  i : SmallInt;
  lValue : string;
begin
  lValue := Value; {!!.05 [ 783583 ]}
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
    lValue := AbGetShortFileSpec( lValue );
  end;
  {$ENDIF MSWINDOWS}

  {Zip files Always strip the drive path}
  StoreOptions := StoreOptions + [soStripDrive];

  {strip drive stuff}
  if soStripDrive in StoreOptions then
    AbStripDrive( lValue );

  {check for a leading backslash}
  if (Length(lValue) > 1) and (lValue[1] = AbPathDelim) then {!!.05  - [ 799438 ]}
    System.Delete( lValue, 1, 1 );

  if soStripPath in StoreOptions then begin
    lValue := ExtractFileName( lValue );
  end;

  if soRemoveDots in StoreOptions then
    AbStripDots( lValue );

  for i := 1 to Length( lValue ) do
    if lValue[i] = AbDosPathDelim then
      lValue[i] := AbUnixPathDelim;
  Result := lValue;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetItem( Index : Integer ) : TAbZipItem;
begin
  Result := TAbZipItem(FItemList.Items[Index]);
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetZipfileComment : AnsiString;
begin
  Result := FInfo.ZipfileComment;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.LoadArchive;
var
  Abort : Boolean;
  TailPosition : int64;
  Item : TAbZipItem;
  i : Integer;
  Progress : Byte;
  FileSignature : DWord;                                             {!!.02}
  IsZip : Boolean;
  Lowest : Int64;
begin
  Lowest := High(Int64);
  Abort := False;
  FAutoGen := False;                                                 {!!.02}
  if FStream.Size = 0 then
    Exit;

  {Get signature info}
  FStream.Position := 0;
  FStream.Read( FileSignature, sizeof( FileSignature ) );

  IsZip := (FileSignature and $0000FFFF) = Ab_GeneralZipSignature;   {!!.02}

{$IFDEF MSWINDOWS}
// [ 719083 ] Windows exe signature check
  IsExecutable := (FileSignature and $0000FFFF) = Ab_WindowsExeSignature;
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
  if (FileSignature = DWord(Ab_ZipSpannedSetSignature)) then         {!!.02}
  begin
    if FDriveIsRemovable then        {!!.05}
      TailPosition := -1              {!!.02}
    else
      TailPosition := FindCDTail;  {!!.05}
  end
  else                                                               {!!.02}
  begin                                                              {!!.02}
    TailPosition := FindCDTail;
    // 885670 (Second Part Better Error Message)
    if TailPosition = -1 then
      raise EAbZipInvalid.Create;
  end;

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
  //Possible Bug, Remove Drives could be split instead of spanned.
  If FSpanned and (Not FDriveIsRemovable) then
    FAutoGen := True;

  if FInfo.StartDiskNumber <> FInfo.DiskNumber then begin
  	DoRequestNextImage(FInfo.FStartDiskNumber, FStream, Abort);
    if Abort then
      raise EAbUserAbort.Create;
  end;

  { build Items list from central directory records }
  i := 0;
  FStream.Seek(FInfo.DirectoryOffset, soBeginning);
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
    try
    Item.LoadFromStream(FStream);
    except {!!.05 [ 800130 ] ZIP - Potential Memory Leak }
      Item.Free;
      raise;
    end;
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
procedure TAbZipArchive.SaveArchive;
  {builds a new archive and copies it to FStream}
var
  Abort              : Boolean;
  CDHStream          : TMemoryStream;
  HasDataDescriptor  : Boolean;
  i                  : LongWord;
  LFH                : TAbZipLocalFileHeader;
  NewStream          : TAbVirtualMemoryStream;
  WorkingStream      : TAbVirtualMemoryStream;
  CurrItem           : TAbZipItem;
  SCurrentImage      : LongWord;
  SCurrentOffset     : int64;
  SSpanningThreshold : Int64;
  CanSpan            : Boolean;
  MediaType          : TAbMediaType;                                     {!!.01}
  BlockSize          : int64;
  ByteBuf            : Byte;
begin
  if Count = 0 then
    Exit;

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
      if FOwnsStream then begin // [ 914427 ]
        FStream.Free;
        FStream := TAbSpanstream.Create(ArchiveName, fmOpenRead or fmShareDenyWrite,
          MediaType, FSpanningThreshold);
      end;
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
    begin
      if StubSize > 0 then
        NewStream.CopyFrom( FStream, StubSize );
    end
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
            try
              try {WorkingStream}

                WorkingStream.SwapFileDirectory := NewStream.SwapFileDirectory;

                if (CurrItem.Action = aaStreamAdd) then
                  DoInsertFromStreamHelper(i, WorkingStream)
                else
                  DoInsertHelper(i, WorkingStream);

                CurrItem.SaveLFHToStream(NewStream);
                NewStream.CopyFrom(WorkingStream, 0); // copy whole stream
                if CurrItem.IsEncrypted then                               {!!.01}
                  CurrItem.SaveDDToStream(NewStream);                      {!!.01}

                FInfo.EntriesOnDisk := FInfo.EntriesOnDisk + 1;
                FInfo.TotalEntries := FInfo.TotalEntries + 1;
                CurrItem.SaveCDHToStream(CDHStream);

              except
                on E : Exception do
                begin
                  { Exception was caused by a User Abort and Item Failure should not be called
                    Question:  Do we want an New Event when this occurs or should the
                    exception just be re-raised }
                  if (E is EAbUserAbort) then {!!.05 [ 783614 ]}
                     raise;
                  CurrItem.Action := aaDelete;
                  DoProcessItemFailure(CurrItem, ptAdd, ecFileOpenError, 0);
                end;
              end;
            finally
              WorkingStream.Free;
            end;
          end;
        end; { case }

        {Now add the data descriptor record to new stream}
        HasDataDescriptor := (CurrItem.CompressionMethod = cmDeflated) and
          ((CurrItem.GeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0);
        if (CurrItem.Action <> aaDelete) and HasDataDescriptor then    // it's already above
          CurrItem.SaveDDToStream(NewStream);                          // in aaAdd ?

        DoArchiveProgress(AbPercentage(9 * succ( i ), 10 * Count), Abort);

        if Abort then
          raise EabUserAbort.Create;
        if (SSpanningThreshold > 0) then
          SCurrentImage := NewStream.Position div SSpanningThreshold;
        SCurrentOffset := NewStream.Position - (SCurrentImage * SSpanningThreshold);
      end;
      { if Spanning and pad archive so directory starts on last disk }
      if SSpanningThreshold>0 then
        begin
          // is the disk space left on the disk enough, if not pad till it is
          BlockSize:=(SSpanningThreshold-SCurrentOffset);
          if (BlockSize>0) and (BlockSize<CDHStream.Size+512) then
            begin
              bytebuf := 0;
              for i:=0 to BlockSize do
                NewStream.Write(bytebuf,sizeof(bytebuf));
              SCurrentImage := NewStream.Position div SSpanningThreshold;
              SCurrentOffset := NewStream.Position - (SCurrentImage * SSpanningThreshold);
            end;
        end;

      {append the central directory}
      FInfo.StartDiskNumber := SCurrentImage;
      FInfo.DirectoryOffset := SCurrentOffset;

      { we're not sure if the CDH may span disks }
      if (SpanningThreshold > 0) then
        SCurrentImage := NewStream.Position div SSpanningThreshold;

      {append the central directory footer}
      FInfo.DirectorySize := CDHStream.Size;
      FInfo.DiskNumber := SCurrentImage;
      FInfo.SaveToStream(CDHStream);
      
      CDHStream.Position := 0;
      if CDHStream.Size > 0 then
        NewStream.CopyFrom( CDHStream, CDHStream.Size );
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
    else if not FOwnsStream then begin // [ 914427 ]
       FStream.Size := 0;
       FStream.Position := 0;
       if NewStream.Size > 0 then
         FStream.CopyFrom(NewStream, NewStream.Size)
    end else begin
      { need new stream to write }
      FreeAndNil(FStream);

{!!.01 -- Modified to take better advantage of TAbSpanStream features }
      if Spanned and AbDriveIsRemovable(FArchiveName) then begin         {!!.03}
        {reset image number }
        SCurrentImage := 0;
        AbWriteVolumeLabel(Format(AB_SPAN_VOL_LABEL,
          [Succ(SCurrentImage)]), AbDrive(FArchiveName));
        FStream := TAbSpanStream.Create(FArchiveName,
          fmOpenWrite or fmShareDenyWrite, mtRemoveable,
           FSpanningThreshold)
        {!!! write spanning signature here?}
      end
      else
       if SpanningThreshold > 0 then
        begin
          //NOTE: It is possible that a archive written through this method will not be
          //split.   I.e. The SpanningThreshold > TotalArchiveSize
          //First File of a Split Archive is 'ARCHIVENAME.Z01' however if not split
          //it should be 'ARCHIVENAME.ZIP'
          //To handle this problem the first file is created with. '.ZIP'
          //Later on in the process it is renamed to '.Z01' if spanned'
          //The last file in a split archive has .ZIP extension
          //It will be named .Z## initially.  Then renamed to .ZIP
          //So Split archive will have two renames, with last and first file.
          FStream := TAbSpanStream.Create(FArchiveName,
                    fmOpenWrite or fmShareDenyWrite, mtLocal,
                      FSpanningThreshold);
        end
       else
        begin
          FStream := TFileStreamEx.Create(FArchiveName,
            fmOpenReadWrite or fmShareDenyWrite);
        end;

      try
        if (FStream is TAbSpanStream) then
         begin
          TAbSpanStream(FStream).OnRequestImage :=
                                 DoSpanningMediaRequest;
          TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
//        TAbSpanStream(FStream).SpanNumber := SCurrentImage;              {!!.01}
         end;

        FStream.Size := 0;
        { copy temporary archive to the stream }
        
        if (FStream is TAbSpanStream) then
           TAbSpanStream(FStream).ArchiveTotalSize := NewStream.Size; {!!.04}

        if NewStream.Size > 0 then
          FStream.CopyFrom(NewStream, NewStream.Size);
      except
       on E : Exception do
        begin
         if E is EAbUserAbort                      {!!.05 [783614] }
           then raise
           else raise EAbBadStream.CreateInner(E);
         end;
      end;
{!!.01 -- End Modified }
    end;

   {rename if split archive}
    if (SCurrentImage > 0) then
      begin
       //Other archive types we hold the stream for the last file.
       //However, with a split archive we need to free it so we can rename it.
        MediaType := (FStream as TAbSpanStream).MediaType;
        FStream.Free;
       //Rename .ZIP to Z01
        RenameFile(FArchiveName,ChangeFileExt(FArchiveName,'.Z01'));
       //Rename .Z## (where ## is last image) to .ZIP
        if SCurrentImage < 9 then
         RenameFile(ChangeFileExt(FArchiveName,'.z0' + IntToStr(SCurrentImage+1)),FArchiveName)
        else
         RenameFile(ChangeFileExt(FArchiveName,'.z' + IntToStr(SCurrentImage+1)),FArchiveName);
        // SCurrentImage > 0 only if FStream is a Spanned Archive.
        //Note: Possible memory leak
        // Open the Split archive for reading to duplicate behavior of single file archives.
        FStream := TAbSpanstream.Create(ArchiveName, fmOpenRead or fmShareDenyWrite,
          MediaType, FSpanningThreshold);
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
procedure TAbZipArchive.SetZipfileComment(const Value : AnsiString );
begin
  FInfo.FZipfileComment := Value;
  FIsDirty := True;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.TestItemAt(Index : Integer);
begin
  DoTestHelper(Index);
end;
{ -------------------------------------------------------------------------- }        
class function TAbZipArchive.SupportsEmptyFolder: Boolean;
begin
	Result := True;
end;

end.


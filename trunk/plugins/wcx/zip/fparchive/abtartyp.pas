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
 * Joel Haynie
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbTarTyp.pas                                *}
{*********************************************************}
{* ABBREVIA: TAbTarArchive, TAbTarItem classes           *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with Tar files                                        *}
{*********************************************************}

unit AbTarTyp;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbUtils, AbArcTyp;

const
  AB_TAR_RECORDSIZE  = 512; {Note: SizeOf(TAbTarHeaderRec) = AB_TAR_RECORDSIZE}
  AB_TAR_NAMESIZE    = 100;
  AB_TAR_V7_EMPTY_SIZE = 167;
  AB_TAR_USTAR_PREFIX_SIZE = 155;
  AB_TAR_STAR_PREFIX_SIZE = 131;
  AB_TAR_OLD_GNU_EMPTY1_SIZE = 5;
  AB_TAR_OLD_GNU_SPARSE_SIZE = 96;
  AB_TAR_OLD_GNU_EMPTY2_SIZE = 17;
  AB_TAR_SIZE_AFTER_STDHDR = 167;
  AB_TAR_TUSRNAMELEN = 32;
  AB_TAR_TGRPNAMELEN = 32;


{ The checksum field is filled with this while the checksum is computed. }
  AB_TAR_CHKBLANKS     = '        ';  { 8 blank spaces(#20), no null }
  AB_TAR_L_HDR_NAME    = '././@LongLink'; { As seen in the GNU File Examples}
  AB_TAR_L_HDR_USR_NAME='root';       { On Cygwin this is #0, Redhat it is 'root' }
  AB_TAR_L_HDR_GRP_NAME='root';       { Same on all OS's }
  AB_TAR_L_HDR_ARR8_0  ='0000000'#0;  { 7 zeros and one null }
  AB_TAR_L_HDR_ARR12_0 ='00000000000'#0;{ 11 zeros and one null }
  AB_TAR_MAGIC_VAL     = 'ustar'#0;   { 5 chars & a nul }
  AB_TAR_MAGIC_VER     = '00';        { 2 chars }
  AB_TAR_MAGIC_GNUOLD  = 'ustar  '#0; { 7 chars & a null }
  AB_TAR_MAGIC_V7_NONE = #0#0#0#0#0#0#0#0;{ 8, #0 }

{ The linkflag defines the type of file(FH), and Meta Data about File(MDH) }
  AB_TAR_LF_OLDNORMAL = #0;   {  FH, Normal disk file, Unix compatible } { Historically used for V7 }
  AB_TAR_LF_NORMAL    = '0';  {  FH, Normal disk file }
  AB_TAR_LF_LINK      = '1';  {  FH, Link to previously archived file }
  AB_TAR_LF_SYMLINK   = '2';  {  FH, Symbolic(soft) link }
  AB_TAR_LF_CHR       = '3';  {  FH, Character special file }{ Used for device nodes, Conditionally compiled into GNUTAR }
  AB_TAR_LF_BLK       = '4';  {  FH, Block special file }{ Used for device nodes, Conditionally compiled into GNUTAR }
  AB_TAR_LF_DIR       = '5';  {  FH, Directory, Zero size File }
  AB_TAR_LF_FIFO      = '6';  {  FH, FIFO special file }{ Used for fifo files(pipe like), Conditionally complied into GNUTAR }
  AB_TAR_LF_CONTIG    = '7';  {  FH, Contiguous file } { Normal File, but All blocks should be contiguos on the disk }
  AB_TAR_LF_XHDR      = 'x';  { MDH, POSIX, Next File has Extended Header }
  AB_TAR_LF_XGL       = 'g';  { MDH, POSIX, Global Extended Header }
  AB_TAR_LF_DUMPDIR   = 'D';  {  FH, Extra GNU, Dump Directory} { Generated Dump of Files in a directory, has a size }
  AB_TAR_LF_LONGLINK  = 'K';  { MDH, Extra GNU, Next File has Long LinkName}
  AB_TAR_LF_LONGNAME  = 'L';  { MDH, Extra GNU, Next File has Long Name}
  AB_TAR_LF_MULTIVOL  = 'M';  {  FH, Extra GNU, MultiVolume File Cont.}{ End of a file that spans multiple TARs }
  AB_TAR_LF_SPARSE    = 'S';  {  FH, Extra GNU, Sparse File Cont.}
  AB_TAR_LF_VOLHDR    = 'V';  {  FH, Extra GNU, File is Volume Header }
  AB_TAR_LF_EXHDR     = 'X';  { MDH, Extra GNU, Solaris Extended Header }
  { The only questionable MetaData type is 'V', file or meta-data? will treat as file header }
  AB_SUPPORTED_F_HEADERS   = [AB_TAR_LF_OLDNORMAL, AB_TAR_LF_NORMAL, AB_TAR_LF_LINK,
                              AB_TAR_LF_SYMLINK, AB_TAR_LF_DIR];
  AB_UNSUPPORTED_F_HEADERS = [AB_TAR_LF_CHR, AB_TAR_LF_BLK, AB_TAR_LF_FIFO,
                              AB_TAR_LF_CONTIG, AB_TAR_LF_DUMPDIR, AB_TAR_LF_MULTIVOL,
                              AB_TAR_LF_SPARSE, AB_TAR_LF_VOLHDR];
  AB_SUPPORTED_MD_HEADERS  = [AB_TAR_LF_LONGNAME, AB_TAR_LF_LONGLINK];
  AB_UNSUPPORTED_MD_HEADERS= [AB_TAR_LF_XHDR, AB_TAR_LF_XGL, AB_TAR_LF_EXHDR];
  AB_GNU_MD_HEADERS        = [AB_TAR_LF_LONGLINK, AB_TAR_LF_LONGNAME]; { If present then OLD_/GNU_FORMAT }
  AB_PAX_MD_HEADERS        = [AB_TAR_LF_XHDR, AB_TAR_LF_XGL]; { If present then POSIX_FORMAT }
  AB_IGNORE_SIZE_HEADERS   = [AB_TAR_LF_LINK, AB_TAR_LF_SYMLINK, AB_TAR_LF_CHR,
                              AB_TAR_LF_BLK, AB_TAR_LF_DIR, AB_TAR_LF_FIFO];
{ The rest of the Chars are unsupported and unknown types Treat those headers as File types }
{ Further link types may be defined later. }

{ Bits used in the mode field - values in octal }
  AB_TAR_TSUID   = $0800;   { Set UID on execution }
  AB_TAR_TSGID   = $0400;   { Set GID on execution }
  AB_TAR_TSVTX   = $0200;   { Save text (sticky bit) }


type
  Arr8  = array [0..7] of AnsiChar;
  Arr12 = array [0..11] of AnsiChar;
  Arr12B = array[0..11] of Byte;
  ArrName = array [0..AB_TAR_NAMESIZE-1] of AnsiChar;
  TAbTarHeaderFormat = (UNKNOWN_FORMAT, V7_FORMAT, OLDGNU_FORMAT, GNU_FORMAT,
                        USTAR_FORMAT, STAR_FORMAT, POSIX_FORMAT);
  TAbTarItemType = (SUPPORTED_ITEM, UNSUPPORTED_ITEM, UNKNOWN_ITEM);
  TAbTarHeaderType = (FILE_HEADER, META_DATA_HEADER, MD_DATA_HEADER, UNKNOWN_HEADER);
  TAbTarMagicType = (GNU_OLD, NORMAL);
  TAbTarMagicRec = packed record
    case TAbTarMagicType of
      GNU_OLD: (gnuOld : array[0..7] of AnsiChar); { Old GNU magic: (Magic.gnuOld) }
      NORMAL : (value  : array[0..5] of AnsiChar;  {   Magic value: (Magic.value)}
                version: array[0..1] of AnsiChar); {       Version: (Magic.version) }
  end;

{ Notes from GNU Tar & POSIX Spec.: }
{All the first 345 bytes are the same. }
{   "USTAR_header": Prefix(155): 345-499,
                    empty(12): 500-511 }
{ "old_gnu_header": atime(12): 345-356,
                    ctime(12): 357-368,
                    offset(12): 369-380,
                    longnames(4): 381-384,
                    empty(1): 385,
                    sparse structs(4x(12+12)=96): 386-481,
                    isextended(1): 482,
                    realsize(12): 483-494,
                    empty(16): 495-511 }
{    "star_header": Prefix(131): 345-475,
                    atime(12): 476-487,
                    ctime(12): 488-499,
                    empty(12): 500-511 }
{ "star_in_header": prefix(1): 345,
                    empty(9): 346-354,
                    isextended(1): 355,
                    sparse structs(4x(12+12)=96): 356-451,
                    realsize(12): 452-463,
                    offset(12): 464-475,
                    atime(12): 476-487,
                    ctime(12): 488-499,
                    empty(8): 500-507,
                    xmagic(4): 508-511 }
{  "sparse_header": These two structs are the same, and they are Meta data about file. }
{"star_ext_header": sparse structs(21x(12+12)=504): 0-503,
                    isextended(1): 504 }
{POSIX(PAX) extended header: is a buffer packed with content of this form:
     This if from the POSIX spec. References the C printf command string.
     "%d %s=%s\n".  Then they are simply concatenated. }
    { PAX Extended Header Keywords: }
    { 'atime', 'charset', 'comment', 'ctime', 'gid', 'gname', 'linkpath', 'mtime', 'path',
      'realtime.', 'security.', 'size', 'uid', 'uname' }
    { GNU Added PAX Extended Header Keywords: }
    { 'GNU.sparse.name', 'GNU.sparse.major', 'GNU.sparse.minor',
      'GNU.sparse.realsize', 'GNU.sparse.numblocks', 'GNU.sparse.size',
      'GNU.sparse.offset', 'GNU.sparse.numbytes', 'GNU.sparse.map', 'GNU.dumpdir',
      'GNU.volume.label', 'GNU.volume.filename', 'GNU.volume.size',
      'GNU.volume.offset' }

  { V7 uses AB_TAR_LF_OLDNORMAL linkflag, has no magic field & no Usr/Grp Names }
  { V7 Format ends Empty(padded with zeros), as does the POSIX record. }
  TAbTarEnd_Empty_Rec = packed record
    Empty: array[0..AB_TAR_V7_EMPTY_SIZE-1] of Byte; { 345-511, $159-1FF, Empty Space }
  end;
  { UStar End Format }
  TAbTarEnd_UStar_Rec = packed record
    Prefix: array[0..AB_TAR_USTAR_PREFIX_SIZE-1] of AnsiChar;
                   { 345-499, $159-1F3, Prefix of file & path name, null terminated ASCII string }
    Empty : Arr12B;{ 500-512, $1F4-1FF, Empty Space }
  end;
  { Old GNU End Format }
  TAbTarEnd_GNU_old_Rec = packed record
    Atime : Arr12;  { 345-356, $159-164, time of last access (UNIX Date in ASCII coded Octal)}
    Ctime : Arr12;  { 357-368, $165-170, time of last status change (UNIX Date in ASCII coded Octal)}
    Offset: Arr12;  { 369-380, $171-17C, Multirecord specific value }
    Empty1: array[0..AB_TAR_OLD_GNU_EMPTY1_SIZE-1] of Byte;
                    { 381-385, $17D-181, Empty Space, Once contained longname ref. }
    Sparse: array[0..AB_TAR_OLD_GNU_SPARSE_SIZE-1] of Byte;
                    { 386-481, $182-1E1, Sparse File specific values }
    IsExtended: byte;{    482, $    1E2, Flag to signify Sparse file headers follow }
    RealSize: Arr12;{ 483-494, $1E3-1EE, Real size of a Sparse File. }
    Empty2: array[0..AB_TAR_OLD_GNU_EMPTY2_SIZE-1] of Byte;
                    { 495-511, $1EF-1FF, Empty Space }
  end;
  { Star End Format }
  TAbTarEnd_Star_Rec = packed record
    Prefix: array[0..AB_TAR_STAR_PREFIX_SIZE-1] of AnsiChar;
                   { 345-499, $159-1F3, prefix of file & path name, null terminated ASCII string }
    Atime : Arr12; { 476-487, $1DC-1E7, time of last access (UNIX Date in ASCII coded Octal)}
    Ctime : Arr12; { 488-499, $1E8-1F3, time of last status change (UNIX Date in ASCII coded Octal)}
    Empty : Arr12B;{ 500-512, $1F4-1FF, Empty Space }
  end;
  { When support for sparse files is added, Add another record for sparse in header }


{ Note: SizeOf(TAbTarHeaderRec) = AB_TAR_RECORDSIZE by design }
  PAbTarHeaderRec = ^TAbTarHeaderRec; { Declare pointer type for use in the list }
  TAbTarHeaderRec = packed record
    Name    : ArrName;  {   0- 99, $  0- 63, filename, null terminated ASCII string, unless length is 100 }
    Mode    : Arr8;     { 100-107, $ 64- 6B, file mode (UNIX style, ASCII coded Octal) }
    uid     : Arr8;     { 108-115, $ 6C- 73, usrid # (UNIX style, ASCII coded Octal) }
    gid     : Arr8;     { 116-123, $ 74- 7B, grpid # (UNIX style, ASCII coded Octal) }
    Size    : Arr12;    { 124-135, $ 7C- 87, size of TARred file (ASCII coded Octal) }
    ModTime : Arr12;    { 136-147, $ 88- 93, time of last modification.(UNIX Date in ASCII coded Octal)
                                             UTC time }
    ChkSum  : Arr8;     { 148-155, $ 94- 9B, checksum of header (6 bytes ASCII coded Octal, #00, #20) }
    LinkFlag: AnsiChar; {     156, $     9C, type of item, one of the Link Flag constants from above }
    LinkName: ArrName;  { 157-256, $ 9D-100, name of link, null terminated ASCII string }
    Magic   : TAbTarMagicRec;
                        { 257-264, $101-108, identifier, usually 'ustar'#00'00' }
    UsrName : array [0..AB_TAR_TUSRNAMELEN-1] of AnsiChar;
                        { 265-296, $109-128, username, null terminated ASCII string }
    GrpName : array [0..AB_TAR_TGRPNAMELEN-1] of AnsiChar;
                        { 297-328, $129-148, groupname, null terminated ASCII string }
    DevMajor: Arr8;     { 329-336, $149-150, major device ID (UNIX style, ASCII coded Octal) }
    DevMinor: Arr8;     { 337-344, $151-158, minor device ID (UNIX style, ASCII coded Octal) }
    case TAbTarHeaderFormat of{ 345-511, $159-1FF  See byte Definitions above.}
      V7_FORMAT    : ( v7    : TAbTarEnd_Empty_Rec );
      OLDGNU_FORMAT: ( gnuOld: TAbTarEnd_GNU_old_Rec );
      GNU_FORMAT   : ( gnu   : TAbTarEnd_GNU_old_Rec );
      USTAR_FORMAT : ( ustar : TAbTarEnd_UStar_Rec );
      STAR_FORMAT  : ( star  : TAbTarEnd_Star_Rec );
      POSIX_FORMAT : ( pax   : TAbTarEnd_Empty_Rec );
  end;{ end TAbTarHeaderRec }
   { There are three main types of headers we will see in a Tar file }
   { TAbTarHeaderType = (STANDARD_HDR, SPARSE_HDR, POSIX_EXTENDED_HDR); }
   { The 1st is defined above, The later two are simply organized data types. }

  TAbTarItemRec = record
    { Note: that the actual The name needs to be coherient with the name Inherited
            from parent type TAbArchiveItem }
    Name       : string;   { Path & File name. }
    Mode       : LongWord; { File Permissions }
    uid        : Integer;  { User ID }
    gid        : Integer;  { Group ID }
    Size       : Int64;    { Tared File size }
    ModTime    : Int64;    { Last time of Modification, in UnixTime }
    ChkSumPass : Boolean;  { Header Check sum found to be good }
    LinkFlag   : AnsiChar; { Link Flag, Echos the actual File Type of this Item. }
    ItemType   : TAbTarItemType; { Item Type Assigned from LinkFlag Header Types. }
    LinkName   : string;   { Link Name }
    Magic      : AnsiString;   { Magic value }
    Version    : Integer;  { Version Number }
    UsrName    : string;   { User Name, for User ID }
    GrpName    : string;   { Group Name, for Group ID }
    DevMajor   : Integer;  { Major Device ID }
    DevMinor   : Integer;  { Minor Device ID }
    { Additional Types used for holding info. }
    AccessTime : Int64;    { Time of Last Access, in UnixTime }
    ChangeTime : Int64;    { Time of Last Status Change, in UnixTime }
    ArchiveFormat: TAbTarHeaderFormat; { Type of Archive of this record }
    StreamPosition: Int64; { Pointer to the top of the item in the file. }
    Dirty      : Boolean;  { Indication if this record needs to have its headers CheckSum recalculated }
    ItemReadOnly: Boolean; { Indication if this record is READ ONLY }
    FileHeaderCount:Integer;{ Number of Headers in the Orginal TarHeaders in the File Stream }
  end;

type
  PTAbTarItem = ^TAbTarItem;
  TAbTarItem = class(TAbArchiveItem)
  private
    {  The following private members are used for Stuffing FTarItem struct }
    procedure ParseTarHeaders; { Error in header if }
    procedure DetectHeaderFormat; { Helper to stuff HeaderFormat }
    procedure GetFileNameFromHeaders; { Helper to pull name from Headers }
    procedure GetLinkNameFromHeaders; { Helper to pull name from Headers }
    function  TestCheckSum: Boolean;  { Helper to Calculate Checksum of a header. }
    procedure DoGNUExistingLongNameLink(LinkFlag: AnsiChar; I: Integer; const Value: AnsiString);
    procedure DoGNUNewLongNameLink(LinkFlag: AnsiChar; I: Integer; const Value: AnsiString);
  protected {private}
    PTarHeader: PAbTarHeaderRec;{ Points to FTarHeaderList.Items[FTarHeaderList.Count-1] }
    FTarHeaderList: TList;      { List of The Headers }
    FTarHeaderTypeList: TList;  { List of the Header Types }
    FTarItem: TAbTarItemRec;    { Data about current TAR Item }
  protected
    function GetDevMajor: Integer;
    function GetDevMinor: Integer;
    function GetGroupID: Integer;
    function GetGroupName: string;
    function GetLinkName: string;
    function GetUserID: Integer;
    function GetUserName: string;
    function GetModTime: Int64;
    function GetNumHeaders: Integer;
    function GetMagic: string;

    { All Sets shall update the headers Or add headers as needed.  }
    procedure SetDevMajor(const Value: Integer);
    procedure SetDevMinor(const Value: Integer);
    procedure SetGroupID(const Value: Integer);  { Extended Headers }
    procedure SetGroupName(const Value: string); { Extended Headers }
    procedure SetLinkFlag(Value: AnsiChar);
    procedure SetLinkName(const Value: string);  { Extended Headers }
    procedure SetUserID(const Value: Integer);   { Extended Headers }
    procedure SetUserName(const Value: string);  { Extended Headers }
    procedure SetModTime(const Value: Int64);
    Procedure SetMagic(const Value: string);
    { TODO: add support for Atime and Ctime here }

    { Overrides for Inherited Properties from type TAbArchiveItem }
    function GetCompressedSize : Int64; override;
    function GetExternalFileAttributes : LongWord; override;
    function GetFileName : string; override;
    function GetIsDirectory: Boolean; override;
    function GetIsEncrypted : Boolean; override;
    function GetLastModFileDate : Word; override;
    function GetLastModFileTime : Word; override;
    function GetLastModTimeAsDateTime: TDateTime; override;
    function GetNativeFileAttributes : LongInt; override;
    function GetNativeLastModFileTime: Longint; override;
    function GetUncompressedSize : Int64; override;

    procedure SetCompressedSize(const Value : Int64); override;       { Extended Headers }
    procedure SetExternalFileAttributes( Value : LongWord ); override;
    procedure SetFileName(const Value : string); override;            { Extended Headers }
    procedure SetIsEncrypted(Value : Boolean); override;
    procedure SetLastModFileDate(const Value : Word); override;       { Extended Headers }
    procedure SetLastModFileTime(const Value : Word); override;       { Extended Headers }
    procedure SetLastModTimeAsDateTime(const Value: TDateTime); override;
    procedure SetUncompressedSize(const Value : Int64); override;     { Extended Headers }

    procedure SaveTarHeaderToStream(AStream : TStream);
    procedure LoadTarHeaderFromStream(AStream : TStream);

    property Magic : string { Magic value }
      read GetMagic write SetMagic;
  public
  { property Name : STRING; Path & File name. Inherited from parent type TAbArchiveItem }
  {   read GetFileName write SetFileName;  overridden above}
    property Mode : LongWord   { File Permissions }
      read GetExternalFileAttributes write SetExternalFileAttributes;
    property UserID : Integer { User ID }
      read GetUserID write SetUserID;
    property GroupID : Integer { Group ID }
      read GetGroupID write SetGroupID;
    property ModTime : Int64
      read GetModTime write SetModTime;
  { property UncompressedSize/CompressedSize(Size): Int64; File size (comp/uncomp) Inherited from parent type TAbArchiveItem }
  {   read  GetUncompressedSize, GetCompressedSize;  overridden above }
  {   write SetUncompressedSize, SetCompressedSize;  overridden above }
  { property LastModFileTime/LastModFileDate(ModeTime): TDateTime; Last time of Modification Inherited from parent type TAbArchiveItem }
  {   read  GetLastModFileTime, GetLastModFileDate;  overridden above }
  {   write SetLastModFileTime, SetLastModFileDate;  overridden above }

    property CheckSumGood: Boolean
      read FTarItem.ChkSumPass; { Header Check sum found to be good }
    property LinkFlag : AnsiChar { Link Flag of File Header }
      read FTarItem.LinkFlag write SetLinkFlag;
    property LinkName : string { Link Name }
      read GetLinkName write SetLinkName;
    property UserName : string { User Name, for User ID }
      read GetUserName write SetUserName;
    property GroupName : string { Group Name, for Group ID }
      read GetGroupName write SetGroupName;
    property DevMajor : Integer { Major Device ID }
      read GetDevMajor write SetDevMajor;
    property DevMinor : Integer { Minor Device ID }
      read GetDevMinor write SetDevMinor;
    { TODO: Add support ATime and CTime }
    {AccessTime : TDateTime;} { Time of Last Access }
    {ChangeTime : TDateTime;} { Time of Last Status Change }
    { Additional Types used for holding info. }
    property ExternalFileAttributes;
    property ArchiveFormat: TAbTarHeaderFormat
      read FTarItem.ArchiveFormat write FTarItem.ArchiveFormat;
    property ItemType: TAbTarItemType
      read FTarItem.ItemType write FTarItem.ItemType;
    property ItemReadOnly:  Boolean
      read FTarItem.ItemReadOnly write FTarItem.ItemReadOnly;
    property FileHeaderCount: Integer
      read FTarItem.FileHeaderCount;
    property HeaderCount: Integer
      read GetNumHeaders;
    property StreamPosition: Int64
      read FTarItem.StreamPosition write FTarItem.StreamPosition;
    constructor Create;
    destructor Destroy; override;
  end;  { end TAbArchiveItem }


  TAbTarStreamHelper = class(TAbArchiveStreamHelper)
  private
    function FindItem: Boolean; { Tool for FindFirst/NextItem functions }
  protected
    FTarHeader      : TAbTarHeaderRec; { Speed-up Buffer only }
    FCurrItemSize   : Int64;           { Current Item size }
    FCurrItemPreHdrs: Integer;         { Number of Meta-data Headers before the Item }
  public
    destructor Destroy; override;
    procedure ExtractItemData(AStream : TStream); override;
    function FindFirstItem : Boolean; override;
    function FindNextItem : Boolean; override;
    procedure ReadHeader; override;
    procedure ReadTail; override;
    function SeekItem(Index : Integer): Boolean; override;
    procedure WriteArchiveHeader; override;
    procedure WriteArchiveItem(AStream : TStream); override;
    procedure WriteArchiveItemSize(AStream : TStream; Size: Int64);
    procedure WriteArchiveTail; override;
    function GetItemCount : Integer; override;
  end;


  TAbTarArchive = class(TAbArchive)
  private
    FArchReadOnly : Boolean;
    FArchFormat: TAbTarHeaderFormat;
  protected
    function CreateItem(const SourceFileName   : string;
                        const ArchiveDirectory : string): TAbArchiveItem;
      override;
    procedure ExtractItemAt(Index : Integer; const UseName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure TestItemAt(Index : Integer);
      override;
    function FixName(const Value: string): string;
      override;
   	function GetSupportsEmptyFolders: Boolean;
      override;

    function GetItem(Index: Integer): TAbTarItem;
    procedure PutItem(Index: Integer; const Value: TAbTarItem);

  public {methods}
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string);
      override;
    property UnsupportedTypesDetected : Boolean
      read FArchReadOnly;
    property Items[Index : Integer] : TAbTarItem
      read GetItem
      write PutItem; default;
  end;

 procedure UnixAttrsToTarAttrs(const UnixAttrs: LongWord;
                               out Permissions: LongWord; out LinkFlag: AnsiChar);
 procedure TarAttrsToUnixAttrs(const Permissions: LongWord; const LinkFlag: AnsiChar;
                               out UnixAttrs: LongWord);

function VerifyTar(Strm : TStream) : TAbArchiveType;


implementation

uses
  {$IFDEF MSWINDOWS}
  Windows, // Fix inline warnings
  {$ENDIF MSWINDOWS}
  Math, RTLConsts, SysUtils, AbVMStrm, AbExcept,
  DCOSUtils, DCClassesUtf8, DCConvertEncoding;

{ ****************** Helper functions Not from Classes Above ***************** }
function OctalToInt(const Oct : PAnsiChar; aLen : integer): Int64;
var
  i : integer;
begin
  Result := 0;

  i := 0;
  while (i < aLen) and (Oct[i] = ' ') do
    inc(i);

  if (i = aLen) then
    Exit;

  while (i < aLen) and (Oct[i] in ['0'..'7']) do begin
    Result := (Result * 8) + (Ord(Oct[i]) - Ord('0'));
    inc(i);
  end;
end;

function IntToOctal(Value : Int64): AnsiString;
const
  OctDigits  : array[0..7] of AnsiChar = '01234567';
begin
  if Value = 0 then
    Result := '0'
  else begin
    Result := '';
    while Value > 0 do begin
      Result := OctDigits[Value and 7] + Result;
      Value := Value shr 3;
    end;
  end;
end;


function CalcTarHeaderChkSum(const TarH : TAbTarHeaderRec): LongInt;
var
  HdrBuffer : PAnsiChar;
  HdrChkSum : LongInt;
  j : Integer;
begin
  { prepare for the checksum calculation }
  HdrBuffer := PAnsiChar(@TarH);
  HdrChkSum := 0;

  {calculate the checksum, a simple sum of the bytes in the header}
  for j := 0 to Pred(SizeOf(TAbTarHeaderRec)) do
    HdrChkSum := HdrChkSum + Ord(HdrBuffer[j]);

  Result := HdrChkSum;
end;

function VerifyTar(Strm : TStream) : TAbArchiveType;
{ assumes Tar positioned correctly for test of item }
var
  TarItem  : TAbTarItem;
  StartPos : Int64;
begin
  StartPos := Strm.Position;
  try
    { Verifies that the header checksum is valid, and Item type is understood.
      This does not mean that extraction is supported. }
    TarItem := TAbTarItem.Create;
    try
      { get current Tar Header }
      TarItem.LoadTarHeaderFromStream(Strm);
      if TarItem.CheckSumGood then
        Result := atTar
      else
        Result := atUnknown;
    finally
      TarItem.Free;
    end;
  except
    on EReadError do
      Result := atUnknown;
  end;
  Strm.Position := StartPos;
end;

function PadString(const S : AnsiString; Places : Integer) : AnsiString;
{
Pads a string (S) with one right space and as many left spaces as
needed to fill Places

If length S greater than Places, just returns S

Some TAR utilities evidently expect Octal numeric fields to be in
this format
}
begin
  if Length(S) >= LongInt(Places) then
    Result := S
  else begin
    Result := S + ' ';
    Result := StringOfChar(AnsiChar(' '), Places - Length(Result)) + Result;
  end;
end;

{ Round UP to the nearest Tar Block Boundary. }
function RoundToTarBlock(Size: Int64) : Int64;
begin
  Result := (Size + (AB_TAR_RECORDSIZE - 1)) and
             not (AB_TAR_RECORDSIZE - 1);
end;

procedure UnixAttrsToTarAttrs(const UnixAttrs: LongWord;
                              out Permissions: LongWord; out LinkFlag: AnsiChar);
begin
  case (UnixAttrs and $F000) of
    AB_FMODE_SOCKET:
      ;
    AB_FMODE_FILELINK:
      LinkFlag := AB_TAR_LF_SYMLINK;
    AB_FMODE_FILE2:
      LinkFlag := AB_TAR_LF_NORMAL;
    AB_FMODE_BLOCKSPECFILE:
      LinkFlag := AB_TAR_LF_BLK;
    AB_FMODE_DIR:
      LinkFlag := AB_TAR_LF_DIR;
    AB_FMODE_CHARSPECFILE:
      LinkFlag := AB_TAR_LF_CHR;
    AB_FMODE_FIFO:
      LinkFlag := AB_TAR_LF_FIFO;
    AB_FMODE_FILE:
      LinkFlag := AB_TAR_LF_NORMAL;
    else
      LinkFlag := AB_TAR_LF_OLDNORMAL;
  end;

  Permissions := (UnixAttrs and $0FFF);
end;
{ -------------------------------------------------------------------------- }
procedure TarAttrsToUnixAttrs(const Permissions: LongWord; const LinkFlag: AnsiChar;
                              out UnixAttrs: LongWord);
begin
  case LinkFlag of
    AB_TAR_LF_OLDNORMAL:
      UnixAttrs := AB_FMODE_FILE;
    AB_TAR_LF_NORMAL:
      UnixAttrs := AB_FMODE_FILE2;
    AB_TAR_LF_SYMLINK:
      UnixAttrs := AB_FMODE_FILELINK;
    AB_TAR_LF_BLK:
      UnixAttrs := AB_FMODE_BLOCKSPECFILE;
    AB_TAR_LF_DIR:
      UnixAttrs := AB_FMODE_DIR;
    AB_TAR_LF_CHR:
      UnixAttrs := AB_FMODE_CHARSPECFILE;
    AB_TAR_LF_FIFO:
      UnixAttrs := AB_FMODE_FIFO;
    else
      UnixAttrs := AB_FMODE_FILE;
  end;

  UnixAttrs := UnixAttrs or (Permissions and $0FFF);
end;

{ ****************************** TAbTarItem ********************************** }
constructor TAbTarItem.Create;
begin
  inherited Create;
  FTarHeaderList := TList.Create;
  FTarHeaderTypeList := TList.Create;
  GetMem(PTarHeader, AB_TAR_RECORDSIZE); { PTarHeader is our new Header }
  FillChar(PTarHeader^, AB_TAR_RECORDSIZE, #0);
  FTarHeaderList.Add(PTarHeader);
  FTarHeaderTypeList.Add(Pointer(FILE_HEADER));
  FTarItem.FileHeaderCount := 1;
  { set defaults }
  FTarItem.ArchiveFormat := UNKNOWN_FORMAT;

  FileName := '';
  Mode := AB_FPERMISSION_GENERIC;
  UserID := 0;
  GroupID := 0;
  UncompressedSize := 0;
  { ModTime }
  LinkFlag := AB_TAR_LF_OLDNORMAL;
  { Link Name }
  PTarHeader.Magic.gnuOld := AB_TAR_MAGIC_V7_NONE; { Default to GNU type }
  UserName := '';
  GroupName := '';
  DevMajor := 0;
  DevMinor := 0;
  { TODO: atime, ctime }
  FTarItem.ItemType := SUPPORTED_ITEM;
  FTarItem.Dirty := True; { Checksum needs to be generated }
  FTarItem.ItemReadOnly := False;
end;

destructor TAbTarItem.Destroy;
var
  i : Integer;
begin
  if Assigned(FTarHeaderList) then
  begin
    for i := 0 to FTarHeaderList.Count - 1 do
      FreeMem(FTarHeaderList.Items[i]); { This list holds PAbTarHeaderRec's }
    FTarHeaderList.Free;
  end;
  FTarHeaderTypeList.Free;
  inherited Destroy;
end;

function TAbTarItem.GetCompressedSize: Int64;
{ TAR includes no internal compression, returns same value as GetUncompressedSize }
begin
  Result := FTarItem.Size;
end;

function TAbTarItem.GetDevMajor: Integer;
begin
  Result := FTarItem.DevMajor;
end;

function TAbTarItem.GetDevMinor: Integer;
begin
  Result := FTarItem.DevMinor;
end;

function TAbTarItem.GetExternalFileAttributes: LongWord;
begin
  TarAttrsToUnixAttrs(FTarItem.Mode, FTarItem.LinkFlag, Result);
end;

function TAbTarItem.GetFileName: string;
begin
  Result := FTarItem.Name; { Inherited String from Parent Class }
end;

function TAbTarItem.GetGroupID: Integer;
begin
  Result := FTarItem.gid;
end;

function TAbTarItem.GetGroupName: string;
begin
  Result := FTarItem.GrpName;
end;

function TAbTarItem.GetIsDirectory: Boolean;
begin
  Result := (LinkFlag = AB_TAR_LF_DIR);
end;

function TAbTarItem.GetIsEncrypted: Boolean;
begin
  { TAR has no native encryption }
  Result := False;
end;

function TAbTarItem.GetLastModFileDate: Word;
begin
  { convert to local DOS file Date }
  Result := LongRec(AbDateTimeToDosFileDate(LastModTimeAsDateTime)).Hi;
end;

function TAbTarItem.GetLastModFileTime: Word;
begin
  { convert to local DOS file Time }
  Result := LongRec(AbDateTimeToDosFileDate(LastModTimeAsDateTime)).Lo;
end;

function TAbTarItem.GetLastModTimeAsDateTime: TDateTime;
begin
  Result := AbUnixTimeToLocalDateTime(FTarItem.ModTime);
end;

function TAbTarItem.GetNativeLastModFileTime: Longint;
{$IFDEF MSWINDOWS}
var
  DateTime: TDateTime;
{$ENDIF}
begin
  Result   := Self.ModTime;

{$IFDEF MSWINDOWS}
  DateTime := AbUnixTimeToLocalDateTime(Result);
  Result   := AbDateTimeToDosFileDate(DateTime);
{$ENDIF}
end;
function TAbTarItem.GetLinkName: string;
begin
  Result := FTarItem.LinkName;
end;

function TAbTarItem.GetMagic: string;
begin
  Result := string(FTarItem.Magic);
end;

function TAbTarItem.GetNativeFileAttributes : LongInt;
begin
  Result := GetExternalFileAttributes;
{$IFDEF MSWINDOWS}
  Result := AbUnix2DosFileAttributes(Result);
{$ENDIF}
end;

function TAbTarItem.GetUncompressedSize: Int64;
{ TAR includes no internal compression, returns same value as GetCompressedSize }
begin
  Result := FTarItem.Size;
end;

function TAbTarItem.GetUserID: Integer;
begin
  Result := FTarItem.uid;
end;

function TAbTarItem.GetUserName: string;
begin
  Result := FTarItem.UsrName;
end;

function TAbTarItem.GetModTime: Int64;
begin
  Result := FTarItem.ModTime;
end;

{ Get Number of tar headers currently for this item }
function  TAbTarItem.GetNumHeaders: Integer;
begin
  Result := FTarHeaderList.Count;
end;

{ Takes data from Supported Header types stored in TAbTarItem.FTarHeaderList }
{ and updates values in the TAbTarItem.FTarItem.X }

procedure TAbTarItem.DetectHeaderFormat;
begin
  if FTarItem.ArchiveFormat <> UNKNOWN_FORMAT then
    Exit;{ We have already set the format. }
  { In the previous header parsing if pax headers are detected the format is changed }
  { GNU_FORMAT is detected by the presence of GNU extended headers. }

  { These detections are similar to GNU tar's. }
  if (PTarHeader.Magic.value = AB_TAR_MAGIC_VAL) then
  begin { We have one of three types, STAR_FORMAT, USTAR_FORMAT, POSIX_FORMAT }
    { Detect STAR format.  Leave disabled until explicit STAR support is added. }
    {if (PTarHeader.star.Prefix[130] = #00) and
       (PTarHeader.star.Atime[0] in ['0'..'7']) and
       (PTarHeader.star.Atime[11] = #20) and
       (PTarHeader.star.Ctime[0]in ['0'..'7']) and
       (PTarHeader.star.Ctime[11] = #20) then
    begin
      FTarItme.ArchiveType := STAR_FORMAT;
    end }
    { else if } { POSIX uses the existance of x headers }

    { This can define false positives, Pax headers/ STAR format could be detected as this }
    FTarItem.ArchiveFormat := USTAR_FORMAT;
  end
  else if (PTarHeader.Magic.gnuOld = AB_TAR_MAGIC_GNUOLD) then
  begin
    FTarItem.ArchiveFormat := OLDGNU_FORMAT;
  end
  else { V7 uses AB_TAR_LF_OLDNORMAL linkflag, has no magic field & no Usr/Grp Names }
  begin
    FTarItem.ArchiveFormat := V7_FORMAT; { Lowest Common Denominator }
  end;
end;

{ Extract the file name from the headers }
procedure TAbTarItem.GetFileNameFromHeaders;
var
  I, J : Integer;
  PHeader: PAbTarHeaderRec;
  FoundName: Boolean;
  NameLength : Int64;
  NumMHeaders: integer;
  ExtraName: integer;
  RawFileName, TempStr: AnsiString;
begin
 {  UNKNOWN_FORMAT, V7_FORMAT, OLDGNU_FORMAT, GNU_FORMAT, USTAR_FORMAT, STAR_FORMAT, POSIX_FORMAT }
  FoundName := False;
  I := 0;
  while (not FoundName) and (I <= (FTarHeaderList.Count - 1)) do
  begin
    PHeader := FTarHeaderList.Items[I];
    if PHeader.LinkFlag = AB_TAR_LF_LONGNAME then
    begin
      FoundName := True;
      RawFileName := '';
      NameLength := OctalToInt(PHeader.Size, SizeOf(PHeader.Size));
      NumMHeaders := NameLength div AB_TAR_RECORDSIZE;
      ExtraName := NameLength mod AB_TAR_RECORDSIZE; { Chars in the last Header }
      { NumMHeaders should never be zero }
      { It appears that it is not null terminated in the blocks }
      for J := 1 to NumMHeaders do
      begin
        { Copy entire content of Header to String }
        PHeader := FTarHeaderList.Items[I+J];
        SetString(TempStr, PAnsiChar(PHeader), AB_TAR_RECORDSIZE);
        RawFileName := RawFileName + TempStr;
      end;
      if ExtraName <> 0 then
      begin
        PHeader := FTarHeaderList.Items[I+NumMHeaders+1];
        SetString(TempStr, PAnsiChar(PHeader), ExtraName-1);
        RawFileName := RawFileName + TempStr;
      end
      else { We already copied the entire name, but the string is still null terminated. }
      begin
        { Removed the last zero }
        SetLength(RawFileName, (Length(RawFileName)-1));
      end;
    end { end long filename link flag }
    else
      I := I + 1;
  end; { End While }

  if not FoundName then
  begin
    if (FTarItem.ArchiveFormat = USTAR_FORMAT) and
       (PTarHeader.ustar.Prefix[0] <> #0) then
      RawFileName := PTarHeader.ustar.Prefix+'/'+PTarHeader.Name
    else
      { V7_FORMAT, OLDGNU_FORMAT }
      RawFileName := PTarHeader.Name;
  end; { End not FoundName }

  FTarItem.Name := CeRawToUtf8(RawFileName);
end;

{ Extract the file name from the headers }
procedure TAbTarItem.GetLinkNameFromHeaders;
var
  I, J : Integer;
  PHeader: PAbTarHeaderRec;
  FoundName: Boolean;
  NameLength : Int64;
  NumMHeaders: integer;
  ExtraName: integer;
  RawLinkName, TempStr: AnsiString;
begin
 {  UNKNOWN_FORMAT, V7_FORMAT, OLDGNU_FORMAT, GNU_FORMAT, USTAR_FORMAT, STAR_FORMAT, POSIX_FORMAT }
  PHeader := nil;
  FoundName := False;
  I := 0;
  { Note that: FTarHeaderList.Count <= 1, always }
  while (not FoundName) and (I <= (FTarHeaderList.Count - 1)) do
  begin
    PHeader := FTarHeaderList.Items[I];
    if PHeader.LinkFlag = AB_TAR_LF_LONGLINK then
    begin
      FoundName := True;
      RawLinkName := '';
      NameLength := OctalToInt(PHeader.Size, SizeOf(PHeader.Size));
      NumMHeaders := NameLength div AB_TAR_RECORDSIZE;
      ExtraName := NameLength mod AB_TAR_RECORDSIZE; { Chars in the last Header }
      { NumMHeaders should never be zero }
      { It appears that it is not null terminated in the blocks }
      for J := 1 to NumMHeaders do
      begin
        { Copy entire content of Header to String }
        PHeader := FTarHeaderList.Items[I+J];
        SetString(TempStr, PAnsiChar(PHeader), AB_TAR_RECORDSIZE);
        RawLinkName := RawLinkName + TempStr;
      end;
      if ExtraName <> 0 then
      begin
        PHeader := FTarHeaderList.Items[I+NumMHeaders+1];
        SetString(TempStr, PAnsiChar(PHeader), ExtraName-1);
        RawLinkName := RawLinkName + TempStr;
      end
      else { We already copied the entire name, but the string is still null terminated. }
      begin
        { Removed the last zero }
        SetLength(RawLinkName, (Length(RawLinkName)-1));
      end;
    end { end long filename link flag }
    else
      I := I + 1;
  end; { End While }

  if not FoundName then
    RawLinkName := PHeader.LinkName;

  FTarItem.LinkName := CeRawToUtf8(RawLinkName);
end;

{ Return True if CheckSum passes out. }
function TAbTarItem.TestCheckSum : Boolean;
var
  TarChkSum : LongInt;
  TarChkSumArr : Arr8; { ChkSum field is Arr8 }
  PHeader: PAbTarHeaderRec;
  I: Integer;
begin
  Result := True;
  { Check sums are in valid headers but NOT in the data headers. }
  for I := 0 to FTarHeaderList.Count - 1 do
  begin
    if TAbTarHeaderType(FTarHeaderTypeList.Items[I]) in [FILE_HEADER, META_DATA_HEADER] then
    begin
      PHeader := FTarHeaderList.Items[i];
      { Save off old Check sum }
      Move(PHeader.ChkSum, TarChkSumArr, SizeOf(PHeader.ChkSum));
      TarChkSum := OctalToInt(TarChkSumArr, SizeOf(TarChkSumArr));
      { Set to Generator Value }
      PHeader.ChkSum := AB_TAR_CHKBLANKS;
      if CalcTarHeaderChkSum(PHeader^) <> TarChkSum then
        Result := False; { Pass unless one miss-compares }
      { Save back old checksum }
      Move(TarChkSumArr, PHeader.ChkSum, SizeOf(TarChkSumArr));
    end;
  end;
end;

procedure TAbTarItem.ParseTarHeaders;
begin
  { The final index is the Item index }
  DetectHeaderFormat;
  { Long term this parsing is not correct, as the values in extended headers
    override the later values in this header }
  FTarItem.Mode := OctalToInt(PTarHeader.Mode, SizeOf(PTarHeader.Mode));
  FTarItem.uid := OctalToInt(PTarHeader.uid, SizeOf(PTarHeader.uid)); { Extended in PAX Headers }
  FTarItem.gid := OctalToInt(PTarHeader.gid, SizeOf(PTarHeader.gid)); { Extended in PAX Headers }
  FTarItem.Size := OctalToInt(PTarHeader.Size, SizeOf(PTarHeader.Size)); { Extended in PAX Headers }
  { ModTime should be an Int64 but no tool support, No issues until Feb 6th, 2106 :) }
  { ModTime is Extended in PAX Headers }
  FTarItem.ModTime := OctalToInt(PTarHeader.ModTime, SizeOf(PTarHeader.ModTime));
  FTarItem.ChkSumPass := TestCheckSum();
  FTarItem.LinkFlag := PTarHeader.LinkFlag;
  GetLinkNameFromHeaders; { Extended in PAX Headers }
  FTarItem.Magic := PTarHeader.Magic.value;
  FTarItem.Version := OctalToInt(PTarHeader.Magic.version, SizeOf(PTarHeader.Magic.version));
  FTarItem.UsrName := string(PTarHeader.UsrName); { Extended in PAX Headers }
  FTarItem.GrpName := string(PTarHeader.GrpName); { Extended in PAX Headers }
  FTarItem.DevMajor := OctalToInt(PTarHeader.DevMajor, SizeOf(PTarHeader.DevMajor));
  FTarItem.DevMinor := OctalToInt(PTarHeader.DevMinor, SizeOf(PTarHeader.DevMinor));
  GetFileNameFromHeaders;
  { FTarItem.ArchiveFormat;  Already stuffed }
  { FTarItem.StreamPosition: Already Stuffed }
  { FTarItem.Dirty; Stuffed upon creaction }
end;

procedure TAbTarItem.LoadTarHeaderFromStream(AStream: TStream);
var
  NumMHeaders : Integer;
  I : Integer;
  FoundItem : Boolean;
begin
  { Note: The SizeOf(TAbTarHeaderRec) = AB_TAR_RECORDSIZE }
  { We should expect FindNext/FirstItem, and next check for bounds. }
  if FTarHeaderList.Count > 0 then
  begin { We're Going to stomp over the headers that are already present }
    { We need to destory the memory we've used }
    PTarHeader := nil;
    for i := 0 to FTarHeaderList.Count - 1 do
      FreeMem(FTarHeaderList.Items[i]); { This list holds PAbTarHeaderRec's }
    FTarHeaderList.Clear;
    FTarHeaderTypeList.Clear;
    FTarItem.FileHeaderCount := 0;
    { All pointers should now be removed from those headers }
  end;
  { Now lets start filling up that list. }
  FTarItem.ItemType := UNKNOWN_ITEM; { We don't know what we have yet }
  FoundItem := False;
  while not FoundItem do
  begin
    { Create a Header to be Stored in the Items List }
    GetMem(PTarHeader, AB_TAR_RECORDSIZE);
    AStream.ReadBuffer(PTarHeader^, AB_TAR_RECORDSIZE);
    FTarHeaderList.Add(PTarHeader); { Store the Header to the list }
    { Parse header based on LinkFlag }
    if PTarHeader.LinkFlag in (AB_SUPPORTED_MD_HEADERS+AB_UNSUPPORTED_MD_HEADERS) then
    begin { This Header type is in the Set of un/supported Meta data type headers }
      if PTarHeader.LinkFlag in AB_UNSUPPORTED_MD_HEADERS then
        FTarItem.ItemReadOnly := True; { We don't fully support this meta-data type }
      if (PTarHeader.LinkFlag in AB_PAX_MD_HEADERS) and (PTarHeader.Magic.value = AB_TAR_MAGIC_VAL) then
        FTarItem.ArchiveFormat := POSIX_FORMAT; { We have a POSIX_FORMAT, has x headers, and Magic matches }
      if PTarHeader.LinkFlag in AB_GNU_MD_HEADERS then
        FTarItem.ArchiveFormat := OLDGNU_FORMAT; { We have a OLDGNU_FORMAT, has L/K headers }
      { There can be a unknown number of Headers of data }
      { We are for sure going to read at least one more header, but are we going to read more than that? }
      FTarHeaderTypeList.Add(Pointer(META_DATA_HEADER));
      NumMHeaders := Ceil(OctalToInt(PTarHeader.Size, SizeOf(PTarHeader.Size)) / AB_TAR_RECORDSIZE);
      { NumMHeasder should never be zero }
      for I := 1 to NumMHeaders do
      begin
        GetMem(PTarHeader, AB_TAR_RECORDSIZE); { Create a new Header }
        AStream.ReadBuffer(PTarHeader^, AB_TAR_RECORDSIZE); { Get the Meta Data }
        FTarHeaderList.Add(PTarHeader); { Store the Header to the list }
        FTarHeaderTypeList.Add(Pointer(MD_DATA_HEADER));
      end;
      { Loop and reparse }
    end
    else if PTarHeader.LinkFlag in AB_SUPPORTED_F_HEADERS then
    begin { This Header type is in the Set of supported File type Headers }
      FoundItem := True; { Exit Criterion }
      FTarItem.ItemType := SUPPORTED_ITEM;
      if FTarItem.ItemReadOnly then            { Since some of the Headers are read only. }
        FTarItem.ItemType := UNSUPPORTED_ITEM; { This Item is unsupported }
      FTarHeaderTypeList.Add(Pointer(FILE_HEADER));
    end
    else if PTarHeader.LinkFlag in AB_UNSUPPORTED_F_HEADERS then
    begin { This Header type is in the Set of unsupported File type Headers }
      FoundItem := True; { Exit Criterion }
      FTarItem.ItemType := UNSUPPORTED_ITEM;
      FTarHeaderTypeList.Add(Pointer(FILE_HEADER));
    end
    else { These are unknown header types }
    begin { Note: Some of these unknown types could have known Meta-data headers }
      FoundItem := True;
      FTarItem.ItemType := UNKNOWN_ITEM;
      FTarHeaderTypeList.Add(Pointer(UNKNOWN_HEADER));
    end;{ end LinkFlag parsing }
  end; { end Found Item While }
  { PTarHeader points to FTarHeaderList.Items[FTarHeaderList.Count-1]; }

  { Re-wind the Stream back to the begining of this Item inc. all headers }
  AStream.Seek(-(FTarHeaderList.Count*AB_TAR_RECORDSIZE), soFromCurrent);
  { AStream.Position := FTarItem.StreamPosition; } { This should be equivalent as above }
  FTarItem.FileHeaderCount := FTarHeaderList.Count;
  if FTarItem.ItemType <> UNKNOWN_ITEM then
  begin
    ParseTarHeaders; { Update FTarItem values }
    FFileName := FTarItem.Name; {FTarHeader.Name;}
//  FDiskFileName := FileName;
//  AbUnfixName(FDiskFileName);
  end;
  Action := aaNone;
  Tagged := False;
end;


{ ****************** BEGIN SET ********************** }

procedure TAbTarItem.SaveTarHeaderToStream(AStream: TStream);
var
  i : Integer;
  j : Integer;
  PHeader :  PAbTarHeaderRec;
  HdrChkSum : Integer;
  HdrChkStr : AnsiString;
  HdrBuffer : PAnsiChar;
  SkipNextChkSum: Integer;
  SkipChkSum: Boolean;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { Note: The SizeOf(TAbTarHeaderRec) = AB_TAR_RECORDSIZE }
  if FTarItem.Dirty then
    SkipNextChkSum := 0
  else
    SkipNextChkSum := FTarHeaderList.Count; { Don't recalc any chkSums }

  { The first header in the Item list must have a checksum calculation }
  for i := 0 to (FTarHeaderList.Count-1) do
  begin
    SkipChkSum := False;
    PHeader := FTarHeaderList.Items[i];
    if (SkipNextChkSum = 0) then
    begin { We need to parse this header }
      if PHeader.LinkFlag in (AB_SUPPORTED_MD_HEADERS+AB_UNSUPPORTED_MD_HEADERS) then
      begin { We have a Meta-Data Header, Calculate how many headers to skip. }
        { These meta-data headers have non-Header buffers after this Header }
        SkipNextChkSum := Ceil(OctalToInt(PHeader.Size, SizeOf(PHeader.Size)) / AB_TAR_RECORDSIZE);
        { Ceil will mandate one run through, and will handle 512 correctly }
      end
      else if PHeader.LinkFlag in AB_SUPPORTED_F_HEADERS then
      begin
        SkipNextChkSum := 0;
      end
      else
      begin { Un-Supported Header type, Copy but do nothing to the data }
        SkipNextChkSum := 0;
        SkipChkSum := True;
      end;{ end LinkFlag parsing }
    end
    else
    begin { Do not calcuate the check sum on this meta Data header buffer }
      SkipNextChkSum := SkipNextChkSum - 1;
      SkipChkSum := True;
    end;{ end SkipNextChkSum }

    if not SkipChkSum then
    begin { We are Calculating the Checksum for this Header }
      {Tar ChkSum is "odd" The check sum field is filled with #20 chars as empty }
      { ChkSum field itself is #20'd and has an effect on the sum }
      PHeader.ChkSum := AB_TAR_CHKBLANKS;
      { Set up the buffers }
      HdrBuffer := PAnsiChar(PHeader);
      HdrChkSum := 0;
      { Calculate the checksum, a simple sum of the bytes in the header }
      for j := 0 to (AB_TAR_RECORDSIZE-1) do
        HdrChkSum := HdrChkSum + Ord(HdrBuffer[j]);
      { set the checksum in the header }
      HdrChkStr := PadString(IntToOctal(HdrChkSum), SizeOf(PHeader.ChkSum));
      Move(HdrChkStr[1], PHeader.ChkSum, Length(HdrChkStr));
    end; { end Skip Check Sum }
    { write header to the file }
    AStream.Write(PHeader^, AB_TAR_RECORDSIZE);
  end; { End for the number of headers in the list }
  { Updated here as the stream is now updated to the latest number of headers }
  FTarItem.FileHeaderCount := FTarHeaderList.Count;
end;

procedure TAbTarItem.SetCompressedSize(const Value: Int64);
var
  S : AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { Size is extendable in PAX Headers, Remember PAX extended Header Over Rule File Headers }
  FTarItem.Size := Value; { Store our Vitrual Copy }
  S := PadString(IntToOctal(Value), SizeOf(Arr12));{ Stuff to header }
  Move(S[1], PTarHeader.Size, Length(S));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetDevMajor(const Value: Integer);
var
  S : AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { Dev Major and Minor are Only used for AB_TAR_LF_CHR, AB_TAR_LF_BLK }
  { Otherwise they are stuffed with #00 }
  FTarItem.DevMajor := Value; { Store to the struct }
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], PTarHeader.DevMajor, Length(S));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetDevMinor(const Value: Integer);
var
  S : AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { Dev Major and Minor are Only used for AB_TAR_LF_CHR, AB_TAR_LF_BLK }
  { Otherwise they are stuffed with #00 }
  FTarItem.DevMinor := Value;
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], PTarHeader.DevMinor, Length(S));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetExternalFileAttributes(Value: LongWord);
var
  S : AnsiString;
  I: Integer;
  Permissions: LongWord;
  ALinkFlag: AnsiChar;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;

  UnixAttrsToTarAttrs(Value, Permissions, ALinkFlag);

  FTarItem.Mode := Permissions;
  S := PadString(IntToOctal(Permissions), SizeOf(Arr8));
  for I := 0 to FTarHeaderList.Count - 1 do
    if TAbTarHeaderType(FTarHeaderTypeList.Items[I]) in [FILE_HEADER, META_DATA_HEADER] then
      Move(S[1], PAbTarHeaderRec(FTarHeaderList.Items[I]).Mode, Length(S));

  Self.LinkFlag := ALinkFlag;    // also updates headers
  FTarItem.Dirty := True;
end;

{ Add/Remove Headers as needed To/From Existing GNU Long (Link/Name) TarItems }
procedure TAbTarItem.DoGNUExistingLongNameLink(LinkFlag: AnsiChar; I: Integer; const Value: AnsiString);
var
  PHeader: PAbTarHeaderRec;
  J: Integer;
  OldNameLength: Integer;
  TotalOldNumHeaders: Integer;
  TotalNewNumHeaders: Integer;
  NumHeaders: Integer;
  ExtraName: Integer;
  tempStr: AnsiString;
begin
  PHeader := FTarHeaderList.Items[I];

  { Need this data from the old header }
  OldNameLength := OctalToInt(PHeader.Size, SizeOf(PHeader.Size));{ inlcudes Null termination }
  { Length(FTarItem.Name)+1 = OldNameLength; }{ This should be true, always }

  { Save off the new Length, so we don't have to change the pointers later. }
  tempStr := PadString(IntToOctal(Length(Value)+1), SizeOf(PHeader.Size));
  Move(tempStr[1], PHeader.Size, Length(tempStr));

  TotalOldNumHeaders := Ceil(OldNameLength / AB_TAR_RECORDSIZE);
  TotalNewNumHeaders := Ceil((Length(Value)+1) / AB_TAR_RECORDSIZE);{ Null terminated }
  {Length(Value)+1: 1-512 = 1, 513-1024 = 2 ... }
  J := TotalOldNumHeaders - TotalNewNumHeaders;
  while J <> 0 do
  begin
    if J > 0 then
    begin { Old > New, Have to many Headers, Remove }
      FreeMem(FTarHeaderList.Items[I+J]); { Free the Memory for the extra Header }
      FTarHeaderList.Delete(I+J);         { Delete the List index }
      FTarHeaderTypeList.Delete(I+J);
      J := J - 1;
    end
    else { if J < 0 then }
    begin { Old < New, Need more Headers, Insert }
      GetMem(PHeader, AB_TAR_RECORDSIZE);
      FTarHeaderList.Insert(I+1,PHeader);{ Insert: Inserts at index }
      FTarHeaderTypeList.Insert(I+1,Pointer(MD_DATA_HEADER));{ We are only adding MD Data headers here }
      J := J + 1;
    end;
  end;{ end numHeaders while }
  { Yes, GNU Tar adds a Nil filled MD data header if Length(Value) mod AB_TAR_RECORDSIZE = 0 }
  NumHeaders := (Length(Value)+1) div AB_TAR_RECORDSIZE; { Include Null terminator }
  ExtraName := (Length(Value)+1) mod AB_TAR_RECORDSIZE; { Chars in the last Header }
  { Now we have the number of headers set up, stuff the name in the Headers }
  TempStr := AnsiString(Value);
  for J := 1 to NumHeaders do
  begin
    { Copy entire next AB_TAR_RECORDSIZE bytes of tempString to content of Header }
    { There may only be AB_TAR_RECORDSIZE-1 bytes if this is the last rounded header }
    PHeader := FTarHeaderList.Items[I+J];
    Move(TempStr[1], PHeader^, AB_TAR_RECORDSIZE);
    if Length(TempStr) >= AB_TAR_RECORDSIZE then
      Delete(TempStr, 1, AB_TAR_RECORDSIZE);{ Crop string }
  end;
  if ExtraName <> 0 then
  begin
    { Copy whatever is left in tempStr into the rest of the buffer }
    PHeader := FTarHeaderList.Items[I+NumHeaders+1];
    FillChar(PHeader^, AB_TAR_RECORDSIZE, #0); { Zero the whole block }
    Move(TempStr[1], PHeader^, ExtraName-1); { The string is null terminated }
  end
  else { We already copied the entire name, but it must be null terminated }
  begin
    FillChar(Pointer(PtrInt(PHeader)+AB_TAR_RECORDSIZE-1)^, 1, #0); { Zero rest of the block }
  end;

  { Finally we need to stuff the file type Header. }
  { Note: Value.length > AB_TAR_NAMESIZE(100) }
  if LinkFlag = AB_TAR_LF_LONGNAME then
    Move(Value[1], PTarHeader.Name, AB_TAR_NAMESIZE)
  else
    Move(Value[1], PTarHeader.LinkName, AB_TAR_NAMESIZE);
end;


{ Always inserts the L/K Headers at index 0+ }
procedure TAbTarItem.DoGNUNewLongNameLink(LinkFlag: AnsiChar; I: Integer; const Value: AnsiString);
var
  PHeader: PAbTarHeaderRec;
  J: Integer;
  NumHeaders: Integer;
  ExtraName: Integer;
  tempStr: AnsiString;
begin
  { We have a GNU_FORMAT, and no L/K Headers.}
  { Add a new MD Header and MD Data Headers }
  { Make an L/K header }
  GetMem(PHeader, AB_TAR_RECORDSIZE);
  FTarHeaderList.Insert(I, PHeader);{ Insert: Inserts at base index }
  FTarHeaderTypeList.Insert(I, Pointer( META_DATA_HEADER));{ This is the L/K Header }
  FillChar(PHeader^, AB_TAR_RECORDSIZE, #0); { Zero the whole block }
  StrPCopy(PHeader.Name, AB_TAR_L_HDR_NAME); { Stuff L/K String Name }
  StrPCopy(PHeader.Mode, AB_TAR_L_HDR_ARR8_0); { Stuff zeros }
  StrPCopy(PHeader.uid, AB_TAR_L_HDR_ARR8_0);  { Stuff zeros }
  StrPCopy(PHeader.gid, AB_TAR_L_HDR_ARR8_0);  { Stuff zeros }
  tempStr := PadString(IntToOctal(Length(Value)+1), SizeOf(PHeader.Size)); { Stuff Size }
  Move(tempStr[1], PHeader.Size, Length(tempStr));
  StrPCopy(PHeader.ModTime, AB_TAR_L_HDR_ARR12_0);  { Stuff zeros }
  { Check sum will be calculated as the Dirty flag is in caller. }
  PHeader.LinkFlag := LinkFlag;  { Stuff Link FlagSize }
  StrPCopy(PHeader.Magic.gnuOld, AB_TAR_MAGIC_GNUOLD); { Stuff the magic }
  StrPCopy(PHeader.UsrName, AB_TAR_L_HDR_USR_NAME);
  StrPCopy(PHeader.GrpName, AB_TAR_L_HDR_GRP_NAME);
  { All else stays as Zeros. }
  { Completed with L/K Header }

  { OK, now we need to add the proper number of MD Data Headers, and intialize to new name }
  { Yes, GNU Tar adds an extra Nil filled MD data header if Length(Value) mod AB_TAR_RECORDSIZE = 0 }
  NumHeaders := Ceil((Length(Value)+1) / AB_TAR_RECORDSIZE); { Include Null terminator }
  ExtraName := (Length(Value)+1) mod AB_TAR_RECORDSIZE; { Chars in the last Header }
  { Now we have the number of headers set up, stuff the name in the Headers }
  TempStr := AnsiString(Value);
  for J := 1 to NumHeaders-1 do
  begin
    { Make a buffer, and copy entire next AB_TAR_RECORDSIZE bytes of tempStr to content of Header }
    { There may only be AB_TAR_RECORDSIZE-1 bytes if this is the last rounded header }
    GetMem(PHeader, AB_TAR_RECORDSIZE);
    FTarHeaderList.Insert(J+I, PHeader);
    FTarHeaderTypeList.Insert(J+I, Pointer(MD_DATA_HEADER));{ We are adding MD Data headers here }
    Move(TempStr[1], PHeader^, AB_TAR_RECORDSIZE);
    if Length(TempStr) >= AB_TAR_RECORDSIZE then
      Delete(TempStr, 1, AB_TAR_RECORDSIZE);{ Crop string }
  end;
  if ExtraName <> 0 then
  begin
    { Copy what ever is left in tempStr into the rest of the buffer }
    { Create the last MD Data Header }
    GetMem(PHeader, AB_TAR_RECORDSIZE);
    FTarHeaderList.Insert(I+NumHeaders, PHeader);{ Insert: Inserts at base index }
    FTarHeaderTypeList.Insert(I+NumHeaders, Pointer(MD_DATA_HEADER));{ We are only adding MD Data headers here }
    FillChar(PHeader^, AB_TAR_RECORDSIZE, #0); { Zero the whole block }
    Move(TempStr[1], PHeader^, ExtraName-1); { The string is null terminated in the header }
  end
  else { We already copied the entire name, but it must be null terminated }
  begin
    FillChar(Pointer(PtrInt(PHeader)+AB_TAR_RECORDSIZE-1)^, 1, #0); { Zero rest of the block }
  end;

  { Finally we need to stuff the file type Header. }
  { Note: Value.length > AB_TAR_NAMESIZE(100) }
  if LinkFlag = AB_TAR_LF_LONGNAME then
    Move(Value[1], PHeader.Name, AB_TAR_NAMESIZE)
  else
    Move(Value[1], PHeader.LinkName, AB_TAR_NAMESIZE);
end;

procedure TAbTarItem.SetFileName(const Value: string);
var
  FoundMetaDataHeader: Boolean;
  PHeader: PAbTarHeaderRec;
  I, J: Integer;
  TotalOldNumHeaders: Integer;
  RawFileName: AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do  Not Save }
    Exit;
  { Assume ItemReadOnly is set for all Unsupported Type. }

  { Cases:
    New File Name is short, Length <= 100,
      All formats: Zero Name field and move new name to field.
      V7: Work complete, 1 header
      USTAR: zero prefix field, 1 Header
      OLD_GNU & GNU: Remove old name headers, 1 header.
      STAR & PAX: And should not yet get here.
    New File Name is Long, Length >=101
      Note: The Header Parsing sets any V7 to GNU if 'L'/'K" Headers are present
      V7: Raise an exception, as this can NOT be done, no change to header.
      USTAR: if new length <= 254 zero fill header, update name fields,  1 updated Header
             if new Length >= 255 raise an exception, as this can NOT be done, no change to header
      if old was Short,  Add files to match format,
         OLD_GNU & GNU: Create new Name header, Add N Headers for name, Update name in file header, update name fields, min 3 headers
         STAR & PAX: And should not yet get here.
      if old was Long,
         OLD_GNU & GNU: Add N Headers for name, Update name in MD header, update name field in File Headers, min 3 headers

      Add headers to length of new Name Length, update name in file header, update name fields }
  RawFileName := CeUtf8ToSys(Value);
  { In all cases zero out the name fields in the File Header. }
  if Length(RawFileName) > AB_TAR_NAMESIZE then begin { Must be null terminated except at 100 char length }
    { Look for long name meta-data headers already in the archive. }
    FoundMetaDataHeader := False;
    I := 0;
    { FTarHeaderList.Count <= 1 always }
    while (not FoundMetaDataHeader) and (I <= (FTarHeaderList.Count - 1)) do begin
      PHeader := FTarHeaderList.Items[I];
      if PHeader.LinkFlag = AB_TAR_LF_LONGNAME then begin
        { We are growing or Shriking the Name MD Data fields.  }
        FoundMetaDataHeader := True;
        DoGNUExistingLongNameLink(AB_TAR_LF_LONGNAME, I, RawFileName);
        { Need to copy the Name to the header. }
        FTarItem.Name := Value;
      end
      else
        I := I + 1;
    end; { End While }
    { MD Headers & MD Data Headers have been stuffed if FoundMetaDataHeader }
    { Still need to stuff the File type header contents. }
    if not FoundMetaDataHeader then
    begin
      case FTarItem.ArchiveFormat of
        V7_FORMAT: raise EAbTarBadFileName.Create; { File Name to Long }
        USTAR_FORMAT:
          begin
            { Longest file name is AB_TAR_NAMESIZE(100) chars }
            { Longest Prefix is AB_TAR_USTAR_PREFIX_SIZE(155) chars }
            { These two fields are delimted by a '/' char }
            {0123456789012345, Length = 15, NameLength = 5, PrefixLength = 9}
            { AAAA/BBBB/C.txt, Stored as Name := 'C.txt', Prefix := 'AAAA/BBBB' }
            { That means Theoretical maximum is 256 for Length(RawFileName) }
            if Length(RawFileName) > (AB_TAR_NAMESIZE+AB_TAR_USTAR_PREFIX_SIZE+1) then { Check the obvious one. }
              raise EAbTarBadFileName.Create; { File Name to Long }
            for I := Length(RawFileName) downto Length(RawFileName)-AB_TAR_NAMESIZE-1 do begin
              if RawFileName[I] = '/' then begin
                if (I <= AB_TAR_USTAR_PREFIX_SIZE+1) and (Length(RawFileName)-I <= AB_TAR_NAMESIZE) then begin
                  { We have a successfull parse. }
                  FillChar(PTarHeader.Name, SizeOf(PTarHeader.Name), #0);
                  FillChar(PTarHeader.ustar.Prefix, SizeOf(PTarHeader.ustar.Prefix), #0);
                  Move(RawFileName[I+1], PTarHeader.Name, Length(RawFileName)-I);
                  Move(RawFileName[1], PTarHeader.ustar.Prefix, I);
                  break;
                end
                else if (Length(RawFileName)-I > AB_TAR_NAMESIZE) then
                  raise EAbTarBadFileName.Create { File Name not splittable }
                { else continue; }
              end;
            end;{ End for I... }
          end; { End USTAR Format }
        OLDGNU_FORMAT: DoGNUNewLongNameLink(AB_TAR_LF_LONGNAME, 0, RawFileName); {GNU_FORMAT}
        else begin
          { UNKNOWN_FORMAT, STAR_FORMAT, POSIX_FORMAT }
          raise EAbTarBadOp.Create; { Unknown Archive Format }
        end;{ End of Else for case statement }
      end;{ End of case statement }
      FTarItem.Name := Value;
    end; { if no Meta data header found }
  end { End "name length larger than 100" }
  else
  begin { Short new name, Simple Case Just put it in the Name Field & remove any headers }
    { PTarHeader Points to the File type Header }
    { Zero the Name field }
    FillChar(PTarHeader.Name, SizeOf(PTarHeader.Name), #0);
    if FTarItem.ArchiveFormat in [USTAR_FORMAT] then { Zero the prefix field }
      FillChar(PTarHeader.ustar.Prefix, SizeOf(PTarHeader.ustar.Prefix), #0);
    if FTarItem.ArchiveFormat in [GNU_FORMAT, OLDGNU_FORMAT] then
    begin { We may have AB_TAR_LF_LONGNAME Headers to be removed }
      { Remove long file names Headers if they exist}
      FoundMetaDataHeader := False;
      I := 0;
      while not FoundMetaDataHeader and (I <= (FTarHeaderList.Count - 1)) do
      begin
        PHeader := FTarHeaderList.Items[I];
        if PHeader.LinkFlag in [AB_TAR_LF_LONGNAME] then
        begin  { Delete this Header, and the data Headers. }
          FoundMetaDataHeader := True;
          TotalOldNumHeaders := Ceil( OctalToInt(PHeader.Size, SizeOf(PHeader.Size)) / AB_TAR_RECORDSIZE);
          for J := TotalOldNumHeaders downto 0 do
          begin { Note 0 will delete the Long Link MD Header }
            FreeMem(FTarHeaderList.Items[I+J]); { This list holds PAbTarHeaderRec's }
            FTarHeaderList.Delete(I+J);
            FTarHeaderTypeList.Delete(I+J);
          end;
        end
        else
          I := I + 1; { Got to next header }
      end;{ End While not found... }
    end; { End if GNU... }
    { Save off the new name and store to the Header }
    FTarItem.Name := Value;
    { Must add Null Termination before we store to Header }
    StrPLCopy(PTarHeader.Name, RawFileName, AB_TAR_NAMESIZE);
  end;{ End else Short new name,... }

  { Update the inherited file names. }
  FFileName := FTarItem.Name;
//DiskFileName := FFileName;
//AbUnfixName(FDiskFileName); // Don't override DiskFileName
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetGroupID(const Value: Integer);
var
  S : AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { gid is extendable in PAX Headers, Rember PAX extended Header Over Rule File Headers }
  FTarItem.gid := Value;
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], PTarHeader.gid, Length(S));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetGroupName(const Value: string);
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { GrpName is extendable in PAX Headers, Rember PAX extended Header Over Rule File Headers }
  FTarItem.GrpName := Value;
  StrPLCopy(PTarHeader.GrpName, AnsiString(Value), SizeOf(PTarHeader.GrpName));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetIsEncrypted(Value: Boolean);
begin
  { do nothing, TAR has no native encryption }
end;

procedure TAbTarItem.SetLastModFileDate(const Value: Word);
begin
  { replace date, keep existing time }
  LastModTimeAsDateTime :=
    EncodeDate(
      Value shr 9 + 1980,
      Value shr 5 and 15,
      Value and 31) +
    Frac(LastModTimeAsDateTime);
end;

procedure TAbTarItem.SetLastModFileTime(const Value: Word);
begin
  { keep current date, replace time }
  LastModTimeAsDateTime :=
    Trunc(LastModTimeAsDateTime) +
    EncodeTime(
      Value shr 11,
      Value shr 5 and 63,
      Value and 31 shl 1, 0);
end;

procedure TAbTarItem.SetLastModTimeAsDateTime(const Value: TDateTime);
begin
  // TAR stores always Unix time.
  SetModTime(AbLocalDateTimeToUnixTime(Value));    // also updates headers
end;

procedure TAbTarItem.SetLinkFlag(Value: AnsiChar);
begin
  if FTarItem.ItemReadOnly then
    Exit;
  FTarItem.LinkFlag := Value;
  PTarHeader.LinkFlag := Value;
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetLinkName(const Value: string);
var
  FoundMetaDataHeader: Boolean;
  PHeader: PAbTarHeaderRec;
  I, J: Integer;
  TotalOldNumHeaders: Integer;
  RawLinkName: AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
    { Cases:
    New Link Name is short, Length <= 100,
      All formats: Zero Name field and move new name to field.
      V7: Work complete, 1 header
      USTAR: Work complete, 1 Header
      OLD_GNU & GNU: Remove old link headers, 1 header.
      STAR & PAX: And should not yet get here.
    New File Name is Long, Length >=101
      Note: The Header Parsing sets any V7 to GNU if 'L'/'K' Headers are present
      V7: Raise an exception, as this can NOT be done, no change to header.
      USTAR: Raise an exception, as this can NOT be done, no change to header.
      if old was Short,  Add files to match format,
         OLD_GNU & GNU: Create new Link header, Add N Headers for name, Update name in file header, update name fields, min 3 headers
      if old was Long,
         OLD_GNU & GNU: Add N Headers for name, Update name in MD header, update name field in File Headers, min 3 headers
      STAR & PAX: And should not yet get here.}
  RawLinkName := CeUtf8ToSys(Value);
  if Length(RawLinkName) > AB_TAR_NAMESIZE then { Must be null terminated except at 100 char length }
  begin
    { Look for long name meta-data headers already in the archive. }
    FoundMetaDataHeader := False;
    I := 0;
    { FTarHeaderList.Count <= 1 always }
    while (not FoundMetaDataHeader) and (I <= (FTarHeaderList.Count - 1)) do begin
      PHeader := FTarHeaderList.Items[I];
      if PHeader.LinkFlag = AB_TAR_LF_LONGLINK then
      begin { We are growing or Shriking the Name MD Data fields.  }
        FoundMetaDataHeader := True;
        DoGNUExistingLongNameLink(AB_TAR_LF_LONGLINK, I, RawLinkName);
        { Need to copy the Name to the header. }
        FTarItem.LinkName := Value;
      end
      else
        I := I + 1;
    end; { End While }
    { MD Headers & MD Data Headers have been stuffed if FoundMetaDataHeader }
    { Still need to stuff the File type header contents. }
    if not FoundMetaDataHeader then
    begin
      case FTarItem.ArchiveFormat of
        V7_FORMAT: raise EAbTarBadLinkName.Create; { Link Name to Long }
        USTAR_FORMAT: raise EAbTarBadLinkName.Create; { Link Name to Long }
        OLDGNU_FORMAT: DoGNUNewLongNameLink(AB_TAR_LF_LONGLINK, 0, RawLinkName); {GNU_FORMAT}
        else
          begin
          { UNKNOWN_FORMAT, STAR_FORMAT, POSIX_FORMAT }
          raise EAbTarBadOp.Create; { Unknown Archive Format }
        end;{ End of Else for case statement }
      end;{ End of case statement }
      FTarItem.LinkName := Value;
    end; { if no Meta data header found }
  end { End "name length larger than 100" }
  else
  begin { Short new name, Simple Case Just put it in the Link Field & remove any headers }
    { PTarHeader Points to the File type Header }
    { Zero the Link field }
    FillChar(PTarHeader.LinkName, SizeOf(PTarHeader.LinkName), #0);
    if FTarItem.ArchiveFormat in [GNU_FORMAT, OLDGNU_FORMAT] then
    begin { We may have AB_TAR_LF_LONGNAME Headers to be removed }
      { Remove long file names Headers if they exist}
      FoundMetaDataHeader := False;
      I := 0;
      while not FoundMetaDataHeader and (I <= (FTarHeaderList.Count - 1)) do
      begin
        PHeader := FTarHeaderList.Items[I];
        if PHeader.LinkFlag in [AB_TAR_LF_LONGLINK] then
        begin  { Delete this Header, and the data Headers. }
          FoundMetaDataHeader := True;
          TotalOldNumHeaders := Ceil( OctalToInt(PHeader.Size, SizeOf(PHeader.Size)) / AB_TAR_RECORDSIZE);
          for J := TotalOldNumHeaders downto 0 do
          begin { Note 0 will delete the Long Link MD Header }
            FreeMem(FTarHeaderList.Items[I+J]); { This list holds PAbTarHeaderRec's }
            FTarHeaderList.Delete(I+J);
            FTarHeaderTypeList.Delete(I+J);
          end;
        end
        else
          I := I + 1; { Got to next header }
      end;{ End While not found... }
    end; { End if GNU... }
    { Save off the new name and store to the Header }
    FTarItem.LinkName := Value;
    StrPLCopy(PTarHeader.LinkName, RawLinkName, AB_TAR_NAMESIZE);
  end;{ End else Short new name,... }
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetMagic(const Value: String);
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  FTarItem.Magic := AnsiString(Value);
  Move(Value[1], PTarHeader.Magic, SizeOf(TAbTarMagicRec));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetUncompressedSize(const Value: Int64);
var
  S : AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { Size is extendable in PAX Headers, Remember PAX extended Header Over Rule File Headers }
  FTarItem.Size := Value; { Store our Vitrual Copy }
  S := PadString(IntToOctal(Value), SizeOf(Arr12));{ Stuff to header }
  Move(S[1], PTarHeader.Size, Length(S));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetUserID(const Value: Integer);
var
  S : AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { uid is extendable in PAX Headers, Remember PAX extended Header Over Rule File Headers }
  FTarItem.uid := Value;
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], PTarHeader.uid, Length(S));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetUserName(const Value: string);
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { UsrName is extendable in PAX Headers, Remember PAX extended Header Over Rule File Headers }
  FTarItem.UsrName := Value;
  StrPLCopy(PTarHeader.UsrName, AnsiString(Value), SizeOf(PTarHeader.UsrName));
  FTarItem.Dirty := True;
end;

procedure TAbTarItem.SetModTime(const Value: Int64);
var
  S: AnsiString;
begin
  if FTarItem.ItemReadOnly then { Read Only - Do Not Save }
    Exit;
  { ModTime is extendable in PAX Headers, Remember PAX extended Header Over Rule File Headers }
  FTarItem.ModTime := Value; { Store our Virtual Copy }
  S := PadString(IntToOctal(Value), SizeOf(Arr12));{ Stuff to header }
  Move(S[1], PTarHeader.ModTime, Length(S));
  FTarItem.Dirty := True;
end;


{ ************************** TAbTarStreamHelper ****************************** }
destructor TAbTarStreamHelper.Destroy;
begin
  inherited Destroy;
end;

{ This is slow, use the archive class instead }
procedure TAbTarStreamHelper.ExtractItemData(AStream: TStream);
begin
  { Note: The SizeOf(TAbTarHeaderRec) = AB_TAR_RECORDSIZE }
  if FCurrItemSize <> 0 then
  begin
    { copy stored data to output }
    AStream.CopyFrom(FStream, FCurrItemSize);
    {reset the stream to the start of the item}
    FStream.Seek(-(FCurrItemPreHdrs*AB_TAR_RECORDSIZE+FCurrItemSize), soFromCurrent);
  end;
  { else do nothing }
end;

{ This function Should only be used from LoadArchive, as it is slow. }
function TAbTarStreamHelper.FindItem: Boolean;
var
  DataRead : LongInt;
  FoundItem: Boolean;
  SkipHdrs : Integer;
begin
  { Note: The SizeOf(TAbTarHeaderRec) = AB_TAR_RECORDSIZE }
  { Note: Standard LBA size of hard disks is 512 bytes = AB_TAR_RECORDSIZE }
  FoundItem := False;
  { Getting an new Item reset these numbers }
  FCurrItemSize := 0;
  FCurrItemPreHdrs := 0;
  DataRead := FStream.Read(FTarHeader, AB_TAR_RECORDSIZE); { Read in a header }
  { DataRead <> AB_TAR_RECORDSIZE means end of stream, and the End Of Archive
    record is all #0's, which the StrLen(FTarHeader.Name) check will catch }
  while (DataRead = AB_TAR_RECORDSIZE) and (StrLen(FTarHeader.Name) > 0) and not FoundItem do
  begin { Either exit when we find a supported file or end of file or an invalid header name. }
    if FTarHeader.LinkFlag in (AB_SUPPORTED_MD_HEADERS+AB_UNSUPPORTED_MD_HEADERS) then
    begin { We have a un/supported Meta-Data Header }
      { FoundItem := False } { Value remains False. }
      SkipHdrs := Ceil(OctalToInt(FTarHeader.Size, SizeOf(FTarHeader.Size))/AB_TAR_RECORDSIZE);
      FStream.Seek(SkipHdrs*AB_TAR_RECORDSIZE, soFromCurrent);
      { Tally new Headers: Consumed + Current }
      FCurrItemPreHdrs := FCurrItemPreHdrs + SkipHdrs + 1;
      { Read our next header, Loop, and re-parse }
      DataRead := FStream.Read(FTarHeader, AB_TAR_RECORDSIZE);
    end
    else if FTarHeader.LinkFlag in (AB_SUPPORTED_F_HEADERS+AB_UNSUPPORTED_F_HEADERS) then
    begin { We have a un/supported File Header. }
      FoundItem := True;
      if not (FTarHeader.LinkFlag in AB_IGNORE_SIZE_HEADERS) then
        FCurrItemSize := OctalToInt(FTarHeader.Size, SizeOf(FTarHeader.Size))
      else FCurrItemSize := 0; { Per The spec these Headers do not have file content }
      FCurrItemPreHdrs := FCurrItemPreHdrs + 1; { Tally current header }
    end
    else
    begin{ We Have an Unknown header }
      FoundItem := True;
      FCurrItemSize := 0;
      { We could have many un/supported headers before this unknown type }
      FCurrItemPreHdrs := FCurrItemPreHdrs + 1; { Tally current header }
      { These Headers should throw exceptions when TAbTarItem.LoadTarHeaderFromStream is called }
    end; { End of Link Flag parsing }
  end;
  { Rewind to the "The Beginning" of this Item }
  { Really that means to the first supported Header Type before a supported Item Type }
  if FoundItem then
    FStream.Seek(-(FCurrItemPreHdrs*AB_TAR_RECORDSIZE), soFromCurrent);
  Result := FoundItem;
end;

{ Should only be used from LoadArchive, as it is slow. }
function TAbTarStreamHelper.FindFirstItem: Boolean;
begin
  FStream.Seek(0, soFromBeginning);
  Result := FindItem;
end;

{ Should only be used from LoadArchive, as it is slow. }
function TAbTarStreamHelper.FindNextItem: Boolean;
begin
  { Fast Forward Past the current Item }
  FStream.Seek((FCurrItemPreHdrs*AB_TAR_RECORDSIZE + RoundToTarBlock(FCurrItemSize)), soFromCurrent);
  Result := FindItem;
end;

{ This is slow, use the archive class instead }
function TAbTarStreamHelper.GetItemCount : Integer;
var
  Found : Boolean;
begin
  Result := 0;
  Found := FindFirstItem;
  while Found do begin
    Inc(Result);
    Found := FindNextItem;
  end;
end;

procedure TAbTarStreamHelper.ReadHeader;
begin
  { do nothing }
  { Tar archives have no overall header data }
end;

procedure TAbTarStreamHelper.ReadTail;
begin
  { do nothing }
  { Tar archives have no overall tail data }
end;

{ This is slow, use the archive class instead }
function TAbTarStreamHelper.SeekItem(Index: Integer): Boolean;
var
  i : Integer;
begin
  Result := FindFirstItem; { see if can get to first item }
  i := 1;
  while Result and (i < Index) do begin
    Result := FindNextItem;
    Inc(i);
  end;
end;

procedure TAbTarStreamHelper.WriteArchiveHeader;
begin
  { do nothing }
  { Tar archives have no overall header data }
end;

procedure TAbTarStreamHelper.WriteArchiveItem(AStream: TStream);
begin
  WriteArchiveItemSize(AStream, AStream.Size);
end;

procedure TAbTarStreamHelper.WriteArchiveItemSize(AStream: TStream; Size: Int64);
var
  PadBuff : PAnsiChar;
  PadSize : Integer;
begin
  if Size = 0 then
    Exit;
  { transfer actual item data }
  FStream.CopyFrom(AStream, Size);

  { Pad to Next block }
  PadSize := RoundToTarBlock(Size) - Size;
  GetMem(PadBuff, PadSize);
  FillChar(PadBuff^, PadSize, #0);
  FStream.Write(PadBuff^, PadSize);
  FreeMem(PadBuff, PadSize);
end;


procedure TAbTarStreamHelper.WriteArchiveTail;
var
  PadBuff : PAnsiChar;
  PadSize : Integer;
begin
  { append 2 terminating null blocks }
  PadSize := AB_TAR_RECORDSIZE;
  GetMem(PadBuff, PadSize);
  try
    FillChar(PadBuff^, PadSize, #0);
    FStream.Write(PadBuff^, PadSize);
    FStream.Write(PadBuff^, PadSize);
  finally
    FreeMem(PadBuff, PadSize);
  end;
end;



{ ***************************** TAbTarArchive ******************************** }
constructor TAbTarArchive.CreateFromStream(aStream : TStream; const aArchiveName : string);
begin
  inherited;
  FArchFormat := V7_FORMAT;  // Default for new archives
end;

function TAbTarArchive.CreateItem(const SourceFileName   : string;
                                  const ArchiveDirectory : string): TAbArchiveItem;
var
  Item : TAbTarItem;
  I: Integer;
  FullSourceFileName, FullArchiveFileName: String;
begin
  if FArchReadOnly then
    raise EAbTarBadOp.Create; { Create Item Unsupported in this Archive }

  MakeFullNames(SourceFileName, ArchiveDirectory,
                FullSourceFileName, FullArchiveFileName);

  Item := TAbTarItem.Create;
  try
  //  HeaderFormat = (UNKNOWN_FORMAT, V7_FORMAT, OLDGNU_FORMAT, GNU_FORMAT, USTAR_FORMAT, STAR_FORMAT, POSIX_FORMAT);
    if FArchFormat in [OLDGNU_FORMAT, GNU_FORMAT] then
    begin
      Item.ArchiveFormat := FArchFormat;
      Item.LinkFlag := AB_TAR_LF_NORMAL;
      Item.Magic := AB_TAR_MAGIC_GNUOLD;
    end
    else if FArchFormat in [USTAR_FORMAT] then
    begin
      Item.ArchiveFormat := USTAR_FORMAT;
      Item.LinkFlag := AB_TAR_LF_NORMAL;
      Item.Magic := AB_TAR_MAGIC_VAL+AB_TAR_MAGIC_VER;
    end
    else if (FArchFormat = V7_FORMAT) and (Length(FullArchiveFileName) > 100) then
      begin { Switch the rep over to GNU so it can have long file names. }
      FArchFormat := OLDGNU_FORMAT;
      Item.ArchiveFormat := OLDGNU_FORMAT;
      { Leave the Defaults for LinkFlag, and Magic }
      { Update all the rest so that it can transistion to GNU_FORMAT }
      for I := 0 to FItemList.Count - 1 do
        TAbTarItem(FItemList.Items[i]).ArchiveFormat := OLDGNU_FORMAT;
    end;{ This should not execute... }{
    else if FArchFormat in [STAR_FORMAT, POSIX_FORMAT] then
    begin
      Item.ArchiveFormat := FArchFormat;
      Item.LinkFlag := AB_TAR_LF_NORMAL;
      Item.Magic := AB_TAR_MAGIC_VAL+AB_TAR_MAGIC_VER;
    end;
    }{ else  FArchFormat in [ UNKNOWN_FORMAT, V7_FORMAT and Length(S) <= 100 ] } { This is the default. }

    { Most others are initialized in the .Create }
    Item.CRC32 := 0;

    { Note this can raise exceptions for file name lengths. }
    Item.FileName := FullArchiveFileName;
    Item.DiskFileName := FullSourceFileName;
    Item.Action := aaNone;
  finally
    Result := Item;
  end;
end;


procedure TAbTarArchive.ExtractItemAt(Index: Integer; const UseName: string);
var
  OutStream : TStream;
  CurItem : TAbTarItem;
begin
  { Check the index is not out of range. }
  if(Index >= ItemList.Count) then
	  raise EListError.CreateFmt(SListIndexError, [Index]);

  CurItem := TAbTarItem(ItemList[Index]);

  if CurItem.ItemType in [UNKNOWN_ITEM] then
    raise EAbTarBadOp.Create; { Unsupported Type, Cannot Extract }
  if (CurItem.ItemType = UNSUPPORTED_ITEM) and
     ((Length(CurItem.FileName) >= AB_TAR_NAMESIZE) or
      (Length(CurItem.LinkName) >= AB_TAR_NAMESIZE)) then
    raise EAbTarBadOp.Create; { Unsupported Type, Cannot Extract }
  { We will allow extractions if the file name/Link name are strickly less than 100 chars }

  if CurItem.IsDirectory then
    AbCreateDirectory(UseName)
  else begin
    case (CurItem.Mode and $F000) of
      AB_FMODE_FILE, AB_FMODE_FILE2: begin
        OutStream := TFileStreamEx.Create(UseName, fmCreate or fmShareDenyNone);
        try
          try {OutStream}
            ExtractItemToStreamAt(Index, OutStream);
          finally {OutStream}
            OutStream.Free;
          end; {OutStream}
        except
          if ExceptObject is EAbUserAbort then
            FStatus := asInvalid;
          mbDeleteFile(UseName);
          raise;
        end;
      end;

      AB_FMODE_FILELINK: begin
        AbCreateSymlink(CurItem.LinkName, UseName);
      end;
    end;
  end;
  AbSetFileTime(UseName, CurItem.LastModTimeAsDateTime);
  AbSetFileAttr(UseName, CurItem.NativeFileAttributes);
end;

procedure TAbTarArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
var
  CurItem : TAbTarItem;
begin
  if(Index >= ItemList.Count) then
	  raise EListError.CreateFmt(SListIndexError, [Index]);

  CurItem := TAbTarItem(ItemList[Index]);

  if CurItem.ItemType in [UNKNOWN_ITEM] then
    raise EAbTarBadOp.Create; { Unsupported Type, Cannot Extract }
  if (CurItem.ItemType = UNSUPPORTED_ITEM) and
     ((Length(CurItem.FileName) >= AB_TAR_NAMESIZE) or
      (Length(CurItem.LinkName) >= AB_TAR_NAMESIZE)) then
    raise EAbTarBadOp.Create; { Unsupported Type, Cannot Extract }
  { We will allow extractions if the file name is strictly less than 100 chars }

  FStream.Position := CurItem.StreamPosition+CurItem.FileHeaderCount*AB_TAR_RECORDSIZE;
  if CurItem.UncompressedSize <> 0 then
    aStream.CopyFrom(FStream, CurItem.UncompressedSize);
  { Else there is nothing to copy. }
end;

procedure TAbTarArchive.LoadArchive;
var
  TarHelp      : TAbTarStreamHelper;
  Item         : TAbTarItem;
  ItemFound    : Boolean;
  Abort        : Boolean;
  Confirm      : Boolean;
  i            : Integer;
  Progress     : Byte;

begin
  { create helper }
  TarHelp := TAbTarStreamHelper.Create(FStream);
  try {TarHelp}
    {build Items list from tar header records}

    { reset Tar }
    ItemFound := (FStream.Size > 0) and TarHelp.FindFirstItem;
    if ItemFound then FArchFormat := UNKNOWN_FORMAT
    else FArchFormat := V7_FORMAT;

    { while more data in Tar }
    while (FStream.Position < FStream.Size) and ItemFound do begin
      {create new Item}
      Item := TAbTarItem.Create;
      Item.FTarItem.StreamPosition := FStream.Position;
      try  {Item}
        Item.LoadTarHeaderFromStream(FStream);
        if Item.ItemReadOnly then
          FArchReadOnly := True; { Set Archive as Read Only }
        if Item.ItemType in [SUPPORTED_ITEM, UNSUPPORTED_ITEM] then begin
        { List of supported Item/File Types. }
          { Add the New Supported Item to the List }
          if FArchFormat < Item.ArchiveFormat then
            FArchFormat := Item.ArchiveFormat; { Take the max format }
          Item.Action := aaNone;
          FItemList.Add(Item);
        end { end if }
        else begin
        { unhandled Tar file system entity, notify user, but otherwise ignore }
          if Assigned(FOnConfirmProcessItem) then
            FOnConfirmProcessItem(self, Item, ptFoundUnhandled, Confirm);
        end;

        { show progress and allow for aborting }
        Progress := (FStream.Position*100) div FStream.Size;
        DoArchiveProgress(Progress, Abort);
        if Abort then begin
          FStatus := asInvalid;
          raise EAbUserAbort.Create;
        end;

        { get the next item }
        ItemFound := TarHelp.FindNextItem;
      except {Item}
        raise EAbTarBadOp.Create; { Invalid Item }
      end; {Item}
    end; {end while }

    { All the items need to reflect this information. }
    for i := 0 to FItemList.Count - 1 do
    begin
      TAbTarItem(FItemList.Items[i]).ArchiveFormat := FArchFormat;
      TAbTarItem(FItemList.Items[i]).ItemReadOnly := FArchReadOnly;
    end;

    DoArchiveProgress(100, Abort);
    FIsDirty := False;

  finally {TarHelp}
    { Clean Up }
    TarHelp.Free;
  end; {TarHelp}
end;


function TAbTarArchive.FixName(const Value: string): string;
{ fixup filename for storage }
var
  lValue : string;
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
        lValue := BaseDirectory + AbPathDelim + lValue;
    end;
    lValue := AbGetShortFileSpec( lValue );
  end;
  {$ENDIF MSWINDOWS}

  { Should always trip drive info if on a Win/Dos system }
  StoreOptions := StoreOptions + [soStripDrive];

  { strip drive stuff }
  if soStripDrive in StoreOptions then
    AbStripDrive( lValue );

  { check for a leading slash }
  if (Length(lValue) > 0) and (lValue[1] = AbPathDelim) then
    System.Delete( lValue, 1, 1 );

  if soStripPath in StoreOptions then
    lValue := ExtractFileName(lValue);

  if soRemoveDots in StoreOptions then
    AbStripDots(lValue);

  AbFixName(lValue);

  Result := lValue;
end;

function TAbTarArchive.GetItem(Index: Integer): TAbTarItem;
begin
  Result := TAbTarItem(FItemList.Items[Index]);
end;

function TAbTarArchive.GetSupportsEmptyFolders: Boolean;
begin
  Result := True;
end;

procedure TAbTarArchive.PutItem(Index: Integer; const Value: TAbTarItem);
begin
  //TODO: Remove this from all archives
  FItemList.Items[Index] := Value;
end;

procedure TAbTarArchive.SaveArchive;
var
  OutTarHelp     : TAbTarStreamHelper;
  Abort          : Boolean;
  i              : Integer;
  NewStream      : TAbVirtualMemoryStream;
  TempStream     : TStream;
  CurItem        : TAbTarItem;
  AttrEx         : TAbAttrExRec;
begin
  if FArchReadOnly then
    raise EAbTarBadOp.Create; { Archive is read only }

  {init new archive stream}
  NewStream := TAbVirtualMemoryStream.Create;
  OutTarHelp := TAbTarStreamHelper.Create(NewStream);

  try {NewStream/OutTarHelp}
    { create helper }
    NewStream.SwapFileDirectory := AbGetTempDirectory;

    {build new archive from existing archive}
    for i := 0 to pred(Count) do begin
      FCurrentItem := ItemList[i];
      CurItem      := TAbTarItem(ItemList[i]);

      case CurItem.Action of
        aaNone, aaMove : begin  {just copy the file to new stream}
          { "Seek" to the Item Data } { SaveTarHeaders, Updates FileHeaderCount }
          FStream.Position := CurItem.StreamPosition+CurItem.FileHeaderCount*AB_TAR_RECORDSIZE;
          CurItem.StreamPosition := NewStream.Position;{ Reset the Stream Pointer. }
          { Flush The Headers to the new stream }
          CurItem.SaveTarHeaderToStream(NewStream);
          { Copy to new Stream, Round to the AB_TAR_RECORDSIZE boundry, and Pad zeros}
          outTarhelp.WriteArchiveItemSize(FStream, CurItem.UncompressedSize);
        end;

        aaDelete: {doing nothing omits file from new stream} ;

        aaStreamAdd : begin
          try
            { adding from a stream }
            CurItem.StreamPosition := NewStream.Position;{ Reset the Stream Pointer. }
            CurItem.UncompressedSize := InStream.Size;
            CurItem.SaveTarHeaderToStream(NewStream);
            OutTarHelp.WriteArchiveItemSize(InStream, InStream.Size);
          except
            ItemList[i].Action := aaDelete;
            DoProcessItemFailure(ItemList[i], ptAdd, ecFileOpenError, 0);
          end;
        end;

        aaAdd, aaFreshen, aaReplace: begin
          try
            { update metadata }
            if not AbFileGetAttrEx(CurItem.DiskFileName, AttrEx, False) then
              Raise EAbFileNotFound.Create;
            CurItem.ExternalFileAttributes := AttrEx.Mode;
            CurItem.LastModTimeAsDateTime := AttrEx.Time;
            { TODO: uid, gid, uname, gname should be added here }
            { TODO: Add support for different types of files here }
            case (AttrEx.Mode and $F000) of
              AB_FMODE_DIR: begin
                CurItem.UncompressedSize := 0;
                CurItem.SaveTarHeaderToStream(NewStream);
              end;

              AB_FMODE_FILELINK: begin
                CurItem.UncompressedSize := 0;
                CurItem.LinkName := AbReadSymlink(CurItem.DiskFileName);
                CurItem.SaveTarHeaderToStream(NewStream);
              end;

              AB_FMODE_FILE, AB_FMODE_FILE2: begin
                TempStream := TFileStreamEx.Create(CurItem.DiskFileName,
                  fmOpenRead or fmShareDenyWrite );
                try { TempStream }
                  CurItem.UncompressedSize := TempStream.Size;
                  CurItem.StreamPosition := NewStream.Position;{ Reset the Stream Pointer. }
                  CurItem.SaveTarHeaderToStream(NewStream);
                  OutTarHelp.WriteArchiveItemSize(TempStream, TempStream.Size);
                finally { TempStream }
                  TempStream.Free;
                end; { TempStream }
              end;

              else begin
                CurItem.UncompressedSize := AttrEx.Size;
                CurItem.SaveTarHeaderToStream(NewStream);
              end;
            end;

          except
            ItemList[i].Action := aaDelete;
            DoProcessItemFailure(ItemList[i], ptAdd, ecFileOpenError, 0);
          end;
        end; { aaAdd ... }
      end; { case }
    end; { for i ... }

    if NewStream.Size <> 0  then
      OutTarHelp.WriteArchiveTail; { Terminate the TAR }
    { Size of NewStream is still 0, and max of the stream will also be 0 }

    {copy new stream to FStream}
    NewStream.Position := 0;
    if (FStream is TMemoryStream) then
      TMemoryStream(FStream).LoadFromStream(NewStream)
    else if (FStream is TAbVirtualMemoryStream) then begin
      FStream.Position := 0;
      FStream.Size := 0;
      TAbVirtualMemoryStream(FStream).CopyFrom(NewStream, NewStream.Size)
    end
    else begin
      { need new stream to write }
      FreeAndNil(FStream);
      FStream := TFileStreamEx.Create(FArchiveName, fmCreate or fmShareDenyWrite);
      FStream.CopyFrom(NewStream, NewStream.Size);
    end;

    {update Items list}
    for i := pred( Count ) downto 0 do begin
      if ItemList[i].Action = aaDelete then
        FItemList.Delete( i )
      else if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    end;

    DoArchiveSaveProgress( 100, Abort );
    DoArchiveProgress( 100, Abort );
  finally {NewStream/OutTarHelp}
    OutTarHelp.Free;
    NewStream.Free;
  end;
end;

{ This assumes that LoadArchive has been called. }
procedure TAbTarArchive.TestItemAt(Index: Integer);
begin
  FStream.Position := TAbTarItem(FItemList[Index]).StreamPosition;
  if VerifyTar(FStream) <> atTar then
    raise EAbTarInvalid.Create; { Invalid Tar }
end;

end.

{ Contents of file wcxhead.pas }
{ It contains definitions of error codes, flags and callbacks }
{ Ver. 2.20 with Unicode support }

unit WcxPlugin;

interface

const       {Error codes returned to calling application}
  E_SUCCESS=          0;       {Success}
  E_END_ARCHIVE=     10;       {No more files in archive}
  E_NO_MEMORY=       11;       {Not enough memory}
  E_BAD_DATA=        12;       {Data is bad}
  E_BAD_ARCHIVE=     13;       {CRC error in archive data}
  E_UNKNOWN_FORMAT=  14;       {Archive format unknown}
  E_EOPEN=           15;       {Cannot open existing file}
  E_ECREATE=         16;       {Cannot create file}
  E_ECLOSE=          17;       {Error closing file}
  E_EREAD=           18;       {Error reading from file}
  E_EWRITE=          19;       {Error writing to file}
  E_SMALL_BUF=       20;       {Buffer too small}
  E_EABORTED=        21;       {Function aborted by user}
  E_NO_FILES=        22;       {No files found}
  E_TOO_MANY_FILES=  23;       {Too many files to pack}
  E_NOT_SUPPORTED=   24;       {Function not supported}

  E_HANDLED=         -32769;   {Handled error}
  E_UNKNOWN=         +32768;   {Unknown error}

  {Unpacking flags}
  PK_OM_LIST=           0;
  PK_OM_EXTRACT=        1;

  {Flags for ProcessFile}
  PK_SKIP=              0;     {Skip file (no unpacking)}
  PK_TEST=              1;     {Test file integrity}
  PK_EXTRACT=           2;     {Extract file to disk}

  {Flags passed through ChangeVolProc}
  PK_VOL_ASK=           0;     {Ask user for location of next volume}
  PK_VOL_NOTIFY=        1;     {Notify app that next volume will be unpacked}

  {Packing flags}

  {For PackFiles}
  PK_PACK_MOVE_FILES=   1;    {Delete original after packing}
  PK_PACK_SAVE_PATHS=   2;    {Save path names of files}
  PK_PACK_ENCRYPT=      4;    {Ask user for password, then encrypt}


  {Returned by GetPackCaps}
  PK_CAPS_NEW=          1;    {Can create new archives}
  PK_CAPS_MODIFY=       2;    {Can modify exisiting archives}
  PK_CAPS_MULTIPLE=     4;    {Archive can contain multiple files}
  PK_CAPS_DELETE=       8;    {Can delete files}
  PK_CAPS_OPTIONS=     16;    {Supports the options dialogbox}
  PK_CAPS_MEMPACK=     32;    {Supports packing in memory}
  PK_CAPS_BY_CONTENT=  64;    {Detect archive type by content}
  PK_CAPS_SEARCHTEXT= 128;    {Allow searching for text in archives}
                              { created with this plugin}
  PK_CAPS_HIDE=       256;    { Show as normal files (hide packer icon) }
                              { open with Ctrl+PgDn, not Enter }
  PK_CAPS_ENCRYPT=    512;    { Plugin supports PK_PACK_ENCRYPT option }

  BACKGROUND_UNPACK=1;        { Which operations are thread-safe? }
  BACKGROUND_PACK=2;
  BACKGROUND_MEMPACK=4;       { For tar.pluginext in background }

  {Flags for packing in memory}
  MEM_OPTIONS_WANTHEADERS=1;  {Return archive headers with packed data}

  {Errors returned by PackToMem}
  MEMPACK_OK=           0;    {Function call finished OK, but there is more data}
  MEMPACK_DONE=         1;    {Function call finished OK, there is no more data}

  {Flags for PkCryptProc callback}
  PK_CRYPT_SAVE_PASSWORD=1;
  PK_CRYPT_LOAD_PASSWORD=2;
  PK_CRYPT_LOAD_PASSWORD_NO_UI=3;   { Load password only if master password has already been entered!}
  PK_CRYPT_COPY_PASSWORD=4;         { Copy encrypted password to new archive name}
  PK_CRYPT_MOVE_PASSWORD=5;         { Move password when renaming an archive}
  PK_CRYPT_DELETE_PASSWORD=6;       { Delete password}

  PK_CRYPTOPT_MASTERPASS_SET = 1;   // The user already has a master password defined

  { THeaderData Flags }
  RHDF_ENCRYPTED = $04;             { File encrypted with password }

type
  { Unsigned integer with pointer size }
  TArcHandle = {$IFDEF CPU64}QWord{$ELSE}LongWord{$ENDIF};
  {$IFNDEF LCL}
  HWND = type PtrUInt; // Defined as in LCL
  {$ENDIF}

const
  wcxInvalidHandle = TArcHandle(-1);

{ For compatibility with Delphi use $IFDEF's to set calling convention }

type
  {Definition of callback functions called by the DLL}
  {Ask to swap disk for multi-volume archive}
  PChangeVolProc=^TChangeVolProc;
  TChangeVolProc=function(ArcName:pchar;Mode:longint):longint; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  PChangeVolProcW=^TChangeVolProcW;
  TChangeVolProcW=function(ArcName:pwidechar;Mode:longint):longint; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {Notify that data is processed - used for progress dialog}
  PProcessDataProc=^TProcessDataProc;
  TProcessDataProc=function(FileName:pchar;Size:longint):longint; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  PProcessDataProcW=^TProcessDataProcW;
  TProcessDataProcW=function(FileName:pwidechar;Size:longint):longint; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  PPkCryptProc = ^TPkCryptProc;
  TPkCryptProc = function(CryptoNr: Integer; Mode: Integer; ArchiveName,
                          Password: PAnsiChar; MaxLen: Integer): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  PPkCryptProcW = ^TPkCryptProcW;
  TPkCryptProcW = function(CryptoNr: Integer; Mode: Integer; ArchiveName,
                           Password: PWideChar; MaxLen: Integer): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

type
  PHeaderData = ^THeaderData;
  THeaderData=packed record
    ArcName:array [0..259] of char;
    FileName:array [0..259] of char;
    Flags,
    PackSize,
    UnpSize,
    HostOS,
    FileCRC,
    FileTime,
    UnpVer,
    Method,
    FileAttr:longint;
    CmtBuf:pchar;
    CmtBufSize,
    CmtSize,
    CmtState:longint;
  end;

  PHeaderDataEx = ^THeaderDataEx;
  THeaderDataEx=packed record
    ArcName:array [0..1023] of char;
    FileName:array [0..1023] of char;
    Flags:longint;
    PackSize,
    PackSizeHigh,
    UnpSize,
    UnpSizeHigh:longword;
    HostOS,
    FileCRC,
    FileTime,
    UnpVer,
    Method,
    FileAttr:longint;
    CmtBuf:pchar;
    CmtBufSize,
    CmtSize,
    CmtState:longint;
    Reserved:array[0..1023] of char;
  end;

  PHeaderDataExW=^THeaderDataExW;
  THeaderDataExW=packed record
    ArcName:array [0..1023] of widechar;
    FileName:array [0..1023] of widechar;
    Flags:longint;
    PackSize,
    PackSizeHigh,
    UnpSize,
    UnpSizeHigh:longword;
    HostOS,
    FileCRC,
    FileTime,
    UnpVer,
    Method,
    FileAttr:longint;
    CmtBuf:pchar;
    CmtBufSize,
    CmtSize,
    CmtState:longint;
    Reserved:array[0..1023] of char;
    MfileTime: UInt64;
  end;

  tOpenArchiveData=packed record
    ArcName:pchar;
    OpenMode,
    OpenResult:longint;
    CmtBuf:pchar;
    CmtBufSize,
    CmtSize,
    CmtState:longint;
  end;

  tOpenArchiveDataW=packed record
    ArcName:pwidechar;
    OpenMode,
    OpenResult:longint;
    CmtBuf:pwidechar;
    CmtBufSize,
    CmtSize,
    CmtState:longint;
  end;

  tPackDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..259] of char;
  end;
  pPackDefaultParamStruct=^tPackDefaultParamStruct;

implementation

end.



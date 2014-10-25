{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclStreams.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Robert Marquardt. Portions created by              }
{ Robert Marquardt are Copyright (C) Robert Marquardt (robert_marquardt att gmx dott de)           }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{   Heinz Zastrau                                                                                  }
{   Andreas Schmidt                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Stream-related functions and classes                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStreams;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.SysUtils, System.Classes,
  System.Contnrs,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  Contnrs,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclBase, JclStringConversions;

const
  StreamDefaultBufferSize = 4096;

type
  EJclStreamError = class(EJclError);

  // abstraction layer to support Delphi 5 and C++Builder 5 streams
  // 64 bit version of overloaded functions are introduced
  TJclStream = class(TStream)
  protected
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    procedure LoadFromStream(Source: TStream; BufferSize: Longint = StreamDefaultBufferSize); virtual;
    procedure LoadFromFile(const FileName: TFileName; BufferSize: Longint = StreamDefaultBufferSize); virtual;
    procedure SaveToStream(Dest: TStream; BufferSize: Longint = StreamDefaultBufferSize); virtual;
    procedure SaveToFile(const FileName: TFileName; BufferSize: Longint = StreamDefaultBufferSize); virtual;
  end;

  //=== VCL stream replacements ===

  TJclHandleStream = class(TJclStream)
  private
    FHandle: THandle;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;

  TJclFileStream = class(TJclHandleStream)
  public
    constructor Create(const FileName: TFileName; Mode: Word; Rights: Cardinal = $666);
    destructor Destroy; override;
  end;

  {
  TJclCustomMemoryStream = class(TJclStream)
  end;

  TJclMemoryStream = class(TJclCustomMemoryStream)
  end;

  TJclStringStream = class(TJclStream)
  end;

  TJclResourceStream = class(TJclCustomMemoryStream)
  end;
  }

  //=== new stream ideas ===

  TJclEmptyStream = class(TJclStream)
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclNullStream = class(TJclStream)
  private
    FPosition: Int64;
    FSize: Int64;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclRandomStream = class(TJclNullStream)
  protected
    function GetRandSeed: Longint; virtual;
    procedure SetRandSeed(Seed: Longint); virtual;
  public
    function RandomData: Byte; virtual;
    procedure Randomize; dynamic;
    function Read(var Buffer; Count: Longint): Longint; override;
    property RandSeed: Longint read GetRandSeed write SetRandSeed;
  end;

  TJclMultiplexStream = class(TJclStream)
  private
    FStreams: TList;
    FReadStreamIndex: Integer;
    function GetStream(Index: Integer): TStream;
    function GetCount: Integer;
    procedure SetStream(Index: Integer; const Value: TStream);
    function GetReadStream: TStream;
    procedure SetReadStream(const Value: TStream);
    procedure SetReadStreamIndex(const Value: Integer);
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    function Add(NewStream: TStream): Integer;
    procedure Clear;
    function Remove(AStream: TStream): Integer;
    procedure Delete(const Index: Integer);

    property Streams[Index: Integer]: TStream read GetStream write SetStream;
    property ReadStreamIndex: Integer read FReadStreamIndex write SetReadStreamIndex;
    property ReadStream: TStream read GetReadStream write SetReadStream;
    property Count: Integer read GetCount;
  end;

  TJclStreamDecorator = class(TJclStream)
  private
    FAfterStreamChange: TNotifyEvent;
    FBeforeStreamChange: TNotifyEvent;
    FOwnsStream: Boolean;
    FStream: TStream;
    procedure SetStream(Value: TStream);
  protected
    procedure DoAfterStreamChange; virtual;
    procedure DoBeforeStreamChange; virtual;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property AfterStreamChange: TNotifyEvent read FAfterStreamChange write FAfterStreamChange;
    property BeforeStreamChange: TNotifyEvent read FBeforeStreamChange write FBeforeStreamChange;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
    property Stream: TStream read FStream write SetStream;
  end;

  TJclBufferedStream = class(TJclStreamDecorator)
  protected
    FBuffer: array of Byte;
    FBufferCurrentSize: Longint;
    FBufferMaxModifiedPos: Longint;
    FBufferSize: Longint;
    FBufferStart: Int64; // position of the first byte of the buffer in stream
    FPosition: Int64; // current position in stream
    function BufferHit: Boolean;
    function GetCalcedSize: Int64; virtual;
    function LoadBuffer: Boolean; virtual;
    function ReadFromBuffer(var Buffer; Count, Start: Longint): Longint;
    function WriteToBuffer(const Buffer; Count, Start: Longint): Longint;
  protected
    procedure DoAfterStreamChange; override;
    procedure DoBeforeStreamChange; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    procedure Flush; virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property BufferSize: Longint read FBufferSize write FBufferSize;
  end;

  TStreamNotifyEvent = procedure(Sender: TObject; Position: Int64; Size: Int64) of object;

  TJclEventStream = class(TJclStreamDecorator)
  private
    FNotification: TStreamNotifyEvent;
    procedure DoNotification;
  protected
    procedure DoBeforeStreamChange; override;
    procedure DoAfterStreamChange; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AStream: TStream; ANotification: TStreamNotifyEvent = nil;
      AOwnsStream: Boolean = False);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property OnNotification: TStreamNotifyEvent read FNotification write FNotification;
  end;

  TJclEasyStream = class(TJclStreamDecorator)
  public
    function IsEqual(Stream: TStream): Boolean;
    function ReadBoolean: Boolean;
    function ReadChar: Char;
    function ReadAnsiChar: AnsiChar;
    function ReadWideChar: WideChar;
    function ReadByte: Byte;
    function ReadCurrency: Currency;
    function ReadDateTime: TDateTime;
    function ReadExtended: Extended;
    function ReadDouble: Double;
    function ReadInt64: Int64;
    function ReadInteger: Integer;
    function ReadCString: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function ReadCAnsiString: AnsiString;
    function ReadCWideString: WideString;
    function ReadShortString: string;
    function ReadSingle: Single;
    function ReadSizedString: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    function ReadSizedAnsiString: AnsiString;
    function ReadSizedWideString: WideString;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteChar(Value: Char);
    procedure WriteAnsiChar(Value: AnsiChar);
    procedure WriteWideChar(Value: WideChar);
    procedure WriteByte(Value: Byte);
    procedure WriteCurrency(const Value: Currency);
    procedure WriteDateTime(const Value: TDateTime);
    procedure WriteExtended(const Value: Extended);
    procedure WriteDouble(const Value: Double);
    procedure WriteInt64(Value: Int64); overload;
    procedure WriteInteger(Value: Integer); overload;
    procedure WriteCString(const Value: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure WriteCAnsiString(const Value: AnsiString);
    procedure WriteCWideString(const Value: WideString);
    // use WriteCString
    procedure WriteShortString(const Value: ShortString);
    procedure WriteSingle(const Value: Single);
    procedure WriteSizedString(const Value: string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
    procedure WriteSizedAnsiString(const Value: AnsiString);
    procedure WriteSizedWideString(const Value: WideString);
  end;

  TJclScopedStream = class(TJclStream)
  private
    FParentStream: TStream;
    FStartPos: Int64;
    FCurrentPos: Int64;
    FMaxSize: Int64;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    // scopedstream starting at the current position of the ParentStream
    //   if MaxSize is positive or null, read and write operations cannot overrun this size or the ParentStream limitation
    //   if MaxSize is negative, read and write operations are unlimited (up to the ParentStream limitation)
    constructor Create(AParentStream: TStream; const AMaxSize: Int64 = -1); overload;
    constructor Create(AParentStream: TStream; const AStartPos, AMaxSize: Int64); overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property ParentStream: TStream read FParentStream;
    property StartPos: Int64 read FStartPos;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
  end;

  TJclStreamSeekEvent = function(Sender: TObject; const Offset: Int64;
    Origin: TSeekOrigin): Int64 of object;
  TJclStreamReadEvent = function(Sender: TObject; var Buffer; Count: Longint): Longint of object;
  TJclStreamWriteEvent = function(Sender: TObject; const Buffer;Count: Longint): Longint of object;
  TJclStreamSizeEvent = procedure(Sender: TObject; const NewSize: Int64) of object;

  TJclDelegatedStream = class(TJclStream)
  private
    FOnSeek: TJclStreamSeekEvent;
    FOnRead: TJclStreamReadEvent;
    FOnWrite: TJclStreamWriteEvent;
    FOnSize: TJclStreamSizeEvent;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property OnSeek: TJclStreamSeekEvent read FOnSeek write FOnSeek;
    property OnRead: TJclStreamReadEvent read FOnRead write FOnRead;
    property OnWrite: TJclStreamWriteEvent read FOnWrite write FOnWrite;
    property OnSize: TJclStreamSizeEvent read FOnSize write FOnSize;
  end;

  // ancestor classes for streams with checksums and encrypted streams
  // data are stored in sectors: each BufferSize-d buffer is followed by FSectorOverHead bytes
  // containing the checksum. In case of an encrypted stream, there is no byte
  // but sector is encrypted

  // reusing some code from TJclBufferedStream
  TJclSectoredStream = class(TJclBufferedStream)
  protected
    FSectorOverHead: Longint;
    function FlatToSectored(const Position: Int64): Int64;
    function SectoredToFlat(const Position: Int64): Int64;
    function GetCalcedSize: Int64; override;
    function LoadBuffer: Boolean; override;
    procedure DoAfterStreamChange; override;
    procedure AfterBlockRead; virtual;   // override to check protection
    procedure BeforeBlockWrite; virtual; // override to compute protection
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AStorageStream: TStream; AOwnsStream: Boolean = False;
      ASectorOverHead: Longint = 0);

    procedure Flush; override;
  end;

  TJclCRC16Stream = class(TJclSectoredStream)
  protected
    procedure AfterBlockRead; override;
    procedure BeforeBlockWrite; override;
  public
    constructor Create(AStorageStream: TStream; AOwnsStream: Boolean = False);
  end;

  TJclCRC32Stream = class(TJclSectoredStream)
  protected
    procedure AfterBlockRead; override;
    procedure BeforeBlockWrite; override;
  public
    constructor Create(AStorageStream: TStream; AOwnsStream: Boolean = False);
  end;

  {$IFDEF COMPILER7_UP}
    {$DEFINE SIZE64}
  {$ENDIF ~COMPILER7_UP}
  {$IFDEF FPC}
    {$DEFINE SIZE64}
  {$ENDIF FPC}
  TJclSplitStream = class(TJclStream)
  private
    FVolume: TStream;
    FVolumeIndex: Integer;
    FVolumeMaxSize: Int64;
    FPosition: Int64;
    FVolumePosition: Int64;
    FForcePosition: Boolean;
  protected
    function GetVolume(Index: Integer): TStream; virtual; abstract;
    function GetVolumeMaxSize(Index: Integer): Int64; virtual; abstract;
    function GetSize: Int64; {$IFDEF SIZE64}override;{$ENDIF SIZE64}
    procedure SetSize(const NewSize: Int64); override;
    function InternalLoadVolume(Index: Integer): Boolean;
  public
    constructor Create(AForcePosition: Boolean = False);

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property ForcePosition: Boolean read FForcePosition write FForcePosition;
  end;

  TJclVolumeEvent = function(Index: Integer): TStream of object;
  TJclVolumeMaxSizeEvent = function(Index: Integer): Int64 of object;

  TJclDynamicSplitStream = class(TJclSplitStream)
  private
    FOnVolume: TJclVolumeEvent;
    FOnVolumeMaxSize: TJclVolumeMaxSizeEvent;
  protected
    function GetVolume(Index: Integer): TStream; override;
    function GetVolumeMaxSize(Index: Integer): Int64; override;
  public
    property OnVolume: TJclVolumeEvent read FOnVolume write FOnVolume;
    property OnVolumeMaxSize: TJclVolumeMaxSizeEvent read FOnVolumeMaxSize
      write FOnVolumeMaxSize;
  end;

  TJclSplitVolume = class
  public
    MaxSize: Int64;
    Stream: TStream;
    OwnStream: Boolean;
  end;

  TJclStaticSplitStream = class(TJclSplitStream)
  private
    FVolumes: TObjectList;
    function GetVolumeCount: Integer;
  protected
    function GetVolume(Index: Integer): TStream; override;
    function GetVolumeMaxSize(Index: Integer): Int64; override;
  public
    constructor Create(AForcePosition: Boolean = False);
    destructor Destroy; override;

    function AddVolume(AStream: TStream; AMaxSize: Int64 = 0;
      AOwnStream: Boolean = False): Integer;

    property VolumeCount: Integer read GetVolumeCount;
    property Volumes[Index: Integer]: TStream read GetVolume;
    property VolumeMaxSizes[Index: Integer]: Int64 read GetVolumeMaxSize;
  end;

  TJclStringStream = class
  protected
    FStream: TStream;
    FOwnStream: Boolean;
    FBOM: array of Byte;
    FBufferSize: SizeInt;
    FStrPosition: Int64; // current position in characters
    FStrBuffer: TUCS4Array; // buffer for read/write operations
    FStrBufferPosition: Int64; // position of the first character of the read/write buffer
    FStrBufferCurrentSize: Int64; // numbers of characters available in str buffer
    FStrBufferModifiedSize: Int64; // numbers of characters modified in str buffer
    FStrBufferStart: Int64; // position of the first byte of the read/write buffer in stream
    FStrBufferNext: Int64; // position of the next character following the read/write buffer in stream
    FStrPeekPosition: Int64; // current peek position in characters
    FStrPeekBuffer: TUCS4Array; // buffer for peek operations
    FStrPeekBufferPosition: Int64; // index of the first character of the peek buffer
    FStrPeekBufferCurrentSize: SizeInt; // numbers of characters available in peek buffer
    FStrPeekBufferStart: Int64; // position of the first byte of the peek buffer in stream
    FStrPeekBufferNext: Int64; // position of the next character following the peek buffer in stream
    function LoadBuffer: Boolean;
    function LoadPeekBuffer: Boolean;
    function InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean; virtual; abstract;
    function InternalGetNextBuffer(S: TStream; var Buffer: TUCS4Array; Start, Count: SizeInt): Longint; virtual;
    function InternalSetNextChar(S: TStream; Ch: UCS4): Boolean; virtual; abstract;
    function InternalSetNextBuffer(S: TStream; const Buffer: TUCS4Array; Start, Count: SizeInt): Longint; virtual;
    procedure InvalidateBuffers;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); virtual;
    destructor Destroy; override;
    procedure Flush; virtual;
    function ReadString(var Buffer: string; Start, Count: Longint): Longint; overload;
    function ReadString(BufferSize: Longint = StreamDefaultBufferSize): string; overload;
    function ReadAnsiString(var Buffer: AnsiString; Start, Count: Longint): Longint; overload;
    function ReadAnsiString(BufferSize: Longint = StreamDefaultBufferSize): AnsiString; overload;
    function ReadWideString(var Buffer: WideString; Start, Count: Longint): Longint; overload;
    function ReadWideString(BufferSize: Longint = StreamDefaultBufferSize): WideString; overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;
    function WriteString(const Buffer: string; Start, Count: Longint): Longint;
    function WriteAnsiString(const Buffer: AnsiString; Start, Count: Longint): Longint;
    function WriteWideString(const Buffer: WideString; Start, Count: Longint): Longint;
    function PeekChar(out Buffer: Char): Boolean;
    function PeekAnsiChar(out Buffer: AnsiChar): Boolean;
    function PeekUCS4(out Buffer: UCS4): Boolean;
    function PeekWideChar(out Buffer: WideChar): Boolean;
    function ReadChar(out Buffer: Char): Boolean;
    function ReadAnsiChar(out Buffer: AnsiChar): Boolean;
    function ReadUCS4(out Buffer: UCS4): Boolean;
    function ReadWideChar(out Buffer: WideChar): Boolean;
    function WriteChar(Value: Char): Boolean;
    function WriteAnsiChar(Value: AnsiChar): Boolean;
    function WriteUCS4(Value: UCS4): Boolean;
    function WriteWideChar(Value: WideChar): Boolean;
    function SkipBOM: LongInt; virtual;
    function WriteBOM: Longint; virtual;
    property BufferSize: SizeInt read FBufferSize write FBufferSize;
    property PeekPosition: Int64 read FStrPeekPosition;
    property Position: Int64 read FStrPosition;
    property Stream: TStream read FStream;
    property OwnStream: Boolean read FOwnStream;
  end;

  TJclStringStreamClass = class of TJclStringStream;

  TJclAnsiStream = class(TJclStringStream)
  private
    FCodePage: Word;
  protected
    function InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean; override;
    function InternalGetNextBuffer(S: TStream; var Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
    function InternalSetNextChar(S: TStream; Ch: UCS4): Boolean; override;
    function InternalSetNextBuffer(S: TStream; const Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); override;
    property CodePage: Word read FCodePage write FCodePage;
  end;

  TJclUTF8Stream = class(TJclStringStream)
  protected
    function InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean; override;
    function InternalGetNextBuffer(S: TStream; var Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
    function InternalSetNextChar(S: TStream; Ch: UCS4): Boolean; override;
    function InternalSetNextBuffer(S: TStream; const Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); override;
  end;

  TJclUTF16Stream = class(TJclStringStream)
  protected
    function InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean; override;
    function InternalGetNextBuffer(S: TStream; var Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
    function InternalSetNextChar(S: TStream; Ch: UCS4): Boolean; override;
    function InternalSetNextBuffer(S: TStream; const Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); override;
  end;

  TJclStringEncoding = (seAnsi, seUTF8, seUTF16, seAuto);

  TJclAutoStream = class(TJclStringStream)
  private
    FCodePage: Word;
    FEncoding: TJclStringEncoding;
    procedure SetCodePage(Value: Word);
  protected
    function InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean; override;
    function InternalGetNextBuffer(S: TStream; var Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
    function InternalSetNextChar(S: TStream; Ch: UCS4): Boolean; override;
    function InternalSetNextBuffer(S: TStream; const Buffer: TUCS4Array; Start, Count: SizeInt): Longint; override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); override;
    function SkipBOM: LongInt; override;
    property CodePage: Word read FCodePage write SetCodePage;
    property Encoding: TJclStringEncoding read FEncoding;
  end;

// buffered copy of all available bytes from Source to Dest
// returns the number of bytes that were copied
function StreamCopy(Source: TStream; Dest: TStream; BufferSize: Longint = StreamDefaultBufferSize): Int64;

// buffered copy of all available characters from Source to Dest
// retuns the number of characters (in specified encoding) that were copied
function StringStreamCopy(Source, Dest: TJclStringStream; BufferLength: Longint = StreamDefaultBufferSize): Int64;
function AnsiStringStreamCopy(Source, Dest: TJclStringStream; BufferLength: Longint = StreamDefaultBufferSize): Int64;
function WideStringStreamCopy(Source, Dest: TJclStringStream; BufferLength: Longint = StreamDefaultBufferSize): Int64;

// compares 2 streams for differencies
function CompareStreams(A, B : TStream; BufferSize: Longint = StreamDefaultBufferSize): Boolean;
// compares 2 files for differencies (calling CompareStreams)
function CompareFiles(const FileA, FileB: TFileName; BufferSize: Longint = StreamDefaultBufferSize): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Types,
  {$ENDIF HAS_UNITSCOPE}
  JclResources,
  JclCharsets,
  JclMath,
  JclSysUtils;

function StreamCopy(Source: TStream; Dest: TStream; BufferSize: Longint): Int64;
var
  Buffer: array of Byte;
  ByteCount: Longint;
begin
  Result := 0;
  SetLength(Buffer, BufferSize);
  repeat
    ByteCount := Source.Read(Buffer[0], BufferSize);
    Result := Result + ByteCount;
    Dest.WriteBuffer(Buffer[0], ByteCount);
  until ByteCount < BufferSize;
end;

function StringStreamCopy(Source, Dest: TJclStringStream; BufferLength: Longint): Int64;
var
  Buffer: string;
  CharCount: Longint;
begin
  Result := 0;
  SetLength(Buffer, BufferLength);
  repeat
    CharCount := Source.ReadString(Buffer, 1, BufferLength);
    Result := Result + CharCount;
    CharCount := Dest.WriteString(Buffer, 1, CharCount);
  until CharCount = 0;
end;

function AnsiStringStreamCopy(Source, Dest: TJclStringStream; BufferLength: Longint): Int64;
var
  Buffer: AnsiString;
  CharCount: Longint;
begin
  Result := 0;
  SetLength(Buffer, BufferLength);
  repeat
    CharCount := Source.ReadAnsiString(Buffer, 1, BufferLength);
    Result := Result + CharCount;
    CharCount := Dest.WriteAnsiString(Buffer, 1, CharCount);
  until CharCount = 0;
end;

function WideStringStreamCopy(Source, Dest: TJclStringStream; BufferLength: Longint): Int64;
var
  Buffer: WideString;
  CharCount: Longint;
begin
  Result := 0;
  SetLength(Buffer, BufferLength);
  repeat
    CharCount := Source.ReadWideString(Buffer, 1, BufferLength);
    Result := Result + CharCount;
    CharCount := Dest.WriteWideString(Buffer, 1, CharCount);
  until CharCount = 0;
end;

function CompareStreams(A, B : TStream; BufferSize: Longint): Boolean;
var
  BufferA, BufferB: array of Byte;
  ByteCountA, ByteCountB: Longint;
begin
  SetLength(BufferA, BufferSize);
  try
    SetLength(BufferB, BufferSize);
    try
      repeat
        ByteCountA := A.Read(BufferA[0], BufferSize);
        ByteCountB := B.Read(BufferB[0], BufferSize);

        Result := (ByteCountA = ByteCountB);
        Result := Result and CompareMem(BufferA, BufferB, ByteCountA);
      until (ByteCountA <> BufferSize) or (ByteCountB <> BufferSize) or not Result;
    finally
      SetLength(BufferB, 0);
    end;
  finally
    SetLength(BufferA, 0);
  end;
end;

function CompareFiles(const FileA, FileB: TFileName; BufferSize: Longint): Boolean;
var
  A, B: TStream;
begin
  A := TFileStream.Create(FileA, fmOpenRead or fmShareDenyWrite);
  try
    B := TFileStream.Create(FileB, fmOpenRead or fmShareDenyWrite);
    try
      Result := CompareStreams(A, B, BufferSize);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

//=== { TJclStream } =========================================================

function TJclStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  Result64: Int64;
begin
  case Origin of
    soFromBeginning:
      Result64 := Seek(Int64(Offset), soBeginning);
    soFromCurrent:
      Result64 := Seek(Int64(Offset), soCurrent);
    soFromEnd:
      Result64 := Seek(Int64(Offset), soEnd);
  else
    Result64 := -1;
  end;
  if (Result64 < 0) or (Result64 > High(Longint)) then
    Result64 := -1;
  Result := Result64;
end;

procedure TJclStream.LoadFromFile(const FileName: TFileName;
  BufferSize: Integer);
var
  FS: TStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FS, BufferSize);
  finally
    FS.Free;
  end;
end;

procedure TJclStream.LoadFromStream(Source: TStream; BufferSize: Integer);
begin
  StreamCopy(Source, Self, BufferSize);
end;

procedure TJclStream.SaveToFile(const FileName: TFileName; BufferSize: Integer);
var
  FS: TStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS, BufferSize);
  finally
    FS.Free;
  end;
end;

procedure TJclStream.SaveToStream(Dest: TStream; BufferSize: Integer);
begin
  StreamCopy(Self, Dest, BufferSize);
end;

function TJclStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // override to customize
  Result := -1;
end;

procedure TJclStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TJclStream.SetSize(const NewSize: Int64);
begin
  // override to customize
end;

//=== { TJclHandleStream } ===================================================

constructor TJclHandleStream.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TJclHandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  if (Count <= 0) or not ReadFile(Handle, Buffer, DWORD(Count), DWORD(Result), nil) then
    Result := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := __read(Handle, Buffer, Count);
  {$ENDIF LINUX}
end;

function TJclHandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  if (Count <= 0) or not WriteFile(Handle, Buffer, DWORD(Count), DWORD(Result), nil) then
    Result := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := __write(Handle, Buffer, Count);
  {$ENDIF LINUX}
end;

{$IFDEF MSWINDOWS}
function TJclHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  INVALID_SET_FILE_POINTER = -1;
type
  TLarge = record
    case Boolean of
    False:
     (OffsetLo: Longint;
      OffsetHi: Longint);
    True:
      (Offset64: Int64);
  end;
var
  Offs: TLarge;
begin
  Offs.Offset64 := Offset;
  Offs.OffsetLo := SetFilePointer(Handle, Offs.OffsetLo, @Offs.OffsetHi, Ord(Origin));
  if (Offs.OffsetLo = INVALID_SET_FILE_POINTER) and (GetLastError <> NO_ERROR) then
    Result := -1
  else
    Result := Offs.Offset64;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function TJclHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  SeekOrigins: array [TSeekOrigin] of Cardinal = ( SEEK_SET {soBeginning}, SEEK_CUR {soCurrent}, SEEK_END {soEnd} );
begin
  Result := lseek(Handle, Offset, SeekOrigins[Origin]);
end;
{$ENDIF LINUX}

procedure TJclHandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
  {$IFDEF MSWINDOWS}
  if not SetEndOfFile(Handle) then
    RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if ftruncate(Handle, Position) = -1 then
    raise EJclStreamError.CreateRes(@RsStreamsSetSizeError);
  {$ENDIF LINUX}
end;

//=== { TJclFileStream } =====================================================

constructor TJclFileStream.Create(const FileName: TFileName; Mode: Word; Rights: Cardinal);
var
  H: THandle;
{$IFDEF LINUX}
const
  INVALID_HANDLE_VALUE = -1;
{$ENDIF LINUX}
begin
  if Mode = fmCreate then
  begin
    {$IFDEF LINUX}
    H := open(PChar(FileName), O_CREAT or O_RDWR, Rights);
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
    H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    {$ENDIF MSWINDOWS}
    inherited Create(H);
    if Handle = INVALID_HANDLE_VALUE then
      raise EJclStreamError.CreateResFmt(@RsStreamsCreateError, [FileName]);
  end
  else
  begin
    H := THandle(FileOpen(FileName, Mode));
    inherited Create(H);
    if Handle = INVALID_HANDLE_VALUE then
      raise EJclStreamError.CreateResFmt(@RsStreamsOpenError, [FileName]);
  end;
end;

destructor TJclFileStream.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  __close(Handle);
  {$ENDIF LINUX}
  inherited Destroy;
end;

//=== { TJclEmptyStream } ====================================================

// a stream which stays empty no matter what you do
// so it is a Unix /dev/null equivalent

procedure TJclEmptyStream.SetSize(const NewSize: Int64);
begin
  // nothing
end;

function TJclEmptyStream.Read(var Buffer; Count: Longint): Longint;
begin
  // you cannot read anything
  Result := 0;
end;

function TJclEmptyStream.Write(const Buffer; Count: Longint): Longint;
begin
  // you cannot write anything
  Result := 0;
end;

function TJclEmptyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Offset <> 0 then
    // seeking to anywhere except the position 0 is an error
    Result := -1
  else
    Result := 0;
end;

//=== { TJclNullStream } =====================================================

// a stream which only keeps position and size, but no data
// so it is a Unix /dev/zero equivalent (?)

procedure TJclNullStream.SetSize(const NewSize: Int64);
begin
  if NewSize > 0 then
    FSize := NewSize
  else
    FSize := 0;
  if FPosition > FSize then
    FPosition := FSize;
end;

function TJclNullStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count < 0 then
    Count := 0;
  // FPosition > FSize is possible!
  if FSize - FPosition < Count then
    Count := FSize - FPosition;
  // does not read if beyond EOF
  if Count > 0 then
  begin
    ResetMemory(Buffer, Count);
    FPosition := FPosition + Count;
  end;
  Result := Count;
end;

function TJclNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count < 0 then
    Count := 0;
  FPosition := FPosition + Count;
  // writing when FPosition > FSize is possible!
  if FPosition > FSize then
    FSize := FPosition;
  Result := Count;
end;

function TJclNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  Rel: Int64;
begin
  case Origin of
    soBeginning:
      Rel := 0;
    soCurrent:
      Rel := FPosition;
    soEnd:
      Rel := FSize;
  else
    // force Rel + Offset = -1 (code is never reached)
    Rel := Offset - 1;
  end;
  if Rel + Offset >= 0 then
  begin
    // all non-negative destination positions including beyond EOF are valid
    FPosition := Rel + Offset;
    Result := FPosition;
  end
  else
    Result := -1;
end;

//=== { TJclRandomStream } ===================================================

// A TJclNullStream decendant which returns random data when read
// so it is a Unix /dev/random equivalent

function TJclRandomStream.GetRandSeed: Longint;
begin
  Result := System.RandSeed;
end;

procedure TJclRandomStream.SetRandSeed(Seed: Longint);
begin
  System.RandSeed := Seed;
end;

function TJclRandomStream.RandomData: Byte;
begin
  Result := System.Random(256);
end;

procedure TJclRandomStream.Randomize;
begin
  System.Randomize;
end;

function TJclRandomStream.Read(var Buffer; Count: Longint): Longint;
var
  I: Longint;
  BufferPtr: PByte;
begin
  // this handles all necessary checks
  Count := inherited Read(Buffer, Count);
  BufferPtr := @Buffer;
  for I := 0 to Count - 1 do
  begin
    BufferPtr^ := RandomData;
    Inc(BufferPtr);
  end;
  Result := Count;
end;

//=== { TJclMultiplexStream } ================================================

constructor TJclMultiplexStream.Create;
begin
  inherited Create;
  FStreams := TList.Create;
  FReadStreamIndex := -1;
end;

destructor TJclMultiplexStream.Destroy;
begin
  FStreams.Free;
  inherited Destroy;
end;

function TJclMultiplexStream.Add(NewStream: TStream): Integer;
begin
  Result := FStreams.Add(Pointer(NewStream));
end;

procedure TJclMultiplexStream.Clear;
begin
  FStreams.Clear;
  FReadStreamIndex := -1;
end;

procedure TJclMultiplexStream.Delete(const Index: Integer);
begin
  FStreams.Delete(Index);
  if ReadStreamIndex = Index then
    FReadStreamIndex := -1
  else
  if ReadStreamIndex > Index then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.GetReadStream: TStream;
begin
  if FReadStreamIndex >= 0 then
    Result := TStream(FStreams.Items[FReadStreamIndex])
  else
    Result := nil;
end;

function TJclMultiplexStream.GetStream(Index: Integer): TStream;
begin
  Result := TStream(FStreams.Items[Index]);
end;

function TJclMultiplexStream.GetCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclMultiplexStream.Read(var Buffer; Count: Longint): Longint;
var
  Stream: TStream;
begin
  Stream := ReadStream;
  if Assigned(Stream) then
    Result := Stream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TJclMultiplexStream.Remove(AStream: TStream): Integer;
begin
  Result := FStreams.Remove(Pointer(AStream));
  if FReadStreamIndex = Result then
    FReadStreamIndex := -1
  else
  if FReadStreamIndex > Result then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // what should this function do?
  Result := -1;
end;

procedure TJclMultiplexStream.SetReadStream(const Value: TStream);
begin
  FReadStreamIndex := FStreams.IndexOf(Pointer(Value));
end;

procedure TJclMultiplexStream.SetReadStreamIndex(const Value: Integer);
begin
  FReadStreamIndex := Value;
end;

procedure TJclMultiplexStream.SetSize(const NewSize: Int64);
begin
  // what should this function do?
end;

procedure TJclMultiplexStream.SetStream(Index: Integer; const Value: TStream);
begin
  FStreams.Items[Index] := Pointer(Value);
end;

function TJclMultiplexStream.Write(const Buffer; Count: Longint): Longint;
var
  Index: Integer;
  ByteWritten, MinByteWritten: Longint;
begin
  MinByteWritten := Count;
  for Index := 0 to Self.Count - 1 do
  begin
    ByteWritten := TStream(FStreams.Items[Index]).Write(Buffer, Count);
    if ByteWritten < MinByteWritten then
      MinByteWritten := ByteWritten;
  end;
  Result := MinByteWritten;
end;

//=== { TJclStreamDecorator } ================================================

constructor TJclStreamDecorator.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TJclStreamDecorator.Destroy;
begin
  if OwnsStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TJclStreamDecorator.DoAfterStreamChange;
begin
  if Assigned(FAfterStreamChange) then
    FAfterStreamChange(Self);
end;

procedure TJclStreamDecorator.DoBeforeStreamChange;
begin
  if Assigned(FBeforeStreamChange) then
    FBeforeStreamChange(Self);
end;

function TJclStreamDecorator.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(FStream) then
    Result := Stream.Read(Buffer, Count)
  else
    Result := 0;
end;

function TJclStreamDecorator.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := Stream.Seek(Offset, Origin);
end;

procedure TJclStreamDecorator.SetSize(const NewSize: Int64);
begin
  if Assigned(FStream) then
    Stream.Size := NewSize;
end;

procedure TJclStreamDecorator.SetStream(Value: TStream);
begin
  if Value <> FStream then
    try
      DoBeforeStreamChange;
    finally
      if OwnsStream then
        FStream.Free;
      FStream := Value;
      DoAfterStreamChange;
    end;
end;

function TJclStreamDecorator.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FStream) then
    Result := Stream.Write(Buffer, Count)
  else
    Result := 0;
end;

//=== { TJclBufferedStream } =================================================

constructor TJclBufferedStream.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  inherited Create(AStream, AOwnsStream);
  if Stream <> nil then
    FPosition := Stream.Position;
  BufferSize := StreamDefaultBufferSize;
  LoadBuffer;
end;

destructor TJclBufferedStream.Destroy;
begin
  Flush;
  inherited Destroy;
end;

function TJclBufferedStream.BufferHit: Boolean;
begin
  Result := (FBufferStart <= FPosition) and (FPosition < (FBufferStart + FBufferCurrentSize));
end;

procedure TJclBufferedStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  FBufferCurrentSize := 0; // invalidate buffer after stream is changed
  FBufferStart := 0;
  if Stream <> nil then
    FPosition := Stream.Position;
end;

procedure TJclBufferedStream.DoBeforeStreamChange;
begin
  inherited DoBeforeStreamChange;
  Flush;
end;

procedure TJclBufferedStream.Flush;
begin
  if (Stream <> nil) and (FBufferMaxModifiedPos > 0) then
  begin
    Stream.Position := FBufferStart;
    Stream.WriteBuffer(FBuffer[0], FBufferMaxModifiedPos);
    FBufferMaxModifiedPos := 0;
  end;
end;

function TJclBufferedStream.GetCalcedSize: Int64;
begin
  if Assigned(Stream) then
    Result := Stream.Size
  else
    Result := 0;
  if Result < FBufferMaxModifiedPos + FBufferStart then
    Result := FBufferMaxModifiedPos + FBufferStart;
end;

function TJclBufferedStream.LoadBuffer: Boolean;
begin
  Flush;
  if Length(FBuffer) <> FBufferSize then
    SetLength(FBuffer, FBufferSize);
  if Stream <> nil then
  begin
    Stream.Position := FPosition;
    FBufferCurrentSize := Stream.Read(FBuffer[0], FBufferSize);
  end
  else
    FBufferCurrentSize := 0;
  FBufferStart := FPosition;
  Result := (FBufferCurrentSize > 0);
end;

function TJclBufferedStream.Read(var Buffer; Count: Longint): Longint;
const
  Offset = 0;
begin
  Result := Count + Offset;
  while Count > 0 do
  begin
    if not BufferHit then
      if not LoadBuffer then
        Break;
    Dec(Count, ReadFromBuffer(Buffer, Count, Result - Count));
  end;
  Result := Result - Count - Offset;
end;

function TJclBufferedStream.ReadFromBuffer(var Buffer; Count, Start: Longint): Longint;
var
  BufPos: Longint;
  P: PAnsiChar;
begin
  Result := Count;
  BufPos := FPosition - FBufferStart;
  if Result > FBufferCurrentSize - BufPos then
    Result := FBufferCurrentSize - BufPos;
  P := @Buffer;
  Move(FBuffer[BufPos], P[Start], Result);
  Inc(FPosition, Result);
end;

function TJclBufferedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  NewPos: Int64;
begin
  NewPos := FPosition;
  case Origin of
    soBeginning:
      NewPos := Offset;
    soCurrent:
      Inc(NewPos, Offset);
    soEnd:
      NewPos := GetCalcedSize + Offset;
  else
    NewPos := -1;
  end;
  if NewPos < 0 then
    NewPos := -1
  else
    FPosition := NewPos;
  Result := NewPos;
end;

procedure TJclBufferedStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(NewSize);
  if NewSize < (FBufferStart + FBufferMaxModifiedPos) then
  begin
    FBufferMaxModifiedPos := NewSize - FBufferStart;
    if FBufferMaxModifiedPos < 0 then
      FBufferMaxModifiedPos := 0;
  end;
  if NewSize < (FBufferStart + FBufferCurrentSize) then
  begin
    FBufferCurrentSize := NewSize - FBufferStart;
    if FBufferCurrentSize < 0 then
      FBufferCurrentSize := 0;
  end;
  // fix from Marcelo Rocha
  if Stream <> nil then
    FPosition := Stream.Position;
end;

function TJclBufferedStream.Write(const Buffer; Count: Longint): Longint;
const
  Offset = 0;
begin
  Result := Count + Offset;
  while Count > 0 do
  begin
    if (FBufferStart > FPosition) or (FPosition >= (FBufferStart + FBufferSize)) then
      LoadBuffer;
    Dec(Count, WriteToBuffer(Buffer, Count, Result - Count));
  end;
  Result := Result - Count - Offset;
end;

function TJclBufferedStream.WriteToBuffer(const Buffer; Count, Start: Longint): Longint;
var
  BufPos: Longint;
  P: PAnsiChar;
begin
  Result := Count;
  BufPos := FPosition - FBufferStart;
  if Result > Length(FBuffer) - BufPos then
    Result := Length(FBuffer) - BufPos;
  if FBufferCurrentSize < BufPos + Result then
    FBufferCurrentSize := BufPos + Result;
  P := @Buffer;
  Move(P[Start], FBuffer[BufPos], Result);
  if FBufferMaxModifiedPos < BufPos + Result then
    FBufferMaxModifiedPos := BufPos + Result;
  Inc(FPosition, Result);
end;

//=== { TJclEventStream } ====================================================

constructor TJclEventStream.Create(AStream: TStream; ANotification:
  TStreamNotifyEvent = nil; AOwnsStream: Boolean = False);
begin
  inherited Create(AStream, AOwnsStream);
  FNotification := ANotification;
end;

procedure TJclEventStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  if Stream <> nil then
    DoNotification;
end;

procedure TJclEventStream.DoBeforeStreamChange;
begin
  inherited DoBeforeStreamChange;
  if Stream <> nil then
    DoNotification;
end;

procedure TJclEventStream.DoNotification;
begin
  if Assigned(FNotification) then
    FNotification(Self, Stream.Position, Stream.Size);
end;

function TJclEventStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := inherited Read(Buffer, Count);
  DoNotification;
end;

function TJclEventStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := inherited Seek(Offset, Origin);
  DoNotification;
end;

procedure TJclEventStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(NewSize);
  DoNotification;
end;

function TJclEventStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  DoNotification;
end;

//=== { TJclEasyStream } =====================================================

function TJclEasyStream.IsEqual(Stream: TStream): Boolean;
var
  SavePos, StreamSavePos: Int64;
begin
  SavePos := Position;
  StreamSavePos := Stream.Position;
  try
    Position := 0;
    Stream.Position := 0;
    Result := CompareStreams(Self, Stream);
  finally
    Position := SavePos;
    Stream.Position := StreamSavePos;
  end;
end;

function TJclEasyStream.ReadBoolean: Boolean;
begin
  Result := False;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadChar: Char;
begin
  Result := #0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadAnsiChar: AnsiChar;
begin
  Result := #0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadWideChar: WideChar;
begin
  Result := #0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadByte: Byte;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadCurrency: Currency;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadDateTime: TDateTime;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadDouble: Double;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadExtended: Extended;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadInt64: Int64;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadInteger: Integer;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadCString: string;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := ReadCWideString;
  {$ELSE ~SUPPORTS_UNICODE}
  Result := ReadCAnsiString;
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function TJclEasyStream.ReadCAnsiString: AnsiString;
var
  CurrPos: Longint;
  StrSize: Integer;
begin
  CurrPos := Position;
  repeat
  until ReadAnsiChar = #0;
  StrSize := Position - CurrPos;                       // Get number of bytes
  SetLength(Result, StrSize div SizeOf(AnsiChar) - 1); // Set number of chars without #0
  Position := CurrPos;                                 // Seek to start read
  ReadBuffer(Result[1], StrSize);                      // Read ansi data and #0
end;

function TJclEasyStream.ReadCWideString: WideString;
var
  CurrPos: Integer;
  StrSize: Integer;
begin
  CurrPos := Position;
  repeat
  until ReadWideChar = #0;
  StrSize := Position - CurrPos;                       // Get number of bytes
  SetLength(Result, StrSize div SizeOf(WideChar) - 1); // Set number of chars without #0
  Position := CurrPos;                                 // Seek to start read
  ReadBuffer(Result[1], StrSize);                      // Read wide data and #0
end;

function TJclEasyStream.ReadShortString: string;
var
  StrSize: Integer;
begin
  StrSize := Ord(ReadChar);
  SetString(Result, PChar(nil), StrSize);
  ReadBuffer(Pointer(Result)^, StrSize);
end;

function TJclEasyStream.ReadSingle: Single;
begin
  Result := 0;
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadSizedString: string;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := ReadSizedWideString;
  {$ELSE ~SUPPORTS_UNICODE}
  Result := ReadSizedAnsiString;
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function TJclEasyStream.ReadSizedAnsiString: AnsiString;
var
  StrSize: Integer;
begin
  StrSize := ReadInteger;
  SetLength(Result, StrSize);
  ReadBuffer(Result[1], StrSize * SizeOf(Result[1]));
end;

function TJclEasyStream.ReadSizedWideString: WideString;
var
  StrSize: Integer;
begin
  StrSize := ReadInteger;
  SetLength(Result, StrSize);
  ReadBuffer(Result[1], StrSize * SizeOf(Result[1]));
end;

procedure TJclEasyStream.WriteBoolean(Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteChar(Value: Char);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteAnsiChar(Value: AnsiChar);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteWideChar(Value: WideChar);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteByte(Value: Byte);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteCurrency(const Value: Currency);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteDateTime(const Value: TDateTime);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteDouble(const Value: Double);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteExtended(const Value: Extended);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteInt64(Value: Int64);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteInteger(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteCString(const Value: string);
begin
  {$IFDEF SUPPORTS_UNICODE}
  WriteCWideString(Value);
  {$ELSE ~SUPPORTS_UNICODE}
  WriteCAnsiString(Value);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

procedure TJclEasyStream.WriteCAnsiString(const Value: AnsiString);
var
  StrSize: Integer;
begin
  StrSize := Length(Value);
  WriteBuffer(Value[1], (StrSize + 1) * SizeOf(Value[1]));
end;

procedure TJclEasyStream.WriteCWideString(const Value: WideString);
var
  StrSize: Integer;
begin
  StrSize := Length(Value);
  WriteBuffer(Value[1], (StrSize + 1) * SizeOf(Value[1]));
end;

procedure TJclEasyStream.WriteShortString(const Value: ShortString);
begin
  WriteBuffer(Value[0], Length(Value) + 1);
end;

procedure TJclEasyStream.WriteSingle(const Value: Single);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteSizedString(const Value: string);
begin
  {$IFDEF SUPPORTS_UNICODE}
  WriteSizedWideString(Value);
  {$ELSE ~SUPPORTS_UNICODE}
  WriteSizedAnsiString(Value);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

procedure TJclEasyStream.WriteSizedAnsiString(const Value: AnsiString);
var
  StrSize: Integer;
begin
  StrSize := Length(Value);
  WriteInteger(StrSize);
  WriteBuffer(Value[1], StrSize * SizeOf(Value[1]));
end;

procedure TJclEasyStream.WriteSizedWideString(const Value: WideString);
var
  StrSize: Integer;
begin
  StrSize := Length(Value);
  WriteInteger(StrSize);
  WriteBuffer(Value[1], StrSize * SizeOf(Value[1]));
end;

//=== { TJclScopedStream } ===================================================

constructor TJclScopedStream.Create(AParentStream: TStream; const AMaxSize: Int64);
begin
  inherited Create;

  FParentStream := AParentStream;
  FStartPos := ParentStream.Position;
  FCurrentPos := 0;
  FMaxSize := AMaxSize;
end;

constructor TJclScopedStream.Create(AParentStream: TStream; const AStartPos, AMaxSize: Int64);
begin
  inherited Create;

  FParentStream := AParentStream;
  FStartPos := AStartPos;
  FCurrentPos := 0;
  FMaxSize := AMaxSize;
end;

function TJclScopedStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (MaxSize >= 0) and ((FCurrentPos + Count) > MaxSize) then
    Count := MaxSize - FCurrentPos;

  if (Count > 0) and Assigned(ParentStream) then
  begin
    Result := ParentStream.Read(Buffer, Count);
    Inc(FCurrentPos, Result);
  end
  else
    Result := 0;
end;

function TJclScopedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      begin
        if (Offset < 0) or ((MaxSize >= 0) and (Offset > MaxSize)) then
          Result := -1            // low and high bound check
        else
          Result := ParentStream.Seek(StartPos + Offset, soBeginning) - StartPos;
      end;
    soCurrent:
      begin
        if Offset = 0 then
          Result := FCurrentPos   // speeding the Position property up
        else if ((FCurrentPos + Offset) < 0) or ((MaxSize >= 0)
          and ((FCurrentPos + Offset) > MaxSize)) then
          Result := -1            // low and high bound check
        else
          Result := ParentStream.Seek(Offset, soCurrent) - StartPos;
      end;
    soEnd:
      begin
        if (MaxSize >= 0) then
        begin
          if (Offset > 0) or (MaxSize < -Offset) then // low and high bound check
            Result := -1
          else
            Result := ParentStream.Seek(StartPos + MaxSize + Offset, soBeginning) - StartPos;
        end
        else
        begin
          Result := ParentStream.Seek(Offset, soEnd);
          if (Result <> -1) and (Result < StartPos) then // low bound check
          begin
            Result := -1;
            ParentStream.Seek(StartPos + FCurrentPos, soBeginning);
          end;
        end;
      end;
    else
      Result := -1;
  end;
  if Result <> -1 then
    FCurrentPos := Result;
end;

procedure TJclScopedStream.SetSize(const NewSize: Int64);
var
  ScopedNewSize: Int64;
begin
  if (FMaxSize >= 0) and (NewSize >= (FStartPos + FMaxSize)) then
    ScopedNewSize := FMaxSize + FStartPos
  else
    ScopedNewSize := NewSize;
  inherited SetSize(ScopedNewSize);
end;

function TJclScopedStream.Write(const Buffer; Count: Longint): Longint;
begin
  if (MaxSize >= 0) and ((FCurrentPos + Count) > MaxSize) then
    Count := MaxSize - FCurrentPos;

  if (Count > 0) and Assigned(ParentStream) then
  begin
    Result := ParentStream.Write(Buffer, Count);
    Inc(FCurrentPos, Result);
  end
  else
    Result := 0;
end;

//=== { TJclDelegateStream } =================================================

procedure TJclDelegatedStream.SetSize(const NewSize: Int64);
begin
  if Assigned(FOnSize) then
    FOnSize(Self, NewSize);
end;

function TJclDelegatedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Assigned(FOnSeek) then
    Result := FOnSeek(Self, Offset, Origin)
  else
    Result := -1;
end;

function TJclDelegatedStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(FOnRead) then
    Result := FOnRead(Self, Buffer, Count)
  else
    Result := -1;
end;

function TJclDelegatedStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FOnWrite) then
    Result := FOnWrite(Self, Buffer, Count)
  else
    Result := -1;
end;

//=== { TJclSectoredStream } =================================================

procedure TJclSectoredStream.AfterBlockRead;
begin
  // override to customize (checks of protection)
end;

procedure TJclSectoredStream.BeforeBlockWrite;
begin
  // override to customize (computation of protection)
end;

constructor TJclSectoredStream.Create(AStorageStream: TStream;
  AOwnsStream: Boolean; ASectorOverHead: Integer);
begin
  inherited Create(AStorageStream, AOwnsStream);
  FSectorOverHead := ASectorOverHead;
  if Stream <> nil then
    FPosition := SectoredToFlat(Stream.Position);
end;

procedure TJclSectoredStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  if Stream <> nil then
    FPosition := SectoredToFlat(Stream.Position);
end;

function TJclSectoredStream.FlatToSectored(const Position: Int64): Int64;
begin
  Result := (Position div BufferSize) * (Int64(BufferSize) + FSectorOverHead) // add overheads of previous buffers
    + (Position mod BufferSize); // offset in sector
end;

procedure TJclSectoredStream.Flush;
begin
  if (Stream <> nil) and (FBufferMaxModifiedPos > 0) then
  begin
    BeforeBlockWrite;

    Stream.Position := FlatToSectored(FBufferStart);
    Stream.WriteBuffer(FBuffer[0], FBufferCurrentSize + FSectorOverHead);
    FBufferMaxModifiedPos := 0;
  end;
end;

function TJclSectoredStream.GetCalcedSize: Int64;
var
  VirtualSize: Int64;
begin
  if Assigned(Stream) then
    Result := SectoredToFlat(Stream.Size)
  else
    Result := 0;
  VirtualSize := FBufferMaxModifiedPos + FBufferStart;
  if Result < VirtualSize then
    Result := VirtualSize;
end;

function TJclSectoredStream.LoadBuffer: Boolean;
var
  TotalSectorSize: Longint;
begin
  Flush;
  TotalSectorSize := FBufferSize + FSectorOverHead;
  if Length(FBuffer) <> TotalSectorSize then
    SetLength(FBuffer, TotalSectorSize);
  FBufferStart := (FPosition div BufferSize) * BufferSize;
  if Stream <> nil then
  begin
    Stream.Position := FlatToSectored(FBufferStart);
    FBufferCurrentSize := Stream.Read(FBuffer[0], TotalSectorSize);
    if FBufferCurrentSize > 0 then
    begin
      Dec(FBufferCurrentSize, FSectorOverHead);
      AfterBlockRead;
    end;
  end
  else
    FBufferCurrentSize := 0;
  Result := (FBufferCurrentSize > 0);
end;

function TJclSectoredStream.SectoredToFlat(const Position: Int64): Int64;
var
  TotalSectorSize: Int64;
begin
  TotalSectorSize := Int64(BufferSize) + FSectorOverHead;
  Result := (Position div TotalSectorSize) * BufferSize // remove previous overheads
    + Position mod TotalSectorSize; // offset in sector
end;

procedure TJclSectoredStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(FlatToSectored(NewSize));
end;

//=== { TJclCRC16Stream } ====================================================

procedure TJclCRC16Stream.AfterBlockRead;
var
  CRC: Word;
begin
  CRC := Word(FBuffer[FBufferCurrentSize]) or (Word(FBuffer[FBufferCurrentSize + 1]) shl 8);
  if CheckCrc16(FBuffer, FBufferCurrentSize, CRC) < 0 then
    raise EJclStreamError.CreateRes(@RsStreamsCRCError);
end;

procedure TJclCRC16Stream.BeforeBlockWrite;
var
  CRC: Word;
begin
  CRC := Crc16(FBuffer, FBufferCurrentSize);
  FBuffer[FBufferCurrentSize] := CRC and $FF;
  FBuffer[FBufferCurrentSize + 1] := CRC shr 8;
end;

constructor TJclCRC16Stream.Create(AStorageStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStorageStream, AOwnsStream, 2);
end;

//=== { TJclCRC32Stream } ====================================================

procedure TJclCRC32Stream.AfterBlockRead;
var
  CRC: Cardinal;
begin
  CRC := Cardinal(FBuffer[FBufferCurrentSize]) or (Cardinal(FBuffer[FBufferCurrentSize + 1]) shl 8)
    or (Cardinal(FBuffer[FBufferCurrentSize + 2]) shl 16) or (Cardinal(FBuffer[FBufferCurrentSize + 3]) shl 24);
  if CheckCrc32(FBuffer, FBufferCurrentSize, CRC) < 0 then
    raise EJclStreamError.CreateRes(@RsStreamsCRCError);
end;

procedure TJclCRC32Stream.BeforeBlockWrite;
var
  CRC: Cardinal;
begin
  CRC := Crc32(FBuffer, FBufferCurrentSize);
  FBuffer[FBufferCurrentSize] := CRC and $FF;
  FBuffer[FBufferCurrentSize + 1] := (CRC shr 8) and $FF;
  FBuffer[FBufferCurrentSize + 2] := (CRC shr 16) and $FF;
  FBuffer[FBufferCurrentSize + 3] := (CRC shr 24) and $FF;
end;

constructor TJclCRC32Stream.Create(AStorageStream: TStream;
  AOwnsStream: Boolean);
begin
  inherited Create(AStorageStream, AOwnsStream, 4);
end;

//=== { TJclSplitStream } ====================================================

constructor TJclSplitStream.Create(AForcePosition: Boolean);
begin
  inherited Create;
  FVolume := nil;
  FVolumeIndex := -1;
  FVolumeMaxSize := 0;
  FPosition := 0;
  FVolumePosition := 0;
  FForcePosition := AForcePosition;
end;

function TJclSplitStream.GetSize: Int64;
var
  OldVolumeIndex: Integer;
  OldVolumePosition, OldPosition: Int64;
begin
  OldVolumeIndex := FVolumeIndex;
  OldVolumePosition := FVolumePosition;
  OldPosition := FPosition;

  Result := 0;
  try
    FVolumeIndex := -1;
    repeat
      if not InternalLoadVolume(FVolumeIndex + 1) then
        Break;
      Result := Result + FVolume.Size;
    until FVolume.Size = 0;
  finally
    InternalLoadVolume(OldVolumeIndex);
    FPosition := OldPosition;
    if Assigned(FVolume) then
      FVolumePosition := FVolume.Seek(OldVolumePosition, soBeginning);
  end;
end;

function TJclSplitStream.InternalLoadVolume(Index: Integer): Boolean;
var
  OldVolumeIndex: Integer;
  OldVolumeMaxSize: Int64;
  OldVolumePosition: Int64;
  OldVolume: TStream;
begin
  if Index = -1 then
    Index := 0;
  if Index <> FVolumeIndex then
  begin
    // save current pointers
    OldVolumeIndex := FVolumeIndex;
    OldVolumeMaxSize := FVolumeMaxSize;
    OldVolumePosition := FVolumePosition;
    OldVolume := FVolume;

    FVolumeIndex := Index;
    FVolumePosition := 0;
    FVolume := GetVolume(Index);
    FVolumeMaxSize := GetVolumeMaxSize(Index);
    Result := Assigned(FVolume);
    if Result then
      FVolume.Seek(0, soBeginning)
    else
    begin
      // restore old pointers if volume load failed
      FVolumeIndex := OldVolumeIndex;
      FVolumeMaxSize := OldVolumeMaxSize;
      FVolumePosition := OldVolumePosition;
      FVolume := OldVolume;
    end;
  end
  else
    Result := Assigned(FVolume);
end;

function TJclSplitStream.Read(var Buffer; Count: Longint): Longint;
var
  Data: PByte;
  Total, LoopRead: Longint;
begin
  Result := 0;

  if not InternalLoadVolume(FVolumeIndex) then
    Exit;

  Data := PByte(@Buffer);
  Total := Count;

  repeat
    // force position
    if ForcePosition then
      FVolume.Seek(FVolumePosition, soBeginning);

    // try to read (Count) bytes from current stream
    LoopRead := FVolume.Read(Data^, Count);
    FVolumePosition := FVolumePosition + LoopRead;
    FPosition := FPosition + LoopRead;
    Inc(Result, LoopRead);
    if Result = Total then
      Break;

    // with next volume
    Dec(Count, Result);
    Inc(Data, Result);
    if not InternalLoadVolume(FVolumeIndex + 1) then
      Break;
  until False;
end;

function TJclSplitStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  ExpectedPosition, RemainingOffset: Int64;
begin
  case TSeekOrigin(Origin) of
    soBeginning:
      ExpectedPosition := Offset;
    soCurrent:
      ExpectedPosition := FPosition + Offset;
    soEnd:
      ExpectedPosition := Size + Offset;
  else
    raise EJclStreamError.CreateRes(@RsStreamsSeekError);
  end;
  RemainingOffset := ExpectedPosition - FPosition;
  Result := FPosition;
  repeat
    if not InternalLoadVolume(FVolumeIndex) then
      Break;

    if RemainingOffset < 0 then
    begin
      // FPosition > ExpectedPosition, seek backward
      if FVolumePosition >= -RemainingOffset then
      begin
        // seek in current volume
        FVolumePosition := FVolume.Seek(FVolumePosition + RemainingOffset, soBeginning);
        Result := Result + RemainingOffset;
        FPosition := Result;
        RemainingOffset := 0;
      end
      else
      begin
        // seek to previous volume
        if FVolumeIndex = 0 then
          Exit;
        // seek to the beginning of current volume
        RemainingOffset := RemainingOffset + FVolumePosition;
        Result := Result - FVolumePosition;
        FPosition := Result;
        FVolumePosition := FVolume.Seek(0, soBeginning);
        // load previous volume
        if not InternalLoadVolume(FVolumeIndex - 1) then
          Break;
        Result := Result - FVolume.Size;
        FPosition := Result;
        RemainingOffset := RemainingOffset + FVolume.Size;
      end;
    end
    else if RemainingOffset > 0 then
    begin
      // FPosition < ExpectedPosition, seek forward
      if (FVolumeMaxSize = 0) or ((FVolumePosition + RemainingOffset) < FVolumeMaxSize) then
      begin
        // can seek in current volume
        FVolumePosition := FVolume.Seek(FVolumePosition + RemainingOffset, soBeginning);
        Result := Result + RemainingOffset;
        FPosition := Result;
        RemainingOffset := 0;
      end
      else
      begin
        // seek to next volume
        RemainingOffset := RemainingOffset - FVolumeMaxSize + FVolumePosition;
        Result := Result + FVolumeMaxSize - FVolumePosition;
        FPosition := Result;
        if not InternalLoadVolume(FVolumeIndex + 1) then
          Break;
      end;
    end;
  until RemainingOffset = 0;
end;

procedure TJclSplitStream.SetSize(const NewSize: Int64);
var
  OldVolumeIndex: Integer;
  OldVolumePosition, OldPosition, RemainingSize, VolumeSize: Int64;
begin
  OldVolumeIndex := FVolumeIndex;
  OldVolumePosition := FVolumePosition;
  OldPosition := FPosition;

  RemainingSize := NewSize;
  try
    FVolumeIndex := 0;
    repeat
      if not InternalLoadVolume(FVolumeIndex) then
        Break;
      if (FVolumeMaxSize > 0) and (RemainingSize > FVolumeMaxSize) then
        VolumeSize := FVolumeMaxSize
      else
        VolumeSize := RemainingSize;
      FVolume.Size := VolumeSize;
      RemainingSize := RemainingSize - VolumeSize;

      Inc(FVolumeIndex);
    until RemainingSize = 0;
  finally
    InternalLoadVolume(OldVolumeIndex);
    FPosition := OldPosition;
    if Assigned(FVolume) then
      FVolumePosition := FVolume.Seek(OldVolumePosition, soBeginning);
  end;
end;

function TJclSplitStream.Write(const Buffer; Count: Longint): Longint;
var
  Data: PByte;
  Total, LoopWritten: Longint;
begin
  Result := 0;

  if not InternalLoadVolume(FVolumeIndex) then
    Exit;

  Data := PByte(@Buffer);
  Total := Count;

  repeat
    // force position
    if ForcePosition then
      FVolume.Seek(FVolumePosition, soBeginning);

    // do not write more than (VolumeMaxSize) bytes in current stream
    if (FVolumeMaxSize > 0) and ((Count + FVolumePosition) > FVolumeMaxSize) then
      LoopWritten := FVolumeMaxSize - FVolumePosition
    else
      LoopWritten := Count;
    // try to write (Count) bytes from current stream
    LoopWritten := FVolume.Write(Data^, LoopWritten);
    FVolumePosition := FVolumePosition + LoopWritten;
    FPosition := FPosition + LoopWritten;
    Inc(Result, LoopWritten);
    if Result = Total then
      Break;

    // with next volume
    Dec(Count, LoopWritten);
    Inc(Data, LoopWritten);
    if not InternalLoadVolume(FVolumeIndex + 1) then
      Break;
  until False;
end;

//=== { TJclDynamicSplitStream } =============================================

function TJclDynamicSplitStream.GetVolume(Index: Integer): TStream;
begin
  if Assigned(FOnVolume) then
    Result := FOnVolume(Index)
  else
    Result := nil;
end;

function TJclDynamicSplitStream.GetVolumeMaxSize(Index: Integer): Int64;
begin
  if Assigned(FOnVolumeMaxSize) then
    Result := FOnVolumeMaxSize(Index)
  else
    Result := 0;
end;

//=== { TJclStaticSplitStream } ===========================================

constructor TJclStaticSplitStream.Create(AForcePosition: Boolean);
begin
  inherited Create(AForcePosition);
  FVolumes := TObjectList.Create(True);
end;

destructor TJclStaticSplitStream.Destroy;
var
  Index: Integer;
  AVolumeRec: TJclSplitVolume;
begin
  if Assigned(FVolumes) then
  begin
    for Index := 0 to FVolumes.Count - 1 do
    begin
      AVolumeRec := TJclSplitVolume(FVolumes.Items[Index]);
      if AVolumeRec.OwnStream then
        AVolumeRec.Stream.Free;
    end;
    FVolumes.Free;
  end;
  inherited Destroy;
end;

function TJclStaticSplitStream.AddVolume(AStream: TStream; AMaxSize: Int64;
  AOwnStream: Boolean): Integer;
var
  AVolumeRec: TJclSplitVolume;
begin
  AVolumeRec := TJclSplitVolume.Create;
  AVolumeRec.MaxSize := AMaxSize;
  AVolumeRec.Stream := AStream;
  AVolumeRec.OwnStream := AOwnStream;
  Result := FVolumes.Add(AVolumeRec);
end;

function TJclStaticSplitStream.GetVolume(Index: Integer): TStream;
begin
  Result := TJclSplitVolume(FVolumes.Items[Index]).Stream;
end;

function TJclStaticSplitStream.GetVolumeCount: Integer;
begin
  Result := FVolumes.Count;
end;

function TJclStaticSplitStream.GetVolumeMaxSize(Index: Integer): Int64;
begin
  Result := TJclSplitVolume(FVolumes.Items[Index]).MaxSize;
end;

//=== { TJclStringStream } ====================================================

constructor TJclStringStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnStream := AOwnsStream;
  FBufferSize := StreamDefaultBufferSize;
end;

destructor TJclStringStream.Destroy;
begin
  Flush;
  if FOwnStream then
    FStream.Free;
  inherited;
end;

procedure TJclStringStream.Flush;
begin
  if FStrBufferModifiedSize > 0 then
  begin
    FStream.Position := FStrBufferStart;
    InternalSetNextBuffer(FStream, FStrBuffer, 0, FStrBufferModifiedSize);
    FStrBufferNext := FStream.Seek(0, soCurrent);
    FStrBufferModifiedSize := 0;
  end;
end;

function TJclStringStream.InternalGetNextBuffer(S: TStream;
  var Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
var
  Ch: UCS4;
begin
  // override to optimize
  Result := 0;
  while Count > 0 do
  begin
    if InternalGetNextChar(S, Ch) then
    begin
      Buffer[Start] := Ch;
      Inc(Start);
      Inc(Result);
    end
    else
      Break;
    Dec(Count);
  end;
end;

function TJclStringStream.InternalSetNextBuffer(S: TStream;
  const Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  // override to optimize
  Result := 0;
  while Count > 0 do
  begin
    if InternalSetNextChar(S, Buffer[Start]) then
    begin
      Inc(Start);
      Inc(Result);
    end
    else
      Break;
    Dec(Count);
  end;
end;

procedure TJclStringStream.InvalidateBuffers;
begin
  FStrBufferStart := FStream.Seek(0, soCurrent);
  FStrBufferNext := FStrBufferStart;
  FStrBufferPosition := 0;
  FStrBufferCurrentSize := 0;
  FStrBufferModifiedSize := 0;
  FStrPeekBufferStart := FStrBufferStart;
  FStrPeekBufferNext := FStrBufferNext;
  FStrPeekPosition := 0;
  FStrPeekBufferCurrentSize := 0;
end;

function TJclStringStream.LoadBuffer: Boolean;
begin
  Flush;
  // first test if the peek buffer contains the value
  if (FStrBufferNext >= FStrPeekBufferStart) and (FStrBufferNext < FStrPeekBufferNext) then
  begin
    // the requested buffer is already loaded in the peek buffer
    FStrBufferStart := FStrPeekBufferStart;
    FStrBufferNext := FStrPeekBufferNext;
    if Length(FStrBuffer) <> Length(FStrPeekBuffer) then
      SetLength(FStrBuffer, Length(FStrPeekBuffer));
    FStrBufferPosition := FStrPeekBufferPosition;
    FStrBufferCurrentSize := FStrPeekBufferCurrentSize;
    Move(FStrPeekBuffer[0], FStrBuffer[0], FStrBufferCurrentSize * SizeOf(FStrBuffer[0]));
  end
  else
  begin
    // load a new buffer
    if Length(FStrBuffer) <> FBufferSize then
      SetLength(FStrBuffer, FBufferSize);
    Inc(FStrBufferPosition, FStrBufferCurrentSize);
    FStrBufferStart := FStrBufferNext;
    FStream.Seek(FStrBufferStart, soBeginning);
    FStrBufferCurrentSize := InternalGetNextBuffer(FStream, FStrBuffer, 0, FBufferSize);
    FStrBufferNext := FStream.Seek(0, soCurrent);
    // reset the peek buffer
    FStrPeekBufferPosition := FStrBufferPosition + FStrBufferCurrentSize;
    FStrPeekBufferCurrentSize := 0;
    FStrPeekBufferNext := FStrBufferNext;
    FStrPeekBufferStart := FStrBufferNext;
  end;
  Result := (FStrPosition >= FStrBufferPosition) and (FStrPosition < (FStrBufferPosition + FStrBufferCurrentSize));
end;

function TJclStringStream.LoadPeekBuffer: Boolean;
begin
  if Length(FStrPeekBuffer) <> FBufferSize then
    SetLength(FStrPeekBuffer, FBufferSize);
  if FStrPeekBufferPosition > FStrPeekPosition then
  begin
    // the peek position is rolling back, load the buffer after the read buffer
    FStrPeekBufferPosition := FStrBufferPosition;
    FStrPeekBufferCurrentSize := FStrBufferCurrentSize;
    FStrPeekBufferStart := FStrBufferStart;
    FStrPeekBufferNext := FStrBufferNext;
  end;
  FStrPeekBufferStart := FStrPeekBufferNext;
  Inc(FStrPeekBufferPosition, FStrPeekBufferCurrentSize);
  FStream.Seek(FStrPeekBufferStart, soBeginning);
  FStrPeekBufferCurrentSize := InternalGetNextBuffer(FStream, FStrPeekBuffer, 0, FBufferSize);
  FStrPeekBufferNext := FStream.Seek(0, soCurrent);
  Result := (FStrPeekPosition >= FStrPeekBufferPosition) and (FStrPeekPosition < (FStrPeekBufferPosition + FStrPeekBufferCurrentSize));
end;

function TJclStringStream.PeekAnsiChar(out Buffer: AnsiChar): Boolean;
var
  Ch: UCS4;
begin
  Result := PeekUCS4(Ch);
  if Result then
    Buffer := UCS4ToAnsiChar(Ch);
end;

function TJclStringStream.PeekChar(out Buffer: Char): Boolean;
var
  Ch: UCS4;
begin
  Result := PeekUCS4(Ch);
  if Result then
    Buffer := UCS4ToChar(Ch);
end;

function TJclStringStream.PeekUCS4(out Buffer: UCS4): Boolean;
begin
  if (FStrPeekPosition >= FStrPeekBufferPosition) and (FStrPeekPosition < (FStrPeekBufferPosition + FStrPeekBufferCurrentSize)) then
  begin
    // read from the peek buffer
    Result := True;
    Buffer := FStrPeekBuffer[FStrPeekPosition - FStrPeekBufferPosition];
    Inc(FStrPeekPosition);
  end
  else
  if (FStrPeekPosition >= FStrBufferPosition) and (FStrPeekPosition < (FStrBufferPosition + FStrBufferCurrentSize)) then
  begin
    // read from the read/write buffer
    Result := True;
    Buffer := FStrBuffer[FStrPeekPosition - FStrBufferPosition];
    Inc(FStrPeekPosition);
  end
  else
  begin
    // load a new peek buffer
    Result := LoadPeekBuffer;
    if Result then
    begin
      Buffer := FStrPeekBuffer[FStrPeekPosition - FStrPeekBufferPosition];
      Inc(FStrPeekPosition);
    end;
  end;
end;

function TJclStringStream.PeekWideChar(out Buffer: WideChar): Boolean;
var
  Ch: UCS4;
begin
  Result := PeekUCS4(Ch);
  if Result then
    Buffer := UCS4ToWideChar(Ch);
end;

function TJclStringStream.ReadString(var Buffer: string; Start, Count: Longint): Longint;
var
  Index: Integer;
  StrPos: SizeInt;
  Ch: UCS4;
begin
  Index := Start;
  while Index < Start + Count - 1 do // avoid overflow on surrogate pairs for WideString
  begin
    if ReadUCS4(Ch) then
    begin
      StrPos := Index;
      if StringSetNextChar(Buffer, StrPos, Ch) and (StrPos > 0) then
        Index := StrPos
      else
        Break; // end of string (write)
    end
    else
      Break; // end of stream (read)
  end;
  Result := Index - Start;
end;

function TJclStringStream.ReadString(BufferSize: Longint): string;
var
  Buffer: string;
  ProcessedLength: Longint;
begin
  Result := '';
  SetLength(Buffer, BufferSize);
  repeat
    ProcessedLength := ReadString(Buffer, 1, BufferSize);
    if ProcessedLength > 0 then
      Result := Result + Copy(Buffer, 1, ProcessedLength);
  until ProcessedLength = 0;
end;

function TJclStringStream.ReadAnsiChar(out Buffer: AnsiChar): Boolean;
var
  Ch: UCS4;
begin
  Result := ReadUCS4(Ch);
  if Result then
    Buffer := UCS4ToAnsiChar(Ch);
end;

function TJclStringStream.ReadAnsiString(var Buffer: AnsiString; Start, Count: Longint): Longint;
var
  Index: Integer;
  StrPos: SizeInt;
  Ch: UCS4;
begin
  Index := Start;
  while Index < Start + Count do
  begin
    if ReadUCS4(Ch) then
    begin
      StrPos := Index;
      if AnsiSetNextChar(Buffer, StrPos, Ch) and (StrPos > 0) then
        Index := StrPos
      else
        Break; // end of string (write)
    end
    else
      Break; // end of stream (read)
  end;
  Result := Index - Start;
end;

function TJclStringStream.ReadAnsiString(BufferSize: Longint): AnsiString;
var
  Buffer: AnsiString;
  ProcessedLength: Longint;
begin
  Result := '';
  SetLength(Buffer, BufferSize);
  repeat
    ProcessedLength := ReadAnsiString(Buffer, 1, BufferSize);
    if ProcessedLength > 0 then
      Result := Result + Copy(Buffer, 1, ProcessedLength);
  until ProcessedLength = 0;
end;

function TJclStringStream.ReadChar(out Buffer: Char): Boolean;
var
  Ch: UCS4;
begin
  Result := ReadUCS4(Ch);
  if Result then
    Buffer := UCS4ToChar(Ch);
end;

function TJclStringStream.ReadUCS4(out Buffer: UCS4): Boolean;
begin
  if (FStrPosition >= FStrBufferPosition) and (FStrPosition < (FStrBufferPosition + FStrBufferCurrentSize)) then
  begin
    // load from buffer
    Result := True;
    Buffer := FStrBuffer[FStrPosition - FStrBufferPosition];
    Inc(FStrPosition);
  end
  else
  begin
    // load a new buffer
    Result := LoadBuffer;
    if Result then
    begin
      Buffer := FStrBuffer[FStrPosition - FStrBufferPosition];
      Inc(FStrPosition);
    end;
  end;
  FStrPeekPosition := FStrPosition;
end;

function TJclStringStream.ReadWideChar(out Buffer: WideChar): Boolean;
var
  Ch: UCS4;
begin
  Result := ReadUCS4(Ch);
  if Result then
    Buffer := UCS4ToWideChar(Ch);
end;

function TJclStringStream.ReadWideString(var Buffer: WideString; Start, Count: Longint): Longint;
var
  Index: Integer;
  StrPos: SizeInt;
  Ch: UCS4;
begin
  Index := Start;
  while Index < Start + Count - 1 do // avoid overflow on surrogate pairs
  begin
    if ReadUCS4(Ch) then
    begin
      StrPos := Index;
      if UTF16SetNextChar(Buffer, StrPos, Ch) and (StrPos > 0) then
        Index := StrPos
      else
        Break; // end of string (write)
    end
    else
      Break; // end of stream (read)
  end;
  Result := Index - Start;
end;

function TJclStringStream.ReadWideString(BufferSize: Longint): WideString;
var
  Buffer: WideString;
  ProcessedLength: Longint;
begin
  Result := '';
  SetLength(Buffer, BufferSize);
  repeat
    ProcessedLength := ReadWideString(Buffer, 1, BufferSize);
    if ProcessedLength > 0 then
      Result := Result + Copy(Buffer, 1, ProcessedLength);
  until ProcessedLength = 0;
end;

function TJclStringStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      if Offset = 0 then
      begin
        Flush;
        FStrPosition := 0;
        FStrBufferPosition := 0;
        FStrBufferCurrentSize := 0;
        FStrBufferStart := 0;
        FStrBufferNext := 0;
        FStrPeekBufferPosition := 0;
        FStrPeekBufferCurrentSize := 0;
        FStrPeekBufferStart := 0;
        FStrPeekBufferNext := 0;
      end
      else
        raise EJclStreamError.CreateRes(@RsStreamsSeekError);
    soCurrent:
      if Offset <> 0 then
        raise EJclStreamError.CreateRes(@RsStreamsSeekError);
    soEnd:
      raise EJclStreamError.CreateRes(@RsStreamsSeekError);
  end;
  Result := FStrPosition;
  FStrPeekPosition := FStrPosition;
end;

function TJclStringStream.SkipBOM: Longint;
var
  Pos: Int64;
  I: Integer;
  BOM: array of Byte;
begin
  if Length(FBOM) > 0 then
  begin
    Pos := FStream.Seek(0, soCurrent);
    SetLength(BOM, Length(FBOM));
    Result := FStream.Read(BOM[0], Length(BOM) * SizeOf(BOM[0]));
    if Result = Length(FBOM) * SizeOf(FBOM[0]) then
      for I := Low(FBOM) to High(FBOM) do
        if BOM[I - Low(FBOM)] <> FBOM[I] then
          Result := 0;
    if Result <> Length(FBOM) * SizeOf(FBOM[0]) then
      FStream.Seek(Pos, soBeginning);
  end
  else
    Result := 0;
  InvalidateBuffers;
end;

function TJclStringStream.WriteBOM: Longint;
begin
  if Length(FBOM) > 0 then
    Result := FStream.Write(FBOM[0], Length(FBOM) * SizeOf(FBOM[0]))
  else
    Result := 0;
  InvalidateBuffers;
end;

function TJclStringStream.WriteChar(Value: Char): Boolean;
begin
  Result := WriteUCS4(CharToUCS4(Value));
end;

function TJclStringStream.WriteString(const Buffer: string; Start, Count: Longint): Longint;
var
  Index: Integer;
  StrPos: SizeInt;
  Ch: UCS4;
begin
  Index := Start;
  while Index < Start + Count do
  begin
    StrPos := Index;
    Ch := StringGetNextChar(Buffer, StrPos);
    if (StrPos > 0) and WriteUCS4(Ch) then
      Index := StrPos
    else
      Break; // end of string (read) or end of stream (write)
  end;
  Result := Index - Start;
end;

function TJclStringStream.WriteAnsiChar(Value: AnsiChar): Boolean;
begin
  Result := WriteUCS4(AnsiCharToUCS4(Value));
end;

function TJclStringStream.WriteAnsiString(const Buffer: AnsiString; Start, Count: Longint): Longint;
var
  Index: Integer;
  StrPos: SizeInt;
  Ch: UCS4;
begin
  Index := Start;
  while Index < Start + Count do
  begin
    StrPos := Index;
    Ch := AnsiGetNextChar(Buffer, StrPos);
    if (StrPos > 0) and WriteUCS4(Ch) then
      Index := StrPos
    else
      Break; // end of string (read) or end of stream (write)
  end;
  Result := Index - Start;
end;

function TJclStringStream.WriteUCS4(Value: UCS4): Boolean;
var
  BufferPos: Int64;
begin
  if FStrPosition >= (FStrBufferPosition + FBufferSize) then
    // load the next buffer first
    LoadBuffer;
  // write to current buffer
  BufferPos := FStrPosition - FStrBufferPosition;
  Result := True;
  if Length(FStrBuffer) <> FBufferSize then
    SetLength(FStrBuffer, FBufferSize);
  FStrBuffer[BufferPos] := Value;
  Inc(FStrPosition);
  Inc(BufferPos);
  if FStrBufferModifiedSize < BufferPos then
    FStrBufferModifiedSize := BufferPos;
  if FStrBufferCurrentSize < BufferPos then
    FStrBufferCurrentSize := BufferPos;
  FStrPeekPosition := FStrPosition;
end;

function TJclStringStream.WriteWideChar(Value: WideChar): Boolean;
begin
  Result := WriteUCS4(WideCharToUCS4(Value));
end;

function TJclStringStream.WriteWideString(const Buffer: WideString; Start, Count: Longint): Longint;
var
  Index: Integer;
  StrPos: SizeInt;
  Ch: UCS4;
begin
  Index := Start;
  while Index < Start + Count do
  begin
    StrPos := Index;
    Ch := UTF16GetNextChar(Buffer, StrPos);
    if (StrPos > 0) and WriteUCS4(Ch) then
      Index := StrPos
    else
      Break; // end of string (read) or end of stream (write)
  end;
  Result := Index - Start;
end;

//=== { TJclAnsiStream } ======================================================

constructor TJclAnsiStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  SetLength(FBOM, 0);
  FCodePage := CP_ACP;
end;

function TJclAnsiStream.InternalGetNextBuffer(S: TStream;
  var Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  if FCodePage = CP_ACP then
    Result := AnsiGetNextBufferFromStream(S, Buffer, Start, Count)
  else
    Result := AnsiGetNextBufferFromStream(S, FCodePage, Buffer, Start, Count);
end;

function TJclAnsiStream.InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean;
begin
  if FCodePage = CP_ACP then
    Result := AnsiGetNextCharFromStream(S, Ch)
  else
    Result := AnsiGetNextCharFromStream(S, FCodePage, Ch);
end;

function TJclAnsiStream.InternalSetNextBuffer(S: TStream;
  const Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  if FCodePage = CP_ACP then
    Result := AnsiSetNextBufferToStream(S, Buffer, Start, Count)
  else
    Result := AnsiSetNextBufferToStream(S, FCodePage, Buffer, Start, Count);
end;

function TJclAnsiStream.InternalSetNextChar(S: TStream; Ch: UCS4): Boolean;
begin
  if FCodePage = CP_ACP then
    Result := AnsiSetNextCharToStream(S, Ch)
  else
    Result := AnsiSetNextCharToStream(S, FCodePage, Ch);
end;

//=== { TJclUTF8Stream } ======================================================

constructor TJclUTF8Stream.Create(AStream: TStream; AOwnsStream: Boolean);
var
  I: Integer;
begin
  inherited Create(AStream, AOwnsStream);
  SetLength(FBOM, Length(BOM_UTF8));
  for I := Low(BOM_UTF8) to High(BOM_UTF8) do
    FBOM[I - Low(BOM_UTF8)] := BOM_UTF8[I];
end;

function TJclUTF8Stream.InternalGetNextBuffer(S: TStream;
  var Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  Result := UTF8GetNextBufferFromStream(S, Buffer, Start, Count);
end;

function TJclUTF8Stream.InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean;
begin
  Result := UTF8GetNextCharFromStream(S, Ch);
end;

function TJclUTF8Stream.InternalSetNextBuffer(S: TStream;
  const Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  Result := UTF8SetNextBufferToStream(S, Buffer, Start, Count);
end;

function TJclUTF8Stream.InternalSetNextChar(S: TStream; Ch: UCS4): Boolean;
begin
  Result := UTF8SetNextCharToStream(S, Ch);
end;

//=== { TJclUTF16Stream } =====================================================

constructor TJclUTF16Stream.Create(AStream: TStream; AOwnsStream: Boolean);
var
  I: Integer;
begin
  inherited Create(AStream, AOwnsStream);
  SetLength(FBOM, Length(BOM_UTF16_LSB));
  for I := Low(BOM_UTF16_LSB) to High(BOM_UTF16_LSB) do
    FBOM[I - Low(BOM_UTF16_LSB)] := BOM_UTF16_LSB[I];
end;

function TJclUTF16Stream.InternalGetNextBuffer(S: TStream;
  var Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  Result := UTF16GetNextBufferFromStream(S, Buffer, Start, Count);
end;

function TJclUTF16Stream.InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean;
begin
  Result := UTF16GetNextCharFromStream(S, Ch);
end;

function TJclUTF16Stream.InternalSetNextBuffer(S: TStream;
  const Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  Result := UTF16SetNextBufferToStream(S, Buffer, Start, Count);
end;

function TJclUTF16Stream.InternalSetNextChar(S: TStream; Ch: UCS4): Boolean;
begin
  Result := UTF16SetNextCharToStream(S, Ch);
end;

//=== { TJclAutoStream } ======================================================

constructor TJclAutoStream.Create(AStream: TStream; AOwnsStream: Boolean);
var
  I, MaxLength, ReadLength: Integer;
  BOM: array of Byte;
begin
  inherited Create(AStream, AOwnsStream);
  MaxLength := Length(BOM_UTF8);
  if MaxLength < Length(BOM_UTF16_LSB) then
    MaxLength := Length(BOM_UTF16_LSB);

  SetLength(BOM, MaxLength);
  ReadLength := FStream.Read(BOM[0], Length(BOM) * SizeOf(BOM[0])) div SizeOf(BOM[0]);

  FEncoding := seAuto;

  // try UTF8 BOM
  if (FEncoding = seAuto) and (ReadLength >= Length(BOM_UTF8) * SizeOf(BOM_UTF8[0])) then
  begin
    FCodePage := CP_UTF8;
    FEncoding := seUTF8;
    for I := Low(BOM_UTF8) to High(BOM_UTF8) do
      if BOM[I - Low(BOM_UTF8)] <> BOM_UTF8[I] then
    begin
      FEncoding := seAuto;
      Break;
    end;
  end;

  // try UTF16 BOM
  if (FEncoding = seAuto) and (ReadLength >= Length(BOM_UTF16_LSB) * SizeOf(BOM_UTF16_LSB[0])) then
  begin
    FCodePage := CP_UTF16LE;
    FEncoding := seUTF16;
    for I := Low(BOM_UTF16_LSB) to High(BOM_UTF16_LSB) do
      if BOM[I - Low(BOM_UTF8)] <> BOM_UTF16_LSB[I] then
    begin
      FEncoding := seAuto;
      Break;
    end;
  end;

  case FEncoding of
    seUTF8:
      begin
        FCodePage := CP_UTF8;
        SetLength(FBOM, Length(BOM_UTF8));
        for I := Low(BOM_UTF8) to High(BOM_UTF8) do
          FBOM[I - Low(BOM_UTF8)] := BOM_UTF8[I];
      end;
    seUTF16:
      begin
        FCodePage := CP_UTF16LE;
        SetLength(FBOM, Length(BOM_UTF16_LSB));
        for I := Low(BOM_UTF16_LSB) to High(BOM_UTF16_LSB) do
          FBOM[I - Low(BOM_UTF16_LSB)] := BOM_UTF16_LSB[I];
      end;
    seAuto,
    seAnsi:
      begin
        // defaults to Ansi
        FCodePage := CP_ACP;
        FEncoding := seAnsi;
        SetLength(FBOM, 0);
      end;
  end;
  FStream.Seek(Length(FBOM) - ReadLength, soCurrent);
  InvalidateBuffers;
end;

function TJclAutoStream.InternalGetNextBuffer(S: TStream;
  var Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  case FCodePage of
    CP_UTF8:
      Result := UTF8GetNextBufferFromStream(S, Buffer, Start, Count);
    CP_UTF16LE:
      Result := UTF16GetNextBufferFromStream(S, Buffer, Start, Count);
    CP_ACP:
      Result := AnsiGetNextBufferFromStream(S, Buffer, Start, Count);
  else
    Result := AnsiGetNextBufferFromStream(S, CodePage, Buffer, Start, Count);
  end;
end;

function TJclAutoStream.InternalGetNextChar(S: TStream; out Ch: UCS4): Boolean;
begin
  case FCodePage of
    CP_UTF8:
      Result := UTF8GetNextCharFromStream(S, Ch);
    CP_UTF16LE:
      Result := UTF16GetNextCharFromStream(S, Ch);
    CP_ACP:
      Result := AnsiGetNextCharFromStream(S, Ch);
  else
    Result := AnsiGetNextCharFromStream(S, CodePage, Ch);
  end;
end;

function TJclAutoStream.InternalSetNextBuffer(S: TStream;
  const Buffer: TUCS4Array; Start, Count: SizeInt): Longint;
begin
  case FCodePage of
    CP_UTF8:
      Result := UTF8SetNextBufferToStream(S, Buffer, Start, Count);
    CP_UTF16LE:
      Result := UTF16SetNextBufferToStream(S, Buffer, Start, Count);
    CP_ACP:
      Result := AnsiSetNextBufferToStream(S, Buffer, Start, Count);
  else
    Result := AnsiSetNextBufferToStream(S, CodePage, Buffer, Start, Count);
  end;
end;

function TJclAutoStream.InternalSetNextChar(S: TStream; Ch: UCS4): Boolean;
begin
  case FCodePage of
    CP_UTF8:
      Result := UTF8SetNextCharToStream(S, Ch);
    CP_UTF16LE:
      Result := UTF16SetNextCharToStream(S, Ch);
    CP_ACP:
      Result := AnsiSetNextCharToStream(S, Ch);
  else
    Result := AnsiSetNextCharToStream(S, CodePage, Ch);
  end;
end;

procedure TJclAutoStream.SetCodePage(Value: Word);
begin
  if Value = CP_UTF8 then
    FEncoding := seUTF8
  else
  if Value = CP_UTF16LE then
    FEncoding := seUTF16
  else
  if Value = CP_ACP then
    FEncoding := seAnsi
  else
    FEncoding := seAuto;
  FCodePage := Value;
end;

function TJclAutoStream.SkipBOM: LongInt;
begin
  // already skipped to determine encoding
  Result := 0;
  InvalidateBuffers;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

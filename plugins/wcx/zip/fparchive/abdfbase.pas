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
{* ABBREVIA: AbDfBase.pas 3.05                           *}
{*********************************************************}
{* Deflate base unit                                     *}
{*********************************************************}

unit AbDfBase;

{$I AbDefine.inc}

interface

uses
  SysUtils,
  Classes;

type
  PAbDfLongintList = ^TAbDfLongintList;
  TAbDfLongintList =
               array [0..pred(MaxInt div sizeof(longint))] of longint;

const
  dfc_CodeLenCodeLength = 7;
  dfc_LitDistCodeLength = 15;
  dfc_MaxCodeLength     = 15;

const
  dfc_MaxMatchLen = 258;         {lengths are 3..258 for deflate}
  dfc_MaxMatchLen64 = 64 * 1024; {lengths are 3..65536  for deflate64}

const
  dfc_LitExtraOffset = 257;
  dfc_LitExtraBits : array [0..30] of byte =
    (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
     4, 4, 4, 4, 5, 5, 5, 5, 16, 99, 99);
     { note: the last two are required to avoid going beyond the end}
     {       of the array when generating static trees}

  dfc_DistExtraOffset = 0;
  dfc_DistExtraBits : array [0..31] of byte =
    (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9,
     10, 10, 11, 11, 12, 12, 13, 13, 14, 14);
     { note: the last two are only use for deflate64}

  dfc_LengthBase : array [0..28] of word =
    (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43,
     51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 3);
     { note: the final 3 is correct for deflate64; for symbol 285,}
     {       lengths are stored as (length - 3)}
     {       for deflate it's very wrong, but there's special code in}
     {       the (de)compression code to cater for this}

  dfc_DistanceBase : array [0..31] of word =
    (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257,
     385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289,
     16385, 24577, 32769, 49153);

  dfc_CodeLengthIndex : array [0..18] of byte =
    (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

const
  dfc_CanUseStored  = $01;
  dfc_CanUseStatic  = $02;
  dfc_CanUseDynamic = $04;
  dfc_UseLazyMatch  = $08;
  dfc_UseDeflate64  = $10;
  dfc_UseAdler32    = $20;
  dfc_CanUseHuffman = dfc_CanUseStatic or dfc_CanUseDynamic;
  dfc_TestOnly      = $40000000;

type
  TAbProgressStep = procedure (aPercentDone : integer) of object;
    {-progress metering of deflate/inflate; abort with AbortProgress}

  TAbDeflateHelper = class
    private
      FAmpleLength    : longint;
      FChainLength    : longint;
      FCheckValue     : longint;
      FLogFile        : string;
      FMaxLazy        : longint;
      FOnProgressStep : TAbProgressStep;
      FOptions        : longint;
      FPartSize       : Int64;
      FPassphrase     : string;
      FSizeCompressed : Int64;
      FSizeNormal     : Int64;
      FStreamSize     : Int64;
      FWindowSize     : longint;
      FZipOption      : AnsiChar;
    protected
      procedure dhSetAmpleLength(aValue : longint);
      procedure dhSetChainLength(aValue : longint);
      procedure dhSetCheckValue(aValue : longint);
      procedure dhSetLogFile(const aValue : string);
      procedure dhSetMaxLazy(aValue : longint);
      procedure dhSetOnProgressStep(aValue : TAbProgressStep);
      procedure dhSetOptions(aValue : longint);
      procedure dhSetPassphrase(const aValue : string);
      procedure dhSetWindowSize(aValue : longint);
      procedure dhSetZipOption(aValue : AnsiChar);
    public
      constructor Create;

      procedure Assign(aHelper : TAbDeflateHelper);

      property AmpleLength : longint
                  read FAmpleLength write dhSetAmpleLength;
      property ChainLength : longint
                  read FChainLength write dhSetChainLength;
      property CheckValue : longint
                  read FCheckValue write dhSetCheckValue;
      property LogFile : string
                  read FLogFile write dhSetLogFile;
      property MaxLazyLength : longint
                  read FMaxLazy write dhSetMaxLazy;
      property Options : longint
                  read FOptions write dhSetOptions;
      property PartialSize : Int64
                  read FPartSize write FPartSize;
      property Passphrase : string
                  read FPassphrase write dhSetPassphrase;
      property PKZipOption : AnsiChar
                  read FZipOption write dhSetZipOption;
      property StreamSize : Int64
                  read FStreamSize write FStreamSize;
      property WindowSize : longint
                  read FWindowSize write dhSetWindowSize;

      property CompressedSize : Int64
                  read FSizeCompressed write FSizeCompressed;
      property NormalSize : Int64
                  read FSizeNormal write FSizeNormal;

      property OnProgressStep : TAbProgressStep
                  read FOnProgressStep write dhSetOnProgressStep;
  end;

type
  TAbLineDelimiter = (ldCRLF, ldLF);

  TAbLogger = class(TStream)
    private
      FBuffer    : PAnsiChar;
      FCurPos    : PAnsiChar;
      FLineDelim : TAbLineDelimiter;
      FStream    : TFileStream;
    protected
      function logWriteBuffer : boolean;
    public
      constructor Create(const aLogName : string);
      destructor Destroy; override;

      function Read(var Buffer; Count : longint) : longint; override;
      function Seek(const Offset : Int64; Origin : TSeekOrigin) : Int64; override;
      function Write(const Buffer; Count : longint) : longint; override;
      procedure WriteLine(const S : string);
      procedure WriteStr(const S : string);

      property LineDelimiter : TAbLineDelimiter
                  read FLineDelim write FLineDelim;
  end;

type
  TAbNodeManager = class
    private
      FFreeList     : pointer;
      FNodeSize     : cardinal;
      FNodesPerPage : cardinal;
      FPageHead     : pointer;
      FPageSize     : cardinal;
    protected
      function nmAllocNewPage : pointer;
    public
      constructor Create(aNodeSize : cardinal);
      destructor Destroy; override;

      function AllocNode : pointer;
      function AllocNodeClear : pointer;
      procedure FreeNode(aNode : pointer);
  end;

{---exception classes---}
type
  EAbAbortProgress = class(Exception);
  EAbPartSizedInflate = class(Exception);
  EAbInflatePasswordError = class(Exception);
  EAbInternalInflateError = class(Exception);
  EAbInflateError = class(Exception)
    public
      constructor Create(const aMsg : string);
      constructor CreateUnknown(const aMsg : string;
                                const aErrorMsg : string);
  end;
  EAbInternalDeflateError = class(Exception);
  EAbDeflateError = class(Exception)
    public
      constructor Create(const aMsg : string);
      constructor CreateUnknown(const aMsg : string;
                                const aErrorMsg : string);
  end;

{---aborting a process---}
procedure AbortProgress;

{---calculation of checksums---}
procedure AbUpdateAdlerBuffer(var aAdler : longint;
                              var aBuffer; aCount : integer);
procedure AbUpdateCRCBuffer(var aCRC : longint;
                            var aBuffer; aCount : integer);


implementation

uses
  AbUtils;

type
  {$IFDEF HasLongWord}
  DblWord = longword;
  {$ELSE}
  DblWord = longint;
  {$ENDIF}

{===TAbDeflateHelper=================================================}
constructor TAbDeflateHelper.Create;
begin
  inherited Create;
  FAmpleLength := 8;
  FChainLength := 32;
  FCheckValue := -1;
  {FLogFile := '';}
  FMaxLazy := 16;
  {FOnProgressStep := nil;}
  FOptions := $F;
  {FPassphrase := '';}
  {FStreamSize := 0;}
  FWindowSize := 32 * 1024;
  FZipOption := 'n';
end;
{--------}
procedure TAbDeflateHelper.Assign(aHelper : TAbDeflateHelper);
begin
  FAmpleLength := aHelper.FAmpleLength;
  FChainLength := aHelper.FChainLength;
  FCheckValue := aHelper.FCheckValue;
  FLogFile := aHelper.FLogFile;
  FMaxLazy := aHelper.FMaxLazy;
  FOnProgressStep := aHelper.FOnProgressStep;
  FOptions := aHelper.FOptions;
  FPartSize := aHelper.FPartSize;
  FPassphrase := aHelper.FPassphrase;
  FStreamSize := aHelper.FStreamSize;
  FWindowSize := aHelper.FWindowSize;
  FZipOption := aHelper.FZipOption;
end;
{--------}
procedure TAbDeflateHelper.dhSetAmpleLength(aValue : longint);
begin
  if (aValue <> AmpleLength) then begin
    if (aValue <> -1) and (aValue < 4) then
      aValue := 4;
    FAmpleLength := aValue;
    FZipOption := '?';
  end;
end;
{--------}
procedure TAbDeflateHelper.dhSetChainLength(aValue : longint);
begin
  if (aValue <> ChainLength) then begin
    if (aValue <> -1) and (aValue < 4) then
      aValue := 4;
    FChainLength := aValue;
    FZipOption := '?';
  end;
end;
{--------}
procedure TAbDeflateHelper.dhSetCheckValue(aValue : longint);
begin
  {Note: the CheckValue is only required during the inflate of an
         encrypted stream. The encryption header contains part of the
         CheckValue encrypted and this is used to check that the
         supplied passphrase was correct.
         The CheckValue is usually the CRC of the uncompressed stream
         and the zip file has this value readily to hand prior to
         decompression. With encryption during deflate the code will
         calculate the CheckValue of the uncompressed stream prior to
         compressing it.}
  FCheckValue := aValue;
end;
{--------}
procedure TAbDeflateHelper.dhSetLogFile(const aValue : string);
begin
  FLogFile := aValue;
end;
{--------}
procedure TAbDeflateHelper.dhSetMaxLazy(aValue : longint);
begin
  if (aValue <> MaxLazyLength) then begin
    if (aValue <> -1) and (aValue < 4) then
      aValue := 4;
    FMaxLazy := aValue;
    FZipOption := '?';
  end;
end;
{--------}
procedure TAbDeflateHelper.dhSetOnProgressStep(aValue : TAbProgressStep);
begin
  FOnProgressStep := aValue;
end;
{--------}
procedure TAbDeflateHelper.dhSetOptions(aValue : longint);
begin
  if (aValue <> Options) then begin
    FOptions := aValue;
    FZipOption := '?';
  end;
end;
{--------}
procedure TAbDeflateHelper.dhSetPassphrase(const aValue : string);
begin
  FPassphrase := aValue;
end;
{--------}
procedure TAbDeflateHelper.dhSetWindowSize(aValue : longint);
var
  NewValue : longint;
begin
  if (aValue <> WindowSize) then begin
    {calculate the window size rounded to nearest 1024 bytes}
    NewValue := ((aValue + 1023) div 1024) * 1024;
    {if the new window size is greater than 32KB...}
    if (NewValue > 32 * 1024) then
      {if the Deflate64 option is set, force to 64KB}
      if ((Options and dfc_UseDeflate64) <> 0) then
        NewValue := 64 * 1024
      {otherwise, force to 32KB}
      else
        NewValue := 32 * 1024;
    {set the new window size}
    FWindowSize := NewValue;
  end;
end;
{--------}
procedure TAbDeflateHelper.dhSetZipOption(aValue : AnsiChar);
begin
  {notes:
     The original Abbrevia code used the following table for
     setting the equivalent values:
            Good  Lazy  Chain  UseLazy  Option
               4     4      4     N       s        ^
               4     5      8     N                |
               4     6     32     N       f      faster
               4     4     16     Y              slower
               8    16     32     Y       n        |
               8    16    128     Y                |
               8    32    256     Y                |
              32   128   1024     Y                |
              32   258   4096     Y       x        V
     The new Abbrevia 3 code follows these values to a certain extent.
  }

  {force to lower case}
  if ('A' <= aValue) and (aValue <= 'Z') then
    aValue := AnsiChar(ord(aValue) + ord('a') - ord('A'));

  {if the value has changed...}
  if (aValue <> PKZipOption) then begin

    {switch on the new value...}
    case aValue of
      '0' : {no compression}
        begin
          FZipOption := aValue;
          FOptions := (FOptions and (not $0F)) or dfc_CanUseStored;
          FAmpleLength := 8;  { not actually needed}
          FChainLength := 32; { not actually needed}
          FMaxLazy := 16;     { not actually needed}
        end;
      '2' : {hidden option: Abbrevia 2 compatibility}
        begin
          FZipOption := aValue;
          FOptions := FOptions or $0F;
          FAmpleLength := 8;
          FChainLength := 32;
          FMaxLazy := 16;
        end;
      'f' : {fast compression}
        begin
          FZipOption := aValue;
          FOptions := FOptions or $07; { no lazy matching}
          FAmpleLength := 4;
          FChainLength := 32;
          FMaxLazy := 6;
        end;
      'n' : {normal compression}
        begin
          FZipOption := aValue;
          FOptions := FOptions or $0F;
          FAmpleLength := 16;
          FChainLength := 32;
          FMaxLazy := 24;
        end;
      's' : {super fast compression}
        begin
          FZipOption := aValue;
          FOptions := FOptions or $07; { no lazy matching}
          FAmpleLength := 4;
          FChainLength := 4;
          FMaxLazy := 4;
        end;
      'x' : {maximum compression}
        begin
          FZipOption := aValue;
          FOptions := FOptions or $0F;
          FAmpleLength := 64;{32;}
          FChainLength := 4096;
          FMaxLazy := 258;
        end;
    end;
  end;
end;
{====================================================================}


{===TAbLogger========================================================}
const
  LogBufferSize = 4096;
{--------}
constructor TAbLogger.Create(const aLogName : string);
begin
  Assert(aLogName <> '',
         'TAbLogger.Create: a filename must be provided for the logger');

  {create the ancestor}
  inherited Create;

  {set the default line terminator}
  {$IFDEF MSWINDOWS}
  FLineDelim := ldCRLF;
  {$ENDIF}
  {$IFDEF Linux}
  FLineDelim := ldLF;
  {$ENDIF}

  {create and initialize the buffer}
  GetMem(FBuffer, LogBufferSize);
  FCurPos := FBuffer;

  {create the log file}
  FStream := TFileStream.Create(aLogName, fmCreate);
end;
{--------}
destructor TAbLogger.Destroy;
begin
  {if there is a buffer ensure that it is flushed before freeing it}
  if (FBuffer <> nil) then begin
    if (FCurPos <> FBuffer) then
      logWriteBuffer;
    FreeMem(FBuffer, LogBufferSize);
  end;

  {free the stream}
  FStream.Free;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
function TAbLogger.logWriteBuffer : boolean;
var
  BytesToWrite : longint;
  BytesWritten : longint;
begin
  BytesToWrite := FCurPos - FBuffer;
  BytesWritten := FStream.Write(FBuffer^, BytesToWrite);
  if (BytesWritten = BytesToWrite) then begin
    Result := true;
    FCurPos := FBuffer;
  end
  else begin
    Result := false;
    if (BytesWritten <> 0) then begin
      Move(FBuffer[BytesWritten], FBuffer^, BytesToWrite - BytesWritten);
      FCurPos := FBuffer + (BytesToWrite - BytesWritten);
    end;
  end;
end;
{--------}
function TAbLogger.Read(var Buffer; Count : longint) : longint;
begin
  Assert(false, 'TAbLogger.Read: loggers are write-only, no reading allowed');
  Result := 0;
end;
{--------}
function TAbLogger.Seek(const Offset : Int64; Origin : TSeekOrigin) : Int64;
begin
  case Origin of
    soBeginning :
      begin
      end;
    soCurrent :
      if (Offset = 0) then begin
        Result := FStream.Position + (FCurPos - FBuffer);
        Exit;
      end;
    soEnd :
      if (Offset = 0) then begin
        Result := FStream.Position + (FCurPos - FBuffer);
        Exit;
      end;
  end;

  Assert(false, 'TAbLogger.Seek: loggers are write-only, no seeking allowed');
  Result := 0;
end;
{--------}
function TAbLogger.Write(const Buffer; Count : longint) : longint;
var
  UserBuf      : PAnsiChar;
  BytesToGo    : longint;
  BytesToWrite : longint;
begin
  {reference the user's buffer as a PChar}
  UserBuf := @Buffer;

  {start the counter for the number of bytes written}
  Result := 0;

  {if needed, empty the internal buffer into the underlying stream}
  if (LogBufferSize = FCurPos - FBuffer) then
    if not logWriteBuffer then
      Exit;

  {calculate the number of bytes to copy this time from the user's
   buffer to the internal buffer}
  BytesToGo := Count;
  BytesToWrite := LogBufferSize - (FCurPos - FBuffer);
  if (BytesToWrite > BytesToGo) then
    BytesToWrite := BytesToGo;

  {copy the bytes}
  Move(UserBuf^, FCurPos^, BytesToWrite);

  {adjust the counters}
  inc(FCurPos, BytesToWrite);
  dec(BytesToGo, BytesToWrite);
  inc(Result, BytesToWrite);

  {while there are still more bytes to copy, do so}
  while (BytesToGo <> 0) do begin
    {advance the user's buffer}
    inc(UserBuf, BytesToWrite);

    {empty the internal buffer into the underlying stream}
    if not logWriteBuffer then
      Exit;

    {calculate the number of bytes to copy this time from the user's
     buffer to the internal buffer}
    BytesToWrite := LogBufferSize;
    if (BytesToWrite > BytesToGo) then
      BytesToWrite := BytesToGo;

    {copy the bytes}
    Move(UserBuf^, FCurPos^, BytesToWrite);

    {adjust the counters}
    inc(FCurPos, BytesToWrite);
    dec(BytesToGo, BytesToWrite);
    inc(Result, BytesToWrite);
  end;
end;
{--------}
procedure TAbLogger.WriteLine(const S : string);
const
  cLF : AnsiChar = ^J;
  cCRLF : array [0..1] of AnsiChar = ^M^J;
begin
  if (length(S) > 0) then
    Write(S[1], length(S));
  case FLineDelim of
    ldLF   : Write(cLF, sizeof(cLF));
    ldCRLF : Write(cCRLF, sizeof(cCRLF));
  end;
end;
{--------}
procedure TAbLogger.WriteStr(const S : string);
begin
  if (length(S) > 0) then
    Write(S[1], length(S));
end;
{====================================================================}


{===Calculate checksums==============================================}
procedure AbUpdateAdlerBuffer(var aAdler : longint;
                              var aBuffer; aCount : integer);
var
  S1 : DblWord;
  S2 : DblWord;
  i  : integer;
  Buffer     : PAnsiChar;
  BytesToUse : integer;
begin
  {Note: this algorithm will *only* work if the buffer is 4KB or less,
         which is why we go to such lengths to chop up the user buffer
         into usable chunks of 4KB.

         However, for Delphi 3 there is no proper 32-bit longword.
         Although the additions pose no problems in this situation,
         the mod operations below (especially for S2) will be signed
         integer divisions, producing an (invalid) signed result. In
         this case, the buffer is chopped up into 2KB chunks to avoid
         any signed problems.}

  {split the current Adler checksum into its halves}
  S1 := DblWord(aAdler) and $FFFF;
  S2 := DblWord(aAdler) shr 16;

  {reference the user buffer as a PChar: it makes it easier}
  Buffer := @aBuffer;

  {while there's still data to checksum...}
  while (aCount <> 0) do begin

    {calculate the number of bytes to checksum this time}
    {$IFDEF HasLongWord}
    BytesToUse := 4096;
    {$ELSE}
    BytesToUse := 2048;
    {$ENDIF}
    if (BytesToUse > aCount) then
      BytesToUse := aCount;

    {checksum the bytes}
    for i := 0 to pred(BytesToUse) do begin
      inc(S1, ord(Buffer^));
      inc(S2, S1);
      inc(Buffer);
    end;

    {recalibrate the Adler checksum halves}
    S1 := S1 mod 65521;
    S2 := S2 mod 65521;

    {calculate the number of bytes still to go}
    dec(aCount, BytesToUse);
  end;

  {join the halves to produce the complete Adler checksum}
  aAdler := longint((S2 shl 16) or S1);
end;
{--------}
procedure AbUpdateCRCBuffer(var aCRC : longint;
                            var aBuffer; aCount : integer);
var
  i      : integer;
  CRC    : DblWord;
  Buffer : PAnsiChar;
begin
{$R-}{$Q-}
  {reference the user buffer as a PChar: it makes it easier}
  Buffer := @aBuffer;

  {get the current CRC as a local variable, it's faster}
  CRC := aCRC;

  {checksum the bytes in the buffer}
  for i := 0 to pred(aCount) do begin
    CRC := AbCrc32Table[byte(CRC) xor byte(Buffer^)] xor (CRC shr 8);
    inc(Buffer);
  end;

  {return the new CRC}
  aCRC := CRC;
{$R+}{$Q+}
end;
{====================================================================}


{===EAbInflateError==================================================}
constructor EAbInflateError.Create(const aMsg : string);
begin
  inherited Create(
     'Abbrevia inflate error, possibly a corrupted compressed stream. ' +
     '(Internal cause: ' + aMsg + ')');
end;
{--------}
constructor EAbInflateError.CreateUnknown(const aMsg : string;
                                          const aErrorMsg : string);
begin
  inherited Create(aMsg + ': ' + aErrorMsg);
end;
{====================================================================}


{===EAbDeflateError==================================================}
constructor EAbDeflateError.Create(const aMsg : string);
begin
  inherited Create(
     'Abbrevia deflate error. ' +
     '(Internal cause: ' + aMsg + ')');
end;
{--------}
constructor EAbDeflateError.CreateUnknown(const aMsg : string;
                                          const aErrorMsg : string);
begin
  inherited Create(aMsg + ': ' + aErrorMsg);
end;
{====================================================================}


{===Node manager=====================================================}
const
  PageSize = 8 * 1024;
type
  PGenericNode = ^TGenericNode;
  TGenericNode = packed record
    gnNext : PGenericNode;
    gnData : record end;
  end;
{--------}
constructor TAbNodeManager.Create(aNodeSize : cardinal);
const
  Gran = sizeof(pointer);
  Mask = not (Gran - 1);
begin
  {create the ancestor}
  inherited Create;

  {save the node size rounded to nearest 4 bytes}
  if (aNodeSize <= sizeof(pointer)) then
    aNodeSize := sizeof(pointer)
  else
    aNodeSize := (aNodeSize + Gran - 1) and Mask;
  FNodeSize := aNodeSize;

  {calculate the page size (default 1024 bytes) and the number of
   nodes per page; if the default page size is not large enough for
   two or more nodes, force a single node per page}
  FNodesPerPage := (PageSize - sizeof(pointer)) div aNodeSize;
  if (FNodesPerPage > 1) then
    FPageSize := PageSize
  else begin
    FNodesPerPage := 1;
    FPagesize := aNodeSize + sizeof(pointer);
  end;
end;
{--------}
destructor TAbNodeManager.Destroy;
var
  Temp : pointer;
begin
  {dispose of all the pages, if there are any}
  while (FPageHead <> nil) do begin
    Temp := PGenericNode(FPageHead)^.gnNext;
    FreeMem(FPageHead, FPageSize);
    FPageHead := Temp;
  end;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
function TAbNodeManager.AllocNode : pointer;
begin
  Result := FFreeList;
  if (Result = nil) then
    Result := nmAllocNewPage
  else
    FFreeList := PGenericNode(Result)^.gnNext;
end;
{--------}
function TAbNodeManager.AllocNodeClear : pointer;
begin
  Result := FFreeList;
  if (Result = nil) then
    Result := nmAllocNewPage
  else
    FFreeList := PGenericNode(Result)^.gnNext;
  FillChar(Result^, FNodeSize, 0);
end;
{--------}
procedure TAbNodeManager.FreeNode(aNode : pointer);
begin
  {add the node (if non-nil) to the top of the free list}
  if (aNode <> nil) then begin
    PGenericNode(aNode)^.gnNext := FFreeList;
    FFreeList := aNode;
  end;
end;
{--------}
function TAbNodeManager.nmAllocNewPage : pointer;
var
  NewPage  : PAnsiChar;
  i        : integer;
  FreeList : pointer;
  NodeSize : integer;
begin
  {allocate a new page and add it to the front of the page list}
  GetMem(NewPage, FPageSize);
  PGenericNode(NewPage)^.gnNext := FPageHead;
  FPageHead := NewPage;

  {now split up the new page into nodes and push them all onto the
   free list; note that the first 4 bytes of the page is a pointer to
   the next page, so remember to skip over it}
  inc(NewPage, sizeof(pointer));
  FreeList := FFreeList;
  NodeSize := FNodeSize;
  for i := 0 to pred(FNodesPerPage) do begin
    PGenericNode(NewPage)^.gnNext := FreeList;
    FreeList := NewPage;
    inc(NewPage, NodeSize);
  end;

  {return the top of the list}
  Result := FreeList;
  FFreeList := PGenericNode(Result)^.gnNext;
end;
{====================================================================}


{====================================================================}
procedure AbortProgress;
begin
  raise EAbAbortProgress.Create('Abort');
end;
{====================================================================}

end.

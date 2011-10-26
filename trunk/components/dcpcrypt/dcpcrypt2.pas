{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* Main component definitions *************************************************}
{******************************************************************************}
{* Copyright (c) 1999-2003 David Barton                                       *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit DCPcrypt2;

{$MODE Delphi}

interface
uses
  Classes, Sysutils, DCPbase64;

//{$DEFINE DCP1COMPAT}  { DCPcrypt v1.31 compatiblity mode - see documentation }

{******************************************************************************}
    { A few predefined types to help out }

type
  {$IFNDEF FPC}
  Pbyte= ^byte;
  Pword= ^word;
  Pdword= ^dword;
  Pint64= ^int64;
  dword= longword;
  Pwordarray= ^Twordarray;
  Twordarray= array[0..19383] of word;
  {$ENDIF}
  Pdwordarray= ^Tdwordarray;
  Tdwordarray= array[0..8191] of dword;


{******************************************************************************}
    { The base class from which all hash algorithms are to be derived  }

type
  EDCP_hash= class(Exception);
  TDCP_hash= class(TComponent)
  protected
    fInitialized: boolean;  { Whether or not the algorithm has been initialized }

    procedure DeadInt(Value: integer);   { Knudge to display vars in the object inspector   }
    procedure DeadStr(Value: string);    { Knudge to display vars in the object inspector   }
  
  private
    function _GetId: integer;
    function _GetAlgorithm: string;
    function _GetHashSize: integer; 

  public
    property Initialized: boolean
      read fInitialized;

    class function GetId: integer; virtual;
      { Get the algorithm id }
    class function GetAlgorithm: string; virtual;
      { Get the algorithm name }
    class function GetHashSize: integer; virtual;
      { Get the size of the digest produced - in bits }
    class function SelfTest: boolean; virtual;
      { Tests the implementation with several test vectors }

    procedure Init; virtual;
      { Initialize the hash algorithm }
    procedure Final(var Digest); virtual;
      { Create the final digest and clear the stored information.
        The size of the Digest var must be at least equal to the hash size }
    procedure Burn; virtual;
      { Clear any stored information with out creating the final digest }

    procedure Update(const Buffer; Size: longword); virtual;
      { Update the hash buffer with Size bytes of data from Buffer }
    procedure UpdateStream(Stream: TStream; Size: longword);
      { Update the hash buffer with Size bytes of data from the stream }
    procedure UpdateStr(const Str: string);
      { Update the hash buffer with the string }

    destructor Destroy; override;

  published
    property Id: integer
      read _GetId write DeadInt;
    property Algorithm: string
      read _GetAlgorithm write DeadStr;
    property HashSize: integer
      read _GetHashSize write DeadInt;
  end;
  TDCP_hashclass= class of TDCP_hash;


{******************************************************************************}
    { The base class from which all encryption components will be derived. }
    { Stream ciphers will be derived directly from this class where as     }
    { Block ciphers will have a further foundation class TDCP_blockcipher. }

type
  EDCP_cipher= class(Exception);
  TDCP_cipher= class(TComponent)
  protected
    fInitialized: boolean;  { Whether or not the key setup has been done yet }

    procedure DeadInt(Value: integer);   { Knudge to display vars in the object inspector   }
    procedure DeadStr(Value: string);    { Knudge to display vars in the object inspector   }

  private
    function _GetId: integer;
    function _GetAlgorithm: string;
    function _GetMaxKeySize: integer; 

  public
    property Initialized: boolean
      read fInitialized;

    class function GetId: integer; virtual;
      { Get the algorithm id }
    class function GetAlgorithm: string; virtual;
      { Get the algorithm name }
    class function GetMaxKeySize: integer; virtual;
      { Get the maximum key size (in bits) }
    class function SelfTest: boolean; virtual;
      { Tests the implementation with several test vectors }

    procedure Init(const Key; Size: longword; InitVector: pointer); virtual;
      { Do key setup based on the data in Key, size is in bits }
    procedure InitStr(const Key: string; HashType: TDCP_hashclass);
      { Do key setup based on a hash of the key string }
    procedure Burn; virtual;
      { Clear all stored key information }
    procedure Reset; virtual;
      { Reset any stored chaining information }
    procedure Encrypt(const Indata; var Outdata; Size: longword); virtual;
      { Encrypt size bytes of data and place in Outdata }
    procedure Decrypt(const Indata; var Outdata; Size: longword); virtual;
      { Decrypt size bytes of data and place in Outdata }
    function EncryptStream(InStream, OutStream: TStream; Size: longword): longword;
      { Encrypt size bytes of data from InStream and place in OutStream }
    function DecryptStream(InStream, OutStream: TStream; Size: longword): longword;
      { Decrypt size bytes of data from InStream and place in OutStream }
    function EncryptString(const Str: string): string; virtual;
      { Encrypt a string and return Base64 encoded }
    function DecryptString(const Str: string): string; virtual;
      { Decrypt a Base64 encoded string }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Id: integer
      read _GetId write DeadInt;
    property Algorithm: string
      read _GetAlgorithm write DeadStr;
    property MaxKeySize: integer
      read _GetMaxKeySize write DeadInt;
  end;
  TDCP_cipherclass= class of TDCP_cipher;


{******************************************************************************}
    { The base class from which all block ciphers are to be derived, this  }
    { extra class takes care of the different block encryption modes.      }

type
  TDCP_ciphermode= (cmCBC, cmCFB8bit, cmCFBblock, cmOFB, cmCTR); // cmCFB8bit is equal to DCPcrypt v1.xx's CFB mode
  EDCP_blockcipher= class(EDCP_cipher);
  TDCP_blockcipher= class(TDCP_cipher)
  protected
    fCipherMode: TDCP_ciphermode;  { The cipher mode the encrypt method uses  }

    procedure InitKey(const Key; Size: longword); virtual;

  private
    function _GetBlockSize: integer;

  public
    class function GetBlockSize: integer; virtual;
      { Get the block size of the cipher (in bits) }

    procedure SetIV(const Value); virtual;
      { Sets the IV to Value and performs a reset }
    procedure GetIV(var Value); virtual;
      { Returns the current chaining information, not the actual IV }

    procedure Encrypt(const Indata; var Outdata; Size: longword); override;
      { Encrypt size bytes of data and place in Outdata using CipherMode }
    procedure Decrypt(const Indata; var Outdata; Size: longword); override;
      { Decrypt size bytes of data and place in Outdata using CipherMode }
    function EncryptString(const Str: string): string; override;
      { Encrypt a string and return Base64 encoded }
    function DecryptString(const Str: string): string; override;
      { Decrypt a Base64 encoded string }
    procedure EncryptECB(const Indata; var Outdata); virtual; 
      { Encrypt a block of data using the ECB method of encryption }
    procedure DecryptECB(const Indata; var Outdata); virtual; 
      { Decrypt a block of data using the ECB method of decryption }
    procedure EncryptCBC(const Indata; var Outdata; Size: longword); virtual; 
      { Encrypt size bytes of data using the CBC method of encryption }
    procedure DecryptCBC(const Indata; var Outdata; Size: longword); virtual; 
      { Decrypt size bytes of data using the CBC method of decryption }
    procedure EncryptCFB8bit(const Indata; var Outdata; Size: longword); virtual; 
      { Encrypt size bytes of data using the CFB (8 bit) method of encryption }
    procedure DecryptCFB8bit(const Indata; var Outdata; Size: longword); virtual; 
      { Decrypt size bytes of data using the CFB (8 bit) method of decryption }
    procedure EncryptCFBblock(const Indata; var Outdata; Size: longword); virtual; 
      { Encrypt size bytes of data using the CFB (block) method of encryption }
    procedure DecryptCFBblock(const Indata; var Outdata; Size: longword); virtual; 
      { Decrypt size bytes of data using the CFB (block) method of decryption }
    procedure EncryptOFB(const Indata; var Outdata; Size: longword); virtual; 
      { Encrypt size bytes of data using the OFB method of encryption }
    procedure DecryptOFB(const Indata; var Outdata; Size: longword); virtual; 
      { Decrypt size bytes of data using the OFB method of decryption }
    procedure EncryptCTR(const Indata; var Outdata; Size: longword); virtual; 
      { Encrypt size bytes of data using the CTR method of encryption }
    procedure DecryptCTR(const Indata; var Outdata; Size: longword); virtual;
      { Decrypt size bytes of data using the CTR method of decryption }

    constructor Create(AOwner: TComponent); override;

  published
    property BlockSize: integer
      read _GetBlockSize write DeadInt;
    property CipherMode: TDCP_ciphermode
      read fCipherMode write fCipherMode default cmCBC;
  end;
  TDCP_blockcipherclass= class of TDCP_blockcipher;


{******************************************************************************}
    { Helper functions }

procedure XorBlock(var InData1, InData2; Size: longword);
// Supposed to be an optimized version of XorBlock() using 32-bit xor
procedure XorBlockEx(var InData1, InData2; Size: longword);
// removes the compiler hint due to first param being 'var' instead of 'out'
procedure dcpFillChar(out x; count: SizeInt; Value: Byte); overload;
procedure dcpFillChar(out x; count: SizeInt; Value: Char); overload;
procedure ZeroMemory(Destination: Pointer; Length: PtrUInt);



implementation

{$Q-}{$R-}


{** TDCP_hash *****************************************************************}

procedure TDCP_hash.DeadInt(Value: integer);
begin
end;

procedure TDCP_hash.DeadStr(Value: string);
begin
end;

function TDCP_hash._GetId: integer;
begin
  Result:= GetId;
end;

function TDCP_hash._GetAlgorithm: string;
begin
  Result:= GetAlgorithm;
end;

function TDCP_hash._GetHashSize: integer;
begin
  Result:= GetHashSize;
end; 

class function TDCP_hash.GetId: integer;
begin
  Result:= -1;
end;

class function TDCP_hash.GetAlgorithm: string;
begin
  Result:= '';
end;

class function TDCP_hash.GetHashSize: integer;
begin
  Result:= -1;
end;

class function TDCP_hash.SelfTest: boolean;
begin
  Result:= false;
end;

procedure TDCP_hash.Init;
begin
end;

procedure TDCP_hash.Final(var Digest);
begin
end;

procedure TDCP_hash.Burn;
begin
end;

procedure TDCP_hash.Update(const Buffer; Size: longword);
begin
end;

procedure TDCP_hash.UpdateStream(Stream: TStream; Size: longword);
var
  Buffer: array[0..8191] of byte;
  i, read: integer;
begin
  dcpFillChar(Buffer, SizeOf(Buffer), 0);
  for i:= 1 to (Size div Sizeof(Buffer)) do
  begin
    read:= Stream.Read(Buffer,Sizeof(Buffer));
    Update(Buffer,read);
  end;
  if (Size mod Sizeof(Buffer))<> 0 then
  begin
    read:= Stream.Read(Buffer,Size mod Sizeof(Buffer));
    Update(Buffer,read);
  end;
end;

procedure TDCP_hash.UpdateStr(const Str: string);
begin
  Update(Str[1],Length(Str));
end;

destructor TDCP_hash.Destroy;
begin
  if fInitialized then
    Burn;
  inherited Destroy;
end;


{** TDCP_cipher ***************************************************************}

procedure TDCP_cipher.DeadInt(Value: integer);
begin
end;

procedure TDCP_cipher.DeadStr(Value: string);
begin
end;

function TDCP_cipher._GetId: integer;
begin
  Result:= GetId;
end;

function TDCP_cipher._GetAlgorithm: string;
begin
  Result:= GetAlgorithm;
end;

function TDCP_cipher._GetMaxKeySize: integer;
begin
  Result:= GetMaxKeySize;
end; 

class function TDCP_cipher.GetId: integer;
begin
  Result:= -1;
end;

class function TDCP_cipher.GetAlgorithm: string;
begin
  Result:= '';
end;

class function TDCP_cipher.GetMaxKeySize: integer;
begin
  Result:= -1;
end;

class function TDCP_cipher.SelfTest: boolean;
begin
  Result:= false;
end;

procedure TDCP_cipher.Init(const Key; Size: longword; InitVector: pointer);
begin
  if fInitialized then
    Burn;
  if (Size <= 0) or ((Size and 3)<> 0) or (Size> longword(GetMaxKeySize)) then
    raise EDCP_cipher.Create('Invalid key size')
  else
    fInitialized:= true;
end;

procedure TDCP_cipher.InitStr(const Key: string; HashType: TDCP_hashclass);
var
  Hash: TDCP_hash;
  Digest: pointer;
begin
  if fInitialized then
    Burn;
  try
    GetMem(Digest,HashType.GetHashSize div 8);
    Hash:= HashType.Create(Self);
    Hash.Init;
    Hash.UpdateStr(Key);
    Hash.Final(Digest^);
    Hash.Free;
    if MaxKeySize< HashType.GetHashSize then
    begin
      Init(Digest^,MaxKeySize,nil);
    end
    else
    begin
      Init(Digest^,HashType.GetHashSize,nil);
    end;
    FillChar(Digest^,HashType.GetHashSize div 8,$FF);
    FreeMem(Digest);
  except
    raise EDCP_cipher.Create('Unable to allocate sufficient memory for hash digest');
  end;
end;

procedure TDCP_cipher.Burn;
begin
  fInitialized:= false;
end;

procedure TDCP_cipher.Reset;
begin
end;

procedure TDCP_cipher.Encrypt(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_cipher.Decrypt(const Indata; var Outdata; Size: longword);
begin
end;

function TDCP_cipher.EncryptStream(InStream, OutStream: TStream; Size: longword): longword;
var
  Buffer: array[0..8191] of byte;
  i, Read: longword;
begin
  dcpFillChar(Buffer, SizeOf(Buffer), 0);
  Result:= 0;
  for i:= 1 to (Size div Sizeof(Buffer)) do
  begin
    Read:= InStream.Read(Buffer,Sizeof(Buffer));
    Inc(Result,Read);
    Encrypt(Buffer,Buffer,Read);
    OutStream.Write(Buffer,Read);
  end;
  if (Size mod Sizeof(Buffer))<> 0 then
  begin
    Read:= InStream.Read(Buffer,Size mod Sizeof(Buffer));
    Inc(Result,Read);
    Encrypt(Buffer,Buffer,Read);
    OutStream.Write(Buffer,Read);
  end;
end;

function TDCP_cipher.DecryptStream(InStream, OutStream: TStream; Size: longword): longword;
var
  Buffer: array[0..8191] of byte;
  i, Read: longword;
begin
  dcpFillChar(Buffer, SizeOf(Buffer), 0);
  Result:= 0;
  for i:= 1 to (Size div Sizeof(Buffer)) do
  begin
    Read:= InStream.Read(Buffer,Sizeof(Buffer));
    Inc(Result,Read);
    Decrypt(Buffer,Buffer,Read);
    OutStream.Write(Buffer,Read);
  end;
  if (Size mod Sizeof(Buffer))<> 0 then
  begin
    Read:= InStream.Read(Buffer,Size mod Sizeof(Buffer));
    Inc(Result,Read);
    Decrypt(Buffer,Buffer,Read);
    OutStream.Write(Buffer,Read);
  end;
end;

function TDCP_cipher.EncryptString(const Str: string): string;
begin
  SetLength(Result,Length(Str));
  Encrypt(Str[1],Result[1],Length(Str));
  Result:= Base64EncodeStr(Result);
end;

function TDCP_cipher.DecryptString(const Str: string): string;
begin
  Result:= Base64DecodeStr(Str);
  Decrypt(Result[1],Result[1],Length(Result));
end;

constructor TDCP_cipher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Burn;
end;

destructor TDCP_cipher.Destroy;
begin
  if fInitialized then
    Burn;
  inherited Destroy;
end;


{** TDCP_blockcipher **********************************************************}

procedure TDCP_blockcipher.InitKey(const Key; Size: longword);
begin
end;

function TDCP_blockcipher._GetBlockSize: integer;
begin
  Result:= GetBlockSize;
end;

class function TDCP_blockcipher.GetBlockSize: integer;
begin
  Result:= -1;
end;

procedure TDCP_blockcipher.SetIV(const Value);
begin
end;

procedure TDCP_blockcipher.GetIV(var Value);
begin
end;

procedure TDCP_blockcipher.Encrypt(const Indata; var Outdata; Size: longword);
begin
  case fCipherMode of
    cmCBC: EncryptCBC(Indata,Outdata,Size);
    cmCFB8bit: EncryptCFB8bit(Indata,Outdata,Size);
    cmCFBblock: EncryptCFBblock(Indata,Outdata,Size);
    cmOFB: EncryptOFB(Indata,Outdata,Size);
    cmCTR: EncryptCTR(Indata,Outdata,Size);
  end;
end;

function TDCP_blockcipher.EncryptString(const Str: string): string;
begin
  SetLength(Result,Length(Str));
  EncryptCFB8bit(Str[1],Result[1],Length(Str));
  Result:= Base64EncodeStr(Result);
end;

function TDCP_blockcipher.DecryptString(const Str: string): string;
begin
  Result:= Base64DecodeStr(Str);
  DecryptCFB8bit(Result[1],Result[1],Length(Result));
end;

procedure TDCP_blockcipher.Decrypt(const Indata; var Outdata; Size: longword);
begin
  case fCipherMode of
    cmCBC: DecryptCBC(Indata,Outdata,Size);
    cmCFB8bit: DecryptCFB8bit(Indata,Outdata,Size);
    cmCFBblock: DecryptCFBblock(Indata,Outdata,Size);
    cmOFB: DecryptOFB(Indata,Outdata,Size);
    cmCTR: DecryptCTR(Indata,Outdata,Size);
  end;
end;

procedure TDCP_blockcipher.EncryptECB(const Indata; var Outdata);
begin
end;

procedure TDCP_blockcipher.DecryptECB(const Indata; var Outdata);
begin
end;

procedure TDCP_blockcipher.EncryptCBC(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCBC(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptCFB8bit(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCFB8bit(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptCFBblock(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCFBblock(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptOFB(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptOFB(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.EncryptCTR(const Indata; var Outdata; Size: longword);
begin
end;

procedure TDCP_blockcipher.DecryptCTR(const Indata; var Outdata; Size: longword);
begin
end;

constructor TDCP_blockcipher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCipherMode:= cmCBC;
end;


{** Helpher functions *********************************************************}
procedure XorBlock(var InData1, InData2; Size: longword);
var
  b1: PByteArray;
  b2: PByteArray;
  i: longword;
begin
  b1 := @InData1;
  b2 := @InData2;
  for i := 0 to size-1 do
    b1[i] := b1[i] xor b2[i];
end;

procedure dcpFillChar(out x; count: SizeInt; Value: Byte);
begin
  {$HINTS OFF}
  FillChar(x, count, value);
  {$HINTS ON}
end;

procedure ZeroMemory(Destination: Pointer; Length: PtrUInt);
begin
  FillChar(Destination^, Length, 0);
end;

procedure dcpFillChar(out x; count: SizeInt; Value: Char);
begin
  {$HINTS OFF}
  FillChar(x, count, Value);
  {$HINTS ON}
end;

// Supposed to be an optimized version of XorBlock() using 32-bit xor
procedure XorBlockEx(var InData1, InData2; Size: longword);
var
  l1: PIntegerArray;
  l2: PIntegerArray;
  b1: PByteArray;
  b2: PByteArray;
  i: integer;
  c: integer;
begin
  l1 := @inData1;
  l2 := @inData2;
  for i := 0 to size div sizeof(LongWord)-1 do
    l1[i] := l1[i] xor l2[i];

  // the rest of the buffer (3 bytes)
  c := size mod sizeof(longWord);
  if c > 0 then begin
    b1 := @InData1;
    b2 := @InData2;
    for i := (size-c) to size-1 do
      b1[i] := b1[i] xor b2[i];
  end;
end;

end.

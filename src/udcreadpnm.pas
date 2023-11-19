{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    PNM writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{
The PNM (Portable aNyMaps) is a generic name for :
  PBM : Portable BitMaps,
  PGM : Portable GrayMaps,
  PPM : Portable PixMaps.
There is normally no file format associated  with PNM itself.}

{$mode objfpc}{$h+}
unit uDCReadPNM;

interface

uses FPImage, Classes, SysUtils, Graphics;

Const
  BufSize = 1024;

type

  { TDCReaderPNM }

  TDCReaderPNM=class (TFPCustomImageReader)
    private
      FBitMapType : Integer;
      FWidth      : Integer;
      FHeight     : Integer;
      FBufPos : Integer;
      FBufLen : Integer;
      FBuffer : Array of AnsiChar;
      function DropWhiteSpaces(Stream: TStream): AnsiChar;
      function ReadChar(Stream: TStream): AnsiChar;
      function ReadInteger(Stream: TStream): Integer;
      procedure ReadScanlineBuffer(Stream: TStream;p:Pbyte;Len:Integer);
    protected
      FMaxVal     : Cardinal;
      FBitPP        : Byte;
      FScanLineSize : Integer;
      FScanLine   : PByte;
      procedure ReadHeader(Stream : TStream); virtual;
      function  InternalCheck (Stream:TStream):boolean;override;
      procedure InternalRead(Stream:TStream;Img:TFPCustomImage);override;
      procedure ReadScanLine(Row : Integer; Stream:TStream);
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage);
  end;

  { TPortableAnyMapGraphic }

  TPortableAnyMapGraphic = class(Graphics.TPortableAnyMapGraphic)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
  end;

implementation

uses
  LCLStrConsts;

const
  WhiteSpaces=[#9,#10,#13,#32];
  {Whitespace (TABs, CRs, LFs, blanks) are separators in the PNM Headers}

{ The magic number at the beginning of a pnm file is 'P1', 'P2', ..., 'P7'
  followed by a WhiteSpace character }

function TDCReaderPNM.InternalCheck(Stream:TStream):boolean;
var
  hdr: array[0..2] of AnsiChar;
  oldPos: Int64;
  i,n: Integer;
begin
  Result:=False;
  if Stream = nil then
    exit;
  oldPos := Stream.Position;
  try
    n := SizeOf(hdr);
    Result:=(Stream.Size-OldPos>=N);
    if not Result then exit;
    For I:=0 to N-1 do
      hdr[i]:=ReadChar(Stream);
    Result:=(hdr[0] = 'P')
            and (hdr[1] in ['1'..'7']) 
            and (hdr[2] in WhiteSpaces);
  finally
    Stream.Position := oldPos;
    FBufLen:=0;
  end;
end;

function TDCReaderPNM.DropWhiteSpaces(Stream : TStream) :AnsiChar;

begin
  with Stream do
    begin
    repeat
      Result:=ReadChar(Stream);
{If we encounter comment then eate line}
      if DropWhiteSpaces='#' then
      repeat
        Result:=ReadChar(Stream);
      until Result=#10;
    until not (Result in WhiteSpaces);
    end;
end;

function TDCReaderPNM.ReadInteger(Stream : TStream) :Integer;

var
  s:String[7];

begin
  s:='';
  s[1]:=DropWhiteSpaces(Stream);
  repeat
    Inc(s[0]);
    s[Length(s)+1]:=ReadChar(Stream);
  until (s[0]=#7) or (s[Length(s)+1] in WhiteSpaces);
  Result:=StrToInt(s);
end;

procedure TDCReaderPNM.ReadScanlineBuffer(Stream: TStream;p:Pbyte;Len:Integer);
// after the header read, there are still bytes in the buffer.
// drain the buffer before going for direct stream reads.
var BytesLeft : integer;
begin
  BytesLeft:=FBufLen-FBufPos;
  if BytesLeft>0 then
    begin
      if BytesLeft>Len then
        BytesLeft:=Len;
      Move (FBuffer[FBufPos],p^,BytesLeft);
      Dec(Len,BytesLeft);
      Inc(FBufPos,BytesLeft);
      Inc(p,BytesLeft);
      if Len>0 then
         Stream.ReadBuffer(p^,len);
    end
  else
    Stream.ReadBuffer(p^,len);
end;

Function TDCReaderPNM.ReadChar(Stream : TStream) : AnsiChar;

begin
  If (FBufPos>=FBufLen) then
    begin
    if Length(FBuffer)=0 then
      SetLength(FBuffer,BufSize);
    FBufLen:=Stream.Read(FBuffer[0],Length(FBuffer));
    if FBuflen=0 then
      Raise EReadError.Create('Failed to read from stream');
    FBufPos:=0;
    end;
  Result:=FBuffer[FBufPos];
  Inc(FBufPos);
end;

procedure TDCReaderPNM.ReadHeader(Stream : TStream);

Var
  C : AnsiChar;

begin
  C:=ReadChar(Stream);
  If (C<>'P') then
    Raise Exception.Create('Not a valid PNM image.');
  C:=ReadChar(Stream);
  FBitmapType:=Ord(C)-Ord('0');
  If Not (FBitmapType in [1..6]) then
    Raise Exception.CreateFmt('Unknown PNM subtype : %s',[C]);
  FWidth:=ReadInteger(Stream);
  FHeight:=ReadInteger(Stream);
  if FBitMapType in [1,4]
  then
    FMaxVal:=1
  else
    FMaxVal:=ReadInteger(Stream);
  If (FWidth<=0) or (FHeight<=0) or (FMaxVal<=0) then
    Raise Exception.Create('Invalid PNM header data');
  case FBitMapType of
    1: FBitPP := 1;                  // 1bit PP (text)
    2: FBitPP := 8 * SizeOf(Word);   // Grayscale (text)
    3: FBitPP := 8 * SizeOf(Word)*3; // RGB (text)
    4: FBitPP := 1;            // 1bit PP (raw)
    5: If (FMaxval>255) then   // Grayscale (raw);
         FBitPP:= 8 * 2
       else
         FBitPP:= 8;
    6: if (FMaxVal>255) then    // RGB (raw)
         FBitPP:= 8 * 6
       else
         FBitPP:= 8 * 3
  end;
//  Writeln(FWidth,'x',Fheight,' Maxval: ',FMaxVal,' BitPP: ',FBitPP);
end;

procedure TDCReaderPNM.InternalRead(Stream:TStream;Img:TFPCustomImage);

var
  Row:Integer;

begin
  ReadHeader(Stream);
  Img.SetSize(FWidth,FHeight);
  Case FBitmapType of
    5,6 : FScanLineSize:=(FBitPP div 8) * FWidth;
  else  
    FScanLineSize:=FBitPP*((FWidth+7) shr 3);
  end;
  GetMem(FScanLine,FScanLineSize);
  try
    for Row:=0 to img.Height-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
//      Writeln(Stream.Position,' ',Stream.Size);
      end;
  finally
    FreeMem(FScanLine);
  end;
end;

procedure TDCReaderPNM.ReadScanLine(Row : Integer; Stream:TStream);

Var
  P : PWord;
  I,j,bitsLeft : Integer;
  PB: PByte;

begin
  Case FBitmapType of
    1 : begin
        PB:=FScanLine;
        For I:=0 to ((FWidth+7)shr 3)-1 do
          begin
            PB^:=0;
            bitsLeft := FWidth-(I shl 3)-1;
            if bitsLeft > 7 then bitsLeft := 7;
            for j:=0 to bitsLeft do
              PB^:=PB^ or (ReadInteger(Stream) shl (7-j));
            Inc(PB);
          end;
        end;
    2 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream);
          Inc(P);
          end;
        end;
    3 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream); // Red
          Inc(P);
          P^:=ReadInteger(Stream); // Green
          Inc(P);
          P^:=ReadInteger(Stream); // Blue;
          Inc(P)
          end;
        end;
    4,5,6 : if FBufPos>=FBufLen then // still bytes in buffer?
              Stream.ReadBuffer(FScanLine^,FScanLineSize)
            else
              ReadScanLineBuffer(Stream,FScanLine,FScanLineSize);
    end;
end;


procedure TDCReaderPNM.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  C : TFPColor;
  L : Cardinal;
  Scale: Int64;

  function ScaleByte(B: Byte):Word;
  begin
    if FMaxVal = 255 then
      Result := (B shl 8) or B { As used for reading .BMP files }
    else { Mimic the above with multiplications }
      Result := (B*(FMaxVal+1) + B) * 65535 div Scale;
  end;

  function ScaleWord(W: Word):Word;
  begin
    if FMaxVal = 65535 then
      Result := BEtoN(W)
    else { Mimic the above with multiplications }
      Result := Int64(W*(FMaxVal+1) + W) * 65535 div Scale;
  end;

  Procedure ByteBnWScanLine;

  Var
    P : PByte;
    I,j,x,bitsLeft : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to ((FWidth+7)shr 3)-1 do
      begin
      L:=P^;
      x := I shl 3;
      bitsLeft := FWidth-x-1;
      if bitsLeft > 7 then bitsLeft := 7;
      for j:=0 to bitsLeft do
        begin
          if L and $80 <> 0 then
            Img.Colors[x,Row]:=colBlack
          else
            Img.Colors[x,Row]:=colWhite;
          L:=L shl 1;
          inc(x);
        end;
      Inc(P);
      end;
  end;

  Procedure WordGrayScanLine;

  Var
    P : PWord;
    I : Integer;

  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleWord(P^);
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure WordRGBScanLine;

  Var
    P : PWord;
    I : Integer;

  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=ScaleWord(P^);
      Inc(P);
      C.Green:=ScaleWord(P^);
      Inc(P);
      C.Blue:=ScaleWord(P^);
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure ByteGrayScanLine;

  Var
    P : PByte;
    I : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleByte(P^);
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure ByteRGBScanLine;

  Var
    P : PByte;
    I : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=ScaleByte(P^);
      Inc(P);
      C.Green:=ScaleByte(P^);
      Inc(P);
      C.Blue:=ScaleByte(P^);
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

begin
  C.Alpha:=AlphaOpaque;
  Scale := FMaxVal*(FMaxVal+1) + FMaxVal;
  Case FBitmapType of
    1 : ByteBnWScanLine;
    2 : WordGrayScanline;
    3 : WordRGBScanline;
    4 : ByteBnWScanLine;
    5 : If FBitPP=8 then
          ByteGrayScanLine
        else
          WordGrayScanLine;
    6 : If FBitPP=24 then
          ByteRGBScanLine
        else
          WordRGBScanLine;
    end;
end;

{ TPortableAnyMapGraphic }

class function TPortableAnyMapGraphic.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TDCReaderPNM;
end;

procedure Initialize;
begin
  // Replace image handler
  GraphicFilter(Graphics.TPortableAnyMapGraphic);
  TPicture.UnregisterGraphicClass(Graphics.TPortableAnyMapGraphic);
  TPicture.RegisterFileFormat(TPortableAnyMapGraphic.GetFileExtensions, rsPortablePixmap, TPortableAnyMapGraphic);
end;

initialization
  Initialize;

end.


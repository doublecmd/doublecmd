{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012-2013 by the Free Pascal development team

    Tiff reader for fpImage.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Working:
   Black and white 1 bit,
   Grayscale 8,16bit (optional alpha),
   RGB 8,16bit (optional alpha),
   Orientation,
   skipping Thumbnail to read first image,
   compression: packbits, LZW, deflate
   endian
   multiple images
   strips and tiles

 ToDo:
   Compression: jpeg, ...
   PlanarConfiguration 2
   ColorMap
   separate mask
   fillorder - not needed by baseline tiff reader
   bigtiff 64bit offsets
   XMP tag 700
   ICC profile tag 34675
   orientation with rotation
}
unit uFPReadTiff;

{$mode objfpc}{$H+}

{$inline on}

interface

uses
  Math, Classes, SysUtils, ctypes, zinflate, zbase, FPimage, FPTiffCmn;

type
  TFPReaderTiff = class;

  TTiffCreateCompatibleImgEvent = procedure(Sender: TFPReaderTiff;
                                            ImgFileDir: TTiffIFD) of object;

  TTiffCheckIFDOrder = (
    tcioSmart,
    tcioAlways,
    tcioNever
    );

  { TFPReaderTiff }

  TFPReaderTiff = class(TFPCustomImageReader)
  private
    FCheckIFDOrder: TTiffCheckIFDOrder;
    FFirstIFDStart: DWord;
    FOnCreateImage: TTiffCreateCompatibleImgEvent;
    FReverserEndian: boolean;
    IFD: TTiffIFD;
    {$ifdef FPC_Debug_Image}
    FDebug: boolean;
    {$endif}
    fIFDStarts: TFPList;
    FReverseEndian: Boolean;
    fStartPos: int64;
    s: TStream;
    function GetImages(Index: integer): TTiffIFD;
    procedure TiffError(Msg: string);
    procedure SetStreamPos(p: DWord);
    function ReadTiffHeader(QuickTest: boolean; out IFDStart: DWord): boolean; // returns IFD: offset to first IFD
    function ReadIFD(Start: DWord): DWord;// Image File Directory
    procedure ReadDirectoryEntry(var EntryTag: Word);
    function ReadEntryUnsigned: DWord;
    function ReadEntrySigned: Cint32;
    function ReadEntryRational: TTiffRational;
    function ReadEntryString: string;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: DWord;
    procedure ReadValues(StreamPos: DWord;
                         out EntryType: word; out EntryCount: DWord;
                         out Buffer: Pointer; out ByteCount: PtrUInt);
    procedure ReadShortOrLongValues(StreamPos: DWord;
                                    out Buffer: PDWord; out Count: DWord);
    procedure ReadShortValues(StreamPos: DWord;
                              out Buffer: PWord; out Count: DWord);
    procedure ReadImageProperties(
      out RedBits, GreenBits, BlueBits, GrayBits, AlphaBits: Word;
      out ExtraSamples: PWord; out ExtraSampleCnt: DWord;
      out SampleBits: PWord; out SampleBitsPerPixel: DWord);
    procedure ReadImgValue(BitCount: Word; var Run: Pointer; x: dword;
      Predictor: word; var LastValue: word; out Value: Word); inline;
    function FixEndian(w: Word): Word; inline;
    function FixEndian(d: DWord): DWord; inline;
    procedure SetFPImgExtras(CurImg: TFPCustomImage);
    procedure DecodePackBits(var Buffer: Pointer; var Count: PtrInt);
    procedure DecodeLZW(var Buffer: Pointer; var Count: PtrInt);
    procedure DecodeDeflate(var Buffer: Pointer; var Count: PtrInt; ExpectedCount: PtrInt);
  protected
    procedure InternalRead(Str: TStream; AnImage: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
    procedure DoCreateImage(ImgFileDir: TTiffIFD); virtual;
  public
    ImageList: TFPList; // list of TTiffIFD
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(aStream: TStream; AutoClear: boolean = true);
    procedure LoadHeaderFromStream(aStream: TStream);
    procedure LoadIFDsFromStream; // requires LoadHeaderFromStream, creates Images
    procedure LoadImageFromStream(Index: integer); // requires LoadIFDsFromStream
    {$ifdef FPC_Debug_Image}
    property Debug: boolean read FDebug write FDebug;
    {$endif}
    property StartPos: int64 read fStartPos;
    property ReverserEndian: boolean read FReverserEndian;
    property TheStream: TStream read s;
    property OnCreateImage: TTiffCreateCompatibleImgEvent read FOnCreateImage
                                                          write FOnCreateImage;
    property CheckIFDOrder: TTiffCheckIFDOrder read FCheckIFDOrder write FCheckIFDOrder;
    function FirstImg: TTiffIFD;
    function GetBiggestImage: TTiffIFD;
    function ImageCount: integer;
    property Images[Index: integer]: TTiffIFD read GetImages; default;
    property FirstIFDStart: DWord read FFirstIFDStart;
  end;

procedure DecompressPackBits(Buffer: Pointer; Count: PtrInt;
  out NewBuffer: Pointer; out NewCount: PtrInt);
procedure DecompressLZW(Buffer: Pointer; Count: PtrInt;
  out NewBuffer: PByte; out NewCount: PtrInt);
function DecompressDeflate(Compressed: PByte; CompressedCount: cardinal;
  out Decompressed: PByte; var DecompressedCount: cardinal;
  ErrorMsg: PAnsiString = nil): boolean;

implementation

function CMYKToFPColor(C,M,Y,K: Word): TFPColor;
var R, G, B : LongWord;
begin
   R := $ffff - ((LongWord(C)*($ffff-LongWord(K))) shr 16) - LongWord(K) ;
   G := $ffff - ((LongWord(M)*($ffff-LongWord(K))) shr 16) - LongWord(K) ;
   B := $ffff - ((LongWord(Y)*($ffff-LongWord(K))) shr 16) - LongWord(K) ;
   Result := FPColor(R and $ffff,G and $ffff,B and $ffff);
end ;

procedure TFPReaderTiff.TiffError(Msg: string);
begin
  Msg:=Msg+' at position '+IntToStr(s.Position);
  if fStartPos>0 then
    Msg:=Msg+' (TiffPosition='+IntToStr(fStartPos)+')';
  raise Exception.Create(Msg);
end;

function TFPReaderTiff.GetImages(Index: integer): TTiffIFD;
begin
  Result:=TTiffIFD(ImageList[Index]);
end;

procedure TFPReaderTiff.ReadImageProperties(out RedBits, GreenBits, BlueBits,
  GrayBits, AlphaBits: Word; out ExtraSamples: PWord; out
  ExtraSampleCnt: DWord; out SampleBits: PWord; out SampleBitsPerPixel: DWord);
var
  BytesPerPixel: Word;
  SampleCnt: DWord;
  i: Integer;
begin
  ReadShortValues(IFD.BitsPerSample, SampleBits, SampleCnt);
  if SampleCnt<>IFD.SamplesPerPixel then
    TiffError('Samples='+IntToStr(SampleCnt)+' <> SamplesPerPixel='+IntToStr(IFD
      .SamplesPerPixel));
  if IFD.ExtraSamples>0 then
    ReadShortValues(IFD.ExtraSamples, ExtraSamples, ExtraSampleCnt);
  if ExtraSampleCnt>=SampleCnt then
    TiffError('Samples='+IntToStr(SampleCnt)+' ExtraSampleCnt='+IntToStr(
      ExtraSampleCnt));

  case IFD.PhotoMetricInterpretation of
  0, 1: if SampleCnt-ExtraSampleCnt<>1 then
    TiffError('gray images expect one sample per pixel, but found '+IntToStr(
      SampleCnt));
  2: if SampleCnt-ExtraSampleCnt<>3 then
    TiffError('rgb images expect three samples per pixel, but found '+IntToStr(
      SampleCnt));
  3: if SampleCnt-ExtraSampleCnt<>1 then
    TiffError('palette images expect one sample per pixel, but found '+IntToStr(
      SampleCnt));
  4: if SampleCnt-ExtraSampleCnt<>1 then
    TiffError('mask images expect one sample per pixel, but found '+IntToStr(
      SampleCnt));
  5: if SampleCnt-ExtraSampleCnt<>4 then
    TiffError('cmyk images expect four samples per pixel, but found '+IntToStr(
      SampleCnt));
  end;

  GrayBits:=0;
  RedBits:=0;
  GreenBits:=0;
  BlueBits:=0;
  AlphaBits:=0;
  BytesPerPixel:=0;
  SampleBitsPerPixel:=0;
  for i:=0 to SampleCnt-1 do begin
    if SampleBits[i]>64 then
      TiffError('Samples bigger than 64 bit not supported');
    if not (SampleBits[i] in [1, 8, 16]) then
      TiffError('Only samples of 1, 8 and 16 bit are supported');
    inc(SampleBitsPerPixel, SampleBits[i]);
  end;
  case IFD.PhotoMetricInterpretation of
  0, 1:
    begin
      GrayBits:=SampleBits[0];
      IFD.GrayBits:=GrayBits;
      for i:=0 to ExtraSampleCnt-1 do begin
        if ExtraSamples[i] in [1, 2] then begin
          AlphaBits:=SampleBits[1+i];
          IFD.AlphaBits:=AlphaBits;
        end;
      end;
      if not (GrayBits in [1, 8, 16]) then
        TiffError('gray image only supported with gray BitsPerSample 1, 8 or 16');
      if not (AlphaBits in [0, 8, 16]) then
        TiffError('gray image only supported with alpha BitsPerSample 8 or 16');
    end;
  2:
    begin
      RedBits:=SampleBits[0];
      GreenBits:=SampleBits[1];
      BlueBits:=SampleBits[2];
      IFD.RedBits:=RedBits;
      IFD.GreenBits:=GreenBits;
      IFD.BlueBits:=BlueBits;
      IFD.AlphaBits:=0;
      for i:=0 to ExtraSampleCnt-1 do begin
        //writeln('  ',i,'/',ExtraSampleCnt,' Type=',ExtraSamples[i],' Count=',SampleBits[3+i]);
        if ExtraSamples[i] in [1, 2] then begin
          AlphaBits:=SampleBits[3+i];
          IFD.AlphaBits:=AlphaBits;
        end;
      end;
      if not (RedBits in [8, 16]) then
        TiffError('RGB image only supported with red BitsPerSample 8 or 16');
      if not (GreenBits in [8, 16]) then
        TiffError('RGB image only supported with green BitsPerSample 8 or 16');
      if not (BlueBits in [8, 16]) then
        TiffError('RGB image only supported with blue BitsPerSample 8 or 16');
      if not (AlphaBits in [0, 8, 16]) then
        TiffError('RGB image only supported with alpha BitsPerSample 8 or 16');
    end;
  5:
    begin
      RedBits:=SampleBits[0];
      GreenBits:=SampleBits[1];
      BlueBits:=SampleBits[2];
      GrayBits:=SampleBits[3];
      IFD.RedBits:=RedBits;
      IFD.GreenBits:=GreenBits;
      IFD.BlueBits:=BlueBits;
      IFD.GrayBits:=GrayBits;
      IFD.AlphaBits:=0;
      for i:=0 to ExtraSampleCnt-1 do begin
        if ExtraSamples[i] in [1, 2] then begin
          AlphaBits:=SampleBits[4+i];
          IFD.AlphaBits:=AlphaBits;
        end;
      end;
      if not (RedBits in [8, 16]) then
        TiffError('CMYK image only supported with cyan BitsPerSample 8 or 16');
      if not (GreenBits in [8, 16]) then
        TiffError('CMYK image only supported with magenta BitsPerSample 8 or 16'
          );
      if not (BlueBits in [8, 16]) then
        TiffError('CMYK image only supported with yellow BitsPerSample 8 or 16'
          );
      if not (GrayBits in [8, 16]) then
        TiffError('CMYK image only supported with black BitsPerSample 8 or 16');
      if not (AlphaBits in [0, 8, 16]) then
        TiffError('CMYK image only supported with alpha BitsPerSample 8 or 16');
    end;
  end;
  BytesPerPixel:=(GrayBits+RedBits+GreenBits+BlueBits+AlphaBits) div 8;
  IFD.BytesPerPixel:=BytesPerPixel;
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('BytesPerPixel=', BytesPerPixel);
  {$endif}

  if not (IFD.FillOrder in [0, 1]) then
    TiffError('FillOrder unsupported: '+IntToStr(IFD.FillOrder));
end;

procedure TFPReaderTiff.SetFPImgExtras(CurImg: TFPCustomImage);
begin
  ClearTiffExtras(CurImg);
  // set Tiff extra attributes
  CurImg.Extra[TiffPhotoMetric]:=IntToStr(IFD.PhotoMetricInterpretation);
  //writeln('TFPReaderTiff.SetFPImgExtras PhotoMetric=',CurImg.Extra[TiffPhotoMetric]);
  if IFD.Artist<>'' then
    CurImg.Extra[TiffArtist]:=IFD.Artist;
  if IFD.Copyright<>'' then
    CurImg.Extra[TiffCopyright]:=IFD.Copyright;
  if IFD.DocumentName<>'' then
    CurImg.Extra[TiffDocumentName]:=IFD.DocumentName;
  if IFD.DateAndTime<>'' then
    CurImg.Extra[TiffDateTime]:=IFD.DateAndTime;
  if IFD.HostComputer<>'' then
    CurImg.Extra[TiffHostComputer]:=IFD.HostComputer;
  if IFD.ImageDescription<>'' then
    CurImg.Extra[TiffImageDescription]:=IFD.ImageDescription;
  if IFD.Make_ScannerManufacturer<>'' then
    CurImg.Extra[TiffMake_ScannerManufacturer]:=IFD.Make_ScannerManufacturer;
  if IFD.Model_Scanner<>'' then
    CurImg.Extra[TiffModel_Scanner]:=IFD.Model_Scanner;
  if IFD.Software<>'' then
    CurImg.Extra[TiffSoftware]:=IFD.Software;
  if not (IFD.Orientation in [1..8]) then
    IFD.Orientation:=1;
  CurImg.Extra[TiffOrientation]:=IntToStr(IFD.Orientation);
  if IFD.ResolutionUnit<>0 then
    CurImg.Extra[TiffResolutionUnit]:=IntToStr(IFD.ResolutionUnit);
  if (IFD.XResolution.Numerator<>0) or (IFD.XResolution.Denominator<>0) then
    CurImg.Extra[TiffXResolution]:=TiffRationalToStr(IFD.XResolution);
  if (IFD.YResolution.Numerator<>0) or (IFD.YResolution.Denominator<>0) then
    CurImg.Extra[TiffYResolution]:=TiffRationalToStr(IFD.YResolution);
  CurImg.Extra[TiffRedBits]:=IntToStr(IFD.RedBits);
  CurImg.Extra[TiffGreenBits]:=IntToStr(IFD.GreenBits);
  CurImg.Extra[TiffBlueBits]:=IntToStr(IFD.BlueBits);
  CurImg.Extra[TiffGrayBits]:=IntToStr(IFD.GrayBits);
  CurImg.Extra[TiffAlphaBits]:=IntToStr(IFD.AlphaBits);
  if IFD.PageCount>0 then begin
    CurImg.Extra[TiffPageNumber]:=IntToStr(IFD.PageNumber);
    CurImg.Extra[TiffPageCount]:=IntToStr(IFD.PageCount);
  end;
  if IFD.PageName<>'' then
    CurImg.Extra[TiffPageName]:=IFD.PageName;
  if IFD.ImageIsThumbNail then
    CurImg.Extra[TiffIsThumbnail]:='1';
  if IFD.ImageIsMask then
    CurImg.Extra[TiffIsMask]:='1';
  if IFD.Compression<>TiffCompressionNone then
    CurImg.Extra[TiffCompression]:=IntToStr(IFD.Compression);

  {$ifdef FPC_Debug_Image}
  if Debug then
    WriteTiffExtras('SetFPImgExtras', CurImg);
  {$endif}
end;

procedure TFPReaderTiff.ReadImgValue(BitCount: Word; var Run: Pointer; x: dword;
  Predictor: word; var LastValue: word; out Value: Word); inline;
var
  BitNumber: byte;
begin
  case BitCount of
  1:
    begin
      //Get the value of the right bit depending on x value and scale it to dword.
      BitNumber:=7-(x mod 8); //Leftmost pixel starts with bit 7
      Value:=$ffff*((PCUInt8(Run)^) and (1 shl BitNumber) shr BitNumber);
      if Predictor=2 then begin
        TiffError('predictor 2 not supported for bilevel images');
      end;
      if ((x+1) mod 8)=0 then
        inc(Run); //next byte when all bits read
    end;
  8:
    begin
      Value:=PCUInt8(Run)^;
      if Predictor=2 then begin
        // horizontal difference
        if x>0 then
          Value:=(Value+LastValue) and $ff;
        LastValue:=Value;
      end;
      Value:=Value shl 8+Value;
      inc(Run);
    end;
  16:
    begin
      Value:=FixEndian(PCUInt16(Run)^);
      if Predictor=2 then begin
        // horizontal difference
        if x>0 then
          Value:=(Value+LastValue) and $ffff;
        LastValue:=Value;
      end;
      inc(Run,2);
    end;
  end;
end;

procedure TFPReaderTiff.SetStreamPos(p: DWord);
var
  NewPosition: int64;
begin
  NewPosition:=Int64(p)+fStartPos;
  if NewPosition>s.Size then
    TiffError('Offset outside of stream');
  s.Position:=NewPosition;
end;

procedure TFPReaderTiff.LoadFromStream(aStream: TStream; AutoClear: boolean);
var
  IFDStart: DWord;
  i: Integer;
  aContinue: Boolean;
begin
  if AutoClear then
    Clear;
  aContinue:=true;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', aContinue);
  if not aContinue then exit;
  LoadHeaderFromStream(aStream);
  try
    IFDStart:=FirstIFDStart;
    i:=0;
    while IFDStart>0 do begin
      if i=ImageCount then
        ImageList.Add(TTiffIFD.Create);
      IFD:=Images[i];
      IFDStart:=ReadIFD(IFDStart);
      LoadImageFromStream(i);
      inc(i);
    end;
  finally
    IFD:=nil;
  end;
  Progress(psEnding, 100, False, Rect(0,0,0,0), '', aContinue);
end;

procedure TFPReaderTiff.LoadHeaderFromStream(aStream: TStream);
begin
  FFirstIFDStart:=0;
  s:=aStream;
  fStartPos:=s.Position;
  ReadTiffHeader(false,FFirstIFDStart);
end;

procedure TFPReaderTiff.LoadIFDsFromStream;
var
  i: Integer;
  IFDStart: DWord;
begin
  try
    IFDStart:=FirstIFDStart;
    i:=0;
    while IFDStart>0 do begin
      if ImageCount=i then
        ImageList.Add(TTiffIFD.Create);
      IFD:=Images[i];
      IFDStart:=ReadIFD(IFDStart);
      inc(i);
    end;
  finally
    IFD:=nil;
  end;
end;

function TFPReaderTiff.FirstImg: TTiffIFD;
begin
  Result:=nil;
  if (ImageList=nil) or (ImageList.Count=0) then exit;
  Result:=TTiffIFD(ImageList[0]);
end;

function TFPReaderTiff.GetBiggestImage: TTiffIFD;
var
  Size: Int64;
  Img: TTiffIFD;
  CurSize: int64;
  i: Integer;
begin
  Result:=nil;
  Size:=0;
  for i:=0 to ImageCount-1 do begin
    Img:=Images[i];
    CurSize:=Int64(Img.ImageWidth)*Img.ImageHeight;
    if CurSize<Size then continue;
    Size:=CurSize;
    Result:=Img;
  end;
end;

function TFPReaderTiff.ImageCount: integer;
begin
  Result:=ImageList.Count;
end;

function TFPReaderTiff.ReadTiffHeader(QuickTest: boolean; out IFDStart: DWord): boolean;
var
  ByteOrder: String;
  BigEndian: Boolean;
  FortyTwo: Word;
begin
  Result:=false;
  // read byte order  II low endian, MM big endian
  ByteOrder:='  ';
  s.Read(ByteOrder[1],2);
  //debugln(['TForm1.ReadTiffHeader ',dbgstr(ByteOrder)]);
  if ByteOrder='II' then
    BigEndian:=false
  else if ByteOrder='MM' then
    BigEndian:=true
  else if QuickTest then
    exit
  else
    TiffError('expected II or MM');
  FReverseEndian:={$ifdef FPC_BIG_ENDIAN}not{$endif} BigEndian;
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TFPReaderTiff.ReadTiffHeader Endian Big=',BigEndian,' ReverseEndian=',FReverseEndian);
  {$endif}
  // read magic number 42
  FortyTwo:=ReadWord;
  if FortyTwo<>42 then begin
    if QuickTest then
      exit
    else
      TiffError('expected 42, because of its deep philosophical impact, but found '+IntToStr(FortyTwo));
  end;
  // read offset to first IFD
  IFDStart:=ReadDWord;
  //debugln(['TForm1.ReadTiffHeader IFD=',IFD]);
  Result:=true;
end;

function TFPReaderTiff.ReadIFD(Start: DWord): DWord;
var
  Count: Word;
  i: Integer;
  EntryTag: Word;
  p: Int64;
begin
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('ReadIFD Start=',Start);
  {$endif}
  Result:=0;
  SetStreamPos(Start);
  IFD.IFDStart:=Start;
  Count:=ReadWord;
  EntryTag:=0;
  p:=s.Position;
  for i:=1 to Count do begin
    ReadDirectoryEntry(EntryTag);
    inc(p,12);
    s.Position:=p;
  end;
  // read start of next IFD
  Result:=ReadDWord;
  IFD.IFDNext:=Result;
  if (Result<>0) and (Result<Start) then begin
    // backward jump: check for loops
    if fIFDStarts=nil then
      fIFDStarts:=TFPList.Create
    else if fIFDStarts.IndexOf({%H-}Pointer(PtrUInt(Result)))>0 then
      TiffError('endless loop in Image File Descriptors');
    fIFDStarts.Add({%H-}Pointer(PtrUInt(Result)));
  end;
end;

procedure TFPReaderTiff.ReadDirectoryEntry(var EntryTag: Word);
var
  EntryType: Word;
  EntryCount: DWord;
  EntryStart: DWord;
  NewEntryTag: Word;
  UValue: DWord;
  SValue: integer;
  WordBuffer: PWord;
  Count: DWord;
  i: Integer;

  function GetPos: DWord;
  begin
     Result:=DWord(s.Position-fStartPos-2)
  end;

begin
  NewEntryTag:=ReadWord;
  if (NewEntryTag<EntryTag) then begin
    // the TIFF specification insists on ordered entry tags in each IFD
    // This allows to spot damaged files.
    // But some programs like 'GraphicConverter' do not order the extension tags
    // properly.
    {$ifdef FPC_Debug_Image}
    if Debug then
      writeln('WARNING: Tags must be in ascending order: Last='+IntToStr(EntryTag)+' Next='+IntToStr(NewEntryTag));
    {$endif}
    case CheckIFDOrder of
    tcioAlways: TiffError('Tags must be in ascending order: Last='+IntToStr(EntryTag)+' Next='+IntToStr(NewEntryTag));
    tcioSmart:
      if NewEntryTag<30000 then
        TiffError('Tags must be in ascending order: Last='+IntToStr(EntryTag)+' Next='+IntToStr(NewEntryTag));
    end;
  end;
  EntryTag:=NewEntryTag;
  case EntryTag of
  254:
    begin
      // NewSubFileType
      UValue:=ReadEntryUnsigned;
      IFD.ImageIsThumbNail:=UValue and 1<>0;
      IFD.ImageIsPage:=UValue and 2<>0;
      IFD.ImageIsMask:=UValue and 4<>0;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 254: NewSubFileType ThumbNail=',IFD.ImageIsThumbNail,' Page=',IFD.ImageIsPage,' Mask=',IFD.ImageIsMask);
      {$endif}
    end;
  255:
    begin
      // SubFileType (deprecated)
      UValue:=ReadEntryUnsigned;
      IFD.ImageIsThumbNail:=false;
      IFD.ImageIsPage:=false;
      IFD.ImageIsMask:=false;
      case UValue of
      1: ;
      2: IFD.ImageIsThumbNail:=true;
      3: IFD.ImageIsPage:=true;
      else
        TiffError('SubFileType expected, but found '+IntToStr(UValue));
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 255: SubFileType ThumbNail=',IFD.ImageIsThumbNail,' Page=',IFD.ImageIsPage,' Mask=',IFD.ImageIsMask);
      {$endif}
    end;
  256:
    begin
      // fImageWidth
      IFD.ImageWidth:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 256: ImageWidth=',IFD.ImageWidth);
      {$endif}
    end;
  257:
    begin
      // ImageLength according to TIFF spec, here used as imageheight
      IFD.ImageHeight:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 257: ImageHeight=',IFD.ImageHeight);
      {$endif}
    end;
  258:
    begin
      // BitsPerSample
      IFD.BitsPerSample:=GetPos;
      ReadShortValues(IFD.BitsPerSample,WordBuffer,Count);
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TFPReaderTiff.ReadDirectoryEntry Tag 258: BitsPerSample: ');
        for i:=0 to Count-1 do
          write(IntToStr(WordBuffer[i]),' ');
        writeln;
      end;
      {$endif}
      try
        SetLength(IFD.BitsPerSampleArray,Count);
        for i:=0 to Count-1 do
          IFD.BitsPerSampleArray[i]:=WordBuffer[i];
      finally
        ReAllocMem(WordBuffer,0);
      end;
    end;
  259:
    begin
      // Compression
      UValue:=ReadEntryUnsigned;
      case UValue of
      TiffCompressionNone,
      TiffCompressionCCITTRLE,
      TiffCompressionCCITTFAX3,
      TiffCompressionCCITTFAX4,
      TiffCompressionLZW,
      TiffCompressionOldJPEG,
      TiffCompressionJPEG,
      TiffCompressionDeflateAdobe,
      TiffCompressionJBIGBW,
      TiffCompressionJBIGCol,
      TiffCompressionNeXT,
      TiffCompressionCCITTRLEW,
      TiffCompressionPackBits,
      TiffCompressionThunderScan,
      TiffCompressionIT8CTPAD,
      TiffCompressionIT8LW,
      TiffCompressionIT8MP,
      TiffCompressionIT8BL,
      TiffCompressionPixarFilm,
      TiffCompressionPixarLog,
      TiffCompressionDeflateZLib,
      TiffCompressionDCS,
      TiffCompressionJBIG,
      TiffCompressionSGILog,
      TiffCompressionSGILog24,
      TiffCompressionJPEG2000: ;
      else
        TiffError('expected Compression, but found '+IntToStr(UValue));
      end;
      IFD.Compression:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 259: Compression=',IntToStr(IFD.Compression),'=',TiffCompressionName(IFD.Compression));
      {$endif}
    end;
  262:
    begin
      // PhotometricInterpretation
      UValue:=ReadEntryUnsigned;
      case UValue of
      0: ; // bilevel grayscale 0 is white
      1: ; // bilevel grayscale 0 is black
      2: ; // RGB 0,0,0 is black
      3: ; // Palette color
      4: ; // Transparency Mask
      5: ; // CMYK
      else
        TiffError('expected PhotometricInterpretation, but found '+IntToStr(UValue));
      end;
      IFD.PhotoMetricInterpretation:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TFPReaderTiff.ReadDirectoryEntry Tag 262: PhotometricInterpretation=');
        case IFD.PhotoMetricInterpretation of
        0: write('0=bilevel grayscale 0 is white');
        1: write('1=bilevel grayscale 0 is black');
        2: write('2=RGB 0,0,0 is black');
        3: write('3=Palette color');
        4: write('4=Transparency Mask');
        5: write('5=CMYK 8bit');
        end;
        writeln;
      end;
      {$endif}
    end;
  263:
    begin
      // Tresholding
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: ; // no dithering or halftoning was applied
      2: ; // an ordered dithering or halftoning was applied
      3: ; // a randomized dithering or halftoning was applied
      else
        TiffError('expected Tresholding, but found '+IntToStr(UValue));
      end;
      IFD.Tresholding:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 263: Tresholding=',IFD.Tresholding);
      {$endif}
    end;
  264:
    begin
      // CellWidth
      IFD.CellWidth:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 264: CellWidth=',IFD.CellWidth);
      {$endif}
    end;
  265:
    begin
      // CellLength
      IFD.CellLength:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 265: CellLength=',IFD.CellLength);
      {$endif}
    end;
  266:
    begin
      // FillOrder
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: IFD.FillOrder:=1; // left to right = high to low
      2: IFD.FillOrder:=2; // left to right = low to high
      else
        TiffError('expected FillOrder, but found '+IntToStr(UValue));
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TFPReaderTiff.ReadDirectoryEntry Tag 266: FillOrder=',IntToStr(IFD.FillOrder),'=');
        case IFD.FillOrder of
        1: write('left to right = high to low');
        2: write('left to right = low to high');
        end;
        writeln;
      end;
      {$endif}
    end;
  269:
    begin
      // DocumentName
      IFD.DocumentName:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 269: DocumentName=',IFD.DocumentName);
      {$endif}
    end;
  270:
    begin
      // ImageDescription
      IFD.ImageDescription:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 270: ImageDescription=',IFD.ImageDescription);
      {$endif}
    end;
  271:
    begin
      // Make - scanner manufacturer
      IFD.Make_ScannerManufacturer:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 271: Make_ScannerManufacturer=',IFD.Make_ScannerManufacturer);
      {$endif}
    end;
  272:
    begin
      // Model - scanner model
      IFD.Model_Scanner:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 272: Model_Scanner=',IFD.Model_Scanner);
      {$endif}
    end;
  273:
    begin
      // StripOffsets (store offset to entity, not the actual contents of the offsets)
      IFD.StripOffsets:=GetPos; //Store position of entity so we can look up multiple offsets later
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 273: StripOffsets, offset for entry=',IFD.StripOffsets);
      {$endif}
    end;
  274:
    begin
      // Orientation
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: ;// 0,0 is left, top
      2: ;// 0,0 is right, top
      3: ;// 0,0 is right, bottom
      4: ;// 0,0 is left, bottom
      5: ;// 0,0 is top, left (rotated)
      6: ;// 0,0 is top, right (rotated)
      7: ;// 0,0 is bottom, right (rotated)
      8: ;// 0,0 is bottom, left (rotated)
      else
        TiffError('expected Orientation, but found '+IntToStr(UValue));
      end;
      IFD.Orientation:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TFPReaderTiff.ReadDirectoryEntry Tag 274: Orientation=',IntToStr(IFD.Orientation),'=');
        case IFD.Orientation of
        1: write('0,0 is left, top');
        2: write('0,0 is right, top');
        3: write('0,0 is right, bottom');
        4: write('0,0 is left, bottom');
        5: write('0,0 is top, left (rotated)');
        6: write('0,0 is top, right (rotated)');
        7: write('0,0 is bottom, right (rotated)');
        8: write('0,0 is bottom, left (rotated)');
        end;
        writeln;
      end;
      {$endif}
    end;
  277:
    begin
      // SamplesPerPixel
      IFD.SamplesPerPixel:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 277: SamplesPerPixel=',IFD.SamplesPerPixel);
      {$endif}
    end;
  278:
    begin
      // RowsPerStrip
      UValue:=ReadEntryUnsigned;
      if UValue=0 then
        TiffError('expected RowsPerStrip, but found '+IntToStr(UValue));
      IFD.RowsPerStrip:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 278: RowsPerStrip=',IFD.RowsPerStrip);
      {$endif}
    end;
  279:
    begin
      // StripByteCounts (the number of bytes in each strip).
      // We're storing the position of the tag, not the various bytecounts themselves
      IFD.StripByteCounts:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 279: StripByteCounts, offset for entry=',IFD.StripByteCounts);
      {$endif}
    end;
  280:
    begin
      // MinSampleValue
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 280: skipping MinSampleValue');
      {$endif}
    end;
  281:
    begin
      // MaxSampleValue
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 281: skipping MaxSampleValue');
      {$endif}
    end;
  282:
    begin
      // XResolution
      IFD.XResolution:=ReadEntryRational;
      {$ifdef FPC_Debug_Image}
      try
        if Debug then
          writeln('TFPReaderTiff.ReadDirectoryEntry Tag 282: XResolution=',IFD.XResolution.Numerator,'/',IFD.XResolution.Denominator,'=',IFD.XResolution.Numerator/IFD.XResolution.Denominator);
      except
        //ignore division by 0
      end;
      {$endif}
    end;
  283:
    begin
      // YResolution
      IFD.YResolution:=ReadEntryRational;
      {$ifdef FPC_Debug_Image}
      try
        if Debug then
          writeln('TFPReaderTiff.ReadDirectoryEntry Tag 283: YResolution=',IFD.YResolution.Numerator,'/',IFD.YResolution.Denominator,'=',IFD.YResolution.Numerator/IFD.YResolution.Denominator);
      except
        //ignore division by 0
      end;      {$endif}
    end;
  284:
    begin
      // PlanarConfiguration
      SValue:=ReadEntrySigned;
      case SValue of
      TiffPlanarConfigurationChunky: ; // 1
      TiffPlanarConfigurationPlanar: ; // 2
      else
        TiffError('expected PlanarConfiguration, but found '+IntToStr(SValue));
      end;
      IFD.PlanarConfiguration:=SValue;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TFPReaderTiff.ReadDirectoryEntry Tag 284: PlanarConfiguration=');
        case SValue of
        TiffPlanarConfigurationChunky: write('chunky format');
        TiffPlanarConfigurationPlanar: write('planar format');
        end;
        writeln;
      end;
      {$endif}
    end;
  285:
    begin
      // PageName
      IFD.PageName:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 285: PageName="'+IFD.PageName+'"');
      {$endif}
    end;
  288:
    begin
      // FreeOffsets
      // The free bytes in a tiff file are described with FreeByteCount and FreeOffsets
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 288: skipping FreeOffsets');
      {$endif}
    end;
  289:
    begin
      // FreeByteCount
      // The free bytes in a tiff file are described with FreeByteCount and FreeOffsets
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 289: skipping FreeByteCount');
      {$endif}
    end;
  290:
    begin
      // GrayResponseUnit
      // precision of GrayResponseCurve
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 290: skipping GrayResponseUnit');
      {$endif}
    end;
  291:
    begin
      // GrayResponseCurve
      // the optical density for each possible pixel value
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 291: skipping GrayResponseCurve');
      {$endif}
    end;
  296:
    begin
      // fResolutionUnit
      UValue:=ReadEntryUnsigned;
      case UValue of
      1: IFD.ResolutionUnit:=1; // none
      2: IFD.ResolutionUnit:=2; // inch
      3: IFD.ResolutionUnit:=3; // centimeter
      else
        TiffError('expected ResolutionUnit, but found '+IntToStr(UValue));
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        write('TFPReaderTiff.ReadDirectoryEntry Tag 296: ResolutionUnit=');
        case IFD.ResolutionUnit of
        1: write('none');
        2: write('inch');
        3: write('centimeter');
        end;
        writeln;
      end;
      {$endif}
    end;
  297:
    begin
      // page number (starting at 0) and total number of pages
      UValue:=GetPos;
      ReadShortValues(UValue,WordBuffer,Count);
      try
        if Count<>2 then begin
          {$ifdef FPC_Debug_Image}
          if Debug then begin
            write('TFPReaderTiff.ReadDirectoryEntry Tag 297: PageNumber/Count: ');
            for i:=0 to Count-1 do
              write(IntToStr(WordBuffer[i]),' ');
            writeln;
          end;
          {$endif}
          TiffError('PageNumber Count=2 expected, but found '+IntToStr(Count));
        end;
        IFD.PageNumber:=WordBuffer[0];
        IFD.PageCount:=WordBuffer[1];
        if IFD.PageNumber>=IFD.PageCount then begin
          // broken order => repair
          UValue:=IFD.PageNumber;
          IFD.PageNumber:=IFD.PageCount;
          IFD.PageCount:=UValue;
        end;
      finally
        ReAllocMem(WordBuffer,0);
      end;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 297: PageNumber=',IFD.PageNumber,'/',IFD.PageCount);
      end;
      {$endif}
    end;
  305:
    begin
      // Software
      IFD.Software:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 305: Software="',IFD.Software,'"');
      {$endif}
    end;
  306:
    begin
      // DateAndTime
      IFD.DateAndTime:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 306: DateAndTime="',IFD.DateAndTime,'"');
      {$endif}
    end;
  315:
    begin
      // Artist
      IFD.Artist:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 315: Artist="',IFD.Artist,'"');
      {$endif}
    end;
  316:
    begin
      // HostComputer
      IFD.HostComputer:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 316: HostComputer="',IFD.HostComputer,'"');
      {$endif}
    end;
  317:
    begin
      // Predictor
      UValue:=word(ReadEntryUnsigned);
      case UValue of
      1: ;
      2: ;
      else TiffError('expected Predictor, but found '+IntToStr(UValue));
      end;
      IFD.Predictor:=UValue;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 317: Predictor="',IFD.Predictor,'"');
      {$endif}
    end;
  320:
    begin
      // ColorMap: N = 3*2^BitsPerSample
      IFD.ColorMap:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 320: skipping ColorMap');
      {$endif}
    end;
  322:
    begin
      // TileWidth
      IFD.TileWidth:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 322: TileWidth=',IFD.TileWidth);
      {$endif}
      if IFD.TileWidth=0 then
        TiffError('TileWidth=0');
    end;
  323:
    begin
      // TileLength = TileHeight
      IFD.TileLength:=ReadEntryUnsigned;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 323: TileLength=',IFD.TileLength);
      {$endif}
      if IFD.TileLength=0 then
        TiffError('TileLength=0');
    end;
  324:
    begin
      // TileOffsets
      IFD.TileOffsets:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 324: TileOffsets=',IFD.TileOffsets);
      {$endif}
      if IFD.TileOffsets=0 then
        TiffError('TileOffsets=0');
    end;
  325:
    begin
      // TileByteCounts
      IFD.TileByteCounts:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 325: TileByteCounts=',IFD.TileByteCounts);
      {$endif}
      if IFD.TileByteCounts=0 then
        TiffError('TileByteCounts=0');
    end;
  338:
    begin
      // ExtraSamples: if SamplesPerPixel is bigger than PhotometricInterpretation
      // then ExtraSamples is an array defining the extra samples
      // 0=unspecified
      // 1=alpha (premultiplied)
      // 2=alpha (unassociated)
      IFD.ExtraSamples:=GetPos;
      {$ifdef FPC_Debug_Image}
      if Debug then begin
        ReadShortValues(IFD.ExtraSamples,WordBuffer,Count);
        write('TFPReaderTiff.ReadDirectoryEntry Tag 338: ExtraSamples: ');
        for i:=0 to Count-1 do
          write(IntToStr(WordBuffer[i]),' ');
        writeln;
        ReAllocMem(WordBuffer,0);
      end;
      {$endif}
    end;
  347:
    begin
      // ToDo: JPEGTables
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 347: skipping JPEG Tables');
      {$endif}
    end;
  512:
    begin
      // ToDo: JPEGProc
      // short
      // 1 = baseline sequential
      // 14 = lossless process with Huffman encoding
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 512: skipping JPEGProc');
      {$endif}
    end;
  513:
    begin
      // ToDo: JPEGInterchangeFormat
      // long
      // non zero: start of start of image SOI marker
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 513: skipping JPEGInterchangeFormat');
      {$endif}
    end;
  514:
    begin
      // ToDo: JPEGInterchangeFormatLength
      // long
      // length in bytes of 513
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 514: skipping JPEGInterchangeFormatLength');
      {$endif}
    end;
  515:
    begin
      // ToDo: JPEGRestartInterval
      // short
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 515: skipping JPEGRestartInterval');
      {$endif}
    end;
  517:
    begin
      // ToDo: JPEGLosslessPredictor
      // short
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 517: skipping JPEGLosslessPredictor');
      {$endif}
    end;
  518:
    begin
      // ToDo: JPEGPointTransforms
      // short
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 518: skipping JPEGPointTransforms');
      {$endif}
    end;
  519:
    begin
      // ToDo: JPEGQTables
      // long
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 519: skipping JPEGQTables');
      {$endif}
    end;
  520:
    begin
      // ToDo: JPEGDCTables
      // long
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 520: skipping JPEGDCTables');
      {$endif}
    end;
  521:
    begin
      // ToDo: JPEGACTables
      // long
      // Count: SamplesPerPixels
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 521: skipping JPEGACTables');
      {$endif}
    end;
  530:
    begin
      // ToDo: YCbCrSubSampling alias ChromaSubSampling
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 530: skipping YCbCrSubSampling alias ChromaSubSampling');
      {$endif}
    end;
  700:
    begin
      // ToDo: XMP
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 700: skipping XMP');
      {$endif}
    end;
  33432:
    begin
      // Copyright
      IFD.Copyright:=ReadEntryString;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 33432: Copyright="',IFD.Copyright,'"');
      {$endif}
    end;
  34675:
    begin
      // ToDo: ICC Profile
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag 34675: skipping ICC profile');
      {$endif}
    end;
  else
    begin
      EntryType:=ReadWord;
      EntryCount:=ReadDWord;
      EntryStart:=ReadDWord;
      if (EntryType=0) and (EntryCount=0) and (EntryStart=0) then ;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.ReadDirectoryEntry Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart);
      {$endif}
    end;
  end;
end;

function TFPReaderTiff.ReadEntryUnsigned: DWord;
var
  EntryCount: LongWord;
  EntryType: Word;
begin
  Result:=0;
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount<>1 then
    TiffError('EntryCount=1 expected, but found '+IntToStr(EntryCount));
  //writeln('TFPReaderTiff.ReadEntryUnsigned Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart]);
  case EntryType of
  1: begin
      // byte: 8bit unsigned
      Result:=ReadByte;
    end;
  3: begin
      // short: 16bit unsigned
      Result:=ReadWord;
    end;
  4: begin
      // long: 32bit unsigned long
      Result:=ReadDWord;
    end;
  else
    TiffError('expected single unsigned value, but found type='+IntToStr(EntryType));
  end;
end;

function TFPReaderTiff.ReadEntrySigned: Cint32;
var
  EntryCount: LongWord;
  EntryType: Word;
begin
  Result:=0;
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount<>1 then
    TiffError('EntryCount+1 expected, but found '+IntToStr(EntryCount));
  //writeln('TFPReaderTiff.ReadEntrySigned Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart]);
  case EntryType of
  1: begin
      // byte: 8bit unsigned
      Result:=cint8(ReadByte);
    end;
  3: begin
      // short: 16bit unsigned
      Result:=cint16(ReadWord);
    end;
  4: begin
      // long: 32bit unsigned long
      Result:=cint32(ReadDWord);
    end;
  6: begin
      // sbyte: 8bit signed
      Result:=cint8(ReadByte);
    end;
  8: begin
      // sshort: 16bit signed
      Result:=cint16(ReadWord);
    end;
  9: begin
      // slong: 32bit signed long
      Result:=cint32(ReadDWord);
    end;
  else
    TiffError('expected single signed value, but found type='+IntToStr(EntryType));
  end;
end;

function TFPReaderTiff.ReadEntryRational: TTiffRational;
var
  EntryCount: LongWord;
  EntryStart: LongWord;
  EntryType: Word;
begin
  Result:=TiffRational0;
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount<>1 then
    TiffError('EntryCount+1 expected, but found '+IntToStr(EntryCount));
  //writeln('TFPReaderTiff.ReadEntryUnsigned Tag=',EntryTag,' Type=',EntryType,' Count=',EntryCount,' ValuesStart=',EntryStart]);
  case EntryType of
  1: begin
      // byte: 8bit unsigned
      Result.Numerator:=ReadByte;
    end;
  3: begin
      // short: 16bit unsigned
      Result.Numerator:=ReadWord;
    end;
  4: begin
      // long: 32bit unsigned long
      Result.Numerator:=ReadDWord;
    end;
  5: begin
      // rational: Two longs: numerator + denominator
      // this does not fit into 4 bytes
      EntryStart:=ReadDWord;
      SetStreamPos(EntryStart);
      Result.Numerator:=ReadDWord;
      Result.Denominator:=ReadDWord;
    end;
  else
    TiffError('expected rational unsigned value, but found type='+IntToStr(EntryType));
  end;
end;

function TFPReaderTiff.ReadEntryString: string;
var
  EntryType: Word;
  EntryCount: LongWord;
  EntryStart: LongWord;
begin
  Result:='';
  EntryType:=ReadWord;
  if EntryType<>2 then
    TiffError('asciiz expected, but found '+IntToStr(EntryType));
  EntryCount:=ReadDWord;
  EntryStart:=ReadDWord;
  SetStreamPos(EntryStart);
  SetLength(Result,EntryCount-1);
  if EntryCount>1 then
    s.Read(Result[1],EntryCount-1);
end;

function TFPReaderTiff.ReadByte: Byte;
begin
  Result:=s.ReadByte;
end;

function TFPReaderTiff.ReadWord: Word;
begin
  Result:=FixEndian(s.ReadWord);
end;

function TFPReaderTiff.ReadDWord: DWord;
begin
  Result:=FixEndian(s.ReadDWord);
end;

procedure TFPReaderTiff.ReadValues(StreamPos: DWord;
  out EntryType: word; out EntryCount: DWord;
  out Buffer: Pointer; out ByteCount: PtrUint);
var
  EntryStart: DWord;
begin
  Buffer:=nil;
  ByteCount:=0;
  EntryType:=0;
  EntryCount:=0;
  SetStreamPos(StreamPos);
  ReadWord; // skip tag
  EntryType:=ReadWord;
  EntryCount:=ReadDWord;
  if EntryCount=0 then exit;
  case EntryType of
  1,6,7: ByteCount:=EntryCount; // byte
  2: ByteCount:=EntryCount; // asciiz
  3,8: ByteCount:=2*EntryCount; // short
  4,9: ByteCount:=4*EntryCount; // long
  5,10: ByteCount:=8*EntryCount; // rational
  11: ByteCount:=4*EntryCount; // single
  12: ByteCount:=8*EntryCount; // double
  else
    TiffError('invalid EntryType '+IntToStr(EntryType));
  end;
  if ByteCount>4 then begin
    EntryStart:=ReadDWord;
    SetStreamPos(EntryStart);
  end;
  GetMem(Buffer,ByteCount);
  s.Read(Buffer^,ByteCount);
end;

procedure TFPReaderTiff.ReadShortOrLongValues(StreamPos: DWord; out
  Buffer: PDWord; out Count: DWord);
var
  p: Pointer;
  ByteCount: PtrUInt;
  EntryType: word;
  i: DWord;
begin
  Buffer:=nil;
  Count:=0;
  p:=nil;
  try
    ReadValues(StreamPos,EntryType,Count,p,ByteCount);
    if Count=0 then exit;
    if EntryType=3 then begin
      // short
      GetMem(Buffer,SizeOf(DWord)*Count);
      for i:=0 to Count-1 do
        Buffer[i]:=FixEndian(PWord(p)[i]);
    end else if EntryType=4 then begin
      // long
      Buffer:=p;
      p:=nil;
      if FReverseEndian then
        for i:=0 to Count-1 do
          Buffer[i]:=FixEndian(PDWord(Buffer)[i]);
    end else
      TiffError('only short or long allowed');
  finally
    if p<>nil then FreeMem(p);
  end;
end;

procedure TFPReaderTiff.ReadShortValues(StreamPos: DWord; out Buffer: PWord;
  out Count: DWord);
var
  p: Pointer;
  ByteCount: PtrUInt;
  EntryType: word;
  i: DWord;
begin
  Buffer:=nil;
  Count:=0;
  p:=nil;
  try
    ReadValues(StreamPos,EntryType,Count,p,ByteCount);
    //writeln('ReadShortValues ',FReverseEndian,' ',EntryType,' Count=',Count,' ByteCount=',ByteCount);
    if Count=0 then exit;
    if EntryType=3 then begin
      // short
      Buffer:=p;
      p:=nil;
      if FReverseEndian then
        for i:=0 to Count-1 do
          Buffer[i]:=FixEndian(Buffer[i]);
      //for i:=0 to Count-1 do writeln(i,' ',Buffer[i]);
    end else
      TiffError('only short allowed, but found '+IntToStr(EntryType));
  finally
    if p<>nil then FreeMem(p);
  end;
end;

procedure TFPReaderTiff.LoadImageFromStream(Index: integer);
var
  ChunkOffsets: PDWord;
  ChunkByteCounts: PDWord;
  Chunk: PByte;
  ChunkCount: DWord;
  ChunkIndex: Dword;
  CurCount: DWord;
  CurOffset: DWord;
  CurByteCnt: PtrInt;
  Run: PByte;
  x, y, cx, cy, dx, dy, sx: integer;
  SampleBits: PWord;
  SampleBitsPerPixel: DWord;
  ExtraSamples: PWord;
  ExtraSampleCnt: DWord;
  GrayBits, GrayValue, LastGrayValue: Word;
  RedBits, RedValue, LastRedValue: Word;
  GreenBits, GreenValue, LastGreenValue: Word;
  BlueBits, BlueValue, LastBlueValue: Word;
  AlphaBits, AlphaValue, LastAlphaValue: Word;
  Col: TFPColor;
  i: Integer;
  CurFPImg: TFPCustomImage;
  aContinue: Boolean;
  ExpectedChunkLength: PtrInt;
  ChunkType: TTiffChunkType;
  TilesAcross, TilesDown: DWord;
  ChunkLeft, ChunkTop, ChunkWidth, ChunkHeight: DWord;
  CurImg: TTiffIFD;
  ChunkBytesPerLine: DWord;
begin
  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TFPReaderTiff.LoadImageFromStream Index=',Index);
  {$endif}
  IFD:=Images[Index];

  if IFD.PhotoMetricInterpretation=High(IFD.PhotoMetricInterpretation) then
    TiffError('missing PhotometricInterpretation');
  if IFD.BitsPerSample=0 then
    TiffError('missing BitsPerSample');
  if IFD.TileWidth>0 then begin
    ChunkType:=tctTile;
    if IFD.TileLength=0 then
      TiffError('missing TileLength');
    if IFD.TileOffsets=0 then
      TiffError('missing TileOffsets');
    if IFD.TileByteCounts=0 then
      TiffError('missing TileByteCounts');
  end else begin
    ChunkType:=tctStrip;
    if IFD.RowsPerStrip=0 then
      TiffError('missing RowsPerStrip');
    if IFD.StripOffsets=0 then
      TiffError('missing StripOffsets');
    if IFD.StripByteCounts=0 then
      TiffError('missing StripByteCounts');
  end;

  if (IFD.ImageWidth=0) or (IFD.ImageHeight=0) then
    exit;

  if Index=ImageCount then
    ImageList.Add(TTiffIFD.Create);
  CurImg:=Images[Index];

  {$ifdef FPC_Debug_Image}
  if Debug then
    writeln('TFPReaderTiff.LoadImageFromStream reading ...');
  {$endif}

  ChunkOffsets:=nil;
  ChunkByteCounts:=nil;
  Chunk:=nil;
  ExtraSamples:=nil;
  SampleBits:=nil;
  ExtraSampleCnt:=0;
  try
    // read chunk starts and sizes
    if ChunkType=tctTile then begin
      TilesAcross:=(IFD.ImageWidth+IFD.TileWidth-1) div IFD.TileWidth;
      TilesDown:=(IFD.ImageHeight+IFD.TileLength-1) div IFD.TileLength;
      {$ifdef FPC_Debug_Image}
      if Debug then
        writeln('TFPReaderTiff.LoadImageFromStream TilesAcross=',TilesAcross,' TilesDown=',TilesDown);
      {$endif}
      ChunkCount := TilesAcross * TilesDown;
      ReadShortOrLongValues(IFD.TileOffsets,ChunkOffsets,CurCount);
      if CurCount<>ChunkCount then
        TiffError('number of TileCounts is wrong');
      ReadShortOrLongValues(IFD.TileByteCounts,ChunkByteCounts,CurCount);
      if CurCount<>ChunkCount then
        TiffError('number of TileByteCounts is wrong');
    end else begin //strip
      ChunkCount:=((IFD.ImageHeight-1) div IFD.RowsPerStrip)+1;
      ReadShortOrLongValues(IFD.StripOffsets,ChunkOffsets,CurCount);
      if CurCount<>ChunkCount then
        TiffError('number of StripCounts is wrong');
      ReadShortOrLongValues(IFD.StripByteCounts,ChunkByteCounts,CurCount);
      if CurCount<>ChunkCount then
        TiffError('number of StripByteCounts is wrong');
    end;

    // read image structure
    ReadImageProperties(RedBits, GreenBits, BlueBits, GrayBits, AlphaBits,
      ExtraSamples, ExtraSampleCnt, SampleBits, SampleBitsPerPixel);
    CurImg.Assign(IFD);

    // create FPimage
    DoCreateImage(CurImg);
    CurFPImg:=CurImg.Img;
    if CurFPImg=nil then exit;

    SetFPImgExtras(CurFPImg);

    case IFD.Orientation of
    0,1..4: CurFPImg.SetSize(IFD.ImageWidth,IFD.ImageHeight);
    5..8: CurFPImg.SetSize(IFD.ImageHeight,IFD.ImageWidth);
    end;

    {$ifdef FPC_Debug_Image}
    if Debug then
      writeln('TFPReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel);
    {$endif}

    // read chunks
    for ChunkIndex:=0 to ChunkCount-1 do begin
      CurOffset:=ChunkOffsets[ChunkIndex];
      CurByteCnt:=ChunkByteCounts[ChunkIndex];
      //writeln('TFPReaderTiff.LoadImageFromStream CurOffset=',CurOffset,' CurByteCnt=',CurByteCnt);
      if CurByteCnt<=0 then continue;
      ReAllocMem(Chunk,CurByteCnt);
      SetStreamPos(CurOffset);
      s.Read(Chunk^,CurByteCnt);

      // decompress
      if ChunkType=tctTile then
        ExpectedChunkLength:=(SampleBitsPerPixel*IFD.TileWidth+7) div 8*IFD.TileLength
      else
        ExpectedChunkLength:=((SampleBitsPerPixel*IFD.ImageWidth+7) div 8)*IFD.RowsPerStrip;
      case IFD.Compression of
      TiffCompressionNone: ;
      TiffCompressionPackBits: DecodePackBits(Chunk,CurByteCnt);
      TiffCompressionLZW: DecodeLZW(Chunk,CurByteCnt);
      TiffCompressionDeflateAdobe,
      TiffCompressionDeflateZLib: DecodeDeflate(Chunk,CurByteCnt,ExpectedChunkLength);
      else
        TiffError('compression '+TiffCompressionName(IFD.Compression)+' not supported yet');
      end;
      if CurByteCnt<=0 then continue;

      // compute current chunk area
      if ChunkType=tctTile then begin
        ChunkLeft:=(ChunkIndex mod TilesAcross)*IFD.TileWidth;
        ChunkTop:=(ChunkIndex div TilesAcross)*IFD.TileLength;
        ChunkWidth:=Min(IFD.TileWidth,IFD.ImageWidth-ChunkLeft);
        ChunkHeight:=Min(IFD.TileLength,IFD.ImageHeight-ChunkTop);
        ChunkBytesPerLine:=(SampleBitsPerPixel*ChunkWidth+7) div 8;
        ExpectedChunkLength:=ChunkBytesPerLine*ChunkHeight;
        if CurByteCnt<ExpectedChunkLength then begin
          //writeln('TFPReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' y=',y,' IFD.TileWidth=',IFD.TileWidth,' IFD.TileLength=',IFD.TileLength,' ExpectedChunkLength=',ExpectedChunkLength,' CurByteCnt=',CurByteCnt);
          TiffError('TFPReaderTiff.LoadImageFromStream Tile too short ByteCnt='+IntToStr(CurByteCnt)+' ChunkWidth='+IntToStr(ChunkWidth)+' ChunkHeight='+IntToStr(ChunkHeight)+' expected='+IntToStr(ExpectedChunkLength));
        end else if CurByteCnt>ExpectedChunkLength then begin
          // boundary tiles have padding
          ChunkBytesPerLine:=(SampleBitsPerPixel*IFD.TileWidth+7) div 8;
        end;
      end else begin //tctStrip
        ChunkLeft:=0;
        ChunkTop:=IFD.RowsPerStrip*ChunkIndex;
        ChunkWidth:=IFD.ImageWidth;
        ChunkHeight:=Min(IFD.RowsPerStrip,IFD.ImageHeight-ChunkTop);
        ChunkBytesPerLine:=(SampleBitsPerPixel*ChunkWidth+7) div 8;
        ExpectedChunkLength:=ChunkBytesPerLine*ChunkHeight;
        //writeln('TFPReaderTiff.LoadImageFromStream SampleBitsPerPixel=',SampleBitsPerPixel,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' y=',y,' IFD.RowsPerStrip=',IFD.RowsPerStrip,' ExpectedChunkLength=',ExpectedChunkLength,' CurByteCnt=',CurByteCnt);
        if CurByteCnt<ExpectedChunkLength then
          TiffError('TFPReaderTiff.LoadImageFromStream Strip too short ByteCnt='+IntToStr(CurByteCnt)+' ChunkWidth='+IntToStr(ChunkWidth)+' ChunkHeight='+IntToStr(ChunkHeight)+' expected='+IntToStr(ExpectedChunkLength));
      end;

      // progress
      aContinue:=true;
      Progress(psRunning, 0, false, Rect(0,0,IFD.ImageWidth,ChunkTop), '', aContinue);
      if not aContinue then break;

      // Orientation
      if IFD.Orientation in [1..4] then begin
        x:=ChunkLeft; y:=ChunkTop;
        case IFD.Orientation of
        1: begin dx:=1; dy:=1; end;// 0,0 is left, top
        2: begin x:=IFD.ImageWidth-x-1; dx:=-1; dy:=1; end;// 0,0 is right, top
        3: begin x:=IFD.ImageWidth-x-1; dx:=-1; y:=IFD.ImageHeight-y-1; dy:=-1; end;// 0,0 is right, bottom
        4: begin dx:=1; y:=IFD.ImageHeight-y-1; dy:=-1; end;// 0,0 is left, bottom
        end;
      end else begin
        // rotated
        x:=ChunkTop; y:=ChunkLeft;
        case IFD.Orientation of
        5: begin dx:=1; dy:=1; end;// 0,0 is top, left (rotated)
        6: begin dx:=1; y:=IFD.ImageWidth-y-1; dy:=-1; end;// 0,0 is top, right (rotated)
        7: begin x:=IFD.ImageHeight-x-1; dx:=-1; y:=IFD.ImageWidth-y-1; dy:=-1; end;// 0,0 is bottom, right (rotated)
        8: begin x:=IFD.ImageHeight-x-1; dx:=-1; dy:=1; end;// 0,0 is bottom, left (rotated)
        end;
      end;

      //writeln('TFPReaderTiff.LoadImageFromStream Chunk ',ChunkIndex,' ChunkLeft=',ChunkLeft,' ChunkTop=',ChunkTop,' IFD.ImageWidth=',IFD.ImageWidth,' IFD.ImageHeight=',IFD.ImageHeight,' ChunkWidth=',ChunkWidth,' ChunkHeight=',ChunkHeight,' PaddingRight=',PaddingRight);
      sx:=x;
      for cy:=0 to ChunkHeight-1 do begin
        //writeln('TFPReaderTiff.LoadImageFromStream y=',y);
        Run:=Chunk+ChunkBytesPerLine*cy;
        LastRedValue:=0;
        LastGreenValue:=0;
        LastBlueValue:=0;
        LastGrayValue:=0;
        LastAlphaValue:=0;
        x:=sx;
        for cx:=0 to ChunkWidth-1 do begin
          case IFD.PhotoMetricInterpretation of
          0,1: // 0:bilevel grayscale 0 is white; 1:0 is black
            begin
              ReadImgValue(GrayBits,Run,cx,IFD.Predictor,LastGrayValue,GrayValue);
              if IFD.PhotoMetricInterpretation=0 then
                GrayValue:=$ffff-GrayValue;
              AlphaValue:=alphaOpaque;
              for i:=0 to ExtraSampleCnt-1 do begin
                if ExtraSamples[i] in [1,2] then begin
                  ReadImgValue(AlphaBits,Run,cx,IFD.Predictor,LastAlphaValue,AlphaValue);
                end else begin
                  inc(Run,ExtraSamples[i] div 8);
                end;
              end;
              Col:=FPColor(GrayValue,GrayValue,GrayValue,AlphaValue);
            end;

          2: // RGB(A)
            begin
              ReadImgValue(RedBits,Run,cx,IFD.Predictor,LastRedValue,RedValue);
              ReadImgValue(GreenBits,Run,cx,IFD.Predictor,LastGreenValue,GreenValue);
              ReadImgValue(BlueBits,Run,cx,IFD.Predictor,LastBlueValue,BlueValue);
              AlphaValue:=alphaOpaque;
              for i:=0 to ExtraSampleCnt-1 do begin
                if ExtraSamples[i] in [1,2] then begin
                  ReadImgValue(AlphaBits,Run,cx,IFD.Predictor,LastAlphaValue,AlphaValue);
                end else begin
                  inc(Run,ExtraSamples[i] div 8);
                end;
              end;
              Col:=FPColor(RedValue,GreenValue,BlueValue,AlphaValue);
            end;

          5: // CMYK plus optional alpha
            begin
              ReadImgValue(RedBits,Run,cx,IFD.Predictor,LastRedValue,RedValue);
              ReadImgValue(GreenBits,Run,cx,IFD.Predictor,LastGreenValue,GreenValue);
              ReadImgValue(BlueBits,Run,cx,IFD.Predictor,LastBlueValue,BlueValue);
              ReadImgValue(GrayBits,Run,cx,IFD.Predictor,LastGrayValue,GrayValue);
              AlphaValue:=alphaOpaque;
              for i:=0 to ExtraSampleCnt-1 do begin
                if ExtraSamples[i] in [1,2] then begin
                  ReadImgValue(AlphaBits,Run,cx,IFD.Predictor,LastAlphaValue,AlphaValue);
                end else begin
                  inc(Run,ExtraSamples[i] div 8);
                end;
              end;
              // CMYK to RGB
              Col:=CMYKToFPColor(RedValue,GreenValue,BlueValue,GrayValue);
            end;

          else
            TiffError('PhotometricInterpretation='+IntToStr(IFD.PhotoMetricInterpretation)+' not supported');
          end;

          CurFPImg.Colors[x,y]:=Col;
          // next column
          inc(x,dx);
        end;

        // next line
        inc(y,dy);
      end;
      // next chunk
    end;
  finally
    ReAllocMem(ExtraSamples,0);
    ReAllocMem(SampleBits,0);
    ReAllocMem(ChunkOffsets,0);
    ReAllocMem(ChunkByteCounts,0);
    ReAllocMem(Chunk,0);
  end;
end;

function TFPReaderTiff.FixEndian(w: Word): Word; inline;
begin
  Result:=w;
  if FReverseEndian then
    Result:=((Result and $ff) shl 8) or (Result shr 8);
end;

function TFPReaderTiff.FixEndian(d: DWord): DWord; inline;
begin
  Result:=d;
  if FReverseEndian then
    Result:=((Result and $ff) shl 24)
          or ((Result and $ff00) shl 8)
          or ((Result and $ff0000) shr 8)
          or (Result shr 24);
end;

procedure TFPReaderTiff.DecodePackBits(var Buffer: Pointer; var Count: PtrInt);
var
  NewBuffer: Pointer;
  NewCount: PtrInt;
begin
  DecompressPackBits(Buffer,Count,NewBuffer,NewCount);
  FreeMem(Buffer);
  Buffer:=NewBuffer;
  Count:=NewCount;
end;

procedure TFPReaderTiff.DecodeLZW(var Buffer: Pointer; var Count: PtrInt);
var
  NewBuffer: Pointer;
  NewCount: PtrInt;
begin
  DecompressLZW(Buffer,Count,NewBuffer,NewCount);
  FreeMem(Buffer);
  Buffer:=NewBuffer;
  Count:=NewCount;
end;

procedure TFPReaderTiff.DecodeDeflate(var Buffer: Pointer; var Count: PtrInt;
  ExpectedCount: PtrInt);
var
  NewBuffer: PByte;
  NewCount: cardinal;
  ErrorMsg: String;
begin
  ErrorMsg:='';
  NewBuffer:=nil;
  try
    NewCount:=ExpectedCount;
    if not DecompressDeflate(Buffer,Count,NewBuffer,NewCount,@ErrorMsg) then
      TiffError(ErrorMsg);
    FreeMem(Buffer);
    Buffer:=NewBuffer;
    Count:=NewCount;
    NewBuffer:=nil;
  finally
    ReAllocMem(NewBuffer,0);
  end;
end;

procedure TFPReaderTiff.InternalRead(Str: TStream; AnImage: TFPCustomImage);
// read the biggest image
var
  Img: TTiffIFD;
  aContinue: Boolean;
  BestSize: PtrInt;
  NewSize: PtrInt;
  Best: integer;
  CurImg: TTiffIFD;
  i: Integer;
begin
  Clear;
  // read header
  aContinue:=true;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', aContinue);
  if not aContinue then exit;
  LoadHeaderFromStream(Str);
  LoadIFDsFromStream;
  // find the biggest image
  BestSize:=-1;
  Best:=-1;
  for i:=0 to ImageCount-1 do begin
    CurImg:=Images[i];
    NewSize:=Int64(CurImg.ImageWidth)*CurImg.ImageHeight;
    if (NewSize<BestSize) then continue;
    BestSize:=NewSize;
    Best:=i;
  end;
  Progress(psRunning, 0, False, Rect(0,0,0,0), '', aContinue);
  // read image
  if Best>=0 then begin
    Img:=Images[Best];
    Img.Img:=AnImage;
    LoadImageFromStream(Best);
  end;
  // end
  Progress(psEnding, 100, False, Rect(0,0,0,0), '', aContinue);
end;

function TFPReaderTiff.InternalCheck(Str: TStream): boolean;
var
  IFDStart: DWord;
begin
  try
    s:=Str;
    fStartPos:=s.Position;
    Result:=ReadTiffHeader(true,IFDStart) and (IFDStart<>0);
    s.Position:=fStartPos;
  except
    Result:=false;
  end;
end;

procedure TFPReaderTiff.DoCreateImage(ImgFileDir: TTiffIFD);
begin
  if Assigned(OnCreateImage) then
    OnCreateImage(Self,ImgFileDir);
end;

constructor TFPReaderTiff.Create;
begin
  ImageList:=TFPList.Create;
end;

destructor TFPReaderTiff.Destroy;
begin
  Clear;
  FreeAndNil(ImageList);
  inherited Destroy;
end;

procedure TFPReaderTiff.Clear;
var
  i: Integer;
  Img: TTiffIFD;
begin
  for i:=ImageCount-1 downto 0 do begin
    Img:=Images[i];
    ImageList.Delete(i);
    if IFD=Img then IFD:=nil;
    Img.Free;
  end;
  FReverseEndian:=false;
  FreeAndNil(fIFDStarts);
end;

procedure DecompressPackBits(Buffer: Pointer; Count: PtrInt; out
  NewBuffer: Pointer; out NewCount: PtrInt);
{ Algorithm:
    while not got the expected number of bytes
      read one byte n
      if n in 0..127 copy the next n+1 bytes
      else if n in -127..-1 then copy the next byte 1-n times
      else continue
    end
}
var
  p: Pcint8;
  n: cint8;
  d: pcint8;
  i,j: integer;
  EndP: Pcint8;
begin
  // compute NewCount
  NewCount:=0;
  NewBuffer:=nil;
  if Count=0 then exit;
  p:=Pcint8(Buffer);
  EndP:=p+Count;
  while p<EndP do begin
    n:=p^;
    case n of
    0..127:   begin inc(NewCount,n+1);  inc(p,n+2); end; // copy the next n+1 bytes
    -127..-1: begin inc(NewCount,1-n); inc(p,2);   end; // copy the next byte 1-n times
    else inc(p); // noop
    end;
  end;

  // decompress
  if NewCount=0 then exit;
  GetMem(NewBuffer,NewCount);
  p:=Pcint8(Buffer);
  d:=Pcint8(NewBuffer);
  while p<EndP do begin
    n:=p^;
    case n of
    0..127:
      begin
        // copy the next n+1 bytes
        i:=n+1;
        inc(NewCount,i);
        inc(p);
        System.Move(p^,d^,i);
        inc(p,i);
        inc(d,i);
      end;
    -127..-1:
      begin
        // copy the next byte 1-n times
        i:=1-n;
        inc(NewCount,i);
        inc(p);
        n:=p^;
        for j:=0 to i-1 do
          d[j]:=n;
        inc(d,i);
        inc(p);
      end;
    else inc(p); // noop
    end;
  end;
end;

procedure DecompressLZW(Buffer: Pointer; Count: PtrInt; out NewBuffer: PByte;
  out NewCount: PtrInt);
type
  TLZWString = packed record
    Count: integer;
    Data: PByte;
  end;
  PLZWString = ^TLZWString;
const
  ClearCode = 256; // clear table, start with 9bit codes
  EoiCode = 257; // end of input
var
  NewCapacity: PtrInt;
  SrcPos: PtrInt;
  SrcPosBit: integer;
  CurBitLength: integer;
  Code: Word;
  Table: PLZWString;
  TableCapacity: integer;
  TableCount: integer;
  OldCode: Word;

  procedure Error(const Msg: string);
  begin
    raise Exception.Create(Msg);
  end;

  function GetNextCode: Word;
  var
    v: Integer;
  begin
    Result:=0;
    // CurBitLength can be 9 to 12
    //writeln('GetNextCode CurBitLength=',CurBitLength,' SrcPos=',SrcPos,' SrcPosBit=',SrcPosBit,' ',hexstr(PByte(Buffer)[SrcPos],2),' ',hexstr(PByte(Buffer)[SrcPos+1],2),' ',hexstr(PByte(Buffer)[SrcPos+2],2));
    // read two or three bytes
    if CurBitLength+SrcPosBit>16 then begin
      // read from three bytes
      if SrcPos+3>Count then Error('LZW stream overrun');
      v:=PByte(Buffer)[SrcPos];
      inc(SrcPos);
      v:=(v shl 8)+PByte(Buffer)[SrcPos];
      inc(SrcPos);
      v:=(v shl 8)+PByte(Buffer)[SrcPos];
      v:=v shr (24-CurBitLength-SrcPosBit);
    end else begin
      // read from two bytes
      if SrcPos+2>Count then Error('LZW stream overrun');
      v:=PByte(Buffer)[SrcPos];
      inc(SrcPos);
      v:=(v shl 8)+PByte(Buffer)[SrcPos];
      if CurBitLength+SrcPosBit=16 then
        inc(SrcPos);
      v:=v shr (16-CurBitLength-SrcPosBit);
    end;
    Result:=v and ((1 shl CurBitLength)-1);
    SrcPosBit:=(SrcPosBit+CurBitLength) and 7;
    //writeln('GetNextCode END SrcPos=',SrcPos,' SrcPosBit=',SrcPosBit,' Result=',Result,' Result=',hexstr(Result,4));
  end;

  procedure ClearTable;
  var
    i: Integer;
  begin
    for i:=0 to TableCount-1 do
      ReAllocMem(Table[i].Data,0);
    TableCount:=0;
  end;

  procedure InitializeTable;
  begin
    CurBitLength:=9;
    ClearTable;
  end;

  function IsInTable(Code: word): boolean;
  begin
    Result:=Code<258+TableCount;
  end;

  procedure WriteStringFromCode(Code: integer; AddFirstChar: boolean = false);
  var
    s: TLZWString;
    b: byte;
  begin
    //WriteLn('WriteStringFromCode Code=',Code,' AddFirstChar=',AddFirstChar,' x=',(NewCount div 4) mod IFD.ImageWidth,' y=',(NewCount div 4) div IFD.ImageWidth,' PixelByte=',NewCount mod 4);
    if Code<256 then begin
      // write byte
      b:=Code;
      s.Data:=@b;
      s.Count:=1;
    end else if Code>=258 then begin
      // write string
      if Code-258>=TableCount then
        Error('LZW code out of bounds');
      s:=Table[Code-258];
    end else
      Error('LZW code out of bounds');
    if NewCount+s.Count+1>NewCapacity then begin
      NewCapacity:=NewCapacity*2+8;
      ReAllocMem(NewBuffer,NewCapacity);
    end;
    System.Move(s.Data^,NewBuffer[NewCount],s.Count);
    //for i:=0 to s.Count-1 do write(HexStr(NewBuffer[NewCount+i],2)); // debug
    inc(NewCount,s.Count);
    if AddFirstChar then begin
      NewBuffer[NewCount]:=s.Data^;
      //write(HexStr(NewBuffer[NewCount],2)); // debug
      inc(NewCount);
    end;
    //writeln(',WriteStringFromCode'); // debug
  end;

  procedure AddStringToTable(Code, AddFirstCharFromCode: integer);
  // add string from code plus first character of string from code as new string
  var
    b1, b2: byte;
    s1, s2: TLZWString;
    p: PByte;
  begin
    //WriteLn('AddStringToTable Code=',Code,' FCFCode=',AddFirstCharFromCode,' TableCount=',TableCount,' TableCapacity=',TableCapacity);
    if TableCount=4096-259 then
      Error('LZW too many codes');
    // grow table
    if TableCount>=TableCapacity then begin
      TableCapacity:=TableCapacity*2+128;
      ReAllocMem(Table,TableCapacity*SizeOf(TLZWString));
    end;
    // find string 1
    if Code<256 then begin
      // string is byte
      b1:=Code;
      s1.Data:=@b1;
      s1.Count:=1;
    end else if Code>=258 then begin
      // normal string
      if Code-258>=TableCount then
        Error('LZW code out of bounds');
      s1:=Table[Code-258];
    end else
      Error('LZW code out of bounds');
    // find string 2
    if AddFirstCharFromCode<256 then begin
      // string is byte
      b2:=AddFirstCharFromCode;
      s2.Data:=@b2;
      s2.Count:=1;
    end else begin
      // normal string
      if AddFirstCharFromCode-258>=TableCount then
        Error('LZW code out of bounds');
      s2:=Table[AddFirstCharFromCode-258];
    end;
    // set new table entry
    Table[TableCount].Count:=s1.Count+1;
    p:=nil;
    GetMem(p,s1.Count+1);
    Table[TableCount].Data:=p;
    System.Move(s1.Data^,p^,s1.Count);
    // add first character from string 2
    p[s1.Count]:=s2.Data^;
    // increase TableCount
    inc(TableCount);
    case TableCount+259 of
    512,1024,2048: inc(CurBitLength);
    end;
  end;

begin
  NewBuffer:=nil;
  NewCount:=0;
  if Count=0 then exit;
  //WriteLn('DecompressLZW START Count=',Count);
  //for SrcPos:=0 to 19 do
  //  write(HexStr(PByte(Buffer)[SrcPos],2));
  //writeln();

  NewCapacity:=Count*2;
  ReAllocMem(NewBuffer,NewCapacity);

  SrcPos:=0;
  SrcPosBit:=0;
  CurBitLength:=9;
  Table:=nil;
  TableCount:=0;
  TableCapacity:=0;
  try
    repeat
      Code:=GetNextCode;
      //WriteLn('DecompressLZW Code=',Code);
      if Code=EoiCode then break;
      if Code=ClearCode then begin
        InitializeTable;
        Code:=GetNextCode;
        //WriteLn('DecompressLZW after clear Code=',Code);
        if Code=EoiCode then break;
        if Code=ClearCode then
          Error('LZW code out of bounds');
        WriteStringFromCode(Code);
        OldCode:=Code;
      end else begin
        if Code<TableCount+258 then begin
          WriteStringFromCode(Code);
          AddStringToTable(OldCode,Code);
          OldCode:=Code;
        end else if Code=TableCount+258 then begin
          WriteStringFromCode(OldCode,true);
          AddStringToTable(OldCode,OldCode);
          OldCode:=Code;
        end else
          Error('LZW code out of bounds');
      end;
    until false;
  finally
    ClearTable;
    ReAllocMem(Table,0);
  end;

  ReAllocMem(NewBuffer,NewCount);
end;

function DecompressDeflate(Compressed: PByte; CompressedCount: cardinal;
  out Decompressed: PByte; var DecompressedCount: cardinal;
  ErrorMsg: PAnsiString = nil): boolean;
var
  stream : z_stream;
  err : integer;
begin
  Result:=false;
  //writeln('DecompressDeflate START');
  Decompressed:=nil;
  if CompressedCount=0 then begin
    DecompressedCount:=0;
    exit;
  end;

  err := inflateInit(stream{%H-});
  if err <> Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='inflateInit failed';
    exit;
  end;

  // set input = compressed data
  stream.avail_in := CompressedCount;
  stream.next_in  := Compressed;

  // set output = decompressed data
  if DecompressedCount=0 then
    DecompressedCount:=CompressedCount;
  Getmem(Decompressed,DecompressedCount);
  stream.avail_out := DecompressedCount;
  stream.next_out := Decompressed;

  // Finish the stream
  while TRUE do begin
    //writeln('run: total_in=',stream.total_in,' avail_in=',stream.avail_in,' total_out=',stream.total_out,' avail_out=',stream.avail_out);
    if (stream.avail_out=0) then begin
      // need more space
      if DecompressedCount<128 then
        DecompressedCount:=DecompressedCount+128
      else if DecompressedCount>High(DecompressedCount)-1024 then begin
        if ErrorMsg<>nil then
          ErrorMsg^:='inflate decompression failed, because not enough space';
        exit;
      end else
        DecompressedCount:=DecompressedCount*2;
      ReAllocMem(Decompressed,DecompressedCount);
      stream.next_out:=Decompressed+stream.total_out;
      stream.avail_out:=DecompressedCount-stream.total_out;
    end;
    err := inflate(stream, Z_NO_FLUSH);
    if err = Z_STREAM_END then
      break;
    if err<>Z_OK then begin
      if ErrorMsg<>nil then
        ErrorMsg^:='inflate finish failed';
      exit;
    end;
  end;

  //writeln('decompressed: total_in=',stream.total_in,' total_out=',stream.total_out);
  DecompressedCount:=stream.total_out;
  ReAllocMem(Decompressed,DecompressedCount);

  err := inflateEnd(stream);
  if err<>Z_OK then begin
    if ErrorMsg<>nil then
      ErrorMsg^:='inflateEnd failed';
    exit;
  end;
  Result:=true;
end;

initialization
  if ImageHandlers.ImageReader[TiffHandlerName]=nil then
    ImageHandlers.RegisterImageReader (TiffHandlerName, 'tif;tiff', TFPReaderTiff);
end.


{
  Copyright (C) 2009 Laurent Jacques

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

  Version 1.4
}
unit GifAnim;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLProc, Lresources, SysUtils, Controls, Graphics, ExtCtrls,
  IntfGraphics, FPimage, Contnrs, GraphType, dialogs, types;

const

  EXT_INTRODUCER  = $21;
  EXT_GRAPHICS_CONTROL = $F9;
  EXT_PLAIN_TEXT  = $01;
  EXT_APPLICATION = $FF;
  EXT_COMMENT     = $FE;

  DSC_LOCAL_IMAGE = $2C;

  ID_TRANSPARENT = $01;
  ID_COLOR_TABLE_SIZE = $07;
  ID_SORT    = $20;
  ID_INTERLACED = $40;
  ID_COLOR_TABLE = $80;
  ID_IMAGE_DESCRIPTOR = $2C;
  ID_TRAILER = $3B;

  CODE_TABLE_SIZE = 4096;

type
  TRGB = packed record
    Red, Green, Blue: byte;
  end;

  TGIFHeader = packed record
    Signature:    array[0..2] of char;  //* Header Signature (always "GIF") */
    Version:      array[0..2] of char;  //* GIF format version("87a" or "89a") */
    ScreenWidth:  word;                 //* Width of Display Screen in Pixels */
    ScreenHeight: word;                 //* Height of Display Screen in Pixels */
    Packedbit,                          //* Screen and Color Map Information */
    BackgroundColor,                    //* Background Color Index */
    AspectRatio:  byte;                 //* Pixel Aspect Ratio */
  end;

  TGifImageDescriptor = packed record
    Left,                 //* X position of image on the display */
    Top,                  //* Y position of image on the display */
    Width,                //* Width of the image in pixels */
    Height:    word;      //* Height of the image in pixels */
    Packedbit: byte;      //* Image and Color Table Data Information */
  end;

  TGifGraphicsControlExtension = packed record
    BlockSize,          //* Size of remaining fields (always 04h) */
    Packedbit:  byte;   //* Method of graphics disposal to use */
    DelayTime:  word;   //* Hundredths of seconds to wait  */
    ColorIndex,         //* Transparent Color Index */
    Terminator: byte;   //* Block Terminator (always 0) */
  end;

  TGifAnim = class;

  { TGifImage }

  TGifImage = class
  private
    FBitmap: TBitmap;
    FPosX:   word;
    FPosY:   word;
    FDelay:  word;
    FMethod: byte;
  public
    constructor Create;
    destructor Destroy; override;
    property Bitmap: TBitmap Read FBitmap;
    property Delay: word Read FDelay;
    property Method: byte Read FMethod;
    property PosX: word Read FPosX;
    property PosY: word Read FPosY;
  end;

  { TGifList }

  TGifList = class(TObjectList)
  private
  protected
    function GetItems(Index: integer): TGifImage;
    procedure SetItems(Index: integer; AGifImage: TGifImage);
  public
    function Add(AGifImage: TGifImage): integer;
    function Extract(Item: TGifImage): TGifImage;
    function Remove(AGifImage: TGifImage): integer;
    function IndexOf(AGifImage: TGifImage): integer;
    function First: TGifImage;
    function Last: TGifImage;
    procedure Insert(Index: integer; AGifImage: TGifImage);
    property Items[Index: integer]: TGifImage Read GetItems Write SetItems; default;
  end;


  { TGifLoader }

  TGifLoader = class
  private
    FGifHeader:  TGIFHeader;
    FGifDescriptor: TGifImageDescriptor;
    FGifGraphicsCtrlExt: TGifGraphicsControlExtension;
    FGifUseGraphCtrlExt: boolean;
    FGifBackgroundColor: byte;
    FInterlaced: boolean;
    FScanLine:   PByte;
    FLineSize:   integer;
    FDisposalMethod: byte;
    FEmpty:      boolean;
    FFileName:   string;
    FHeight:     integer;
    FIsTransparent: boolean;
    FWidth:      integer;
    FPalette:    TFPPalette;
    FLocalHeight: integer;
    FLocalWidth: integer;
    procedure ReadPalette(Stream: TStream; Size: integer);
    procedure ReadScanLine(Stream: TStream);
    procedure ReadHeader(Stream: TStream);
    procedure ReadGlobalPalette(Stream: TStream);
    procedure ReadGraphCtrlExt;
    procedure SetInterlaced(const AValue: boolean);
    procedure SetTransparent(const AValue: boolean);
    function SkipBlock(Stream: TStream): byte;
    procedure WriteScanLine(Img: TFPCustomImage);
    procedure ReadGifBitmap(Stream: TStream);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function LoadAllBitmap(var AGifList: TGifList): boolean;
    function LoadFromLazarusResource(const ResName: String; var AGifList: TGifList): boolean;
    function LoadFirstBitmap(var ABitmap: TBitmap): boolean;
    property Empty: boolean Read FEmpty;
    property Height: integer Read FHeight;
    property Width: integer Read FWidth;
    property IsTransparent: boolean Read FIsTransparent Write SetTransparent;
    property Interlaced: boolean Read FInterlaced Write SetInterlaced;
  end;

  { TGifAnim }

  TGifAnim = class(TGraphicControl)
  private
    { Private declarations }
    FAnimate:   boolean;
    FEmpty:     boolean;
    FFileName:  string;
    FGifBitmaps: TGifList;
    FOnFrameChanged: TNotifyEvent;
    FOnStart:   TNotifyEvent;
    FOnStop:    TNotifyEvent;
    FWait:      TTimer;
    FCurrentImage: integer;
    FGifHeight: integer;
    FGifWidth:  integer;
    procedure OnTime(Sender: TObject);
    procedure SetAnimate(const AValue: boolean);
    procedure SetFileName(const AValue: string);
    procedure DefineSize(AWidth, AHeight: integer);
  protected
    { Protected declarations }
    BufferImg:   TBitmap;
    CurrentView: TBitmap;
    procedure CalculatePreferredSize(
      var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure DoAutoSize; override;
    procedure DoStartAnim;
    procedure DoStopAnim;
    {
    class function GetControlClassDefaultSize: TSize; override;
    }
    procedure GifChanged;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure Paint; override;
    procedure ResetImage;
    procedure SetColor(Value: TColor); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NextFrame;
    procedure PriorFrame;
    property Empty: boolean Read FEmpty;
    property GifBitmaps: TGifList Read FGifBitmaps;
    property GifIndex: integer Read FCurrentImage;
    function LoadFromLazarusResource(const ResName: String): boolean;
  published
    { Published declarations }
    property Anchors;
    property AutoSize default True;
    property Animate: boolean Read FAnimate Write SetAnimate default True;
    property BorderSpacing;
    property Color default clBtnFace;
    property Constraints;
    property FileName: string Read FFileName Write SetFileName;
    property Height;
    property OnClick;
    property OnDblClick;
    property OnFrameChanged: TNotifyEvent Read FOnFrameChanged Write FOnFrameChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartAnim: TNotifyEvent Read FOnStart Write FOnStart;
    property OnStopAnim: TNotifyEvent Read FOnStop Write FOnStop;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property Width;
  end;

procedure Register;

implementation

uses LazIDEIntf, propedits;
Type
  TGifFileNamePropertyEditor=class(TFileNamePropertyEditor)
  protected
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;
function TGifFileNamePropertyEditor.GetFilter: String;
begin
  Result := 'GIF|*.gif';
end;

function TGifFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

procedure Register;
begin
  RegisterComponents('Wile64', [TGifAnim]);
  RegisterPropertyEditor(TypeInfo(String),
    TGifAnim, 'FileName', TGifFileNamePropertyEditor);
end;

{ TGifAnim }

constructor TGifAnim.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize     := True;
  SetInitialBounds(0, 0, 90{GetControlClassDefaultSize.CX}, 90{GetControlClassDefaultSize.CY});
  FEmpty      := True;
  FCurrentImage := 0;
  CurrentView := TBitmap.Create;
  if not (csDesigning in ComponentState) then
  begin
    BufferImg := TBitmap.Create;
    FWait     := TTimer.Create(Self);
    with FWait do
    begin
      Interval := 100;
      OnTimer  := @OnTime;
      Enabled  := False;
    end;
  end;
  Animate := True;
end;

destructor TGifAnim.Destroy;
begin
  inherited Destroy;
  if assigned(FGifBitmaps) then
    FreeAndNil(FGifBitmaps);
  BufferImg.Free;
  CurrentView.Free;
end;

procedure TGifAnim.NextFrame;
begin
  if (not FEmpty) and Visible and (not FAnimate) then
  begin
    Dec(FCurrentImage);
    if FCurrentImage < 0 then
      FCurrentImage := GifBitmaps.Count - 1;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(Self);
    Repaint;
  end;
end;

procedure TGifAnim.PriorFrame;
begin
  if (not FEmpty) and Visible and (not FAnimate) then
  begin
    Inc(FCurrentImage);
    if FCurrentImage > GifBitmaps.Count - 1 then
      FCurrentImage := 0;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(self);
    Repaint;
  end;
end;

function TGifAnim.LoadFromLazarusResource(const ResName: String): boolean;
var
  GifLoader: TGifLoader;
  StateAnimate: boolean;
  Resource: TLResource;
begin
  Result:=false;
  StateAnimate:= Animate;
  FWait.Enabled:= false;
  ResetImage;
  Resource:=nil;
  Resource:=LazarusResources.Find(ResName);
  if Resource <> nil then
  if CompareText(LazarusResources.Find(ResName).ValueType, 'gif')=0 then begin
    GifLoader := TGifLoader.Create(Filename);
    FEmpty := not GifLoader.LoadFromLazarusResource(ResName, FGifBitmaps);
    DefineSize(GifLoader.Width, GifLoader.Height);
    GifLoader.Free;
    Result:= FEmpty;
  end;
  if not Empty then
    GifChanged;
  FWait.Enabled:= StateAnimate;
end;

procedure TGifAnim.LoadFromFile(const Filename: string);
var
  GifLoader: TGifLoader;
begin
  FEmpty    := True;
  if not FileExists(Filename) then
    Exit;
  GifLoader := TGifLoader.Create(Filename);
  if (csDesigning in ComponentState) then
    FEmpty := not GifLoader.LoadFirstBitmap(CurrentView)
  else
    FEmpty := not GifLoader.LoadAllBitmap(FGifBitmaps);
  DefineSize(GifLoader.Width, GifLoader.Height);
  GifLoader.Free;
end;

procedure TGifAnim.OnTime(Sender: TObject);
begin
  if (not Empty) and Visible then
  begin
    if FCurrentImage > GifBitmaps.Count - 1 then
      FCurrentImage := 0;
    if assigned(FOnFrameChanged) then
      FOnFrameChanged(self);
    Paint;
    Inc(FCurrentImage);
  end;
end;

procedure TGifAnim.SetAnimate(const AValue: boolean);
begin
  if FAnimate = AValue then
    exit;
  FAnimate := AValue;
  if not (csDesigning in ComponentState) then
  begin
    FWait.Enabled := Animate;
    if Animate then
      DoStartAnim
    else
      DoStopAnim;
  end;
end;

procedure TGifAnim.SetFileName(const AValue: string);
var
  fn: string;
begin

  if (FFileName = AValue) then
    exit;
  FFileName := AValue;
  ResetImage;
  if (FFileName = '') then exit;
  if (csDesigning in ComponentState) then
  begin
     fn:= ExtractFileName(AValue);
     FFileName:= ExtractFilePath(AValue);
     FFileName:= ExtractRelativepath(ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile) ,FFileName);
     FFileName:=FFileName+fn;
     LoadFromFile(FFileName+fn);
  end
  else begin
     FFileName := AValue;
     LoadFromFile(FFileName);
  end;
  if not Empty then
    GifChanged;
end;

procedure TGifAnim.DefineSize(AWidth, AHeight: integer);
begin
  if (AWidth = FGifWidth) and (AHeight = FGifHeight) then
    Exit;
  FGifWidth  := AWidth;
  FGifHeight := AHeight;
  Height     := FGifHeight;
  Width      := FGifWidth;
  if not (csDesigning in ComponentState) then
  begin
    BufferImg.Height := Height;
    BufferImg.Width  := Width;
  end;
end;

procedure TGifAnim.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: boolean);
begin
  PreferredWidth  := FGifWidth;
  PreferredHeight := FGifHeight;
end;

procedure TGifAnim.DoAutoSize;
var
  ModifyWidth, ModifyHeight: boolean;
  NewWidth:  integer;
  NewHeight: integer;
begin
  if AutoSizing then
    Exit;    // we shouldn't come here in the first place

  BeginAutoSizing;
  try
    GetPreferredSize(NewWidth, NewHeight);
    ModifyWidth  := [akLeft, akRight] * (Anchors + AnchorAlign[Align]) <> [akLeft, akRight];
    ModifyHeight := [akTop, akBottom] * (Anchors + AnchorAlign[Align]) <> [akTop, akBottom];

    if not ModifyWidth then
      NewWidth := Width;
    if not ModifyHeight then
      NewHeight := Height;

    if (NewWidth <> Width) or (NewHeight <> Height) then
    begin
      SetBounds(Left, Top, NewWidth, NewHeight);
    end;
  finally
    EndAutoSizing;
  end;
end;

{
class function TGifAnim.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;
}

procedure TGifAnim.GifChanged;
begin
  if not (csDesigning in ComponentState) then
  begin
    BufferImg.Canvas.Brush.Color := (self.Color);
    BufferImg.Canvas.FillRect(Rect(0, 0, Width, Height));
    with GifBitmaps.Items[FCurrentImage] do
      BufferImg.Canvas.Draw(PosX, PosY, Bitmap);
    CurrentView.Assign(BufferImg);
  end;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TGifAnim.Paint;
begin
  if (not Empty) and Visible then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if (FCurrentImage < GifBitmaps.Count) then
        with GifBitmaps.Items[FCurrentImage] do
        begin
          BufferImg.Canvas.Brush.Color := (self.Color);
          if FCurrentImage = 0 then
            BufferImg.Canvas.FillRect(Rect(0, 0, Width, Height));
          if Delay <> 0 then
            FWait.Interval := Delay * 10;
          BufferImg.Canvas.Draw(PosX, PosY, Bitmap);
          CurrentView.Assign(BufferImg);
          case Method of
            //0 : Not specified...
            //1 : No change Background
            2: BufferImg.Canvas.FillRect(
                Rect(PosX, PosY, Bitmap.Width + PosX, Bitmap.Height + PosY));

            3: BufferImg.Canvas.FillRect(Rect(0, 0, Width, Height));
          end;
        end;
    end
    else
    begin
      Canvas.Brush.Color := (self.Color);
      Canvas.FillRect(Rect(0, 0, Width, Height));
    end;
    Canvas.Draw(0, 0, CurrentView);
  end;
  inherited Paint;
end;

procedure TGifAnim.ResetImage;
begin
  if assigned(FGifBitmaps) then
    FreeAndNil(FGifBitmaps);
  FCurrentImage:=0;
  with CurrentView do
  begin
    Canvas.Brush.Color := (self.Color);
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;
end;

procedure TGifAnim.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
end;

procedure TGifAnim.DoStartAnim;
begin
  if assigned(OnStartAnim) then
    OnStartAnim(Self);
end;

procedure TGifAnim.DoStopAnim;
begin
  if assigned(OnStopAnim) then
    OnStartAnim(Self);
end;

{ TGifLoader }

constructor TGifLoader.Create(const FileName: string);
begin
  FFileName := FileName;
  FGifUseGraphCtrlExt := False;
  FPalette  := TFPPalette.Create(0);
  FHeight   := 20;
  FWidth    := 20;
end;

destructor TGifLoader.Destroy;
begin
  inherited Destroy;
  FPalette.Free;
end;

function TGifLoader.LoadAllBitmap(var AGifList: TGifList): boolean;
var
  GifStream:  TMemoryStream;
  Introducer: byte;
  FPImage:    TLazIntfImage;
  ImgFormatDescription: TRawImageDescription;
  GifBitmap:  TGifImage;
begin
  Result := False;
  if not FileExists(FFileName) then
    exit;
  if not assigned(AGifList) then
    AGifList := TGifList.Create(True);

  GifStream := TMemoryStream.Create;
  GifStream.LoadFromFile(FFileName);
  GifStream.Position := 0;

  ReadHeader(GifStream);
  if (FGifHeader.Version <> '89a') then
    Exit;

  // skip first block extention if exist
  repeat
    Introducer := SkipBlock(GifStream);
  until (Introducer = ID_IMAGE_DESCRIPTOR) or (Introducer = ID_TRAILER);

  repeat
    ReadGifBitmap(GifStream);
    // decode Gif bitmap in Scanline buffer
    ReadScanLine(GifStream);
    // Create temp Fp Image for put scanline pixel
    FPImage := TLazIntfImage.Create(FLocalWidth, FLocalHeight);
    ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(FLocalWidth,
      FLocalHeight);
    FPImage.DataDescription := ImgFormatDescription;

    WriteScanLine(FPImage);

    GifBitmap := TGifImage.Create;
    GifBitmap.FBitmap.LoadFromIntfImage(FPImage);
    GifBitmap.FPosX   := FGifDescriptor.Left;
    GifBitmap.FPosY   := FGifDescriptor.Top;
    GifBitmap.FMethod := FDisposalMethod;
    GifBitmap.FDelay  := FGifGraphicsCtrlExt.DelayTime;

    AGifList.Add(GifBitmap);

    FPImage.Free;
    FreeMem(FScanLine, FLineSize);
    // reset FGifUseGraphCtrlExt flag
    FGifUseGraphCtrlExt := False;

    repeat
      Introducer := SkipBlock(GifStream);
    until (Introducer = ID_IMAGE_DESCRIPTOR) or (Introducer = ID_TRAILER);

  until (Introducer = ID_TRAILER);
  GifStream.Free;
  Result := True;
end;

function TGifLoader.LoadFromLazarusResource(const ResName: String; var AGifList: TGifList): boolean;
var
  GifStream:  TLazarusResourceStream;
  Introducer: byte;
  FPImage:    TLazIntfImage;
  ImgFormatDescription: TRawImageDescription;
  GifBitmap:  TGifImage;
begin
  Result := False;
  if not assigned(AGifList) then
    AGifList := TGifList.Create(True);

  GifStream := TLazarusResourceStream.Create(ResName, nil);
  GifStream.Position := 0;

  ReadHeader(GifStream);
  if (FGifHeader.Version <> '89a') then
    Exit;

  // skip first block extention if exist
  repeat
    Introducer := SkipBlock(GifStream);
  until (Introducer = ID_IMAGE_DESCRIPTOR) or (Introducer = ID_TRAILER);

  repeat
    ReadGifBitmap(GifStream);
    // decode Gif bitmap in Scanline buffer
    ReadScanLine(GifStream);
    // Create temp Fp Image for put scanline pixel
    FPImage := TLazIntfImage.Create(FLocalWidth, FLocalHeight);
    ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(FLocalWidth,
      FLocalHeight);
    FPImage.DataDescription := ImgFormatDescription;

    WriteScanLine(FPImage);

    GifBitmap := TGifImage.Create;
    GifBitmap.FBitmap.LoadFromIntfImage(FPImage);
    GifBitmap.FPosX   := FGifDescriptor.Left;
    GifBitmap.FPosY   := FGifDescriptor.Top;
    GifBitmap.FMethod := FDisposalMethod;
    GifBitmap.FDelay  := FGifGraphicsCtrlExt.DelayTime;

    AGifList.Add(GifBitmap);

    FPImage.Free;
    FreeMem(FScanLine, FLineSize);
    // reset FGifUseGraphCtrlExt flag
    FGifUseGraphCtrlExt := False;

    repeat
      Introducer := SkipBlock(GifStream);
    until (Introducer = ID_IMAGE_DESCRIPTOR) or (Introducer = ID_TRAILER);

  until (Introducer = ID_TRAILER);
  GifStream.Free;
  Result := True;
end;

function TGifLoader.LoadFirstBitmap(var ABitmap: TBitmap): boolean;
var
  GifStream:  TMemoryStream;
  Introducer: byte;
  FPImage:    TLazIntfImage;
  ImgFormatDescription: TRawImageDescription;
begin
  Result := False;
  if not FileExists(FFileName) then
    exit;
  if not assigned(ABitmap) then
    ABitmap := TBitmap.Create;

  GifStream := TMemoryStream.Create;
  GifStream.LoadFromFile(FFileName);
  GifStream.Position := 0;

  ReadHeader(GifStream);
  if (FGifHeader.Version <> '89a') then
    Exit;

  // skip first block extention if exist
  repeat
    Introducer := SkipBlock(GifStream);
  until (Introducer = ID_IMAGE_DESCRIPTOR) or (Introducer = ID_TRAILER);

  ReadGifBitmap(GifStream);
  // decode Gif bitmap in Scanline buffer
  ReadScanLine(GifStream);
  // Create temp Fp Image for put scanline pixel
  FPImage := TLazIntfImage.Create(FLocalWidth, FLocalHeight);
  ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(FLocalWidth,
    FLocalHeight);
  FPImage.DataDescription := ImgFormatDescription;

  WriteScanLine(FPImage);

  ABitmap.LoadFromIntfImage(FPImage);
  FPImage.Free;
  FreeMem(FScanLine, FLineSize);
  // reset FGifUseGraphCtrlExt flag
  FGifUseGraphCtrlExt := False;

  GifStream.Free;
  Result := True;
end;

procedure TGifLoader.SetTransparent(const AValue: boolean);
begin
  if FIsTransparent = AValue then
    exit;
  FIsTransparent := AValue;
end;

function TGifLoader.SkipBlock(Stream: TStream): byte;
var
  Introducer, Labels, SkipByte: byte;
begin
  Introducer := 0;
  Labels     := 0;
  SkipByte   := 0;
  Stream.Read(Introducer, 1);
  if Introducer = EXT_INTRODUCER then
  begin
    Stream.Read(Labels, 1);
    case Labels of
      EXT_COMMENT,
      EXT_APPLICATION:
        while True do
        begin
          Stream.Read(SkipByte, 1);
          if SkipByte = 0 then
            Break;
          Stream.Seek(SkipByte, soFromCurrent);
        end;
      EXT_GRAPHICS_CONTROL:
      begin
        Stream.Read(FGifGraphicsCtrlExt, SizeOf(FGifGraphicsCtrlExt));
        FGifUseGraphCtrlExt := True;
      end;
      EXT_PLAIN_TEXT:
      begin
        Stream.Read(SkipByte, 1);
        Stream.Seek(SkipByte, soFromCurrent);
        while True do
        begin
          Stream.Read(SkipByte, 1);
          if SkipByte = 0 then
            Break;
          Stream.Seek(SkipByte, soFromCurrent);
        end;
      end;
    end;
  end;
  Result := Introducer;
end;

procedure TGifLoader.ReadScanLine(Stream: TStream);
var
  OldPos, UnpackedSize, PackedSize: longint;
  I:      integer;
  Data, Bits, Code: cardinal;
  SourcePtr: PByte;
  InCode: cardinal;

  CodeSize: cardinal;
  CodeMask: cardinal;
  FreeCode: cardinal;
  OldCode:  cardinal;
  Prefix:   array[0..CODE_TABLE_SIZE - 1] of cardinal;
  Suffix, Stack: array [0..CODE_TABLE_SIZE - 1] of byte;
  StackPointer: PByte;
  DataComp, Target: PByte;
  B, FInitialCodeSize, FirstChar: byte;
  ClearCode, EOICode: word;

begin
  FInitialCodeSize := 0;
  B := 0;
  DataComp := nil;

  // initialisation du dictionnaire de decompression
  Stream.Read(FInitialCodeSize, 1);

  // Recherche la taille des données compresser
  OldPos     := Stream.Position;
  PackedSize := 0;
  repeat
    Stream.Read(B, 1);
    if B > 0 then
    begin
      Inc(PackedSize, B);
      Stream.Seek(B, soFromCurrent);
    end;
  until B = 0;

  Getmem(DataComp, PackedSize);
  // lecture des données conpresser
  SourcePtr := DataComp;
  Stream.Position := OldPos;
  repeat
    Stream.Read(B, 1);
    if B > 0 then
    begin
      Stream.ReadBuffer(SourcePtr^, B);
      Inc(SourcePtr, B);
    end;
  until B = 0;

  SourcePtr := DataComp;
  Target    := FScanLine;
  CodeSize  := FInitialCodeSize + 1;
  ClearCode := 1 shl FInitialCodeSize;
  EOICode   := ClearCode + 1;
  FreeCode  := ClearCode + 2;
  OldCode   := CODE_TABLE_SIZE;
  CodeMask  := (1 shl CodeSize) - 1;
  UnpackedSize := FLocalWidth * FLocalHeight;
  for I := 0 to ClearCode - 1 do
  begin
    Prefix[I] := CODE_TABLE_SIZE;
    Suffix[I] := I;
  end;
  StackPointer := @Stack;
  FirstChar := 0;
  Data := 0;
  Bits := 0;
  //Decompression LZW gif
  while (UnpackedSize > 0) and (PackedSize > 0) do
  begin
    Inc(Data, SourcePtr^ shl Bits);
    Inc(Bits, 8);
    while Bits >= CodeSize do
    begin
      Code := Data and CodeMask;
      Data := Data shr CodeSize;
      Dec(Bits, CodeSize);
      if Code = EOICode then
        Break;
      if Code = ClearCode then
      begin
        CodeSize := FInitialCodeSize + 1;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode  := CODE_TABLE_SIZE;
        Continue;
      end;
      if Code > FreeCode then
        Break;
      if OldCode = CODE_TABLE_SIZE then
      begin
        FirstChar := Suffix[Code];
        Target^   := FirstChar;
        Inc(Target);
        Dec(UnpackedSize);
        OldCode := Code;
        Continue;
      end;
      InCode := Code;
      if Code = FreeCode then
      begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;
      while Code > ClearCode do
      begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;
      FirstChar     := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;
      if (FreeCode = CodeMask) and (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;
      if FreeCode < CODE_TABLE_SIZE - 1 then
        Inc(FreeCode);
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Target^ := StackPointer^;
        Inc(Target);
        Dec(UnpackedSize);
      until StackPointer = @Stack;
    end;
    Inc(SourcePtr);
    Dec(PackedSize);
  end;
  FreeMem(DataComp);
end;

procedure TGifLoader.ReadHeader(Stream: TStream);
begin
  Stream.Read(FGifHeader, SizeOf(FGifHeader));

  with FGifHeader do
  begin
    FGifBackgroundColor := BackgroundColor;

    FWidth  := ScreenWidth;
    FHeight := ScreenHeight;

    FLocalWidth  := ScreenWidth;
    FLocalHeight := ScreenHeight;

    IsTransparent := False;
  end;
  ReadGlobalPalette(Stream);
end;

procedure TGifLoader.ReadGlobalPalette(Stream: TStream);
var
  ColorTableSize: integer;
begin
  if (FGifHeader.Packedbit and ID_COLOR_TABLE) <> 0 then
  begin
    ColorTableSize := FGifHeader.Packedbit and ID_COLOR_TABLE_SIZE + 1;
    ReadPalette(Stream, 1 shl ColorTableSize);
  end;
end;

procedure TGifLoader.ReadGraphCtrlExt;
var
  C: TFPColor;
begin
  IsTransparent := (FGifGraphicsCtrlExt.Packedbit and ID_TRANSPARENT) <> 0;

  FDisposalMethod := (FGifGraphicsCtrlExt.Packedbit and $1C) shr 2;

  if IsTransparent then
  begin
    // if Transparent bitmap change alpha channel
    FGifBackgroundColor := FGifGraphicsCtrlExt.ColorIndex;
    C := FPalette[FGifBackgroundColor];
    C.alpha := alphaTransparent;
    FPalette[FGifBackgroundColor] := C;
  end;
end;

procedure TGifLoader.SetInterlaced(const AValue: boolean);
begin
  if FInterlaced = AValue then
    exit;
  FInterlaced := AValue;
end;

procedure TGifLoader.ReadPalette(Stream: TStream; Size: integer);
var
  RGBEntry: TRGB;
  I: integer;
  C: TFPColor;
begin
  FPalette.Clear;
  FPalette.Count := 0;
  Fillchar(RGBEntry, SizeOf(RGBEntry), 0);
  for I := 0 to Size - 1 do
  begin
    Stream.Read(RGBEntry, SizeOf(RGBEntry));
    with C do
    begin
      Red   := RGBEntry.Red or (RGBEntry.Red shl 8);
      Green := RGBEntry.Green or (RGBEntry.Green shl 8);
      Blue  := RGBEntry.Blue or (RGBEntry.Blue shl 8);
      Alpha := alphaOpaque;
    end;
    FPalette.Add(C);
  end;
end;

procedure TGifLoader.WriteScanLine(Img: TFPCustomImage);
var
  Row, Col: integer;
  Pass, Every: byte;
  P: PByte;
begin
  P := FScanLine;
  if Interlaced then
  begin
    for Pass := 1 to 4 do
    begin
      case Pass of
        1:
        begin
          Row   := 0;
          Every := 8;
        end;
        2:
        begin
          Row   := 4;
          Every := 8;
        end;
        3:
        begin
          Row   := 2;
          Every := 4;
        end;
        4:
        begin
          Row   := 1;
          Every := 2;
        end;
      end;
      repeat
        for Col := 0 to FLocalWidth - 1 do
        begin
          Img.Colors[Col, Row] := FPalette[P^];
          Inc(P);
        end;
        Inc(Row, Every);
      until Row >= FLocalHeight;
    end;
  end
  else
  begin
    for Row := 0 to FLocalHeight - 1 do
      for Col := 0 to FLocalWidth - 1 do
      begin
        Img.Colors[Col, Row] := FPalette[P^];
        Inc(P);
      end;
  end;
end;

procedure TGifLoader.ReadGifBitmap(Stream: TStream);
var
  ColorTableSize: integer;
begin
  Stream.Read(FGifDescriptor, SizeOf(FGifDescriptor));

  with FGifDescriptor do
  begin
    FLocalWidth  := Width;
    FLocalHeight := Height;
    Interlaced   := (Packedbit and ID_INTERLACED = ID_INTERLACED);
  end;

  FLineSize := FLocalWidth * (FLocalHeight + 1);
  GetMem(FScanLine, FLineSize);

  if (FGifDescriptor.Packedbit and ID_COLOR_TABLE) <> 0 then
  begin
    ColorTableSize := FGifDescriptor.Packedbit and ID_COLOR_TABLE_SIZE + 1;
    ReadPalette(Stream, 1 shl ColorTableSize);
  end;
  if FGifUseGraphCtrlExt then
    ReadGraphCtrlExt;

end;

{ TGifImage }

constructor TGifImage.Create;
begin
  FBitmap := TBitmap.Create;
  FPosX   := 0;
  FPosY   := 0;
  FDelay  := 0;
  FMethod := 0;
end;

destructor TGifImage.Destroy;
begin
  inherited Destroy;
  FBitmap.Free;
end;

{ TGifList }

function TGifList.GetItems(Index: integer): TGifImage;
begin
  Result := TGifImage(inherited Items[Index]);
end;

procedure TGifList.SetItems(Index: integer; AGifImage: TGifImage);
begin
  Put(Index, AGifImage);
end;

function TGifList.Add(AGifImage: TGifImage): integer;
begin
  Result := inherited Add(AGifImage);
end;

function TGifList.Extract(Item: TGifImage): TGifImage;
begin
  Result := TGifImage(inherited Extract(Item));
end;

function TGifList.Remove(AGifImage: TGifImage): integer;
begin
  Result := inherited Remove(AGifImage);
end;

function TGifList.IndexOf(AGifImage: TGifImage): integer;
begin
  Result := inherited IndexOf(AGifImage);
end;

function TGifList.First: TGifImage;
begin
  Result := TGifImage(inherited First);
end;

function TGifList.Last: TGifImage;
begin
  Result := TGifImage(inherited Last);
end;

procedure TGifList.Insert(Index: integer; AGifImage: TGifImage);
begin
  inherited Insert(Index, AGifImage);
end;

initialization
{$I gifanim.lrs}

end.

unit VTGraphics; 

{$mode delphi}

interface

uses
  DelphiCompat, Types, LCLIntf, LCLType;

type
  // Describes the mode how to blend pixels.
  TBlendMode = (
    bmConstantAlpha,         // apply given constant alpha
    bmPerPixelAlpha,         // use alpha value of the source pixel
    bmMasterAlpha,           // use alpha value of source pixel and multiply it with the constant alpha value
    bmConstantAlphaAndColor  // blend the destination color with the given constant color und the constant alpha value
  );



procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;

function GetBitmapBitsFromBitmap(Bitmap: HBITMAP): Pointer;

implementation

{$IFDEF CPU32}
{$i vtgraphicsi.inc}
{$ENDIF CPU32}

{$IFDEF CPU64}

procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);
begin
end;

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;
begin
  Result := nil;
end;

function GetBitmapBitsFromBitmap(Bitmap: HBITMAP): Pointer;
begin
  Result := nil;
end;

{$ENDIF CPU64}

end.


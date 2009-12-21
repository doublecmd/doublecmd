// -----------------------------------------------------------------------------
// Project:	bitmap resampler
// Module:	resample
// Description: Interpolated Bitmap Resampling using filters.
// Version:	01.03
// Release:	1
// Date:	19-DEC-2009
// Target:	Free Pascal 2.2.4, Lazarus 0.9.29
// Author(s):	anme: Anders Melander, anders@melander.dk
// 	            Alexx2000: Alexander Koblov, Alexx2000@mail.ru
// Copyright	(c) 1997,98 by Anders Melander
// Copyright	(c) 2009 by Alexander Koblov
// Formatting:	2 space indent, 8 space tabs, 80 columns.
// -----------------------------------------------------------------------------
// This software is copyrighted as noted above.  It may be freely copied,
// modified, and redistributed, provided that the copyright notice(s) is
// preserved on all copies.
//
// There is no warranty or other guarantee of fitness for this software,
// it is provided solely "as is".  Bug reports or fixes may be sent
// to the author, who may or may not act on them as he desires.
//
// You may not include this software in a program or other software product
// without supplying the source, or without informing the end-user that the
// source is available for no extra charge.
//
// If you modify this software, you should include a notice in the "Revision
// history" section giving the name of the person performing the modification,
// the date of modification, and the reason for such modification.
// -----------------------------------------------------------------------------
// Here's some additional copyrights for you:
//
// From filter.c:
// The authors and the publisher hold no copyright restrictions
// on any of these files; this source code is public domain, and
// is freely available to the entire computer graphics community
// for study, use, and modification.  We do request that the
// comment at the top of each file, identifying the original
// author and its original publication in the book Graphics
// Gems, be retained in all programs that use these files.
//
// -----------------------------------------------------------------------------
// Revision history:
//
// 0100	110997	anme	- Adapted from fzoom v0.20 by Dale Schumacher.
//
// 0101	110198	anme	- Added Lanczos3 and Mitchell filters.
//			- Fixed range bug.
//			  Min value was not checked on conversion from Single to
//			  byte.
//			- Numerous optimizations.
//			- Added TImage stretch on form resize.
//			- Added support for Delphi 2 via TCanvas.Pixels.
//			- Renamed module from stretch to resample.
//			- Moved demo code to separate module.
//
// 0102 150398	anme	- Fixed a problem that caused all pixels to be shifted
//			  1/2 pixel down and to the right (in source
//			  coordinates). Thanks to David Ullrich for the
//			  solution.
// 0103 191209	Alexx2000	- Ported to FreePascal/Lazarus
//			- Added alpha channel support
// -----------------------------------------------------------------------------
// Credits:
// The algorithms and methods used in this library are based on the article
// "General Filtered Image Rescaling" by Dale Schumacher which appeared in the
// book Graphics Gems III, published by Academic Press, Inc.
//
// The edge offset problem was fixed by:
//   * David Ullrich <ullrich@hardy.math.okstate.edu>
// -----------------------------------------------------------------------------
// To do (in rough order of priority):
// * Implement Dale Schumacher's "Optimized Bitmap Scaling Routines".
// * Fix BoxFilter.
// * Optimize to use integer math instead of floating point where possible.
// -----------------------------------------------------------------------------

unit uReSample;

interface

{$mode delphi}{$R-}

uses
  SysUtils, Classes, Graphics;

type
  // Type of a filter for use with Stretch()
  TFilterProc = function(Value: Single): Single;

  // Sample filters for use with Stretch()
  function SplineFilter(Value: Single): Single;
  function BellFilter(Value: Single): Single;
  function TriangleFilter(Value: Single): Single;
  function BoxFilter(Value: Single): Single;
  function HermiteFilter(Value: Single): Single;
  function Lanczos3Filter(Value: Single): Single;
  function MitchellFilter(Value: Single): Single;

  // Interpolator
  // Src:	Source bitmap
  // Dst:	Destination bitmap
  // filter:	Weight calculation filter
  // fwidth:	Relative sample radius
  procedure Stretch(Src, Dst: TBitmap; filter: TFilterProc; fwidth: single);

// -----------------------------------------------------------------------------
//
//			List of Filters
//
// -----------------------------------------------------------------------------

const
  ResampleFilters: array[0..6] of record
    Name: string;	// Filter name
    Filter: TFilterProc;// Filter implementation
    Width: Single;	// Suggested sampling width/radius
  end = (
    (Name: 'Box';	Filter: BoxFilter;	Width: 0.5),
    (Name: 'Triangle';	Filter: TriangleFilter;	Width: 1.0),
    (Name: 'Hermite';	Filter: HermiteFilter;	Width: 1.0),
    (Name: 'Bell';	Filter: BellFilter;	Width: 1.5),
    (Name: 'B-Spline';	Filter: SplineFilter;	Width: 2.0),
    (Name: 'Lanczos3';	Filter: Lanczos3Filter;	Width: 3.0),
    (Name: 'Mitchell';	Filter: MitchellFilter;	Width: 2.0)
    );

implementation

uses
  Math, IntfGraphics, GraphType, FPImage;

// -----------------------------------------------------------------------------
//
//			Filter functions
//
// -----------------------------------------------------------------------------

// Hermite filter
function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Box filter
// a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable
//       results with this filter for subsampling.
function BoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

// Triangle filter
// a.k.a. "Linear" or "Bilinear" filter
function TriangleFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

// Bell filter
function BellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end else
    Result := 0.0;
end;

// B-spline filter
function SplineFilter(Value: Single): Single;
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5*tt*Value - tt + 2.0 / 3.0;
  end else if (Value < 2.0) then
  begin
    Value := 2.0 - Value;
    Result := 1.0/6.0 * Sqr(Value) * Value;
  end else
    Result := 0.0;
end;

// Lanczos3 filter
function Lanczos3Filter(Value: Single): Single;
  function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B		= (1.0 / 3.0);
  C		= (1.0 / 3.0);
var
  tt			: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end else
  if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end else
    Result := 0.0;
end;

// -----------------------------------------------------------------------------
//
//			Interpolator
//
// -----------------------------------------------------------------------------
type
  // Contributor for a pixel
  TContributor = record
    pixel: integer;		// Source pixel
    weight: single;		// Pixel weight
  end;

  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n		: integer;
    p		: PContributorList;
  end;

  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

  TRGBA = packed record
    r, g, b, a	: single;
  end;

  // Physical bitmap pixel
  TColorRGBA = packed record
    r, g, b, a	: BYTE;
  end;
  PColorRGBA = ^TColorRGBA;

  // Physical bitmap scanline (row)
  TRGBAList = packed array[0..0] of TColorRGBA;
  PRGBAList = ^TRGBAList;

function CreateAlphaFromMask(Bitmap: TBitmap): TLazIntfImage;
var
  SrcIntfImage: TLazIntfImage;
  x, y, xStop, yStop: Integer;
  Color: TFPColor;
begin
  SrcIntfImage := TLazIntfImage.Create(Bitmap.RawImage, False);
  with SrcIntfImage do
  begin
    if MaskData = nil then Exit(SrcIntfImage);
    Result := TLazIntfImage.Create(Width, Height, [riqfRGB, riqfAlpha]);
    Result.CreateData;
    xStop := Width - 1;
    yStop := Height - 1;
  end;
  for x:= 0 to xStop do
    for y:= 0 to yStop do
    begin
      Color := SrcIntfImage.Colors[x, y];
      if SrcIntfImage.Masked[x, y] then
        Color.Alpha := Low(Color.Alpha)
      else
        Color.Alpha := High(Color.Alpha);
      Result.Colors[x, y] := Color;
    end;
  SrcIntfImage.Free;
end;

procedure Stretch(Src, Dst: TBitmap; filter: TFilterProc; fwidth: single);
var
  xscale, yscale	: single;		// Zoom scale factors
  i, j, k		: integer;		// Loop variables
  center		: single;		// Filter calculation variables
  width, fscale, weight	: single;		// Filter calculation variables
  left, right		: integer;		// Filter calculation variables
  n			: integer;		// Pixel number
  Work			: PRGBAList;
  contrib		: PCListList;
  rgba			: TRGBA;
  color			: TColorRGBA;
  SourceLine		,
  DestLine		: PRGBAList;
  SrcDelta		: integer;
  SrcIntfImage,
  DstIntfImage          : TLazIntfImage;
  ImgFormatDescription  : TRawImageDescription;
  SrcWidth		,
  SrcHeight		,
  DstWidth		,
  DstHeight		: integer;

  function Color2RGBA(Color: TFPColor): TColorRGBA;
  begin
    Result.r := Color.Red shr 8;
    Result.g := Color.Green shr 8;
    Result.b := Color.Blue shr 8;
    Result.a := Color.Alpha shr 8;
  end;

  function RGBA2Color(Color: TColorRGBA): TFPColor;
  begin
    Result.Red   := Color.r shl 8;
    Result.Green := Color.g shl 8;
    Result.Blue  := Color.b shl 8;
    Result.Alpha := Color.a shl 8;
  end;

begin
  DstWidth := Dst.Width;
  DstHeight := Dst.Height;
  SrcWidth := Src.Width;
  SrcHeight := Src.Height;
  if (SrcWidth < 1) or (SrcHeight < 1) then
    raise Exception.Create('Source bitmap too small');

  // Create intermediate buffer to hold horizontal zoom
  Work := GetMem(DstWidth * SrcHeight * SizeOf(TColorRGBA));
  try
    // xscale := DstWidth / SrcWidth;
    // yscale := DstHeight / SrcHeight;
    // Improvement suggested by David Ullrich:
    if (SrcWidth = 1) then
      xscale:= DstWidth / SrcWidth
    else
      xscale:= (DstWidth - 1) / (SrcWidth - 1);
    if (SrcHeight = 1) then
      yscale:= DstHeight / SrcHeight
    else
      yscale:= (DstHeight - 1) / (SrcHeight - 1);

{++++++++++++++++++++}
    if Src.RawImage.Description.AlphaPrec = 0 then // if bitmap has not alpha channel
      SrcIntfImage := CreateAlphaFromMask(Src)
    else
      SrcIntfImage := Src.CreateIntfImage;
    DstIntfImage := Dst.CreateIntfImage;
    ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(DstWidth, DstHeight);
    DstIntfImage.DataDescription := ImgFormatDescription;
{++++++++++++++++++++}

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(contrib, DstWidth* sizeof(TCList));
    // Horizontal sub-sampling
    // Scales from bigger to smaller width
    if (xscale < 1.0) then
    begin
      width := fwidth / xscale;
      fscale := 1.0 / xscale;
      for i := 0 to DstWidth-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end else
    // Horizontal super-sampling
    // Scales from smaller to bigger width
    begin
      for i := 0 to DstWidth-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end;

    // ----------------------------------------------------
    // Apply filter to sample horizontally from Src to Work
    // ----------------------------------------------------
    for k := 0 to SrcHeight-1 do
    begin
{++++++++++++++++++++}
      DestLine := Work + k * DstWidth;
{++++++++++++++++++++}
      for i := 0 to DstWidth-1 do
      begin
        rgba.r := 0.0;
        rgba.g := 0.0;
        rgba.b := 0.0;
        rgba.a := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
{++++++++++++++++++++}
          color := Color2RGBA(SrcIntfImage.Colors[contrib^[i].p^[j].pixel, k]);
{++++++++++++++++++++}
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgba.r := rgba.r + color.r * weight;
          rgba.g := rgba.g + color.g * weight;
          rgba.b := rgba.b + color.b * weight;
          rgba.a := rgba.a + color.a * weight;
        end;
        if (rgba.r > 255.0) then
          color.r := 255
        else if (rgba.r < 0.0) then
          color.r := 0
        else
          color.r := round(rgba.r);
        if (rgba.g > 255.0) then
          color.g := 255
        else if (rgba.g < 0.0) then
          color.g := 0
        else
          color.g := round(rgba.g);
        if (rgba.b > 255.0) then
          color.b := 255
        else if (rgba.b < 0.0) then
          color.b := 0
        else
          color.b := round(rgba.b);
        if (rgba.a > 255.0) then
          color.a := 255
        else if (rgba.a < 0.0) then
          color.a := 0
        else
          color.a := round(rgba.a);
{++++++++++++++++++++}
        // Set new pixel value
        DestLine^[i] := color;
{++++++++++++++++++++}
      end;
    end;

    // Free the memory allocated for horizontal filter weights
    for i := 0 to DstWidth-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(contrib, DstHeight* sizeof(TCList));
    // Vertical sub-sampling
    // Scales from bigger to smaller height
    if (yscale < 1.0) then
    begin
      width := fwidth / yscale;
      fscale := 1.0 / yscale;
      for i := 0 to DstHeight-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end
    end else
    // Vertical super-sampling
    // Scales from smaller to bigger height
    begin
      for i := 0 to DstHeight-1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end;

    // --------------------------------------------------
    // Apply filter to sample vertically from Work to Dst
    // --------------------------------------------------
{++++++++++++++++++++}
    SourceLine := Work;
    SrcDelta := DstWidth;
{++++++++++++++++++++}
    for k := 0 to DstWidth-1 do
    begin
      for i := 0 to DstHeight-1 do
      begin
        rgba.r := 0;
        rgba.g := 0;
        rgba.b := 0;
        rgba.a := 0;
        // weight := 0.0;
        for j := 0 to contrib^[i].n-1 do
        begin
{++++++++++++++++++++}
          color := PColorRGBA(SourceLine+contrib^[i].p^[j].pixel*SrcDelta)^;
{++++++++++++++++++++}
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgba.r := rgba.r + color.r * weight;
          rgba.g := rgba.g + color.g * weight;
          rgba.b := rgba.b + color.b * weight;
          rgba.a := rgba.a + color.a * weight;
        end;
        if (rgba.r > 255.0) then
          color.r := 255
        else if (rgba.r < 0.0) then
          color.r := 0
        else
          color.r := round(rgba.r);
        if (rgba.g > 255.0) then
          color.g := 255
        else if (rgba.g < 0.0) then
          color.g := 0
        else
          color.g := round(rgba.g);
        if (rgba.b > 255.0) then
          color.b := 255
        else if (rgba.b < 0.0) then
          color.b := 0
        else
          color.b := round(rgba.b);
        if (rgba.a > 255.0) then
          color.a := 255
        else if (rgba.a < 0.0) then
          color.a := 0
        else
          color.a := round(rgba.a);
{++++++++++++++++++++}
        DstIntfImage.Colors[k, i]:= RGBA2Color(color);
{++++++++++++++++++++}
      end;
{++++++++++++++++++++}
      Inc(SourceLine);
{++++++++++++++++++++}
    end;

    // Free the memory allocated for vertical filter weights
    for i := 0 to DstHeight-1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    Dst.LoadFromIntfImage(DstIntfImage);

  finally
    FreeMem(Work);
    DstIntfImage.Free;
    SrcIntfImage.Free;
  end;
end;

end.

{ This file is part of Delphi Compatibility Unit

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

}

{$define HAS_GETBKCOLOR}
{$define HAS_GETTEXTEXTENTEXPOINT}

{$i ../generic/stubs.inc}
{$i ../generic/independentfunctions.inc}
{$i ../generic/unicodefunctions.inc}


procedure pango_extents_to_pixels (ink_rect: PPangoRectangle;
			  logical_rect: PPangoRectangle); cdecl; external 'libpango-1.0.so.0';

function GetBkColor(DC:HDC):COLORREF;
begin
  if GTK2WidgetSet.IsValidDC(DC) then
    Result := TGtkDeviceContext(DC).CurrentBackColor.ColorRef
  else
    Result := CLR_INVALID;
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  layout: PPangoLayout;
  i: Integer;
  Rect: TPangoRectangle;
  iter : PPangoLayoutIter;
begin
  Result := GTK2WidgetSet.IsValidDC(DC);
  if Result then
  with TGtk2DeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then
      layout := GTK2WidgetSet.GetDefaultGtkFont(false)
    else
      layout := CurrentFont^.GDIFontObject;
    pango_layout_set_text(layout, Str, Count);
    if PartialWidths = nil then
      pango_layout_get_pixel_size (layout, @Size.cx, @Size.cy)
    else
      begin
        i := 0;
        Size.cx := 0;
        Size.cy := 0;
        iter := pango_layout_get_iter(layout);
        repeat
          pango_layout_iter_get_char_extents(iter,@Rect);
          pango_extents_to_pixels(nil,@Rect);
          inc(Size.cx, Rect.Width);
          PartialWidths[i] := Size.cx;
          if Size.cy < Rect.Height then
            Size.cy := Rect.Height;
          inc(i);
        until not pango_layout_iter_next_char(iter);
        pango_layout_iter_free(iter);
      end;
  end;
end;

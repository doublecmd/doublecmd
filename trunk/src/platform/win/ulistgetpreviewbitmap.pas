{
   Double Commander
   -------------------------------------------------------------------------
   Lister plugins thumbnail provider

   Copyright (C) 2018 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uListGetPreviewBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WlxPlugin;

implementation

uses
  Types, Graphics, DCOSUtils, uThumbnails, uWlxModule, uBitmap, uGlobs;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
const
  MAX_LEN = 8192;
var
  Data: String;
  Index: Integer;
  Bitmap: HBITMAP;
  Handle: THandle;
  Module: TWlxModule;
begin
  if gWLXPlugins.Count = 0 then Exit(nil);

  SetLength(Data, MAX_LEN);
  Handle:= mbFileOpen(aFileName, fmOpenRead or fmShareDenyNone);
  if (Handle = feInvalidHandle) then Exit(nil);
  Index:= FileRead(Handle, Data[1], MAX_LEN);
  if Index >= 0 then SetLength(Data, Index);
  FileClose(Handle);

  for Index:= 0 to gWLXPlugins.Count - 1 do
  begin
    Module:= gWLXPlugins.GetWlxModule(Index);
    if Module.FileParamVSDetectStr(aFileName, True) then
    begin
      if (Module.IsLoaded or Module.LoadModule) and Module.CanPreview then
      begin
        Bitmap:= Module.CallListGetPreviewBitmap(aFileName, aSize.cx, aSize.cy, Data);
        if Bitmap <> 0 then
        begin
          Result:= BitmapCreateFromHBITMAP(Bitmap);
          Exit;
        end;
      end;
    end;
  end;
  Result:= nil;
end;

initialization
  TThumbnailManager.RegisterProvider(@GetThumbnail);

end.


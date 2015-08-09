{
   Double Commander
   -------------------------------------------------------------------------
   Photoshop Document image class

   Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit uDCReadPSD;

{$mode objfpc}{$H+}

interface

uses
  Graphics, FPImage;

type

  { TPhotoshopDocument }

  TPhotoshopDocument = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: String; override;
  end;

implementation

uses
  FPReadPSD;

{ TPhotoshopDocument }

class function TPhotoshopDocument.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TFPReaderPSD;
end;

class function TPhotoshopDocument.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:= TSharedBitmap;
end;

class function TPhotoshopDocument.GetFileExtensions: String;
begin
  Result:= 'psd';
end;

procedure Initialize;
begin
  // Register image format
  TPicture.RegisterFileFormat('psd', 'Photoshop Document', TPhotoshopDocument);
end;

initialization
  Initialize;

end.


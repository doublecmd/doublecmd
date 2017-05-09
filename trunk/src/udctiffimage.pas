unit uDCTiffImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPImage;

type

  { TTiffImage }

  TTiffImage = class(Graphics.TTiffImage)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
  end;

implementation

uses
  uFPReadTiff;

{ TTiffImage }

class function TTiffImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TFPReaderTiff;
end;

initialization
  TPicture.UnregisterGraphicClass(Graphics.TTiffImage);
  TPicture.RegisterFileFormat(TTiffImage.GetFileExtensions, 'Tagged Image File Format', TTiffImage);
end.


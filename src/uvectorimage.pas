unit uVectorImage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FPImage;

type

  { TVectorReader }

  TVectorReader = class(TFPCustomImageReader)
  public
    class function CreateBitmap(const FileName: String; AWidth, AHeight: Integer): TBitmap; virtual; abstract;
  end;

  { TVectorImage }

  TVectorImage = class(TFPImageBitmap)
  protected
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class procedure RegisterReaderClass(AReaderClass: TFPCustomImageReaderClass); virtual; abstract;
    class function CreateBitmap(const FileName: String; AWidth, AHeight: Integer): TBitmap; virtual;
  end;

  { TScalableVectorGraphics }

  TScalableVectorGraphics = class(TVectorImage)
  protected
    FReaderClass: TFPCustomImageReaderClass; static;
  public
    class function GetFileExtensions: String; override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class procedure RegisterReaderClass(AReaderClass: TFPCustomImageReaderClass); override;
  end;

implementation

uses
  uIconTheme;

type
  TVectorReaderClass = class of TVectorReader;

{ TVectorImage }

class function TVectorImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:= TSharedBitmap;
end;

class function TVectorImage.CreateBitmap(const FileName: String; AWidth,
  AHeight: Integer): TBitmap;
begin
  Result:= TVectorReaderClass(GetReaderClass).CreateBitmap(FileName, AWidth, AHeight);
end;

{ TScalableVectorGraphics }

class function TScalableVectorGraphics.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= FReaderClass;
end;

class function TScalableVectorGraphics.GetFileExtensions: String;
begin
  Result:= 'svg;svgz';
end;

class procedure TScalableVectorGraphics.RegisterReaderClass(AReaderClass: TFPCustomImageReaderClass);
begin
  FReaderClass:= AReaderClass;
end;

procedure Initialize;
begin
  TIconTheme.RegisterExtension('svg;svgz');
  TPicture.RegisterFileFormat('svg;svgz', 'Scalable Vector Graphics', TScalableVectorGraphics);
end;

initialization
  Initialize;

end.


{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GifView;

{$warn 5023 off : no warning about unused units}
interface

uses
  GifViewerStrConsts, uFastBitmap, uGifViewer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uGifViewer', @uGifViewer.Register);
end;

initialization
  RegisterPackage('GifView', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit pkg_gifanim; 

interface

uses
  GifAnim, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('GifAnim', @GifAnim.Register); 
end; 

initialization
  RegisterPackage('pkg_gifanim', @Register); 
end.

{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit dcpcrypt_laz; 

interface

uses
  DCPreg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DCPreg', @DCPreg.Register); 
end; 

initialization
  RegisterPackage('dcpcrypt_laz', @Register); 
end.

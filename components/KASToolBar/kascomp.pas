{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit KASComp; 

interface

uses
  KAStoolbar, KASEdit, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('KAStoolbar', @KAStoolbar.Register); 
  RegisterUnit('KASEdit', @KASEdit.Register); 
end; 

initialization
  RegisterPackage('KASComp', @Register); 
end.

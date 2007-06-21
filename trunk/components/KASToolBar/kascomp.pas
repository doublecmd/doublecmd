{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit KASComp; 

interface

uses
  KASEdit, KAStoolBar, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('KASEdit', @KASEdit.Register); 
  RegisterUnit('KAStoolBar', @KAStoolBar.Register); 
end; 

initialization
  RegisterPackage('KASComp', @Register); 
end.

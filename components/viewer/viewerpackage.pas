{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit viewerpackage; 

interface

uses
  ViewerControl, UnicodeUtils, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ViewerControl', @ViewerControl.Register); 
end; 

initialization
  RegisterPackage('viewerpackage', @Register); 
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit virtualtreeview_package_doublecmd;

interface

uses
  VirtualTrees, VTHeaderPopup, registervirtualtreeview, VTGraphics, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registervirtualtreeview', @registervirtualtreeview.Register);
end;

initialization
  RegisterPackage('virtualtreeview_package_doublecmd', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit KASComp;

interface

uses
  KASToolBar, KASProgressBar, KASPathEdit, KASToolItems, KASComboBox, 
  KASCDEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('KASToolBar', @KASToolBar.Register);
  RegisterUnit('KASProgressBar', @KASProgressBar.Register);
  RegisterUnit('KASPathEdit', @KASPathEdit.Register);
  RegisterUnit('KASComboBox', @KASComboBox.Register);
  RegisterUnit('KASCDEdit', @KASCDEdit.Register);
end;

initialization
  RegisterPackage('KASComp', @Register);
end.

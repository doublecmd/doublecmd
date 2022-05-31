{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit KASComp;

{$warn 5023 off : no warning about unused units}
interface

uses
  KASToolBar, KASProgressBar, KASPathEdit, KASToolItems, KASComboBox, 
  KASCDEdit, KASStatusBar, KASToolPanel, KASButton, KASButtonPanel, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('KASToolBar', @KASToolBar.Register);
  RegisterUnit('KASProgressBar', @KASProgressBar.Register);
  RegisterUnit('KASPathEdit', @KASPathEdit.Register);
  RegisterUnit('KASComboBox', @KASComboBox.Register);
  RegisterUnit('KASCDEdit', @KASCDEdit.Register);
  RegisterUnit('KASStatusBar', @KASStatusBar.Register);
  RegisterUnit('KASToolPanel', @KASToolPanel.Register);
  RegisterUnit('KASButton', @KASButton.Register);
  RegisterUnit('KASButtonPanel', @KASButtonPanel.Register);
end;

initialization
  RegisterPackage('KASComp', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cmdbox;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCmdBox, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uCmdBox', @uCmdBox.Register);
end;

initialization
  RegisterPackage('cmdbox', @Register);
end.

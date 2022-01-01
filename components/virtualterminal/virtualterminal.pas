{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VirtualTerminal;

{$warn 5023 off : no warning about unused units}
interface

uses
  VTColorTable, VTEmuCtl, VTEmuEsc, VTEmuPty, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('VirtualTerminal', @Register);
end.

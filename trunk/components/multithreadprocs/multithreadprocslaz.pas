{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MultiThreadProcsLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  MTProcs, MTPUtils, MTPCPU, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MultiThreadProcsLaz', @Register);
end.

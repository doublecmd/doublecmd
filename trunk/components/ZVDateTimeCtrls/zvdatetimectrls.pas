{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ZVDateTimeCtrls; 

interface

uses
    ZVDateTimePicker, DBZVDateTimePicker, ZVDateTimePickerPropEdit, 
  ZVDateTimeControlsReg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ZVDateTimeControlsReg', @ZVDateTimeControlsReg.Register); 
end; 

initialization
  RegisterPackage('ZVDateTimeCtrls', @Register); 
end.

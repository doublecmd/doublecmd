{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit MultiThreadProcsLaz; 

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

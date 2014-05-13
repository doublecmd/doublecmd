{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pkg_gifanim_dsgn;

interface

uses
  GifAnimDsgn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GifAnimDsgn', @GifAnimDsgn.Register);
end;

initialization
  RegisterPackage('pkg_gifanim_dsgn', @Register);
end.

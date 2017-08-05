unit uRandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Random(ABlock: PByte; ACount: Integer);

implementation

{$IF DEFINED(MSWINDOWS)}
uses JwaWinCrypt;
{$ENDIF}

procedure Random(ABlock: PByte; ACount: Integer);
var
  I: Integer;
{$IF DEFINED(MSWINDOWS)}
  Result: Boolean;
  phProv: HCRYPTPROV = 0;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  Result:= CryptAcquireContext(phProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);
  if Result then begin
    Result:= CryptGenRandom(phProv, ACount, ABlock);
    CryptReleaseContext(phProv, 0);
  end;
  if not Result then
  {$ENDIF}
  for I:= 0 to ACount - 1 do ABlock[I]:= Byte(System.Random(256));
end;

end.


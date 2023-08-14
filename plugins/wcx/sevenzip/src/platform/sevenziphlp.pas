unit SevenZipHlp;

{$mode delphi}

interface

uses
  Classes, SysUtils, ActiveX;

procedure VarStringClear(var PropVariant: TPropVariant);
function BinaryToUnicode(const bstrVal: TBstr): UnicodeString;

implementation

uses
  Windows;

procedure VarStringClear(var PropVariant: TPropVariant);
begin
  PropVariant.vt:= VT_EMPTY;
  SysFreeString(PropVariant.bstrVal);
end;

function BinaryToUnicode(const bstrVal: TBstr): UnicodeString;
var
  PropSize: Cardinal;
begin
  PropSize:= SysStringByteLen(bstrVal);
  SetLength(Result, PropSize div SizeOf(WideChar));
  Move(bstrVal^, PWideChar(Result)^, PropSize);
end;

end.

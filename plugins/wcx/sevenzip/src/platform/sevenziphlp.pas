unit SevenZipHlp;

{$mode delphi}

interface

uses
  Classes, SysUtils, ActiveX;

function WideToBinary(const Value: WideString): TBstr;
procedure VarStringClear(var PropVariant: TPropVariant);
function BinaryToUnicode(const bstrVal: TBstr): UnicodeString;

implementation

uses
  Windows
{$IF DEFINED(UNIX)}
  , SevenZip
{$ENDIF}
  ;

procedure VarStringClear(var PropVariant: TPropVariant);
begin
  PropVariant.vt:= VT_EMPTY;
  SysFreeString(PropVariant.bstrVal);
end;

function WideToBinary(const Value: WideString): TBstr;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= SysAllocString(PWideChar(Value));
end;
{$ELSE}
var
  S: UCS4String;
begin
  S:= WideStringToUCS4String(Value);
  Result:= SysAllocString(PUCS4Char(S));
end;
{$ENDIF}

function BinaryToUnicode(const bstrVal: TBstr): UnicodeString;
var
  PropSize: Cardinal;
{$IF DEFINED(MSWINDOWS)}
begin
  PropSize:= SysStringByteLen(bstrVal);
  SetLength(Result, PropSize div SizeOf(WideChar));
  Move(bstrVal^, PWideChar(Result)^, PropSize);
end;
{$ELSE}
var
  S: UCS4String;
begin
  PropSize:= SysStringByteLen(bstrVal);
  SetLength(S, PropSize div SizeOf(UCS4Char) + 1);
  Move(bstrVal^, Pointer(S)^, PropSize);
  Result:= UCS4StringToUnicodeString(S);
end;
{$ENDIF}

end.

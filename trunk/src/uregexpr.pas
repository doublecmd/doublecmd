unit uRegExpr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

type
  TRecodeTable = array[Byte] of Byte;

type

  { TRegExprEx }

  TRegExprEx = class(TRegExpr)
  private
    FLowerCase,
    FUpperCase: TRecodeTable;
  protected
    function RegExprInvertCaseFunction(const Ch: REChar): REChar;
  public
    constructor Create(const AEncoding: RegExprString); reintroduce;
  public
    function ExecRegExpr(const ARegExpr, AInputStr: RegExprString): Boolean;
    function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: RegExprString;
                                  AUseSubstitution: Boolean = False): RegExprString;
  end;

implementation

uses
  LazUTF8, LConvEncoding, uConvEncoding;

function InitRecodeTable(Encoding: String; ALowerCase: Boolean): TRecodeTable;
var
  I: Byte;
  C: String;
begin
  if ALowerCase then
  begin
    for I:= 0 to 255 do
    begin
     C:= ConvertEncoding(Chr(I), Encoding, EncodingUTF8);
     C:= UTF8LowerCase(C);
     C:= ConvertEncoding(C, EncodingUTF8, Encoding);
     if Length(C) > 0 then Result[I]:= Ord(C[1]);
    end;
  end
  else begin
   for I:= 0 to 255 do
   begin
    C:= ConvertEncoding(Chr(I), Encoding, EncodingUTF8);
    C:= UTF8UpperCase(C);
    C:= ConvertEncoding(C, EncodingUTF8, Encoding);
    if Length(C) > 0 then Result[I]:= Ord(C[1]);
   end;
  end;
end;

{ TRegExprEx }

function TRegExprEx.RegExprInvertCaseFunction(const Ch: REChar): REChar;
begin
  Result:= Chr(FUpperCase[Ord(Ch)]);
  if Result = Ch then Result:= Chr(FLowerCase[Ord(Ch)]);
end;

constructor TRegExprEx.Create(const AEncoding: RegExprString);
begin
  inherited Create;
  InvertCase:= @RegExprInvertCaseFunction;
  FLowerCase:= InitRecodeTable(AEncoding, True);
  FUpperCase:= InitRecodeTable(AEncoding, False);
end;

function TRegExprEx.ExecRegExpr(const ARegExpr, AInputStr: RegExprString): Boolean;
begin
  Self.Expression:= ARegExpr;
  Result:= Self.Exec(AInputStr);
end;

function TRegExprEx.ReplaceRegExpr(const ARegExpr, AInputStr,
  AReplaceStr: RegExprString; AUseSubstitution: Boolean): RegExprString;
begin
  Self.Expression:= ARegExpr;
  Result:= Self.Replace(AInputStr, AReplaceStr, AUseSubstitution);
end;

end.


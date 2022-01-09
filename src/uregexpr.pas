unit uRegExpr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LConvEncoding, uConvEncoding, uRegExprA, uRegExprW, uRegExprU;

type
  TRegExprType = (retAnsi, retUtf16le, retUtf8);

type

  { TRegExprEx }

  TRegExprEx = class
  private
    FEncoding: String;
    FRegExpA: TRegExpr;
    FRegExpW: TRegExprW;
    FRegExpU: TRegExprU;
    FType: TRegExprType;
    procedure SetExpression(const AValue: String);
    function GetMatchLen(Idx : Integer): PtrInt;
    function GetMatchPos(Idx : Integer): PtrInt;
  public
    constructor Create(const AEncoding: String = EncodingDefault; ASetEncoding: Boolean = False);
    destructor Destroy; override;
    function Exec(AOffset: UIntPtr = 1): Boolean;
    function ReplaceAll(const AExpression, AStr, AReplacement: String): String;
    procedure ChangeEncoding(const AEncoding: String);
    procedure SetInputString(AInputString : Pointer; ALength : UIntPtr);
  public
    property Expression : String write SetExpression;
    property MatchPos [Idx : Integer] : PtrInt read GetMatchPos;
    property MatchLen [Idx : Integer] : PtrInt read GetMatchLen;
  end;

implementation

uses
  LazUTF8;

{ TRegExprEx }

procedure TRegExprEx.SetExpression(const AValue: String);
begin
  case FType of
    retUtf8:    FRegExpU.Expression:= AValue;
    retUtf16le: FRegExpW.Expression:= UTF8ToUTF16(AValue);
    retAnsi:    FRegExpA.Expression:= ConvertEncoding(AValue, EncodingUTF8, FEncoding);
  end;
end;

function TRegExprEx.GetMatchLen(Idx: integer): PtrInt;
begin
  case FType of
    retAnsi:    Result:= FRegExpA.MatchLen[Idx];
    retUtf8:    Result:= FRegExpU.MatchLen[Idx];
    retUtf16le: Result:= FRegExpW.MatchLen[Idx] * SizeOf(WideChar);
  end;
end;

function TRegExprEx.GetMatchPos(Idx: integer): PtrInt;
begin
  case FType of
    retAnsi:    Result:= FRegExpA.MatchPos[Idx];
    retUtf8:    Result:= FRegExpU.MatchPos[Idx];
    retUtf16le: Result:= FRegExpW.MatchPos[Idx] * SizeOf(WideChar);
  end;
end;

constructor TRegExprEx.Create(const AEncoding: String; ASetEncoding: Boolean = False);
begin
  FRegExpW:= TRegExprW.Create;
  FRegExpU:= TRegExprU.Create;
  FRegExpA:= TRegExpr.Create(AEncoding);
  if ASetEncoding then ChangeEncoding(AEncoding);
end;

destructor TRegExprEx.Destroy;
begin
  FRegExpA.Free;
  FRegExpW.Free;
  FRegExpU.Free;
  inherited Destroy;
end;

function TRegExprEx.Exec(AOffset: UIntPtr): Boolean;
begin
  case FType of
    retAnsi:    Result:= FRegExpA.Exec(AOffset);
    retUtf8:    Result:= FRegExpU.Exec(AOffset);
    retUtf16le: Result:= FRegExpW.Exec((AOffset + 1) div SizeOf(WideChar));
  end;
end;

function TRegExprEx.ReplaceAll(const AExpression, AStr, AReplacement: String): String;
var
  InputString: String;
begin
  case FType of
    retAnsi:
      Result := FRegExpA.ReplaceRegExpr(AExpression, AStr, AReplacement, True);
    retUtf8:
    begin
      FRegExpU.Expression := AExpression;
      InputString := AStr;
      FRegExpU.SetInputString(PAnsiChar(InputString), Length(InputString));
      if not FRegExpU.ReplaceAll(AReplacement, Result) then
        Result := InputString;
    end;
    retUtf16le:
      Result := AStr; // TODO : Implement ReplaceAll for TRegExprW
  end;
end;

procedure TRegExprEx.ChangeEncoding(const AEncoding: String);
begin
  FEncoding:= NormalizeEncoding(AEncoding);
  if FEncoding = EncodingDefault then
    FEncoding:= GetDefaultTextEncoding;
  if FEncoding = EncodingUTF16LE then
    FType:= retUtf16le
  else if (FEncoding = EncodingUTF8) or (FEncoding = EncodingUTF8BOM) then
    FType:= retUtf8
  else begin
    FType:= retAnsi;
    FRegExpA.ChangeEncoding(FEncoding);
  end;
end;

procedure TRegExprEx.SetInputString(AInputString: Pointer; ALength: UIntPtr);
begin
  case FType of
    retAnsi:    FRegExpA.SetInputString(AInputString, ALength);
    retUtf8:    FRegExpU.SetInputString(AInputString, ALength);
    retUtf16le: FRegExpW.SetInputString(AInputString, ALength div SizeOf(WideChar));
  end;
end;

end.


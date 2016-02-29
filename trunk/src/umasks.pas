{
   Double Commander
   -------------------------------------------------------------------------
   Modified version of standart Masks unit

   Copyright (C) 2010-2015 Alexander Koblov (alexx2000@mail.ru)

   This file is based on masks.pas from the Lazarus Component Library (LCL)

   See the file COPYING.modifiedLGPL.txt, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit uMasks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, DCStrUtils;

type
  TMaskCharType = (mcChar, mcAnyChar, mcAnyText);

  TMaskChar = record
    case CharType: TMaskCharType of
      mcChar: (CharValue: WideChar);
      mcAnyChar, mcAnyText: ();
  end;

  TMaskString = record
    MinLength: Integer;
    MaxLength: Integer;
    Chars: Array of TMaskChar;
  end;

  { TMask }

  TMask = class
  private
    FTemplate:string;
    FMask: TMaskString;
    FCaseSensitive: Boolean;

    procedure SetCaseSence(ACaseSence:boolean);
    procedure SetTemplate(AValue: String);
    procedure Update;
//    procedure SetTo(AValue: string);
  public
    constructor Create(const AValue: String; CaseSensitive: Boolean = False);
    function Matches(const AFileName: String): Boolean;

    property CaseSensitive:boolean read FCaseSensitive write SetCaseSence;
    property Template:string read FTemplate write SetTemplate;
  end;

  { TParseStringList }

  TParseStringList = class(TStringList)
  public
    constructor Create(const AText, ASeparators: String);
  end;

  { TMaskList }

  TMaskList = class
  private
    FMasks: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMask;
  public
    constructor Create(const AValue: String; ASeparatorCharset: string=';,'; ACaseSence:boolean=True);
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
  end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean = False): Boolean;
function MatchesMaskList(const FileName, Mask: String; ASeparatorCharset: string = ';, ';ACaseSens: Boolean = False): Boolean;

implementation

uses
  LazUTF8;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean = False): Boolean;
var
  AMask: TMask;
begin
  if Mask <> '' then
  begin
    AMask := TMask.Create(Mask, CaseSensitive);
    try
      Result := AMask.Matches(FileName);
    finally
      AMask.Free;
    end;
  end
  else
    Result := False;
end;

function MatchesMaskList(const FileName, Mask: String; ASeparatorCharset: string;ACaseSens: Boolean = False): Boolean;
var
  AMaskList: TMaskList;
begin
  if Mask <> '' then
  begin
    AMaskList := TMaskList.Create(Mask, ASeparatorCharset,ACaseSens);
    try
      Result := AMaskList.Matches(FileName);
    finally
      AMaskList.Free;
    end;
  end
  else
    Result := False;
end;

{ TMask }

constructor TMask.Create(const AValue: String; CaseSensitive: Boolean = False);
begin
  FTemplate:=AValue;
  FCaseSensitive:=CaseSensitive;
  Update;
  {
  FCaseSensitive := CaseSensitive;

  if FCaseSensitive then
    S := UTF8Decode(AValue)
  else begin
    S := UTF8Decode(UTF8LowerCase(AValue));
  end;

  I := 1;
  while I <= Length(S) do
  begin
    case S[I] of
      '*': AddAnyText;
      '?': AddAnyChar;
      else AddChar;
    end;
  end;
  }
end;

procedure TMask.SetCaseSence(ACaseSence:boolean);
begin
  FCaseSensitive:=ACaseSence;
  Update;
end;

procedure TMask.SetTemplate(AValue: String);
begin
  FTemplate:=AValue;
  Update;
end;

procedure TMask.Update;
var
  I: Integer;
  S: UnicodeString;
  SkipAnyText: Boolean;
  AValue:string;

  procedure AddAnyText;
  begin
    if SkipAnyText then
    begin
      Inc(I);
      Exit;
    end;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    FMask.Chars[High(FMask.Chars)].CharType := mcAnyText;

    FMask.MaxLength := MaxInt;
    SkipAnyText := True;
    Inc(I);
  end;

  procedure AddAnyChar;
  begin
    SkipAnyText := False;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    FMask.Chars[High(FMask.Chars)].CharType := mcAnyChar;

    Inc(FMask.MinLength);
    if FMask.MaxLength < MaxInt then Inc(FMask.MaxLength);

    Inc(I);
  end;

  procedure AddChar;
  begin
    SkipAnyText := False;

    SetLength(FMask.Chars, Length(FMask.Chars) + 1);
    with FMask.Chars[High(FMask.Chars)] do
    begin
      CharType := mcChar;
      CharValue := S[I];
    end;

    Inc(FMask.MinLength);
    if FMask.MaxLength < MaxInt then Inc(FMask.MaxLength);

    Inc(I);
  end;

begin
//  FTemplate:=AValue;
  AValue:=FTemplate;

  SetLength(FMask.Chars, 0);
  FMask.MinLength := 0;
  FMask.MaxLength := 0;
  SkipAnyText := False;

  if FCaseSensitive then
    S := UTF8Decode(AValue)
  else begin
    S := UTF8Decode(UTF8LowerCase(AValue));
  end;

  I := 1;
  while I <= Length(S) do
  begin
    case S[I] of
      '*': AddAnyText;
      '?': AddAnyChar;
      else AddChar;
    end;
  end;
end;

function TMask.Matches(const AFileName: String): Boolean;
var
  L: Integer;
  S: UnicodeString;

  function MatchToEnd(MaskIndex, CharIndex: Integer): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;

    for I := MaskIndex to High(FMask.Chars) do
    begin
      case FMask.Chars[I].CharType of
        mcChar:
          begin
            if CharIndex > L then Exit;
            //DCDebug('Match ' + S[CharIndex] + '<?>' + FMask.Chars[I].CharValue);
            if S[CharIndex] <> FMask.Chars[I].CharValue then Exit;
            Inc(CharIndex);
          end;
        mcAnyChar:
          begin
            if CharIndex > L then Exit;
            Inc(CharIndex);
          end;
        mcAnyText:
          begin
            if I = High(FMask.Chars) then
            begin
              Result := True;
              Exit;
            end;

            for J := CharIndex to L do
              if MatchToEnd(I + 1, J) then
              begin
                Result := True;
                Exit;
              end;
          end;
      end;
    end;

    Result := CharIndex > L;
  end;

begin
  Result := False;

  if FCaseSensitive then
    S := UTF8Decode(AFileName)
  else begin
    S := UTF8Decode(UTF8LowerCase(AFileName));
  end;

  L := Length(S);
  if L = 0 then
  begin
    if FMask.MinLength = 0 then Result := True;
    Exit;
  end;

  if (L < FMask.MinLength) or (L > FMask.MaxLength) then Exit;

  Result := MatchToEnd(0, 1);
end;

{ TParseStringList }

constructor TParseStringList.Create(const AText, ASeparators: String);
var
  I, S: Integer;
begin
  inherited Create;

  S := 1;
  for I := 1 to Length(AText) do
  begin
    if Pos(AText[I], ASeparators) > 0 then
    begin
      if I > S then Add(Copy(AText, S, I - S));
      S := I + 1;
    end;
  end;

  if Length(AText) >= S then Add(Copy(AText, S, Length(AText) - S + 1));
end;

{ TMaskList }

function TMaskList.GetItem(Index: Integer): TMask;
begin
  Result := TMask(FMasks.Items[Index]);
end;

function TMaskList.GetCount: Integer;
begin
  Result := FMasks.Count;
end;

constructor TMaskList.Create(const AValue: String; ASeparatorCharset: string; ACaseSence:boolean);
var
  L: String;
  I: Integer;
  S: TParseStringList;

  sSearchName,sSearchExt,sSearchNameNoExt:string;
  nMask:TMask;
begin
  FMasks := TObjectList.Create(True);
  if AValue='' then exit;

//  if not ACaseSence then L := UTF8LowerCase(AValue) else L := AValue;
  L := UTF8LowerCase(AValue);
  S := TParseStringList.Create(L, ASeparatorCharset);
  try
    for I := 0 to S.Count - 1 do
      FMasks.Add(TMask.Create(S[I], True));
  finally
    S.Free;
  end;
  {

  try
    for I := 0 to S.Count - 1 do
    begin

      sSearchName:=S[i];

      if Pos('.', sSearchName) <> 0 then
      begin
        sSearchNameNoExt := ExtractOnlyFileName(sSearchName);
        sSearchExt := ExtractFileExt(sSearchName);
        if AAnyPrefix  then sSearchNameNoExt := '*' + sSearchNameNoExt;
        if AAnyPostfix then sSearchNameNoExt := sSearchNameNoExt + '*';
        sSearchName := sSearchNameNoExt + sSearchExt;
      end
      else
      begin
        if AAnyPrefix then sSearchName := '*' + sSearchName;
        sSearchName := sSearchName + '*';
      end;

      nMask:=TMask.Create(sSearchName, ACaseSence);
      FMasks.Add(nMask);
    end;
  finally
    S.Free;
  end;
  }
end;

destructor TMaskList.Destroy;
begin
  FMasks.Free;

  inherited Destroy;
end;

function TMaskList.Matches(const AFileName: String): Boolean;
var
  S: String;
  I: Integer;
begin
  Result := False;

  S := UTF8LowerCase(AFileName);
  for I := 0 to FMasks.Count - 1 do
  begin
    if TMask(FMasks.Items[I]).Matches(S) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.

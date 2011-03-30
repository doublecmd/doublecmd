{
   Double Commander
   -------------------------------------------------------------------------
   Modified version of standart Masks unit

   Copyright (C) 2010 Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, Contnrs;

type
  TMaskCharType = (mcChar, mcAnyChar, mcAnyText);

  TCharSet = set of Char;

  TMaskChar = record
    case CharType: TMaskCharType of
      mcChar: (CharValue: Char);
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
    FMask: TMaskString;
  public
    constructor Create(const AValue: String);

    function Matches(const AFileName: String): Boolean;
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
    constructor Create(const AValue: String; ASeparator: Char = ';');
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
  end;

function MatchesMask(const FileName, Mask: String): Boolean;
function MatchesMaskList(const FileName, Mask: String; Separator: Char = ';'): Boolean;

implementation

function MatchesMask(const FileName, Mask: String): Boolean;
var
  AMask: TMask;
begin
  AMask := TMask.Create(Mask);
  try
    Result := AMask.Matches(FileName);
  finally
    AMask.Free;
  end;
end;

function MatchesMaskList(const FileName, Mask: String; Separator: Char): Boolean;
var
  AMaskList: TMaskList;
begin
  AMaskList := TMaskList.Create(Mask, Separator);
  try
    Result := AMaskList.Matches(FileName);
  finally
    AMaskList.Free;
  end;
end;

{ TMask }

constructor TMask.Create(const AValue: String);
var
  I: Integer;
  SkipAnyText: Boolean;

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
      CharValue := UpCase(AValue[I]);
    end;

    Inc(FMask.MinLength);
    if FMask.MaxLength < MaxInt then Inc(FMask.MaxLength);

    Inc(I);
  end;

begin
  SetLength(FMask.Chars, 0);
  FMask.MinLength := 0;
  FMask.MaxLength := 0;
  SkipAnyText := False;

  I := 1;
  while I <= Length(AValue) do
  begin
    case AValue[I] of
      '*': AddAnyText;
      '?': AddAnyChar;
      else AddChar;
    end;
  end;
end;

function TMask.Matches(const AFileName: String): Boolean;
var
  L: Integer;
  S: String;

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
  L := Length(AFileName);
  if L = 0 then
  begin
    if FMask.MinLength = 0 then Result := True;
    Exit;
  end;

  if (L < FMask.MinLength) or (L > FMask.MaxLength) then Exit;

  S := UpperCase(AFileName);
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

constructor TMaskList.Create(const AValue: String; ASeparator: Char);
var
  S: TParseStringList;
  I: Integer;
begin
  FMasks := TObjectList.Create(True);

  S := TParseStringList.Create(AValue, ASeparator);
  try
    for I := 0 to S.Count - 1 do
      FMasks.Add(TMask.Create(S[I]));
  finally
    S.Free;
  end;
end;

destructor TMaskList.Destroy;
begin
  FMasks.Free;

  inherited Destroy;
end;

function TMaskList.Matches(const AFileName: String): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to FMasks.Count - 1 do
  begin
    if TMask(FMasks.Items[I]).Matches(AFileName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
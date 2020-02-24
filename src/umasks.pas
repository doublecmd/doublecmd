{
   Double Commander
   -------------------------------------------------------------------------
   Modified version of standard Masks unit

   Copyright (C) 2010-2016 Alexander Koblov (alexx2000@mail.ru)

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
    fIgnoreAccents: Boolean;
    fWindowsInterpretation: boolean;

    procedure SetCaseSence(ACaseSence:boolean);
    procedure SetTemplate(AValue: String);
    procedure Update;
  public
    constructor Create(const AValue: string; bCaseSensitive: boolean = False; bIgnoreAccents: boolean = False; bWindowsInterpretation: boolean = False);
    function Matches(const AFileName: string): boolean;
    function LegacyMatches(const AFileName: string): boolean;
    function WindowsMatches(const AFileName: string): boolean;
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
    constructor Create(const AValue: string; ASeparatorCharset: string = ';'; bCaseSensitive: boolean = False; bIgnoreAccents: boolean = False; bWindowsInterpretation: boolean = False);
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
  end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean = False): Boolean;
function MatchesMaskList(const FileName, Mask: string; ASeparatorCharset: string = ';'; ACaseSensitive: boolean = False; AIgnoreAccents: boolean = False; AWindowsInterpretation: boolean = False): boolean;


implementation

uses
  //Lazarus, Free-Pascal, etc.
  LazUTF8,

  //DC
  uAccentsUtils;


{ MatchesMask }
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

{ MatchesMaskList }
function MatchesMaskList(const FileName, Mask: string; ASeparatorCharset: string; ACaseSensitive: boolean = False; AIgnoreAccents: boolean = False; AWindowsInterpretation: boolean = False): boolean;
var
  AMaskList: TMaskList;
begin
  if Mask <> '' then
  begin
    AMaskList := TMaskList.Create(Mask, ASeparatorCharset, ACaseSensitive, AIgnoreAccents, AWindowsInterpretation);
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

{ TMask.Create }
constructor TMask.Create(const AValue: string; bCaseSensitive: boolean = False; bIgnoreAccents: boolean = False; bWindowsInterpretation: boolean = False);
begin
  FTemplate:=AValue;
  FCaseSensitive := bCaseSensitive;
  fIgnoreAccents := bIgnoreAccents;
  fWindowsInterpretation := bWindowsInterpretation;
  if bIgnoreAccents then FTemplate := NormalizeAccentedChar(FTemplate); //Let's set the mask early in straight letters if match attempt has to be with accent and ligature removed.
  if not bCaseSensitive then FTemplate := UTF8LowerCase(FTemplate); //Let's set the mask early in lowercase if match attempt has to be case insensitive.
  Update;
end;

{ TMask.SetCaseSence }
procedure TMask.SetCaseSence(ACaseSence:boolean);
begin
  FCaseSensitive:=ACaseSence;
  Update;
end;

{ TMask.SetTemplate }
procedure TMask.SetTemplate(AValue: String);
begin
  FTemplate:=AValue;
  Update;
end;

{ TMask.Update }
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
  AValue:=FTemplate;

  SetLength(FMask.Chars, 0);
  FMask.MinLength := 0;
  FMask.MaxLength := 0;
  SkipAnyText := False;
  S := UTF8Decode(AValue);

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

{ TMask.Matches }
function TMask.Matches(const AFileName: string): boolean;
var
  sFilename: string;
begin
  //Let's set the AFileName in straight letters if match attempt has to be with accent and ligature removed.
  if FIgnoreAccents then
    sFilename := NormalizeAccentedChar(AFileName)
  else
    sFilename := AFileName;

  //Let's set our AFileName is lowercase early if not case-sensitive
  if not FCaseSensitive then
    sFilename := UTF8LowerCase(sFilename);

  if not fWindowsInterpretation then
    Result := LegacyMatches(sFileName)
  else
    Result := WindowsMatches(sFileName);
end;

{ TMask.LegacyMatches }
function TMask.LegacyMatches(const AFileName: string): boolean;
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
  S := UTF8Decode(AFileName);
  L := Length(S);
  if L = 0 then
  begin
    if FMask.MinLength = 0 then Result := True;
    Exit;
  end;

  if (L < FMask.MinLength) or (L > FMask.MaxLength) then Exit;

  Result := MatchToEnd(0, 1);
end;

{ TMask.WindowsMatches }
// treat initial mask differently for special cases:
// foo*.* -> foo*
// foo*. -> match foo*, but muts not have an extension
// *. -> any file without extension ( .foo is a filename without extension according to Windows)
// foo. matches only foo but not foo.txt
// foo.* -> match either foo or foo.*
function TMask.WindowsMatches(const AFileName: string): boolean;
var
  Ext, sInitialTemplate: string;
  sInitialMask: UnicodeString;

begin
  sInitialMask := UTF8Decode(FTemplate);

  if (Length(sInitialMask) > 2) and (RightStr(sInitialMask, 3) = '*.*') then // foo*.*
  begin
    sInitialTemplate := FTemplate; //Preserve initial state of FTemplate
    FTemplate := Copy(sInitialMask, 1, Length(sInitialMask) - 2);
    Update;
    Result := LegacyMatches(AFileName);
    FTemplate := sInitialTemplate; //Restore initial state of FTemplate
    Update;
  end
  else if (Length(sInitialMask) > 1) and (RightStr(sInitialMask, 1) = '.') then //foo*. or *. or foo.
  begin
    //if AFileName has an extension then Result is False, otherwise see if it LegacyMatches foo*/foo
    //a filename like .foo under Windows is considered to be a file without an extension
    Ext := ExtractFileExt(AFileName);
    if (Ext = '') or (Ext = AFileName) then
    begin
      sInitialTemplate := FTemplate; //Preserve initial state of FTemplate
      FTemplate := Copy(sInitialMask, 1, Length(sInitialMask) - 1);
      Update;
      Result := LegacyMatches(AFileName);
      FTemplate := sInitialTemplate; //Restore initial state of FTemplate
      Update;
    end
    else
    begin
      Result := False;
    end;
  end
  else if (Length(sInitialMask) > 2) and (RightStr(sInitialMask, 2) = '.*') then //foo.*  (but not '.*')
  begin
    //First see if we have 'foo'
    Result := (AFileName = Copy(sInitialMask, 1, Length(sInitialMask) - 2));
    if not Result then Result := LegacyMatches(AFileName);
  end
  else
  begin
    Result := LegacyMatches(AFileName); //all other cases just call LegacyMatches()
  end;
end;

{ TParseStringList }
{ TParseStringList.Create }
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

{ TMaskList.GetCount }
function TMaskList.GetCount: Integer;
begin
  Result := FMasks.Count;
end;

{ TMaskList.Create }
constructor TMaskList.Create(const AValue: string; ASeparatorCharset: string; bCaseSensitive: boolean = False; bIgnoreAccents: boolean = False; bWindowsInterpretation: boolean = False);
var
  I: Integer;
  S: TParseStringList;
begin
  FMasks := TObjectList.Create(True);
  if AValue = '' then exit;

  S := TParseStringList.Create(AValue, ASeparatorCharset);
  try
    for I := 0 to S.Count - 1 do
      FMasks.Add(TMask.Create(S[I], bCaseSensitive, bIgnoreAccents, bWindowsInterpretation));
  finally
    S.Free;
  end;
end;

{ TMaskList.Destroy }
destructor TMaskList.Destroy;
begin
  FMasks.Free;

  inherited Destroy;
end;

{ TMaskList.Matches }
function TMaskList.Matches(const AFileName: string): boolean;
var
  I: integer;
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

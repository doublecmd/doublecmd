{
   Double Commander
   -------------------------------------------------------------------------
   Modified version of standard Masks unit

   Copyright (C) 2010-2025 Alexander Koblov (alexx2000@mail.ru)

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
  TMaskOption = (moCaseSensitive, moIgnoreAccents, moWindowsMask, moPinyin);
  TMaskOptions = set of TMaskOption;

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
    FTemplate: String;
    FOriginal: String;
    FMask: TMaskString;
    FNormalize: Boolean;
    FUsePinyin: Boolean;
    FCaseSensitive: Boolean;
    fIgnoreAccents: Boolean;
    fWindowsInterpretation: boolean;

    procedure SetCaseSence(ACaseSence:boolean);
    procedure SetTemplate(AValue: String);
    procedure UpdateTemplate;
    procedure Update;
  public
    constructor Create(const AValue: string; const AOptions: TMaskOptions = []);
    function Matches(const AFileName: string): boolean;
    function RegularMatches(const AFileName: string): boolean;
    function WindowsMatches(const AFileName: string): boolean;
    property CaseSensitive:boolean read FCaseSensitive write SetCaseSence;
    property Template: String read FOriginal write SetTemplate;
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
    constructor Create(const AValue: string; ASeparatorCharset: string = ';'; const AOptions: TMaskOptions = []);
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
  end;

function MatchesMask(const FileName, Mask: String; const AOptions: TMaskOptions = []): Boolean;
function MatchesMaskList(const FileName, Mask: string; ASeparatorCharset: string = ';'; const AOptions: TMaskOptions = []): boolean;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  LazUTF8,

  //DC
  DCConvertEncoding, DCOSUtils, DCStrUtils, uPinyin, uAccentsUtils;

{ MatchesMask }
function MatchesMask(const FileName, Mask: String; const AOptions: TMaskOptions): Boolean;
var
  AMask: TMask;
begin
  if Mask <> '' then
  begin
    AMask := TMask.Create(Mask, AOptions);
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
function MatchesMaskList(const FileName, Mask: string; ASeparatorCharset: string; const AOptions: TMaskOptions): boolean;
var
  AMaskList: TMaskList;
begin
  if Mask <> '' then
  begin
    AMaskList := TMaskList.Create(Mask, ASeparatorCharset, AOptions);
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
constructor TMask.Create(const AValue: string; const AOptions: TMaskOptions);
begin
  FOriginal:= AValue;
  FTemplate:= AValue;
  FNormalize:= FileNameNormalized;
  FUsePinyin:= moPinyin in AOptions;
  FCaseSensitive := moCaseSensitive in AOptions;
  fIgnoreAccents := moIgnoreAccents in AOptions;
  fWindowsInterpretation := moWindowsMask in AOptions;
  UpdateTemplate;
end;

{ TMask.SetCaseSence }
procedure TMask.SetCaseSence(ACaseSence: Boolean);
begin
  FCaseSensitive:= ACaseSence;
  FTemplate:= FOriginal;
  UpdateTemplate;
end;

{ TMask.SetTemplate }
procedure TMask.SetTemplate(AValue: String);
begin
  FOriginal:= AValue;
  FTemplate:= AValue;
  UpdateTemplate;
end;

procedure TMask.UpdateTemplate;
begin
  // Let's set the mask early in straight letters if match attempt has to be with accent and ligature removed.
  if FIgnoreAccents then FTemplate := NormalizeAccentedChar(FTemplate);
  // Let's set the mask early in lowercase if match attempt has to be case insensitive.
  if not FCaseSensitive then FTemplate := UTF8LowerCase(FTemplate);
  if FNormalize then FTemplate := NormalizeFileName(FTemplate);

  // Treat mask differently for special cases:
  // 1. foo*.* -> foo*
  // 2. foo*. -> match foo*, but must not have an extension
  //    foo*. -> any file without extension ( .foo is a filename without extension according to Windows)
  // 3. foo. matches only foo but not foo.txt
  // 4. foo.* -> match either foo or foo.*
  if FWindowsInterpretation then
  begin
    // Mask: foo*.*
    if StrEnds(FTemplate, '*.*') then
    begin
      FTemplate := Copy(FTemplate, 1, Length(FTemplate) - 2);
    end
    // Mask: foo*. or *. or foo.
    else if (Length(FTemplate) > 1) and (StrEnds(FTemplate, '.')) then
    begin
      FTemplate := Copy(FTemplate, 1, Length(FTemplate) - 1);
    end;
  end;

  Update;
end;

{ TMask.Update }
procedure TMask.Update;
var
  I: Integer;
  S: UnicodeString;
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
      CharValue := S[I];
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
  S := CeUtf8ToUtf16(FTemplate);

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

  if FNormalize then
    sFilename := NormalizeFileName(sFilename);

  if not fWindowsInterpretation then
    Result := RegularMatches(sFileName)
  else
    Result := WindowsMatches(sFileName);
end;

{ TMask.RegularMatches }
function TMask.RegularMatches(const AFileName: string): boolean;
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
            if FUsePinyin then
            begin
              if not PinyinMatch(S[CharIndex], FMask.Chars[I].CharValue) then exit;
            end
            else
            begin
              if S[CharIndex] <> FMask.Chars[I].CharValue then Exit;
            end;
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
  S := CeUtf8ToUtf16(AFileName);
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
function TMask.WindowsMatches(const AFileName: string): boolean;
var
  Ext: String;
begin
  // Mask: foo*. or *. or foo.
  if (Length(FOriginal) > 1) and (StrEnds(FOriginal, '.')) then
  begin
    // if AFileName has an extension then Result is False, otherwise see if it RegularMatches foo*/foo
    // a filename like .foo under Windows is considered to be a file without an extension
    Ext := ExtractFileExt(AFileName);
    if (Ext = '') or (Ext = AFileName) then
    begin
      Result := RegularMatches(AFileName);
    end
    else
    begin
      Result := False;
    end;
  end
  // Mask: foo.* (but not '.*')
  else if (Length(FTemplate) > 2) and (StrEnds(FTemplate, '.*')) then
  begin
    // First see if we have 'foo'
    Result := (AFileName = Copy(FTemplate, 1, Length(FTemplate) - 2));
    if not Result then Result := RegularMatches(AFileName);
  end
  // All other cases just call RegularMatches()
  else begin
    Result := RegularMatches(AFileName);
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
constructor TMaskList.Create(const AValue: string; ASeparatorCharset: string; const AOptions: TMaskOptions);
var
  I: Integer;
  S: TParseStringList;
begin
  FMasks := TObjectList.Create(True);
  if AValue = '' then exit;

  S := TParseStringList.Create(AValue, ASeparatorCharset);
  try
    for I := 0 to S.Count - 1 do
      FMasks.Add(TMask.Create(S[I], AOptions));
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
function TMaskList.Matches(const AFileName: String): Boolean;
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

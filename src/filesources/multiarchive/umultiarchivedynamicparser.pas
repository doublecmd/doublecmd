{
   Double Commander
   -------------------------------------------------------------------------
   Multi archive dynamic parser

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

   Based on TFTPList (http://www.ararat.cz/synapse)
   Copyright (C) 1999-2011, Lukas Gebauer
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

   Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

   Neither the name of Lukas Gebauer nor the names of its contributors may
   be used to endorse or promote products derived from this software without
   specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
   ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
   DAMAGE.
}

unit uMultiArchiveDynamicParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMultiArc, uMultiArchiveParser;

type

  { TMultiArchiveDynamicParser }

  TMultiArchiveDynamicParser = class(TMultiArchiveParser)
  protected
    FLines: TStringList;
    FMasks: TStringList;
    FUnparsedLines: TStringList;
    BlockSize: string;
    PackBlockSize: string;
    FileName: string;
    FileExt: string;
    Day: string;
    Month: string;
    ThreeMonth: string;
    Year: string;
    Hours: string;
    HoursModif: Ansistring;
    Minutes: string;
    Seconds: string;
    Size: Ansistring;
    PackSize: AnsiString;
    Attributes: Ansistring;
    procedure ClearStore; virtual;
    function CheckValues: Boolean; virtual;
    procedure FillRecord(const Value: TArchiveItem); virtual;
    function ParseByMask(Value, NextValue, Mask: ansistring): Integer; virtual;
  public
    constructor Create(AMultiArcItem: TMultiArcItem); override;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Prepare; override;
    procedure ParseLines; override;
    procedure AddLine(const Str: String); override;

    class function NeedDynamic(Format: TStringList): Boolean;

    property Masks: TStringList read FMasks;
    property UnparsedLines: TStringList read FUnparsedLines;
    property OnGetArchiveItem: TOnGetArchiveItem write FOnGetArchiveItem;
  end;

implementation

uses
  DCDateTimeUtils, DCStrUtils;

{ TMultiArchiveDynamicParser }

constructor TMultiArchiveDynamicParser.Create(AMultiArcItem: TMultiArcItem);
begin
  inherited Create(AMultiArcItem);
  FLines := TStringList.Create;
  FMasks := AMultiArcItem.FFormat;
  FUnparsedLines := TStringList.Create;
end;

destructor TMultiArchiveDynamicParser.Destroy;
begin
  Clear;
  FLines.Free;
  FUnparsedLines.Free;
  inherited Destroy;
end;

procedure TMultiArchiveDynamicParser.Clear;
begin
  FLines.Clear;
  FUnparsedLines.Clear;
end;

procedure TMultiArchiveDynamicParser.Prepare;
begin

end;

procedure TMultiArchiveDynamicParser.ClearStore;
begin
  BlockSize := '';
  PackBlockSize := '';
  FileName := '';
  FileExt := '';
  Day := '';
  Month := '';
  ThreeMonth := '';
  Year := '';
  Hours := '';
  HoursModif := '';
  Minutes := '';
  Seconds := '';
  Size := '';
  PackSize := '';
  Attributes := '';
end;

function TMultiArchiveDynamicParser.ParseByMask(Value, NextValue, Mask: ansistring): Integer;
var
  Ivalue, IMask: integer;
  MaskC, LastMaskC: AnsiChar;
  c: AnsiChar;
  s: string;
begin
  ClearStore;
  Result := 0;
  if Value = '' then
    Exit;
  if Mask = '' then
    Exit;
  Ivalue := 1;
  IMask := 1;
  Result := 1;
  LastMaskC := ' ';
  while Imask <= Length(mask) do
  begin
    if (Mask[Imask] <> '+') and (Ivalue > Length(Value)) then
    begin
      Result := 0;
      Exit;
    end;
    MaskC := Mask[Imask];
    if Ivalue > Length(Value) then
      Exit;
    c := Value[Ivalue];
    case MaskC of
      'n':
        FileName := FileName + c;
      'e':
        FileExt := FileExt + c;
      'd':
        Day := Day + c;
      't':
        Month := Month + c;
      'T':
        ThreeMonth := ThreeMonth + c;
      'y':
        Year := Year + c;
      'h':
        Hours := Hours + c;
      'H':
        HoursModif := HoursModif + c;
      'm':
        Minutes := Minutes + c;
      's':
        Seconds := Seconds + c;
      'z':
        Size := Size + c;
      'p':
        PackSize := PackSize + c;
      'a':
        Attributes := Attributes + c;
      'x':
        if c <> ' ' then
          begin
            Result := 0;
            Exit;
          end;
      '+':
        begin
          s := '';
          if LastMaskC in ['n', 'e'] then
          begin
            if Imask = Length(Mask) then
              s := Copy(Value, IValue, Maxint)
            else
              while IValue <= Length(Value) do
              begin
                if Value[Ivalue] = ' ' then
                  break;
                s := s + Value[Ivalue];
                Inc(Ivalue);
              end;
            case LastMaskC of
              'n':
                FileName := FileName + s;
              'e':
                FileExt := FileExt + s;
            end;
          end
          else
          begin
            while IValue <= Length(Value) do
            begin
              if not(Value[Ivalue] in ['0'..'9']) then
                break;
              s := s + Value[Ivalue];
              Inc(Ivalue);
            end;
            case LastMaskC of
              'z':
                Size := Size + s;
              'p':
                PackSize := PackSize + s;
            end;
          end;
          Dec(IValue);
        end;
      '*':
        begin
          while IValue <= Length(Value) do
          begin
            if Value[Ivalue] = ' ' then
              break;
            Inc(Ivalue);
          end;
          while IValue <= Length(Value) do
          begin
            if Value[Ivalue] <> ' ' then
              break;
            Inc(Ivalue);
          end;
          Dec(IValue);
        end;
      '$':
        begin
          while IValue <= Length(Value) do
          begin
            if not(Value[Ivalue] in [' ', #9]) then
              break;
            Inc(Ivalue);
          end;
          Dec(IValue);
        end;
      '=':
        begin
          s := '';
          case LastmaskC of
            'z', 'p':
              begin
                while Imask <= Length(Mask) do
                begin
                  if not(Mask[Imask] in ['0'..'9']) then
                    break;
                  s := s + Mask[Imask];
                  Inc(Imask);
                end;
                Dec(Imask);
                case LastMaskC of
                  'z':
                    BlockSize := s;
                  'p':
                    PackBlockSize := s;
                end;
              end;
          end;
        end;
      '\':
        begin
          Value := NextValue;
          IValue := 0;
          Result := 2;
        end;
      '?': ;
    end;
    Inc(Ivalue);
    Inc(Imask);
    LastMaskC := MaskC;
  end;
end;

function TMultiArchiveDynamicParser.CheckValues: Boolean;
var
  x, n: integer;
begin
  Result := false;

  if (FileName = '') then
    Exit;
  if Day <> '' then
  begin
    Day := Trim(Day);
    x := StrToIntDef(day, -1);
    if (x < 1) or (x > 31) then
      Exit;
  end;
  if Month <> '' then
  begin
    Month := Trim(Month);
    x := StrToIntDef(Month, -1);
    if (x < 1) or (x > 12) then
      Exit;
  end;
  if Hours <> '' then
  begin
    Hours := Trim(Hours);
    x := StrToIntDef(Hours, -1);
    if (x < 0) or (x > 24) then
      Exit;
  end;
  if HoursModif <> '' then
  begin
    if not (HoursModif[1] in ['a', 'A', 'p', 'P']) then
      Exit;
  end;
  if Minutes <> '' then
  begin
    Minutes := Trim(Minutes);
    x := StrToIntDef(Minutes, -1);
    if (x < 0) or (x > 59) then
      Exit;
  end;
  if Seconds <> '' then
  begin
    Seconds := Trim(Seconds);
    x := StrToIntDef(Seconds, -1);
    if (x < 0) or (x > 59) then
      Exit;
  end;
  if Size <> '' then
  begin
    Size := Trim(Size);
    for n := 1 to Length(Size) do
      if not (Size[n] in ['0'..'9']) then
        Exit;
  end;
  if ThreeMonth <> '' then
  begin
    x := MonthToNumberDef(ThreeMonth, 0);
    if (x = 0) then
      Exit;
  end;
  if Year <> '' then
  begin
    Year := Trim(Year);
    x := StrToIntDef(Year, -1);
    if (x = -1) then
      Exit;
    if Length(Year) = 4 then
    begin
      if not((x > 1900) and (x < 2100)) then
        Exit;
    end
    else
      if Length(Year) = 2 then
      begin
        if not((x >= 0) and (x <= 99)) then
          Exit;
      end
      else
        if Length(Year) = 3 then
        begin
          if not((x >= 100) and (x <= 110)) then
            Exit;
        end
        else
          Exit;
  end;
  Result := True;
end;

procedure TMultiArchiveDynamicParser.FillRecord(const Value: TArchiveItem);
var
  X: Int64;
begin
  Value.FileName:= FGetFileName(FileName);
  Value.FileExt:= FGetFileName(FileExt);
  Value.PackSize:= StrToInt64Def(PackSize, 0);
  Value.UnpSize:= StrToInt64Def(Size, 0);
  Value.Year:= YearShortToLong(StrToIntDef(Year, 0));
  Value.Month:= StrToIntDef(Month, 1);
  Value.Day:= StrToIntDef(Day, 1);
  Value.Hour:= StrToIntDef(Hours, 0);
  Value.Minute:= StrToIntDef(Minutes, 0);
  Value.Second:= StrToIntDef(Seconds, 0);
  Value.Attributes:= FGetFileAttr(Attributes);

  if ThreeMonth <> '' then begin
    Value.Month:= MonthToNumberDef(ThreeMonth, 1);
  end;

  if HoursModif <> '' then begin
    Value.Hour:= TwelveToTwentyFour(Value.Hour, HoursModif);
  end;

  if BlockSize <> '' then begin
    X := StrToIntDef(BlockSize, 1);
    Value.UnpSize := X * Value.UnpSize;
  end;

  if PackBlockSize <> '' then begin
    X := StrToIntDef(PackBlockSize, 1);
    Value.PackSize := X * Value.PackSize;
  end;
end;

procedure TMultiArchiveDynamicParser.ParseLines;
var
  n, m: Integer;
  S: string;
  x: integer;
  b: Boolean;
begin
  n := 0;
  while n < FLines.Count do
  begin
    if n = FLines.Count - 1 then
      s := ''
    else
      s := FLines[n + 1];
    b := False;
    x := 0;
    for m := 0 to Masks.Count - 1 do
    begin
      x := ParseByMask(FLines[n], s, Masks[m]);
      if x > 0 then
        if CheckValues then
        begin
          if Assigned(FOnGetArchiveItem) then
          begin
            FArchiveItem := TArchiveItem.create;
            FillRecord(FArchiveItem);
            UpdateFileName;
            FOnGetArchiveItem(FArchiveItem);
          end;
          b := True;
          Break;
        end;
    end;
    if not b then
      FUnparsedLines.Add(FLines[n]);
    Inc(n);
    if x > 1 then
      Inc(n, x - 1);
  end;
end;

procedure TMultiArchiveDynamicParser.AddLine(const Str: String);
begin
  FLines.Add(Str);
end;

class function TMultiArchiveDynamicParser.NeedDynamic(Format: TStringList): Boolean;
var
  P: Integer;
  Index: Integer;
begin
  Result := False;
  for Index:= 0 to Format.Count - 1 do
  begin
    P:= Pos('+', Format[Index]);
    if (P > 0) and (P < Length(Format[Index])) then Exit(True);
    if ContainsOneOf(Format[Index], '$x=\') then Exit(True);
  end;
end;

end.


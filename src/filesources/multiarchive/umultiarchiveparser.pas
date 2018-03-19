unit uMultiArchiveParser;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, DCBasicTypes, uMultiArc;

type

  TGetFileAttr = function(sAttr: String): TFileAttrs;
  TGetFileName = function(const Str: String): String;
  TOnGetArchiveItem = procedure(ArchiveItem: TArchiveItem) of object;

  TKeyPos = record
    Index,
    Start,
    Count: longint;
  end;

type

  { TMultiArchiveParser }

  TMultiArchiveParser = class
  protected
    FArchiveItem: TArchiveItem;
    FGetFileAttr: TGetFileAttr;
    FGetFileName: TGetFileName;
    FMultiArcItem: TMultiArcItem;
    FOnGetArchiveItem: TOnGetArchiveItem;
  private
    procedure SplitFileName;
  protected
    procedure UpdateFileName;
  public
    constructor Create(AMultiArcItem: TMultiArcItem); virtual;
    procedure Prepare; virtual; abstract;
    procedure ParseLines; virtual; abstract;
    procedure AddLine(const Str: String); virtual; abstract;
    property OnGetArchiveItem: TOnGetArchiveItem write FOnGetArchiveItem;
  end;

  { TMultiArchiveStaticParser }

  TMultiArchiveStaticParser = class(TMultiArchiveParser)
  private
    FExtPos,
    FNamePos,
    FUnpSizePos,
    FPackSizePos,
    FYearPos,
    FMonthPos,
    FMonthNamePos,
    FDayPos,
    FHourPos,
    FHourModifierPos,
    FMinPos,
    FSecPos,
    FAttrPos: TKeyPos;
  private
    FFormatIndex: Integer;
  private
    function FixPosition(const Str: String; Key: TKeyPos): LongInt;
    function KeyPos(Key: char; out Position: TKeyPos): boolean;
    function GetKeyValue(const str: String; Key: TKeyPos): String;
  public
    procedure Prepare; override;
    procedure ParseLines; override;
    procedure AddLine(const Str: String); override;
  end;

implementation

uses
  LazUTF8, StrUtils, DCFileAttributes, DCDateTimeUtils;

function GetUnixFileName(const Str: String): String;
var
  I: Integer;
begin
  Result:= Str;
  for I:= 1 to Length(Str) do
    if Result[I] = '/' then Result[I]:= PathDelim;
end;

function GetWinFileName(const Str: String): String;
var
  I: Integer;
begin
  Result:= Str;
  for I:= 1 to Length(Str) do
    if Result[I] = '\' then Result[I]:= PathDelim;
end;

function GetDefFileName(const Str: String): String;
begin
  Result:= Str;
end;

{ TMultiArchiveParser }

procedure TMultiArchiveParser.SplitFileName;
var
  Index: Integer;
begin
  Index:= Pos(' -> ', FArchiveItem.FileName);
  if Index > 0 then
  begin
    FArchiveItem.FileLink:= Copy(FArchiveItem.FileName, Index + 4, MaxInt);
    FArchiveItem.FileName:= Copy(FArchiveItem.FileName, 1, Index - 1);
  end
end;

constructor TMultiArchiveParser.Create(AMultiArcItem: TMultiArcItem);
begin
  FMultiArcItem:= AMultiArcItem;

  with FMultiArcItem do
  begin
    // Setup function to process file attributes
    if (FFormMode and MAF_UNIX_ATTR) <> 0 then
      FGetFileAttr:= @UnixStrToFileAttr
    else if (FFormMode and MAF_WIN_ATTR) <> 0 then
      FGetFileAttr:= @WinStrToFileAttr
    else
      FGetFileAttr:= @StrToFileAttr;
    // Setup function to process file name
    if ((FFormMode and MAF_UNIX_PATH) <> 0) and (PathDelim <> '/') then
      FGetFileName:= @GetUnixFileName
    else if ((FFormMode and MAF_WIN_PATH) <> 0) and (PathDelim <> '\') then
      FGetFileName:= @GetWinFileName
    else
      FGetFileName:= @GetDefFileName;
  end;
end;

procedure TMultiArchiveParser.UpdateFileName;
begin
  with FArchiveItem do
  begin
    if ((Attributes and S_IFLNK) <> 0) or
       ((Attributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) then
      SplitFileName;

    if Length(FileExt) > 0 then begin
      FileName := FileName + ExtensionSeparator + FileExt;
    end;
  end;
end;

{ TMultiArchiveStaticParser }

function TMultiArchiveStaticParser.FixPosition(const Str: String; Key: TKeyPos): LongInt;
var
  I, K, U, C: LongInt;
  Format: String;
begin
  I:= 0;
  U:= 0;
  Result:= Key.Start;
  Format:= FMultiArcItem.FFormat[Key.Index];
  repeat
    C:= 0;
    I:= PosEx('*', Format, I + 1);
    if (I = 0) or (I >= Result) then Exit;
    if (I > 0) then
    begin
      I:= I + U;
      K:= I;
      while (K <= Length(Str)) and (Str[K] <> #32) do
        begin
          Inc(C);
          Inc(K);
        end;
      if C > 0 then
        U:= C - 1
      else
        U:= 0;
      Result:= Result + U;
    end;
  until I = 0;
end;

function TMultiArchiveStaticParser.KeyPos(Key: char; out Position: TKeyPos): boolean;
var
  I, L: Integer;
  Format: String;
begin
  Result := False;
  Position.Index := -1;
  for I := 0 to FMultiArcItem.FFormat.Count - 1 do
    with FMultiArcItem do
    begin
      Format := FFormat[I];
      Position.Start := Pos(Key, Format);
      if Position.Start = 0 then
        Continue;
      L := Length(Format);
      if (Position.Start = L - 1) and (Format[L] = '+') then
        Position.Count := MaxInt
      else begin
        Position.Count := Position.Start;
        while ((Position.Count <= L) and (Format[Position.Count] = Key)) do
          Inc(Position.Count);
        Position.Count := Position.Count - Position.Start;
      end;
      Position.Index := I;
      {$IFDEF DEBUG}
      DCDebug('Key: ' + Key, ' Format: ' + IntToStr(I), ' Start: ' + IntToStr(Position.Start), ' Count: ' + IntToStr(Position.Count));
      {$ENDIF}
      Result := True;
      Break;
    end;
end;

function TMultiArchiveStaticParser.GetKeyValue(const str: String; Key: TKeyPos): String;
begin
  Result:= Copy(str, FixPosition(str, Key), Key.Count);
end;

procedure TMultiArchiveStaticParser.Prepare;
begin
  // get positions of all properties
  KeyPos('e', FExtPos);  // file ext
  KeyPos('n', FNamePos);  // file name
  KeyPos('z', FUnpSizePos); // unpacked size
  KeyPos('p', FPackSizePos); // packed size
  KeyPos('y', FYearPos);
  KeyPos('t', FMonthPos);
  KeyPos('T', FMonthNamePos);
  KeyPos('d', FDayPos);
  KeyPos('h', FHourPos);
  KeyPos('H', FHourModifierPos);
  KeyPos('m', FMinPos);
  KeyPos('s', FSecPos);
  KeyPos('a', FAttrPos);
end;

procedure TMultiArchiveStaticParser.ParseLines;
begin

end;

procedure TMultiArchiveStaticParser.AddLine(const Str: String);
begin
  // if next item
  if FFormatIndex = 0 then
    FArchiveItem := TArchiveItem.Create;
  // get all file properties
  if FExtPos.Index = FFormatIndex then
    FArchiveItem.FileExt := FGetFileName(Trim(GetKeyValue(str, FExtPos)));
  if FNamePos.Index = FFormatIndex then
    FArchiveItem.FileName := FGetFileName(Trim(GetKeyValue(str, FNamePos)));
  if FUnpSizePos.Index = FFormatIndex then
    FArchiveItem.UnpSize := StrToInt64Def(Trim(GetKeyValue(str, FUnpSizePos)), 0);
  if FPackSizePos.Index = FFormatIndex then
    FArchiveItem.PackSize := StrToInt64Def(Trim(GetKeyValue(str, FPackSizePos)), 0);
  if FYearPos.Index = FFormatIndex then
    FArchiveItem.Year := YearShortToLong(StrToIntDef(Trim(GetKeyValue(str, FYearPos)), 0));
  if FMonthPos.Index = FFormatIndex then
    FArchiveItem.Month := StrToIntDef(Trim(GetKeyValue(str, FMonthPos)), 0);
  if FMonthNamePos.Index = FFormatIndex then
    FArchiveItem.Month := MonthToNumberDef(GetKeyValue(str, FMonthNamePos), 0);
  if FDayPos.Index = FFormatIndex then
    FArchiveItem.Day := StrToIntDef(Trim(GetKeyValue(str, FDayPos)), 0);
  if FHourPos.Index = FFormatIndex then
    FArchiveItem.Hour := StrToIntDef(Trim(GetKeyValue(str, FHourPos)), 0);
  if FHourModifierPos.Index = FFormatIndex then
    FArchiveItem.Hour := TwelveToTwentyFour(FArchiveItem.Hour, GetKeyValue(str, FHourModifierPos));
  if FMinPos.Index = FFormatIndex then
    FArchiveItem.Minute := StrToIntDef(Trim(GetKeyValue(str, FMinPos)), 0);
  if FSecPos.Index = FFormatIndex then
    FArchiveItem.Second := StrToIntDef(Trim(GetKeyValue(str, FSecPos)), 0);
  if FAttrPos.Index = FFormatIndex then
    FArchiveItem.Attributes := FGetFileAttr(GetKeyValue(str, FAttrPos));

  FFormatIndex := FFormatIndex + 1;
  if FFormatIndex >= FMultiArcItem.FFormat.Count then
  begin
    FFormatIndex := 0;
    UpdateFileName;
    {$IFDEF DEBUG}
    DCDebug('FileName: ', FArchiveItem.FileName);
    DCDebug('Size: ', IntToStr(FArchiveItem.UnpSize));
    DCDebug('Pack size: ', IntToStr(FArchiveItem.PackSize));
    DCDebug('Attributes: ', IntToStr(FArchiveItem.Attributes));
    DCDebug('-------------------------------------');
    {$ENDIF}
    if Assigned(FOnGetArchiveItem) then
      FOnGetArchiveItem(FArchiveItem);
  end;
end;

end.


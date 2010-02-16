unit uMultiArc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes;

type

  { TArchiveItem }

  TArchiveItem = class
    FileName,
    FileExt:  UTF8String;
    PackSize,
    UnpSize: Int64;
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Second: Word;
    MonthName: AnsiString;
    Attributes: TFileAttrs;
  end;

  { TMultiArcItem }

  TMultiArcItem = class
  public
    FArchiver,
    FDescription,
    FID,
    FIDPos,
    FExtension,
    FStart,
    FEnd: UTF8String;
    FFormat: TStringList;
    FList,
    FExtract,
    FTest,
    FDelete,
    FAdd,
    FMove: UTF8String;
  public
    FEnabled: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMultiArcList }

  TMultiArcList = class
    FList: TStringList;
  private
    function GetCount: LongInt;
    function GetItem(Index: Integer): TMultiArcItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName: UTF8String);
    procedure SaveToFile(const FileName: UTF8String);
    property Items[Index: Integer]: TMultiArcItem read GetItem;
    property Count: LongInt read GetCount;
  end;

implementation

uses
  LCLProc, StrUtils, uClassesEx, uDCUtils;

{ TMultiArcList }

function TMultiArcList.GetCount: LongInt;
begin
  Result:= FList.Count;
end;

function TMultiArcList.GetItem(Index: Integer): TMultiArcItem;
begin
  Result:= TMultiArcItem(FList.Objects[Index]);
end;

constructor TMultiArcList.Create;
begin
  FList:= TStringList.Create;
end;

destructor TMultiArcList.Destroy;
begin
  Clear;
  FreeThenNil(FList);
  inherited Destroy;
end;

procedure TMultiArcList.Clear;
var
  I: Integer;
begin
  for I:= FList.Count - 1 downto 0 do
    if Assigned(FList.Objects[I]) then
      begin
        FList.Objects[I].Free;
        FList.Objects[I]:= nil;
        FList.Delete(I);
      end;
end;

procedure TMultiArcList.LoadFromFile(const FileName: UTF8String);
var
  I, J: Integer;
  IniFile: TIniFileEx;
  Sections: TStringList;
  Section,
  Format: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  IniFile:= TIniFileEx.Create(FileName, fmOpenRead);
  Sections:= TStringList.Create;
  IniFile.ReadSections(Sections);
  for I:= 0 to Sections.Count - 1 do
    begin
      Section:= Sections[I];
      MultiArcItem:= TMultiArcItem.Create;
      with MultiArcItem do
      begin
        FArchiver:= TrimQuotes(IniFile.ReadString(Section, 'Archiver', EmptyStr));
        FDescription:= TrimQuotes(IniFile.ReadString(Section, 'Description', EmptyStr));
        FID:= TrimQuotes(IniFile.ReadString(Section, 'ID', EmptyStr));
        FIDPos:= TrimQuotes(IniFile.ReadString(Section, 'IDPos', EmptyStr));
        FExtension:= TrimQuotes(IniFile.ReadString(Section, 'Extension', EmptyStr));
        FStart:= TrimQuotes(IniFile.ReadString(Section, 'Start', EmptyStr));
        FEnd:= TrimQuotes(IniFile.ReadString(Section, 'End', EmptyStr));
        for J:= 0 to 50 do
        begin
          Format:= TrimQuotes(IniFile.ReadString(Section, 'Format' + IntToStr(J), EmptyStr));
          if Format <> EmptyStr then
            FFormat.Add(Format)
          else
            Break;
        end;
        FList:= TrimQuotes(IniFile.ReadString(Section, 'List', EmptyStr));
        FExtract:= TrimQuotes(IniFile.ReadString(Section, 'Extract', EmptyStr));
        FTest:= TrimQuotes(IniFile.ReadString(Section, 'Test', EmptyStr));
        FDelete:= TrimQuotes(IniFile.ReadString(Section, 'Delete', EmptyStr));
        FAdd:= TrimQuotes(IniFile.ReadString(Section, 'Add', EmptyStr));
        FMove:= TrimQuotes(IniFile.ReadString(Section, 'Move', EmptyStr));
      end;
      FList.AddObject(Section, MultiArcItem);
    end;
end;

procedure TMultiArcList.SaveToFile(const FileName: UTF8String);
var
  I, J: Integer;
  IniFile: TIniFileEx;
  Section: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  IniFile:= TIniFileEx.Create(FileName);
  for I:= 0 to FList.Count - 1 do
    begin
      Section:= FList.Strings[I];
      MultiArcItem:= TMultiArcItem(FList.Objects[I]);
      with MultiArcItem do
      begin
        IniFile.WriteString(Section, 'Archiver', FArchiver);
        IniFile.WriteString(Section, 'Description', FDescription);
        IniFile.WriteString(Section, 'ID', FID);
        IniFile.WriteString(Section, 'IDPos', FIDPos);
        IniFile.WriteString(Section, 'Extension', FExtension);
        IniFile.WriteString(Section, 'Start', FStart);
        IniFile.WriteString(Section, 'End', FEnd);
        for J:= 0 to FFormat.Count - 1 do
        begin
          IniFile.WriteString(Section, 'Format' + IntToStr(J), FFormat[J]);
        end;
        IniFile.WriteString(Section, 'List', FList);
        IniFile.WriteString(Section, 'Extract', FExtract);
        IniFile.WriteString(Section, 'Test', FTest);
        IniFile.WriteString(Section, 'Delete', FDelete);
        IniFile.WriteString(Section, 'Add', FAdd);
        IniFile.WriteString(Section, 'Move', FMove);
      end;
    end;
end;

{ TMultiArcItem }

constructor TMultiArcItem.Create;
begin
  FFormat:= TStringList.Create;
end;

destructor TMultiArcItem.Destroy;
begin
  if Assigned(FFormat) then
    FreeAndNil(FFormat);
  inherited Destroy;
end;

end.


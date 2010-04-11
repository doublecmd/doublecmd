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
    FAddMultiVolume,
    FAddSelfExtract: UTF8String;
  public
    FEnabled: Boolean;
    FOutput: Boolean;
    FDebug: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMultiArcList }

  TMultiArcList = class
    FList: TStringList;
  private
    function GetCount: LongInt;
    function GetItem(Index: Integer): TMultiArcItem;
    function GetName(Index: Integer): UTF8String;
    procedure SetName(Index: Integer; const AValue: UTF8String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName: UTF8String);
    procedure SaveToFile(const FileName: UTF8String);
    function Add(const S: UTF8String; aMultiArcItem: TMultiArcItem): Integer;
    procedure Delete(Index: Integer);
    property Names[Index: Integer]: UTF8String read GetName write SetName;
    property Items[Index: Integer]: TMultiArcItem read GetItem; default;
    property Count: LongInt read GetCount;
  end;

implementation

uses
  LCLProc, uClassesEx, uDCUtils, uOSUtils;

{ TMultiArcList }

function TMultiArcList.GetCount: LongInt;
begin
  Result:= FList.Count;
end;

function TMultiArcList.GetItem(Index: Integer): TMultiArcItem;
begin
  Result:= TMultiArcItem(FList.Objects[Index]);
end;

function TMultiArcList.GetName(Index: Integer): UTF8String;
begin
  Result:= FList.Strings[Index];
end;

procedure TMultiArcList.SetName(Index: Integer; const AValue: UTF8String);
begin
  FList.Strings[Index]:= AValue;
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
  IniFile: TIniFileEx = nil;
  Sections: TStringList = nil;
  Section,
  Format: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  try
    IniFile:= TIniFileEx.Create(FileName, fmOpenRead);
    Sections:= TStringList.Create;
    IniFile.ReadSections(Sections);
    for I:= 0 to Sections.Count - 1 do
    begin
      Section:= Sections[I];
      MultiArcItem:= TMultiArcItem.Create;
      with MultiArcItem do
      begin
        FArchiver:= FixExeExt(TrimQuotes(IniFile.ReadString(Section, 'Archiver', EmptyStr)));
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
        FAddMultiVolume:= TrimQuotes(IniFile.ReadString(Section, 'AddMultiVolume', EmptyStr));
        FAddSelfExtract:= TrimQuotes(IniFile.ReadString(Section, 'AddSelfExtract', EmptyStr));
        // optional
        FEnabled:= IniFile.ReadBool(Section, 'Enabled', True);
        FOutput:= IniFile.ReadBool(Section, 'Output', False);
        FDebug:= IniFile.ReadBool(Section, 'Debug', False);
      end;
      FList.AddObject(Section, MultiArcItem);
    end;
  finally
    FreeThenNil(IniFile);
    FreeThenNil(Sections);
  end;
end;

procedure TMultiArcList.SaveToFile(const FileName: UTF8String);
var
  I, J: Integer;
  IniFile: TIniFileEx;
  Section: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  mbDeleteFile(FileName + '.bak');
  mbRenameFile(FileName, FileName + '.bak');
  try
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
        IniFile.WriteString(Section, 'AddMultiVolume', FAddMultiVolume);
        IniFile.WriteString(Section, 'AddSelfExtract', FAddSelfExtract);
        // optional
        IniFile.WriteBool(Section, 'Enabled', FEnabled);
        IniFile.WriteBool(Section, 'Output', FOutput);
        IniFile.WriteBool(Section, 'Debug', FDebug);
      end;
    end;
  finally
    FreeThenNil(IniFile);
  end;
end;

function TMultiArcList.Add(const S: UTF8String; aMultiArcItem: TMultiArcItem): Integer;
begin
  Result := FList.AddObject(S, aMultiArcItem);
end;

procedure TMultiArcList.Delete(Index: Integer);
begin
  Items[Index].Free;
  FList.Delete(Index);
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


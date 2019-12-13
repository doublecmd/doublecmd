{
   Double Commander
   -------------------------------------------------------------------------
   Implementation of multi archiver support

   Copyright (C) 2010-2019  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uMultiArc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes, uMasks;

const
  MaxSignSize = 1024;
  SignSeekRange = 1024 * 1024;

const
  MAF_UNIX_PATH        = 1; // Use Unix path delimiter (/)
  MAF_WIN_PATH         = 2; // Use Windows path delimiter (\)
  MAF_UNIX_ATTR        = 4; // Use Unix file attributes
  MAF_WIN_ATTR         = 8; // Use Windows file attributes

type
  TMultiArcFlag = (mafFileNameList);
  TMultiArcFlags = set of TMultiArcFlag;

type

  TSignature = record
    Value: array[0..Pred(MaxSignSize)] of Byte;
    Size: LongInt;
  end;
  PSignature = ^TSignature;

  { TSignatureList }

  TSignatureList = class(TFPList)
  private
    function GetSignature(Index: Integer): PSignature;
  public
    destructor Destroy; override;
    procedure Clean;
    property Items[Index: Integer]: PSignature read GetSignature; default;
  end;

  TSignaturePosition = record
    Value: LongInt;
    Sign: Boolean;
  end;
  PSignaturePosition = ^TSignaturePosition;

  { TSignaturePositionList }

  TSignaturePositionList = class(TFPList)
  private
    function GetSignaturePosition(Index: Integer): PSignaturePosition;
  public
    destructor Destroy; override;
    procedure Clean;
    property Items[Index: Integer]: PSignaturePosition read GetSignaturePosition; default;
  end;

  { TArchiveItem }

  TArchiveItem = class
    FileName,
    FileExt,
    FileLink:  String;
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
  private
    FExt: String;
    FMaskList: TMaskList;
    FSeekAfterSignPos: Boolean;
    FSignature,
    FSignaturePosition: AnsiString;
    FSignatureSeekRange: LongInt;
    FSignatureList: TSignatureList;
    FSignaturePositionList: TSignaturePositionList;
    function GetSignatureSeekRange: AnsiString;
    procedure SetExtension(const AValue: String);
    procedure SetSignature(const AValue: AnsiString);
    procedure SetSignaturePosition(const AValue: AnsiString);
    procedure SetSignatureSeekRange(const AValue: AnsiString);
  public
    FArchiver,
    FDescription,
    FStart,
    FEnd: String;
    FFormat: TStringList;
    FList,
    FExtract,
    FExtractWithoutPath,
    FTest,
    FDelete,
    FAdd,
    FAddSelfExtract,
    FPasswordQuery: String;
    FFormMode: Integer;
    FFlags: TMultiArcFlags;
  public
    FEnabled: Boolean;
    FOutput: Boolean;
    FDebug: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Matches(const AFileName: String): Boolean;
    function CanYouHandleThisFile(const FileName: String): Boolean;
    function Clone: TMultiArcItem;
    property FExtension: String read FExt write SetExtension;
    property FID: AnsiString read FSignature write SetSignature;
    property FIDPos: AnsiString read FSignaturePosition write SetSignaturePosition;
    property FIDSeekRange: AnsiString read GetSignatureSeekRange write SetSignatureSeekRange;
  end;

  { TMultiArcList }

  TMultiArcList = class
    FList: TStringList;
  private
    function GetCount: LongInt;
    function GetItem(Index: Integer): TMultiArcItem;
    function GetName(Index: Integer): String;
    procedure SetName(Index: Integer; const AValue: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AutoConfigure;
    procedure Clear;
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    function Add(const S: String; aMultiArcItem: TMultiArcItem): Integer;
    function Insert(Index: integer; const S: string; aMultiArcItem: TMultiArcItem): integer;
    function Clone: TMultiArcList;
    function ComputeSignature(Seed: dword = $00000000): dword;
    procedure Delete(Index: Integer);
    property Names[Index: Integer]: String read GetName write SetName;
    property Items[Index: Integer]: TMultiArcItem read GetItem; default;
    property Count: LongInt read GetCount;
  end;

implementation

uses
  crc, LCLProc, StrUtils, Math, FileUtil, DCClassesUtf8, uDCUtils, DCOSUtils,
  DCStrUtils;

{ TMultiArcList }

function TMultiArcList.GetCount: LongInt;
begin
  Result:= FList.Count;
end;

function TMultiArcList.GetItem(Index: Integer): TMultiArcItem;
begin
  Result:= TMultiArcItem(FList.Objects[Index]);
end;

function TMultiArcList.GetName(Index: Integer): String;
begin
  Result:= FList.Strings[Index];
end;

procedure TMultiArcList.SetName(Index: Integer; const AValue: String);
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

procedure TMultiArcList.AutoConfigure;
var
  I: Integer;
  ExePath: String;
begin
  for I:= 0 to Count - 1 do
  begin
    ExePath:= Items[I].FArchiver;
    if not mbFileExists(ReplaceEnvVars(ExePath)) then
      ExePath:= FindDefaultExecutablePath(ExePath);
    if ExePath = EmptyStr then
      Items[I].FEnabled:= False
    else
      begin
        Items[I].FArchiver:= ExePath;
        Items[I].FEnabled:= True;
      end;
  end;
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

procedure TMultiArcList.LoadFromFile(const FileName: String);
var
  I, J: Integer;
  IniFile: TIniFileEx = nil;
  Sections: TStringList = nil;
  Section,
  Format: String;
  FirstTime: Boolean = True;
  MultiArcItem: TMultiArcItem;
begin
  Self.Clear;
  IniFile:= TIniFileEx.Create(FileName, fmOpenRead);
  try
    Sections:= TStringList.Create;
    IniFile.ReadSections(Sections);
    for I:= 0 to Sections.Count - 1 do
    begin
      Section:= Sections[I];
      if SameText(Section, 'MultiArc') then
      begin
        FirstTime:= IniFile.ReadBool(Section, 'FirstTime', True);
        Continue;
      end;
      MultiArcItem:= TMultiArcItem.Create;
      with MultiArcItem do
      begin
        FArchiver:= FixExeExt(TrimQuotes(IniFile.ReadString(Section, 'Archiver', EmptyStr)));
        FDescription:= TrimQuotes(IniFile.ReadString(Section, 'Description', EmptyStr));
        FID:= TrimQuotes(IniFile.ReadString(Section, 'ID', EmptyStr));
        FIDPos:= TrimQuotes(IniFile.ReadString(Section, 'IDPos', EmptyStr));
        FIDSeekRange:= IniFile.ReadString(Section, 'IDSeekRange', EmptyStr);
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
        FExtractWithoutPath:= TrimQuotes(IniFile.ReadString(Section, 'ExtractWithoutPath', EmptyStr));
        FTest:= TrimQuotes(IniFile.ReadString(Section, 'Test', EmptyStr));
        FDelete:= TrimQuotes(IniFile.ReadString(Section, 'Delete', EmptyStr));
        FAdd:= TrimQuotes(IniFile.ReadString(Section, 'Add', EmptyStr));
        FAddSelfExtract:= TrimQuotes(IniFile.ReadString(Section, 'AddSelfExtract', EmptyStr));
        FPasswordQuery:= IniFile.ReadString(Section, 'PasswordQuery', EmptyStr);
        // optional
        FFlags:= TMultiArcFlags(IniFile.ReadInteger(Section, 'Flags', 0));
        FFormMode:= IniFile.ReadInteger(Section, 'FormMode', 0);
        FEnabled:= IniFile.ReadBool(Section, 'Enabled', True);
        FOutput:= IniFile.ReadBool(Section, 'Output', False);
        FDebug:= IniFile.ReadBool(Section, 'Debug', False);
      end;
      FList.AddObject(Section, MultiArcItem);
    end;
    if FirstTime then AutoConfigure;
  finally
    FreeThenNil(IniFile);
    FreeThenNil(Sections);
  end;
end;

procedure TMultiArcList.SaveToFile(const FileName: String);
var
  I, J: Integer;
  IniFile: TIniFileEx;
  Section: String;
  MultiArcItem: TMultiArcItem;
begin
  IniFile:= TIniFileEx.Create(FileName, fmOpenWrite);
  try
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
        IniFile.WriteString(Section, 'IDSeekRange', FIDSeekRange);
        IniFile.WriteString(Section, 'Extension', FExtension);
        IniFile.WriteString(Section, 'Start', FStart);
        IniFile.WriteString(Section, 'End', FEnd);
        for J:= 0 to FFormat.Count - 1 do
        begin
          IniFile.WriteString(Section, 'Format' + IntToStr(J), FFormat[J]);
        end;
        IniFile.WriteString(Section, 'List', FList);
        IniFile.WriteString(Section, 'Extract', FExtract);
        IniFile.WriteString(Section, 'ExtractWithoutPath', FExtractWithoutPath);
        IniFile.WriteString(Section, 'Test', FTest);
        IniFile.WriteString(Section, 'Delete', FDelete);
        IniFile.WriteString(Section, 'Add', FAdd);
        IniFile.WriteString(Section, 'AddSelfExtract', FAddSelfExtract);
        IniFile.WriteString(Section, 'PasswordQuery', FPasswordQuery);
        // optional
        IniFile.WriteInteger(Section, 'Flags', Integer(FFlags));
        IniFile.WriteInteger(Section, 'FormMode', FFormMode);
        IniFile.WriteBool(Section, 'Enabled', FEnabled);
        IniFile.WriteBool(Section, 'Output', FOutput);
        IniFile.WriteBool(Section, 'Debug', FDebug);
      end;
    end;
    IniFile.WriteBool('MultiArc', 'FirstTime', False);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

function TMultiArcList.Add(const S: String; aMultiArcItem: TMultiArcItem): Integer;
begin
  Result := FList.AddObject(S, aMultiArcItem);
end;

function TMultiArcList.Insert(Index: integer; const S: string; aMultiArcItem: TMultiArcItem): integer;
begin
  try
    FList.InsertObject(Index, S, aMultiArcItem);
    Result := Index;
  except
    Result := -1;
  end;
end;
procedure TMultiArcList.Delete(Index: Integer);
begin
  Items[Index].Free;
  FList.Delete(Index);
end;
function TMultiArcList.Clone: TMultiArcList;
var
  Index: integer;
begin
  Result := TMultiArcList.Create;
  for Index := 0 to pred(Self.Count) do
    Result.Add(Self.FList.Strings[Index], Self.Items[Index].Clone);
end;
{ TMultiArcList.ComputeSignature }
// Routine tries to pickup all char chain from element of all entries and compute a unique CRC32.
// This CRC32 will be a kind of signature of the MultiArc settings.
function TMultiArcList.ComputeSignature(Seed: dword): dword;
  procedure UpdateSignature(sInfo: string);
  begin
    if length(sInfo) > 0 then
      Result := crc32(Result, @sInfo[1], length(sInfo));
  end;
var
  Index, iInnerIndex: integer;
begin
  Result := Seed;
  for Index := 0 to pred(Count) do
  begin
    UpdateSignature(Self.FList.Strings[Index]);
    UpdateSignature(Self.Items[Index].FDescription);
    UpdateSignature(Self.Items[Index].FArchiver);
    UpdateSignature(Self.Items[Index].FExtension);
    UpdateSignature(Self.Items[Index].FList);
    UpdateSignature(Self.Items[Index].FStart);
    UpdateSignature(Self.Items[Index].FEnd);
    for iInnerIndex := 0 to pred(Self.Items[Index].FFormat.Count) do
      UpdateSignature(Self.Items[Index].FFormat.Strings[iInnerIndex]);
    UpdateSignature(Self.Items[Index].FExtract);
    UpdateSignature(Self.Items[Index].FAdd);
    UpdateSignature(Self.Items[Index].FDelete);
    UpdateSignature(Self.Items[Index].FTest);
    UpdateSignature(Self.Items[Index].FExtractWithoutPath);
    UpdateSignature(Self.Items[Index].FAddSelfExtract);
    UpdateSignature(Self.Items[Index].FPasswordQuery);
    UpdateSignature(Self.Items[Index].FID);
    UpdateSignature(Self.Items[Index].FIDPos);
    UpdateSignature(Self.Items[Index].FIDSeekRange);
    Result := crc32(Result, @Self.Items[Index].FFlags, sizeof(Self.Items[Index].FFlags));
    Result := crc32(Result, @Self.Items[Index].FFormMode, sizeof(Self.Items[Index].FFormMode));
    Result := crc32(Result, @Self.Items[Index].FEnabled, sizeof(Self.Items[Index].FEnabled));
    Result := crc32(Result, @Self.Items[Index].FOutput, sizeof(Self.Items[Index].FOutput));
    Result := crc32(Result, @Self.Items[Index].FDebug, sizeof(Self.Items[Index].FDebug));
  end;
end;

{ TMultiArcItem }

function TMultiArcItem.GetSignatureSeekRange: AnsiString;
begin
  if FSignatureSeekRange = SignSeekRange then
    Result:= EmptyStr
  else
    Result:= IntToStr(FSignatureSeekRange);
end;

procedure TMultiArcItem.SetExtension(const AValue: String);
var
  AMask: String;
  Index: Integer;
  AMaskList: TStringArray;
begin
  if FExt <> AValue then
  begin
    FExt:= AValue;
    AMask:= EmptyStr;
    FreeAndNil(FMaskList);
    AMaskList:= SplitString(AValue, ',');
    for Index:= Low(AMaskList) to High(AMaskList) do
    begin
      AddStrWithSep(AMask, AllFilesMask + ExtensionSeparator + AMaskList[Index], ',');
    end;
    FMaskList:= TMaskList.Create(AMask, ',');
  end;
end;

procedure TMultiArcItem.SetSignature(const AValue: AnsiString);
var
  I: Integer;
  Sign: AnsiString;
  Value: AnsiString;
  Signature: PSignature;
begin
  FSignature:= AValue;
  FSignatureList.Clean;
  if AValue = EmptyStr then Exit;
  Value:= AValue;
  repeat
    I:= 0;
    New(Signature);
    Sign:= Trim(Copy2SymbDel(Value, ','));
    try
      while (Sign <> EmptyStr) and (I < MaxSignSize) do
      begin
       Signature^.Value[I]:= StrToInt('$' + Copy2SymbDel(Sign, #32));
       Inc(I);
      end;
      Signature^.Size:= I;
      FSignatureList.Add(Signature);
    except
      Dispose(Signature);
    end;
  until Value = EmptyStr;
end;

procedure TMultiArcItem.SetSignaturePosition(const AValue: AnsiString);
var
  SignPos,
  Value: AnsiString;
  SignaturePosition: PSignaturePosition;
begin
  FSignaturePosition:= AValue;
  FSignaturePositionList.Clean;
  if AValue = EmptyStr then Exit;
  Value:= StringReplace(AValue, '0x', '$', [rfReplaceAll]);
  repeat
    SignPos:= Trim(Copy2SymbDel(Value, ','));
    if SignPos = '<SeekID>' then
      FSeekAfterSignPos:= True
    else
      try
        New(SignaturePosition);
        SignaturePosition^.Value:= StrToInt(SignPos);
        SignaturePosition^.Sign:= not (SignaturePosition^.Value < 0);
        SignaturePosition^.Value:= abs(SignaturePosition^.Value);
        FSignaturePositionList.Add(SignaturePosition);
      except
        Dispose(SignaturePosition);
      end;
  until Value = EmptyStr;
end;

procedure TMultiArcItem.SetSignatureSeekRange(const AValue: AnsiString);
begin
  if not TryStrToInt(AValue, FSignatureSeekRange) then
    FSignatureSeekRange:= SignSeekRange;
end;

constructor TMultiArcItem.Create;
begin
  FSignatureList:= TSignatureList.Create;
  FSignaturePositionList:= TSignaturePositionList.Create;
  FFormat:= TStringList.Create;
end;

destructor TMultiArcItem.Destroy;
begin
  FreeAndNil(FMaskList);
  FreeAndNil(FSignatureList);
  FreeAndNil(FSignaturePositionList);
  FreeAndNil(FFormat);
  inherited Destroy;
end;

function TMultiArcItem.Matches(const AFileName: String): Boolean;
begin
  if (FMaskList = nil) then
    Result:= False
  else
    Result:= FMaskList.Matches(AFileName);
end;

function TMultiArcItem.CanYouHandleThisFile(const FileName: String): Boolean;
var
  FileMapRec : TFileMapRec;
  hFile: THandle;
  I, J: LongInt;
  lpBuffer: PByte = nil;
  Origin: LongInt;
  dwMaxSignSize: LongWord = 0;
  dwReaded: LongWord;
  dwOffset: LongWord = 0;
begin
  Result:= False;
  hFile:= mbFileOpen(FileName,  fmOpenRead or fmShareDenyNone);
  if hFile <> feInvalidHandle then
  begin
    // Determine maximum signature size
    for J:= 0 to FSignatureList.Count - 1 do
      dwMaxSignSize := Max(FSignatureList[J]^.Size, dwMaxSignSize);
    {
    if (FSkipSfxPart) then
      dwOffset := FSfxOffset
    }
    lpBuffer:= GetMem(dwMaxSignSize);
    if Assigned(lpBuffer) then
    try
      // Try to determine by IDPOS
      for I:= 0 to FSignaturePositionList.Count - 1 do
      begin
        case FSignaturePositionList[I]^.Sign of
          True: Origin:= fsFromBeginning;
          False: Origin:= fsFromEnd;
        end;
        if (FileSeek(hFile, dwOffset + FSignaturePositionList[I]^.Value, Origin) <> -1) then
        begin
          dwReaded:= FileRead(hFile, lpBuffer^, dwMaxSignSize);
          if (dwReaded = dwMaxSignSize) then
          begin
            for J := 0 to FSignatureList.Count - 1 do
            begin
              if(CompareByte(lpBuffer^, FSignatureList[J]^.Value, FSignatureList[J]^.Size) = 0) then
                Exit(True);
            end;
          end;
        end;
      end;
    finally
      FreeMem(lpBuffer);
      FileClose(hFile);
    end; // if Assigned(lpBuffer)
  end;

  // Try raw seek id
  if (Result = False) and FSeekAfterSignPos then
  begin
    FillByte(FileMapRec, SizeOf(FileMapRec), 0);
    if MapFile(FileName, FileMapRec) then
    try
      dwOffset:= Min(FSignatureSeekRange, FileMapRec.FileSize);
      for I:= 0 to dwOffset do
      begin
        for J:= 0 to FSignatureList.Count - 1 do
        begin
          if(CompareByte((FileMapRec.MappedFile + I)^, FSignatureList[J]^.Value, FSignatureList[J]^.Size) = 0) then
            Exit(True);
        end;
      end;
    finally
      UnMapFile(FileMapRec);
    end;
  end;
end;

function TMultiArcItem.Clone: TMultiArcItem;
begin
  Result := TMultiArcItem.Create;
  //Keep elements in some ordre a when loading them from the .ini, it will be simpler to validate if we are missing one.
  Result.FArchiver := Self.FArchiver;
  Result.FDescription := Self.FDescription;
  Result.FID := Self.FID;
  Result.FIDPos := Self.FIDPos;
  Result.FIDSeekRange := Self.FIDSeekRange;
  Result.FExtension := Self.FExtension;
  Result.FStart := Self.FStart;
  Result.FEnd := Self.FEnd;
  Result.FFormat.Assign(Self.FFormat);
  Result.FList := Self.FList;
  Result.FExtract := Self.FExtract;
  Result.FExtractWithoutPath := Self.FExtractWithoutPath;
  Result.FTest := Self.FTest;
  Result.FDelete := Self.FDelete;
  Result.FAdd := Self.FAdd;
  Result.FAddSelfExtract := Self.FAddSelfExtract;
  Result.FPasswordQuery := Self.FPasswordQuery;
  Result.FFlags := Self.FFlags;
  Result.FFormMode := Self.FFormMode;
  Result.FEnabled := Self.FEnabled;
  Result.FOutput := Self.FOutput;
  Result.FDebug := Self.FDebug;
end;

{ TSignatureList }

function TSignatureList.GetSignature(Index: Integer): PSignature;
begin
  Result:= PSignature(Get(Index));
end;

destructor TSignatureList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TSignatureList.Clean;
var
  I: Integer;
begin
  for I:= Count - 1 downto 0 do
  begin
    Dispose(Items[I]);
    Delete(I);
  end;
end;

{ TSignaturePositionList }

function TSignaturePositionList.GetSignaturePosition(Index: Integer): PSignaturePosition;
begin
  Result:= PSignaturePosition(Get(Index));
end;

destructor TSignaturePositionList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TSignaturePositionList.Clean;
var
  I: Integer;
begin
  for I:= Count - 1 downto 0 do
  begin
    Dispose(Items[I]);
    Delete(I);
  end;
end;

end.


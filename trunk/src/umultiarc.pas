{
   Double Commander
   -------------------------------------------------------------------------
   Implementation of multi archiver support

   Copyright (C) 2010-2012  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, DCBasicTypes;

const
  MaxSignSize = 1024;
  SignSeekRange = 1024 * 1024;

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
    FileLink:  UTF8String;
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
    FSeekAfterSignPos: Boolean;
    FSignature,
    FSignaturePosition: AnsiString;
    FSignatureSeekRange: LongInt;
    FSignatureList: TSignatureList;
    FSignaturePositionList: TSignaturePositionList;
    function GetSignatureSeekRange: AnsiString;
    procedure SetSignature(const AValue: AnsiString);
    procedure SetSignaturePosition(const AValue: AnsiString);
    procedure SetSignatureSeekRange(const AValue: AnsiString);
  public
    FArchiver,
    FDescription,
    FExtension,
    FStart,
    FEnd: UTF8String;
    FFormat: TStringList;
    FList,
    FExtract,
    FExtractWithoutPath,
    FTest,
    FDelete,
    FAdd,
    FAddSelfExtract,
    FPasswordQuery: UTF8String;
    FFormMode: Integer;
  public
    FEnabled: Boolean;
    FOutput: Boolean;
    FDebug: Boolean;
    constructor Create;
    destructor Destroy; override;
    function CanYouHandleThisFile(const FileName: UTF8String): Boolean;
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
    function GetName(Index: Integer): UTF8String;
    procedure SetName(Index: Integer; const AValue: UTF8String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AutoConfigure;
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
  LCLProc, StrUtils, Math, FileUtil, DCClassesUtf8, uDCUtils, DCOSUtils;

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

procedure TMultiArcList.AutoConfigure;
var
  I: Integer;
  ExePath: UTF8String;
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

procedure TMultiArcList.LoadFromFile(const FileName: UTF8String);
var
  I, J: Integer;
  IniFile: TIniFileEx = nil;
  Sections: TStringList = nil;
  Section,
  Format: UTF8String;
  FirstTime: Boolean = True;
  MultiArcItem: TMultiArcItem;
begin
  try
    IniFile:= TIniFileEx.Create(FileName, fmOpenRead);
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

procedure TMultiArcList.SaveToFile(const FileName: UTF8String);
var
  I, J: Integer;
  IniFile: TIniFileEx;
  Section: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  try
    IniFile:= TIniFileEx.Create(FileName, fmOpenWrite);
    try
      IniFile.Clear;
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
          IniFile.WriteInteger(Section, 'FormMode', FFormMode);
          IniFile.WriteBool(Section, 'Enabled', FEnabled);
          IniFile.WriteBool(Section, 'Output', FOutput);
          IniFile.WriteBool(Section, 'Debug', FDebug);
        end;
      end;
      IniFile.WriteBool('MultiArc', 'FirstTime', False);
    finally
      IniFile.Free;
    end;
  except

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

function TMultiArcItem.GetSignatureSeekRange: AnsiString;
begin
  if FSignatureSeekRange = SignSeekRange then
    Result:= EmptyStr
  else
    Result:= IntToStr(FSignatureSeekRange);
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
  I:= 0;
  Value:= AValue;
  repeat
    New(Signature);
    Sign:= Copy2SymbDel(Value, ',');
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
  FreeThenNil(FSignatureList);
  FreeThenNil(FSignaturePositionList);
  FreeThenNil(FFormat);
  inherited Destroy;
end;

function TMultiArcItem.CanYouHandleThisFile(const FileName: UTF8String): Boolean;
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


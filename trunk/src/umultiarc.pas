{
   Double Commander
   -------------------------------------------------------------------------
   Implementation of multi archiver support

   Copyright (C) 2010  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, uTypes;

const
  MaxSignSize = 1024;

type

  TSignature = array[0..Pred(MaxSignSize)] of Byte;
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
  private
    FSignature,
    FSignaturePosition: AnsiString;
    FSignatureList: TSignatureList;
    FSignaturePositionList: TSignaturePositionList;
    procedure SetSignature(const AValue: AnsiString);
    procedure SetSignaturePosition(const AValue: AnsiString);
  public
    FArchiver,
    FDescription,
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
    FIDSeekRange: LongInt;
  public
    FEnabled: Boolean;
    FOutput: Boolean;
    FDebug: Boolean;
    constructor Create;
    destructor Destroy; override;
    function CanYouHandleThisFile(const FileName: UTF8String): Boolean;
    property FID: AnsiString read FSignature write SetSignature;
    property FIDPos: AnsiString read FSignaturePosition write SetSignaturePosition;
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
  LCLProc, StrUtils, uClassesEx, uDCUtils, uOSUtils;

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
        FIDSeekRange:= IniFile.ReadInteger(Section, 'IDSeekRange', 0);
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
        IniFile.WriteInteger(Section, 'IDSeekRange', FIDSeekRange);
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

procedure TMultiArcItem.SetSignature(const AValue: AnsiString);
var
  I, J: Integer;
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
       Signature^[I]:= StrToInt('$' + Copy2SymbDel(Sign, #32));
       Inc(I);
      end;
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
    New(SignaturePosition);
    SignPos:= Copy2SymbDel(Value, ',');
    try
      while (SignPos <> EmptyStr) do
      begin
       SignaturePosition^.Value:= StrToInt(Copy2SymbDel(SignPos, ','));
       SignaturePosition^.Sign:= not (SignaturePosition^.Value < 0);
       SignaturePosition^.Value:= abs(SignaturePosition^.Value);
      end;
      FSignaturePositionList.Add(SignaturePosition);
    except
      Dispose(SignaturePosition);
    end;
  until Value = EmptyStr;
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
begin

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


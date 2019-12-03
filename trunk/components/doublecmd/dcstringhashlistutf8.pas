{
   Double Commander
   -------------------------------------------------------------------------
   Modified version of StringHashList unit with UTF-8 support

   Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)

   This file is based on stringhashlist.pas from the LazUtils package

   See the file COPYING.modifiedLGPL.txt, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit DCStringHashListUtf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazUtilsStrConsts;

type
  PStringHashItem = ^TStringHashItem;
  TStringHashItem = record
    HashValue: Cardinal;
    Key: String;
    Data: Pointer;
  end;

  PStringHashItemList = ^PStringHashItem;

  { TStringHashListUtf8 }

  TStringHashListUtf8 = class(TObject)
  private
    FList: PStringHashItemList;
    FCount: Integer;
    fCaseSensitive: Boolean;
    function BinarySearch(HashValue: Cardinal): Integer;
    function CompareString(const Low, Key: String): Boolean;
    function CompareValue(const Value1, Value2: Cardinal): Integer;
    procedure FindHashBoundaries(HashValue: Cardinal; StartFrom: Integer; out First, Last: Integer);
    function GetData(const S: String): Pointer;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure Delete(Index: Integer);
    procedure SetData(const S: String; const AValue: Pointer);
  protected
    function HashOf(const Key: string): Cardinal;
    procedure Insert(Index: Integer; Item: PStringHashItem);
  public
    constructor Create(CaseSensitivity: boolean);
    destructor Destroy; override;
    function Add(const S: String): Integer;
    function Add(const S: String; ItemData: Pointer): Integer;
    procedure Clear;
    function Find(const S: String): Integer;
    function Find(const S: String; Data: Pointer): Integer;
    function Remove(const S: String): Integer;
    function Remove(const S: String; Data: Pointer): Integer;
    procedure FindBoundaries(StartFrom: Integer; out First, Last: Integer);
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Count: Integer read FCount;
    property Data[const S: String]: Pointer read GetData write SetData; default;
    property List: PStringHashItemList read FList;
  end;

implementation

uses
  LazUTF8;

{ TStringHashListUtf8 }

function TStringHashListUtf8.Add(const S: String): Integer;
begin
  Result:=Add(S,nil);
end;

function TStringHashListUtf8.Add(const S: String; ItemData: Pointer): Integer;
var
  Text: String;
  Item: PStringHashItem;
  First, Last, I: Integer;
  Val: Cardinal;
  Larger: boolean;
begin
  if fCaseSensitive then
    Text := S
  else begin
    Text:= UTF8LowerCase(S);
  end;
  New(Item);
  Val:= HashOf(Text);
  Item^.HashValue := Val;
  Item^.Key := S;
  Item^.Data := ItemData;
  if FCount > 0 then
  begin
    First:=0;
    Last:= FCount-1;
    Larger:=False;
    while First<=Last do
    begin
      I:=(First+Last)shr 1;
      Case CompareValue(Val, fList[I]^.HashValue)<=0 of
        True:
          begin
            Last:=I-1;
            Larger:=False;
          end;
        False:
          begin
            First:=I+1;
            Larger:=True;
          end;
      end;
    end;
    Case Larger of
      True: Result:=I+1;
      False: Result:=I;
    end;
  end else
    Result:=0;
  Insert(Result,Item);
end;

function TStringHashListUtf8.BinarySearch(HashValue: Cardinal): Integer;
var
  First, Last, Temp: Integer;
begin
  Result:= -1;
  First:= 0;
  Last:= Count -1;
  while First <= Last do
  begin
    Temp:= (First + Last) div 2;
    case CompareValue(HashValue, FList[Temp]^.HashValue) of
      1: First:= Temp + 1;
      0: exit(Temp);
     -1: Last:= Temp-1;
    end;
  end;
end;

procedure TStringHashListUtf8.Clear;
var
  I: Integer;
begin
  for I:= 0 to fCount -1 do
    Dispose(fList[I]);
  if FList<>nil then begin
    FreeMem(FList);
    FList:=nil;
  end;
  fCount:= 0;
end;

function TStringHashListUtf8.CompareString(const Low, Key: String): Boolean;
var
  P: Pointer;
  Len: Integer;
  LKey: String;
begin
  P:= Pointer(Low);
  Len:= Length(Low);
  if fCaseSensitive then
  begin
    Result:= (Len = Length(Key));
    if Result then Result:= (CompareByte(P^, Pointer(Key)^, Len) = 0);
  end
  else begin
    LKey:= UTF8LowerCase(Key);
    Result:= (Len = Length(LKey));
    if Result then Result:= (CompareByte(P^, Pointer(LKey)^, Len) = 0);
  end;
end;

function TStringHashListUtf8.CompareValue(const Value1, Value2: Cardinal): Integer;
begin
  Result:= 0;
  if Value1 > Value2 then
    Result:= 1
  else if Value1 < Value2 then
    Result:= -1;
end;

function TStringHashListUtf8.GetData(const S: String): Pointer;
var i: integer;
begin
  i:=Find(S);
  if i>=0 then
    Result:=FList[i]^.Data
  else
    Result:=nil;
end;

procedure TStringHashListUtf8.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    dec(FCount);
    if Index < FCount then
      System.Move(FList[Index + 1], FList[Index],
        (FCount - Index) * SizeOf(PStringHashItem));
  end;
end;

procedure TStringHashListUtf8.SetData(const S: String; const AValue: Pointer);
var i: integer;
begin
  i:=Find(S);
  if i>=0 then
    FList[i]^.Data:=AValue
  else
    Add(S,AValue);
end;

destructor TStringHashListUtf8.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TStringHashListUtf8.Find(const S: String): Integer;
var
  Text: String;
  Value: Cardinal;
  First, Last, I: Integer;
begin
  if fCaseSensitive then
    Text := S
  else begin
    Text:= UTF8LowerCase(S);
  end;
  Value:= HashOf(Text);
  Result:= BinarySearch(Value);
  if (Result <> -1) and not CompareString(Text, FList[Result]^.Key) then
  begin
    FindHashBoundaries(Value, Result, First, Last);
    Result:= -1;
    for I := First to Last do
      if CompareString(Text, FList[I]^.Key) then
      begin
        Result:= I;
        Exit;
      end;
  end;
end;

function TStringHashListUtf8.Find(const S: String; Data: Pointer): Integer;
var
  Text: String;
  Value: Cardinal;
  First, Last, I: Integer;
begin
  if fCaseSensitive then
    Text := S
  else begin
    Text:= UTF8LowerCase(S);
  end;
  Value:= HashOf(Text);
  Result:= BinarySearch(Value);
  if (Result <> -1) and
     not (CompareString(Text, FList[Result]^.Key) and (FList[Result]^.Data = Data)) then
  begin
    FindHashBoundaries(Value, Result, First, Last);
    Result:= -1;
    for I := First to Last do
      if CompareString(Text, FList[I]^.Key) and (FList[I]^.Data = Data) then
      begin
        Result:= I;
        Exit;
      end;
  end;
end;

procedure TStringHashListUtf8.FindHashBoundaries(HashValue: Cardinal; StartFrom: Integer; out First, Last: Integer);
begin
  First:= StartFrom -1;
  //Find first matching hash index
  while (First >= 0) and (CompareValue(HashValue, FList[First]^.HashValue) = 0) do
    dec(First);
  if (First < 0) or ((CompareValue(HashValue, FList[First]^.HashValue) <> 0)) then
    inc(First);
  //Find the last matching hash index
  Last:= StartFrom +1;
  while (Last <= (FCount - 1)) and (CompareValue(HashValue, FList[Last]^.HashValue) = 0) do
    inc(Last);
  if (Last > (FCount - 1)) or (CompareValue(HashValue, FList[Last]^.HashValue) <> 0) then
    dec(Last);
end;

function TStringHashListUtf8.HashOf(const Key: string): Cardinal;
var
  P: PAnsiChar;
  I, Len: Integer;
begin
  P:= PAnsiChar(Key);
  Len:= Length(Key);
  Result := Len;
  {$PUSH}{$R-}{$Q-} // no range, no overflow checks
  for I := Len - 1 downto 0 do
    Inc(Result, Cardinal(Ord(P[I])) shl I);
  {$POP}
end;

procedure TStringHashListUtf8.Insert(Index: Integer; Item: PStringHashItem);
begin
  ReallocMem(FList, (fCount +1) * SizeOf(PStringHashItem));
  if Index > fCount then Index:= fCount;
  if Index < 0 then Index:= 0;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(PStringHashItem));
  FList[Index] := Item;
  Inc(FCount);
end;

constructor TStringHashListUtf8.Create(CaseSensitivity: boolean);
begin
  fCaseSensitive:=CaseSensitivity;
  inherited Create;
end;

function TStringHashListUtf8.Remove(const S: String): Integer;
begin
  Result:= Find(S);
  if Result > -1 then
  begin
    Dispose(fList[Result]);
    Delete(Result);
  end;
end;

function TStringHashListUtf8.Remove(const S: String; Data: Pointer): Integer;
begin
  Result:= Find(S, Data);
  if Result > -1 then
  begin
    Dispose(fList[Result]);
    Delete(Result);
  end;
end;

procedure TStringHashListUtf8.FindBoundaries(StartFrom: Integer; out First,
  Last: Integer);
begin
  FindHashBoundaries(FList[StartFrom]^.HashValue, StartFrom, First, Last);
end;

procedure TStringHashListUtf8.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    if Count > 0 then
    begin
      raise EListError.Create(lrsListMustBeEmpty);
      exit;
    end;
    fCaseSensitive := Value;
  end;
end;

end.

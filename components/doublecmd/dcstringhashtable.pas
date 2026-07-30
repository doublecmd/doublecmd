unit DCStringHashTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TDCHashItem }

  PDCHashItem = ^TDCHashItem;
  TDCHashItem = record
    Key: String;
    Value: TObject;
    NextItem: PDCHashItem;
  end;

  { TDCStringHashTable }

  TDCStringHashTable = class
  private const
    MaxTableSize = 8192;
  private
    FList: TFPList;
    FTable: array[0..Pred(MaxTableSize)] of PDCHashItem;
  private
    function GetCount: Integer;
    function GetItem(Index: Integer): PDCHashItem;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(const AName: String): PDCHashItem;
    function Add(const AName: String; AObject: TObject): Integer;
    property Items[Index: Integer]: PDCHashItem read GetItem;
    property Count: Integer read GetCount;
  end;

implementation

function CompareKey(const S1, S2: String): Boolean;
var
  Count: SizeInt;
begin
  Count:= Length(S1);

  if (Count <> Length(S2)) then
    Result:= False
  else begin
    Result:= CompareByte(Pointer(S1)^, Pointer(S2)^, Count) = 0;
  end;
end;

{ TDCStringHashTable }

function TDCStringHashTable.GetCount: Integer;
begin
  Result:= FList.Count;
end;

function TDCStringHashTable.GetItem(Index: Integer): PDCHashItem;
begin
  Result:= PDCHashItem(FList.List^[Index]);
end;

constructor TDCStringHashTable.Create;
begin
  FList:= TFPList.Create;
end;

destructor TDCStringHashTable.Destroy;
var
  Index: Integer;
  AItem: PDCHashItem;
begin
  for Index:= 0 to FList.Count - 1 do
  begin
    AItem:= PDCHashItem(FList.List^[Index]);
    AItem^.Value.Free;
    Dispose(AItem);
  end;
  FList.Free;
  inherited Destroy;
end;

function TDCStringHashTable.Find(const AName: String): PDCHashItem;
begin
  Result:= FTable[RSHash(AName, MaxTableSize)];

  while Assigned(Result) do
  begin
    if CompareKey(AName, Result^.Key) then
      Exit;

    Result:= Result^.NextItem;
  end;
end;

function TDCStringHashTable.Add(const AName: String; AObject: TObject): Integer;
var
  AHash: UInt32;
  AItem, ANext: PDCHashItem;
begin
  AHash:= RSHash(AName, MaxTableSize);
  ANext:= FTable[AHash];

  New(AItem);
  AItem^.Key:= AName;
  AItem^.Value:= AObject;
  AItem^.NextItem:= ANext;

  FTable[AHash]:= AItem;
  Result:= FList.Add(AItem);
end;

end.

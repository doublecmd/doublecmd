unit Hashes;
interface

  uses SysUtils;

  const
    c_HashInitialItemShift = 7;
    c_HashCompactR         = 2;   { This many spaces per item. }
    c_HashCompactM         = 100; { Never for less than this number of spaces. }
type
    EHashError = class(Exception);
    EHashFindError = class(EHashError);
    EHashIterateError = class(EHashError);
    EHashInvalidKeyError = class(EHashError);
    THashRecord = record
      Hash: Cardinal;
      ItemIndex: integer;
      Key: string;
    end;
    THashIterator = record
      ck, cx: integer;
    end;
    THash = class
      protected
        f_Keys: array of array of THashRecord;
        f_CurrentItemShift: integer;
        f_CurrentItemCount: integer;
        f_CurrentItemMask: integer;
        f_CurrentItemMaxIdx: integer;
        f_SpareItems: array of integer;
        f_NextAllowed: boolean;
        f_CurrentKey: string;
        f_AllowCompact: boolean;
        f_CurrentIterator: THashIterator;
        procedure FUpdateMasks;
        procedure FUpdateBuckets;
        function FFindKey(const Key: string; var k, x: integer): boolean;
        procedure FSetOrAddKey(const Key: string; ItemIndex: integer);
        procedure FDeleteIndex(i: integer); virtual; abstract;
        function FGetItemCount: integer;
        function FAllocItemIndex: integer;
        procedure FMoveIndex(oldIndex, newIndex: integer); virtual; abstract;
        procedure FTrimIndexes(count: integer); virtual; abstract;
        procedure FClearItems; virtual; abstract;
        function FIndexMax: integer; virtual; abstract;
        procedure FAutoCompact;
      public
        constructor Create; reintroduce; virtual;
        function Exists(const Key: string): boolean;
        procedure Rename(const Key, NewName: string);
        procedure Delete(const Key: string);
        procedure Restart;
        function Next: boolean;
        function Previous: boolean;
        function CurrentKey: string;
        property ItemCount: integer read FGetItemCount;
        procedure Compact;
        procedure Clear;
        property AllowCompact: boolean read f_AllowCompact write f_AllowCompact;
        property CurrentIterator: THashIterator read f_CurrentIterator write
          f_CurrentIterator;
        function NewIterator: THashIterator;
    end;
    TStringHash = class(THash)
      protected
        f_Items: array of string;
        procedure FDeleteIndex(i: integer); override;
        function FGetItem(const Key: string): string;
        procedure FSetItem(const Key, Value: string);
        procedure FMoveIndex(oldIndex, newIndex: integer); override;
        procedure FTrimIndexes(count: integer); override;
        procedure FClearItems; override;
        function FIndexMax: integer; override;
      public
        property Items[const Key: string]: string read FGetItem
          write FSetItem; default;
    end;
    TIntegerHash = class(THash)
      protected
        f_Items: array of integer;
        procedure FDeleteIndex(i: integer); override;
        function FGetItem(const Key: string): integer;
        procedure FSetItem(const Key: string; Value: integer);
        procedure FMoveIndex(oldIndex, newIndex: integer); override;
        procedure FTrimIndexes(count: integer); override;
        procedure FClearItems; override;
        function FIndexMax: integer; override;
      public
        property Items[const Key: string]: integer read FGetItem
        write FSetItem; default;
    end;
    TObjectHash = class(THash)
    protected
        f_Items: array of TObject;
        procedure FDeleteIndex(i: integer); override;
        function FGetItem(const Key: string): TObject;
        procedure FSetItem(const Key: string; Value: TObject);
        procedure FMoveIndex(oldIndex, newIndex: integer); override;
        procedure FTrimIndexes(count: integer); override;
        procedure FClearItems; override;
        function FIndexMax: integer; override;
    public
        property Items[const Key: string]: TObject read FGetItem
        write FSetItem; default;
        destructor Destroy; override;
    end;

implementation
  
  function HashThis(const s: string): cardinal;
  var
    h, g, i: cardinal;
  begin
    if (s = '') then
      raise EHashInvalidKeyError.Create('Key cannot be an empty string');
    h := $12345670;
    for i := 1 to Length(s) do begin
      h := (h shl 4) + ord(s[i]);
      g := h and $f0000000;
      if (g > 0) then
        h := h or (g shr 24) or g;
    end;
    result := h;
  end;

constructor THash.Create;
begin
  inherited Create;
  self.f_CurrentIterator.ck := -1;
  self.f_CurrentIterator.cx := 0;
  self.f_CurrentItemShift := c_HashInitialItemShift;
  self.FUpdateMasks;
  self.FUpdateBuckets;
  self.f_AllowCompact := true;
end;

procedure THash.Delete(const Key: string);
var
  k, x, i: integer;
begin
  { Hash has been modified, so disallow Next. }
  self.f_NextAllowed := false;
  if (self.FFindKey(Key, k, x)) then begin
    { Delete the Index entry. }
    i := self.f_Keys[k][x].ItemIndex;
    self.FDeleteIndex(i);
    { Add the index to the Spares list. }
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) + 1);
    self.f_SpareItems[High(self.f_SpareItems)] := i;
    { Overwrite key with the last in the list. }
    self.f_Keys[k][x] := self.f_Keys[k][High(self.f_Keys[k])];
    { Delete the last in the list. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) - 1);
  end else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);

  self.FAutoCompact;
end;

function THash.Exists(const Key: string): boolean;
var
  dummy1, dummy2: integer;
begin
  result := FFindKey(Key, dummy1, dummy2);
end;

procedure THash.FSetOrAddKey(const Key: string; ItemIndex: integer);
var
  k, x, i: integer;
begin
  { Exists already? }
  if (self.FFindKey(Key, k, x)) then begin
    { Yep. Delete the old stuff and set the new value. }
    i := self.f_Keys[k][x].ItemIndex;
    self.FDeleteIndex(i);
    self.f_Keys[k][x].ItemIndex := ItemIndex;
    { Add the index to the spares list. }
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) + 1);
    self.f_SpareItems[High(self.f_SpareItems)] := i;
  end else begin
    { No, create a new one. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := ItemIndex;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
  end;
end;

function THash.FFindKey(const Key: string; var k, x: integer): boolean;
var
  i: integer;
  h: cardinal;
begin
  { Which bucket? }
  h := HashThis(Key);
  k := h and f_CurrentItemMask;
  result := false;
  { Look for it. }
  for i := 0 to High(self.f_Keys[k]) do
    if (self.f_Keys[k][i].Hash = h) or true then
      if (self.f_Keys[k][i].Key = Key) then begin
        { Found it! }
        result := true;
        x := i;
        break;
      end;
end;

procedure THash.Rename(const Key, NewName: string);
var
  k, x, i: integer;
begin
  { Hash has been modified, so disallow Next. }
  self.f_NextAllowed := false;
  if (self.FFindKey(Key, k, x)) then begin
    { Remember the ItemIndex. }
    i := self.f_Keys[k][x].ItemIndex;
    { Overwrite key with the last in the list. }
    self.f_Keys[k][x] := self.f_Keys[k][High(self.f_Keys[k])];
    { Delete the last in the list. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) - 1);
    { Create the new item. }
    self.FSetOrAddKey(NewName, i);
  end else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);

  self.FAutoCompact;
end;

function THash.CurrentKey: string;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot find CurrentKey as the hash has '
      + 'been modified since Restart was called')
  else if (self.f_CurrentKey = '') then
    raise EHashIterateError.Create('Cannot find CurrentKey as Next has not yet '
      + 'been called after Restart')
  else
    result := self.f_CurrentKey;
end;

function THash.Next: boolean;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot get Next as the hash has '
      + 'been modified since Restart was called');
  result := false;
  if (self.f_CurrentIterator.ck = -1) then begin
    self.f_CurrentIterator.ck := 0;
    self.f_CurrentIterator.cx := 0;
  end;
  while ((not result) and (self.f_CurrentIterator.ck <= f_CurrentItemMaxIdx)) do begin
    if (self.f_CurrentIterator.cx < Length(self.f_Keys[self.f_CurrentIterator.ck])) then begin
      result := true;
      self.f_CurrentKey := self.f_Keys[self.f_CurrentIterator.ck][self.f_CurrentIterator.cx].Key;
      inc(self.f_CurrentIterator.cx);
    end else begin
      inc(self.f_CurrentIterator.ck);
      self.f_CurrentIterator.cx := 0;
    end;
  end;
end;

procedure THash.Restart;
begin
  self.f_CurrentIterator.ck := -1;
  self.f_CurrentIterator.cx := 0;
  self.f_NextAllowed := true;
end;

function THash.FGetItemCount: integer;
var
  i: integer;
begin
  { Calculate our item count. }
  result := 0;
  for i := 0 to f_CurrentItemMaxIdx do
    inc(result, Length(self.f_Keys[i]));
end;

function THash.FAllocItemIndex: integer;
begin
  if (Length(self.f_SpareItems) > 0) then begin
    { Use the top SpareItem. }
    result := self.f_SpareItems[High(self.f_SpareItems)];
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) - 1);
  end else begin
    result := self.FIndexMax + 1;
  end;
end;

procedure THash.Compact;
var
  aSpaces: array of boolean;
  aMapping: array of integer;
  i, j: integer;
begin
  { Find out where the gaps are. We could do this by sorting, but that's at
    least O(n log n), and sometimes O(n^2), so we'll go for the O(n) method,
    even though it involves multiple passes. Note that this is a lot faster
    than it looks. Disabling this saves about 3% in my benchmarks, but uses a
    lot more memory. }
  if (self.AllowCompact) then begin
    SetLength(aSpaces, self.FIndexMax + 1);
    SetLength(aMapping, self.FIndexMax + 1);
    for i := 0 to High(aSpaces) do
      aSpaces[i] := false;
    for i := 0 to High(aMapping) do
      aMapping[i] := i;
    for i := 0 to High(self.f_SpareItems) do
      aSpaces[self.f_SpareItems[i]] := true;

    { Starting at the low indexes, fill empty ones from the high indexes. }
    i := 0;
    j := self.FIndexMax;
    while (i < j) do begin
      if (aSpaces[i]) then begin
        while ((i < j) and (aSpaces[j])) do
          dec(j);
        if (i < j) then begin
          aSpaces[i] := false;
          aSpaces[j] := true;
          self.FMoveIndex(j, i);
          aMapping[j] := i
        end;
      end else
        inc(i);
    end;

    j := self.FIndexMax;
    while (aSpaces[j]) do
      dec(j);

    { Trim the items array down to size. }
    self.FTrimIndexes(j + 1);

    { Clear the spaces. }
    SetLength(self.f_SpareItems, 0);

    { Update our buckets. }
    for i := 0 to f_CurrentItemMaxIdx do
      for j := 0 to High(self.f_Keys[i]) do
        self.f_Keys[i][j].ItemIndex := aMapping[self.f_Keys[i][j].ItemIndex];
  end;
end;

procedure THash.FAutoCompact;
begin
  if (self.AllowCompact) then
    if (Length(self.f_SpareItems) >= c_HashCompactM) then
      if (self.FIndexMax * c_HashCompactR > Length(self.f_SpareItems)) then
        self.Compact;
end;

procedure THash.Clear;
var
  i: integer;
begin
  self.FClearItems;
  SetLength(self.f_SpareItems, 0);
  for i := 0 to f_CurrentItemMaxIdx do
    SetLength(self.f_Keys[i], 0);
end;

procedure THash.FUpdateMasks;
begin
  f_CurrentItemMask := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemMaxIdx := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemCount := (1 shl f_CurrentItemShift);
end;

procedure THash.FUpdateBuckets;
begin
  { This is just a temporary thing. }
  SetLength(self.f_Keys, self.f_CurrentItemCount);
end;

function THash.NewIterator: THashIterator;
begin
  result.ck := -1;
  result.cx := 0;
end;

function THash.Previous: boolean;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot get Next as the hash has '
      + 'been modified since Restart was called');
  result := false;
  if (self.f_CurrentIterator.ck >= 0) then begin
    while ((not result) and (self.f_CurrentIterator.ck >= 0)) do begin
      dec(self.f_CurrentIterator.cx);
      if (self.f_CurrentIterator.cx >= 0) then begin
        result := true;
        self.f_CurrentKey := self.f_Keys[self.f_CurrentIterator.ck][self.f_CurrentIterator.cx].Key;
      end else begin
        dec(self.f_CurrentIterator.ck);
        if (self.f_CurrentIterator.ck >= 0) then
          self.f_CurrentIterator.cx := Length(self.f_Keys[self.f_CurrentIterator.ck]);
      end;
    end;
  end;
end;

{ TStringHash }

procedure TStringHash.FDeleteIndex(i: integer);
begin
  self.f_Items[i] := '';
end;

function TStringHash.FGetItem(const Key: string): string;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TStringHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TStringHash.FSetItem(const Key, Value: string);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value
  else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TStringHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TStringHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TStringHash.FClearItems;
begin
  SetLength(self.f_Items, 0);
end;

{ TIntegerHash }

procedure TIntegerHash.FDeleteIndex(i: integer);
begin
  self.f_Items[i] := 0;
end;

function TIntegerHash.FGetItem(const Key: string): integer;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TIntegerHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TIntegerHash.FSetItem(const Key: string; Value: integer);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value
  else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TIntegerHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TIntegerHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TIntegerHash.FClearItems;
begin
  SetLength(self.f_Items, 0);
end;

{ TObjectHash }

procedure TObjectHash.FDeleteIndex(i: integer);
begin
  self.f_Items[i].Free;
  self.f_Items[i] := nil;
end;

function TObjectHash.FGetItem(const Key: string): TObject;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TObjectHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TObjectHash.FSetItem(const Key: string; Value: TObject);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then begin
    self.f_Items[self.f_Keys[k][x].ItemIndex].Free;
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value;
  end else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TObjectHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TObjectHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TObjectHash.FClearItems;
var
  i: integer;
begin
  for i := 0 to High(self.f_Items) do
    if (Assigned(self.f_Items[i])) then
      self.f_Items[i].Free;
  SetLength(self.f_Items, 0);
end;

destructor TObjectHash.Destroy;
var
  i: integer;
begin
  for i := 0 to High(self.f_Items) do
    if (Assigned(self.f_Items[i])) then
      self.f_Items[i].Free;
  inherited;
end;
end.
    
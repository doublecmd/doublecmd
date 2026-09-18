unit uSyncDirsService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  IntegerList,
  DCStrUtils, uDCUtils,
  uGlobs,
  uSyncDirsModel;

const
  SYNC_REC_STATE_SYMBOL: array[TSyncRecState] of String = (
    '?',
    '=',
    '!=',
    '<-',
    '->',
    'X_',
    '_X',
    'XX',
    '',

    'ERR(NextAction)',
    'ERR(NoAction)',
    'ERR(DEL)'
  );

type

  { TSyncDirsService }

  TSyncDirsService = class
  private
    _sortIndex: Integer;
    _sortDesc: Boolean;
  public
    procedure sortTree( const tree: TTwoLevelTree );
    procedure sortDirItem( const dirItem: TTwoLevelTreeDirItem );
    function selectionToStringList(
      const FFilteredList: TFlatDirFileList;
      const indexes: TIntegerList;
      const Option: TCompareOption ): TStringList;

    property sortIndex: Integer write _sortIndex;
    property sortDesc: Boolean write _sortDesc;
  end;

implementation

{ TSyncDirsService }

procedure TSyncDirsService.sortTree( const tree: TTwoLevelTree );
var
  i: Integer;
begin
  if _sortIndex < 0 then
    Exit;
  for i:= 0 to tree.Count-1 do
    self.sortDirItem( tree.dirItem(i) );
end;

procedure TSyncDirsService.sortDirItem( const dirItem: TTwoLevelTreeDirItem );

  function CompareFn(sl: TStringList; i, j: Integer): Integer;
  var
    r1, r2: TFileSyncRec;
  begin
    if _sortIndex in [1..5] then
    begin
      r1:= dirItem.fileSyncRec(i);
      r2:= dirItem.fileSyncRec(j);
    end;
    case _sortIndex of
    0:
      Result := mbCompareStr(sl[i], sl[j]);
    1:
      if (Assigned(r1.leftFile) < Assigned(r2.leftFile))
      or Assigned(r2.leftFile) and (r1.leftFile.Size < r2.leftFile.Size) then
        Result := -1
      else
      if (Assigned(r1.leftFile) > Assigned(r2.leftFile))
      or Assigned(r1.leftFile) and (r1.leftFile.Size > r2.leftFile.Size) then
        Result := 1
      else
        Result := 0;
    2:
      if (Assigned(r1.leftFile) < Assigned(r2.leftFile))
      or Assigned(r2.leftFile)
      and (r1.leftFile.ModificationTime < r2.leftFile.ModificationTime) then
        Result := -1
      else
      if (Assigned(r1.leftFile) > Assigned(r2.leftFile))
      or Assigned(r1.leftFile)
      and (r1.leftFile.ModificationTime > r2.leftFile.ModificationTime) then
        Result := 1
      else
        Result := 0;
    4:
      if (Assigned(r1.rightFile) < Assigned(r2.rightFile))
      or Assigned(r2.rightFile)
      and (r1.rightFile.ModificationTime < r2.rightFile.ModificationTime) then
        Result := -1
      else
      if (Assigned(r1.rightFile) > Assigned(r2.rightFile))
      or Assigned(r1.rightFile)
      and (r1.rightFile.ModificationTime > r2.rightFile.ModificationTime) then
        Result := 1
      else
        Result := 0;
    5:
      if (Assigned(r1.rightFile) < Assigned(r2.rightFile))
      or Assigned(r2.rightFile) and (r1.rightFile.Size < r2.rightFile.Size) then
        Result := -1
      else
      if (Assigned(r1.rightFile) > Assigned(r2.rightFile))
      or Assigned(r1.rightFile) and (r1.rightFile.Size > r2.rightFile.Size) then
        Result := 1
      else
        Result := 0;
    6:
      Result := mbCompareStr(sl[i], sl[j]);
    end;
    if _sortDesc then
      Result := -Result;
  end;

  procedure QuickSort(L, R: Integer; sl: TStringList);
  var
    Pivot, vL, vR: Integer;
  begin
    if R - L <= 1 then begin // a little bit of time saver
      if L < R then
        if CompareFn(sl, L, R) > 0 then
          sl.Exchange(L, R);
      Exit;
    end;

    vL := L;
    vR := R;

    Pivot := L + Random(R - L); // they say random is best

    while vL < vR do begin
      while (vL < Pivot) and (CompareFn(sl, vL, Pivot) <= 0) do
        Inc(vL);

      while (vR > Pivot) and (CompareFn(sl, vR, Pivot) > 0) do
        Dec(vR);

      sl.Exchange(vL, vR);

      if Pivot = vL then // swap pivot if we just hit it from one side
        Pivot := vR
      else if Pivot = vR then
        Pivot := vL;
    end;

    if Pivot - 1 >= L then
      QuickSort(L, Pivot - 1, sl);
    if Pivot + 1 <= R then
      QuickSort(Pivot + 1, R, sl);
  end;

begin
  QuickSort( 0, dirItem.fileCount-1, dirItem.files );
end;

function TSyncDirsService.selectionToStringList(
  const FFilteredList: TFlatDirFileList;
  const indexes: TIntegerList;
  const Option: TCompareOption ): TStringList;

  procedure PrintRow(sl: TStringList; R: Integer);
  var
    s: string;
    SyncRec: TFileSyncRec;
  begin
    SyncRec := FFilteredList.fileSyncRec(R);
    if SyncRec.isDir then
    begin
      s := FFilteredList.path(R);
      if cfEmptyDirs in Option.flags then begin
        if SyncRec.state <> srsDoNothing then
          s := s + #9#9#9 + SYNC_REC_STATE_SYMBOL[SyncRec.action];
      end;
    end
    else
    begin
      if Assigned(SyncRec.leftFile) then
      begin
        s := FFilteredList.path(R) + #9 +
             IntToStrTS(SyncRec.leftFile.Size) + #9 +
             FormatDateTime(gDateTimeFormatSync, SyncRec.leftFile.ModificationTime);
      end
      else
      begin
        s := #9#9;
      end;
      s := s + #9 + SYNC_REC_STATE_SYMBOL[SyncRec.action] + #9;
      if Assigned(SyncRec.rightFile) then
      begin
        s := s +
             FormatDateTime(gDateTimeFormatSync, SyncRec.rightFile.ModificationTime) + #9 +
             IntToStrTS(SyncRec.rightFile.Size) + #9 +
             FFilteredList.path(R);
      end;
    end;
    sl.Add(s);
  end;

var
  sl: TStringList;
  i: Integer;
begin
  sl:= TStringList.Create;
  for i:= 0 to indexes.Count-1 do
    PrintRow(sl, indexes[i]);
  Result:= sl;
end;

end.


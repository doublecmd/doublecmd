unit uSyncDirsService;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils, SysConst, syncobjs, IntegerList,
  DCStrUtils, uDCUtils, DCClassesUtf8,
  uDebug, uGlobs,
  uFileSourceCopyOperation,
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

  { ISyncDirsCheckContentThreadCallback }

  ISyncDirsCheckContentThreadCallback = interface
    procedure onCheckContentThreadStart;
    procedure onCheckContentThreadFinish;
    procedure onCheckContentThreadReapplyFilter;
    procedure onCheckContentThreadCountUpdated( const equalInc: Integer; const notEqInc: Integer );
  end;

  { TSyncDirsCheckContentThread }

  TSyncDirsCheckContentThread = class( TThread )
  private
    _fullTree: TTwoLevelTree;
    _callback: ISyncDirsCheckContentThreadCallback;
    _done: Boolean;
    _mutex: TCriticalSection;
    _statistics: TFileSourceCopyOperationStatistics;
  protected
    procedure Execute; override;
    procedure UpdateStatistics(var NewStatistics: TFileSourceCopyOperationStatistics);
  public
    constructor Create(const fullTree: TTwoLevelTree; const callback: ISyncDirsCheckContentThreadCallback);
    destructor Destroy; override;
    function RetrieveStatistics: TFileSourceCopyOperationStatistics;
    property Done: Boolean read _done;
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

{ TSyncDirsCheckContentThread }

procedure TSyncDirsCheckContentThread.Execute;
const
  BUF_LEN = 1024 * 1024;
var
  Buffer1, Buffer2: PByte;
  Statistics: TFileSourceCopyOperationStatistics;

  function CompareFiles(const FileName1, FileName2: String; Size: Int64): Boolean;
  var
    DoneBytes, Count: Int64;
    File1, File2: TFileStreamEx;
  begin
    File1 := TFileStreamEx.Create(FileName1, fmOpenRead or fmShareDenyWrite);
    try
      File2 := TFileStreamEx.Create(FileName2, fmOpenRead or fmShareDenyWrite);
      try
        DoneBytes := 0;

        repeat
          if Size - DoneBytes <= BUF_LEN then
            Count := Size - DoneBytes
          else begin
            Count := BUF_LEN;
          end;

          File1.ReadBuffer(Buffer1^, Count);
          File2.ReadBuffer(Buffer2^, Count);

          if (Count <> BUF_LEN) then
            Result := CompareByte(Buffer1^, Buffer2^, Count) = 0
          else begin
            Result := CompareDWord(Buffer1^, Buffer2^, Count div SizeOf(Dword)) = 0;
          end;

          Statistics.DoneBytes += Count;
          DoneBytes := DoneBytes + Count;

          UpdateStatistics(Statistics);

        until Terminated or not Result or (DoneBytes >= Size);
      finally
        File2.Free;
      end;
    finally
      File1.Free;
    end;
  end;

var
  isEqual: Boolean;
  dirIndex, fileIndex: Integer;
  rec: TFileSyncRec;
begin
  Synchronize(@_callback.onCheckContentThreadStart);
  Buffer1:= GetMem(BUF_LEN);
  Buffer2:= GetMem(BUF_LEN);
  try
    if (Buffer1 = nil) or (Buffer2 = nil) then
      raise EOutOfMemory.Create(SOutOfMemory);

    with _callback do
    begin
      Statistics.DoneBytes:= 0;
      Statistics.TotalBytes:= 0;
      for dirIndex := 0 to _fullTree.Count - 1 do
      begin
        for fileIndex := 0 to _fullTree.dirItem(dirIndex).fileCount - 1 do
        begin
          if Terminated then Exit;
          rec := _fullTree.fileSyncRec(dirIndex, fileIndex);
          if NOT rec.isDir and (rec.state = srsUnknown) then
          begin
            Statistics.TotalBytes+= rec.leftFile.Size;
          end;
        end;
      end;
      UpdateStatistics(Statistics);
    end;

    with _callback do
    for dirIndex := 0 to _fullTree.Count - 1 do
    begin
      for fileIndex := 0 to _fullTree.dirItem(dirIndex).fileCount - 1 do
      begin
        if Terminated then Exit;
        rec := _fullTree.fileSyncRec(dirIndex, fileIndex);
        if NOT rec.isDir and (rec.state = srsUnknown) then
        begin
          try
            isEqual:= CompareFiles(rec.leftFile.FullPath, rec.rightFile.FullPath, rec.leftFile.Size);
            if Terminated then Exit;
            if isEqual then
            begin
              _callback.onCheckContentThreadCountUpdated( 1, -1 );
              rec.state := srsEqual
            end
            else begin
              if cfAsymmetric in rec.option.flags then begin
                rec.state := srsCopyToRight;
              end else begin
                rec.state := srsNotEq;
              end;
            end;
            if rec.action = srsUnknown then
            begin
              rec.action := rec.state;
            end;
          except
            on E: Exception do
              DCDebug('[SyncDirs::CmpContentThread] ' + E.Message);
          end;
        end;
      end;
    end;
    _done := True;
    Synchronize(@_callback.onCheckContentThreadReapplyFilter);
  finally
    Synchronize(@_callback.onCheckContentThreadFinish);
    if Assigned(Buffer1) then FreeMem(Buffer1);
    if Assigned(Buffer2) then FreeMem(Buffer2);
  end;
end;

function TSyncDirsCheckContentThread.RetrieveStatistics: TFileSourceCopyOperationStatistics;
begin
  _mutex.Acquire;
  try
    Result := _statistics;
  finally
    _mutex.Release;
  end;
end;

procedure TSyncDirsCheckContentThread.UpdateStatistics(var NewStatistics: TFileSourceCopyOperationStatistics);
begin
  _mutex.Acquire;
  try
    _statistics := NewStatistics;
  finally
    _mutex.Release;
  end;
end;

constructor TSyncDirsCheckContentThread.Create(
  const fullTree: TTwoLevelTree;
  const callback: ISyncDirsCheckContentThreadCallback );
begin
  _fullTree:= fullTree;
  _callback:= callback;
  _mutex:= TCriticalSection.Create;
  inherited Create(False);
end;

destructor TSyncDirsCheckContentThread.Destroy;
begin
  inherited Destroy;
  _mutex.Free;
end;

end.


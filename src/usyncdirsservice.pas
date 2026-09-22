unit uSyncDirsService;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, SysConst, syncobjs, IntegerList,
  LazFileUtils,
  DCStrUtils, DCOSUtils, DCClassesUtf8, uDCUtils,
  uDebug, uGlobs,
  uFile, uFileSource, uFileSourceManager, uFileSourceUtil,
  uFileSourceOperation, uFileSourceCopyOperation, uFileSourceOperationTypes,
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

  { TSyncDirsOperationHandle }

  TSyncDirsOperationHandle = procedure ( const operation: TFileSourceOperation; const state: TFileSourceOperationState ) is nested;

  { TSyncDirsFileUtil }

  TSyncDirsFileUtil = class
  public
    class function consultCopyOperation(var params: TFileSourceConsultParams): Boolean;
    class function consultAndConfirmCopyOperation(var params: TFileSourceConsultParams): Boolean;
    class function supportsSyncDirs(const sourceFS: IFileSource; const targetFS: IFileSource): Boolean;
  public
    class function copyFiles(
      const sourceFS: IFileSource;
      const targetFS: IFileSource;
      const files: TFiles;
      const targetPath: String;
      const operationHandle: TSyncDirsOperationHandle ): Boolean;
    class function deleteFiles(
      const fs: IFileSource;
      var files: TFiles;
      const operationHandle: TSyncDirsOperationHandle ): Boolean;
  end;

  { ISyncDirsFileProcessorWithUI }

  ISyncDirsFileProcessorWithUI = interface
    function fileProcessorWithUICopyFiles(
      const sourceFS: IFileSource;
      const targetFS: IFileSource;
      const files: TFiles;
      const targetPath: String): Boolean;
    function fileProcessorWithUIDeleteFiles(
      const fs: IFileSource;
      var files: TFiles): Boolean;
    function fileProcessorWithUIDeleteFile(
      const fs: IFileSource;
      const f: TFile): Boolean;
  end;

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
      const Option: TSyncDirsCompareOption ): TStringList;

    property sortIndex: Integer write _sortIndex;
    property sortDesc: Boolean write _sortDesc;
  end;

  { ISyncDirsTreeBuilderCallback }

  ISyncDirsTreeBuilderCallback = interface
    function treeBuilderCheckRunning( const processMessages: Boolean ): Boolean;
    function treeBuilderMaskFilt( const f: TFile ): Boolean;
    function treeBuilderSelectedFilt( const filename: String ): Boolean;
    procedure onTreeBuilderUpdateProgress( const percent: Integer );
  end;

  { TSyncDirsTreeBuilder }

  TSyncDirsTreeBuilder = class
  private
    _callback: ISyncDirsTreeBuilderCallback;
    _sortedService: TSyncDirsService;
    _compareOption: TSyncDirsCompareOption;
    _baseDirL: String;
    _baseDirR: String;
    _fileSourceL: IFileSource;
    _fileSourceR: IFileSource;
    _leftFirst: Boolean;
    _rightFirst: Boolean;
  public
    constructor Create(
      const callback: ISyncDirsTreeBuilderCallback;
      const sortService: TSyncDirsService;
      const compareOption: TSyncDirsCompareOption );
    procedure build( const FFullTree: TTwoLevelTree );

    property baseDirL: String write _baseDirL;
    property baseDirR: String write _baseDirR;
    property fileSourceL: IFileSource write _fileSourceL;
    property fileSourceR: IFileSource write _fileSourceR;
  end;

  { ISyncDirsSynchronizerCallback }

  ISyncDirsSynchronizerCallback = interface
    function synchronizerCheckRunning: Boolean;
  end;

  { TSyncDirsSynchronizer }

  TSyncDirsSynchronizer = class
  private
    _callback: ISyncDirsSynchronizerCallback;
    _fileProcessor: ISyncDirsFileProcessorWithUI;
    _filteredList: TFlatDirFileList;
    _leftFS: IFileSource;
    _rightFS: IFileSource;
    _leftBasePath: String;
    _rightBasePath: String;
  public
    constructor Create(
      const callback: ISyncDirsSynchronizerCallback;
      const fileProcessor: ISyncDirsFileProcessorWithUI;
      const filteredList: TFlatDirFileList );
    function count: TSyncDirsSyncCount;
    procedure sync( const syncFlags: TSyncDirsSyncFlags );

    property leftFS: IFileSource write _leftFS;
    property rightFS: IFileSource write _rightFS;
    property leftBasePath: String write _leftBasePath;
    property rightBasePath: String write _rightBasePath;
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

{ TSyncDirsFileUtil }

class function TSyncDirsFileUtil.consultCopyOperation( var params: TFileSourceConsultParams ): Boolean;
begin
  Result:= False;
  params.operationType:= fsoCopy;
  FileSourceManager.consultOperation(params);
  if params.consultResult <> fscrSuccess then
    Exit;
  if params.operationTemp then
    Exit;
  Result:= True;
end;

class function TSyncDirsFileUtil.consultAndConfirmCopyOperation( var params: TFileSourceConsultParams ): Boolean;
begin
  Result:= False;
  if consultCopyOperation(params) then
    FileSourceManager.confirmOperation(params);
  if params.consultResult <> fscrSuccess then
    Exit;
  if params.operationTemp then
    Exit;
  Result:= True;
end;

class function TSyncDirsFileUtil.supportsSyncDirs(
  const sourceFS: IFileSource;
  const targetFS: IFileSource): Boolean;
var
  params: TFileSourceConsultParams;
begin
  params:= Default(TFileSourceConsultParams);
  params.sourceFS:= sourceFS;
  params.targetFS:= targetFS;
  Result:= consultCopyOperation(params);
end;

class function TSyncDirsFileUtil.copyFiles(
  const sourceFS: IFileSource;
  const targetFS: IFileSource;
  const files: TFiles;
  const targetPath: String;
  const operationHandle: TSyncDirsOperationHandle ): Boolean;
var
  params: TFileSourceConsultParams;
  fsOperation: TFileSourceOperation;
begin
  files.Path:= files[0].Path;

  params:= Default(TFileSourceConsultParams);
  params.sourceFS:= sourceFS;
  params.targetFS:= targetFS;
  params.files:= files;
  params.targetPath:= targetPath;
  Result:= TSyncDirsFileUtil.consultAndConfirmCopyOperation(params);
  if NOT Result then
    Exit;

  // Create destination directory
  targetFS.CreateDirectory(ExcludeBackPathDelimiter(targetPath));

  // Determine fsOperation type
  case params.resultOperationType of
    fsoCopy:
      begin
        // Copy within the same file source.
        fsOperation := params.resultFS.CreateCopyOperation(
                      params.files,
                      params.resultTargetPath ) as TFileSourceCopyOperation;
      end;
    fsoCopyOut:
      begin
        // CopyOut to filesystem.
        fsOperation := params.resultFS.CreateCopyOutOperation(
                       targetFS,
                       params.files,
                       params.resultTargetPath) as TFileSourceCopyOperation;
      end;
    fsoCopyIn:
      begin
        // CopyIn from filesystem.
        fsOperation := params.resultFS.CreateCopyInOperation(
                       sourceFS,
                       params.files,
                       params.resultTargetPath) as TFileSourceCopyOperation;
      end;
  end;
  Result:= Assigned(fsOperation);
  if NOT Result then
    Exit;

  try
    operationHandle( fsOperation, TFileSourceOperationState.fsosStarting );
    fsOperation.Execute;
    Result := fsOperation.Result = fsorFinished;
    operationHandle( fsOperation, TFileSourceOperationState.fsosStopped );
  finally
    FreeAndNil(fsOperation);
  end;
end;

class function TSyncDirsFileUtil.deleteFiles(
  const fs: IFileSource;
  var files: TFiles;
  const operationHandle: TSyncDirsOperationHandle ): Boolean;
var
  fsOperation: TFileSourceOperation;
begin
  files.Path:= files[0].Path;
  fsOperation:= fs.CreateDeleteOperation(files);
  Result:= Assigned( fsOperation );
  if NOT Result then
    Exit;
  try
    operationHandle( fsOperation, TFileSourceOperationState.fsosStarting );
    fsOperation.Execute;
    Result:= fsOperation.Result = fsorFinished;
    operationHandle( fsOperation, TFileSourceOperationState.fsosStopped );
  finally
    FreeAndNil(fsOperation);
  end;
end;

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
  const Option: TSyncDirsCompareOption ): TStringList;

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

{ TSyncDirsTreeBuilder }

constructor TSyncDirsTreeBuilder.Create(
  const callback: ISyncDirsTreeBuilderCallback;
  const sortService: TSyncDirsService;
  const compareOption: TSyncDirsCompareOption );
begin
  _callback:= callback;
  _sortedService:= sortService;
  _compareOption:= compareOption;
  _leftFirst:= True;
  _rightFirst:= True;
end;

procedure TSyncDirsTreeBuilder.build(const FFullTree: TTwoLevelTree);
  procedure ScanDir(
    dir: string;
    const leftParentDirs: TStringList;
    const rightParentDirs: TStringList);

    procedure ProcessOneSide(dirItem: TTwoLevelTreeDirItem; dirs: TStringList; var ASide: Boolean; sideLeft: Boolean);
    var
      fs: TFiles;
      i, j: Integer;
      f: TFile;
      r: TFileSyncRec;
      fn: String;
      dirFullPath: String;
      dirSyncRec: TDirSyncRec;
      currentFileSource: IFileSource;
    begin
      dirSyncRec := dirItem.dirSyncRec;
      if sideLeft then begin
        currentFileSource := _fileSourceL;
        dirFullPath := _baseDirL + dir;
      end else begin
        currentFileSource := _fileSourceR;
        dirFullPath := _baseDirR + dir;
      end;
      fs := currentFileSource.GetFiles(dirFullPath);
      if (cfOnlySelected in _compareOption.flags) and ASide then
      begin
        ASide:= False;
        for I:= fs.Count - 1 downto 0 do
        begin
          if NOT _callback.treeBuilderSelectedFilt(fs[I].Name) then
            fs.Delete(I);
        end;
      end;
      try
        for i := 0 to fs.Count - 1 do
        begin
          f := fs.Items[i];
          if f.Name = EmptyStr then
            f.Name := currentFileSource.GetDisplayFileName(f);
          fn := NormalizeFileName(f.Name);
          if f.IsDirectory or f.IsLinkToDirectory then begin
            if (f.NameNoExt <> '.') and (f.NameNoExt <> '..') then
            begin
              if _callback.treeBuilderMaskFilt(f) then begin
                dirs.AddObject(fn, f.Clone);  // dirs don't own Object
                dirSyncRec.incDirCount(sideLeft);
              end;
            end;
          end else if _callback.treeBuilderMaskFilt(f) then begin
            j := dirItem.indexOfFile(fn);
            if j < 0 then
              r := TFileSyncRec.Create(_compareOption, dir)
            else
              r := dirItem.fileSyncRec(j);
            if sideLeft then
            begin
              r.leftFile := f.Clone;
            end else begin
              r.rightFile := f.Clone;
            end;
            r.updateState;
            dirItem.addFile(fn, r);
            dirSyncRec.incFileCount(sideLeft);
          end;
        end;
      finally
        fs.Free;
      end;
    end;

    procedure setDirSyncRecFile(dirSyncRec: TDirSyncRec);
    var
      i: Integer;
      currentDirPart: String;
    begin
      currentDirPart:= GetLastDir(dir);
      i:= leftParentDirs.IndexOf(currentDirPart);
      if i >= 0 then
        dirSyncRec.leftFile:= TFile(leftParentDirs.Objects[i]);    // owns file
      i:= rightParentDirs.IndexOf(currentDirPart);
      if i >= 0 then
        dirSyncRec.rightFile:= TFile(rightParentDirs.Objects[i]);   // owns file
    end;

  var
    i, j, tot: Integer;
    dirItem: TTwoLevelTreeDirItem;
    dirsLeft, dirsRight: TStringListEx;
    d: string;
    dirSyncRec: TDirSyncRec;
  begin
    i := FFullTree.indexOfDir(dir);
    if i < 0 then begin
      dirSyncRec := TDirSyncRec.Create(_compareOption, dir);
      dirItem := TTwoLevelTreeDirItem.Create(dirSyncRec);
      FFullTree.addDir(dir, dirItem);
    end else begin
      dirItem := FFullTree.dirItem(i);
      dirSyncRec := dirItem.dirSyncRec;
    end;

    if dir <> '' then begin
      setDirSyncRecFile(dirSyncRec);
      dir := AppendPathDelim(dir);
    end;

    dirsLeft := TStringListEx.Create;
    dirsLeft.CaseSensitive := FileNameCaseSensitive;
    dirsLeft.Sorted := True;
    dirsRight := TStringListEx.Create;
    dirsRight.CaseSensitive := FileNameCaseSensitive;
    dirsRight.Sorted := True;
    try
      if NOT _callback.treeBuilderCheckRunning(True) then
        Exit;
      ProcessOneSide(dirItem, dirsLeft, _leftFirst, True);
      ProcessOneSide(dirItem, dirsRight, _rightFirst, False);
      dirSyncRec.updateState;
      _sortedService.sortDirItem(dirItem);
      if not (cfSubdirs in _compareOption.flags) then Exit;
      tot := dirsLeft.Count + dirsRight.Count;
      for i := 0 to dirsLeft.Count - 1 do
      begin
        if dir = '' then
          _callback.onTreeBuilderUpdateProgress( i * 100 div tot );
        d := dirsLeft[i];
        ScanDir(dir + d, dirsLeft, dirsRight);
        if  NOT _callback.treeBuilderCheckRunning(False) then
          Exit;
        j := dirsRight.IndexOf(d);
        if j >= 0 then
        begin
          dirsRight.Delete(j);
          Dec(tot);
        end
      end;
      for i := 0 to dirsRight.Count - 1 do
      begin
        if dir = '' then
          _callback.onTreeBuilderUpdateProgress( (dirsLeft.Count + i) * 100 div tot );
        d := dirsRight[i];
        ScanDir(dir + d, dirsLeft, dirsRight);
        if  NOT _callback.treeBuilderCheckRunning(False) then
          Exit;
      end;
    finally
      dirsLeft.Free;
      dirsRight.Free;
    end;
  end;

begin
  ScanDir('', nil, nil);
end;

{ TSyncDirsSynchronizer }

constructor TSyncDirsSynchronizer.Create(
  const callback: ISyncDirsSynchronizerCallback;
  const fileProcessor: ISyncDirsFileProcessorWithUI;
  const filteredList: TFlatDirFileList );
begin
  _callback:= callback;
  _fileProcessor:= fileProcessor;
  _filteredList:= filteredList;
end;

function TSyncDirsSynchronizer.count: TSyncDirsSyncCount;
var
  i: Integer;
  rec: TFileSyncRec;
begin
  Result:= Default( TSyncDirsSyncCount );
  for i:= 0 to _filteredList.Count-1 do begin
    rec := _filteredList.fileSyncRec( i );
    case rec.action of
      srsCopyToLeft:
        begin
          Inc(Result.copyToLeftCount);
          Inc(Result.copyToLeftSize, rec.rightFile.Size);
        end;
      srsCopyToRight:
        begin
          Inc(Result.copyToRightCount);
          Inc(Result.copyToRightSize, rec.leftFile.Size);
        end;
      srsDeleteLeft:
        begin
          Inc(Result.deleteLeftCount);
        end;
      srsDeleteRight:
        begin
          Inc(Result.deleteRightCount);
        end;
      srsDeleteBoth:
        begin
          Inc(Result.deleteLeftCount);
          Inc(Result.deleteRightCount);
        end;
    end;
  end;
end;

procedure TSyncDirsSynchronizer.sync(const syncFlags: TSyncDirsSyncFlags);

  procedure processDir(const syncRec: TFileSyncRec);
  begin
    case syncRec.action of
      srsCopyToRight:
        CreateDirectoryFromFile(
          _rightFS,
          _rightBasePath + syncRec.relPath,
          _leftFS,
          syncRec.leftFile);
      srsCopyToLeft:
        CreateDirectoryFromFile(
          _leftFS,
          _leftBasePath + syncRec.relPath,
          _rightFS,
          syncRec.rightFile);
      srsDeleteRight:
        _fileProcessor.fileProcessorWithUIDeleteFile(_rightFS, syncRec.rightFile);
      srsDeleteLeft:
        _fileProcessor.fileProcessorWithUIDeleteFile(_leftFS, syncRec.leftFile);
    end;
  end;

var
  i: Integer;
  rec: TFileSyncRec;
  copyToLeftFiles: TFiles;
  copyToRightFiles: TFiles;
  deleteLeftFiles: TFiles;
  deleteRightFiles: TFiles;
  targetPath: string;
begin
  i:= 0;
  while i < _filteredList.Count do begin
    copyToLeftFiles:= TFiles.Create('');
    copyToRightFiles:= TFiles.Create('');
    deleteLeftFiles:= TFiles.Create('');
    deleteRightFiles:= TFiles.Create('');

    rec:= _filteredList.fileSyncRec(i);
    if rec.isDir then begin
      processDir(rec);
      i:= i + 1;
      continue;
    end;

    repeat
      targetPath := rec.relPath;
      case rec.action of
        srsCopyToRight:
          if sfCopyToRight in syncFlags then
            copyToRightFiles.Add(rec.leftFile.Clone);
        srsCopyToLeft:
          if sfCopyToLeft in syncFlags then
            copyToLeftFiles.Add(rec.rightFile.Clone);
        srsDeleteRight:
          if sfDeleteRight in syncFlags then
            deleteRightFiles.Add(rec.rightFile.Clone);
        srsDeleteLeft:
          if sfDeleteLeft in syncFlags then
            deleteLeftFiles.Add(rec.leftFile.Clone);
        srsDeleteBoth:
          begin
            if sfDeleteRight in syncFlags then
              deleteRightFiles.Add(rec.rightFile.Clone);
            if sfDeleteLeft in syncFlags then
              deleteLeftFiles.Add(rec.leftFile.Clone);
          end;
      end;
      i:= i + 1;
      if i < _filteredList.Count then
        rec:= _filteredList.fileSyncRec(i);
    until (i = _filteredList.Count) or rec.isDir;

    if copyToLeftFiles.Count > 0 then begin
      if NOT _fileProcessor.fileProcessorWithUICopyFiles(_rightFS, _leftFS, copyToLeftFiles, _leftBasePath + targetPath) then
        Break;
    end else begin
      copyToLeftFiles.Free;
    end;

    if copyToRightFiles.Count > 0 then begin
      if NOT _fileProcessor.fileProcessorWithUICopyFiles(_leftFS, _rightFS, copyToRightFiles, _rightBasePath + targetPath) then
        Break;
    end else begin
      copyToRightFiles.Free;
    end;

    if deleteLeftFiles.Count > 0 then begin
      if NOT _fileProcessor.fileProcessorWithUIDeleteFiles(_leftFS, deleteLeftFiles) then
        Break;
    end else begin
      deleteLeftFiles.Free;
    end;

    if deleteRightFiles.Count > 0 then begin
      if NOT _fileProcessor.fileProcessorWithUIDeleteFiles(_rightFS, deleteRightFiles) then
        Break;
    end else begin
      deleteRightFiles.Free;
    end;

    if NOT _callback.synchronizerCheckRunning then
      Break;
  end;
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


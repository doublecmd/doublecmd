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
  uFile, uFileSource, uFileSourceManager, uFileSourceUtil, uFileSystemFileSource,
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

  { TSyncDirsUtil }

  TSyncDirsUtil = class
  public
    class function consultCopyOperation(var params: TFileSourceConsultParams): Boolean;
    class function consultAndConfirmCopyOperation(var params: TFileSourceConsultParams): Boolean;
    class function supportsSyncDirs(const sourceFS: IFileSource; const targetFS: IFileSource): Boolean;
    class function supportsVerify(const sourceFS: IFileSource; const targetFS: IFileSource): Boolean;
  public
    class procedure filterFlatListWithFlags(
      const fullTree: TTwoLevelTree;
      const filterList: TFlatDirFileList;
      const filterFlags: TFilterFlags );

    class function selectionToStringList(
      const filteredList: TFlatDirFileList;
      const indexes: TIntegerList;
      const option: TSyncDirsCompareOption ): TStringList;
  public
    class function copyFiles(
      const sourceFS: IFileSource;
      const targetFS: IFileSource;
      var files: TFiles;
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
      var files: TFiles;
      const targetPath: String): Boolean;
    function fileProcessorWithUIDeleteFiles(
      const fs: IFileSource;
      var files: TFiles): Boolean;
    function fileProcessorWithUIDeleteFile(
      const fs: IFileSource;
      const f: TFile): Boolean;
  end;

  { TSyncDirsSortService }

  TSyncDirsSortService = class
  private
    _sortIndex: Integer;
    _sortDesc: Boolean;
  public
    procedure sortTree( const tree: TTwoLevelTree );
    procedure sortDirItem( const dirItem: TTwoLevelTreeDirItem );

    property sortIndex: Integer write _sortIndex;
    property sortDesc: Boolean write _sortDesc;
  end;

  { TSyncDirsDeleteService }

  TSyncDirsDeleteService = class
  private
    _fileProcessor: ISyncDirsFileProcessorWithUI;
    _filteredList: TFlatDirFileList;
    _leftFS: IFileSource;
    _rightFS: IFileSource;
  public
    constructor Create( const fileProcessor: ISyncDirsFileProcessorWithUI; const filteredList: TFlatDirFileList );
    procedure delete( const indexes: TIntegerList; const deleteLeft: Boolean; const deleteRight: Boolean );
    function deleteAllEmptyDirs( const leftSide: Boolean ): Boolean;

    property leftFS: IFileSource write _leftFS;
    property rightFS: IFileSource write _rightFS;
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
    _sortedService: TSyncDirsSortService;
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
      const sortService: TSyncDirsSortService;
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
    function sync( const syncFlags: TSyncDirsSyncFlags ): Boolean;

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

{ TSyncDirsUtil }

class function TSyncDirsUtil.consultCopyOperation( var params: TFileSourceConsultParams ): Boolean;
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

class function TSyncDirsUtil.consultAndConfirmCopyOperation( var params: TFileSourceConsultParams ): Boolean;
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

class function TSyncDirsUtil.supportsSyncDirs(
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

class function TSyncDirsUtil.supportsVerify(
  const sourceFS: IFileSource;
  const targetFS: IFileSource): Boolean;
begin
  Result:= sourceFS.IsClass(TFileSystemFileSource) AND targetFS.IsClass(TFileSystemFileSource);
end;

class procedure TSyncDirsUtil.filterFlatListWithFlags(
  const fullTree: TTwoLevelTree;
  const filterList: TFlatDirFileList;
  const filterFlags: TFilterFlags );

  function isMatching(const rec: TSyncRec): Boolean;
  begin
    if rec.state = srsDeleted then
      Exit(False);

    Result:=
      ((rec.hasFileOnOnlyOneSide and (ffSingle in filterFlags)) or
       (rec.hasFilesOnBothSides and (ffDuplicate in filterFlags)))
       and
       (((rec.state = srsCopyToLeft) or (rec.action = srsCopyToLeft)) and (ffCopyLeft in filterFlags) or
        ((rec.state = srsCopyToRight) or (rec.action = srsCopyToRight)) and (ffCopyRight in filterFlags) or
        (rec.state = srsDeleteLeft) and (ffCopyRight in filterFlags) or
        (rec.state = srsDeleteRight) and (ffCopyLeft in filterFlags) or
        (rec.state = srsEqual) and (ffEqual in filterFlags) or
        (rec.state = srsNotEq) and (ffNotEqual in filterFlags) or
        (rec.state = srsUnknown) and (ffUnknown in filterFlags));
  end;

  function isDirMatching(const syncRec: TSyncRec): Boolean;
  begin
    if syncRec.state = srsDeleted then begin
      Result:= False;
    end else if syncRec.state = srsDoNothing then begin
      Result:= True;
    end else begin
      Result:= isMatching(syncRec);
    end;
  end;

var
  dirIndex: Integer;
  fileIndex: Integer;
  rec: TSyncRec;
  currentDirItem: TTwoLevelTreeDirItem;
  currentDirPath: String;
begin
  filterList.Clear;
  for dirIndex:= 0 to fullTree.Count-1 do begin
    currentDirItem:= fullTree.dirItem( dirIndex );
    currentDirPath:= fullTree.dirPath( dirIndex );
    if currentDirPath <> EmptyStr then begin
      rec:= currentDirItem.dirSyncRec;
      if isDirMatching(rec) then
        filterList.addPath( IncludeTrailingPathDelimiter(currentDirPath), rec );
    end;
    for fileIndex:= 0 to currentDirItem.fileCount-1 do begin
      rec:= currentDirItem.fileSyncRec( fileIndex );
      if isMatching(rec) then
        filterList.addPath( currentDirItem.files[fileIndex], rec );
    end;
  end;
  filterList.clearInvisibleDirs;
end;

class function TSyncDirsUtil.selectionToStringList(
  const filteredList: TFlatDirFileList;
  const indexes: TIntegerList;
  const option: TSyncDirsCompareOption ): TStringList;

  procedure PrintRow(sl: TStringList; R: Integer);
  var
    s: string;
    rec: TSyncRec;
  begin
    rec := filteredList.fileSyncRec(R);
    if rec.isDir then
    begin
      s := filteredList.path(R);
      if cfEmptyDirs in option.flags then begin
        if rec.state <> srsDoNothing then
          s := s + #9#9#9 + SYNC_REC_STATE_SYMBOL[rec.action];
      end;
    end
    else
    begin
      if Assigned(rec.leftFile) then
      begin
        s := filteredList.path(R) + #9 +
             IntToStrTS(rec.leftFile.Size) + #9 +
             FormatDateTime(gDateTimeFormatSync, rec.leftFile.ModificationTime);
      end
      else
      begin
        s := #9#9;
      end;
      s := s + #9 + SYNC_REC_STATE_SYMBOL[rec.action] + #9;
      if Assigned(rec.rightFile) then
      begin
        s := s +
             FormatDateTime(gDateTimeFormatSync, rec.rightFile.ModificationTime) + #9 +
             IntToStrTS(rec.rightFile.Size) + #9 +
             filteredList.path(R);
      end;
    end;
    sl.Add(s);
  end;

var
  sl: TStringList;
  i: Integer;
begin
  sl:= TStringList.Create;
  for i in indexes do
    PrintRow( sl, i );
  Result:= sl;
end;

class function TSyncDirsUtil.copyFiles(
  const sourceFS: IFileSource;
  const targetFS: IFileSource;
  var files: TFiles;
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
  Result:= TSyncDirsUtil.consultAndConfirmCopyOperation(params);
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
  files:= params.files;
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

class function TSyncDirsUtil.deleteFiles(
  const fs: IFileSource;
  var files: TFiles;
  const operationHandle: TSyncDirsOperationHandle ): Boolean;
var
  fsOperation: TFileSourceOperation;
begin
  Result:= True;
  if files.Count = 0 then
    Exit;

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

{ TSyncDirsSortService }

procedure TSyncDirsSortService.sortTree( const tree: TTwoLevelTree );
var
  i: Integer;
begin
  if _sortIndex < 0 then
    Exit;
  for i:= 0 to tree.Count-1 do
    self.sortDirItem( tree.dirItem(i) );
end;

procedure TSyncDirsSortService.sortDirItem( const dirItem: TTwoLevelTreeDirItem );

  function CompareFn(sl: TStringList; i, j: Integer): Integer;
  var
    r1, r2: TSyncRec;
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

{ TSyncDirsDeleteService }

constructor TSyncDirsDeleteService.Create(
  const fileProcessor: ISyncDirsFileProcessorWithUI;
  const filteredList: TFlatDirFileList );
begin
  _fileProcessor:= fileProcessor;
  _filteredList:= filteredList;
end;

procedure TSyncDirsDeleteService.delete(
  const indexes: TIntegerList;
  const deleteLeft: Boolean;
  const deleteRight: Boolean );
var
  leftFiles: TFiles = nil;
  rightFiles: TFiles = nil;
begin
  try
    if deleteLeft then
      leftFiles:= TFiles.Create(EmptyStr);
    if deleteRight then
      rightFiles:= TFiles.Create(EmptyStr);

    _filteredList.deleteAndGetSelected( indexes, leftFiles, rightFiles );

    if deleteLeft then
      _fileProcessor.fileProcessorWithUIDeleteFiles( _leftFS, leftFiles );
    if deleteRight then
      _fileProcessor.fileProcessorWithUIDeleteFiles( _rightFS, rightFiles );
  finally
    leftFiles.Free;
    rightFiles.Free;
  end;
end;

function TSyncDirsDeleteService.deleteAllEmptyDirs(const leftSide: Boolean): Boolean;
var
  fs: IFileSource;
  fullTree: TTwoLevelTree;
  dirSyncRec: TSyncDirRec;
  dirIndex: Integer;

  function doRemoveDir: Boolean;
  var
    f: TFile;
  begin
    Result:= True;
    f:= dirSyncRec.doubleFiles[leftSide];
    if NOT Assigned(f) then
      Exit;

    Result:= _fileProcessor.fileProcessorWithUIDeleteFile( fs, f );
    if NOT Result then
      Exit;

    fullTree.removeFile( dirSyncRec, leftSide );
    if NOT dirSyncRec.hasFileOnAnySide then
      dirSyncRec.state:= srsDeleted;
  end;

begin
  Result:= True;
  if leftSide then
    fs:= _leftFS
  else
    fs:= _rightFS;

  fullTree:= _filteredList.fullTree;
  for dirIndex:= fullTree.Count-1 downto 0 do begin
    dirSyncRec:= fullTree.dirItem(dirIndex).dirSyncRec;
    if dirSyncRec.state = srsDeleted then
      continue;
    if dirSyncRec.relPath = EmptyStr then
      continue;
    if dirSyncRec.isEmpty(leftSide) then begin
      Result:= doRemoveDir;
      if NOT Result then
        break;
    end;
  end;
end;

{ TSyncDirsTreeBuilder }

constructor TSyncDirsTreeBuilder.Create(
  const callback: ISyncDirsTreeBuilderCallback;
  const sortService: TSyncDirsSortService;
  const compareOption: TSyncDirsCompareOption );
begin
  _callback:= callback;
  _sortedService:= sortService;
  _compareOption:= compareOption;
  _leftFirst:= True;
  _rightFirst:= True;
end;

procedure TSyncDirsTreeBuilder.build(const FFullTree: TTwoLevelTree);
  procedure processOneSide(
    const dirItem: TTwoLevelTreeDirItem;
    const parentDirIndex: Integer;
    const dirs: TStringList;
    var ASide: Boolean;
    const sideLeft: Boolean);
  var
    dir: String;
    fs: TFiles;
    i, j: Integer;
    f: TFile;
    rec: TSyncRec;
    fn: String;
    dirFullPath: String;
    dirSyncRec: TSyncDirRec;
    currentFileSource: IFileSource;
  begin
    dirSyncRec := dirItem.dirSyncRec;
    dir:= dirSyncRec.relPath;
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
              dirSyncRec.incDirCount(sideLeft, 1);
            end;
          end;
        end else if _callback.treeBuilderMaskFilt(f) then begin
          j := dirItem.indexOfFile(fn);
          if j < 0 then
            rec := TSyncFileRec.Create(_compareOption, dir, parentDirIndex)
          else
            rec := dirItem.fileSyncRec(j);
          rec.doubleFiles[sideLeft]:= f.Clone;
          rec.updateState;
          dirItem.addFile(fn, rec);
          dirSyncRec.incFileCount(sideLeft, 1);
        end;
      end;
    finally
      fs.Free;
    end;
  end;

  procedure setDirSyncRecFile(
    const dirSyncRec: TSyncDirRec;
    const leftParentDirs: TStringList;
    const rightParentDirs: TStringList);
  var
    i: Integer;
    currentDirPart: String;
  begin
    currentDirPart:= GetLastDir(dirSyncRec.relPath);
    i:= leftParentDirs.IndexOf(currentDirPart);
    if i >= 0 then
      dirSyncRec.leftFile:= TFile(leftParentDirs.Objects[i]);     // owns file
    i:= rightParentDirs.IndexOf(currentDirPart);
    if i >= 0 then
      dirSyncRec.rightFile:= TFile(rightParentDirs.Objects[i]);   // owns file
  end;

  procedure scanDir(
    dir: string;
    const parentDirIndex: Integer;
    const leftParentDirs: TStringList;
    const rightParentDirs: TStringList);
  var
    dirItem: TTwoLevelTreeDirItem;
    dirSyncRec: TSyncDirRec;
    currentDirIndex: Integer;
    dirsLeft: TStringListEx;
    dirsRight: TStringListEx;

    procedure addFiles;
    var
      i: Integer;
      rightIndex: Integer;
      totalCount: Integer;
      dirPart: String;
    begin
      totalCount:= dirsLeft.Count + dirsRight.Count;
      for i:= 0 to dirsLeft.Count - 1 do begin
        if dir = '' then
          _callback.onTreeBuilderUpdateProgress( i * 100 div totalCount );
        dirPart:= dirsLeft[i];
        scanDir(dir + dirPart, currentDirIndex, dirsLeft, dirsRight);
        if NOT _callback.treeBuilderCheckRunning(False) then
          Exit;
        rightIndex:= dirsRight.IndexOf(dirPart);
        if rightIndex >= 0 then begin
          dirsRight.Delete(rightIndex);
          Dec(totalCount);
        end
      end;

      for i:= 0 to dirsRight.Count-1 do begin
        if dir = '' then
          _callback.onTreeBuilderUpdateProgress( (dirsLeft.Count + i) * 100 div totalCount );
        dirPart:= dirsRight[i];
        scanDir(dir + dirPart, currentDirIndex, dirsLeft, dirsRight);
        if NOT _callback.treeBuilderCheckRunning(False) then
          Exit;
      end;
    end;

  begin
    currentDirIndex:= FFullTree.indexOfDir(dir);
    if currentDirIndex < 0 then begin
      dirSyncRec:= TSyncDirRec.Create(_compareOption, dir, parentDirIndex);
      dirItem:= TTwoLevelTreeDirItem.Create(dirSyncRec);
      currentDirIndex:= FFullTree.addDir(dir, dirItem);
    end else begin
      dirItem:= FFullTree.dirItem(currentDirIndex);
      dirSyncRec:= dirItem.dirSyncRec;
    end;

    if dir <> '' then begin
      setDirSyncRecFile(dirSyncRec, leftParentDirs, rightParentDirs);
      dir:= AppendPathDelim(dir);
    end;

    dirsLeft:= TStringListEx.Create;
    dirsLeft.CaseSensitive:= FileNameCaseSensitive;
    dirsLeft.Sorted:= True;
    dirsRight:= TStringListEx.Create;
    dirsRight.CaseSensitive:= FileNameCaseSensitive;
    dirsRight.Sorted:= True;
    try
      if NOT _callback.treeBuilderCheckRunning(True) then
        Exit;
      processOneSide(dirItem, currentDirIndex, dirsLeft, _leftFirst, True);
      processOneSide(dirItem, currentDirIndex, dirsRight, _rightFirst, False);
      dirSyncRec.updateState;
      _sortedService.sortDirItem(dirItem);
      if not (cfSubdirs in _compareOption.flags) then
        Exit;
      addFiles;
    finally
      dirsLeft.Free;
      dirsRight.Free;
    end;
  end;

begin
  scanDir('', -1, nil, nil);
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
  rec: TSyncRec;
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

function TSyncDirsSynchronizer.sync(const syncFlags: TSyncDirsSyncFlags): Boolean;
var
  index: Integer;
  rec: TSyncRec;

  procedure doRemoveFile( const leftFiles: TFiles; const rightFiles: TFiles );
  begin
    if Assigned(leftFiles) and Assigned(rec.leftFile) then begin
      leftFiles.Add( rec.leftFile );
      _filteredList.removeFile( index, True );
    end;

    if Assigned(rightFiles) and Assigned(rec.rightFile) then begin
      rightFiles.Add( rec.rightFile );
      _filteredList.removeFile( index, False );
    end;

    if NOT rec.hasFileOnAnySide then
      rec.state:= srsDeleted;
  end;

  function doRemoveDir(const leftFS: IFileSource; const rightFS: IFileSource): Boolean;
  var
    fs: IFileSource;
    f: TFile;
  begin
    if Assigned(leftFS) then begin
      fs:= leftFS;
      f:= rec.leftFile;
      _filteredList.removeFile( index, True );
    end else if Assigned(rightFS) then begin
      fs:= rightFS;
      f:= rec.rightFile;
      _filteredList.removeFile( index, False );
    end;

    if NOT rec.hasFileOnAnySide then
      rec.state:= srsDeleted;

    Result:= Assigned(fs) and Assigned(f);
    if Result then
      Result:= _fileProcessor.fileProcessorWithUIDeleteFile( fs, f );
  end;

  procedure doCopyDir;
  var
    newPath: String;
    newFile: TFile;
  begin
    if rec.action = srsCopyToRight then begin
      newPath:= _rightBasePath + rec.relPath;
      CreateDirectoryFromFile(
        _rightFS,
        newPath,
        _leftFS,
        rec.leftFile);
      newFile:= _rightFS.CreateFileObject( EmptyStr );
      newFile.FullPath:= newPath;
      _filteredList.addFile( index, False, newFile );
    end else begin
      newPath:= _leftBasePath + rec.relPath;
      CreateDirectoryFromFile(
        _leftFS,
        newPath,
        _rightFS,
        rec.rightFile);
      newFile:= _leftFS.CreateFileObject( EmptyStr );
      newFile.FullPath:= newPath;
      _filteredList.addFile( index, True, newFile );
    end;
  end;

  procedure doCopyFile( const copyToLeftFiles: TFiles; const copyToRightFiles: TFiles );
  var
    newPath: String;
    oldFile: TFile;
    newFile: TFile;
  begin
    if Assigned(copyToRightFiles) then begin
      oldFile:= rec.leftFile.Clone;
      copyToRightFiles.Add( oldFile );
      newPath:= _rightBasePath + rec.relPath;
      newFile:= _rightFS.CreateFileObject( newPath );
      newFile.Name:= oldFile.Name;
      _filteredList.addFile( index, False, newFile );
    end else if Assigned(copyToLeftFiles) then begin
      oldFile:= rec.rightFile.Clone;
      copyToLeftFiles.Add( oldFile );
      newPath:= _leftBasePath + rec.relPath;
      newFile:= _leftFS.CreateFileObject( newPath );
      newFile.Name:= oldFile.Name;
      _filteredList.addFile( index, True, newFile );
    end;
  end;

  function processDir: Boolean;
  begin
    Result:= False;
    case rec.action of
      srsCopyToRight:
        if sfCopyToRight in syncFlags then
          doCopyDir;
      srsCopyToLeft:
        if sfCopyToLeft in syncFlags then
          doCopyDir;
      srsDeleteRight:
        if sfDeleteRight in syncFlags then
          if NOT doRemoveDir(nil, _rightFS) then
            Exit;
      srsDeleteLeft:
        if sfDeleteLeft in syncFlags then
          if NOT doRemoveDir(_leftFS, nil) then
            Exit;
    end;
    Inc( index );
    Result:= True;
  end;

  function processFiles: Boolean;
  var
    copyToLeftFiles: TFiles;
    copyToRightFiles: TFiles;
    deleteLeftFiles: TFiles;
    deleteRightFiles: TFiles;
    targetPath: string;

    function doProcessFiles: Boolean;
    begin
      Result:= False;

      repeat
        case rec.action of
          srsCopyToRight:
            if sfCopyToRight in syncFlags then
              doCopyFile( nil, copyToRightFiles );
          srsCopyToLeft:
            if sfCopyToLeft in syncFlags then
              doCopyFile( copyToLeftFiles, nil );
          srsDeleteRight:
            if sfDeleteRight in syncFlags then
              doRemoveFile( nil, deleteRightFiles );
          srsDeleteLeft:
            if sfDeleteLeft in syncFlags then
              doRemoveFile( deleteLeftFiles, nil );
          srsDeleteBoth:
            begin
              if sfDeleteRight in syncFlags then
                doRemoveFile( nil, deleteRightFiles );
              if sfDeleteLeft in syncFlags then
                doRemoveFile( deleteLeftFiles, nil );
            end;
        end;
        index:= index + 1;
        if index < _filteredList.Count then
          rec:= _filteredList.fileSyncRec(index);
      until (index = _filteredList.Count) or rec.isDir;

      if copyToLeftFiles.Count > 0 then begin
        if NOT _fileProcessor.fileProcessorWithUICopyFiles(_rightFS, _leftFS, copyToLeftFiles, _leftBasePath + targetPath) then
          Exit;
      end;

      if copyToRightFiles.Count > 0 then begin
        if NOT _fileProcessor.fileProcessorWithUICopyFiles(_leftFS, _rightFS, copyToRightFiles, _rightBasePath + targetPath) then
          Exit;
      end;

      if deleteLeftFiles.Count > 0 then begin
        if NOT _fileProcessor.fileProcessorWithUIDeleteFiles(_leftFS, deleteLeftFiles) then
          Exit;
      end;

      if deleteRightFiles.Count > 0 then begin
        if NOT _fileProcessor.fileProcessorWithUIDeleteFiles(_rightFS, deleteRightFiles) then
          Exit;
      end;

      Result:= True;
    end;
  begin
    targetPath:= rec.relPath;

    copyToLeftFiles:= TFiles.Create('');
    copyToRightFiles:= TFiles.Create('');
    deleteLeftFiles:= TFiles.Create('');
    deleteRightFiles:= TFiles.Create('');

    try
      Result:= doProcessFiles;
    finally
      copyToLeftFiles.Free;
      copyToRightFiles.Free;
      deleteLeftFiles.Free;
      deleteRightFiles.Free;
    end;
  end;

  function deleteAllEmptyDirs( const leftSide: Boolean ): Boolean;
  var
    deleteService: TSyncDirsDeleteService;
  begin
    deleteService:= TSyncDirsDeleteService.Create(_fileProcessor, _filteredList);
    deleteService.leftFS:= _leftFS;
    deleteService.rightFS:= _rightFS;

    try
      Result:= deleteService.deleteAllEmptyDirs( leftSide );
    finally
      deleteService.Free;
    end;
  end;

begin
  Result:= True;
  index:= 0;
  while index < _filteredList.Count do begin
    rec:= _filteredList.fileSyncRec(index);
    if rec.isDir then begin
      if NOT processDir then
        break;
    end else begin
      if NOT processFiles then
        break;
    end;
    Result:= _callback.synchronizerCheckRunning;
    if NOT Result then
      break;
  end;

  if Result AND (sfDeleteLeftAllEmptyDirs in syncFlags) then
    Result:= deleteAllEmptyDirs( True );
  if Result AND (sfDeleteRightAllEmptyDirs in syncFlags) then
    Result:= deleteAllEmptyDirs( False );
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
  rec: TSyncRec;
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
          if rec.isFile and (rec.state = srsUnknown) then
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
        if rec.isFile and (rec.state = srsUnknown) then
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


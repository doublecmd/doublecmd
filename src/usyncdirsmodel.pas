unit uSyncDirsModel;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Types,
  IntegerList,
  LazFileUtils,
  DCClassesUtf8, DCDateTimeUtils,
  uFile;

type

  { TSyncRecState }

  TSyncRecState = (
    srsUnknown,
    srsEqual,
    srsNotEq,
    srsCopyToLeft,
    srsCopyToRight,
    srsDeleteLeft,
    srsDeleteRight,
    srsDeleteBoth,
    srsDoNothing,

    srsNextAction,
    srsNoAction,
    srsDeleted
  );

  { TSyncDirsCompareFlag }

  TSyncDirsCompareFlag = (
    cfOnlySelected,
    cfEmptyDirs,
    cfAsymmetric,
    cfSubdirs,
    cfByContent,
    cfIgnoreDate,

    cfNtfsShift
  );

  TSyncDirsCompareFlags = set of TSyncDirsCompareFlag;

  { TSyncDirsCompareOption }

  TSyncDirsCompareOption = class
  private
    _flags: TSyncDirsCompareFlags;
    _stateWithoutLeft: TSyncRecState;

  public
    constructor Create(const flags: TSyncDirsCompareFlags);
    property flags: TSyncDirsCompareFlags read _flags;
    property stateWithoutLeft: TSyncRecState read _stateWithoutLeft;
  end;

  { TSyncDirsFilterFlag }

  TSyncDirsFilterFlag = (
    ffCopyRight,
    ffCopyLeft,
    ffEqual,
    ffNotEqual,
    ffUnknown,
    ffDuplicate,
    ffSingle
  );

  TFilterFlags = set of TSyncDirsFilterFlag;

  { TSyncDirsSyncFlag }

  TSyncDirsSyncFlag = (
    sfCopyToLeft,
    sfCopyToRight,
    sfDeleteLeft,
    sfDeleteRight
  );

  TSyncDirsSyncFlags = set of TSyncDirsSyncFlag;

  { TSyncDirsSyncCount }

  TSyncDirsSyncCount = record
    copyToLeftSize: Int64;
    copyToRightSize: Int64;
    copyToLeftCount: Integer;
    copyToRightCount: Integer;
    deleteLeftCount: Integer;
    deleteRightCount: Integer;

    function copySize: Int64;
    function copyCount: Integer;
    function deleteCount: Integer;
  end;

  { TSyncRec }

  TSyncRec = class
  protected
    _relPath: String;
    _parentDirIndex: Integer;
    _state: TSyncRecState;
    _action: TSyncRecState;
    _option: TSyncDirsCompareOption;
    _leftFile: TFile;
    _rightFile: TFile;
  public
    constructor Create(const option: TSyncDirsCompareOption; const relPath: String; const parentDirIndex: Integer);
    destructor Destroy; override;

    function isFile: Boolean;
    function isDir: Boolean; virtual; abstract;
    function isDeletable( const leftSide: Boolean ): Boolean; virtual;

    function fileBySide( const leftSide: Boolean ): TFile;
    function hasFilesOnBothSides: Boolean;
    function hasFileOnOnlyOneSide: Boolean;
    function hasFileOnAnySide: Boolean;

    procedure updateState; virtual;
    function getProperAction(const expectAction: TSyncRecState): TSyncRecState; virtual;
    function getNextAction: TSyncRecState; virtual;

    property relPath: String read _relPath;
    property parentDirIndex: Integer read _parentDirIndex;
    property state: TSyncRecState read _state write _state;
    property action: TSyncRecState read _action write _action;

    property leftFile: TFile read _leftFile write _leftFile;
    property rightFile: TFile read _rightFile write _rightFile;

    property option: TSyncDirsCompareOption read _option;
  end;

  { TSyncFileRec }

  TSyncFileRec = class( TSyncRec )
  public
    function isDir: Boolean; override;
    procedure updateState; override;
    function getNextAction: TSyncRecState; override;
  end;

  { TSyncDirRec }

  TSyncDirRec = class( TSyncRec )
  private
    _dirCount: array [Boolean] of Integer;
    _fileCount: array [Boolean] of Integer;
  public
    procedure updateState; override;
    function getNextAction: TSyncRecState; override;

    function isDir: Boolean; override;
    function isDeletable(const leftSide: Boolean): Boolean; override;
    procedure incDirCount(const side: Boolean);
    procedure decDirCount(const side: Boolean);
    procedure incFileCount(const side: Boolean);
    procedure decFileCount(const side: Boolean);
    function fileCount(const side: Boolean): Integer;
    function noDirDescendant(const side: Boolean): Boolean;
    function noDirDescendant: Boolean;
    function noFileDescendant(const side: Boolean): Boolean;
    function noFileDescendant: Boolean;
    function isEmpty(const side: Boolean): Boolean;
  end;

  { TTwoLevelTreeDirItem }

  TTwoLevelTreeDirItem = class
  private
    _dirSyncRec: TSyncDirRec;
    _files: TStringListEx;
  public
    constructor Create(const dirSyncRec: TSyncDirRec);
    destructor Destroy; override;

    procedure addFile( const filename: String; const fileSyncRec: TSyncRec );

    function fileCount: Integer;
    function fileSyncRec( const fileIndex: Integer ): TSyncRec;
    function files: TStringListEx;
    function indexOfFile( const filename: String ): Integer;

    property dirSyncRec: TSyncDirRec read _dirSyncRec;
  end;

  { TTwoLevelTree }

  TTwoLevelTree = class
  private
    _dirs: TStringListEx;
  public
    constructor Create;
    destructor Destroy; override;

    function addDir( const dirPath: String; const item: TTwoLevelTreeDirItem ): Integer;
    procedure Clear;

    function Count: Integer;
    function indexOfDir( const dirPath: String ): Integer;
    function dirPath( const dirIndex: Integer ): String;
    function dirItem( const dirIndex: Integer ): TTwoLevelTreeDirItem;
    function fileSyncRec( const dirIndex: Integer; const fileIndex: Integer ): TSyncRec;
  end;

  { TSyncDirsFlatCount }

  TSyncDirsFlatCount = record
    total: Integer;
    equal: Integer;
    notEqual: Integer;
    leftUnique: Integer;
    rightUnique: Integer;
  end;

  { TFlatDirFileList }

  TFlatDirFileList = class
  private
    _list: TStringListEx;
    _fullTree: TTwoLevelTree;
  private
    function findParentDirRec( const childIndex: Integer ): TSyncDirRec;
    procedure decParentDirRecChildrenCount( const childIndex: Integer; const leftSide: Boolean );
  public
    constructor Create( const fullTree: TTwoLevelTree );
    destructor Destroy; override;

    procedure addPath( const path: String; const syncRec: TSyncRec );
    procedure Delete( const index: Integer );
    procedure FullyDelete( const index: Integer );
    procedure clearInvisibleDirs;
    procedure Clear;

    procedure removeLeft( const index: Integer );
    procedure removeRight( const index: Integer );

    function Count: Integer;
    function path( const index: Integer ): String;
    function fileSyncRec( const index: Integer ): TSyncRec;
    procedure countLeftRight( const indexes: TIntegerList; out leftCount: Integer; out rightCount: Integer );
    function flatCount: TSyncDirsFlatCount;

    function lastFileInCurrentDir(const fromIndex: Integer): Integer;
    procedure deleteAndGetSelected(const indexes: TIntegerList; const leftFiles: TFiles; const rightFiles: TFiles);

    procedure setNewAction( const indexes: TIntegerList; const newAction: TSyncRecState );
  end;

implementation

{ TSyncDirsCompareOption }

constructor TSyncDirsCompareOption.Create(const flags: TSyncDirsCompareFlags);
begin
  _flags:= flags;
  if cfAsymmetric in flags then
    _stateWithoutLeft:= srsDeleteRight
  else
    _stateWithoutLeft:= srsCopyToLeft;
end;

{ TSyncDirsSyncCount }

function TSyncDirsSyncCount.copySize: Int64;
begin
  Result:= self.copyToLeftSize + self.copyToRightSize;
end;

function TSyncDirsSyncCount.copyCount: Integer;
begin
  Result:= self.copyToLeftCount + self.copyToRightCount;
end;

function TSyncDirsSyncCount.deleteCount: Integer;
begin
  Result:= self.deleteLeftCount + self.deleteRightCount;
end;

{ TSyncRec }

constructor TSyncRec.Create(
  const option: TSyncDirsCompareOption;
  const relPath: String;
  const parentDirIndex: Integer );
begin
  _option:= option;
  _relPath:= relPath;
  _parentDirIndex:= parentDirIndex;
end;

destructor TSyncRec.Destroy;
begin
  FreeAndNil( _leftFile );
  FreeAndNil( _rightFile );
  inherited Destroy;
end;

function TSyncRec.isFile: Boolean;
begin
  Result:= NOT self.isDir;
end;

procedure TSyncRec.updateState;
begin
  if Assigned(_rightFile) and NOT Assigned(_leftFile) then begin
    _state:= _option.stateWithoutLeft;
  end else if NOT Assigned(_rightFile) and Assigned(_leftFile) then begin
    _state:= srsCopyToRight;
  end;
  _action:= _state;
end;

function TSyncRec.getProperAction( const expectAction: TSyncRecState ): TSyncRecState;
begin
  Result:= expectAction;
  case expectAction of
    srsDoNothing:           // expect Clear Action
      if _state = srsEqual then
        Result:= srsEqual;
    srsUnknown:             // expect CopyDefault
      Result:= _state;
    srsNotEq:               // expect CopyReverse
      begin
        if (_action = srsCopyToLeft) and Assigned(_leftFile) then
          Result:= srsCopyToRight
        else if (_action = srsCopyToRight) and Assigned(_rightFile) then
          Result:= srsCopyToLeft
        else
          Result:= _action;
      end;
    srsCopyToLeft,
    srsDeleteRight:
      if NOT Assigned(_rightFile) then
        Result:= srsDoNothing;
    srsCopyToRight,
    srsDeleteLeft:
      if NOT Assigned(_leftFile) then
        Result:= srsDoNothing;
    srsDeleteBoth:
      begin
        if NOT Assigned(_leftFile) then
          Result:= srsDeleteRight;
        if NOT Assigned(_rightFile) then
          Result:= srsDeleteLeft;
      end;
    srsNextAction:
      Result:= self.getNextAction;
  end;
end;

function TSyncRec.getNextAction: TSyncRecState;
begin
  Result:= _action;
  case _action of
    srsNotEq:
      Result:= srsCopyToRight;
    srsCopyToRight:
      if Assigned(_rightFile) then
        Result:= srsCopyToLeft
      else
        Result:= srsDoNothing;
    srsCopyToLeft:
      if Assigned(_leftFile) then
        Result:= srsNotEq
      else
        Result:= srsDoNothing;
    srsDeleteRight:
      if not (cfAsymmetric in _option.flags) then
        Result:= _state
      else
        Result:= srsDoNothing;
    srsDeleteLeft,
    srsDeleteBoth:
      Result:= _state;
    srsDoNothing:
      if Assigned(_leftFile) then
        Result:= srsCopyToRight
      else
        Result:= _option.stateWithoutLeft;
  end;
end;

function TSyncRec.isDeletable( const leftSide: Boolean ): Boolean;
begin
  Result:= Assigned( self.fileBySide(leftSide) );
end;

function TSyncRec.fileBySide(const leftSide: Boolean): TFile;
begin
  if leftSide then
    Result:= _leftFile
  else
    Result:= _rightFile;
end;

function TSyncRec.hasFilesOnBothSides: Boolean;
begin
  Result:= Assigned(_leftFile) and Assigned(_rightFile);
end;

function TSyncRec.hasFileOnOnlyOneSide: Boolean;
begin
  Result:= Assigned(_leftFile) <> Assigned(_rightFile);
end;

function TSyncRec.hasFileOnAnySide: Boolean;
begin
  Result:= Assigned(_leftFile) or Assigned(_rightFile);
end;

{ TSyncFileRec }

function TSyncFileRec.isDir: Boolean;
begin
  Result:= False;
end;

procedure TSyncFileRec.updateState;
  procedure compareTwoSides;
    procedure compareDate;
    var
      dateDiff: Integer;
    begin
      if cfIgnoreDate in _option.flags then begin
        _state:= srsEqual;
        Exit;
      end;

      dateDiff:= FileTimeCompare(_leftFile.ModificationTime, _rightFile.ModificationTime, cfNtfsShift in _option.flags);
      if dateDiff = 0 then begin
        _state:= srsEqual;
      end else if dateDiff > 0 then begin
        _state:= srsCopyToRight;
      end else if dateDiff < 0 then begin
        _state:= srsCopyToLeft;
      end;
    end;
  begin
    _state:= srsNotEq;
    // by datetime
    compareDate;
    // by size
    if _state = srsEqual then begin
      if _leftFile.Size <> _rightFile.Size then
        _state:= srsNotEq;
    end;
    // by content
    if _state = srsEqual then begin
      if cfByContent in _option.flags then
        _state:= srsUnknown;
    end;
    // asymmetric
    if NOT (_state in [srsUnknown,srsEqual]) then begin
      if cfAsymmetric in _option.flags then
        _state:= srsCopyToRight;
    end;
    _action:= _state;
  end;
begin
  if self.hasFilesOnBothSides then begin
    compareTwoSides;
  end else begin
    inherited;
  end;
end;

function TSyncFileRec.getNextAction: TSyncRecState;
begin
  if _state = srsEqual then
    Exit( srsNoAction );
  Result:=inherited getNextAction;
end;

{ TSyncDirRec }

procedure TSyncDirRec.updateState;
begin
  _state:= srsDoNothing;
  _action:= srsDoNothing;
  if NOT (cfEmptyDirs in _option.flags) then
    Exit;
  if self.hasFileOnOnlyOneSide then
    inherited;
end;

function TSyncDirRec.getNextAction: TSyncRecState;
begin
  if _state = srsDoNothing then
    Result:= srsNoAction
  else
    Result:= inherited getNextAction;
end;

function TSyncDirRec.isDir: Boolean;
begin
  Result:= True;
end;

function TSyncDirRec.isDeletable( const leftSide: Boolean ): Boolean;
begin
  Result:= False;
  if NOT (cfEmptyDirs in _option.flags) then
    Exit;
  Result:= inherited isDeletable( leftSide );
  Result:= Result and self.isEmpty( leftSide );
end;

procedure TSyncDirRec.incDirCount(const side: Boolean);
begin
  Inc( _dirCount[side] );
end;

procedure TSyncDirRec.decDirCount(const side: Boolean);
begin
  Dec( _dirCount[side] );
end;

procedure TSyncDirRec.incFileCount(const side: Boolean);
begin
  Inc( _fileCount[side] );
end;

procedure TSyncDirRec.decFileCount(const side: Boolean);
begin
  Dec( _fileCount[side] );
end;

function TSyncDirRec.fileCount(const side: Boolean): Integer;
begin
  Result:= _fileCount[side];
end;

function TSyncDirRec.noDirDescendant(const side: Boolean): Boolean;
begin
  Result:= _dirCount[side] = 0;
end;

function TSyncDirRec.noDirDescendant: Boolean;
begin
  Result:= noDirDescendant(True) and noDirDescendant(False);
end;

function TSyncDirRec.noFileDescendant(const side: Boolean): Boolean;
begin
  Result:= _fileCount[side] = 0;
end;

function TSyncDirRec.noFileDescendant: Boolean;
begin
  Result:= noFileDescendant(True) and noFileDescendant(False);
end;

function TSyncDirRec.isEmpty(const side: Boolean): Boolean;
begin
  Result:= self.noDirDescendant(side) and self.noFileDescendant(side);
end;

{ TTwoLevelTreeDirItem }

constructor TTwoLevelTreeDirItem.Create(const dirSyncRec: TSyncDirRec);
begin
  _dirSyncRec:= dirSyncRec;
  _files:= TStringListEx.Create;
  _files.OwnsObjects:= True;
  _files.CaseSensitive:= FileNameCaseSensitive;
  _files.Sorted:= True;
end;

destructor TTwoLevelTreeDirItem.Destroy;
begin
  FreeAndNil( _dirSyncRec );
  FreeAndNil( _files );
end;

procedure TTwoLevelTreeDirItem.addFile( const filename: String; const fileSyncRec: TSyncRec );
begin
  _files.AddObject( filename, fileSyncRec );
end;

function TTwoLevelTreeDirItem.fileCount: Integer;
begin
  Result:= _files.Count;
end;

function TTwoLevelTreeDirItem.fileSyncRec(const fileIndex: Integer): TSyncRec;
begin
  Result:= TSyncRec( _files.Objects[fileIndex] );
end;

function TTwoLevelTreeDirItem.files: TStringListEx;
begin
  Result:= _files;
end;

function TTwoLevelTreeDirItem.indexOfFile(const filename: String): Integer;
begin
  Result:= _files.IndexOf( filename );
end;

{ TTwoLevelTree }

constructor TTwoLevelTree.Create;
begin
  _dirs:= TStringListEx.Create;
  _dirs.OwnsObjects:= True;
  _dirs.CaseSensitive:= FileNameCaseSensitive;
  // since the default comparison function performs a simple string comparison
  // without considering the path structure, the resulting path order does not
  // follow standard conventions.
  // Sorting is simply disabled here, it can be re-enabled if an efficient
  // path comparison function becomes available.
  // eg.
  // 1. /a/b
  // 2. /a-/b
  // 1st should come before 2nd, but if sorting is enabled with the default
  // comparison function is used, 2nd will come before 1st.
  //
  // _dirs.Sorted:= True;
end;

destructor TTwoLevelTree.Destroy;
begin
  FreeAndNil( _dirs );
end;

function TTwoLevelTree.addDir(const dirPath: String; const item: TTwoLevelTreeDirItem): Integer;
begin
  Result:= _dirs.AddObject( dirPath, item );
end;

procedure TTwoLevelTree.Clear;
begin
  _dirs.Clear;
end;

function TTwoLevelTree.Count: Integer;
begin
  Result:= _dirs.Count;
end;

function TTwoLevelTree.indexOfDir(const dirPath: String): Integer;
begin
  Result:= _dirs.IndexOf( dirPath );
end;

function TTwoLevelTree.dirPath(const dirIndex: Integer): String;
begin
  Result:= _dirs[dirIndex];
end;

function TTwoLevelTree.dirItem(const dirIndex: Integer): TTwoLevelTreeDirItem;
begin
  Result:= TTwoLevelTreeDirItem( _dirs.Objects[dirIndex] );
end;

function TTwoLevelTree.fileSyncRec(const dirIndex: Integer; const fileIndex: Integer): TSyncRec;
begin
  Result:= self.dirItem(dirIndex).fileSyncRec(fileIndex);
end;

{ TFlatDirFileList }

function TFlatDirFileList.findParentDirRec(const childIndex: Integer): TSyncDirRec;
var
  rec: TSyncRec;
  parentDirIndexInTree: Integer;
begin
  Result:= nil;
  rec:= self.fileSyncRec( childIndex );
  parentDirIndexInTree:= rec.parentDirIndex;
  if parentDirIndexInTree < 0 then
    Exit;
  Result:= _fullTree.dirItem(parentDirIndexInTree).dirSyncRec;
end;

procedure TFlatDirFileList.decParentDirRecChildrenCount(
  const childIndex: Integer; const leftSide: Boolean);
var
  parentDirRec: TSyncDirRec;
  childRec: TSyncRec;
begin
  parentDirRec:= findParentDirRec( childIndex );
  if Assigned(parentDirRec) then begin
    childRec:= self.fileSyncRec( childIndex );
    if childRec.isDir then begin
      parentDirRec.decDirCount( leftSide );
    end else begin
      parentDirRec.decFileCount( leftSide );
    end;
  end;
end;

constructor TFlatDirFileList.Create( const fullTree: TTwoLevelTree );
begin
  // not own Object
  _list:= TStringListEx.Create;
  _list.CaseSensitive:= FileNameCaseSensitive;
  _fullTree:= fullTree;
end;

destructor TFlatDirFileList.Destroy;
begin
  FreeAndNil( _list );
end;

procedure TFlatDirFileList.addPath( const path: String; const syncRec: TSyncRec );
begin
  _list.AddObject( path, syncRec );
end;

procedure TFlatDirFileList.Delete(const index: Integer);
begin
  _list.Delete( index );
end;

procedure TFlatDirFileList.FullyDelete(const index: Integer);
var
  rec: TSyncRec;
begin
  rec:= self.fileSyncRec( index );
  rec.state:= srsDeleted;
  self.Delete( index );
end;

procedure TFlatDirFileList.clearInvisibleDirs;
var
  i: Integer;
  rec: TSyncRec;
begin
  for i:= self.Count-1 downto 0 do begin
    rec:= self.fileSyncRec( i );
    if rec.isFile then
      continue;
    if rec.state <> srsDoNothing then
      continue;
    if i + 1 < self.Count then begin
      rec:= self.fileSyncRec(i+1);
      if rec.isFile then
        continue;
    end;
    self.Delete(i);
  end;
end;

procedure TFlatDirFileList.Clear;
begin
  _list.Clear;
end;

procedure TFlatDirFileList.removeLeft(const index: Integer);
var
  rec: TSyncRec;
begin
  decParentDirRecChildrenCount( index, True );
  rec:= self.fileSyncRec( index );
  rec.leftFile:= nil;
end;

procedure TFlatDirFileList.removeRight(const index: Integer);
var
  rec: TSyncRec;
begin
  decParentDirRecChildrenCount( index, False );
  rec:= self.fileSyncRec( index );
  rec.rightFile:= nil;
end;

function TFlatDirFileList.Count: Integer;
begin
  Result:= _list.Count;
end;

function TFlatDirFileList.path(const index: Integer): String;
begin
  Result:= _list[index];
end;

function TFlatDirFileList.fileSyncRec(const index: Integer): TSyncRec;
begin
  Result:= TSyncRec( _list.Objects[index] );
end;

procedure TFlatDirFileList.countLeftRight(
  const indexes: TIntegerList;
  out leftCount: Integer;
  out rightCount: Integer );
var
  i: Integer;
  rec: TSyncRec;
begin
  leftCount:= 0;
  rightCount:= 0;
  for i in indexes do begin
    rec:= self.fileSyncRec( i );
    if rec.isDir and NOT (cfEmptyDirs in rec.option.flags) then
      continue;
    if Assigned(rec.leftFile) then
      Inc( leftCount );
    if Assigned(rec.rightFile) then
      Inc( rightCount );
  end;
end;

function TFlatDirFileList.flatCount: TSyncDirsFlatCount;
var
  i: Integer;
  rec: TSyncRec;
begin
  Result:= Default( TSyncDirsFlatCount );
  for i:= 0 to self.Count-1 do begin
    rec:= self.fileSyncRec(i);
    if rec.isDir then
      continue;

    Inc( Result.total);

    if Assigned(rec.leftFile) and NOT Assigned(rec.rightFile) then
      Inc( Result.leftUnique )
    else if Assigned(rec.rightFile) and NOT Assigned(rec.leftFile) then
      Inc( Result.rightUnique );

    if rec.state = srsEqual then
      Inc( Result.equal )
    else if rec.state = srsNotEq then
      Inc( Result.notEqual )
    else if rec.hasFilesOnBothSides then
      Inc( Result.notEqual );
  end;
end;

function TFlatDirFileList.lastFileInCurrentDir( const fromIndex: Integer ): Integer;
var
  rec: TSyncRec;
begin
  Result:= fromIndex;
  rec:= self.fileSyncRec(fromIndex);
  if rec.isFile then
    Exit;

  Inc( Result );
  while Result < self.Count do begin
    rec:= self.fileSyncRec( Result );
    if rec.isDir then
      break;
    Inc( Result );
  end;
  Dec( Result );
end;

{
  when deleting an item in FilterList, FullTree will be synchronized
  the change via marking rather than actual deletion.

  if an item is deleted from FilteredList during the process,
  the SyncRec.state of that item will be marked as srsDeleted.

  since FilteredList and FullTree share the SyncRec, accessing
  the SyncRec.state of the item via FullTree also yields srcDeleted.

  it eliminates the need to actually delete these items from FullTree.
}
procedure TFlatDirFileList.deleteAndGetSelected(
  const indexes: TIntegerList;
  const leftFiles: TFiles;
  const rightFiles: TFiles );

  procedure doRemoveItem(const index: Integer);
  var
    rec: TSyncRec;
  begin
    rec:= self.fileSyncRec(index);

    if Assigned(leftFiles) and rec.isDeletable(True) then begin
      leftFiles.Add( rec.leftFile );
      self.removeLeft( index );
    end;

    if Assigned(rightFiles) and rec.isDeletable(False) then begin
      rightFiles.Add( rec.rightFile );
      self.removeRight( index );
    end;

    if rec.hasFileOnAnySide then begin
      rec.updateState;
    end else begin
      self.FullyDelete(index);
    end;
  end;

var
  i: Integer;
begin
  for i:=indexes.Count-1 downto 0 do
    doRemoveItem( indexes[i] );
end;

procedure TFlatDirFileList.setNewAction(
  const indexes: TIntegerList;
  const newAction: TSyncRecState );
var
  handled: TBooleanDynArray;

  procedure doUpdateAction(const index: Integer; const expectAction: TSyncRecState);
  var
    rec: TSyncRec;
    properAction: TSyncRecState;
  begin
    if handled[index] then
      Exit;
    handled[index]:= True;

    rec:= self.fileSyncRec(index);
    properAction:= rec.getProperAction( expectAction );
    if properAction <> srsNoAction then
      rec.action:= properAction;
  end;

  procedure checkAncestorsDirs(index: Integer; const cascadingAction: TSyncRecState);
  var
    rec: TSyncRec;
    basePath: String;
  begin
    rec:= self.fileSyncRec(index);
    if NOT (cfEmptyDirs in rec._option.flags) then
      Exit;

    basePath:= IncludeTrailingPathDelimiter(rec.relPath);

    Dec(index);
    while index >= 0 do begin
      rec:= self.fileSyncRec(index);
      if rec.relPath = EmptyStr then
        break;
      if NOT PathIsInPath(basePath, rec.relPath) then
        break;
      if rec.isDir then begin
        if rec.state = srsDoNothing then
          break;
        if cascadingAction = srsDoNothing then begin
          if NOT (rec.action in [srsDeleteLeft, srsDeleteRight, srsDeleteBoth]) then
            break;
        end;
        doUpdateAction(index, cascadingAction);
      end;
      Dec(index);
    end;
  end;

  procedure uncheckDescendantsDirsAndFiles(index: Integer; const cascadingAction: TSyncRecState);
  var
    rec: TSyncRec;
    basePath: String;
  begin
    rec:= self.fileSyncRec(index);
    basePath:= IncludeTrailingPathDelimiter(rec.relPath);
    Inc(index);
    if NOT (cfEmptyDirs in rec._option.flags) then begin
      while index < self.Count do begin
        rec:= self.fileSyncRec(index);
        if rec.isDir then
          break;
        doUpdateAction(index, cascadingAction);
        Inc(index);
      end;
    end else begin
      while index < self.Count do begin
        rec:= self.fileSyncRec(index);
        if cascadingAction = srsDoNothing then begin
          if NOT (rec.action in [srsCopyToLeft, srsCopyToRight]) then begin
            Inc(index);
            continue;
          end;
        end;
        if NOT PathIsInPath(rec.relPath, basePath) then
          break;
        doUpdateAction(index, cascadingAction);
        Inc(index);
      end;
    end;
  end;

  procedure processOneRec( const index: Integer );
  var
    rec: TSyncRec;
  begin
    if handled[index] then
      Exit;

    rec:= self.fileSyncRec(index);
    if rec.state = srsDoNothing then
      Exit;

    doUpdateAction(index, newAction);

    case rec.action of
      srsCopyToLeft,
      srsCopyToRight:
        checkAncestorsDirs(index, rec.action);
      srsDeleteLeft,
      srsDeleteRight,
      srsDeleteBoth:
        if rec.isDir then
          uncheckDescendantsDirsAndFiles(index, rec.action);
      srsDoNothing:
        if rec.isDir then
          uncheckDescendantsDirsAndFiles(index, rec.action)
        else
          checkAncestorsDirs(index, rec.action);
    end;
  end;

var
  i: Integer;
begin
  SetLength( handled, self.Count );    // handled auto released
  for i in indexes do
    processOneRec( i );
end;

end.


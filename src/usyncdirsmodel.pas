unit uSyncDirsModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types,
  IntegerList,
  LazFileUtils,
  DCClassesUtf8, DCDateTimeUtils,
  uFile;

type

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

  TCompareFlag = (
    cfOnlySelected,
    cfEmptyDirs,
    cfAsymmetric,
    cfSubdirs,
    cfByContent,
    cfIgnoreDate,

    cfNtfsShift
  );

  TCompareFlags = set of TCompareFlag;

  { TCompareOption }

  TCompareOption = class
  private
    _flags: TCompareFlags;
    _stateWithoutLeft: TSyncRecState;

  public
    constructor Create(const flags: TCompareFlags);
    property flags: TCompareFlags read _flags;
    property stateWithoutLeft: TSyncRecState read _stateWithoutLeft;
  end;

  TFilterFlag = (
    ffCopyRight,
    ffCopyLeft,
    ffEqual,
    ffNotEqual,
    ffUnknown,
    ffDuplicate,
    ffSingle
  );

  TFilterFlags = set of TFilterFlag;

  { TFileSyncRec }

  TFileSyncRec = class
  protected
    _relPath: String;
    _state: TSyncRecState;
    _action: TSyncRecState;
    _option: TCompareOption;
    _leftFile: TFile;
    _rightFile: TFile;
  public
    constructor Create(const option: TCompareOption; const relPath: String);
    destructor Destroy; override;

    procedure updateState; virtual;
    function getProperAction(const expectAction: TSyncRecState): TSyncRecState; virtual;
    function getNextAction: TSyncRecState; virtual;

    function isDir: Boolean; virtual;
    function isDeletable( const leftSide: Boolean ): Boolean; virtual;

    function fileBySide( const leftSide: Boolean ): TFile;

    property relPath: String read _relPath;
    property state: TSyncRecState read _state write _state;
    property action: TSyncRecState read _action write _action;

    property leftFile: TFile read _leftFile write _leftFile;
    property rightFile: TFile read _rightFile write _rightFile;

    property option: TCompareOption read _option;
  end;

  { TDirSyncRec }

  TDirSyncRec = class(TFileSyncRec)
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
    function noDir(const side: Boolean): Boolean;
    function noDir: Boolean;
    function noFile(const side: Boolean): Boolean;
    function noFile: Boolean;
    function isEmpty(const side: Boolean): Boolean;
  end;

  { TTwoLevelTreeDirItem }

  TTwoLevelTreeDirItem = class
  private
    _dirSyncRec: TDirSyncRec;
    _files: TStringListEx;
  public
    constructor Create(const dirSyncRec: TDirSyncRec);
    destructor Destroy; override;

    procedure addFile( const filename: String; const fileSyncRec: TFileSyncRec );

    function fileCount: Integer;
    function fileSyncRec( const fileIndex: Integer ): TFileSyncRec;
    function files: TStringListEx;
    function indexOfFile( const filename: String ): Integer;

    property dirSyncRec: TDirSyncRec read _dirSyncRec;
  end;

  { TFlatDirFileList }

  TFlatDirFileList = class
  private
    _list: TStringListEx;
  private
    function findParentDirRec( const childIndex: Integer ): TDirSyncRec;
    procedure decParentDirRecChildrenCount( const childIndex: Integer; const leftSide: Boolean );
  public
    constructor Create;
    destructor Destroy; override;

    procedure addPath( const path: String; const syncRec: TFileSyncRec );
    procedure Delete( const index: Integer );
    procedure FullyDelete( const index: Integer );
    procedure clearInvisibleDirs;
    procedure Clear;

    procedure removeLeft( const index: Integer );
    procedure removeRight( const index: Integer );

    function Count: Integer;
    function path( const index: Integer ): String;
    function fileSyncRec( const index: Integer ): TFileSyncRec;

    function lastFileInCurrentDir(const fromIndex: Integer): Integer;

    procedure setNewAction( const indexes: TIntegerList; const newAction: TSyncRecState );
  end;

  { TTwoLevelTree }

  TTwoLevelTree = class
  private
    _dirs: TStringListEx;
  public
    constructor Create;
    destructor Destroy; override;

    procedure filterFlatListWithFlags( const flatList: TFlatDirFileList; const filterFlags: TFilterFlags );

    procedure addDir( const dirPath: String; const item: TTwoLevelTreeDirItem );
    procedure Clear;

    function Count: Integer;
    function indexOfDir( const dirPath: String ): Integer;
    function dirPath( const dirIndex: Integer ): String;
    function dirItem( const dirIndex: Integer ): TTwoLevelTreeDirItem;
    function fileSyncRec( const dirIndex: Integer; const fileIndex: Integer ): TFileSyncRec;
  end;

implementation

{ TCompareOption }

constructor TCompareOption.Create(const flags: TCompareFlags);
begin
  _flags:= flags;
  if cfAsymmetric in flags then
    _stateWithoutLeft:= srsDeleteRight
  else
    _stateWithoutLeft:= srsCopyToLeft;
end;

{ TFileSyncRec }

constructor TFileSyncRec.Create(const option: TCompareOption;
  const relPath: String);
begin
  _option:= option;
  _relPath := relPath;
end;

destructor TFileSyncRec.Destroy;
begin
  FreeAndNil( _leftFile );
  FreeAndNil( _rightFile );
  inherited Destroy;
end;

procedure TFileSyncRec.updateState;
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
  end;

begin
  _state := srsNotEq;
  if Assigned(_rightFile) and NOT Assigned(_leftFile) then begin
    _state:= _option.stateWithoutLeft;
  end else if NOT Assigned(_rightFile) and Assigned(_leftFile) then begin
    _state:= srsCopyToRight;
  end else begin
    compareTwoSides;
  end;
  _action := _state;
end;

function TFileSyncRec.getProperAction( const expectAction: TSyncRecState ): TSyncRecState;
begin
  Result:= expectAction;
  case expectAction of
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

function TFileSyncRec.getNextAction: TSyncRecState;
begin
  if _state = srsEqual then
    Exit( srsNoAction );

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

function TFileSyncRec.isDir: Boolean;
begin
  Result:= False;
end;

function TFileSyncRec.isDeletable( const leftSide: Boolean ): Boolean;
begin
  Result:= Assigned( self.fileBySide(leftSide) );
end;

function TFileSyncRec.fileBySide(const leftSide: Boolean): TFile;
begin
  if leftSide then
    Result:= _leftFile
  else
    Result:= _rightFile;
end;

{ TDirSyncRec }

procedure TDirSyncRec.updateState;
begin
  _state:= srsDoNothing;
  _action:= srsDoNothing;
  if NOT (cfEmptyDirs in _option.flags) then
    Exit;
  if NOT Assigned(_leftFile) and NOT Assigned(_rightFile) then
    Exit;
  if Assigned(_leftFile) and Assigned(_rightFile) then
    Exit;
  inherited updateState;
end;

function TDirSyncRec.getNextAction: TSyncRecState;
begin
  if _state = srsDoNothing then
    Result:= srsNoAction
  else
    Result:= inherited getNextAction;
end;

function TDirSyncRec.isDir: Boolean;
begin
  Result:= True;
end;

function TDirSyncRec.isDeletable( const leftSide: Boolean ): Boolean;
begin
  Result:= inherited isDeletable( leftSide );
  Result:= Result and self.isEmpty( leftSide );
end;

procedure TDirSyncRec.incDirCount(const side: Boolean);
begin
  Inc( _dirCount[side] );
end;

procedure TDirSyncRec.decDirCount(const side: Boolean);
begin
  Dec( _dirCount[side] );
end;

procedure TDirSyncRec.incFileCount(const side: Boolean);
begin
  Inc( _fileCount[side] );
end;

procedure TDirSyncRec.decFileCount(const side: Boolean);
begin
  Dec( _fileCount[side] );
end;

function TDirSyncRec.fileCount(const side: Boolean): Integer;
begin
  Result:= _fileCount[side];
end;

function TDirSyncRec.noDir(const side: Boolean): Boolean;
begin
  Result:= _dirCount[side] = 0;
end;

function TDirSyncRec.noDir: Boolean;
begin
  Result:= noDir(True) and noDir(False);
end;

function TDirSyncRec.noFile(const side: Boolean): Boolean;
begin
  Result:= _fileCount[side] = 0;
end;

function TDirSyncRec.noFile: Boolean;
begin
  Result:= noFile(True) and noFile(False);
end;

function TDirSyncRec.isEmpty(const side: Boolean): Boolean;
begin
  Result:= self.noDir(side) and self.noFile(side);
end;

{ TTwoLevelTreeDirItem }

constructor TTwoLevelTreeDirItem.Create(const dirSyncRec: TDirSyncRec);
begin
  _dirSyncRec:= dirSyncRec;
  _files:= TStringListEx.Create;
  _files.OwnsObjects:= True;
  _files.CaseSensitive := FileNameCaseSensitive;
  _files.Sorted := True;
end;

destructor TTwoLevelTreeDirItem.Destroy;
begin
  FreeAndNil( _dirSyncRec );
  FreeAndNil( _files );
end;

procedure TTwoLevelTreeDirItem.addFile( const filename: String; const fileSyncRec: TFileSyncRec );
begin
  _files.AddObject( filename, fileSyncRec );
end;

function TTwoLevelTreeDirItem.fileCount: Integer;
begin
  Result:= _files.Count;
end;

function TTwoLevelTreeDirItem.fileSyncRec(const fileIndex: Integer): TFileSyncRec;
begin
  Result:= TFileSyncRec( _files.Objects[fileIndex] );
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
  _dirs.CaseSensitive := FileNameCaseSensitive;
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
  // _dirs.Sorted := True;
end;

destructor TTwoLevelTree.Destroy;
begin
  FreeAndNil( _dirs );
end;

procedure TTwoLevelTree.filterFlatListWithFlags(
  const flatList: TFlatDirFileList;
  const filterFlags: TFilterFlags );

  function isMatching(const syncRec: TFileSyncRec): Boolean;
  begin
    if syncRec.state = srsDeleted then
      Exit(False);

    Result:=
      ((Assigned(syncRec.leftFile) <> Assigned(syncRec.rightFile)) and (ffSingle in filterFlags) or
       (Assigned(syncRec.leftFile) = Assigned(syncRec.rightFile)) and (ffDuplicate in filterFlags))
       and
       (((syncRec.state = srsCopyToLeft) or (syncRec.action = srsCopyToLeft)) and (ffCopyLeft in filterFlags) or
        ((syncRec.state = srsCopyToRight) or (syncRec.action = srsCopyToRight)) and (ffCopyRight in filterFlags) or
        (syncRec.state = srsDeleteLeft) and (ffCopyRight in filterFlags) or
        (syncRec.state = srsDeleteRight) and (ffCopyLeft in filterFlags) or
        (syncRec.state = srsEqual) and (ffEqual in filterFlags) or
        (syncRec.state = srsNotEq) and (ffNotEqual in filterFlags) or
        (syncRec.state = srsUnknown) and (ffUnknown in filterFlags));
  end;

  function isDirMatching(const syncRec: TFileSyncRec): Boolean;
  var
    dirSyncRec: TDirSyncRec absolute syncRec;
  begin
    if syncRec.state = srsDoNothing then begin
      Result:= True;
    end else if dirSyncRec.noFile and (syncRec.state=srsEqual) then begin
      Result:= False;
    end else begin
      Result:= isMatching(syncRec);
    end;
  end;

var
  dirIndex: Integer;
  fileIndex: Integer;
  rec: TFileSyncRec;
  currentDirItem: TTwoLevelTreeDirItem;
begin
  flatList.Clear;
  for dirIndex:= 0 to self.Count-1 do begin
    currentDirItem:= self.dirItem( dirIndex );
    if self.dirPath(dirIndex) <> EmptyStr then begin
      rec:= currentDirItem.dirSyncRec;
      if isDirMatching(rec) then
        flatList.addPath( IncludeTrailingPathDelimiter(self.dirPath(dirIndex)), rec );
    end;
    for fileIndex:= 0 to currentDirItem.fileCount-1 do begin
      rec:= currentDirItem.fileSyncRec( fileIndex );
      if isMatching(rec) then
        flatList.addPath( currentDirItem.files[fileIndex], rec );
    end;
  end;
  flatList.clearInvisibleDirs;
end;

procedure TTwoLevelTree.addDir(const dirPath: String; const item: TTwoLevelTreeDirItem);
begin
  _dirs.AddObject( dirPath, item );
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

function TTwoLevelTree.fileSyncRec(const dirIndex: Integer; const fileIndex: Integer): TFileSyncRec;
begin
  Result:= self.dirItem(dirIndex).fileSyncRec(fileIndex);
end;

{ TFlatDirFileList }

function TFlatDirFileList.findParentDirRec(const childIndex: Integer): TDirSyncRec;
var
  i: Integer;
  rec: TFileSyncRec;
begin
  Result:= nil;
  rec:= self.fileSyncRec( childIndex );
  if rec.isDir then
    Exit;
  for i:= childIndex-1 downto 0 do begin
    rec:= self.fileSyncRec( i );
    if rec.relPath = EmptyStr then
      break;
    if NOT rec.isDir then
      continue;
    Result:= TDirSyncRec( rec );
    Exit;
  end;
end;

procedure TFlatDirFileList.decParentDirRecChildrenCount(
  const childIndex: Integer; const leftSide: Boolean);
var
  parentDirRec: TDirSyncRec;
  childRec: TFileSyncRec;
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

constructor TFlatDirFileList.Create;
begin
  // not own Object
  _list:= TStringListEx.Create;
  _list.CaseSensitive := FileNameCaseSensitive;
end;

destructor TFlatDirFileList.Destroy;
begin
  FreeAndNil( _list );
end;

procedure TFlatDirFileList.addPath( const path: String; const syncRec: TFileSyncRec );
begin
  _list.AddObject( path, syncRec );
end;

procedure TFlatDirFileList.Delete(const index: Integer);
begin
  _list.Delete( index );
end;

procedure TFlatDirFileList.FullyDelete(const index: Integer);
var
  rec: TFileSyncRec;
begin
  rec:= self.fileSyncRec( index );
  rec.state:= srsDeleted;
  self.Delete( index );
end;

procedure TFlatDirFileList.clearInvisibleDirs;
var
  i: Integer;
  rec: TFileSyncRec;
begin
  for i:= self.Count-1 downto 0 do begin
    rec:= self.fileSyncRec( i );
    if NOT rec.isDir then
      continue;
    if rec.state <> srsDoNothing then
      continue;
    if i + 1 < self.Count then begin
      rec:= self.fileSyncRec(i+1);
      if NOT rec.isDir then
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
  rec: TFileSyncRec;
begin
  decParentDirRecChildrenCount( index, True );
  rec:= self.fileSyncRec( index );
  FreeAndNil( rec._leftFile );
end;

procedure TFlatDirFileList.removeRight(const index: Integer);
var
  rec: TFileSyncRec;
begin
  decParentDirRecChildrenCount( index, False );
  rec:= self.fileSyncRec( index );
  FreeAndNil( rec._rightFile );
end;

function TFlatDirFileList.Count: Integer;
begin
  Result:= _list.Count;
end;

function TFlatDirFileList.path(const index: Integer): String;
begin
  Result:= _list[index];
end;

function TFlatDirFileList.fileSyncRec(const index: Integer): TFileSyncRec;
begin
  Result:= TFileSyncRec( _list.Objects[index] );
end;

function TFlatDirFileList.lastFileInCurrentDir( const fromIndex: Integer ): Integer;
var
  rec: TFileSyncRec;
begin
  Result:= fromIndex;
  rec:= self.fileSyncRec(fromIndex);
  if NOT rec.isDir then
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

procedure TFlatDirFileList.setNewAction(
  const indexes: TIntegerList;
  const newAction: TSyncRecState );
var
  handled: TBooleanDynArray;

  procedure doUpdateAction(const index: Integer; const expectAction: TSyncRecState);
  var
    rec: TFileSyncRec;
  begin
    if handled[index] then
      Exit;
    handled[index]:= True;

    rec:= self.fileSyncRec(index);
    rec.action:= rec.getProperAction( expectAction );
  end;

  procedure checkAncestorsDirs(index: Integer; const cascadingAction: TSyncRecState);
  var
    rec: TFileSyncRec;
    basePath: String;
  begin
    rec:= self.fileSyncRec(index);
    if NOT (cfEmptyDirs in rec._option.flags) then
      Exit;

    basePath:= IncludeTrailingPathDelimiter(rec.relPath);

    Dec(index);
    while index >= 0 do begin
      rec := self.fileSyncRec(index);
      if rec.relPath = EmptyStr then
        break;
      if NOT PathIsInPath(basePath, rec.relPath) then
        break;
      if rec.isDir then begin
        if rec.state = srsDoNothing then
          break;
        doUpdateAction(index, cascadingAction);
      end;
      Dec(index);
    end;
  end;

  procedure uncheckDescendantsDirsAndFiles(index: Integer; const cascadingAction: TSyncRecState);
  var
    rec: TFileSyncRec;
    basePath: String;
  begin
    rec:= self.fileSyncRec(index);
    basePath:= IncludeTrailingPathDelimiter(rec.relPath);
    Inc(index);
    if NOT (cfEmptyDirs in rec._option.flags) then begin
      while index < self.Count do
      begin
        rec:= self.fileSyncRec(index);
        if rec.isDir then
          break;
        doUpdateAction(index, cascadingAction);
        Inc(index);
      end;
    end else begin
      while index < self.Count do
      begin
        rec:= self.fileSyncRec(index);
        if NOT PathIsInPath(rec.relPath, basePath) then
          break;
        doUpdateAction(index, cascadingAction);
        Inc(index);
      end;
    end;
  end;

  procedure processOneRec( const index: Integer );
  var
    rec: TFileSyncRec;
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
      srsDeleteBoth,
      srsDoNothing:
        if rec.isDir then
          uncheckDescendantsDirsAndFiles(index, rec.action);
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


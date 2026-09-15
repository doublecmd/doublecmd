unit uSyncDirsModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  DCClassesUtf8, DCDateTimeUtils,
  uFile;

type

  TSyncRecState = (
    srsUnknown,
    srsEqual,
    srsNotEq,
    srsCopyLeft,
    srsCopyRight,
    srsDeleteLeft,
    srsDeleteRight,
    srsDeleteBoth,
    srsDoNothing,

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

    function isDir: Boolean; virtual;
    function isDeletable( const leftSide: Boolean ): Boolean; virtual;

    function fileBySide( const leftSide: Boolean ): TFile;

    property relPath: String read _relPath;
    property state: TSyncRecState read _state write _state;
    property action: TSyncRecState read _action write _action;

    property leftFile: TFile read _leftFile write _leftFile;
    property rightFile: TFile read _rightFile write _rightFile;
  end;

  { TDirSyncRec }

  TDirSyncRec = class(TFileSyncRec)
  private
    _childrenCount: array [Boolean] of Integer;
  public
    procedure updateState; override;
    function isDir: Boolean; override;
    function isDeletable(const leftSide: Boolean): Boolean; override;
    procedure incChildrenCount(const side: Boolean);
    procedure decChildrenCount(const side: Boolean);
    function childrenCount(const side: Boolean): Integer;
    function isEmpty(const side: Boolean): Boolean;
    function isEmpty: Boolean;
    procedure resetEmpty;
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

  { TTwoLevelTree }

  TTwoLevelTree = class
  private
    _dirs: TStringListEx;
  public
    constructor Create;
    destructor Destroy; override;

    procedure addDir( const dirPath: String; const item: TTwoLevelTreeDirItem );
    procedure Clear;

    function Count: Integer;
    function indexOfDir( const dirPath: String ): Integer;
    function dirPath( const dirIndex: Integer ): String;
    function dirItem( const dirIndex: Integer ): TTwoLevelTreeDirItem;
    function fileSyncRec( const dirIndex: Integer; const fileIndex: Integer ): TFileSyncRec;
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
    procedure Clear;

    procedure removeLeft( const index: Integer );
    procedure removeRight( const index: Integer );

    function Count: Integer;
    function path( const index: Integer ): String;
    function fileSyncRec( const index: Integer ): TFileSyncRec;
  end;

implementation

{ TCompareOption }

constructor TCompareOption.Create(const flags: TCompareFlags);
begin
  _flags:= flags;
  if cfAsymmetric in flags then
    _stateWithoutLeft:= srsDeleteRight
  else
    _stateWithoutLeft:= srsCopyLeft;
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
var
  FileTimeDiff: Integer;
begin
  _state := srsNotEq;
  if Assigned(_rightFile) and not Assigned(_leftFile) then
    _state := _option.stateWithoutLeft
  else
  if not Assigned(_rightFile) and Assigned(_leftFile) then
    _state := srsCopyRight
  else begin
    FileTimeDiff := FileTimeCompare(_leftFile.ModificationTime, _rightFile.ModificationTime, cfNtfsShift in _option.flags);
    if ((FileTimeDiff = 0) or (cfIgnoreDate in _option.flags)) and (_leftFile.Size = _rightFile.Size) then
      _state := srsEqual
    else
    if not (cfIgnoreDate in _option.flags) then
      if FileTimeDiff > 0 then
        _state := srsCopyRight
      else
      if FileTimeDiff < 0 then
        _state := srsCopyLeft;
  end;
  if (cfAsymmetric in _option.flags) and (_state = srsCopyLeft) then
    _action := srsDoNothing
  else begin
    _action := _state;
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

function TDirSyncRec.isDir: Boolean;
begin
  Result:= True;
end;

function TDirSyncRec.isDeletable( const leftSide: Boolean ): Boolean;
begin
  Result:= inherited isDeletable( leftSide );
  Result:= Result and self.isEmpty( leftSide );
end;

procedure TDirSyncRec.incChildrenCount(const side: Boolean);
begin
  Inc( _childrenCount[side] );
end;

procedure TDirSyncRec.decChildrenCount(const side: Boolean);
begin
  Dec( _childrenCount[side] );
end;

function TDirSyncRec.childrenCount(const side: Boolean): Integer;
begin
  Result:= _childrenCount[side];
end;

function TDirSyncRec.isEmpty(const side: Boolean): Boolean;
begin
  Result:= _childrenCount[side] = 0;
end;

function TDirSyncRec.isEmpty: Boolean;
begin
  Result:= isEmpty(True) and isEmpty(False);
end;

procedure TDirSyncRec.resetEmpty;
begin
  _childrenCount[True]:= 0;
  _childrenCount[False]:= 0;
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
  _dirs.Sorted := True;
end;

destructor TTwoLevelTree.Destroy;
begin
  FreeAndNil( _dirs );
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
begin
  parentDirRec:= findParentDirRec( childIndex );
  if Assigned(parentDirRec) then
    parentDirRec.decChildrenCount( leftSide );
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

end.


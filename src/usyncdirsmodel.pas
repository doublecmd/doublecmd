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
    srsDoNothing );

  TCompareFlag = (
    coOnlySelected,
    coEmptyDir,
    coAsymmetric,
    coSubdirs,
    coByContent,
    coIgnoreDate,

    coNtfsShift
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

  TFiltFlag = (
    foCopyRight,
    foCopyLeft,
    foEqual,
    foNotEqual,
    foUnknown
  );

  TFiltFlags = set of TFiltFlag;

  { TFileSyncRec }

  TFileSyncRec = class
  protected
    _relPath: String;
    _state: TSyncRecState;
    _action: TSyncRecState;
    _option: TCompareOption;
  public
    fileL: TFile;
    fileR: TFile;
  public
    constructor Create(const option: TCompareOption; const relPath: String);
    destructor Destroy; override;
    procedure updateState; virtual;
    function isDir: Boolean; virtual;

    property relPath: String read _relPath;
    property state: TSyncRecState read _state write _state;
    property action: TSyncRecState read _action write _action;
  end;

  { TDirSyncRec }

  TDirSyncRec = class(TFileSyncRec)
  private
    _childrenCount: array [Boolean] of Integer;
  public
    procedure updateState; override;
    function isDir: Boolean; override;
    procedure incChildrenCount(const side: Boolean);
    function childrenCount(const side: Boolean): Integer;
    function isEmpty(const side: Boolean): Boolean;
    function isEmpty: Boolean;
    procedure resetEmpty;
  end;

  { TDirSyncObject }

  TDirSyncObject = class(TStringListEx)
  private
    _dirSyncRec: TDirSyncRec;
  public
    constructor Create(const dirSyncRec: TDirSyncRec);
    destructor Destroy; override;
    property dirSyncRec: TDirSyncRec read _dirSyncRec;
  end;

implementation

{ TCompareOption }

constructor TCompareOption.Create(const flags: TCompareFlags);
begin
  _flags:= flags;
  if coAsymmetric in flags then
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
  FreeAndNil(fileL);
  FreeAndNil(fileR);
  inherited Destroy;
end;

procedure TFileSyncRec.updateState;
var
  FileTimeDiff: Integer;
begin
  _state := srsNotEq;
  if Assigned(fileR) and not Assigned(fileL) then
    _state := _option.stateWithoutLeft
  else
  if not Assigned(fileR) and Assigned(fileL) then
    _state := srsCopyRight
  else begin
    FileTimeDiff := FileTimeCompare(fileL.ModificationTime, fileR.ModificationTime, coNtfsShift in _option.flags);
    if ((FileTimeDiff = 0) or (coIgnoreDate in _option.flags)) and (fileL.Size = fileR.Size) then
      _state := srsEqual
    else
    if not (coIgnoreDate in _option.flags) then
      if FileTimeDiff > 0 then
        _state := srsCopyRight
      else
      if FileTimeDiff < 0 then
        _state := srsCopyLeft;
  end;
  if (coAsymmetric in _option.flags) and (_state = srsCopyLeft) then
    _action := srsDoNothing
  else begin
    _action := _state;
  end;
end;

function TFileSyncRec.isDir: Boolean;
begin
  Result:= False;
end;

{ TDirSyncRec }

procedure TDirSyncRec.updateState;
begin
  _state:= srsDoNothing;
  _action:= srsDoNothing;
  if NOT (coEmptyDir in _option.flags) then
    Exit;
  if NOT Assigned(fileL) and NOT Assigned(fileR) then
    Exit;
  if Assigned(fileL) and Assigned(fileR) then
    Exit;
  inherited updateState;
end;

function TDirSyncRec.isDir: Boolean;
begin
  Result:= True;
end;

procedure TDirSyncRec.incChildrenCount(const side: Boolean);
begin
  Inc( _childrenCount[side] );
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

{ TDirSyncObject }

constructor TDirSyncObject.Create(const dirSyncRec: TDirSyncRec);
begin
  Inherited Create;
  _dirSyncRec:= dirSyncRec;
end;

destructor TDirSyncObject.Destroy;
begin
  FreeAndNil(_dirSyncRec);
  Inherited;
end;

end.


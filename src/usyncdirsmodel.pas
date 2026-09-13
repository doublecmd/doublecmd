unit uSyncDirsModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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

end.


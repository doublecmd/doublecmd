{
TDBZVDateTimePicker control for Lazarus
- - - - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

Last change: April 2011

This unit is part of ZVDateTimeCtrls package for Lazarus.
TDBZVDateTimePicker is data-aware version of TZVDateTimePicker control.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see COPYING.TXT.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope this control will be useful.
}
unit DBZVDateTimePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZVDateTimePicker, db, DBCtrls;

type

  { TDBZVDateTimePicker }

  TDBZVDateTimePicker = class(TCustomZVDateTimePicker)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FReadOnly: Boolean;
    FDataChanging: Boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure SetReadOnly(const AValue: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    function GetField: TField;
    procedure CheckField;
  protected
    { Protected declarations }
    procedure Change; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
    procedure EditingDone; override;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;

    property ArrowShape;
    property ShowCheckBox;
    property Checked;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property AutoSize;
    property Font;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property BorderStyle;
    property BorderSpacing;
    property Enabled;
    property Color;
    property ParentColor;
    property DateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property LeadingZeros;
    property ShowHint;
    property ParentShowHint;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
    property NullInputAllowed;
    property Kind;
    property TimeSeparator;
    property TimeFormat;
    property TimeDisplay;
    { property Time; This property should NOT be published here, it was
                           accidentally added in first release. }
    property DateMode;
    property UseDefaultSeparators;
  //events:
    property OnChange;
    property OnDropDown;
    property OnCloseUp;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave;
    property OnResize;
    property OnUTF8KeyPress;
  end;

implementation

{ TDBZVDateTimePicker }

function TDBZVDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBZVDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBZVDateTimePicker.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
  CheckField;
end;

procedure TDBZVDateTimePicker.SetDataSource(const AValue: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, AValue);
  CheckField;
end;

procedure TDBZVDateTimePicker.DataChange(Sender: TObject);
begin
  FDataChanging := True;
  try
    if Assigned(FDataLink.Field) then begin
      if FDataLink.Field.IsNull then begin
        DateTime := NullDate;
      end else begin
              // Using the SetTheDateJumpMinMax procedure, instead of property
        SetDateTimeJumpMinMax(FDataLink.Field.AsDateTime); // assignment allows
              // this control to display dates from database whose value falls
              // outside of MinDate and MaxDate interval.
              // Note that user still cannot enter such values in the control.
      end;
    end else begin
      DateTime := NullDate;
    end;

  finally
    FDataChanging := False;
  end;
end;

procedure TDBZVDateTimePicker.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly <> AValue then begin
    FReadOnly := AValue;
    CheckField;
  end;
end;

procedure TDBZVDateTimePicker.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then begin
    if DateIsNull then
      FDataLink.Field.AsVariant := Null
    else
      FDataLink.Field.AsDateTime := DateTime;
  end;
end;

procedure TDBZVDateTimePicker.ActiveChange(Sender: TObject);
begin
  CheckField;
end;

function TDBZVDateTimePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBZVDateTimePicker.CheckField;
var
  FieldOK: Boolean;
begin
  FieldOK := (FDataLink.Active) and Assigned(FDataLink.Field);

  if FieldOK then
    inherited ReadOnly := FReadOnly or (not FDataLink.CanModify)
  else begin
    inherited ReadOnly := True;
    DateTime := NullDate;
  end;
end;

procedure TDBZVDateTimePicker.Change;
begin
  if (not FDataChanging) and Assigned(FDataLink) then begin
    if FDataLink.Edit then begin
      FDataLink.Modified;
      inherited Change; // calls OnChange event handler
    end else
      FDataLink.Reset; // reverts user changes

  end;
end;

constructor TDBZVDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataChanging := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  DateTime := NullDate;
  FDataLink.OnActiveChange := @ActiveChange;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;

  CheckField;
end;

destructor TDBZVDateTimePicker.Destroy;
begin
  FDataLink.OnUpdateData := nil;
  FDataLink.OnDataChange := nil;
  FDataLink.OnActiveChange := nil;
  FreeAndNil(FDataLink);

  inherited Destroy;
end;

procedure TDBZVDateTimePicker.EditingDone;
begin
  inherited EditingDone;
  if Assigned(FDataLink) then
    try
      FDataLink.UpdateRecord;
    except
      SetFocus;
      raise;
    end;
end;

end.

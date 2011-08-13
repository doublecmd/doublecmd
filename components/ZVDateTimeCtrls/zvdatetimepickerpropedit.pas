{
ZVDateTimePickerPropEdit
- - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

Last change: April 2011

   This unit is part of ZVDateTimeCtrls package for Lazarus. It contains
component and property editors for TZVDateTimePicker control.

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
   I do hope the ZVDateTimeCtrls package will be useful.
}
unit ZVDateTimePickerPropEdit;

{$mode objfpc}{$H+}

interface
// Nothing needs to be in interface section!

implementation

uses
  Classes, SysUtils, Forms, Controls, ButtonPanel, ZVDateTimePicker,
  DBZVDateTimePicker, StdCtrls, Math, Menus, ComponentEditors, PropEdits;

type
    { TFormZVDateTimePickerEditor }

  TFormZVDateTimePickerEditor = class(TForm)
  private
    CallerZVDateTimePicker: TZVDateTimePicker;
    Prop: String;
    Modified: Boolean;

    ButtonPanel: TButtonPanel;
    ZVDateTimePickerMin: TZVDateTimePicker;
    ZVDateTimePicker1: TZVDateTimePicker;
    ZVDateTimePickerMax: TZVDateTimePicker;
    Label1: TLabel;
    LabelMin: TLabel;
    LabelMax: TLabel;
    LabelNull: TLabel;
    procedure ZVDateTimePickerMaxExit(Sender: TObject);
    procedure ZVDateTimePickerMinExit(Sender: TObject);
    procedure ZVDateTimePickersChange(Sender: TObject);
    procedure ZVDateTimePicker1Enter(Sender: TObject);
    procedure ZVDateTimePicker1Exit(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure Initialize(const Caller: TZVDateTimePicker;
                                     const PropertyName, PropertyType: String);
    procedure UpdateMinMaxBounds;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  end;

  { TZVDateTimePickerComponentEditor }

  TZVDateTimePickerComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TZVDateTimePickerDateTimePropEditor }

  TZVDateTimePickerDateTimePropEditor = class(TDateTimePropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function AllEqual: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

  { TSimpleDatePropEditor }

  TSimpleDatePropEditor = class(TDatePropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure RegPropEdits;
begin
  RegisterPropertyEditor(TypeInfo(TTime), TZVDateTimePicker, 'Time', TZVDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TZVDateTimePicker, 'Date', TZVDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TZVDateTimePicker, 'MaxDate', TZVDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TZVDateTimePicker, 'MinDate', TZVDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDBZVDateTimePicker, 'MaxDate', TSimpleDatePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDBZVDateTimePicker, 'MinDate', TSimpleDatePropEditor);
end;

{ TZVDateTimePickerComponentEditor }

procedure TZVDateTimePickerComponentEditor.ExecuteVerb(Index: Integer);
var
  F: TFormZVDateTimePickerEditor;
  DTPicker: TZVDateTimePicker;
begin
  if Index = 0 then begin
    if GetComponent is TZVDateTimePicker then begin
      F := TFormZVDateTimePickerEditor.CreateNew(nil, 0);
      try
        DTPicker := TZVDateTimePicker(GetComponent);
        if DTPicker.Kind = dtkTime then
          F.Initialize(DTPicker, '', 'TTime')
        else
          F.Initialize(DTPicker, '', '');

        if F.ShowModal = mrOK then begin
          if F.Modified then begin
            DTPicker.MinDate := TheSmallestDate;
            DTPicker.MaxDate := F.ZVDateTimePickerMax.DateTime;
            DTPicker.MinDate := F.ZVDateTimePickerMin.DateTime;

            DTPicker.DateTime := F.ZVDateTimePicker1.DateTime;
            Modified;
            GlobalDesignHook.RefreshPropertyValues;
          end;
        end;
      finally
        F.Free;
      end;
    end else
      raise Exception.Create('Unknown ZVDateTimePicker object to edit.');
  end;
end;

function TZVDateTimePickerComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := '&Date/Time Editor...';
end;

function TZVDateTimePickerComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TFormZVDateTimePickerEditor }

procedure TFormZVDateTimePickerEditor.ZVDateTimePickerMaxExit(Sender: TObject);
begin
  if ZVDateTimePickerMin.Date > ZVDateTimePickerMax.Date then
    ZVDateTimePickerMin.Date := ZVDateTimePickerMax.Date;

  UpdateMinMaxBounds;
end;

procedure TFormZVDateTimePickerEditor.ZVDateTimePickerMinExit(Sender: TObject);
begin
  if ZVDateTimePickerMax.Date < ZVDateTimePickerMin.Date then
    ZVDateTimePickerMax.Date := ZVDateTimePickerMin.Date;

  UpdateMinMaxBounds;
end;

procedure TFormZVDateTimePickerEditor.ZVDateTimePickersChange(Sender: TObject);
begin
  Modified := True;
end;

procedure TFormZVDateTimePickerEditor.ZVDateTimePicker1Enter(Sender: TObject);
begin
  if ZVDateTimePicker1.NullInputAllowed then
    LabelNull.Show;
end;

procedure TFormZVDateTimePickerEditor.ZVDateTimePicker1Exit(Sender: TObject);
begin
  LabelNull.Hide;
end;

procedure TFormZVDateTimePickerEditor.FormActivate(Sender: TObject);
var
  B: Boolean;
begin
  OnActivate := nil;
  B := False;
  if Prop = 'MAXDATE' then ZVDateTimePickerMax.SetFocus
  else if Prop = 'MINDATE' then ZVDateTimePickerMin.SetFocus
  else begin
    ZVDateTimePicker1.SetFocus;
    B := ZVDateTimePicker1.NullInputAllowed;
  end;
  LabelNull.Visible := B;
  LabelNull.BringToFront;
end;

procedure TFormZVDateTimePickerEditor.Initialize(const Caller: TZVDateTimePicker;
                                    const PropertyName, PropertyType: String);
var
  I: Integer;
  DTP: array[1..3] of TZVDateTimePicker;

  L, T, W, H: Integer;
  R: TRect;
begin
  if Assigned(Caller) then begin
    CallerZVDateTimePicker := Caller;
    Prop := UpperCase(PropertyName);

    Modified := False;
    ZVDateTimePicker1.Kind := dtkDateTime;
    if UpperCase(PropertyType) = 'TTIME' then
      ZVDateTimePicker1.SelectTime
    else
      ZVDateTimePicker1.SelectDate;

    Label1.Caption := 'Date / Time:';
    LabelMax.Caption := 'MaxDate:';
    LabelMin.Caption := 'MinDate:';
    LabelNull.Caption := '(Press N to set to NULL)';

    ZVDateTimePickerMin.DateTime := CallerZVDateTimePicker.MinDate;
    ZVDateTimePickerMax.DateTime := CallerZVDateTimePicker.MaxDate;
    ZVDateTimePicker1.MinDate := CallerZVDateTimePicker.MinDate;
    ZVDateTimePicker1.MaxDate := CallerZVDateTimePicker.MaxDate;
    ZVDateTimePicker1.DateTime := CallerZVDateTimePicker.DateTime;

    DTP[1] := ZVDateTimePickerMin;
    DTP[2] := ZVDateTimePickerMax;
    DTP[3] := ZVDateTimePicker1;
    for I := 1 to 3 do begin
      DTP[I].NullInputAllowed := I = 3;
      DTP[I].CenturyFrom := CallerZVDateTimePicker.CenturyFrom;
      DTP[I].DateDisplayOrder := CallerZVDateTimePicker.DateDisplayOrder;
      DTP[I].LeadingZeros := CallerZVDateTimePicker.LeadingZeros;
      DTP[I].DateSeparator := CallerZVDateTimePicker.DateSeparator;
      DTP[I].TrailingSeparator := CallerZVDateTimePicker.TrailingSeparator;
    end;
    ZVDateTimePicker1.TextForNullDate := CallerZVDateTimePicker.TextForNullDate;
    ZVDateTimePicker1.TimeSeparator := CallerZVDateTimePicker.TimeSeparator;
    ZVDateTimePicker1.TimeDisplay := tdHMSMs;
    ZVDateTimePicker1.TimeFormat := CallerZVDateTimePicker.TimeFormat;

    ZVDateTimePickerMax.AnchorParallel(akTop, 20, Self);
    ZVDateTimePickerMax.AnchorParallel(akRight, 20, Self);
    LabelMax.AnchorVerticalCenterTo(ZVDateTimePickerMax);
    LabelMax.AnchorToNeighbour(akRight, 2, ZVDateTimePickerMax);
    ZVDateTimePickerMin.AnchorParallel(akTop, 20, Self);
    ZVDateTimePickerMin.AnchorToNeighbour(akRight, 20, LabelMax);
    LabelMin.AnchorToNeighbour(akRight, 2, ZVDateTimePickerMin);
    LabelMin.AnchorVerticalCenterTo(ZVDateTimePickerMin);
    ZVDateTimePicker1.AnchorParallel(akLeft, 0, ZVDateTimePickerMin);
    ZVDateTimePicker1.AnchorToNeighbour(akTop, 20, ZVDateTimePickerMin);
    Label1.AnchorToNeighbour(akRight, 2, ZVDateTimePicker1);
    Label1.AnchorVerticalCenterTo(ZVDateTimePicker1);
    LabelNull.AnchorToNeighbour(akTop, 2, ZVDateTimePicker1);
    LabelNull.AnchorHorizontalCenterTo(ZVDateTimePicker1);

    ButtonPanel.Spacing := 10;
    ButtonPanel.BorderSpacing.Around := 10;

    W := Max(Label1.Width, LabelMin.Width);

    W := ZVDateTimePickerMax.Width + ZVDateTimePickerMin.Width
                                   + LabelMax.Width + W + 80;

    H := 2 * ZVDateTimePickerMax.Height + LabelNull.Height + ButtonPanel.Height + 58;

    R := Screen.MonitorFromWindow(CallerZVDateTimePicker.Handle).WorkareaRect;
    L := (R.Left + R.Right - W) div 2;
    T := (R.Top + R.Bottom - H) div 2;

    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;

    SetBounds(L, T, W, H);
  end;

end;

procedure TFormZVDateTimePickerEditor.UpdateMinMaxBounds;
begin
  ZVDateTimePicker1.MinDate := TheSmallestDate;
  ZVDateTimePicker1.MaxDate := ZVDateTimePickerMax.Date;
  ZVDateTimePicker1.MinDate := ZVDateTimePickerMin.Date;
end;

constructor TFormZVDateTimePickerEditor.CreateNew(AOwner: TComponent;
  Num: Integer);
var
  I: Integer;
begin
  inherited CreateNew(AOwner, Num);

  Hide;
  if Font.Size > 10 then
    Font.Size := 10;

  SetBounds(-8000, -8000, 4, 5);
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];
  Caption := 'ZVDateTimePicker Editor';

  ZVDateTimePickerMax := TZVDateTimePicker.Create(Self);
  ZVDateTimePickerMin := TZVDateTimePicker.Create(Self);
  ZVDateTimePicker1 := TZVDateTimePicker.Create(Self);
  Label1 := TLabel.Create(Self);
  LabelMin := TLabel.Create(Self);
  LabelMax := TLabel.Create(Self);
  LabelNull := TLabel.Create(Self);

  ButtonPanel := TButtonPanel.Create(Self);
  ButtonPanel.ShowButtons := [pbOK, pbCancel];
  ButtonPanel.OKButton.GlyphShowMode := gsmAlways;
  ButtonPanel.CancelButton.GlyphShowMode := gsmAlways;
  ButtonPanel.ShowBevel := False;

  ZVDateTimePickerMax.Parent := Self;
  ZVDateTimePickerMin.Parent := Self;
  ZVDateTimePicker1.Parent := Self;
  Label1.Parent := Self;
  LabelMin.Parent := Self;
  LabelMax.Parent := Self;
  LabelNull.Parent := Self;
  ButtonPanel.Parent := Self;

  ButtonPanel.TabOrder := 0;
  ZVDateTimePickerMin.TabOrder := 1;
  ZVDateTimePickerMax.TabOrder := 2;
  ZVDateTimePicker1.TabOrder := 3;

  for I := 0 to ControlCount - 1 do begin
    Controls[I].Anchors := [];
    Controls[I].AutoSize := True;
  end;

  ZVDateTimePickerMax.OnExit := @ZVDateTimePickerMaxExit;
  ZVDateTimePickerMin.OnExit := @ZVDateTimePickerMinExit;
  ZVDateTimePicker1.OnExit := @ZVDateTimePicker1Exit;
  ZVDateTimePicker1.OnEnter := @ZVDateTimePicker1Enter;
  ZVDateTimePickerMin.OnChange := @ZVDateTimePickersChange;
  ZVDateTimePickerMax.OnChange := @ZVDateTimePickersChange;
  ZVDateTimePicker1.OnChange := @ZVDateTimePickersChange;

  OnActivate := @FormActivate;
end;

destructor TFormZVDateTimePickerEditor.Destroy;
begin
  OnActivate := nil;
  OnShow := nil;

  ZVDateTimePicker1.OnChange := nil;
  ZVDateTimePickerMax.OnChange := nil;
  ZVDateTimePickerMin.OnChange := nil;
  ZVDateTimePicker1.OnEnter := nil;
  ZVDateTimePicker1.OnExit := nil;
  ZVDateTimePickerMin.OnExit := nil;
  ZVDateTimePickerMax.OnExit := nil;

  ButtonPanel.Free;
  LabelNull.Free;
  LabelMax.Free;
  LabelMin.Free;
  Label1.Free;
  ZVDateTimePicker1.Free;
  ZVDateTimePickerMin.Free;
  ZVDateTimePickerMax.Free;

  inherited Destroy;
end;

{ TZVDateTimePickerDateTimePropEditor }

function TZVDateTimePickerDateTimePropEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TZVDateTimePickerDateTimePropEditor.AllEqual: Boolean;
var
  DT: TDateTime;
  N: Integer;
begin
  Result := True;
  N := PropCount;
  if N > 1 then begin
    DT := TDateTime(GetFloatValue);
    repeat
      Dec(N);
      Result := EqualDateTime(DT, TDateTime(GetFloatValueAt(N)));
    until not(Result and (N > 1));
  end;
end;

function TZVDateTimePickerDateTimePropEditor.GetValue: string;
var
  DT: TDateTime;
  S: String;
begin
  DT := TDateTime(GetFloatValue);
  if IsNullDate(DT) then
    Result := 'NULL'
  else begin
    S := UpperCase(GetPropType^.Name);
    if S = 'TDATE' then
      Result := DateToStr(DT)
    else if S = 'TTIME' then
      Result := TimeToStr(DT)
    else
      Result := DateTimeToStr(DT);
  end;
end;

procedure TZVDateTimePickerDateTimePropEditor.SetValue(const Value: string);
var
  S: String;
begin
  S := Trim(Value);
  if (S > '') and (UpCase(S[1]) <> 'N') then begin
    S := UpperCase(GetPropType^.Name);
    if S = 'TDATE' then
      SetFloatValue(StrToDate(Value))
    else if S = 'TTIME' then
      SetFloatValue(StrToTime(Value))
    else
      inherited SetValue(Value);
  end else
    SetFloatValue(NullDate);
end;

procedure TZVDateTimePickerDateTimePropEditor.Edit;
var
  F: TFormZVDateTimePickerEditor;
  I: Integer;
  DT: TZVDateTimePicker;
begin
  for I := 0 to PropCount - 1 do
    if not (GetComponent(I) is TZVDateTimePicker) then
      Exit;

  F := TFormZVDateTimePickerEditor.CreateNew(nil, 0);
  try
    F.Initialize(TZVDateTimePicker(GetComponent(0)), GetName, GetPropType^.Name);
    if F.ShowModal = mrOK then begin
      if F.Modified then begin
        for I := 0 to PropCount - 1 do begin
          DT := TZVDateTimePicker(GetComponent(I));
          DT.MinDate := TheSmallestDate;
          DT.MaxDate := F.ZVDateTimePickerMax.Date;
          DT.MinDate := F.ZVDateTimePickerMin.Date;

          DT.DateTime := F.ZVDateTimePicker1.DateTime;
        end;

        Modified;
        GlobalDesignHook.RefreshPropertyValues;
      end;
    end;
  finally
    F.Free;
  end;
end;

{ TSimpleDatePropEditor }

function TSimpleDatePropEditor.GetValue: string;
begin
  Result := DateToStr(GetFloatValue);
end;

procedure TSimpleDatePropEditor.SetValue(const Value: string);
var
  S: String;
begin
  S := Trim(Value);
  if (S > '') and (UpCase(S[1]) <> 'N') then
    inherited SetValue(S);
end;

initialization
  RegPropEdits;
  RegisterComponentEditor(TZVDateTimePicker, TZVDateTimePickerComponentEditor);

end.

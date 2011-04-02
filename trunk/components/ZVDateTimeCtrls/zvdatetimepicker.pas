{
TZVDateTimePicker control for Lazarus
- - - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

Last change: May 2010

   This unit is part of ZVDateTimeCtrls package for Lazarus.

   Delphi's Visual Component Library (VCL) has a control named TDateTimePicker,
which I find very useful for editing dates. Lazarus Component Library (LCL),
however, does not have this control, because VCL wraps native Windows control
and it seems that such control does not exist on other platforms. Given that
LCL is designed to be platform independent, it could not use native Win control.
   Instead, for editing dates LCL has a control named TDateEdit, but I prefer
the VCL's TDateTimePicker.
   Therefore, I tried to create a custom control which would resemble VCL's
TDateTimePicker as much as possible, but not to rely on native Windows control.

   This TZVDateTimePicker control does not use native Win control. It has been
written and initially tested on Windows XP with win widgetset, but then tested
and adjusted on Ubuntu Linux 9.10 with gtk2 widgetset.
   Additionaly, tests were made on Qt widget set too, on both Windows and Linux.

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
unit ZVDateTimePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, LCLType, Graphics, Math, StdCtrls,
  Buttons, ExtCtrls, Forms, Calendar, ComCtrls, Types, LCLVersion;

const
  { We will deal with the NullDate value the special way. It will be
    especially useful for dealing with null values from database. }
  NullDate = TDateTime(Math.MaxDouble);

  TheBiggestDate = TDateTime(2958465.0); // 31. dec. 9999.
//{$IFDEF WINDOWS}
// TCalendar does not accept smaller dates then 14. sep. 1752 on Windows
// platform (see TCustomCalendar.SetDateTime).
// In Delphi help it is documented that Windows controls act weird with dates
// older than 24. sep. 1752. Actually, TCalendar control has problems to show
// dates before 1. okt. 1752. (try putting one calendar on the form, run the
// application and see what september 1752. looks like). So, this will be the
// down limit:
  TheSmallestDate = TDateTime(-53780.0); // 1. okt. 1752.
//{$ELSE} -- I just commented this out. Let's behave uniformely as much as
// possible -- I won't allow dates before 1. okt. 1752. on any platform (who
// cares about those).
//  TheSmallestDate = TDateTime(-693593.0); // 1. jan. 0001.
//{$ENDIF}

{$IF (lcl_major > 0) OR (lcl_minor > 9) OR ((lcl_minor = 9) AND (lcl_release >= 29))}
  {$DEFINE LCL_0_9_29_OR_AFTER}
{$ELSE}
  {$IFDEF LCLQt}
    {$DEFINE QT_BEFORE_0_9_29}
  {$ENDIF}
{$IFEND}

type
  TYMD = record
    Year, Month, Day: Word;
  end;
  THMSMs = record
    Hour, Minute, Second, MiliSec: Word;
  end;

  TDateDisplayOrder = (ddoDMY, ddoMDY, ddoYMD, ddoTryDefault);

  TDateTextPart = (dtpDay, dtpMonth, dtpYear, dtpTime);

  TTimeDisplay = (tdHM,   // hour and minute
                  tdHMS,  // hour, minute and second
                  tdHMSMs // hour, minute, second and milisecond
                  );

  TTimeFormat = (tf12, // 12 hours format, with am/pm string
                 tf24  // 24 hours format
                 );

  TDateTimeKind = (dtkDate, dtkTime, dtkDateTime); // Determines if we should
                                                 // display date, time or both.

  TTimeTextPart = (ttpHour, ttpMinute, ttpSecond, ttpMiliSec, ttpAMPM);

  { TCustomDateTimeEditor }

  TCustomDateTimeEditor = class(TCustomControl)
  private
    FCenturyFrom, FEffectiveCenturyFrom: Word;
    FDateDisplayOrder: TDateDisplayOrder;
    FKind: TDateTimeKind;
    FLeadingZeros: Boolean;
    FNullInputAllowed: Boolean;
    FDateTime: TDateTime;
    FConfirmedDate: TDateTime;
    FDateSeparator: UTF8String;
    FReadOnly: Boolean;
    FMaxDate, FMinDate: TDate;
    FTextForNullDate: UTF8String;
    FTimeSeparator: UTF8String;
    FTimeDisplay: TTimeDisplay;
    FTimeFormat: TTimeFormat;
    FTrailingSeparator: Boolean;
    FUseDefaultSeparators: Boolean;
    FUserChangedText: Boolean;
    FTextPart: array[1..3] of UTF8String;
    FTimeText: array[TTimeTextPart] of UTF8String;
    FOnChange: TNotifyEvent;
    FStoredLockCount: Integer;
    FDigitWidth: Integer;
    FTextHeight: Integer;
    FSeparatorWidth: Integer;
    FSepNoSpaceWidth: Integer;
    FTimeSeparatorWidth: Integer;
    FSelectedTextPart: 1..8;
    FRecalculatingTextSizesNeeded: Boolean;
    FJumpMinMax: Boolean;
    FAMPMWidth: Integer;
    FDateWidth, FTimeWidth: Integer;

    function AreSeparatorsStored: Boolean;
    function GetHour: Word;
    function GetMiliSec: Word;
    function GetMinute: Word;
    function GetSecond: Word;
    function GetDate: TDate;
    function GetTime: TTime;
    procedure RecalculateTextSizesIfNeeded;
    function GetDay: Word;
    function GetMonth: Word;
    function GetYear: Word;
    function GetHMSMs(const NowIfNull: Boolean = False): THMSMs;
    function GetYYYYMMDD(const TodayIfNull: Boolean = False): TYMD;
    procedure SetCenturyFrom(const AValue: Word);
    procedure SetKind(const AValue: TDateTimeKind);
    procedure SetHour(const AValue: Word);
    procedure SetLeadingZeros(const AValue: Boolean);
    procedure SetMiliSec(const AValue: Word);
    procedure SetMinute(const AValue: Word);
    procedure SetSecond(const AValue: Word);
    procedure SetDate(const AValue: TDate);
    procedure SetDateTime(const AValue: TDateTime);
    procedure SetDateDisplayOrder(const AValue: TDateDisplayOrder);
    procedure SetDateSeparator(const AValue: UTF8String);
    procedure SetSeparators(const DateSep, TimeSep: UTF8String);
    procedure SetDay(const AValue: Word);
    procedure SetMaxDate(const AValue: TDate);
    procedure SetMinDate(const AValue: TDate);
    procedure SetMonth(const AValue: Word);
    procedure SetTextForNullDate(const AValue: UTF8String);
    procedure SetTime(const AValue: TTime);
    procedure SetTimeSeparator(const AValue: UTF8String);
    procedure SetTimeDisplay(const AValue: TTimeDisplay);
    procedure SetTimeFormat(const AValue: TTimeFormat);
    procedure SetTrailingSeparator(const AValue: Boolean);
    procedure SetUseDefaultSeparators(const AValue: Boolean);
    procedure SetYear(const AValue: Word);
    procedure SetReadOnly(Value: Boolean);
    procedure SetYYYYMMDD(const AValue: TYMD);
    procedure SetHMSMs(const AValue: THMSMs);
    procedure UpdateIfUserChangedText;
    function GetSelectedText: UTF8String;
    procedure AdjustEffectiveCenturyFrom;
    procedure SelectDateTextPart(const DateTextPart: TDateTextPart);
    procedure SelectTimeTextPart(const TimeTextPart: TTimeTextPart);
  protected
    procedure ConfirmChanges;
    procedure UndoChanges;
    procedure SetDateTimeJumpMinMax(const AValue: TDateTime);
    procedure ChangeDateTimeInternally(const AValue: TDateTime);
    function GetEffectiveDateDisplayOrder: TDateDisplayOrder; virtual;
    function GetCurrentDateTextPart: TDateTextPart;
    function GetCurrentTimeTextPart: TTimeTextPart;
    procedure FontChanged(Sender: TObject); override;
    function GetTextOrigin: TPoint;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure SelectTextPartUnderMouse(XMouse: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure UpdateDate; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure IncreaseCurrentTextPart;
    procedure DecreaseCurrentTextPart;
    procedure IncreaseMonth;
    procedure IncreaseYear;
    procedure IncreaseDay;
    procedure DecreaseMonth;
    procedure DecreaseYear;
    procedure DecreaseDay;
    procedure IncreaseHour;
    procedure IncreaseMinute;
    procedure IncreaseSecond;
    procedure IncreaseMiliSec;
    procedure DecreaseHour;
    procedure DecreaseMinute;
    procedure DecreaseSecond;
    procedure DecreaseMiliSec;
    procedure ChangeAMPM;

    procedure Change; virtual;

    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property AutoSize default True;
    property TabStop default True;
    property BorderStyle default bsSingle;
    property ParentColor default False;

    property DateDisplayOrder: TDateDisplayOrder
             read FDateDisplayOrder write SetDateDisplayOrder default ddoTryDefault;
    property MaxDate: TDate read FMaxDate write SetMaxDate;
    property MinDate: TDate read FMinDate write SetMinDate;
    property DateSeparator: UTF8String
             read FDateSeparator write SetDateSeparator stored AreSeparatorsStored;
    property UseDefaultSeparators: Boolean
             read FUseDefaultSeparators write SetUseDefaultSeparators;
    property TrailingSeparator: Boolean
             read FTrailingSeparator write SetTrailingSeparator default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TextForNullDate: UTF8String
             read FTextForNullDate write SetTextForNullDate;
    property LeadingZeros: Boolean
             read FLeadingZeros write SetLeadingZeros default True;
    property NullInputAllowed: Boolean
             read FNullInputAllowed write FNullInputAllowed default True;
    property Kind: TDateTimeKind read FKind write SetKind;
    property TimeSeparator: UTF8String
             read FTimeSeparator write SetTimeSeparator stored AreSeparatorsStored;
    property TimeFormat: TTimeFormat
             read FTimeFormat write SetTimeFormat default tf24;
    property TimeDisplay: TTimeDisplay
             read FTimeDisplay write SetTimeDisplay default tdHMS;

    property Date: TDate read GetDate write SetDate;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property Time: TTime read GetTime write SetTime;
    property CenturyFrom: Word
             read FCenturyFrom write SetCenturyFrom default 1941;
    property Day: Word read GetDay write SetDay stored False;
    property Month: Word read GetMonth write SetMonth stored False;
    property Year: Word read GetYear write SetYear stored False;
    property Hour: Word read GetHour write SetHour stored False;
    property Minute: Word read GetMinute write SetMinute stored False;
    property Second: Word read GetSecond write SetSecond stored False;
    property MiliSec: Word read GetMiliSec write SetMiliSec stored False;
  public
    constructor Create(AOwner: TComponent); override;

    function DateIsNull: Boolean;
    procedure SelectDay;
    procedure SelectMonth;
    procedure SelectYear;
    procedure SelectHour;
    procedure SelectMinute;
    procedure SelectSecond;
    procedure SelectMiliSec;
    procedure SelectAMPM;
    procedure SelectDate;
    procedure SelectTime;

    procedure Paint; override;
    procedure EditingDone; override;
  published
    //
  end;

  {TDateTimeEditor}

  TDateTimeEditor = class(TCustomDateTimeEditor)
  public
    property DateTime;
  published
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property ReadOnly;
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
    property ShowHint;
    property ParentShowHint;
    property Date;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
    property LeadingZeros;
    property NullInputAllowed;
    property Kind;
    property TimeSeparator;
    property TimeFormat;
    property TimeDisplay;
    property Time;
    property UseDefaultSeparators;
// events:
    property OnChange;
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

  TArrowShape = (asClassicSmaller, asClassicLarger, asModernSmaller,
                                               asModernLarger, asYetAnotherShape);
  TDTDateMode = (dmComboBox, dmUpDown, dmNone);

  { TCustomZVDateTimePicker }

  TCustomZVDateTimePicker = class(TCustomControl)
  private
    FArrowShape: TArrowShape;
    FDateMode: TDTDateMode;
    FDateTimeEditor: TCustomDateTimeEditor;
    FCheckBox: TCheckBox;
    FUpDown: TCustomUpDown;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;

    FPanelForArrowButton: TPanel;
    FArrowButton: TSpeedButton;
    FCalendarForm: TCustomForm;
    FCal: TCalendar;
    FShape: TShape;
    FRememberedCalendarFormOrigin: TPoint;
    FDoNotArrangeControls: Boolean;
    FClosingCalendarForm: Boolean;
    FCallFromDateTimeEditorEnter: Boolean;
    FCallFromDateTimeEditorExit: Boolean;
    FCloseCalendarOnChange: Boolean;
    FForceShowCalendar: Boolean;

    function AreSeparatorsStored: Boolean;
    function GetCenturyFrom: Word;
    function GetChecked: Boolean;
    function GetDateDisplayOrder: TDateDisplayOrder;
    function GetKind: TDateTimeKind;
    function GetLeadingZeros: Boolean;
    function GetNullInputAllowed: Boolean;
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetDateSeparator: UTF8String;
    function GetMaxDate: TDate;
    function GetMinDate: TDate;
    function GetReadOnly: Boolean;
    function GetShowCheckBox: Boolean;
    function GetTextForNullDate: UTF8String;
    function GetTime: TTime;
    function GetTimeSeparator: UTF8String;
    function GetTimeDisplay: TTimeDisplay;
    function GetTimeFormat: TTimeFormat;
    function GetTrailingSeparator: Boolean;
    function GetUseDefaultSeparators: Boolean;
    procedure SetArrowShape(const AValue: TArrowShape);
    procedure SetCenturyFrom(const AValue: Word);
    procedure SetChecked(const AValue: Boolean);
    procedure CheckIfDateEditorIsEnabled;
    procedure SetDateDisplayOrder(const AValue: TDateDisplayOrder);
    procedure SetDateMode(const AValue: TDTDateMode);
    procedure SetKind(const AValue: TDateTimeKind);
    procedure SetLeadingZeros(const AValue: Boolean);
    procedure SetNullInputAllowed(const AValue: Boolean);
    procedure SetDate(const AValue: TDate);
    procedure SetDateTime(const AValue: TDateTime);
    procedure SetDateSeparator(const AValue: UTF8String);
    procedure SetMaxDate(const AValue: TDate);
    procedure SetMinDate(const AValue: TDate);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetShowCheckBox(const AValue: Boolean);
    procedure SetTextForNullDate(const AValue: UTF8String);
    procedure SetTime(const AValue: TTime);
    procedure SetTimeSeparator(const AValue: UTF8String);
    procedure SetTimeDisplay(const AValue: TTimeDisplay);
    procedure SetTimeFormat(const AValue: TTimeFormat);
    procedure SetTrailingSeparator(const AValue: Boolean);
    procedure SetUseDefaultSeparators(const AValue: Boolean);
    procedure DestroyTheCalendar;
    procedure AdjustCalendarFormSize;
    procedure AdjustCalendarFormScreenPosition;
    procedure CreateCalendarForm;
    procedure DestroyCalendarForm;
    procedure CloseCalendarForm(AndSetTheDate: Boolean = False);
    procedure DropDownCalendarForm;
    procedure UpdateShowArrowButton(NewDateMode: TDTDateMode;
                                                 NewKind: TDateTimeKind);
    procedure DestroyUpDown;
    procedure DestroyArrowBtn;
    procedure DateTimeEditorChange(Sender: TObject);
    procedure DateTimeEditorEnter(Sender: TObject);
    procedure DateTimeEditorExit(Sender: TObject);
    procedure DateTimeEditorEditingDone(Sender: TObject);
    procedure DateTimeEditorKeyDown(Sender: TObject; var Key: Word;
                                                     Shift: TShiftState);
    procedure DateTimeEditorKeyUp(Sender: TObject; var Key: Word;
                                                   Shift: TShiftState);
    procedure DateTimeEditorKeyPress(Sender: TObject; var Key: char);
    procedure DateTimeEditorUTF8KeyPress(Sender: TObject;
                                                 var UTF8Key: TUTF8Char);
    procedure DateTimeEditorMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure DateTimeEditorMouseMove(Sender: TObject; Shift: TShiftState;
                                                           X, Y: Integer);
    procedure DateTimeEditorMouseUp(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure DateTimeEditorClick(Sender: TObject);
    procedure DateTimeEditorDblClick(Sender: TObject);
    procedure DateTimeEditorTripleClick(Sender: TObject);
    procedure DateTimeEditorQuadClick(Sender: TObject);

    procedure ArrowMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);

    procedure CheckBoxChange(Sender: TObject);

    procedure CalendarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CalendarResize(Sender: TObject);
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure CalendarChange(Sender: TObject);

    procedure CalendarFormDeactivate(Sender: TObject);
    procedure CalendarFormShow(Sender: TObject);
    procedure CalendarFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CalendarFormDestroy(Sender: TObject);

  protected
    // In older Lazarus versions, GetControlClassDefaultSize is of type TPoint,
    // since 0.9.29, svn rev. 25204, it's TSize.
    {$IFDEF LCL_0_9_29_OR_AFTER}
    class function GetControlClassDefaultSize: TSize; override;
    {$ELSE}
    class function GetControlClassDefaultSize: TPoint; override;
    {$ENDIF}

    procedure SetHint(const Value: TTranslateString); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CreateWnd; override;
    procedure SetDateTimeJumpMinMax(const AValue: TDateTime);
    procedure ArrangeCtrls; virtual;
    procedure Change; virtual;
    procedure DoDropDown; virtual;
    procedure DoCloseUp; virtual;
    procedure DrawArrowButtonGlyph; virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;

    procedure DoEnter; override;
    procedure DoExit; override;

    property BorderStyle default bsSingle;
    property AutoSize default True;
    property TabStop default True;
    property ParentColor default False;
    property CenturyFrom: Word
             read GetCenturyFrom write SetCenturyFrom;
    property DateDisplayOrder: TDateDisplayOrder
             read GetDateDisplayOrder write SetDateDisplayOrder default ddoTryDefault;
    property MaxDate: TDate
             read GetMaxDate write SetMaxDate;
    property MinDate: TDate
             read GetMinDate write SetMinDate;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property TrailingSeparator: Boolean
             read GetTrailingSeparator write SetTrailingSeparator;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property LeadingZeros: Boolean read GetLeadingZeros write SetLeadingZeros;
    property TextForNullDate: UTF8String
             read GetTextForNullDate write SetTextForNullDate;
    property NullInputAllowed: Boolean
             read GetNullInputAllowed write SetNullInputAllowed default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property ShowCheckBox: Boolean
             read GetShowCheckBox write SetShowCheckBox default False;
    property Checked: Boolean read GetChecked write SetChecked default True;
    property ArrowShape: TArrowShape
        read FArrowShape write SetArrowShape default asModernSmaller;
    property Kind: TDateTimeKind
             read GetKind write SetKind;
    property DateSeparator: UTF8String
             read GetDateSeparator write SetDateSeparator stored AreSeparatorsStored;
    property TimeSeparator: UTF8String
             read GetTimeSeparator write SetTimeSeparator stored AreSeparatorsStored;
    property UseDefaultSeparators: Boolean
             read GetUseDefaultSeparators write SetUseDefaultSeparators;
    property TimeFormat: TTimeFormat read GetTimeFormat write SetTimeFormat;
    property TimeDisplay: TTimeDisplay read GetTimeDisplay write SetTimeDisplay;
    property Time: TTime read GetTime write SetTime;
    property Date: TDate read GetDate write SetDate;
    property DateMode: TDTDateMode read FDateMode write SetDateMode;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DateIsNull: Boolean;
    procedure SelectDate;
    procedure SelectTime;
  published
    //
  end;

  {TZVDateTimePicker}

  TZVDateTimePicker = class(TCustomZVDateTimePicker)
  public
    property DateTime;
  published
    property ArrowShape;
    property ShowCheckBox;
    property Checked;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property ReadOnly;
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
    property DateMode;
    property Date;
    property Time;
    property UseDefaultSeparators;
// events:
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

function EqualDateTime(const A, B: TDateTime): Boolean;
function IsNullDate(DT: TDateTime): Boolean;

implementation

function NumberOfDaysInMonth(const Month, Year: Word): Word;
begin
  Result := 0;
  if Month in [1..12] then
    Result := MonthDays[IsLeapYear(Year), Month];
end;

function EqualDateTime(const A, B: TDateTime): Boolean;
begin
  if IsNullDate(A) then
    Result := IsNullDate(B)
  else
    Result := (not IsNullDate(B)) and (A = B);
end;

function IsNullDate(DT: TDateTime): Boolean;
begin
  Result := IsNan(DT) or IsInfinite(DT) or
            (DT > SysUtils.MaxDateTime) or (DT < SysUtils.MinDateTime);
end;

procedure Exchange(var W1, W2: Word);
var
  W: Word;
begin
  W := W1;
  W1 := W2;
  W2 := W;
end;

{ TCustomDateTimeEditor }

procedure TCustomDateTimeEditor.SetDateTime(const AValue: TDateTime);
begin
  if not EqualDateTime(AValue, FDateTime) then begin
    if IsNullDate(AValue) then
      FDateTime := NullDate
    else
      FDateTime := AValue;

    Change;
  end;
  UpdateDate;
end;

procedure TCustomDateTimeEditor.SetDateDisplayOrder(const AValue: TDateDisplayOrder);
var
  PreviousEffectiveDDO: TDateDisplayOrder;
begin
  if FDateDisplayOrder <> AValue then begin
    PreviousEffectiveDDO := GetEffectiveDateDisplayOrder;
    FDateDisplayOrder := AValue;
    if PreviousEffectiveDDO <> GetEffectiveDateDisplayOrder then
      UpdateDate;
  end;
end;

procedure TCustomDateTimeEditor.SetDateSeparator(const AValue: UTF8String);
begin
  SetSeparators(AValue, FTimeSeparator);
end;

procedure TCustomDateTimeEditor.SetSeparators(const DateSep,
  TimeSep: UTF8String);
var
  SeparatorsChanged: Boolean;
begin
  FUseDefaultSeparators := False;
  SeparatorsChanged := False;
  if FDateSeparator <> DateSep then begin
    FDateSeparator := DateSep;
    SeparatorsChanged := True;
  end;
  if FTimeSeparator <> TimeSep then begin
    FTimeSeparator := TimeSep;
    SeparatorsChanged := True;
  end;
  if SeparatorsChanged then begin
    FRecalculatingTextSizesNeeded := True;
    Invalidate;
  end;
end;

procedure TCustomDateTimeEditor.RecalculateTextSizesIfNeeded;
var
  C: Char;
  N: Integer;
  S: UTF8String;
begin
  if FRecalculatingTextSizesNeeded then begin
    FRecalculatingTextSizesNeeded := False;

    FDigitWidth := 0;
    for C := '0' to '9' do begin
      N := Canvas.GetTextWidth(C);
      if N > FDigitWidth then
        FDigitWidth := N;
    end;

    if FKind in [dtkDate, dtkDateTime] then begin

      FSeparatorWidth := Canvas.GetTextWidth(FDateSeparator);
      FDateWidth := 8 * FDigitWidth + 2 * FSeparatorWidth;

      if FTrailingSeparator then begin
        FSepNoSpaceWidth := Canvas.GetTextWidth(TrimRight(FDateSeparator));
        Inc(FDateWidth, FSepNoSpaceWidth);
      end else
        FSepNoSpaceWidth := 0;

      S := FDateSeparator;
    end else begin
      if FSelectedTextPart < 4 then
        FSelectedTextPart := 4;

      S := '';
      FSeparatorWidth := 0;
      FSepNoSpaceWidth := 0;
      FDateWidth := 0;
    end;

    FAMPMWidth := 0;
    if FKind in [dtkTime, dtkDateTime] then begin
      S := S + FTimeSeparator;

      FTimeSeparatorWidth := Canvas.GetTextWidth(FTimeSeparator);

      case FTimeDisplay of
        tdHM:
          FTimeWidth := 4 * FDigitWidth + FTimeSeparatorWidth;
        tdHMS:
          FTimeWidth := 6 * FDigitWidth + 2 * FTimeSeparatorWidth;
        tdHMSMs:
          FTimeWidth := 9 * FDigitWidth + 3 * FTimeSeparatorWidth;
      end;

      if FTimeFormat = tf12 then begin
        S := S + 'APM';
        FAMPMWidth := Max(Canvas.TextWidth('AM'), Canvas.TextWidth('PM'));
        FTimeWidth := FTimeWidth + FDigitWidth + FAMPMWidth;
      end;

      if Ord(FTimeDisplay) + 5 < FSelectedTextPart then
        if (FSelectedTextPart < 8) or (FTimeFormat = tf24) then
          FSelectedTextPart := 4;

    end else begin
      if FSelectedTextPart > 3 then
        FSelectedTextPart := 1;

      FTimeSeparatorWidth := 0;
      FTimeWidth := 0;
    end;

    FTextHeight := Canvas.GetTextHeight('0123456789' + S);

  end;
end;

function TCustomDateTimeEditor.GetTime: TTime;
begin
  if DateIsNull then
    Result := NullDate
  else
    Result := Abs(Frac(FDateTime));
end;

function TCustomDateTimeEditor.AreSeparatorsStored: Boolean;
begin
  Result := not FUseDefaultSeparators;
end;

function TCustomDateTimeEditor.GetHour: Word;
begin
  Result := GetHMSMs.Hour;
end;

function TCustomDateTimeEditor.GetMiliSec: Word;
begin
  Result := GetHMSMs.MiliSec;
end;

function TCustomDateTimeEditor.GetMinute: Word;
begin
  Result := GetHMSMs.Minute;
end;

function TCustomDateTimeEditor.GetSecond: Word;
begin
  Result := GetHMSMs.Second;
end;

function TCustomDateTimeEditor.GetDate: TDate;
begin
  if DateIsNull then
    Result := NullDate
  else
    Result := Int(FDateTime);
end;

function TCustomDateTimeEditor.GetDay: Word;
begin
  Result := GetYYYYMMDD.Day;
end;

function TCustomDateTimeEditor.GetMonth: Word;
begin
  Result := GetYYYYMMDD.Month;
end;

function TCustomDateTimeEditor.GetYear: Word;
begin
  Result := GetYYYYMMDD.Year;
end;

function TCustomDateTimeEditor.GetHMSMs(const NowIfNull:Boolean):THMSMs;
begin
  if DateIsNull then begin
    if NowIfNull then
      DecodeTime(SysUtils.Time, Result.Hour, Result.Minute, Result.Second, Result.MiliSec)
    else
      with Result do begin
        Hour := 0;
        Minute := 0;
        Second := 0;
        MiliSec := 0;
      end;
  end else
    DecodeTime(FDateTime, Result.Hour, Result.Minute, Result.Second, Result.MiliSec);
end;

function TCustomDateTimeEditor.GetYYYYMMDD(const TodayIfNull: Boolean): TYMD;
begin
  if DateIsNull then begin
    if TodayIfNull then
      DecodeDate(SysUtils.Date, Result.Year, Result.Month, Result.Day)
    else
      with Result do begin
        Day := 0;
        Month := 0;
        Year := 0;
      end;
  end else
    DecodeDate(FDateTime, Result.Year, Result.Month, Result.Day);
end;

procedure TCustomDateTimeEditor.SetCenturyFrom(const AValue: Word);
begin
  if FCenturyFrom = AValue then Exit;

  FCenturyFrom := AValue;
  AdjustEffectiveCenturyFrom;
end;

procedure TCustomDateTimeEditor.SetKind(const AValue: TDateTimeKind);
begin
  if FKind = AValue then Exit;

  FKind := AValue;
  FRecalculatingTextSizesNeeded := True;

  UpdateDate;
end;

procedure TCustomDateTimeEditor.SetHour(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectHour;

  HMSMs := GetHMSMs(True);
  HMSMs.Hour := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.SetLeadingZeros(const AValue: Boolean);
begin
  if FLeadingZeros = AValue then Exit;

  FLeadingZeros := AValue;
  UpdateDate;
end;

procedure TCustomDateTimeEditor.SetMiliSec(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectMiliSec;

  HMSMs := GetHMSMs(True);
  HMSMs.MiliSec := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.SetMinute(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectMinute;

  HMSMs := GetHMSMs(True);
  HMSMs.Minute := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.SetSecond(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectSecond;

  HMSMs := GetHMSMs(True);
  HMSMs.Second := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.SetDate(const AValue: TDate);
begin
  if IsNullDate(AValue) then
    DateTime := NullDate
  else if DateIsNull then
    DateTime := Int(AValue)
  else
    DateTime := ComposeDateTime(AValue, FDateTime);
end;

procedure TCustomDateTimeEditor.SetDay(const AValue: Word);
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  YMD.Day := AValue;
  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.SetMaxDate(const AValue: TDate);
begin
  if not IsNullDate(AValue) then begin

    if AValue > TheBiggestDate then
      FMaxDate := TheBiggestDate
    else if AValue <= FMinDate then
      FMaxDate := FMinDate
    else
      FMaxDate := Int(AValue);

    if not DateIsNull then
      if FMaxDate < GetDate then
        SetDate(FMaxDate);

    AdjustEffectiveCenturyFrom;
  end;
end;

procedure TCustomDateTimeEditor.SetMinDate(const AValue: TDate);
begin
  if not IsNullDate(AValue) then begin

    if AValue < TheSmallestDate then
      FMinDate := TheSmallestDate
    else if AValue >= FMaxDate then
      FMinDate := FMaxDate
    else
      FMinDate := Int(AValue);

    if not DateIsNull then
      if FMinDate > GetDate then
        SetDate(FMinDate);

    AdjustEffectiveCenturyFrom;
  end;
end;

procedure TCustomDateTimeEditor.SetMonth(const AValue: Word);
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  YMD.Month := AValue;

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.SetTextForNullDate(const AValue: UTF8String);
begin
  if FTextForNullDate = AValue then Exit;

  FTextForNullDate := AValue;
  if DateIsNull then
    Invalidate;
end;

procedure TCustomDateTimeEditor.SetTime(const AValue: TTime);
begin
  if IsNullDate(AValue) then
    DateTime := NullDate
  else if DateIsNull then
    DateTime := ComposeDateTime(Max(Min(SysUtils.Date, MaxDate), MinDate), AValue)
  else
    DateTime := ComposeDateTime(FDateTime, AValue);
end;

procedure TCustomDateTimeEditor.SetTimeSeparator(const AValue: UTF8String);
begin
  SetSeparators(FDateSeparator, AValue);
end;

procedure TCustomDateTimeEditor.SetTimeDisplay(const AValue: TTimeDisplay);
begin
  if FTimeDisplay = AValue then Exit;

  FTimeDisplay:=AValue;
  FRecalculatingTextSizesNeeded := True;

  UpdateDate;
end;

procedure TCustomDateTimeEditor.SetTimeFormat(const AValue: TTimeFormat);
begin
  if FTimeFormat <> AValue then begin
    FTimeFormat := AValue;
    FRecalculatingTextSizesNeeded := True;

    UpdateDate;
  end;
end;

procedure TCustomDateTimeEditor.SetTrailingSeparator(const AValue: Boolean);
begin
  if FTrailingSeparator = AValue then Exit;

  FTrailingSeparator := AValue;
  FRecalculatingTextSizesNeeded := True;
  UpdateIfUserChangedText;
  Invalidate;
end;

procedure TCustomDateTimeEditor.SetUseDefaultSeparators(const AValue: Boolean);
begin
  if FUseDefaultSeparators <> AValue then begin
    if AValue then begin
      SetSeparators(SysUtils.DateSeparator, SysUtils.TimeSeparator);
           // Note that here, in SetSeparators procedure,
           // the field FUseDefaultSeparators is set to False.
    end;
    // Therefore, the next line must NOT be moved above.
    FUseDefaultSeparators := AValue;
  end;
end;

procedure TCustomDateTimeEditor.SetYear(const AValue: Word);
var
  YMD: TYMD;
begin
  SelectYear;

  YMD := GetYYYYMMDD(True);
  YMD.Year := AValue;
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.SetYYYYMMDD(const AValue: TYMD);
var
  D: TDateTime;
begin
  if TryEncodeDate(AValue.Year, AValue.Month, AValue.Day, D) then
    SetDate(D)
  else
    UpdateDate;
end;

procedure TCustomDateTimeEditor.SetHMSMs(const AValue: THMSMs);
var
  T: TDateTime;
begin
  if TryEncodeTime(AValue.Hour, AValue.Minute,
                                  AValue.Second, AValue.MiliSec, T) then begin
    SetTime(T);

  end else
    UpdateDate;
end;

{ GetEffectiveDateDisplayOrder function
 ----------------------------------
  If date display order ddoTryDefault is set, then we will decide which
  display order to use according to ShortDateFormat global variable. The
  function tries to achieve that by searching through short date format string,
  to see which letter comes first -- d, m or y. When it finds any of these
  characters, it assumes that date order should be d-m-y, m-d-y, or y-m-d
  respectively. If the search through ShortDateFormat is unsuccessful by any
  chance, we try the same with LongDateFormat global variable. If we don't
  succeed again, we'll assume y-m-d order.  }
function TCustomDateTimeEditor.GetEffectiveDateDisplayOrder: TDateDisplayOrder;
var
  S: String;
  I: Integer;
begin
  if FDateDisplayOrder = ddoTryDefault then begin
    S := ShortDateFormat;
    Result := ddoTryDefault;

    repeat

      for I := 1 to Length(S) do begin
        case upCase(S[I]) of
          'D': begin
                 Result := ddoDMY;
                 Break;
               end;
          'M': begin
                 Result := ddoMDY;
                 Break;
               end;
          'Y': begin
                 Result := ddoYMD;
                 Break;
               end;
        end;
      end;

      if Result = ddoTryDefault then begin
        S := LongDateFormat; { We couldn't decide with ShortDateFormat, let's
                                             try with LongDateFormat now. }
        Result := ddoYMD; { -- But now we must set something to be default. This
                ensures that the repeat loop breaks next time. If we don't find
                anything in LongDateFormat, we'll leave with y-m-d order. }
      end else
        Break;

    until False;

  end else
    Result := FDateDisplayOrder;
end;

procedure TCustomDateTimeEditor.UpdateIfUserChangedText;
var
  W: Word;
  S: UTF8String;
begin
  if FUserChangedText then begin
    Inc(FStoredLockCount);
    try
      FUserChangedText := False;
      S := Trim(GetSelectedText);
      if FSelectedTextPart = 8 then begin
        S := UTF8UpperCase(UTF8Copy(S, 1, 1));
        W := GetHour;
        if S = 'A' then begin
          if W >= 12 then
            Dec(W, 12);
        end else begin
          if W < 12 then
            Inc(W, 12);
        end;
        SetHour(W);
        FSelectedTextPart := 8;
      end else begin

        W := StrToInt(S);
        case GetCurrentDateTextPart of
          dtpYear:
            begin
              if Length(S) <= 2 then begin
          // If user entered the year in two digit format (or even only one
          // digit), we will set the year according to the CenturyFrom property
          // (We actually use FEffectiveCenturyFrom field, which is adjusted to
          //  take care of MinDate and MaxDate besides CenturyFrom properties).
                if W >= (FEffectiveCenturyFrom mod 100) then
                  W := W + 100 * (FEffectiveCenturyFrom div 100)
                else
                  W := W + 100 * (FEffectiveCenturyFrom div 100 + 1);

              end;
              SetYear(W);
            end;

          dtpDay:
            SetDay(W);

          dtpMonth:
            SetMonth(W);
        else
          case GetCurrentTimeTextPart of
            ttpHour:
              begin
                if (FTimeFormat = tf12) then begin
                  if GetHour < 12 then begin
                    if W = 12 then
                      SetHour(0)
                    else
                      SetHour(W);
                  end else begin
                    if W = 12 then
                      SetHour(W)
                    else
                      SetHour(W + 12);
                  end;
                end else
                  SetHour(W);
              end;
            ttpMinute:
              SetMinute(W);
            ttpSecond:
              SetSecond(W);
            ttpMiliSec:
              SetMiliSec(W);
          end;
        end;

      end;
    finally
      Dec(FStoredLockCount);
    end;
  end;
end;

function TCustomDateTimeEditor.GetSelectedText: UTF8String;
begin
  if FSelectedTextPart <= 3 then
    Result := FTextPart[FSelectedTextPart]
  else
    Result := FTimeText[TTimeTextPart(FSelectedTextPart - 4)];
end;

procedure TCustomDateTimeEditor.AdjustEffectiveCenturyFrom;
var
  Y1, Y2, M, D: Word;
begin
  DecodeDate(FMinDate, Y1, M, D);

  if Y1 > FCenturyFrom then
    FEffectiveCenturyFrom := Y1 // If we use CenturyFrom which is set to value
         // below MinDate's year, then when user enters two digit year, the
         // DateTime would automatically be set to MinDate value, even though
         // we perhaps allow same two-digit year in following centuries. It
         // would be less user friendly.
         // This is therefore better.

  else begin
    DecodeDate(FMaxDate, Y2, M, D);

    if Y2 < 100 then
      Y2 := 0
    else
      Dec(Y2, 99); // -- We should not use CenturyFrom if it is set to value
       // greater then MaxDate's year minus 100 years.
       // For example:
       // if CenturyFrom = 1941 and MaxDate = 31.12.2025, then if user enters
       // Year 33, we could not set the year to 2033 anyway, because of MaxDate
       // limit. Note that if we just leave CenturyFrom to effectively remain as
       // is, then in case of our example the DateTime would be automatically
       // reduced to MaxDate value. Setting the year to 1933 is rather expected
       // behaviour, so our internal field FEffectiveCenturyFrom should be 1926.

    // Therefore:
    if Y2 < FCenturyFrom then
      FEffectiveCenturyFrom := Max(Y1, Y2)
    else
      FEffectiveCenturyFrom := FCenturyFrom; // -- FCenturyFrom has passed all
                   // our tests, so we'll really use it without any correction.
  end;
end;

procedure TCustomDateTimeEditor.FontChanged(Sender: TObject);
begin
  FRecalculatingTextSizesNeeded := True;
  inherited FontChanged(Sender);
end;

function TCustomDateTimeEditor.GetTextOrigin: TPoint;
begin
  Result.x := BorderSpacing.InnerBorder + BorderWidth;
  Result.y := Result.x;
end;

procedure TCustomDateTimeEditor.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then begin
    if Value then begin
      ConfirmChanges;
      UpdateDate;
    end;

    FReadOnly := Value;
  end;
end;

procedure TCustomDateTimeEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  M, K, N: Integer;
begin
  Inc(FStoredLockCount);
  try
    inherited KeyDown(Key, Shift); // calls OnKeyDown event

    case Key of
      VK_LEFT, VK_RIGHT:
        begin
          UpdateIfUserChangedText;
          if FKind in [dtkDate, dtkDateTime] then
            M := 1
          else
            M := 4;

          if FKind in [dtkTime, dtkDateTime] then begin
            K := Ord(FTimeDisplay) + 5;
            if FTimeFormat = tf12 then
              N := 8
            else
              N := K;
          end else begin
            N := 3;
            K := 3;
          end;

          if Key = VK_LEFT then begin
            if FSelectedTextPart = M then
              FSelectedTextPart := N
            else if (FSelectedTextPart = N) and (K < N) then
              FSelectedTextPart := K
            else
              Dec(FSelectedTextPart);
          end else begin
            if FSelectedTextPart = N then
              FSelectedTextPart := M
            else if (FSelectedTextPart = K) and (K < N) then
              FSelectedTextPart := N
            else
              Inc(FSelectedTextPart);
          end;

          Invalidate;
        end;
      VK_UP:
        begin
          UpdateIfUserChangedText;
          if not FReadOnly then
            IncreaseCurrentTextPart;
        end;
      VK_DOWN:
        begin
          UpdateIfUserChangedText;
          if not FReadOnly then
            DecreaseCurrentTextPart;
        end;
      VK_RETURN:
        if not FReadOnly then begin
          ConfirmChanges;
          EditingDone;
        end;
      VK_ESCAPE:
        if not FReadOnly then begin
          UndoChanges;
          EditingDone;
        end;
      VK_N:
        if (not FReadOnly) and FNullInputAllowed then begin
          if FUserChangedText or (not DateIsNull) then begin
            SetDateTime(NullDate);
            //Change;
          end else
            SetDateTime(NullDate);
        end;
    end;
  finally
    Dec(FStoredLockCount);
  end;
end;

procedure TCustomDateTimeEditor.KeyPress(var Key: char);
var
  S: String;
  DTP: TDateTextPart;
  TTP: TTimeTextPart;
  N, L: Integer;
  YMD: TYMD;
  HMSMs: THMSMs;
  D, T: TDateTime;
  Finished: Boolean;
begin
  Inc(FStoredLockCount);
  try
    inherited KeyPress(Key);

    if (not ReadOnly) then begin
      Finished := False;

      if FSelectedTextPart = 8 then begin
        case upCase(Key) of
          'A': S := 'AM';
          'P': S := 'PM';
        else
          Finished := True;
        end;

      end else if Key in ['0'..'9'] then begin
        TTP := ttpAMPM;
        DTP := GetCurrentDateTextPart;

        if DTP = dtpYear then
          N := 4
        else if DTP = dtpTime then begin
          TTP := GetCurrentTimeTextPart;
          if TTP = ttpMiliSec then
            N := 3
          else
            N := 2;
        end else
          N := 2;

        S := Trim(GetSelectedText);
        if FUserChangedText and (UTF8Length(S) < N) then begin
          S := S + Key;

          if (not FLeadingZeros) and (FSelectedTextPart <= 4) then
            while (UTF8Length(S) > 1) and (UTF8Copy(S, 1, 1) = '0') do
              UTF8Delete(S, 1, 1);

        end else begin
          S := Key;
        end;

        if (UTF8Length(S) >= N) then begin

          L := StrToInt(S);
          if DTP <> dtpTime then begin
            YMD := GetYYYYMMDD(True);
            case DTP of
              dtpDay: YMD.Day := L;
              dtpMonth: YMD.Month := L;
              dtpYear: YMD.Year := L;
            end;
            if not TryEncodeDate(YMD.Year, YMD.Month, YMD.Day, D) then begin
              D := MinDate - 1;
            end;
            if (D < MinDate) or (D > MaxDate) then begin
              if N = 4 then begin
                UpdateDate;
                //Change;
                Finished := True;
              end else
                S := Key;
            end;
          end else begin
            if (TTP = ttpHour) and (FTimeFormat = tf12) then begin
              if not (L in [1..12]) then
                S := Key;

            end else begin

              HMSMs := GetHMSMs(True);
              case TTP of
                ttpHour: HMSMs.Hour := L;
                ttpMinute: HMSMs.Minute := L;
                ttpSecond: HMSMs.Second := L;
                ttpMiliSec: HMSMs.MiliSec := L;
              end;
              if not TryEncodeTime(HMSMs.Hour, HMSMs.Minute, HMSMs.Second,
                                           HMSMs.MiliSec, T) then
                S := Key;

            end;
          end;

        end;
      end else
        Finished := True;

      if (not Finished) and (GetSelectedText <> S) then begin
        if (not FUserChangedText) and DateIsNull then
          if FSelectedTextPart <= 3 then
            DateTime := SysUtils.Date
          else
            DateTime := SysUtils.Now;

        if FSelectedTextPart <= 3 then
          FTextPart[FSelectedTextPart] := S
        else
          FTimeText[TTimeTextPart(FSelectedTextPart - 4)] := S;

        FUserChangedText := True;
        //Change;
        Invalidate;
      end;

    end;

  finally
    Dec(FStoredLockCount);
  end;
end;

procedure TCustomDateTimeEditor.SelectTextPartUnderMouse(XMouse: Integer);
var
  M, NX: Integer;
  InTime: Boolean;
begin
  UpdateIfUserChangedText;
  if CanFocus then
    SetFocus;

  if Focused then begin
// Calculating mouse position inside text
//       in order to select date part under mouse cursor.
    FSelectedTextPart := 8;
    NX := XMouse - GetTextOrigin.x;

    if FKind = dtkDateTime then begin
      if NX >= FDateWidth + FDigitWidth then begin
        InTime := True;
        NX := NX - FDateWidth - 2 * FDigitWidth;
      end else
        InTime := False;
    end else
      InTime := FKind = dtkTime;

    if InTime then begin
      if (FTimeFormat = tf24) or
            (NX < FTimeWidth - FAMPMWidth - FDigitWidth div 2) then begin
        M := 2 * FDigitWidth + FTimeSeparatorWidth div 2;
        if M > NX then
          FSelectedTextPart := 4
        else begin
          if FTimeDisplay = tdHM then
            FSelectedTextPart := 5
          else begin
            M := M + FTimeSeparatorWidth + 2 * FDigitWidth;
            if M > NX then
              FSelectedTextPart := 5
            else begin
              if FTimeDisplay = tdHMS then
                FSelectedTextPart := 6
              else begin
                M := M + FTimeSeparatorWidth + 2 * FDigitWidth;
                if M > NX then
                  FSelectedTextPart := 6
                else
                  FSelectedTextPart := 7;
              end;
            end;
          end;
        end;
      end;


    end else  begin
      M := 2 * FDigitWidth;
      if GetEffectiveDateDisplayOrder = ddoYMD then
        M := 2 * M;
      Inc(M, FSeparatorWidth div 2);

      if M > NX then begin
        FSelectedTextPart := 1;
      end else begin
        M := M + FSeparatorWidth + 2 * FDigitWidth;
        if M > NX then begin
          FSelectedTextPart := 2;
        end else begin
          FSelectedTextPart := 3
        end;
      end;
    end;
    Invalidate;
//-------------------------------------------------------
  end;
end;

procedure TCustomDateTimeEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  SelectTextPartUnderMouse(X);
  inherited;
end;

function TCustomDateTimeEditor.DoMouseWheel(Shift: TShiftState; WheelDelta:
  Integer; MousePos: TPoint): Boolean;
begin
  {$IFDEF QT_BEFORE_0_9_29}
  if not Enabled then Exit; // On Qt the control receives
                            // MouseWheel event even when disabled!!!
  // Željko fixed this in 0.9.29, rev. 24348
  {$ENDIF}

  SelectTextPartUnderMouse(MousePos.x);
  if not FReadOnly then begin
    if WheelDelta < 0 then
      DecreaseCurrentTextPart
    else
      IncreaseCurrentTextPart;

    Result := True;
  end;
end;

procedure TCustomDateTimeEditor.UpdateDate;
var
  W: Array[1..3] of Word;
  WT: Array[TTimeTextPart] of Word;
  YearPos, I: Integer;
  TTP, TTPEnd: TTimeTextPart;
begin
  FUserChangedText := False;

  if not (DateIsNull or FJumpMinMax) then begin
    if Int(FDateTime) > FMaxDate then
      FDateTime := ComposeDateTime(FMaxDate, FDateTime);

    if FDateTime < FMinDate then
      FDateTime := ComposeDateTime(FMinDate, FDateTime);
  end;

  if FKind in [dtkTime, dtkDateTime] then begin
    if DateIsNull then begin
      FTimeText[ttpHour] := '99';
      FTimeText[ttpMinute] := '99';

      FTimeText[ttpMiliSec] := '';
      if FTimeDisplay >= tdHMS then begin
        FTimeText[ttpSecond] := '99';
        if FTimeDisplay >= tdHMSMs then
          FTimeText[ttpMiliSec] := '999';
      end else
        FTimeText[ttpSecond] := '';

      if FTimeFormat = tf12 then
        FTimeText[ttpAMPM] := 'XX'
      else
        FTimeText[ttpAMPM] := '';

    end else begin
      case FTimeDisplay of
        tdHMSMs: TTPEnd := ttpMiliSec;
        tdHMS: TTPEnd := ttpSecond;
      else
        TTPEnd := ttpMinute;
      end;

      DecodeTime(FDateTime, WT[ttpHour], WT[ttpMinute], WT[ttpSecond], WT[ttpMiliSec]);

      if FTimeFormat = tf12 then begin
        if WT[ttpHour] < 12 then begin
          FTimeText[ttpAMPM] := 'AM';
          if WT[ttpHour] = 0 then
            WT[ttpHour] := 12;
        end else begin
          FTimeText[ttpAMPM] := 'PM';
          if WT[ttpHour] > 12 then
            Dec(WT[ttpHour], 12);
        end;
      end else
        FTimeText[ttpAMPM] := '';

      if FLeadingZeros then
        FTimeText[ttpHour] := RightStr('0' + IntToStr(WT[ttpHour]), 2)
      else
        FTimeText[ttpHour] := IntToStr(WT[ttpHour]);

      for TTP := ttpMinute to ttpMiliSec do begin
        if TTP <= TTPEnd then begin
          if TTP = ttpMiliSec then
            FTimeText[TTP] := RightStr('00' + IntToStr(WT[TTP]), 3)
          else
            FTimeText[TTP] := RightStr('0' + IntToStr(WT[TTP]), 2);
        end else
          FTimeText[TTP] := '';
      end;

    end;
  end else
    for TTP := Low(TTimeTextPart) to High(TTimeTextPart) do
      FTimeText[TTP] := '';

  if FKind in [dtkDate, dtkDateTime] then begin
    if DateIsNull then begin
      if GetEffectiveDateDisplayOrder = ddoYMD then begin
        FTextPart[1] := '0000';
        FTextPart[3] := '00';
      end else begin
        FTextPart[1] := '00';
        FTextPart[3] := '0000';
      end;
      FTextPart[2] := '00';

    end else begin
      DecodeDate(FDateTime, W[3], W[2], W[1]);
      YearPos := 3;
      case GetEffectiveDateDisplayOrder of
        ddoMDY:
          Exchange(W[1], W[2]);

        ddoYMD:
          begin
            Exchange(W[1], W[3]);
            YearPos := 1;
          end;
      end;

      for I := Low(FTextPart) to High(FTextPart) do begin
        if I = YearPos then
          FTextPart[I] := RightStr('000' + IntToStr(W[I]), 4)
        else if FLeadingZeros then
          FTextPart[I] := RightStr('0' + IntToStr(W[I]), 2)
        else
          FTextPart[I] := IntToStr(W[I]);

      end;
    end;

  end else
    for I := Low(FTextPart) to High(FTextPart) do
      FTextPart[I] := '';

  if FStoredLockCount = 0 then
    ConfirmChanges;

  Invalidate;
end;

procedure TCustomDateTimeEditor.SelectDay;
begin
  SelectDateTextPart(dtpDay);
end;

procedure TCustomDateTimeEditor.SelectMonth;
begin
  SelectDateTextPart(dtpMonth);
end;

procedure TCustomDateTimeEditor.SelectYear;
begin
  SelectDateTextPart(dtpYear);
end;

procedure TCustomDateTimeEditor.SelectHour;
begin
  SelectTimeTextPart(ttpHour);
end;

procedure TCustomDateTimeEditor.SelectMinute;
begin
  SelectTimeTextPart(ttpMinute);
end;

procedure TCustomDateTimeEditor.SelectSecond;
begin
  SelectTimeTextPart(ttpSecond);
end;

procedure TCustomDateTimeEditor.SelectMiliSec;
begin
  SelectTimeTextPart(ttpMiliSec);
end;

procedure TCustomDateTimeEditor.SelectAMPM;
begin
  SelectTimeTextPart(ttpAMPM);
end;

procedure TCustomDateTimeEditor.SelectDate;
begin
  if FSelectedTextPart > 3 then
    SelectDay;
end;

procedure TCustomDateTimeEditor.SelectTime;
begin
  if FSelectedTextPart < 4 then
    SelectHour;
end;

procedure TCustomDateTimeEditor.Paint;
var
  I, M, N, K: Integer;
  DD: Array[1..8] of Integer;
  R: TRect;

  SelectStep: 0..8;
  TextStyle: TTextStyle;
begin
  if FRecalculatingTextSizesNeeded then begin
    if AutoSize then begin
      InvalidatePreferredSize;
      AdjustSize;
    end;

    RecalculateTextSizesIfNeeded;
  end;

  TextStyle := Canvas.TextStyle;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  R.TopLeft := GetTextOrigin;

  M := 2 * R.Top + FTextHeight;
  if ClientHeight > M then begin
    M := (ClientHeight - M) div 2;
    Inc(R.Top, M);
  end;

  R.Bottom := R.Top + FTextHeight;

  TextStyle.Layout := tlCenter;
  TextStyle.Wordbreak := False;
  TextStyle.Opaque := False;
  if DateIsNull and (FTextForNullDate > '')
                       and (not Focused) then begin

    R.Right := R.Left + FDateWidth + FTimeWidth;

    if Kind = dtkDateTime then
      R.Right := R.Right + 2 * FDigitWidth;

    if R.Right - R.Left > Canvas.GetTextWidth(FTextForNullDate) then
      TextStyle.Alignment := taCenter
    else
      TextStyle.Alignment := taLeftJustify;

    if Enabled then
      Canvas.Font.Color := Font.Color
    else
      Canvas.Font.Color := clGrayText;

    Canvas.TextRect(R, R.Left, R.Top, FTextForNullDate, TextStyle);

  end else begin
    TextStyle.Alignment := taRightJustify;

    SelectStep := 0;
    if Enabled then begin
      Canvas.Font.Color := Font.Color;
      if Focused then
        SelectStep := FSelectedTextPart;
    end else begin
      Canvas.Font.Color := clGrayText;
    end;

    if FKind in [dtkDate, dtkDateTime] then begin
      DD[2] := 2 * FDigitWidth;
      if GetEffectiveDateDisplayOrder = ddoYMD then begin
        DD[1] := 4 * FDigitWidth;
        DD[3] := 2 * FDigitWidth;
      end else begin
        DD[1] := 2 * FDigitWidth;
        DD[3] := 4 * FDigitWidth;
      end;
      M := 1;
    end else begin
      M := 4;
      //for I := 1 to 3 do DD[I] := 0;
    end;

    if FKind in [dtkTime, dtkDateTime] then begin
      DD[4] := 2 * FDigitWidth;
      DD[5] := 2 * FDigitWidth;

      if FTimeDisplay = tdHMSMs then begin
        DD[7] := 3 * FDigitWidth;
        DD[6] := 2 * FDigitWidth;
        K := 7;
      end else begin
        DD[7] := 0;
        if FTimeDisplay = tdHM then begin
          DD[6] := 0;
          K := 5;
        end else begin
          DD[6] := 2 * FDigitWidth;
          K := 6;
        end;
      end;

      if FTimeFormat = tf12 then begin
        N := 8;
        DD[8] := FAMPMWidth;
      end else begin
        DD[8] := 0;
        N := K;
      end;

    end else begin
      N := 3;
      K := 3;
    end;

    for I := M to N do begin
      if DD[I] <> 0 then begin
        if SelectStep = I then begin
          TextStyle.Opaque := True;
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
        end;

        R.Right := R.Left + DD[I];
        if I <= 3 then
          Canvas.TextRect(R, R.Left, R.Top, FTextPart[I], TextStyle)
        else
          Canvas.TextRect(R, R.Left, R.Top, FTimeText[TTimeTextPart(I - 4)], TextStyle);

        R.Left := R.Right;

        if SelectStep = I then begin
          TextStyle.Opaque := False;
          Canvas.Brush.Color := Color;
          Canvas.Font.Color := Self.Font.Color;
        end;

        if I < 3 then begin
          R.Right := R.Left + FSeparatorWidth;
          Canvas.TextRect(R, R.Left, R.Top, FDateSeparator, TextStyle);
        end else if I > 3 then begin
          if I = K then begin
            R.Right := R.Left + FDigitWidth;
          end else if I < K then begin
            R.Right := R.Left + FTimeSeparatorWidth;
            Canvas.TextRect(R, R.Left, R.Top, FTimeSeparator, TextStyle);
          end;
        end else begin
          if FTrailingSeparator then begin
            R.Right := R.Left + FSepNoSpaceWidth;
            Canvas.TextRect(R, R.Left, R.Top,
                                      TrimRight(FDateSeparator), TextStyle);
          end;
          if FKind = dtkDateTime then
            R.Right := R.Right + 2 * FDigitWidth;

        end;
        R.Left := R.Right;
      end;
    end;

  end;
  inherited Paint;
end;

function TCustomDateTimeEditor.DateIsNull: Boolean;
begin
  Result := IsNullDate(FDateTime);
end;

function TCustomDateTimeEditor.GetCurrentDateTextPart: TDateTextPart;
begin
  if FSelectedTextPart > 3 then
    Result := dtpTime
  else begin
    case FSelectedTextPart of
      1: Result := dtpDay;
      2: Result := dtpMonth;
      3: Result := dtpYear;
    end;

    case GetEffectiveDateDisplayOrder of
      ddoMDY: if Result = dtpDay then Result := dtpMonth
              else if Result = dtpMonth then Result := dtpDay;
      ddoYMD: if Result = dtpDay then Result := dtpYear
              else if Result = dtpYear then Result := dtpDay;
    end;

  end;
end;

function TCustomDateTimeEditor.GetCurrentTimeTextPart: TTimeTextPart;
begin
  if FSelectedTextPart > 4 then
    Result := TTimeTextPart(FSelectedTextPart - 4)
  else
    Result := ttpHour;
end;

procedure TCustomDateTimeEditor.SelectDateTextPart(const DateTextPart: TDateTextPart);
begin
  if FKind in [dtkDate, dtkDateTime] then begin

    case DateTextPart of
      dtpDay: //SelectDay;
        begin
          case GetEffectiveDateDisplayOrder of
            ddoDMY: FSelectedTextPart := 1;
            ddoMDY: FSelectedTextPart := 2;
            ddoYMD: FSelectedTextPart := 3;
          end;
        end;
      dtpMonth: //SelectMonth;
        begin
          if GetEffectiveDateDisplayOrder = ddoMDY then
            FSelectedTextPart := 1
          else
            FSelectedTextPart := 2;
        end;
      dtpYear: //SelectYear;
        begin
          if GetEffectiveDateDisplayOrder = ddoYMD then
            FSelectedTextPart := 1
          else
            FSelectedTextPart := 3;
        end;
    end;

    Invalidate;
  end;
end;

procedure TCustomDateTimeEditor.SelectTimeTextPart(const TimeTextPart:
  TTimeTextPart);
var
  B: Boolean;
begin
  if FKind in [dtkTime, dtkDateTime] then begin

    if TimeTextPart = ttpAMPM then
      B := FTimeFormat = tf12
    else
      B := Ord(FTimeDisplay) + 1 >= Ord(TimeTextPart);

    if B then
      FSelectedTextPart := 4 + Ord(TimeTextPart);
  end;
  Invalidate;
end;

procedure TCustomDateTimeEditor.ConfirmChanges;
begin
  UpdateIfUserChangedText;
  FConfirmedDate := FDateTime;
end;

procedure TCustomDateTimeEditor.UndoChanges;
begin
  SetDateTime(FConfirmedDate);
end;

procedure TCustomDateTimeEditor.SetDateTimeJumpMinMax(const AValue: TDateTime);
begin
  FJumpMinMax := True;
  try
    SetDateTime(AValue);
  finally
    FJumpMinMax := False;
  end;
end;

procedure TCustomDateTimeEditor.ChangeDateTimeInternally(const AValue: TDateTime);
begin
  Inc(FStoredLockCount);
  try
    SetDateTime(AValue);
  finally
    Dec(FStoredLockCount);
  end;
end;

procedure TCustomDateTimeEditor.DoEnter;
begin
  inherited DoEnter;

  Invalidate;
end;

procedure TCustomDateTimeEditor.DoExit;
begin
  ConfirmChanges;
  inherited DoExit;
  Invalidate;
end;

procedure TCustomDateTimeEditor.EditingDone;
begin
  UpdateIfUserChangedText;

  inherited EditingDone;
end;

procedure TCustomDateTimeEditor.SetEnabled(Value: Boolean);
begin
  if GetEnabled <> Value then begin
    inherited SetEnabled(Value);
    Invalidate;
  end;
end;

procedure TCustomDateTimeEditor.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  TextOrigin: TPoint;
  Was0: Boolean;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  Was0 := PreferredHeight = 0;

  RecalculateTextSizesIfNeeded;
  TextOrigin := GetTextOrigin;

  PreferredWidth := PreferredWidth + 2 * TextOrigin.x + FDateWidth + FTimeWidth;

  if Kind = dtkDateTime then
    PreferredWidth := PreferredWidth + 2 * FDigitWidth;

  PreferredHeight := Max(2 * TextOrigin.y + FTextHeight, PreferredHeight);

  if Was0 then begin
    if BorderStyle = bsSingle then begin
{
Only by experimenting, I came to conclusion that BorderStyle bsSingle needs two
pixels on each side around. It has nothing to do with BorderWidth property,
that's apparently separate thing. Is there some property which gives this value?
For now, I just assume two pixels on each side. Therefore, I add 4 to width and
height:
This seems to work well on both Windows (win WS) and Linux (gtk2 WS).
  -- and on Qt - Windows and Linux --
}
      PreferredWidth := PreferredWidth + 4;
      PreferredHeight := PreferredHeight + 4;
    end;
  end;
end;

procedure TCustomDateTimeEditor.IncreaseCurrentTextPart;
begin
  if DateIsNull then begin
    if FSelectedTextPart <= 3 then
      SetDateTime(SysUtils.Date)
    else
      SetDateTime(SysUtils.Now);

    //Change;
  end else begin
    case GetCurrentDateTextPart of
      dtpDay: IncreaseDay;
      dtpMonth: IncreaseMonth;
      dtpYear: IncreaseYear;
    else
      case GetCurrentTimeTextPart of
        ttpHour: IncreaseHour;
        ttpMinute: IncreaseMinute;
        ttpSecond: IncreaseSecond;
        ttpMiliSec: IncreaseMiliSec;
        ttpAMPM: ChangeAMPM;
      end;
    end;
  end;
end;

procedure TCustomDateTimeEditor.DecreaseCurrentTextPart;
begin
  if DateIsNull then begin
    if FSelectedTextPart <= 3 then
      SetDateTime(SysUtils.Date)
    else
      SetDateTime(SysUtils.Now);

    //Change;
  end else begin
    case GetCurrentDateTextPart of
      dtpDay: DecreaseDay;
      dtpMonth: DecreaseMonth;
      dtpYear: DecreaseYear;
    else
      case GetCurrentTimeTextPart of
        ttpHour: DecreaseHour;
        ttpMinute: DecreaseMinute;
        ttpSecond: DecreaseSecond;
        ttpMiliSec: DecreaseMiliSec;
        ttpAMPM: ChangeAMPM;
      end;
    end;
  end;
end;

constructor TCustomDateTimeEditor.Create(AOwner: TComponent);
var
  I: Integer;
  TTP: TTimeTextPart;
begin
  inherited Create(AOwner);

  FKind := dtkDate;
  FNullInputAllowed := True;
  FTextForNullDate := 'NULL';
  FCenturyFrom := 1941;
  FRecalculatingTextSizesNeeded := True;
  FOnChange := nil;
  FSeparatorWidth := 0;
  FSepNoSpaceWidth := 0;
  FDigitWidth := 0;
  FTimeSeparatorWidth := 0;
  FAMPMWidth := 0;
  FDateWidth := 0;
  FTimeWidth := 0;
  FTextHeight := 0;
  for I := Low(FTextPart) to High(FTextPart) do
    FTextPart[I] := '';
  for TTP := Low(TTimeTextPart) to High(TTimeTextPart) do
    FTimeText[TTP] := '';
  FTimeDisplay := tdHMS;
  FTimeFormat := tf24;

  FLeadingZeros := True;
  FStoredLockCount := 0;
  FReadOnly := False;
  FDateTime := SysUtils.Now;
  FConfirmedDate := FDateTime;
  FMinDate := TheSmallestDate;
  FMaxDate := TheBiggestDate;
  FTrailingSeparator := False;
  FDateDisplayOrder := ddoTryDefault;
  FSelectedTextPart := 1;
  FUseDefaultSeparators := True;
  FDateSeparator := SysUtils.DateSeparator;
  FTimeSeparator := SysUtils.TimeSeparator;
  FEffectiveCenturyFrom := FCenturyFrom;
  FJumpMinMax := False;

  ParentColor := False;
  TabStop := True;
  BorderWidth := 2;
  BorderStyle := bsSingle;
  ParentFont := True;
  AutoSize := True;

  UpdateDate;
end;

procedure TCustomDateTimeEditor.IncreaseMonth;
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  if YMD.Month >= 12 then
    YMD.Month := 1
  else
    Inc(YMD.Month);

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.IncreaseYear;
var
  YMD: TYMD;
begin
  SelectYear;
  YMD := GetYYYYMMDD(True);

  Inc(YMD.Year);
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.IncreaseDay;
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  if YMD.Day >= NumberOfDaysInMonth(YMD.Month, YMD.Year) then
    YMD.Day := 1
  else
    Inc(YMD.Day);

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.DecreaseMonth;
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  if YMD.Month <= 1 then
    YMD.Month := 12
  else
    Dec(YMD.Month);

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.DecreaseYear;
var
  YMD: TYMD;
begin
  SelectYear;
  YMD := GetYYYYMMDD(True);
  Dec(YMD.Year);
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;
  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.DecreaseDay;
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  if YMD.Day <= 1 then
    YMD.Day := NumberOfDaysInMonth(YMD.Month, YMD.Year)
  else
    Dec(YMD.Day);

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateTimeEditor.IncreaseHour;
var
  HMSMs: THMSMs;
begin
  SelectHour;
  HMSMs := GetHMSMs(True);

  if HMSMs.Hour >= 23 then
    HMSMs.Hour := 0
  else
    Inc(HMSMs.Hour);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.IncreaseMinute;
var
  HMSMs: THMSMs;
begin
  SelectMinute;
  HMSMs := GetHMSMs(True);

  if HMSMs.Minute >= 59 then
    HMSMs.Minute := 0
  else
    Inc(HMSMs.Minute);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.IncreaseSecond;
var
  HMSMs: THMSMs;
begin
  SelectSecond;
  HMSMs := GetHMSMs(True);

  if HMSMs.Second >= 59 then
    HMSMs.Second := 0
  else
    Inc(HMSMs.Second);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.IncreaseMiliSec;
var
  HMSMs: THMSMs;
begin
  SelectMiliSec;
  HMSMs := GetHMSMs(True);

  if HMSMs.MiliSec >= 999 then
    HMSMs.MiliSec := 0
  else
    Inc(HMSMs.MiliSec);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.DecreaseHour;
var
  HMSMs: THMSMs;
begin
  SelectHour;
  HMSMs := GetHMSMs(True);

  if HMSMs.Hour <= 0 then
    HMSMS.Hour := 23
  else
    Dec(HMSMs.Hour);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.DecreaseMinute;
var
  HMSMs: THMSMs;
begin
  SelectMinute;
  HMSMs := GetHMSMs(True);

  if HMSMs.Minute <= 0 then
    HMSMs.Minute := 59
  else
    Dec(HMSMs.Minute);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.DecreaseSecond;
var
  HMSMs: THMSMs;
begin
  SelectSecond;
  HMSMs := GetHMSMs(True);

  if HMSMs.Second <= 0 then
    HMSMs.Second := 59
  else
    Dec(HMSMs.Second);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.DecreaseMiliSec;
var
  HMSMs: THMSMs;
begin
  SelectMiliSec;
  HMSMs := GetHMSMs(True);

  if HMSMs.MiliSec <= 0 then
    HMSMs.MiliSec := 999
  else
    Dec(HMSMs.MiliSec);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.ChangeAMPM;
var
  HMSMs: THMSMs;
begin
  SelectAMPM;
  HMSMs := GetHMSMs(True);

  if HMSMs.Hour >= 12 then
    Dec(HMSMS.Hour, 12)
  else
    Inc(HMSMS.Hour, 12);

  SetHMSMs(HMSMs);
end;

procedure TCustomDateTimeEditor.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TCustomZVDateTimePicker }

procedure TCustomZVDateTimePicker.SetChecked(const AValue: Boolean);
begin
  if Assigned(FCheckBox) then
    FCheckBox.Checked := AValue;

  Invalidate;
end;

procedure TCustomZVDateTimePicker.CheckIfDateEditorIsEnabled;
begin
  FDateTimeEditor.SetEnabled(Self.Enabled and GetChecked);

  if Assigned(FArrowButton) then begin
    FPanelForArrowButton.Enabled := FDateTimeEditor.Enabled;
    FArrowButton.Enabled := FDateTimeEditor.Enabled;
  end;
  if Assigned(FUpDown) then
    FUpDown.Enabled := FDateTimeEditor.Enabled;

end;

procedure TCustomZVDateTimePicker.SetDateDisplayOrder(const AValue: TDateDisplayOrder);
begin
  FDateTimeEditor.DateDisplayOrder := AValue;
end;

procedure TCustomZVDateTimePicker.SetDateMode(const AValue: TDTDateMode);
begin
  UpdateShowArrowButton(AValue, Kind);
  FDateMode := AValue;
end;

procedure TCustomZVDateTimePicker.SetKind(const AValue: TDateTimeKind);
begin
  if FDateTimeEditor.Kind <> AValue then begin
    UpdateShowArrowButton(FDateMode, AValue);
    FDateTimeEditor.Kind := AValue;
  end;
end;

procedure TCustomZVDateTimePicker.SetLeadingZeros(const AValue: Boolean);
begin
  FDateTimeEditor.LeadingZeros := AValue;
end;

procedure TCustomZVDateTimePicker.SetNullInputAllowed(const AValue: Boolean);
begin
  FDateTimeEditor.NullInputAllowed := AValue;
end;

procedure TCustomZVDateTimePicker.SetDate(const AValue: TDate);
begin
  FDateTimeEditor.Date := AValue;
end;

procedure TCustomZVDateTimePicker.SetDateTime(const AValue: TDateTime);
begin
  if not EqualDateTime(FDateTimeEditor.DateTime, AValue) then
    FDateTimeEditor.DateTime := AValue;
end;

procedure TCustomZVDateTimePicker.SetDateSeparator(const AValue: UTF8String);
begin
  FDateTimeEditor.DateSeparator := AValue;
end;

procedure TCustomZVDateTimePicker.SetMaxDate(const AValue: TDate);
begin
  FDateTimeEditor.MaxDate := AValue;
end;

procedure TCustomZVDateTimePicker.SetMinDate(const AValue: TDate);
begin
  FDateTimeEditor.MinDate := AValue;
end;

procedure TCustomZVDateTimePicker.SetReadOnly(const AValue: Boolean);
begin
  FDateTimeEditor.ReadOnly := AValue;
end;

procedure TCustomZVDateTimePicker.SetShowCheckBox(const AValue: Boolean);
var
  CB: TCheckBox;
begin
  if GetShowCheckBox <> AValue then begin
    DisableAlign;
    try
      if AValue then begin
        FCheckBox := TCheckBox.Create(Self);

        {$IFNDEF WINDOWS}
        {  On Windows, the following line seems to not have any effect, but I
           enclosed it in IFNDEF anyway. }
        FCheckBox.Color := clBtnFace; { This line is here because of CheckBox's
           strange behavior in Linux -- when parent's colour is white, which is
           the default in our case (actually, our default is clWindow, but it's
           usually white) and when the check box is on a form shown modally, if
           we close the form and then show it again, the check box refuses to
           paint it's "checker" shape.

           I spent a lot of time trying to solve this and this is the best I
           came up with -- setting the check box's colour to clBtnFace seems to
           be a workaround.

           Nice thing is that it seems not to really effect neither the checker's
           colour on the screen, nor the colour of check box's "box", so we didn't
           actually spoil the check box's default appearence on the screen.   }
        {$ENDIF}

        FCheckBox.ControlStyle := FCheckBox.ControlStyle + [csNoDesignSelectable];
        FCheckBox.AllowGrayed := False;
        FCheckBox.TabStop := False;

        FCheckBox.Parent := Self;

        FCheckBox.Checked := True;

        FCheckBox.OnChange := @CheckBoxChange;
        ArrangeCtrls;
      end else begin
        FCheckBox.OnChange := nil;

        CB := FCheckBox;
        FCheckBox := nil;

        ArrangeCtrls;

        if not CB.Checked then
          CheckIfDateEditorIsEnabled;

        if Self.Enabled and CB.Focused and FDateTimeEditor.Enabled then
          FDateTimeEditor.SetFocus;

        FreeAndNil(CB);
      end;

    finally
      EnableAlign;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.SetTextForNullDate(const AValue: UTF8String);
begin
  FDateTimeEditor.TextForNullDate := AValue;
end;

procedure TCustomZVDateTimePicker.SetTime(const AValue: TTime);
begin
  FDateTimeEditor.Time := AValue;
end;

procedure TCustomZVDateTimePicker.SetTimeSeparator(const AValue: UTF8String);
begin
  FDateTimeEditor.TimeSeparator := AValue;
end;

procedure TCustomZVDateTimePicker.SetTimeDisplay(const AValue: TTimeDisplay);
begin
  FDateTimeEditor.TimeDisplay := AValue;
end;

procedure TCustomZVDateTimePicker.SetTimeFormat(const AValue: TTimeFormat);
begin
  FDateTimeEditor.TimeFormat := AValue;
end;

procedure TCustomZVDateTimePicker.SetTrailingSeparator(const AValue: Boolean);
begin
  FDateTimeEditor.TrailingSeparator := AValue;
end;

procedure TCustomZVDateTimePicker.SetUseDefaultSeparators(const AValue: Boolean);
begin
  FDateTimeEditor.UseDefaultSeparators := AValue;
end;

procedure TCustomZVDateTimePicker.DestroyTheCalendar;
begin
  if Assigned(FCal) then begin
    FCal.OnChange := nil;
    FCal.OnResize := nil;
    FCal.OnMouseUp := nil;
    FCal.OnKeyDown := nil;
    FreeAndNil(FCal);
  end;
  FreeAndNil(FShape);
end;

procedure TCustomZVDateTimePicker.AdjustCalendarFormSize;
begin
  FCalendarForm.ClientWidth := FCal.Width + 2;
  FCalendarForm.ClientHeight := FCal.Height + 2;

  FShape.SetBounds(0, 0, FCalendarForm.ClientWidth, FCalendarForm.ClientHeight);

  AdjustCalendarFormScreenPosition;
end;

procedure TCustomZVDateTimePicker.CreateCalendarForm;
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) then begin
    DestroyCalendarForm;

    FCloseCalendarOnChange := False;

    P := Point(0, 0);
    FCal := TCalendar.Create(nil);
    FCal.AutoSize := True;
    FCal.GetPreferredSize(P.x, P.y);

    FCal.Align := alNone;

    FCal.SetBounds(1, 1, P.x, P.y);
    FCal.TabStop := True;

    FCalendarForm := TCustomForm.CreateNew(nil);

  {$IFDEF LCL_0_9_29_OR_AFTER}
  // Nice new property!
    FCalendarForm.PopupMode := pmAuto;
  {$ENDIF}

    FCalendarForm.SetBounds(-8000, -8000, P.x + 2, P.y + 2);
    FRememberedCalendarFormOrigin := Point(-8000, -8000);

    FCalendarForm.ShowInTaskBar := stNever;
    FCalendarForm.BorderStyle := bsNone;

    FShape := TShape.Create(nil);
    FShape.Brush.Style := bsClear;

    FCal.Parent := FCalendarForm;
    FShape.Parent := FCalendarForm;

    FCal.OnResize := @CalendarResize;
    FCal.OnMouseUp := @CalendarMouseUp;
    FCal.OnKeyDown := @CalendarKeyDown;
    FCal.OnChange := @CalendarChange;

    FCalendarForm.OnDeactivate := @CalendarFormDeactivate;
    FCalendarForm.OnClose := @CalendarFormClose;
    FCalendarForm.OnShow := @CalendarFormShow;
    FCalendarForm.OnDestroy := @CalendarFormDestroy;

    //AdjustCalendarFormSize;
  end;
end;

procedure TCustomZVDateTimePicker.DestroyCalendarForm;
begin
  if Assigned(FCalendarForm) then begin
    DestroyTheCalendar;
    FCalendarForm.Release;
    FCalendarForm := nil;
  end;
end;

procedure TCustomZVDateTimePicker.AdjustCalendarFormScreenPosition;
var
  R: TRect;
  P: TPoint;
  H, W: Integer;
begin
  H := FCalendarForm.Height;
  W := FCalendarForm.Width;

  P := ControlToScreen(Point(0, Height));

  R := Screen.MonitorFromWindow(Self.Handle).WorkareaRect;

  if P.y > R.Bottom - H then
    P.y := P.y - H - Height;

  if P.y < R.Top then
    P.y := R.Top;

  if P.x > R.Right - W then
    P.x := R.Right - W;

  if P.x < R.Left then
    P.x := R.Left;

  if (P.x <> FRememberedCalendarFormOrigin.x)
            or (P.y <> FRememberedCalendarFormOrigin.y) then begin
    FCalendarForm.SetBounds(P.x, P.y, W, H);
    FRememberedCalendarFormOrigin := P;
  end;

end;

// In older Lazarus versions, GetControlClassDefaultSize is of type TPoint,
// since 0.9.29, svn rev. 25204, it's TSize.
{$IFDEF LCL_0_9_29_OR_AFTER}
class function TCustomZVDateTimePicker.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 102;
  Result.cy := 23;
end;
{$ELSE}
class function TCustomZVDateTimePicker.GetControlClassDefaultSize: TPoint;
begin
  Result.x := 102;
  Result.y := 23;
end;
{$ENDIF}

procedure TCustomZVDateTimePicker.SetHint(const Value: TTranslateString);
begin
  inherited SetHint(Value);
  FDateTimeEditor.Hint := Hint;
end;

procedure TCustomZVDateTimePicker.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  CheckIfDateEditorIsEnabled;
end;

// I had to override CreateWnd, because in design time on Linux Lazarus crashes
// if we try to do anchoring of child controls in constructor.
// Therefore, I needed to ensure that controls anchoring does not take place
// before CreateWnd has done. So, I moved all anchoring code to a procedure
// ArrangeCtrls and introduced a boolean field FDoNotArrangeControls which
// prevents that code from executing before CreateWnd.
//!!! Later, I simplified the arranging procedure, so maybe it can be done now
//    before window creation is done. It's better to leave this delay system,
//    anyway -- we might change anchoring code again for some reason.
procedure TCustomZVDateTimePicker.CreateWnd;
begin
  inherited CreateWnd;

  if FDoNotArrangeControls then begin { This field is set to True in constructor.
    Its purpose is to prevent control anchoring until this point. That's because
    on Linux Lazarus crashes when control is dropped on form in designer if
    particular anchoring code executes before CreateWnd has done its job. }
    FDoNotArrangeControls := False;
    ArrangeCtrls;
  end;
end;

procedure TCustomZVDateTimePicker.SetDateTimeJumpMinMax(const AValue: TDateTime);
begin
  FDateTimeEditor.SetDateTimeJumpMinMax(AValue);
end;


procedure TCustomZVDateTimePicker.ArrangeCtrls;
begin
  if not FDoNotArrangeControls then begin //Read the note above CreateWnd procedure.
    DisableAutoSizing;
    DisableAlign;
    try
      FDateTimeEditor.Align := alNone;

      if GetShowCheckBox then begin
        FCheckBox.Align := alLeft;
        FCheckBox.BorderSpacing.Left := 2;
        FDateTimeEditor.AnchorToCompanion(akLeft, 0, FCheckBox);
        FCheckBox.BringToFront;
      end else begin
        FDateTimeEditor.Align := alLeft;
      end;

      CheckIfDateEditorIsEnabled;
      InvalidatePreferredSize;
      AdjustSize;

      Invalidate;
    finally
      EnableAlign;
      EnableAutoSizing;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomZVDateTimePicker.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  if Assigned(FArrowButton) then
    PreferredWidth := PreferredWidth +
                     Min(FPanelForArrowButton.Width, FDateTimeEditor.Width) + 2;

  if Assigned(FUpDown) then
    PreferredWidth := PreferredWidth +
                        Min(FUpDown.Width, FDateTimeEditor.Width) + 2;

end;

procedure TCustomZVDateTimePicker.SelectDate;
begin
  FDateTimeEditor.SelectDate;
end;

procedure TCustomZVDateTimePicker.SelectTime;
begin
  FDateTimeEditor.SelectTime;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorChange(Sender: TObject);
begin
  Change;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(Key, Shift); // We call our event handler first, so that handler can
    // prevent the default code from executing (by assigning 0 to Key; which
    // would also prevent FDateTimeEditor's default key handling, as this code
    // is called from there first).

  if Key = VK_SPACE then begin
    // Pressing the space bar checks / unchecks the check box.
    if GetShowCheckBox then begin
    { On Linux, it seems to be enough to call FCheckBox.SetFocus. Check box
      gets checked or unchecked automatically. Maybe the check box receives
      this key event too, I don't know, but when I did testing on Linux,
      leaving both next lines acted as if the check box is clicked twice, so
      we need to isolate the first line from Linux.
      On Windows, however, both next lines should execute.  }
//    {$IFDEF LCLWin32}
// It seems that this behaviour is special to Gtk2 ws. So, we will use this:
    {$IFNDEF LCLGtk2}
      FCheckBox.Checked := not FCheckBox.Checked;
    {$ENDIF}
      FCheckBox.SetFocus;
    end;
  end;

end;

procedure TCustomZVDateTimePicker.DateTimeEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyUp(Key, Shift);
end;

procedure TCustomZVDateTimePicker.DateTimeEditorKeyPress(Sender: TObject; var Key: char);
begin
  KeyPress(Key);
end;

procedure TCustomZVDateTimePicker.DateTimeEditorUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

procedure TCustomZVDateTimePicker.DateTimeEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  P := FDateTimeEditor.ClientOrigin;
  P1 := ClientOrigin;
  X := X + P.x - P1.x;
  Y := Y + P.y - P1.y;

  MouseDown(Button, Shift, X, Y);
end;

procedure TCustomZVDateTimePicker.DateTimeEditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  P := FDateTimeEditor.ClientOrigin;
  P1 := ClientOrigin;
  X := X + P.x - P1.x;
  Y := Y + P.y - P1.y;

  MouseMove(Shift, X, Y);
end;

procedure TCustomZVDateTimePicker.DateTimeEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  P := FDateTimeEditor.ClientOrigin;
  P1 := ClientOrigin;
  X := X + P.x - P1.x;
  Y := Y + P.y - P1.y;

  MouseUp(Button, Shift, X, Y);
end;

procedure TCustomZVDateTimePicker.DateTimeEditorEnter(Sender: TObject);
begin
  FCallFromDateTimeEditorEnter := True;
  try
    DoEnter;
  finally
    FCallFromDateTimeEditorEnter := False;
  end;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorExit(Sender: TObject);
begin
  FCallFromDateTimeEditorExit := True;
  try
    DoExit;
  finally
    FCallFromDateTimeEditorExit := False;
  end;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorEditingDone(Sender: TObject);
begin
  EditingDone;
end;

procedure TCustomZVDateTimePicker.ArrowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DropDownCalendarForm;
end;

procedure TCustomZVDateTimePicker.UpDownClick(Sender: TObject;
  Button: TUDBtnType);
begin
  FDateTimeEditor.SetFocus;
  if not ReadOnly then begin
    if Button = btNext then
      FDateTimeEditor.IncreaseCurrentTextPart
    else
      FDateTimeEditor.DecreaseCurrentTextPart;
  end;
end;

procedure TCustomZVDateTimePicker.DoDropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TCustomZVDateTimePicker.DoCloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

function TCustomZVDateTimePicker.GetChecked: Boolean;
begin
  Result := (not Assigned(FCheckBox)) or (FCheckBox.State = cbChecked);
end;

procedure TCustomZVDateTimePicker.DrawArrowButtonGlyph;
const
  ArrowColor = TColor($8D665A);
begin
// First I ment to put arrow images in a lrs file. In my opinion, however, that
// wouldn't be an elegant option for so simple shapes.

  if Assigned(FArrowButton) then begin
    FArrowButton.Glyph.TransparentColor := clRed;
    FArrowButton.Glyph.SetSize(9, 6);
    FArrowButton.Glyph.Canvas.Brush.Style := bsSolid;
    FArrowButton.Glyph.Canvas.Brush.Color := clSkyBlue;
    FArrowButton.Glyph.Canvas.FillRect(0, 0, 9, 6);
    FArrowButton.Glyph.Canvas.Pen.Color := ArrowColor;
    FArrowButton.Glyph.Canvas.Brush.Color := FArrowButton.Glyph.Canvas.Pen.Color;

{ Let's draw shape of the arrow on the button: }
    case FArrowShape of
      asClassicLarger:
        { triangle: }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(8, 1), Point(4, 5)]);
      asClassicSmaller:
        { triangle -- smaller variant:  }
        FArrowButton.Glyph.Canvas.Polygon([Point(1, 2), Point(7, 2), Point(4, 5)]);
      asModernLarger:
        { modern: }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0), Point(4, 3),
                                        Point(7, 0), Point(8, 1), Point(4, 5)]);
      asModernSmaller:
        { modern -- smaller variant:    }
        FArrowButton.Glyph.Canvas.Polygon([Point(1, 2), Point(2, 1), Point(4, 3),
                                        Point(6, 1), Point(7, 2), Point(4, 5)]);
      asYetAnotherShape:
        { something in between, not very pretty:  }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0), Point(2, 1),
                            Point(6, 1),Point(7, 0), Point(8, 1), Point(4, 5)]);
    end;
    FArrowButton.Glyph.TransparentColor := clSkyBlue;
  end;
end;

function TCustomZVDateTimePicker.GetCenturyFrom: Word;
begin
  Result := FDateTimeEditor.CenturyFrom;
end;

function TCustomZVDateTimePicker.AreSeparatorsStored: Boolean;
begin
  Result := not GetUseDefaultSeparators;
end;

function TCustomZVDateTimePicker.GetDateDisplayOrder: TDateDisplayOrder;
begin
  Result := FDateTimeEditor.DateDisplayOrder;
end;

function TCustomZVDateTimePicker.GetKind: TDateTimeKind;
begin
  Result := FDateTimeEditor.Kind;
end;

function TCustomZVDateTimePicker.GetLeadingZeros: Boolean;
begin
  Result := FDateTimeEditor.LeadingZeros;
end;

function TCustomZVDateTimePicker.GetNullInputAllowed: Boolean;
begin
  Result := FDateTimeEditor.NullInputAllowed;
end;

function TCustomZVDateTimePicker.GetDate: TDate;
begin
  Result := FDateTimeEditor.Date;
end;

function TCustomZVDateTimePicker.GetDateTime: TDateTime;
begin
  Result := FDateTimeEditor.DateTime;
end;

function TCustomZVDateTimePicker.GetDateSeparator: UTF8String;
begin
  Result := FDateTimeEditor.DateSeparator;
end;

function TCustomZVDateTimePicker.GetMaxDate: TDate;
begin
  Result := FDateTimeEditor.MaxDate;
end;

function TCustomZVDateTimePicker.GetMinDate: TDate;
begin
  Result := FDateTimeEditor.MinDate;
end;

function TCustomZVDateTimePicker.GetReadOnly: Boolean;
begin
  Result := FDateTimeEditor.ReadOnly;
end;

function TCustomZVDateTimePicker.GetShowCheckBox: Boolean;
begin
  Result := Assigned(FCheckBox);
end;

function TCustomZVDateTimePicker.GetTextForNullDate: UTF8String;
begin
  Result := FDateTimeEditor.TextForNullDate;
end;

function TCustomZVDateTimePicker.GetTime: TTime;
begin
  Result := FDateTimeEditor.GetTime;
end;

function TCustomZVDateTimePicker.GetTimeSeparator: UTF8String;
begin
  Result := FDateTimeEditor.TimeSeparator;
end;

function TCustomZVDateTimePicker.GetTimeDisplay: TTimeDisplay;
begin
  Result := FDateTimeEditor.TimeDisplay;
end;

function TCustomZVDateTimePicker.GetTimeFormat: TTimeFormat;
begin
  Result := FDateTimeEditor.TimeFormat;
end;

function TCustomZVDateTimePicker.GetTrailingSeparator: Boolean;
begin
  Result := FDateTimeEditor.TrailingSeparator;
end;

function TCustomZVDateTimePicker.GetUseDefaultSeparators: Boolean;
begin
  Result := FDateTimeEditor.UseDefaultSeparators;
end;

procedure TCustomZVDateTimePicker.SetArrowShape(const AValue: TArrowShape);
begin
  if FArrowShape = AValue then Exit;

  FArrowShape := AValue;
  DrawArrowButtonGlyph;
end;

procedure TCustomZVDateTimePicker.SetCenturyFrom(const AValue: Word);
begin
  FDateTimeEditor.CenturyFrom := AValue;
end;

procedure TCustomZVDateTimePicker.CheckBoxChange(Sender: TObject);
begin
  CheckIfDateEditorIsEnabled;
  if Assigned(FCheckBox) then begin
    if FCheckBox.Focused and FDateTimeEditor.Enabled then
      FDateTimeEditor.SetFocus;
    FCheckBox.TabStop := not FDateTimeEditor.Enabled;
  end;
end;

procedure TCustomZVDateTimePicker.CalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FCal.HitTest(Point(X, Y)) in [cpDate, cpNoWhere] then begin

    // According to tests made by Željko Rikalo,
    // on Qt widgetset the calendar's DateTime field does not get updated if
    // we close the calendar form here, because on Qt change is not made until
    // after the OnMouseUp event.

    // Let's then try something else, as proposed by Željko:
    // Closing the calendar form is moved to Calendar.OnChange.
    {$IFDEF QT_BEFORE_0_9_29}
    // Željko changed the Qt behaviour since Lazarus 0.9.29, revision 23641.
      FCloseCalendarOnChange := True; // This is asked in
                                          // CalendarChange procedure.
    {$ELSE}
    // On the other hand, on other widgetsets, the previous wouldn't work
    // because OnChange gets called before OnMoueseUp event, so the OnChange
    // event is already executed when we are here, so it's too late to notify
    // it now.
    // But the calendar's date is already changed then and we can simply
    // call CloseCalendarForm immidiately.
      CloseCalendarForm(True);
    {$ENDIF}
  end;
end;

procedure TCustomZVDateTimePicker.CalendarChange(Sender: TObject);
begin
{$IFDEF QT_BEFORE_0_9_29}
  // See the coments in CalendarMouseUp procedure.
  if FCloseCalendarOnChange then
    CloseCalendarForm(True);
{$ENDIF}
end;

procedure TCustomZVDateTimePicker.CalendarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        CloseCalendarForm;
        //Key := 0;
      end;
    VK_RETURN, VK_SPACE:
      begin
        CloseCalendarForm(True);
        //Key := 0;
      end;
  end;
end;

procedure TCustomZVDateTimePicker.CalendarFormDeactivate(Sender: TObject);
begin
  if not FClosingCalendarForm then
    CloseCalendarForm;
end;

procedure TCustomZVDateTimePicker.CalendarFormShow(Sender: TObject);
begin
  FClosingCalendarForm := False;
  //AdjustCalendarFormScreenPosition;
  AdjustCalendarFormSize;
  DoDropDown; // calls OnDropDown event handler
end;

procedure TCustomZVDateTimePicker.CalendarFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FClosingCalendarForm := True;
  CloseAction := caFree;
end;

procedure TCustomZVDateTimePicker.CalendarFormDestroy(Sender: TObject);
begin
  DestroyTheCalendar;
  FCalendarForm := nil;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorClick(Sender: TObject);
begin
  Click;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorDblClick(Sender: TObject);
begin
  DblClick;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorTripleClick(Sender: TObject);
begin
  TripleClick;
end;

procedure TCustomZVDateTimePicker.DateTimeEditorQuadClick(Sender: TObject);
begin
  QuadClick;
end;

procedure TCustomZVDateTimePicker.DoEnter;
begin
  if FCallFromDateTimeEditorEnter then
    inherited DoEnter
  else if FDateTimeEditor.Enabled then
    FDateTimeEditor.SetFocus
  else if Assigned(FCheckBox) then
    FCheckBox.SetFocus;
end;

procedure TCustomZVDateTimePicker.DoExit;
begin
  if FCallFromDateTimeEditorExit then
    inherited DoExit;
end;

procedure TCustomZVDateTimePicker.CloseCalendarForm(AndSetTheDate: Boolean);
begin
  if Assigned(FCalendarForm) and (not FClosingCalendarForm) then begin
    FClosingCalendarForm := True;
    if AndSetTheDate then begin
      if DateIsNull then begin
        // we'll set the time to 0.0 (midnight)
        FDateTimeEditor.ChangeDateTimeInternally(Int(FCal.DateTime));
//        Change;
      end else if not EqualDateTime(Int(DateTime), Int(FCal.DateTime)) then begin
        // we'll change the date, but keep the time:
        FDateTimeEditor.ChangeDateTimeInternally(
                                ComposeDateTime(FCal.DateTime, DateTime));
//        Change;
      end;
    end;

    try
      FDateTimeEditor.SetFocus;
    except
    end;

    FCalendarForm.Close;
    DoCloseUp;
  end;
end;

procedure TCustomZVDateTimePicker.DropDownCalendarForm;
{$IFNDEF LCLWin32}
 var
   F: TCustomForm;
{$ENDIF}
begin
  if not (ReadOnly or Assigned(FCalendarForm)) then begin
    try
      CreateCalendarForm;

      if DateIsNull then
        FCal.DateTime := Max(MinDate, Min(SysUtils.Date, MaxDate))

      else if DateTime < MinDate then // These "out of bounds" values can
        FCal.DateTime := MinDate     // happen when DateTime was set with
      else if DateTime > MaxDate then // "SetDateTimeJumpMinMax" protected
        FCal.DateTime := MaxDate     // procedure (used in TDBZVDateTimePicker control).

      else
        FCal.DateTime := DateTime;

  {$IFNDEF LCLWin32}
      // On Gtk2, it seems that if a non-modal form is shown on top
      // of a modal one, it can't get user interaction. So it is useless then.
      // Therefore, if our parent is shown modally, we must show the calendar
      // on a modal form too.
      // Seems that it applies to Qt also!
      F := GetParentForm(Self);
      if Assigned(F) and (fsModal in F.FormState) then
        FCalendarForm.ShowModal
      else
  {$ENDIF}
        FCalendarForm.Show;

    finally
      if Assigned(FCalendarForm) and (not FCalendarForm.Visible) then
        DestroyCalendarForm;
    end;
  end;
end;


type

  { TDTUpDown }

{ The two buttons contained by UpDown control are never disabled in original
  UpDown class. This class is defined here to override this behaviour. }
  TDTUpDown = class(TCustomUpDown)
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure CalculatePreferredSize(var PreferredWidth,
                   PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

{ TDTUpDown }

{ When our UpDown control gets enabled/disabled, the two its buttons' Enabled
  property is set accordingly. }
procedure TDTUpDown.SetEnabled(Value: Boolean);
var
  I: Integer;
begin
  inherited SetEnabled(Value);
  for I := 0 to ControlCount - 1 do begin
    Controls[I].Enabled := Value;
  end;
end;

{ Our UpDown control is always alligned, but setting its PreferredHeight
  uncoditionally to 0 prevents the UpDown to mess with our PreferredHeight.
  The problem is that if we didn't do this, when our Height is greater than
  really preffered, UpDown prevents it to be set correctly when we set AutoSize
  to True. }
procedure TDTUpDown.CalculatePreferredSize(var PreferredWidth, PreferredHeight:
  integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  PreferredHeight := 0;
end;

procedure TCustomZVDateTimePicker.UpdateShowArrowButton(
                             NewDateMode: TDTDateMode; NewKind: TDateTimeKind);

  procedure CreateArrowBtn;
  begin
    if not Assigned(FArrowButton) then begin
      DestroyUpDown;

      // We'll add a Panel which will be SpeedButton's parent.
      // It's only needed because it's more appropriate to have speed button
      // above the date text when we are not auto sized.
      // The speed button itself cannot be brought in front of DateEditor,
      // because TSpeedButton does not descend from TWinControl, so we need a
      // panel to carry it.
      FPanelForArrowButton := TPanel.Create(Self);
      FPanelForArrowButton.ControlStyle := FPanelForArrowButton.ControlStyle +
                                           [csNoFocus, csNoDesignSelectable];
      FPanelForArrowButton.BorderStyle := bsNone;
      FPanelForArrowButton.Caption := '';
      FPanelForArrowButton.BevelInner := bvNone;
      FPanelForArrowButton.BevelOuter := bvNone;
      FPanelForArrowButton.SetBounds(0, 0, 17, 1);
      FPanelForArrowButton.Parent := Self;
      FPanelForArrowButton.Align := alRight;
      FPanelForArrowButton.BringToFront;

      FArrowButton := TSpeedButton.Create(Self);
      FArrowButton.ControlStyle := FArrowButton.ControlStyle +
                                            [csNoFocus, csNoDesignSelectable];
      FArrowButton.SetBounds(0, 0, 17, 1);
      FArrowButton.Parent := FPanelForArrowButton;
      FArrowButton.Align := alClient;
      FArrowButton.BringToFront;

      DrawArrowButtonGlyph;

      FArrowButton.OnMouseDown := @ArrowMouseDown;

    end;
  end;

  procedure CreateUpDown;
  begin
    if not Assigned(FUpDown) then begin
      DestroyArrowBtn;

      FUpDown := TDTUpDown.Create(Self);

      FUpDown.ControlStyle := FUpDown.ControlStyle +
                                     [csNoFocus, csNoDesignSelectable];
      FUpDown.SetBounds(0, 0, 15, 1);
      FUpDown.Parent := Self;
      FUpDown.Align := alRight;

      FUpDown.BringToFront;

      TDTUpDown(FUPDown).OnClick := @UpDownClick;

    end;
  end;

var
  ReallyShowCalendar: Boolean;
begin
  if NewDateMode = dmNone then begin
    DestroyArrowBtn;
    DestroyUpDown;
  end else begin
    ReallyShowCalendar := (NewDateMode = dmComboBox) and (NewKind <> dtkTime);

    if (ReallyShowCalendar <> Assigned(FArrowButton)) or
                       (Assigned(FArrowButton) = Assigned(FUpDown)) then begin

      if ReallyShowCalendar then
        CreateArrowBtn
      else
        CreateUpDown;

      ArrangeCtrls;
    end;

  end;
end;

procedure TCustomZVDateTimePicker.DestroyUpDown;
begin
  if Assigned(FUpDown) then begin
    TDTUpDown(FUPDown).OnClick := nil;
    FreeAndNil(FUpDown);
  end;
end;

procedure TCustomZVDateTimePicker.DestroyArrowBtn;
begin
  if Assigned(FArrowButton) then begin
    FArrowButton.OnMouseDown := nil;
    DestroyCalendarForm;
    FreeAndNil(FArrowButton);
    FreeAndNil(FPanelForArrowButton);
  end;
end;

procedure TCustomZVDateTimePicker.CalendarResize(Sender: TObject);
begin
  AdjustCalendarFormSize;
end;

constructor TCustomZVDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do begin
    {$IFDEF LCL_0_9_29_OR_AFTER}
    SetInitialBounds(0, 0, cx, cy); // TSize since 0.9.29, svn rev. 25204
    {$ELSE}
    SetInitialBounds(0, 0, x, y); // TPoint in older Lazarus versions
    {$ENDIF}
  end;

  FArrowShape := asModernSmaller;
  FCallFromDateTimeEditorEnter := False;
  FCallFromDateTimeEditorExit := False;

  FOnDropDown := nil;
  FOnCloseUp := nil;
  FOnChange := nil;
  FForceShowCalendar := False;

  ParentColor := False;
  FCheckBox := nil;
  FArrowButton := nil;
  FPanelForArrowButton := nil;
  FUpDown := nil;
  FDateTimeEditor := TCustomDateTimeEditor.Create(Self);
  FDateTimeEditor.ControlStyle := FDateTimeEditor.ControlStyle + [csNoDesignSelectable];
  FDateTimeEditor.BorderStyle := bsNone;
  FDateTimeEditor.ParentColor := True;
  FDateTimeEditor.Parent := Self;
  FDateTimeEditor.Align := alLeft;

  BorderStyle := bsSingle;
  AutoSize := True;
  TabStop := True;
  FDateTimeEditor.OnChange := @DateTimeEditorChange;
  FDateTimeEditor.OnKeyDown := @DateTimeEditorKeyDown;
  FDateTimeEditor.OnEnter := @DateTimeEditorEnter;
  FDateTimeEditor.OnExit := @DateTimeEditorExit;
  FDateTimeEditor.OnEditingDone := @DateTimeEditorEditingDone;
  FDateTimeEditor.OnClick := @DateTimeEditorClick;
  FDateTimeEditor.OnDblClick := @DateTimeEditorDblClick;
  FDateTimeEditor.OnTripleClick := @DateTimeEditorTripleClick;
  FDateTimeEditor.OnQuadClick := @DateTimeEditorQuadClick;
  FDateTimeEditor.OnKeyUp := @DateTimeEditorKeyUp;
  FDateTimeEditor.OnKeyPress := @DateTimeEditorKeyPress;
  FDateTimeEditor.OnUTF8KeyPress := @DateTimeEditorUTF8KeyPress;
  FDateTimeEditor.OnMouseDown := @DateTimeEditorMouseDown;
  FDateTimeEditor.OnMouseMove := @DateTimeEditorMouseMove;
  FDateTimeEditor.OnMouseUp := @DateTimeEditorMouseUp;

  FShape := nil;
  FCal := nil;
  FCalendarForm := nil;
  FDoNotArrangeControls := True;

  DateMode := dmComboBox;
end;

destructor TCustomZVDateTimePicker.Destroy;
begin
  FDoNotArrangeControls := True;
  DestroyUpDown;
  DestroyArrowBtn;
  SetShowCheckBox(False);

  FDateTimeEditor.OnMouseUp := nil;
  FDateTimeEditor.OnMouseMove := nil;
  FDateTimeEditor.OnMouseDown := nil;
  FDateTimeEditor.OnUTF8KeyPress := nil;
  FDateTimeEditor.OnKeyPress := nil;
  FDateTimeEditor.OnKeyUp := nil;
  FDateTimeEditor.OnQuadClick := nil;
  FDateTimeEditor.OnTripleClick := nil;
  FDateTimeEditor.OnDblClick := nil;
  FDateTimeEditor.OnClick := nil;
  FDateTimeEditor.OnEditingDone := nil;
  FDateTimeEditor.OnExit := nil;
  FDateTimeEditor.OnEnter := nil;
  FDateTimeEditor.OnKeyDown := nil;
  FDateTimeEditor.OnChange := nil;
  FreeThenNil(FDateTimeEditor);

  inherited Destroy;
end;

function TCustomZVDateTimePicker.DateIsNull: Boolean;
begin
  Result := FDateTimeEditor.DateIsNull;
end;

end.
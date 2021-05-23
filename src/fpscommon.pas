unit fpsCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  {@@ These are some basic rgb color volues. FPSpreadsheet will support
    only those built-in color constants originating in the EGA palette.
  }
  {@@ rgb value of @bold(black) color, BIFF2 palette index 0, BIFF8 index 8}
  scBlack = $00000000;
  {@@ rgb value of @bold(white) color, BIFF2 palette index 1, BIFF8 index 9 }
  scWhite = $00FFFFFF;
  {@@ rgb value of @bold(red) color, BIFF2 palette index 2, BIFF8 index 10 }
  scRed = $000000FF;
  {@@ rgb value of @bold(green) color, BIFF2 palette index 3, BIFF8 index 11 }
  scGreen = $0000FF00;
  {@@ rgb value of @bold(blue) color, BIFF2 palette index 4, BIFF8 indexes 12 and 39}
  scBlue = $00FF0000;
  {@@ rgb value of @bold(yellow) color, BIFF2 palette index 5, BIFF8 indexes 13 and 34}
  scYellow = $0000FFFF;
  {@@ rgb value of @bold(magenta) color, BIFF2 palette index 6, BIFF8 index 14 and 33}
  scMagenta = $00FF00FF;
  {@@ rgb value of @bold(cyan) color, BIFF2 palette index 7, BIFF8 indexes 15}
  scCyan = $00FFFF00;

type
  {@@ Colors in fpspreadsheet are given as rgb values in little-endian notation
    (i.e. "r" is the low-value byte). The highest-value byte, if not zero,
    indicates special colors.

    @note(This byte order in TsColor is opposite to that in HTML colors.) }
  TsColor = DWord;

  {@@ Builtin number formats. Only uses a subset of the default formats,
      enough to be able to read/write date/time values.
      nfCustom allows to apply a format string directly. }
  TsNumberFormat = (
    // general-purpose for all numbers
    nfGeneral,
    // numbers
    nfFixed, nfFixedTh, nfExp, nfPercentage, nfFraction,
    // currency
    nfCurrency, nfCurrencyRed,
    // dates and times
    nfShortDateTime, nfShortDate, nfLongDate, nfShortTime, nfLongTime,
    nfShortTimeAM, nfLongTimeAM, nfDayMonth, nfMonthYear, nfTimeInterval,
    // text
    nfText,
    // other (format string goes directly into the file)
    nfCustom);

  {@@ Ancestor of the fpSpreadsheet exceptions }
  EFpSpreadsheet = class(Exception);

resourcestring
  // Format
  rsAmbiguousDecThouSeparator = 'Assuming usage of decimal separator in "%s".';

function Round(AValue: Double): Int64;

procedure FloatToFraction(AValue: Double; AMaxDenominator: Int64;
  out ANumerator, ADenominator: Int64);
function TryStrToFloatAuto(AText: String; out ANumber: Double;
  out ADecimalSeparator, AThousandSeparator: Char; out AWarning: String): Boolean;

procedure AddBuiltinBiffFormats(AList: TStringList;
  AFormatSettings: TFormatSettings; ALastIndex: Integer);

procedure RegisterCurrency(ACurrencySymbol: String);
procedure RegisterCurrencies(AList: TStrings; AReplace: Boolean);
procedure UnregisterCurrency(ACurrencySymbol: String);
function  CurrencyRegistered(ACurrencySymbol: String): Boolean;
procedure GetRegisteredCurrencies(AList: TStrings);

function IsNegative(var AText: String): Boolean;
function RemoveCurrencySymbol(ACurrencySymbol: String;
  var AText: String): Boolean;
function TryStrToCurrency(AText: String; out ANumber: Double;
  out ACurrencySymbol:String; const AFormatSettings: TFormatSettings): boolean;


implementation

uses
  Math, fpsNumFormat;

{@@ ----------------------------------------------------------------------------
  Special rounding function which avoids banker's rounding
-------------------------------------------------------------------------------}
function Round(AValue: Double): Int64;
begin
  if AValue > 0 then
    Result := trunc(AValue + 0.5)
  else
    Result := trunc(AValue - 0.5);
end;

{@@ ----------------------------------------------------------------------------
  Approximates a floating point value as a fraction and returns the values of
  numerator and denominator.

  @param   AValue           Floating point value to be analyzed
  @param   AMaxDenominator  Maximum value of the denominator allowed
  @param   ANumerator       (out) Numerator of the best approximating fraction
  @param   ADenominator     (out) Denominator of the best approximating fraction
-------------------------------------------------------------------------------}
procedure FloatToFraction(AValue: Double; AMaxDenominator: Int64;
  out ANumerator, ADenominator: Int64);
// Uses method of continued fractions, adapted version from a function in
// Bart Broersma's fractions.pp unit:
// http://svn.code.sf.net/p/flyingsheep/code/trunk/ConsoleProjecten/fractions/
const
  MaxInt64 = High(Int64);
  MinInt64 = Low(Int64);
var
  H1, H2, K1, K2, A, NewA, tmp, prevH1, prevK1: Int64;
  B, test, diff, prevdiff: Double;
  PendingOverflow: Boolean;
  i: Integer = 0;
begin
  if (AValue > MaxInt64) or (AValue < MinInt64) then
    raise EFPSpreadsheet.Create('Range error');

  if abs(AValue) < 0.5 / AMaxDenominator then
  begin
    ANumerator := 0;
    ADenominator := AMaxDenominator;
    exit;
  end;

  H1 := 1;
  H2 := 0;
  K1 := 0;
  K2 := 1;
  B := AValue;
  NewA := Round(Floor(B));
  prevH1 := H1;
  prevK1 := K1;
  prevdiff := 1E308;
  repeat
    inc(i);
    A := NewA;
    tmp := H1;
    H1 := A * H1 + H2;
    H2 := tmp;
    tmp := K1;
    K1 := A * K1 + K2;
    K2 := tmp;
    test := H1/K1;
    diff := test - AValue;
    { Use the previous result if the denominator becomes larger than the allowed
      value, or if the difference becomes worse because the "best" result has
      been missed due to rounding error - this is more stable than using a
      predefined precision in comparing diff with zero. }
    if (abs(K1) >= AMaxDenominator) or (abs(diff) > abs(prevdiff)) then
    begin
      H1 := prevH1;
      K1 := prevK1;
      break;
    end;
    if (Abs(B - A) < 1E-30) then
      B := 1E30   //happens when H1/K1 exactly matches Value
    else
      B := 1 / (B - A);
    PendingOverFlow := (B * H1 + H2 > MaxInt64) or
                       (B * K1 + K2 > MaxInt64) or
                       (B > MaxInt64);
    if not PendingOverflow then
      NewA := Round(Floor(B));
    prevH1 := H1;
    prevK1 := K1;
    prevdiff := diff;
  until PendingOverflow;
  ANumerator := H1;
  ADenominator := K1;
end;

{@@ ----------------------------------------------------------------------------
  Converts a string to a floating point number. No assumption on decimal and
  thousand separator are made.

  Is needed for reading CSV files.
-------------------------------------------------------------------------------}
function TryStrToFloatAuto(AText: String; out ANumber: Double;
  out ADecimalSeparator, AThousandSeparator: Char; out AWarning: String): Boolean;
var
  i: Integer;
  testSep: Char;
  testSepPos: Integer;
  lastDigitPos: Integer;
  isPercent: Boolean;
  fs: TFormatSettings;
  done: Boolean;
begin
  Result := false;
  AWarning := '';
  ADecimalSeparator := #0;
  AThousandSeparator := #0;
  if AText = '' then
    exit;

  fs := DefaultFormatSettings;

  // We scan the string starting from its end. If we find a point or a comma,
  // we have a candidate for the decimal or thousand separator. If we find
  // the same character again it was a thousand separator, if not it was
  // a decimal separator.

  // There is one amgiguity: Using a thousand separator for number < 1.000.000,
  // but no decimal separator misinterprets the thousand separator as a
  // decimal separator.

  done := false;      // Indicates that both decimal and thousand separators are found
  testSep := #0;      // Separator candidate to be tested
  testSepPos := 0;    // Position of this separator candidate in the string
  lastDigitPos := 0;  // Position of the last numerical digit
  isPercent := false; // Flag for percentage format

  i := Length(AText);    // Start at end...
  while i >= 1 do        // ...and search towards start
  begin
    case AText[i] of
      '0'..'9':
        if (lastDigitPos = 0) and (AText[i] in ['0'..'9']) then
          lastDigitPos := i;

      'e', 'E':
        ;

      '%':
        begin
          isPercent := true;
          // There may be spaces before the % sign which we don't want
          dec(i);
          while (i >= 1) do
            if AText[i] = ' ' then
              dec(i)
            else
            begin
              inc(i);
              break;
            end;
        end;

      '+', '-':
        ;

      '.', ',':
        begin
          if testSep = #0 then begin
            testSep := AText[i];
            testSepPos := i;
          end;
          // This is the right-most separator candidate in the text
          // It can be a decimal or a thousand separator.
          // Therefore, we continue searching from here.
          dec(i);
          while i >= 1 do
          begin
            if not (AText[i] in ['0'..'9', '+', '-', '.', ',']) then
              exit;

            // If we find the testSep character again it must be a thousand separator,
            // and there are no decimals.
            if (AText[i] = testSep) then
            begin
              // ... but only if there are 3 numerical digits in between
              if (testSepPos - i = 4) then
              begin
                fs.ThousandSeparator := testSep;
                // The decimal separator is the "other" character.
                if testSep = '.' then
                  fs.DecimalSeparator := ','
                else
                  fs.DecimalSeparator := '.';
                AThousandSeparator := fs.ThousandSeparator;
                ADecimalSeparator := #0; // this indicates that there are no decimals
                done := true;
                i := 0;
              end else
              begin
                Result := false;
                exit;
              end;
            end
            else
            // If we find the "other" separator character, then testSep was a
            // decimal separator and the current character is a thousand separator.
            // But there must be 3 digits in between.
            if AText[i] in ['.', ','] then
            begin
              if testSepPos - i <> 4 then  // no 3 digits in between --> no number, maybe a date.
                exit;
              fs.DecimalSeparator := testSep;
              fs.ThousandSeparator := AText[i];
              ADecimalSeparator := fs.DecimalSeparator;
              AThousandSeparator := fs.ThousandSeparator;
              done := true;
              i := 0;
            end;
            dec(i);
          end;
        end;

      else
        exit;  // Non-numeric character found, no need to continue

    end;
    dec(i);
  end;

  // Only one separator candicate found, we assume it is a decimal separator
  if (testSep <> #0) and not done then
  begin
    // Warning in case of ambiguous detection of separator. If only one separator
    // type is found and it is at the third position from the string's end it
    // might by a thousand separator or a decimal separator. We assume the
    // latter case, but create a warning.
    if (lastDigitPos - testSepPos = 3) and not isPercent then
      AWarning := Format(rsAmbiguousDecThouSeparator, [AText]);
    fs.DecimalSeparator := testSep;
    ADecimalSeparator := fs.DecimalSeparator;
    // Make sure that the thousand separator is different from the decimal sep.
    if testSep = '.' then fs.ThousandSeparator := ',' else fs.ThousandSeparator := '.';
  end;

  // Delete all thousand separators from the string - StrToFloat does not like them...
  AText := StringReplace(AText, fs.ThousandSeparator, '', [rfReplaceAll]);

  // Is the last character a percent sign?
  if isPercent then
    while (Length(AText) > 0) and (AText[Length(AText)] in ['%', ' ']) do
      Delete(AText, Length(AText), 1);

  // Try string-to-number conversion
  Result := TryStrToFloat(AText, ANumber, fs);

  // If successful ...
  if Result then
  begin
    // ... take care of the percentage sign
    if isPercent then
      ANumber := ANumber * 0.01;
  end;
end;

{@@ ----------------------------------------------------------------------------
  These are the built-in number formats as expected in the biff spreadsheet file.
  In BIFF5+ they are not written to file but they are used for lookup of the
  number format that Excel used.
-------------------------------------------------------------------------------}
procedure AddBuiltinBiffFormats(AList: TStringList;
  AFormatSettings: TFormatSettings; ALastIndex: Integer);
var
  fs: TFormatSettings absolute AFormatSettings;
  cs: String;
  i: Integer;
begin
  cs := fs.CurrencyString;
  AList.Clear;
  AList.Add('');          // 0
  AList.Add('0');         // 1
  AList.Add('0.00');      // 2
  AList.Add('#,##0');     // 3
  AList.Add('#,##0.00');  // 4
  AList.Add(BuildCurrencyFormatString(nfCurrency, fs, 0, fs.CurrencyFormat, fs.NegCurrFormat, cs));     // 5
  AList.Add(BuildCurrencyFormatString(nfCurrencyRed, fs, 0, fs.CurrencyFormat, fs.NegCurrFormat, cs));  // 6
  AList.Add(BuildCurrencyFormatString(nfCurrency, fs, 2, fs.CurrencyFormat, fs.NegCurrFormat, cs));     // 7
  AList.Add(BuildCurrencyFormatString(nfCurrencyRed, fs, 2, fs.CurrencyFormat, fs.NegCurrFormat, cs));  // 8
  AList.Add('0%');                // 9
  AList.Add('0.00%');             // 10
  AList.Add('0.00E+00');          // 11
  AList.Add('# ?/?');             // 12
  AList.Add('# ??/??');           // 13
  AList.Add(BuildDateTimeFormatString(nfShortDate, fs));     // 14
  AList.Add(BuildDateTimeFormatString(nfLongdate, fs));      // 15
  AList.Add(BuildDateTimeFormatString(nfDayMonth, fs));      // 16: 'd/mmm'
  AList.Add(BuildDateTimeFormatString(nfMonthYear, fs));     // 17: 'mmm/yy'
  AList.Add(BuildDateTimeFormatString(nfShortTimeAM, fs));   // 18
  AList.Add(BuildDateTimeFormatString(nfLongTimeAM, fs));    // 19
  AList.Add(BuildDateTimeFormatString(nfShortTime, fs));     // 20
  AList.Add(BuildDateTimeFormatString(nfLongTime, fs));      // 21
  AList.Add(BuildDateTimeFormatString(nfShortDateTime, fs)); // 22
  for i:=23 to 36 do
    AList.Add('');  // not supported
  AList.Add('_(#,##0_);(#,##0)');              // 37
  AList.Add('_(#,##0_);[Red](#,##0)');         // 38
  AList.Add('_(#,##0.00_);(#,##0.00)');        // 39
  AList.Add('_(#,##0.00_);[Red](#,##0.00)');   // 40
  AList.Add('_("'+cs+'"* #,##0_);_("'+cs+'"* (#,##0);_("'+cs+'"* "-"_);_(@_)');  // 41
  AList.Add('_(* #,##0_);_(* (#,##0);_(* "-"_);_(@_)');          // 42
  AList.Add('_("'+cs+'"* #,##0.00_);_("'+cs+'"* (#,##0.00);_("'+cs+'"* "-"??_);_(@_)'); // 43
  AList.Add('_(* #,##0.00_);_(* (#,##0.00);_(* "-"??_);_(@_)');  // 44
  AList.Add('nn:ss');       // 45
  AList.Add('[h]:nn:ss');   // 46
  AList.Add('nn:ss.z');     // 47
  AList.Add('##0.0E+00');   // 48
  AList.Add('@');           // 49 "Text" format
  for i:=50 to ALastIndex do AList.Add('');  // not supported/used
end;

var
  CurrencyList: TStrings = nil;

{@@ ----------------------------------------------------------------------------
  Registers a currency symbol UTF8 string for usage by fpspreadsheet

  Currency symbols are the key for detection of currency values. In order to
  reckognize strings are currency symbols they have to be registered in the
  internal CurrencyList.

  Registration occurs automatically for USD, "$", the currencystring defined
  in the DefaultFormatSettings and for the currency symbols used explicitly
  when calling WriteCurrency or WriteNumerFormat.
-------------------------------------------------------------------------------}
procedure RegisterCurrency(ACurrencySymbol: String);
begin
  if not CurrencyRegistered(ACurrencySymbol) and (ACurrencySymbol <> '') then
    CurrencyList.Add(ACurrencySymbol);
end;

{@@ RegisterCurrencies registers the currency strings contained in the string list
  If AReplace is true, the list replaces the currently registered list.
-------------------------------------------------------------------------------}
procedure RegisterCurrencies(AList: TStrings; AReplace: Boolean);
var
  i: Integer;
begin
  if AList = nil then
    exit;

  if AReplace then CurrencyList.Clear;
  for i:=0 to AList.Count-1 do
    RegisterCurrency(AList[i]);
end;

{@@ ----------------------------------------------------------------------------
  Removes registration of a currency symbol string for usage by fpspreadsheet
-------------------------------------------------------------------------------}
procedure UnregisterCurrency(ACurrencySymbol: String);
var
  i: Integer;
begin
  i := CurrencyList.IndexOf(ACurrencySymbol);
  if i <> -1 then CurrencyList.Delete(i);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether a string is registered as valid currency symbol string
-------------------------------------------------------------------------------}
function CurrencyRegistered(ACurrencySymbol: String): Boolean;
begin
  Result := CurrencyList.IndexOf(ACurrencySymbol) <> -1;
end;

{@@ ----------------------------------------------------------------------------
  Writes all registered currency symbols to a string list
-------------------------------------------------------------------------------}
procedure GetRegisteredCurrencies(AList: TStrings);
begin
  AList.Clear;
  AList.Assign(CurrencyList);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the given number string is a negative value. In case of
  currency value, this can be indicated by brackets, or a minus sign at string
  start or end.
-------------------------------------------------------------------------------}
function IsNegative(var AText: String): Boolean;
begin
  Result := false;
  if AText = '' then
    exit;
  if (AText[1] = '(') and (AText[Length(AText)] = ')') then
  begin
    Result := true;
    Delete(AText, 1, 1);
    Delete(AText, Length(AText), 1);
    AText := Trim(AText);
  end else
  if (AText[1] = '-') then
  begin
    Result := true;
    Delete(AText, 1, 1);
    AText := Trim(AText);
  end else
  if (AText[Length(AText)] = '-') then
  begin
    Result := true;
    Delete(AText, Length(AText), 1);
    AText := Trim(AText);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Checks wheter a specified currency symbol is contained in a string, removes
  the currency symbol and returns the remaining string.
-------------------------------------------------------------------------------}
function RemoveCurrencySymbol(ACurrencySymbol: String; var AText: String): Boolean;
var
  p: Integer;
begin
  p := pos(ACurrencySymbol, AText);
  if p > 0 then
  begin
    Delete(AText, p, Length(ACurrencySymbol));
    AText := Trim(AText);
    Result := true;
  end else
    Result := false;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether a string is a number with attached currency symbol. Looks also
  for negative values in brackets.
-------------------------------------------------------------------------------}
function TryStrToCurrency(AText: String; out ANumber: Double;
  out ACurrencySymbol:String; const AFormatSettings: TFormatSettings): boolean;
var
  i: Integer;
  s: String;
  isNeg: Boolean;
begin
  Result := false;
  ANumber := 0.0;
  ACurrencySymbol := '';

  // Check the text for the presence of each known curreny symbol
  for i:= 0 to CurrencyList.Count-1 do
  begin
    // Store string in temporary variable since it will be modified
    s := AText;
    // Check for this currency sign being contained in the string, remove it if found.
    if RemoveCurrencySymbol(CurrencyList[i], s) then
    begin
      // Check for negative signs and remove them, but keep this information
      isNeg := IsNegative(s);
      // Try to convert remaining string to number
      if TryStrToFloat(s, ANumber, AFormatSettings) then begin
        // if successful: take care of negative values
        if isNeg then ANumber := -ANumber;
        ACurrencySymbol := CurrencyList[i];
        Result := true;
        exit;
      end;
    end;
  end;
end;

initialization
  // Known currency symbols
  CurrencyList := TStringList.Create;
  with TStringList(CurrencyList) do
  begin
    CaseSensitive := false;
    Duplicates := dupIgnore;
  end;
  RegisterCurrency('USD');
  RegisterCurrency('$');
  RegisterCurrency(AnsiToUTF8(DefaultFormatSettings.CurrencyString));

finalization
  FreeAndNil(CurrencyList);

end.


{
   Double Commander
   -------------------------------------------------------------------------
   Virtual terminal emulator escape codes

   Alexander Koblov, 2021

   Based on ComPort Library
     https://sourceforge.net/projects/comport
   Author:
     Dejan Crnila, 1998 - 2002
   Maintainers:
     Lars B. Dybdahl, 2003
   License:
     Public Domain
}

unit VTEmuEsc;

{$mode delphi}

interface

uses
  Classes;

type
  // terminal character result
  TEscapeResult = (erChar, erCode, erNothing);
  // terminal escape codes
  TEscapeCode = (ecUnknown, ecNotCompleted, ecCursorUp, ecCursorDown,
    ecCursorLeft, ecCursorRight, ecCursorHome, ecCursorEnd, ecCursorMove, ecCursorMoveX, ecCursorMoveY,
    ecReverseLineFeed, ecAppCursorLeft, ecAppCursorRight, ecAppCursorUp, ecAppCursorDown,
    ecAppCursorHome, ecAppCursorEnd, ecInsertKey, ecDeleteKey, ecPageUpKey, ecPageDownKey,
    ecMouseDown, ecMouseUp, ecEraseLineLeft, ecEraseLineRight, ecEraseScreenFrom,
    ecEraseLine, ecEraseScreen, ecEraseChar, ecSetTab, ecClearTab, ecClearAllTabs,
    ecIdentify, ecIdentResponse, ecQueryDevice, ecReportDeviceOK,
    ecReportDeviceFailure, ecQueryCursorPos, ecReportCursorPos,
    ecAttributes, ecSetMode, ecResetMode, ecReset,
    ecSaveCaretAndAttr, ecRestoreCaretAndAttr, ecSaveCaret, ecRestoreCaret,
    ecTest, ecFuncKey, ecSetTextParams, ecScrollRegion, ecReverseIndex);

  // terminal escape codes processor
  TEscapeCodes = class
  private
    Fcharacter: Char;
    FCode: TEscapeCode;
    FData: string;
    FParams: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    function ProcessChar(Ch: Char): TEscapeResult; virtual; abstract;
    function EscCodeToStr(Code: TEscapeCode; AParams: TStrings): string; virtual; abstract;
    function GetParam(Num: Integer; AParams: TStrings): Integer;
    property Data: string read FData;
    property Code: TEscapeCode read FCode;
    property character: Char read Fcharacter;
    property Params: TStrings read FParams;
  end;

  // VT52 escape codes
  TEscapeCodesVT52 = class(TEscapeCodes)
  private
    FInSequence: Boolean;
    function DetectCode(Str: string): TEscapeCode;
  public
    function ProcessChar(Ch: Char): TEscapeResult; override;
    function EscCodeToStr(Code: TEscapeCode; AParams: TStrings): string; override;
  end;

  // ANSI/VT100 escape codes
  TEscapeCodesVT100 = class(TEscapeCodes)
  private
    FInSequence: Boolean;
    FInExtSequence: Boolean;
    FInOscSequence: Boolean;
    function DetectCode(Str: string): TEscapeCode;
    function DetectExtCode(Str: string): TEscapeCode;
    function DetectOscCode(Str: string): TEscapeCode;
  public
    function ProcessChar(Ch: Char): TEscapeResult; override;
    function EscCodeToStr(Code: TEscapeCode; AParams: TStrings): string; override;
  end;

implementation

uses
  SysUtils;

(*****************************************
 * TEscapeCodes class                    *
 *****************************************)

constructor TEscapeCodes.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TEscapeCodes.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function TEscapeCodes.GetParam(Num: Integer; AParams: TStrings): Integer;
begin
  if (AParams = nil) or (AParams.Count < Num) then
    Result := 1
  else
    try
      Result := StrToInt(AParams[Num - 1]);
    except
      Result := 1;
    end;
end;

(*****************************************
 * TEscapeCodesVT52 class                *
 *****************************************)

// process character
function TEscapeCodesVT52.ProcessChar(Ch: Char): TEscapeResult;
var
  TempCode: TEscapeCode;
begin
  Result := erNothing;
  if not FInSequence then
  begin
    if Ch = #27 then
    begin
      FData := '';
      FInSequence := True;
    end
    else begin
      Fcharacter := Ch;
      Result := erChar;
    end;
  end else
  begin
    FData := FData + Ch;
    TempCode := DetectCode(FData);
    if TempCode <> ecNotCompleted then
    begin
      FCode := TempCode;
      FInSequence := False;
      Result := erCode;
    end;
  end;
end;

// escape code to string
function TEscapeCodesVT52.EscCodeToStr(Code: TEscapeCode; AParams: TStrings): string;
begin
  case Code of
    ecCursorUp: Result := #27'A';
    ecCursorDown: Result := #27'B';
    ecCursorRight: Result := #27'C';
    ecCursorLeft: Result := #27'D';
    ecCursorHome: Result := #27'H';
    ecReverseLineFeed: Result := #27'I';
    ecEraseScreenFrom: Result := #27'J';
    ecEraseLineRight: Result := #27'K';
    ecIdentify: Result := #27'Z';
    ecIdentResponse: Result := #27'/Z';
    ecCursorMove: Result := #27'Y' +
      Chr(GetParam(1, AParams) + 31) + Chr(GetParam(2, AParams) + 31);
  else
    Result := '';
  end;
end;

// get escape code from string
function TEscapeCodesVT52.DetectCode(Str: string): TEscapeCode;
begin
  Result := ecUnknown;
  case Str[1] of
    'A': Result := ecCursorUp;
    'B': Result := ecCursorDown;
    'C': Result := ecCursorRight;
    'D': Result := ecCursorLeft;
    'H': Result := ecCursorHome;
    'I': Result := ecReverseLineFeed;
    'J': Result := ecEraseScreenFrom;
    'K': Result := ecEraseLineRight;
    'Z': Result := ecIdentify;
    '/': begin
           if Length(Str) = 1 then
             Result := ecNotCompleted
           else
             if (Length(Str) = 2) and (Str = '/Z') then
               Result := ecIdentResponse;
         end;
    'Y': begin
           if Length(Str) < 3 then
             Result := ecNotCompleted
           else
           begin
             Result := ecCursorMove;
             FParams.Add(IntToStr(Ord(Str[3]) - 31));
             FParams.Add(IntToStr(Ord(Str[2]) - 31));
           end;
         end;
  end;
end;

(*****************************************
 * TEscapeCodesVT100class                *
 *****************************************)

// process character
function TEscapeCodesVT100.ProcessChar(Ch: Char): TEscapeResult;
var
  TempCode: TEscapeCode;
begin
  Result := erNothing;
  if not FInSequence then
  begin
    if Ch = #27 then
    begin
      FData := '';
      FInSequence := True;
    end
    else begin
      Fcharacter := Ch;
      Result := erChar;
    end;
  end else
  begin
    FData := FData + Ch;
    TempCode := ecNotCompleted;
    if FInExtSequence then
      TempCode := DetectExtCode(FData)
    else if FInOscSequence then
      TempCode := DetectOscCode(FData)
    else
      // character [ after ESC defines extended escape code
      if FData[1] = '[' then
        FInExtSequence := True
      else if FData[1] = ']' then
        FInOscSequence := True
      else
        TempCode := DetectCode(FData);
    if TempCode <> ecNotCompleted then
    begin
      FCode := TempCode;
      FInSequence := False;
      FInExtSequence := False;
      FInOscSequence := False;
      Result := erCode;
    end;
  end;
end;

// escape code to string conversion
function TEscapeCodesVT100.EscCodeToStr(Code: TEscapeCode;
  AParams: TStrings): string;
var
  AKey: Integer;
begin
  case Code of
    ecIdentify: Result := #27'[c';
    ecIdentResponse: Result := Format(#27'[?1;%dc', [GetParam(1, AParams)]);
    ecQueryCursorPos: Result := #27'[6n';
    ecReportCursorPos: Result := Format(#27'[%d;%dR', [GetParam(1, AParams), GetParam(2, AParams)]);
    ecQueryDevice: Result := #27'[5n';
    ecReportDeviceOK: Result := #27'[0n';
    ecReportDeviceFailure: Result := #27'[3n';
    ecCursorUp: Result := #27'[A';
    ecCursorDown: Result := #27'[B';
    ecCursorRight: Result := #27'[C';
    ecAppCursorLeft: Result := #27'OD';
    ecAppCursorUp: Result := #27'OA';
    ecAppCursorDown: Result := #27'OB';
    ecAppCursorRight: Result := #27'OC';
    ecAppCursorHome: Result := #27'OH';
    ecAppCursorEnd: Result := #27'OF';
    ecCursorLeft: Result := #27'[D';
    ecCursorHome: Result := #27'[H';
    ecCursorEnd: Result := #27'[F';
    ecCursorMove: Result := Format(#27'[%d;%df', [GetParam(1, AParams), GetParam(2, AParams)]);
    ecEraseScreenFrom: Result := #27'[J';
    ecEraseLineRight: Result := #27'[K';
    ecEraseScreen: Result := #27'[2J';
    ecEraseLine: Result := #27'[2K';
    ecSetTab: Result := #27'H';
    ecClearTab: Result := #27'[g';
    ecClearAllTabs: Result := #27'[3g';
    ecAttributes: Result := #27'[m'; // popravi
    ecSetMode: Result := #27'[h';
    ecResetMode: Result := #27'[l';
    ecReset: Result := #27'c';
    ecSaveCaret: Result := #27'[s';
    ecRestoreCaret: Result := #27'[u';
    ecSaveCaretAndAttr: Result := #27'7';
    ecRestoreCaretAndAttr: Result := #27'8';
    ecTest: Result := #27'#8';
    ecFuncKey:
      begin
        AKey:= GetParam(1, AParams);
        case AKey of
          0: Result := #27'OP';
          1: Result := #27'OQ';
          2: Result := #27'OR';
          3: Result := #27'OS';
          4: Result := #27'[15~';
          5: Result := #27'[17~';
          6: Result := #27'[18~';
          7: Result := #27'[19~';
          8: Result := #27'[20~';
          9: Result := #27'[21~';
         10: Result := #27'[23~';
         11: Result := #27'[24~';
        end;
      end;
    ecInsertKey: Result := #27'[2~';
    ecDeleteKey: Result := #27'[3~';
    ecPageUpKey: Result := #27'[5~';
    ecPageDownKey: Result := #27'[6~';
    ecMouseDown:
        Result := Format(#27'[<%d;%d;%dM', [GetParam(1, AParams), GetParam(2, AParams), GetParam(3, AParams)]);
    ecMouseUp:
        Result := Format(#27'[<%d;%d;%dm', [GetParam(1, AParams), GetParam(2, AParams), GetParam(3, AParams)]);
  else
    Result := '';
  end;
end;

// get vt100 escape code from string
function TEscapeCodesVT100.DetectCode(Str: string): TEscapeCode;
begin
  if Length(Str) = 1 then
    case Str[1] of
      'H': Result := ecSetTab;
      'c': Result := ecReset;
      'M': Result := ecReverseIndex;
      '7': Result := ecSaveCaretAndAttr;
      '8': Result := ecRestoreCaretAndAttr;
      '#': Result := ecNotCompleted;
      'O': Result := ecNotCompleted;
    else
      Result := ecUnknown;
    end
  else
  begin
    Result := ecUnknown;
    if Str = '#8' then
      Result := ecTest;
    if Str[1] = 'O' then
      case Str[2] of
        'A': Result := ecAppCursorUp;
        'B': Result := ecAppCursorDown;
        'C': Result := ecAppCursorRight;
        'D': Result := ecAppCursorLeft;
        'H': Result := ecAppCursorHome;
        'F': Result := ecAppCursorEnd;
      end;
  end;
end;

// get extended vt100 escape code from string
function TEscapeCodesVT100.DetectExtCode(Str: string): TEscapeCode;
var
  LastCh: Char;
  TempParams: TStrings;

  procedure ParseParams(Str: string);
  var
    I: Integer;
    TempStr: string;
  begin
    I := 1;
    TempStr := '';
    while I <= Length(Str) do
    begin
      if (Str[I] = ';') and (TempStr <> '') then
      begin
        TempParams.Add(TempStr);
        TempStr := '';
      end
      else
        TempStr := TempStr + Str[I];
      Inc(I);
    end;
    if (TempStr <> '') then
      TempParams.Add(TempStr);
  end;

  function CodeEraseScreen: TEscapeCode;
  var
    Str: string;
  begin
    if TempParams.Count = 0 then
      Result := ecEraseScreenFrom
    else
    begin
      Str := TempParams[0];
      case Str[1] of
        '0': Result := ecEraseScreenFrom;
        '2': Result := ecEraseScreen;
      else
        Result := ecUnknown;
      end;
    end;
    TempParams.Clear;
  end;

  function CodeEraseLine: TEscapeCode;
  var
    Str: string;
  begin
    if TempParams.Count = 0 then
      Result := ecEraseLineRight
    else
    begin
      Str := TempParams[0];
      case Str[1] of
        '0': Result := ecEraseLineRight;
        '1': Result := ecEraseLineLeft;
        '2': Result := ecEraseLine;
      else
        Result := ecUnknown;
      end;
    end;
    TempParams.Clear;
  end;

  function CodeTab: TEscapeCode;
  var
    Str: string;
  begin
    if TempParams.Count = 0 then
      Result := ecClearTab
    else
    begin
      Str := TempParams[0];
      case Str[1] of
        '0': Result := ecClearTab;
        '3': Result := ecClearAllTabs;
      else
        Result := ecUnknown;
      end;
    end;
    TempParams.Clear;
  end;

  function CodeDevice: TEscapeCode;
  var
    Str: string;
  begin
    if TempParams.Count = 0 then
      Result := ecUnknown
    else
    begin
      Str := TempParams[0];
      case Str[1] of
        '5': Result := ecQueryDevice;
        '0': Result := ecReportDeviceOK;
        '3': Result := ecReportDeviceFailure;
        '6': Result := ecQueryCursorPos;
      else
        Result := ecUnknown;
      end;
    end;
    TempParams.Clear;
  end;

  function CodeIdentify: TEscapeCode;
  begin
    if (TempParams.Count = 0) or
      ((TempParams.Count = 1) and (TempParams[0] = '0'))
    then
      Result := ecIdentify
    else
      if (TempParams.Count = 2) and (TempParams[1] = '?1') then
        Result := ecIdentResponse
      else
        Result := ecUnknown;
  end;

begin
  Result := ecNotCompleted;
  LastCh := Str[Length(Str)];
  {$IFDEF Unicode}  if not CharInSet(LastCh,['A'..'Z', 'a'..'z']) then  Exit;
  {$ELSE}  if not (LastCh in ['A'..'Z', 'a'..'z']) then  Exit;  {$ENDIF}
  TempParams := TStringList.Create;
  try
    ParseParams(Copy(Str, 2, Length(Str) - 2));
    case LastCh of
      'A': Result := ecCursorUp;
      'B': Result := ecCursorDown;
      'C': Result := ecCursorRight;
      'D': Result := ecCursorLeft;
      'H': Result := ecCursorHome;
      'F': Result := ecCursorEnd;
      'f': Result := ecCursorMove;
      'd': Result := ecCursorMoveY;
      'G': Result := ecCursorMoveX;
      'J': Result := CodeEraseScreen;
      'K': Result := CodeEraseLine;
      'X': Result := ecEraseChar;
      'g': Result := CodeTab;
      'm': Result := ecAttributes;
      'h': Result := ecSetMode;
      'l': Result := ecResetMode;
      's': Result := ecSaveCaret;
      'u': Result := ecRestoreCaret;
      'n': Result := CodeDevice;
      'c': Result := CodeIdentify;
      'R': Result := ecReportCursorPos;
      'r': Result := ecScrollRegion;
    else
      Result := ecUnknown;
    end;
    FParams.Assign(TempParams);
  finally
    TempParams.Free;
  end;
end;

function TEscapeCodesVT100.DetectOscCode(Str: string): TEscapeCode;
var
  LastCh: Char;
begin
  Result := ecNotCompleted;
  LastCh := Str[Length(Str)];
  if (LastCh = #7) then
  begin
    Result:= ecSetTextParams;
  end;
end;

end.

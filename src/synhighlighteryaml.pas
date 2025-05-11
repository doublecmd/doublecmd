{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynGen\yaml.pas, released 2008-06-13.
Description: YAML Syntax Parser/Highlighter
The initial author of this file is Kiriakos.
Copyright (c) 2008, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterYAML;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, SynEditHighlighter, SynEditTypes;

const
  rsUnknown = 0;
  rsValue = 1;
  rsLiteralStart = 2;
  rsLiteral = 3;
  rsString1 = 4;
  rsString2 = 5;
  rsDirective = 6;
  rsDocDelimiter = 7;

type
  TtkTokenKind = (
    tkComment,
    tkDocDelimiter,
    tkKey,
    tkNull,
    tkNumericValue,
    tkSpace,
    tkSymbol,
    tkTag,
    tkLiteral,
    tkString,
    tkAnchor,
    tkDirective,
    tkUnknown
  );

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type

  { TSynYAMLSyn }

  TSynYAMLSyn = class(TSynCustomHighlighter)
  private
    Run: LongInt;
    fLine: PChar;
    FLineStr: String;
    fRange: LongWord;
    fLineLen: Integer;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fDocDelimiterAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumericValueAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTagAttri: TSynHighlighterAttributes;
    fTextValueAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fAnchorAttri: TSynHighlighterAttributes;
    fErrorAttri: TSynHighlighterAttributes;
    fTempSpaceAttri : TSynHighlighterAttributes;
    procedure KeyProc;
    procedure LiteralProc;
    procedure LiteralMarkProc;
    procedure FoldedLiteralMarkProc;
    procedure ValueProc;
    procedure StringProc1;
    procedure StringProc2;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure ListItemProc;
    procedure TagProc;
    procedure CommentProc;
    procedure DirectiveProc;
    procedure AnchorProc;
    procedure DocDelimiterProc;
  protected
    function IsLineEnd(ARun: Integer): Boolean;
    function GetSampleSource: String; override;
    function IsFilterStored: Boolean; override;
    function GetIdentChars: TSynIdentChars; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(const Value: String; LineNumber: Integer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;

    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    procedure GetTokenEx(out TokenStart :PChar; out TokenLength :integer); override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DocDelimiterAttri: TSynHighlighterAttributes read fDocDelimiterAttri write fDocDelimiterAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumericValueAttri: TSynHighlighterAttributes read fNumericValueAttri write fNumericValueAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property TagAttri: TSynHighlighterAttributes read fTagAttri write fTagAttri;
    property TextValueAttri: TSynHighlighterAttributes read fTextValueAttri write fTextValueAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri write fDirectiveAttri;
    property AnchorAttri: TSynHighlighterAttributes read fAnchorAttri write fAnchorAttri;
    property ErrorAttri: TSynHighlighterAttributes read fErrorAttri write fErrorAttri;
  end;

const
  SYNS_XML_LangYAML = 'YAML';
  SYNS_XML_AttrAnchor = 'Anchor';
  SYNS_XML_AttrDelimiter = 'Delimiter';
  SYNS_XML_AttrTag = 'Tag';

resourcestring
  SYNS_FilterYAML = 'YAML files (*.yaml;*.yml)|*.yaml;*.yml';
  SYNS_AttrAnchor = 'Anchor';
  SYNS_AttrDelimiter = 'Delimiter';
  SYNS_AttrTag = 'Tag';

implementation

uses
  Math, SynEditStrConst;

function StrIsLeft(AText, ALeft: PChar): Boolean;
begin
  while (ALeft^ <> #0) and (AText^ <> #0) and (ALeft^ = AText^) do begin
    Inc(ALeft);
    Inc(AText);
  end;
  Result := ALeft^ = #0;
end;

function CalcIndent(S : string; TabWidth : integer = 4): integer;
Var
  i : integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = #9 then
      Inc(Result, TabWidth)
    else if S[i] = ' ' then
      Inc(Result)
    else
      break;
end;

function TSynYAMLSyn.IsLineEnd(ARun: Integer): Boolean;
begin
  Result:= FLine[ARun] in [#0, #10, #13];
end;

procedure TSynYAMLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynYAMLSyn.StringProc1;
begin
  fTokenID := tkString;
  while not IsLineEnd(Run) do begin
    if FLine[Run] = '"' then begin
      Inc(Run);
      LongRec(fRange).Lo := rsUnknown;
      Exit;
    end else if FLine[Run] = '\' then
      Inc(Run);
    Inc(Run)
  end;
  LongRec(fRange).Lo := rsString1;
end;

procedure TSynYAMLSyn.StringProc2;
begin
  fTokenID := tkString;
  while not IsLineEnd(Run) do begin
    if FLine[Run] = '''' then begin
      Inc(Run);
      if FLine[Run] <> '''' then begin
        LongRec(fRange).Lo := rsUnknown;
        Exit;
      end;
    end;
    Inc(Run)
  end;
  LongRec(fRange).Lo := rsString2;
end;

procedure TSynYAMLSyn.TagProc;
begin
  while not (IsLineEnd(Run) or (FLine[Run] <= #32)) do
    Inc(Run);
  fTokenID := tkTag;
end;

procedure TSynYAMLSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynYAMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

destructor TSynYAMLSyn.Destroy;
begin
  fTempSpaceAttri.Free;
  inherited Destroy;
end;

procedure TSynYAMLSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  while not IsLineEnd(Run) do
    Inc(Run);
  LongRec(fRange).Lo := rsUnknown;
end;

procedure TSynYAMLSyn.DocDelimiterProc;
begin
  fTokenID := tkDocDelimiter;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynYAMLSyn.SetLine(const Value: String; LineNumber: Integer);
Const
  sDocStart : String = '---';
  sDocEnd : String = '...';
Var
  NewIndent : integer;
begin
  Run := 0;
  FLineStr := Value;
  UniqueString(FLineStr);
  fLine := PChar(fLineStr);
  fLineLen := Length(fLine);
  NewIndent := CalcIndent(fLineStr);

  if LongRec(fRange).Lo = rsDocDelimiter then
   LongRec(fRange).Lo := rsUnknown;

  if fLine^ = '%' then begin
    LongRec(fRange).Lo := rsDirective;
    LongRec(fRange).Hi := NewIndent;
  end else if StrIsLeft(FLine, PChar(sDocStart)) or StrIsLeft(FLine, PChar(sDocEnd)) then begin
    LongRec(fRange).Lo := rsDocDelimiter;
    LongRec(fRange).Hi := NewIndent;
  end else if (LongRec(fRange).Lo = rsLiteralStart) then begin
    LongRec(fRange).Lo := rsLiteral;
    LongRec(fRange).Hi := NewIndent;
  end else if (LongRec(fRange).Lo = rsLiteral) then begin
    if (LongRec(fRange).Hi > NewIndent) then begin
      LongRec(fRange).Lo := rsUnknown;
      LongRec(fRange).Hi := NewIndent;
    end else
      LongRec(fRange).Hi := Min(LongRec(fRange).Hi, NewIndent);
  end else begin
    if not (LongRec(fRange).Lo in [rsString1, rsString2]) then
      LongRec(fRange).Lo := rsUnknown;
    LongRec(fRange).Hi := NewIndent;
  end;
  Next;
end;

procedure TSynYAMLSyn.FoldedLiteralMarkProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  LongRec(fRange).Lo := rsLiteralStart;
end;

procedure TSynYAMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynYAMLSyn.ListItemProc;
begin
  Inc(Run);
  if CharInSet(FLine[Run], [' ', #0, #13, #10]) then begin
    fTokenID := tkSymbol;
    LongRec(fRange).Lo := rsUnknown;
  end else
    Next;
end;

procedure TSynYAMLSyn.LiteralMarkProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '+' then
    Inc(Run)
  else
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
  LongRec(fRange).Lo := rsLiteralStart;
end;

procedure TSynYAMLSyn.LiteralProc;
begin
  fTokenID := tkLiteral;
  while (not IsLineEnd(Run)) and (FLine[Run] <> '#') do
    Inc(Run);
end;

procedure TSynYAMLSyn.AnchorProc;
begin
  while not (IsLineEnd(Run) or (FLine[Run] <= #32)) do
    Inc(Run);
  fTokenID := tkAnchor;
end;

procedure TSynYAMLSyn.CommentProc;
begin
  fTokenID := tkComment;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

constructor TSynYAMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGray;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fDocDelimiterAttri := TSynHighLighterAttributes.Create(SYNS_AttrDelimiter, SYNS_XML_AttrDelimiter);
  fDocDelimiterAttri.Style := [fsBold] ;
  fDocDelimiterAttri.Foreground := clGray;
  fDocDelimiterAttri.Background := clSilver;
  AddAttribute(fDocDelimiterAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrKey, SYNS_XML_AttrKey);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);

  fNumericValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  fNumericValueAttri.Foreground := clPurple;
  AddAttribute(fNumericValueAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Style := [fsBold];
  fSymbolAttri.Foreground := clBlue;
  AddAttribute(fSymbolAttri);

  fTagAttri := TSynHighLighterAttributes.Create(SYNS_AttrTag, SYNS_XML_AttrTag);
  fTagAttri.Foreground := clNavy;
  fTagAttri.Style := [fsBold];
  AddAttribute(fTagAttri);

  fTextValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrText, SYNS_XML_AttrText);
  AddAttribute(fTextValueAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_XML_AttrDirective);
  fDirectiveAttri.Foreground := clTeal;
  AddAttribute(fDirectiveAttri);

  fAnchorAttri := TSynHighlighterAttributes.Create(SYNS_AttrAnchor, SYNS_XML_AttrAnchor);
  fAnchorAttri.Foreground := clMaroon;
  fAnchorAttri.Style := [fsBold];
  AddAttribute(fAnchorAttri);

  fErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_XML_AttrSyntaxError);
  fErrorAttri.Foreground := clRed;
  AddAttribute(fErrorAttri);

  SetAttributesOnChange(@DefHighlightChange);
  fDefaultFilter := SYNS_FilterYAML;
  LongRec(fRange).Lo := rsUnknown;

  // for coloring doc comment background
  fTempSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
end;

procedure TSynYAMLSyn.KeyProc;
begin
  fTokenID := tkLiteral;
  LongRec(fRange).Lo := rsValue;
  while not IsLineEnd(Run) do begin
    if (FLine[Run] = ':') and (CharInSet(FLine[Run+1], [' ', #0, #13, #10])) then begin
      Inc(Run);
      if FLine[Run] = ' ' then
        Inc(Run);
      fTokenID := tkKey;
      break;
    end else
      Inc(Run);
  end;
end;

procedure TSynYAMLSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynYAMLSyn.ValueProc;
var
  Val : String;
  Start : Integer;
  FloatVal : Extended;
begin
  Start := Run;
  fTokenID := tkLiteral;
  while (not IsLineEnd(Run)) and (FLine[Run] <> '#') do
    Inc(Run);
  Val := Copy(FLineStr, Start, Run - Start + 1);
  if TryStrToFloat(Trim(Val), FloatVal) then
    fTokenId := tkNumericValue;
end;

procedure TSynYAMLSyn.Next;
begin
  fTokenPos := Run;

  case LongRec(fRange).Lo of
    rsDirective : DirectiveProc;
    rsString1 :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      else
        StringProc1;
      end;
    rsString2 :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      else
        StringProc2;
      end;
    rsDocDelimiter :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      else
        DocDelimiterProc;
      end;
    rsLiteralStart :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '#': CommentProc;
      else
        UnknownProc;
      end;
    rsLiteral :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '#': CommentProc;
      else
        LiteralProc;
      end;
    rsValue :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '#': CommentProc;
        '|': LiteralMarkProc;
        '>': FoldedLiteralMarkProc;
        '-': ListItemProc;
        '&', '*' : AnchorProc;
        '"':
          begin
            Inc(Run);
            StringProc1;
          end;
        '''':
          begin
            Inc(Run);
            StringProc2;
          end;
        #1..#9, #11, #12, #14..#32: SpaceProc;
      else
        ValueProc;
      end;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '#': CommentProc;
      '-': ListItemProc;
      '!': TagProc;
      '"':
        begin
          Inc(Run);
          StringProc1;
        end;
      '''':
        begin
          Inc(Run);
          StringProc2;
        end;
      #1..#9, #11, #12, #14..#32: SpaceProc;
    else
      KeyProc;
    end;
  end;
end;

function TSynYAMLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_WHITESPACE: 
      begin
        fTempSpaceAttri.Assign(fSpaceAttri);
        if LongRec(fRange).Lo = rsDocDelimiter then
          fTempSpaceAttri.Background := fDocDelimiterAttri.Background;
        Result := fTempSpaceAttri;
      end;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynYAMLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynYAMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynYAMLSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynYAMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynYAMLSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;

function TSynYAMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDocDelimiter: Result := fDocDelimiterAttri;
    tkKey: Result := fKeyAttri;
    tkNumericValue: Result := fNumericValueAttri;
    tkSpace: Result := fSpaceAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTag: Result := fTagAttri;
    tkLiteral: Result := fTextValueAttri;
    tkString: Result := fStringAttri;
    tkDirective: Result := fDirectiveAttri;
    tkAnchor: Result := fAnchorAttri;
    tkUnknown: Result := fErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynYAMLSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynYAMLSyn.GetSampleSource: String;
begin
  Result :=
    '---'#13#10 +
    'receipt:    Oz-Ware Purchase Invoice'#13#10 +
    'date:        2007-08-06'#13#10 +
    'customer:'#13#10 +
    '    given:   Dorothy'#13#10 +
    '    family:  Gale'#13#10 +
    #13#10 +
    'items:'#13#10 +
    '    - part_no:   A4786'#13#10 +
    '      descrip:   !!str Water Bucket (Filled)'#13#10 +
    '      price:     1.47'#13#10 +
    '      quantity:  4'#13#10 +
    #13#10 +
    '    - part_no:   E1628'#13#10 +
    '      descrip:   "High Heeled \"Ruby\" Slippers"'#13#10 +
    '      price:     100.27'#13#10 +
    '      quantity:  1'#13#10 +
    #13#10 +
    'bill-to:  &id001'#13#10 +
    '    street: |'#13#10 +
    '            123 Tornado Alley'#13#10 +
    '            Suite 16'#13#10 +
    '    city:   East Westville'#13#10 +
    '    state:  KS'#13#10 +
    #13#10 +
    'ship-to:  *id001'#13#10 +
    #13#10 +
    'specialDelivery:  >'#13#10 +
    '    Follow the Yellow Brick'#13#10 +
    '    Road to the Emerald City.'#13#10 +
    '...';
end;

function TSynYAMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterYAML;
end;

function TSynYAMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result:= ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynYAMLSyn.GetLanguageName: string;
begin
  Result := SYNS_XML_LangYAML;
end;

procedure TSynYAMLSyn.ResetRange;
begin
  LongRec(fRange).Lo := rsUnknown;
end;

procedure TSynYAMLSyn.SetRange(Value: Pointer);
begin
  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  fRange := LongWord(UIntPtr(Value));
  {$POP}
end;

function TSynYAMLSyn.GetRange: Pointer;
begin
  {$PUSH}{$HINTS OFF}
  Result := Pointer(UIntPtr(fRange));
  {$POP}
end;

initialization
  RegisterPlaceableHighlighter(TSynYAMLSyn);

end.

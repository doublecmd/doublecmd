{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterLua.pas, the Initial
Author of this file is Zhou Kan.
All Rights Reserved.

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

$Id: SynHighlighterLua.pas,v 1.00 2005/01/24 17:58:27 Kan Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Lua Script highlighter for SynEdit)
@author(Zhou Kan [textrush@tom.com])
@created(June 2004)
@lastmod(2005-01-24)
The SynHighlighterLua unit provides SynEdit with a Lua Script (*.lua) highlighter.
The highlighter formats Lua Script source code highlighting keywords, strings, numbers and characters.
}

unit SynHighlighterLua;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, SynEditHighlighter, SynEditTypes, SynEditStrConst;

type
  TtkTokenKind = (
    tkComment,
    tkFunction,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsString, rsQuoteString, rsMultilineString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 185;

type

  { TSynLuaSyn }

  TSynLuaSyn = class(TSynCustomHighlighter)
  private
    fLineRef: string;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRange: TRangeState;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func17: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func137: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func147: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func185: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure IdentProc;
    procedure NumberProc;
    procedure UnknownProc;
    procedure MinusProc;
    procedure CommentProc;
    procedure StringProc;
    procedure QuoteStringProc;
    procedure StringEndProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure GreaterProc;
    procedure LowerProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure SemiColonProc;
    procedure PointProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure PlusProc;
    procedure StarProc;
    procedure SlashProc;
    procedure ModSymbolProc;
    procedure AndSymbolProc;
    procedure NotSymbolProc;
    procedure OrSymbolProc;
    procedure TildeProc;
    procedure ArrowProc;
    procedure QuestionProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: Boolean; override;
    function GetSampleSource: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart :PChar; out TokenLength :integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    class function GetLanguageName :string; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

resourcestring
  SYNS_LangLua   = 'Lua Script';
  SYNS_FilterLua = 'Lua Script File (*.lua)|*.lua';

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z', '0'..'9': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            mHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mHashTable[I] := Ord(I) - 95;
        end;
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynLuaSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := @AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[17] := @Func17;
  fIdentFuncTable[19] := @Func19;
  fIdentFuncTable[21] := @Func21;
  fIdentFuncTable[22] := @Func22;
  fIdentFuncTable[25] := @Func25;
  fIdentFuncTable[26] := @Func26;
  fIdentFuncTable[31] := @Func31;
  fIdentFuncTable[32] := @Func32;
  fIdentFuncTable[33] := @Func33;
  fIdentFuncTable[34] := @Func34;
  fIdentFuncTable[35] := @Func35;
  fIdentFuncTable[37] := @Func37;
  fIdentFuncTable[38] := @Func38;
  fIdentFuncTable[39] := @Func39;
  fIdentFuncTable[40] := @Func40;
  fIdentFuncTable[41] := @Func41;
  fIdentFuncTable[42] := @Func42;
  fIdentFuncTable[44] := @Func44;
  fIdentFuncTable[45] := @Func45;
  fIdentFuncTable[46] := @Func46;
  fIdentFuncTable[47] := @Func47;
  fIdentFuncTable[48] := @Func48;
  fIdentFuncTable[49] := @Func49;
  fIdentFuncTable[50] := @Func50;
  fIdentFuncTable[51] := @Func51;
  fIdentFuncTable[52] := @Func52;
  fIdentFuncTable[53] := @Func53;
  fIdentFuncTable[56] := @Func56;
  fIdentFuncTable[57] := @Func57;
  fIdentFuncTable[60] := @Func60;
  fIdentFuncTable[62] := @Func62;
  fIdentFuncTable[63] := @Func63;
  fIdentFuncTable[66] := @Func66;
  fIdentFuncTable[67] := @Func67;
  fIdentFuncTable[70] := @Func70;
  fIdentFuncTable[71] := @Func71;
  fIdentFuncTable[73] := @Func73;
  fIdentFuncTable[74] := @Func74;
  fIdentFuncTable[75] := @Func75;
  fIdentFuncTable[76] := @Func76;
  fIdentFuncTable[78] := @Func78;
  fIdentFuncTable[79] := @Func79;
  fIdentFuncTable[80] := @Func80;
  fIdentFuncTable[81] := @Func81;
  fIdentFuncTable[82] := @Func82;
  fIdentFuncTable[83] := @Func83;
  fIdentFuncTable[84] := @Func84;
  fIdentFuncTable[88] := @Func88;
  fIdentFuncTable[89] := @Func89;
  fIdentFuncTable[90] := @Func90;
  fIdentFuncTable[92] := @Func92;
  fIdentFuncTable[94] := @Func94;
  fIdentFuncTable[95] := @Func95;
  fIdentFuncTable[97] := @Func97;
  fIdentFuncTable[99] := @Func99;
  fIdentFuncTable[101] := @Func101;
  fIdentFuncTable[102] := @Func102;
  fIdentFuncTable[105] := @Func105;
  fIdentFuncTable[107] := @Func107;
  fIdentFuncTable[108] := @Func108;
  fIdentFuncTable[110] := @Func110;
  fIdentFuncTable[111] := @Func111;
  fIdentFuncTable[112] := @Func112;
  fIdentFuncTable[113] := @Func113;
  fIdentFuncTable[114] := @Func114;
  fIdentFuncTable[116] := @Func116;
  fIdentFuncTable[117] := @Func117;
  fIdentFuncTable[125] := @Func125;
  fIdentFuncTable[130] := @Func130;
  fIdentFuncTable[132] := @Func132;
  fIdentFuncTable[135] := @Func135;
  fIdentFuncTable[137] := @Func137;
  fIdentFuncTable[138] := @Func138;
  fIdentFuncTable[141] := @Func141;
  fIdentFuncTable[143] := @Func143;
  fIdentFuncTable[144] := @Func144;
  fIdentFuncTable[147] := @Func147;
  fIdentFuncTable[149] := @Func149;
  fIdentFuncTable[185] := @Func185;
end;

function TSynLuaSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z', '0'..'9'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynLuaSyn.KeyComp(const aKey :string) :Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

function TSynLuaSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func19: TtkTokenKind;
begin
  if KeyComp('deg') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func21: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func22: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func25: TtkTokenKind;
begin
  if KeyComp('PI') then Result := tkFunction else
    if KeyComp('abs') then Result := tkFunction else
      if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func26: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else
    if KeyComp('rad') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func31: TtkTokenKind;
begin
  if KeyComp('tag') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func32: TtkTokenKind;
begin
  if KeyComp('read') then Result := tkFunction else
    if KeyComp('call') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func33: TtkTokenKind;
begin
  if KeyComp('ceil') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func34: TtkTokenKind;
begin
  if KeyComp('date') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func35: TtkTokenKind;
begin
  if KeyComp('mod') then Result := tkFunction else
    if KeyComp('or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func37: TtkTokenKind;
begin
  if KeyComp('log') then Result := tkFunction else
    if KeyComp('log10') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func38: TtkTokenKind;
begin
  if KeyComp('nil') then Result := tkKey else
    if KeyComp('tan') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func39: TtkTokenKind;
begin
  if KeyComp('min') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func40: TtkTokenKind;
begin
  if KeyComp('atan') then Result := tkFunction else
    if KeyComp('cos') then Result := tkFunction else
      if KeyComp('atan2') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func41: TtkTokenKind;
begin
  if KeyComp('max') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func42: TtkTokenKind;
begin
  if KeyComp('break') then Result := tkKey else
    if KeyComp('for') then Result := tkKey else
      if KeyComp('acos') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func44: TtkTokenKind;
begin
  if KeyComp('debug') then Result := tkFunction else
    if KeyComp('seek') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else
    if KeyComp('sin') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func46: TtkTokenKind;
begin
  if KeyComp('ascii') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func47: TtkTokenKind;
begin
  if KeyComp('asin') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func48: TtkTokenKind;
begin
  if KeyComp('local') then Result := tkKey else
    if KeyComp('exp') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func49: TtkTokenKind;
begin
  if KeyComp('clock') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func50: TtkTokenKind;
begin
  if KeyComp('getn') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func51: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func52: TtkTokenKind;
begin
  if KeyComp('not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func53: TtkTokenKind;
begin
  if KeyComp('gsub') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func56: TtkTokenKind;
begin
  if KeyComp('_ALERT') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func57: TtkTokenKind;
begin
  if KeyComp('dofile') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func60: TtkTokenKind;
begin
  if KeyComp('gcinfo') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func62: TtkTokenKind;
begin
  if KeyComp('elseif') then Result := tkKey else
    if KeyComp('exit') then Result := tkFunction else
      if KeyComp('while') then Result := tkKey else
        if KeyComp('rename') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func63: TtkTokenKind;
begin
  if KeyComp('foreach') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func66: TtkTokenKind;
begin
  if KeyComp('_STDIN') then Result := tkFunction else
    if KeyComp('ldexp') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func67: TtkTokenKind;
begin
  if KeyComp('next') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func70: TtkTokenKind;
begin
  if KeyComp('type') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func71: TtkTokenKind;
begin
  if KeyComp('random') then Result := tkFunction else
    if KeyComp('repeat') then Result := tkKey else
      if KeyComp('floor') then Result := tkFunction else
        if KeyComp('flush') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func73: TtkTokenKind;
begin
  if KeyComp('foreachi') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func74: TtkTokenKind;
begin
  if KeyComp('frexp') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func75: TtkTokenKind;
begin
  if KeyComp('globals') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func76: TtkTokenKind;
begin
  if KeyComp('newtag') then Result := tkFunction else
    if KeyComp('sort') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func78: TtkTokenKind;
begin
  if KeyComp('sqrt') then Result := tkFunction else
    if KeyComp('settag') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func79: TtkTokenKind;
begin
  if KeyComp('format') then Result := tkFunction else
    if KeyComp('getenv') then Result := tkFunction else
      if KeyComp('error') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func80: TtkTokenKind;
begin
  if KeyComp('_INPUT') then Result := tkFunction else
    if KeyComp('write') then Result := tkFunction else
      if KeyComp('rawget') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func81: TtkTokenKind;
begin
  if KeyComp('until') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func82: TtkTokenKind;
begin
  if KeyComp('print') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func83: TtkTokenKind;
begin
  if KeyComp('getinfo') then Result := tkFunction else
    if KeyComp('getlocal') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func84: TtkTokenKind;
begin
  if KeyComp('_STDERR') then Result := tkFunction else
    if KeyComp('getargs') then Result := tkFunction else
      if KeyComp('remove') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func88: TtkTokenKind;
begin
  if KeyComp('assert') then Result := tkFunction else
    if KeyComp('readfrom') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func89: TtkTokenKind;
begin
  if KeyComp('tmpname') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func90: TtkTokenKind;
begin
  if KeyComp('execute') then Result := tkFunction else
    if KeyComp('openfile') then Result := tkFunction else
      if KeyComp('getglobal') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func92: TtkTokenKind;
begin
  if KeyComp('rawset') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func94: TtkTokenKind;
begin
  if KeyComp('strchar') then Result := tkFunction else
    if KeyComp('strlen') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func95: TtkTokenKind;
begin
  if KeyComp('setlocal') then Result := tkFunction else
    if KeyComp('closefile') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func97: TtkTokenKind;
begin
  if KeyComp('strfind') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func99: TtkTokenKind;
begin
  if KeyComp('_STDOUT') then Result := tkFunction else
    if KeyComp('appendto') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func101: TtkTokenKind;
begin
  if KeyComp('setlocale') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func102: TtkTokenKind;
begin
  if KeyComp('setglobal') then Result := tkFunction else
    if KeyComp('return') then Result := tkKey else
      if KeyComp('strrep') then Result := tkFunction else
        if KeyComp('_VERSION') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func105: TtkTokenKind;
begin
  if KeyComp('strsub') then Result := tkFunction else
    if KeyComp('tremove') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func107: TtkTokenKind;
begin
  if KeyComp('foreachvar') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func108: TtkTokenKind;
begin
  if KeyComp('randomseed') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func110: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey else Result := tkIdentifier;
end;

function TSynLuaSyn.Func111: TtkTokenKind;
begin
  if KeyComp('nextvar') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func112: TtkTokenKind;
begin
  if KeyComp('tinsert') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func113: TtkTokenKind;
begin
  if KeyComp('_OUTPUT') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func114: TtkTokenKind;
begin
  if KeyComp('dostring') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func116: TtkTokenKind;
begin
  if KeyComp('tonumber') then Result := tkFunction else
    if KeyComp('strbyte') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func117: TtkTokenKind;
begin
  if KeyComp('writeto') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func125: TtkTokenKind;
begin
  if KeyComp('rawgettable') then Result := tkFunction else
    if KeyComp('collectgarbage') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func130: TtkTokenKind;
begin
  if KeyComp('tostring') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func132: TtkTokenKind;
begin
  if KeyComp('setcallhook') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func135: TtkTokenKind;
begin
  if KeyComp('rawgetglobal') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func137: TtkTokenKind;
begin
  if KeyComp('gettagmethod') then Result := tkFunction else
    if KeyComp('rawsettable') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func138: TtkTokenKind;
begin
  if KeyComp('strlower') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func141: TtkTokenKind;
begin
  if KeyComp('strupper') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func143: TtkTokenKind;
begin
  if KeyComp('_ERRORMESSAGE') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func144: TtkTokenKind;
begin
  if KeyComp('setlinehook') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func147: TtkTokenKind;
begin
  if KeyComp('rawsetglobal') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func149: TtkTokenKind;
begin
  if KeyComp('settagmethod') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.Func185: TtkTokenKind;
begin
  if KeyComp('copytagmethods') then Result := tkFunction else Result := tkIdentifier;
end;

function TSynLuaSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynLuaSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]()
  else
    Result := tkIdentifier;
end;

procedure TSynLuaSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := @NullProc;
      #10: fProcTable[I] := @LFProc;
      #13: fProcTable[I] := @CRProc;
      #1..#9, #11, #12, #14..#32 : fProcTable[I] := @SpaceProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '''': fProcTable[I] := @StringProc;
      '"': fProcTable[I]  := @QuoteStringProc;
      '-': fProcTable[I]  := @MinusProc;
      '}': fProcTable[I]  := @BraceCloseProc;
      '{': fProcTable[I]  := @BraceOpenProc;
      '>': fProcTable[I]  := @GreaterProc;
      '<': fProcTable[I]  := @LowerProc;
      ')': fProcTable[I]  := @RoundCloseProc;
      '(': fProcTable[I]  := @RoundOpenProc;
      ']': fProcTable[I]  := @SquareCloseProc;
      '[': fProcTable[I]  := @SquareOpenProc;
      ':': fProcTable[I]  := @ColonProc;
      ',': fProcTable[I]  := @CommaProc;
      ';': fProcTable[I]  := @SemiColonProc;
      '.': fProcTable[I]  := @PointProc;
      '#': fProcTable[I]  := @DirectiveProc;
      '=': fProcTable[I]  := @EqualProc;
      '+': fProcTable[I]  := @PlusProc;
      '*': fProcTable[I]  := @StarProc;
      '/': fProcTable[I]  := @SlashProc;
      '%': fProcTable[I]  := @ModSymbolProc;
      '&': fProcTable[I]  := @AndSymbolProc;
      '!': fProcTable[I]  := @NotSymbolProc;
      '|': fProcTable[I]  := @OrSymbolProc;
      '~': fProcTable[I]  := @TildeProc;
      '^': fProcTable[I]  := @ArrowProc;
      '?': fProcTable[I]  := @QuestionProc;
    else
      fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynLuaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fFunctionAttri := TSynHighLighterAttributes.Create(@SYNS_AttrFunction, SYNS_XML_AttrFunction);
  fFunctionAttri.Foreground   := $00C05000;
  AddAttribute(fFunctionAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  fIdentifierAttri.Foreground := clWindowText;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Foreground     := clBlue;
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  fNumberAttri.Foreground  := clPurple;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground  := clMaroon;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Foreground  := clNavy;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(@DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterLua;
end;

procedure TSynLuaSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynLuaSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynLuaSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynLuaSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynLuaSyn.MinusProc;
begin
  case fLine[Run + 1] of
    '-':
    begin
      fTokenID := tkComment;
      if (StrComp(fLine + Run + 2, '[[--') = 0) then
      begin
        fRange := rsComment;
        Inc(Run, 6);
      end
      else repeat
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
    '=', '>':
    begin
      Inc(Run, 2);
      fTokenID := tkSymbol;
    end
    else
    begin
      fTokenID := tkSymbol;
      Inc(Run);              {subtract}
    end;
  end;
end;

procedure TSynLuaSyn.CommentProc;
begin
  case FLine[Run] of
    #0:  NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      fTokenID := tkComment;
      repeat
        if (StrComp(fLine + Run, '--]]--') = 0) then
        begin
          fRange := rsUnKnown;
          Inc(Run, 6);
          Break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynLuaSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then
    begin
      if fLine[Run + 1] in [#39, '\'] then
        Inc(Run);
    end;
    Inc(Run);
  until fLine[Run] in [#0, #10, #13, #39];
  if fLine[Run] = #39 then
    Inc(Run);
end;

procedure TSynLuaSyn.QuoteStringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then 
    begin
      case fLine[Run + 1] of
        #34, '\':
          Inc(Run);
        #00:
        begin
          Inc(Run);
          fRange := rsMultilineString;
          Exit;
        end;
      end;
    end;
    Inc(Run);
  until fLine[Run] in [#0, #10, #13, #34];
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynLuaSyn.StringEndProc;
begin
  fTokenID := tkString;

  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  fRange := rsUnknown;

  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '\':
      begin
        case fLine[Run + 1] of
          #34, '\':
            Inc(Run);
          #00:
          begin
            Inc(Run);
            fRange := rsMultilineString;
            Exit;
          end;
        end;
      end;
      #34: Break;
    end;
    Inc(Run);
  until fLine[Run] in [#0, #10, #13, #34];
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynLuaSyn.BraceCloseProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynLuaSyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynLuaSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                {greater than or equal to}
    '>':
    begin
      if FLine[Run + 2] = '=' then   {shift right assign}
        Inc(Run, 3)
      else                           {shift right}
        Inc(Run, 2);
    end;
    else                             {greater than}
      Inc(run);
  end;
end;

procedure TSynLuaSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);               {less than or equal to}
    '<':
    begin
      if FLine[Run + 2] = '=' then  {shift left assign}
        Inc(Run, 3)
      else                          {shift left}
        Inc(Run, 2);
    end;
    else Inc(Run);                  {less than}
  end;
end;

procedure TSynLuaSyn.RoundCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynLuaSyn.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    ':': Inc(Run, 2); {scope resolution operator}
    else              {colon}
      Inc(Run);
  end;
end;

procedure TSynLuaSyn.CommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.SemiColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then {ellipse}
    Inc(Run, 3)
  else if FLine[Run + 1] in ['0'..'9'] then // float
  begin
    Dec(Run); // numberproc must see the point
    NumberProc;
  end
  else                                 {point}
    Inc(Run);
end;

procedure TSynLuaSyn.DirectiveProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLuaSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2); {logical equal}
    else              {assign}
      Inc(Run);
  end;
end;

procedure TSynLuaSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do Inc(Run);
end;

procedure TSynLuaSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);    {add assign}
    '+': Inc(Run, 2);    {increment}
    else                 {add}
      Inc(Run);
  end;
end;

procedure TSynLuaSyn.StarProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                              {multiply assign}
    else Inc(Run);                                 {star}
  end;
end;

procedure TSynLuaSyn.SlashProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                              {multiply assign}
    else Inc(Run);                                 {star}
  end;
end;

procedure TSynLuaSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                              {mod assign}
    else Inc(Run);                                 {mod}
  end;
end;

procedure TSynLuaSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2); // and assign
    '&': Inc(Run, 2); // logical and
    else Inc(Run);    // and
  end;
end;

procedure TSynLuaSyn.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                              {not equal}
    else Inc(Run);                                 {not}
  end;
end;

procedure TSynLuaSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                              {or assign}
    '|': Inc(Run, 2);                              {logical or}
    else Inc(Run);                                 {or}
  end;
end;

procedure TSynLuaSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
end;

procedure TSynLuaSyn.ArrowProc;
begin
  Inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
end;

procedure TSynLuaSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  Inc(Run);
end;

procedure TSynLuaSyn.NumberProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
    ['0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F'] do //Kan
    //['0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X'] do //Commented by Kan
  begin
    case FLine[Run] of
      '.': if FLine[Run + 1] = '.' then break;
    end;
    Inc(Run);
  end;
end;

procedure TSynLuaSyn.UnknownProc;
begin
  Inc(Run);
  while (fLine[Run] in [#128..#191]) or // continued utf8 subcode
   ((fLine[Run] <> #0) and (fProcTable[fLine[Run]] = @UnknownProc)) do Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynLuaSyn.SetLine(const NewValue :String; LineNumber :Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynLuaSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: CommentProc();
    rsMultilineString: StringEndProc;
    else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

class function TSynLuaSyn.GetLanguageName :string;
begin
  Result := SYNS_LangLua;
end;

function TSynLuaSyn.GetDefaultAttribute(Index :integer) :TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
    SYN_ATTR_SYMBOL     : Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynLuaSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynLuaSyn.GetTokenEx(out TokenStart :PChar; out TokenLength :integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;

function TSynLuaSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLuaSyn.GetTokenAttribute :TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynLuaSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynLuaSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z', '0'..'9'];
end;

function TSynLuaSyn.IsFilterStored :Boolean;
begin
  Result := (fDefaultFilter <> SYNS_FilterLua);
end;

function TSynLuaSyn.GetSampleSource: String;
begin
  Result:= '-- Sample comment'#13 +
           'local str = "String"'#13 +
           'a = {}'#13 +
           'local x = 20'#13 +
           'for i = 1,10 do'#13 +
           '  local y = 0'#13 +
           '  a[i] = function () y = y + 1; return x + y end'#13 +
           'end'#13 +
           'print(str)';
end;

procedure TSynLuaSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynLuaSyn.SetRange(Value: Pointer);
begin
  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  fRange := TRangeState(PtrInt(Value));
  {$POP}
end;

function TSynLuaSyn.GetRange: Pointer;
begin
  {$PUSH}{$HINTS OFF}
  Result := Pointer(PtrInt(fRange));
  {$POP}
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynLuaSyn);

end.

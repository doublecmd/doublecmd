{
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SynUniHighlighter.pas, released 2003-01
  All Rights Reserved.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

}{
  @abstract(Provides a universal highlighter for SynEdit)
  @authors(Fantasist [walking_in_the_sky@yahoo.com], Vit [nevzorov@yahoo.com],
           Vitalik [vetal-x@mail.ru])
  @created(2003)
  @lastmod(2004-05-12)
}

unit SynUniRules;

interface

uses
  SysUtils, Graphics,
  Classes, SynEditHighlighter, SynUniClasses, Laz2_DOM;

type
  TSynRange = class;
  TSynSet = class; //Vitalik 2004

  TAbstractSymbol = class
    function GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean; virtual; abstract;
  end;

  TSymbols = class(TAbstractSymbol)
    HeadNode: TSymbolNode;
    SynSets: TList; //Vitalik 2004
    function GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean; override;
    function FindSymbol(st: string): TSymbolNode;
    procedure AddSymbol(st: string; tkSynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType);
    procedure AddSet(SymbolSet: TSynSet); //Vitalik 2004
    constructor Create(ch: char; tkSynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType); reintroduce; overload; virtual;
    constructor Create(SymbolSet: TSynSet); reintroduce; overload; virtual; //Vitalik 2004
    destructor Destroy(); override;
  end;

  TDefaultSymbols = class(TAbstractSymbol)
     tkSynSymbol: TSynSymbol;
     function GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean; override;
     constructor Create(SynSymb: TSynSymbol); reintroduce; virtual;
     destructor Destroy(); override;
  end;

  TDefaultTermSymbols = class(TAbstractSymbol)
     tkSynSymbol: TSynSymbol;
     function GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean; override;
     constructor Create(SynSymb: TSynSymbol); virtual;
     destructor Destroy(); override;
  end;

  TSynKeyList = class (TSynRule)
    KeyList: TStringList;
    constructor Create(st: string = '');
    destructor Destroy(); override;
    procedure LoadHglFromXml(xml: TDOMNode; SchCount,SchIndex: integer); //Vitalik 2004
    procedure LoadFromXml(xml: TDOMNode); override; //Vitalik 2004
    procedure SaveToStream(StreamWriter: TStreamWriter; Ind: integer = 0); overload; override; //Vitalik 2004
  end;

  TSynKeyListLink = class(TAbstractRule) //Vitalik 2004
    KeyList: TSynKeyList;
  end;

  TSynSet = class (TSynRule) //Vitalik 2004
    SymbSet: TSymbSet;
    StartType: TSymbStartType;
    BrakeType: TSymbBrakeType;
    constructor Create(aSymbSet: TSymbSet = []);
    destructor Destroy(); override;
    procedure LoadHglFromXml(xml: TDOMNode; SchCount,SchIndex: integer);
    procedure LoadFromXml(xml: TDOMNode); override;
    procedure SaveToStream(StreamWriter: TStreamWriter; Ind: integer = 0); overload; override;
  end;

  TSynSetLink = class(TAbstractRule) //Vitalik 2004
    SynSet: TSynSet;
  end;

  TSynRangeLink = class(TAbstractRule) //Vitalik 2004
    Range: TSynRange;
    Parent: TSynRange;
    constructor Create(aRange: TSynRange); virtual;
  end;

  TSynRangeRule = class
    fCloseSymbol: TSynSymbol;
    fOpenSymbol: TSynSymbol;
    fCloseOnTerm: boolean;
    fCloseOnEol: boolean;
    fAllowPredClose: boolean; //Vitalik 2004
    constructor Create(OpenSymbs: string = ''; CloseSymbs: string = '');
    destructor Destroy(); override;
  end;

  TSynRange = class (TSynRule)
  private
    fCaseSensitive: boolean;
    fOwner: TSynRange;

    fSynSymbols: TList;
    fSynRanges: TList;
    fSynKeyLists: TList;
    fSynSets: TList; //Vitalik 2004

    StringCaseFunct: function (const st: string): string;
    fPrepared: boolean;

  public {temp}
    OpenCount: integer;
    ParentBackup: TSynRange;

    fRule: TSynRangeRule;
    fClosingSymbol: TSynSymbol;

    fDefaultSynSymbol: TSynSymbol;
    fDefaultSymbols: TDefaultSymbols;
    fDefaultTermSymbol: TDefaultTermSymbols;

    fCommonSynRanges: TList;
    fSynRangeLinks: TList;

    CaseFunct: function (ch: char): char;
    fTermSymbols: TSymbSet;
    HasNodeAnyStart: array[char] of boolean; //Vitalik 2004
    SymbolList: array[char] of TAbstractSymbol;
  private
    function GetSynSymbol(Index: Integer): TSynSymbol;
    function GetCommonSynRange(Index: Integer): TSynRange;
    function GetSynRangeLink(Index: Integer): TSynRangeLink;
    function GetSynRange(Index: Integer): TSynRange;
    function GetSynKeyList(Index: Integer): TSynKeyList;
    function GetSynSet(Index: Integer): TSynSet; //Vitalik 2004

    function GetSynSymbolCount(): Integer;
    function GetCommonSynRangeCount(): Integer;
    function GetSynRangeLinkCount(): Integer;
    function GetSynRangeCount(): Integer;
    function GetSynKeyListCount(): Integer;
    function GetSynSetCount(): Integer; //Vitalik 2004

    function GetCaseSensitive: boolean;
    procedure SetCaseSensitive(const Value: boolean);

  public {temp}
    procedure LoadHglFromXml(xml: TDOMNode; SchCount, SchIndex: integer); //Vitalik 2004
    procedure LoadFromXml(xml: TDOMNode); override; //Vitalik 2004
    procedure SaveToStream(StreamWriter: TStreamWriter; Ind: integer = 0); overload; override; //Vitalik 2004
  public
    constructor Create(OpenSymbs: string = ''; CloseSymbs: string = ''); virtual;
    destructor Destroy(); override;

    procedure AddSynSymbol(NewSymb: TSynSymbol);
    procedure AddRule(NewRule: TSynRule);
    procedure AddCommonRange(Range: TSynRange);
    procedure AddRangeLink(NewRangeLink: TSynRangeLink); overload;
    function AddRangeLink(aRange: TSynRange; aName: string; aColor: TColor): TSynRangeLink; overload;
    procedure AddRange(NewRange: TSynRange); overload;
    function AddRange(aOpen, aClose, aName: string; aColor: TColor): TSynRange; overload;
    procedure AddKeyList(NewKeyList: TSynKeyList); overload;
    function AddKeyList(aName: string; aColor: TColor): TSynKeyList; overload;
    procedure AddSet(NewSet: TSynSet); overload; //Vitalik 2004
    function AddSet(aName: string; aSymbSet: TSymbSet; aColor: TColor): TSynSet; overload;//Vitalik 2004

    function FindSymbol(st: string): TSynSymbol;
    function FindSymbolOwner(Symbol: TSynSymbol): TSynKeyList;

    procedure DeleteCommonRange(index: integer); overload;
    procedure DeleteCommonRange(Range: TSynRange); overload;
    procedure DeleteRangeLink(index: integer); overload;
    procedure DeleteRangeLink(RangeLink: TSynRangeLink); overload;
    procedure DeleteRange(index: integer); overload;
    procedure DeleteRange(Range: TSynRange); overload;
    procedure DeleteKeyList(index: integer); overload;
    procedure DeleteKeyList(KeyList: TSynKeyList); overload;
    procedure DeleteSet(index: integer); overload; //Vitalik 2004
    procedure DeleteSet(SynSet: TSynSet); overload; //Vitalik 2004

{    procedure SetParentColor;
    procedure RestoreOldColor; }
    procedure SetDelimiters(Delimiters: TSymbSet);
//    procedure SetStyles(aStyles: TSynUniStyles);    
    procedure SetColorForChilds(); //Vitalik 2004

    procedure ClearParsingFields();
    procedure ResetParents(aParent: TSynRange);    
    procedure Prepare(Owner: TSynRange);
    procedure Reset();
    procedure Clear();

    function  FindRange(const Name: string): TSynRange;
    procedure LoadHglFromStream(aSrc: TStream);
  public
    property TermSymbols: TSymbSet read fTermSymbols write fTermSymbols;
//    property OpenSymbol: TSynSymbol read fOpenSymbol;
//    property CloseSymbol: TSynSymbol read fCloseSymbol;
//    property CloseOnTerm: boolean read fCloseOnTerm write fCloseOnTerm;
//    property CloseOnEol: boolean read fCloseOnEol write fCloseOnEol;
//    property AllowPredClose: boolean read fAllowPredClose write fAllowPredClose; //Vitalik 2004

    property CommonRanges[index: integer]: TSynRange read GetCommonSynRange;
    property CommonRangeCount: integer read GetCommonSynRangeCount;
    property RangeLinks[index: integer]: TSynRangeLink read GetSynRangeLink;
    property RangeLinkCount: integer read GetSynRangeLinkCount;
    property Ranges[index: integer]: TSynRange read GetSynRange;
    property RangeCount: integer read GetSynRangeCount;
    property Symbols[index: integer]: TSynSymbol read GetSynSymbol;
    property SymbolCount: integer read GetSynSymbolCount;
    property KeyLists[index: integer]: TSynKeyList read GetSynKeyList;
    property KeyListCount: Integer read GetSynKeyListCount;
    property Sets[index: integer]: TSynSet read GetSynSet; //Vitalik 2004
    property SetCount: Integer read GetSynSetCount; //Vitalik 2004

    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    property Prepared: boolean read fPrepared;
    property Parent: TSynRange read fOwner write fOwner;
  end;

//function Verify(tag: string; xml: TXMLParser): boolean; overload;

const
  DefaultTermSymbols: TSymbSet = ['*','/','+','-','=','\','|','&','(',')',
    '[',']','{','}','`','~','!','@',',','$','%','^','?',':',';','''','"','.',
    '>','<','#'];

implementation

uses
  Laz2_XMLRead;

function CaseNone(ch: char): char; //: Need for CaseSensitive
begin
  Result := ch;
end;

function StringCaseNone(const st: string): string; //: Need for CaseSensitive
begin
  Result := st;
end;

//==== TSymbols ==============================================================
procedure TSymbols.AddSymbol(st: string; tkSynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType);
//: Add SynSymbol to the tree Symbols
var
  i: integer;
  l: integer;
  Node: TSymbolNode;
  SList: TSymbolList;
begin
  SList := HeadNode.NextSymbs;      //: All branches of current node (first - root node)
  Node := nil;                      //: Current Node
  l := Length(st);                   //: Length of adding string
  for i := 1 to l do                //: Check all symbols of adding string
  begin
    Node := SList.FindSymbol(st[i]);     //: Try to find current symbol of adding string among branches
    if Node = nil then                  //: If we can't find current symbol
    begin
      Node := TSymbolNode.Create(st[i]);     //: then create node with current symbol
      SList.AddSymbol(Node);                //: and add it to current branches
    end;
    SList := Node.NextSymbs;            //: Go to finded or added node
  end;
  Node.StartType := tkSynSymbol.StartType;
  Node.BrakeType := ABrakeType;     //: Set Break Type and ...
  Node.tkSynSymbol := tkSynSymbol;  //: ... SynSymbol of last Node
end;

constructor TSymbols.Create(ch: char; tkSynSymbol: TSynSymbol;
  ABrakeType: TSymbBrakeType);
begin
  HeadNode := TSymbolNode.Create(ch, tkSynSymbol, ABrakeType);
  SynSets := TList.Create;
end;

constructor TSymbols.Create(SymbolSet: TSynSet);
begin
  SynSets := TList.Create;
  AddSet(SymbolSet);
end;

destructor TSymbols.Destroy;
begin
  if Assigned(HeadNode) then
    HeadNode.Free;
  FreeList(SynSets);
  inherited;
end;

function TSymbols.FindSymbol(st: string): TSymbolNode;
//: Find string st in the tree Symbols
var
  i: integer;
  l: integer;
  Node, prvNode: TSymbolNode;
begin
  Node := HeadNode;   //: Root of the tree
  l := Length(st);     //: Length of string
  for i := 1 to l do
  begin
    prvNode := Node.NextSymbs.FindSymbol(st[i]);
    if prvNode = nil then     //: If don't find
      break;                  //: Exit from cycle
    Node := prvNode;          //: Else go to the brench or nil, if don't find
  end;
  Result := Node;     //: Return node, if found, and nil, if not found
end;

procedure TSymbols.AddSet(SymbolSet: TSynSet);// ABrakeType: TSymbBrakeType);
begin
  SynSets.Add(SymbolSet);
end;

function TSymbols.GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean;
//: Try to find any token
var
  curNode, nxtStart, prevFind: TSymbolNode;
  i, posStart, posNext, posPrev: integer;
  AllowedTermSymbols: TSymbSet;

  function CanBeToken(): boolean;
  var i: integer;
  begin
    CanBeToken := True;
    if curNode.tkSynSymbol = nil then
      CanBeToken := False
    else if (curNode.BrakeType = btTerm) and not (fLine[succ(Run)] in CurRule.fTermSymbols) then
      CanBeToken := False
    else
      case curNode.tkSynSymbol.StartLine of
        slFirstNonSpace:
          for i := 0 to posStart-1 do
            {$IFNDEF FPC}
            if not (fLine[i] in [' ', #32, #9]) then begin
            {$ELSE}
            if not (fLine[i] in [#32, #9]) then begin
            {$ENDIF}
              CanBeToken := False;
              break;
            end;
        slFirst:
          if posStart <> 0 then
            CanBeToken := False;
      end;
  end;

begin //Vitalik 2004
  Result := False;
  posStart := Run;
  if Assigned(HeadNode) then begin
    curNode := HeadNode;
    posNext := posStart;
    nxtStart := nil;
    repeat
      if nxtStart <> nil then begin
        curNode := nxtStart;
        Run := posNext;
        nxtStart := nil;
      end;
      if CanBeToken then
        prevFind := curNode
      else
        prevFind := nil;
      posPrev := Run;
      while (curNode.NextSymbs.Count > 0) and (fLine[Run] <> #0) do begin
        inc(Run);
        curNode := curNode.NextSymbs.FindSymbol(CurRule.CaseFunct(fLine[Run]));
        if curNode = nil then begin
          dec(Run);
          break;
        end;

        if CanBeToken then begin
          prevFind := curNode;
          posPrev := Run;
        end;

        if nxtStart = nil then
         if (CurRule.HasNodeAnyStart[CurRule.CaseFunct(curNode.ch)] or
            (curNode.ch in CurRule.fTermSymbols) or
            (CurRule.CaseFunct(fLine[Run]) in CurRule.fTermSymbols)) then begin
           nxtStart := curNode;
           posNext := Run;
         end;
      end;

      Run := posPrev;

      if prevFind = nil then
        continue;
      if prevFind.tkSynSymbol = nil then
        continue; //Never happened???

      if fLine[Run] <> #0 then                                                     //: Go to next symbol in line if it isn't end of line
        inc(Run);

      if prevFind.BrakeType = btAny then begin                                      //: If token can end by any symbol
        Result := True;                          //: We find it!
        tkSynSymbol := prevFind.tkSynSymbol;      //: Here it is!
        Exit;
      end;

      if fLine[Run] in CurRule.fTermSymbols then begin                        //: If token can end by delimeter and current symbol is delimeter
        Result := True;                      //: We find it!
        tkSynSymbol := prevFind.tkSynSymbol;  //: Here it is!
        Exit;
      end;
    until nxtStart = nil;
  end;
//l1:
{begin}
  Run := posStart;
//  Result := False;
  AllowedTermSymbols := CurRule.fTermSymbols;
  for i := 0 to SynSets.Count-1 do begin
    AllowedTermSymbols := AllowedTermSymbols - TSynSet(SynSets[i]).SymbSet;
  end;

  for i := 0 to SynSets.Count-1 do begin
    Run := posStart;
    repeat
      inc(Run);
    until not (fLine[Run] in TSynSet(SynSets[i]).SymbSet) or (fLine[Run] = #0);
    //: If number ends on some Term-symbol, then
    if TSynSet(SynSets[i]).BrakeType = btAny then begin
      Result := True;                   //: We find it!
      tkSynSymbol := TSynSymbol.Create('', TSynSet(SynSets[i]).Attribs);
      exit;
    end;
    if (fLine[Run] in AllowedTermSymbols) then begin
      Result := True;                   //: We find it!
      tkSynSymbol := TSynSymbol.Create('', TSynSet(SynSets[i]).Attribs);
      exit;
    end;
  end;
  Run := succ(posStart);
{end}

{ was:
 Result := false;
 curNode := HeadNode;
 nxtNode := nil;
 while (curNode.NextSymbs.Count > 0) and (parser.fLine[parser.Run] <> #0) do begin
   inc(parser.Run);
   nxtNode := curNode.NextSymbs.FindSymbol(parser.fCurrentRule.CaseFunct(parser.fLine[parser.Run]));    //: Ищем этот символ среди текущих веток
   if nxtNode = nil then begin
     dec(parser.Run);
     break;
   end;
   curNode := nxtNode;
 end;

 if curNode.tkSynSymbol = nil then
   exit;

 if (nxtNode = nil) and (curNode.NextSymbs.Count > 0) then
   dec(parser.Run);

 if parser.fLine[parser.Run] <> #0 then
   inc(parser.Run);

 if curNode.BrakeType = btAny then
 begin
   Result := True;
   tkSynSymbol := curNode.tkSynSymbol;
   exit;
 end;

 if parser.fLine[parser.Run] in parser.fCurrentRule.fTermSymbols then
 begin
   Result := True;
   tkSynSymbol := curNode.tkSynSymbol;
 end;
}
end;

//==== TDefaultSymbols =======================================================
constructor TDefaultSymbols.Create(SynSymb: TSynSymbol);
begin
  tkSynSymbol := SynSymb;
end;

destructor TDefaultSymbols.Destroy;
begin
  tkSynSymbol.Free;
  inherited;
end;

function TDefaultSymbols.GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean;
//: Read just symbol, nothing to return
begin
  inc(Run);
  Result := False;
end;

//==== TDefaultTermSymbols ===================================================
constructor TDefaultTermSymbols.Create(SynSymb: TSynSymbol);
begin
  tkSynSymbol := SynSymb;
end;

destructor TDefaultTermSymbols.Destroy;
begin
  tkSynSymbol.Free;
  inherited;
end;

function TDefaultTermSymbols.GetToken(CurRule: TSynRange; fLine: PChar; var Run: integer; var tkSynSymbol: TSynSymbol): boolean;
begin
  if fLine[Run] <> #0 then  //: If is not end of line then
     Inc(Run);                       //: go to next symbol in fLine
  tkSynSymbol := self.tkSynSymbol;        //: And return DefaultTermSymbol
  Result := True;                         //: We found token
end;

//////////////////////////////////////////////////////////////////////////////
          //  RRRRRRR   UUU   UUU  LLL      EEEEEEE   SSSSSS   //
          //   R     R   U     U    L        E       S         //
          //   RRRRRR    U     U    L        EEEEE    SSSSSS   //
          //   R   R     U     U    L    L   E              S  //
          //  RRR   RR    UUUUU    LLLLLLL  EEEEEEE   SSSSSS   //
//////////////////////////////////////////////////////////////////////////////

//==== TSynKeyList ===========================================================
constructor TSynKeyList.Create(st: string);
begin
  inherited Create;
//  AddAttribute();
  KeyList := TStringList.Create;
  KeyList.Text := st;
end;

destructor TSynKeyList.Destroy;
begin
  KeyList.Free;
  inherited;
end;

//==== TSynSet =========================================================
constructor TSynSet.Create(aSymbSet: TSymbSet = []); //Vitalik 2004
begin
  inherited Create;
//  AddAttribute();
  SymbSet := aSymbSet;
end;

destructor TSynSet.Destroy; //Vitalik 2004
begin
  inherited;
end;

//==== TSynRangeRule =========================================================
constructor TSynRangeRule.Create(OpenSymbs: string = ''; CloseSymbs: string = '');
begin
  fOpenSymbol := TSynSymbol.Create(OpenSymbs, nil);
  fCloseSymbol := TSynSymbol.Create(CloseSymbs, nil);
end;

destructor TSynRangeRule.Destroy(); 
begin
  fOpenSymbol.Free();
  fCloseSymbol.Free();
end;

//==== TSynRange =============================================================
constructor TSynRange.Create(OpenSymbs: string; CloseSymbs: string);
begin
  inherited Create;
  OpenCount := 0;

  fRule := TSynRangeRule.Create(OpenSymbs, CloseSymbs);

  fRule.fOpenSymbol.StartType := stAny;
  fRule.fOpenSymbol.BrakeType := btAny;
  fRule.fCloseSymbol.StartType := stAny;
  fRule.fCloseSymbol.BrakeType := btAny;

  FillChar(SymbolList, sizeof(SymbolList), 0);

  SetCaseSensitive(False);

  fPrepared := False;
  fRule.fCloseOnTerm := False;
  fRule.fCloseOnEol := False;

  fSynKeyLists := TList.Create;
  fSynSets := TList.Create;
  fSynSymbols := TList.Create;
  fSynRanges := TList.Create;
  fSynRangeLinks := TList.Create;

  fTermSymbols := DefaultTermSymbols;
//  AddAttribute();
end;

destructor TSynRange.Destroy;
//: Destructor of TSynRange
begin
//#  Reset; ???
  fRule.Free();
{  if Assigned(fRule.fOpenSymbol) then
    fRule.fOpenSymbol.Free;
  if Assigned(fRule.fCloseSymbol) then
    fRule.fCloseSymbol.Free;}
//  Attribs.Free;
  FreeList(fSynKeyLists);
  FreeList(fSynSets);
  FreeList(fSynSymbols);
  FreeList(fSynRanges);
  FreeList(fSynRangeLinks);
  inherited;
end;

//=== Work with fSynSymbols ==================================================
procedure TSynRange.AddSynSymbol(NewSymb: TSynSymbol);
//: Add SynSymbol to the list fSynSymbols. If SynSymbol already exist in list
//:  then remove it and add to the end of the list
//: ??? Может надо если существет не добавлять???
var
  SynSym: TSynSymbol;
begin
  SynSym := FindSymbol(NewSymb.Symbol);
  if SynSym <> nil then
  begin
    fSynSymbols.Remove(SynSym);
    SynSym.Free;
  end;
//  NewSymb.Order := Order;
  fSynSymbols.Add(NewSymb);
end;

function TSynRange.FindSymbol(st: string): TSynSymbol;
//: Find SynSymbol (Symbol = st) in the list fSynSymbols
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fSynSymbols.Count-1 do
   if TSynSymbol(fSynSymbols.Items[i]).Symbol = st then
   begin
     Result := TSynSymbol(fSynSymbols.Items[i]);
     exit;
   end;
end;

//============================================================================
function TSynRange.FindSymbolOwner(Symbol: TSynSymbol): TSynKeyList;
//: Find KeyList that contain SynSymbol
//> Never used!!!
var
  i, j: integer;
begin
  Result := nil;
  for i := 0 to fSynKeyLists.Count-1 do
    if TSynKeyList(fSynKeyLists[i]).KeyList.Find(Symbol.Symbol, j) then
    begin
      Result := TSynKeyList(fSynKeyLists[i]);
      exit;
    end;
end;

//=== Adding rules ===========================================================
procedure TSynRange.AddRule(NewRule: TSynRule);
begin
  if NewRule is TSynRange   then AddRange(NewRule as TSynRange) else
  if NewRule is TSynKeyList then AddKeyList(NewRule as TSynKeyList) else
  if NewRule is TSynSet     then AddSet(NewRule as TSynSet) else
    raise Exception.Create('!!!');
end;

procedure TSynRange.AddCommonRange(Range: TSynRange);
begin
  fSynRangeLinks.Add(Range);
end;

procedure TSynRange.AddRangeLink(NewRangeLink: TSynRangeLink);
begin
  fSynRangeLinks.Add(NewRangeLink);
end;

function TSynRange.AddRangeLink(aRange: TSynRange; aName: string; aColor: TColor): TSynRangeLink;
begin
  Result := TSynRangeLink.Create(aRange);
  with Result do begin
    Name := aName;
    Attribs.Foreground := aColor;
    Attribs.ParentForeground := False;
  end;
  AddRangeLink(Result);
end;

procedure TSynRange.AddRange(NewRange: TSynRange);
begin
  fSynRanges.Add(NewRange);
end;

function TSynRange.AddRange(aOpen, aClose, aName: string; aColor: TColor): TSynRange;
begin
  Result := TSynRange.Create(aOpen, aClose);
  with Result do begin
    Name := aName;
    Attribs.Foreground := aColor;
    Attribs.ParentForeground := False;
  end;
  AddRange(Result);
end;

procedure TSynRange.AddKeyList(NewKeyList: TSynKeyList);
begin
  fSynKeyLists.Add(NewKeyList);
end;

function TSynRange.AddKeyList(aName: string; aColor: TColor): TSynKeyList;
begin
  Result := TSynKeyList.Create('');
  with Result do begin
    Name := aName;
    Attribs.Foreground := aColor;
    Attribs.ParentForeground := False;
  end;
  AddKeyList(Result);
end;

procedure TSynRange.AddSet(NewSet: TSynSet); //Vitalik 2004
begin
  fSynSets.Add(NewSet);
end;

function TSynRange.AddSet(aName: string; aSymbSet: TSymbSet; aColor: TColor): TSynSet; //Vitalik 2004
begin
  Result := TSynSet.Create(aSymbSet);
  with Result do begin
    Name := aName;
    Attribs.Foreground := aColor;
    Attribs.ParentForeground := False;
  end;
  AddSet(Result);
end;

//=== Deleting rules =========================================================
procedure TSynRange.DeleteCommonRange(index: integer);
begin
  TSynRangeLink(fCommonSynRanges[index]).Free;
  fCommonSynRanges.Delete(index);
end;

procedure TSynRange.DeleteCommonRange(Range: TSynRange);
begin
  fCommonSynRanges.Remove(Range);
end;

procedure TSynRange.DeleteRangeLink(index: integer);
begin
  TSynRangeLink(fSynRangeLinks[index]).Free;
  fSynRangeLinks.Delete(index);
end;

procedure TSynRange.DeleteRangeLink(RangeLink: TSynRangeLink);
begin
  fSynRangeLinks.Remove(RangeLink);
  RangeLink.Free;
end;

procedure TSynRange.DeleteRange(Range: TSynRange);
begin
  fSynRanges.Remove(Range);
  Range.Free;
end;

procedure TSynRange.DeleteRange(index: integer);
begin
  TSynRange(fSynRanges[index]).Free;
  fSynRanges.Delete(index);
end;

procedure TSynRange.DeleteKeyList(KeyList: TSynKeyList);
begin
  fSynKeyLists.Remove(KeyList);
  KeyList.Free;
end;

procedure TSynRange.DeleteKeyList(index: integer);
begin
  TSynKeyList(fSynKeyLists[index]).Free;
  fSynKeyLists.Delete(index);
end;

procedure TSynRange.DeleteSet(SynSet: TSynSet); //Vitalik 2004
begin
  fSynSets.Remove(SynSet);
  SynSet.Free;
end;

procedure TSynRange.DeleteSet(index: integer); //Vitalik 2004
begin
  TSynSet(fSynSets[index]).Free;
  fSynSets.Delete(index);
end;

//=== GetCount rules =========================================================
function TSynRange.GetSynSymbolCount: Integer;
begin
  Result := fSynSymbols.Count;
end;

function TSynRange.GetSynRangeLinkCount: Integer;
begin
  Result := fSynRangeLinks.Count;
end;

function TSynRange.GetCommonSynRangeCount(): Integer;
begin
  Result := fCommonSynRanges.Count;
end;

function TSynRange.GetSynRangeCount: Integer;
begin
  Result := fSynRanges.Count;
end;

function TSynRange.GetSynKeyListCount: Integer;
begin
  Result := fSynKeyLists.Count;
end;

function TSynRange.GetSynSetCount: Integer; //Vitalik 2004
begin
  Result := fSynSets.Count;
end;

//=== GetRule from list ======================================================
function TSynRange.GetSynSymbol(Index: Integer): TSynSymbol;
begin
  Result := TSynSymbol(fSynSymbols[Index]);
end;

function TSynRange.GetCommonSynRange(Index: Integer): TSynRange;
begin
  Result := TSynRange(fCommonSynRanges[Index]);
end;

function TSynRange.GetSynRangeLink(Index: Integer): TSynRangeLink;
begin
  Result := TSynRangeLink(fSynRangeLinks[Index]);
end;

function TSynRange.GetSynRange(Index: Integer): TSynRange;
begin
  Result := TSynRange(fSynRanges[Index]);
end;

function TSynRange.GetSynKeyList(Index: Integer): TSynKeyList;
begin
  Result := TSynKeyList(fSynKeyLists[Index]);
end;

function TSynRange.GetSynSet(Index: Integer): TSynSet; //Vitalik 2004
begin
  Result := TSynSet(fSynSets[Index]);
end;

//=== SetDelimiters ==========================================================
procedure TSynRange.SetDelimiters(Delimiters: TSymbSet);
var
  i: integer;
begin
  TermSymbols := Delimiters;
  for i := 0 to RangeCount-1 do
    Ranges[i].SetDelimiters(Delimiters);
end;

(*
procedure TSynRange.SetStyles(aStyles: TSynUniStyles);
//var
//  i: integer;
begin
{  Styles := aStyles;
  for i := 0 to RangeCount-1 do
    Ranges[i].SetStyles(aStyles);}
end;
*)

//=== Case Sensitive =========================================================
function TSynRange.GetCaseSensitive: boolean;
//: Return CaseSensitive
begin
  Result := FCaseSensitive;
end;

procedure TSynRange.SetCaseSensitive(const Value: boolean);
//: Set CaseSensitive
begin
  fCaseSensitive := Value;
  if not Value then begin
    CaseFunct := UpCase;
    StringCaseFunct := UpperCase;
  end
  else begin
    CaseFunct := CaseNone;
    StringCaseFunct := StringCaseNone;
  end;
end;

//=== Prepare rules for parsing ==============================================
procedure QuickSortSymbolList(const List: TList; const lowerPos, upperPos: integer);
var
  i, middlePos: integer;
  pivotValue: string;
Begin
  if lowerPos < upperPos then
  begin

    pivotValue := TSynSymbol(List[lowerPos]).Symbol;
    middlePos := lowerPos; 

    for i := lowerPos + 1 to upperPos do
    begin
      if TSynSymbol(List[i]).Symbol < pivotValue then
      begin
        inc(middlePos);
        List.Exchange(i,middlePos);
      end;
    end;
    List.Exchange(lowerPos,middlePos);

    QuickSortSymbolList(List, lowerPos, middlePos-1);
    QuickSortSymbolList(List, middlePos+1, upperPos);
  end;
end;

// Used in prepare
(*  replaced by quicksort... arb2004
procedure SortSymbolList(List: TList);
//: Sort list fSynSymbols
var
  i: integer;
  fin: boolean;
begin
  fin := False;
  while not fin do
  begin
    fin := True;
    for i := 0 to List.Count-2 do
      if TSynSymbol(List[i]).Symbol > TSynSymbol(List[i+1]).Symbol then
      begin
        List.Exchange(i, i+1);
        fin := False;
      end;
  end;
end;*)

procedure TSynRange.ClearParsingFields();
var
  i: integer;
begin
  OpenCount := 0;
  for i := 0 to RangeCount-1 do
    Ranges[i].ClearParsingFields();
end;

procedure TSynRange.ResetParents(aParent: TSynRange);
var
  i: integer;
begin
  Parent := aParent;
  for i := 0 to RangeCount-1 do
    Ranges[i].ResetParents(Self);
end;

procedure TSynRange.Prepare(Owner: TSynRange);
//: This procedure prepare Range for parsing
//: Is called only from SetLine
var
  i, j, Len: integer;
  SynSymbol: TSynSymbol;
  s: string;
  FirstChar: char;
  BrakeType: TSymbBrakeType;

  function SafeInsertSymbol(Symb: TSynSymbol; Rules: TSynRange; Attribs: TSynHighlighterAttributes): TSynSymbol;
  //: This function add Symb to SynRange, if and only if there is no it there
  //: Return added or found element
  begin
    Result := Rules.FindSymbol(Symb.Symbol); //: Find Symb in Rules
    if Result = nil then begin //: If Symb not found, then add Symb to Rules
      Result := TSynSymbol.Create(Symb.Symbol, Symb.Attributes);
      Result.StartType := Symb.StartType;
      Result.BrakeType := Symb.BrakeType;
      Result.StartLine := Symb.StartLine;
      Rules.AddSynSymbol(Result);
    end;
    if Result.Attributes = nil then                //: If attributes of SynSymbol not setted
      Result.Attributes := Attribs;                //: then set them to Attribs
  end;

  function InsertSymbol(Symb: TSynSymbol; Rules: TSynRange): TSynSymbol;
  begin
    Result := Rules.FindSymbol(Symb.Symbol);
    if Result = nil then begin
      Result := TSynSymbol.Create(Symb.Symbol, Symb.Attributes);
      Result.BrakeType := Symb.BrakeType;
      Rules.AddSynSymbol(Result);
    end;
    Result.Attributes := Symb.Attributes;
  end;

var
  Range: TSynRange;
  RangeLink: TSynRangeLink;

begin
  Reset;            //: If already prepared then reset it!
  fOwner := Owner;
  OpenCount := 0;

  fDefaultSynSymbol  := TSynSymbol.Create('', Attribs);
  fDefaultTermSymbol := TDefaultTermSymbols.Create(TSynSymbol.Create('', Attribs));
  fDefaultSymbols    := TDefaultSymbols.Create(TSynSymbol.Create('', Attribs));

  fTermSymbols := fTermSymbols+AbsoluteTermSymbols;

 if Enabled then begin

  //Add all keywords to list fSynSymbols:
  for i := 0 to fSynKeyLists.Count-1 do //: All KeyLists
    if TSynKeyList(fSynKeyLists[i]).Enabled then
      for j := 0 to TSynKeyList(fSynKeyLists[i]).KeyList.Count-1 do begin//: All keywords in KeyLists
        //: Add current keyword to list fSynSymbols:
        InsertSymbol{AddSymbol}(TSynSymbol.Create(TSynKeyList(fSynKeyLists[i]).KeyList[j],
                                TSynKeyList(fSynKeyLists[i]).Attribs),
                                self);
      end;

  //Assign range opening and closing symbols and Prepare range rules.
  for i := 0 to fSynRanges.Count-1 do begin
    Range := TSynRange(fSynRanges[i]);
    if Range.Enabled then begin
      //Assign range opening symbol
      SynSymbol := SafeInsertSymbol(Range.fRule.fOpenSymbol, Self, Range.Attribs);
      SynSymbol.fOpenRule := Range;

      //Assing range closing symbols
      SynSymbol := SafeInsertSymbol(Range.fRule.fCloseSymbol, Range, Range.Attribs);
      Range.fClosingSymbol := SynSymbol;
      Range.Prepare(Self);
    end;
  end;

  for i := 0 to fSynRangeLinks.Count-1 do begin
    RangeLink := TSynRangeLink(fSynRangeLinks[i]);
    Range := RangeLink.Range;
    if RangeLink.Enabled then begin
      //Assign range opening symbol
      SynSymbol := SafeInsertSymbol(Range.fRule.fOpenSymbol, Self, Range.Attribs);
      SynSymbol.fOpenRule := RangeLink;
      RangeLink.Parent := Self;
      //Assing range closing symbols
//      SynSymbol := SafeInsertSymbol(Range.fRule.fCloseSymbol, Range, Range.Attribs.Std);
//      Range.fClosingSymbol := SynSymbol;
//      Range.Prepare(Self);
    end;
  end;

  //Build tokens table
  QuickSortSymbolList(fSynSymbols, 0, fSynSymbols.Count-1); //: Sort fSynSymbols
  for i := 0 to fSynSymbols.Count-1 do begin       //: run all  SynSymbols
    SynSymbol := TSynSymbol(fSynSymbols[i]);       //: SynSymbol - next SymSymbol
    Len := Length(SynSymbol.Symbol);
    if Len < 1 then                                //: If length equal zero
      continue;                                    //:   then next SynSymbol
    s := SynSymbol.Symbol;                         //: String of SynSymbol
    FirstChar := s[1];                             //: First symbol of string of SynSymbol

    if SynSymbol.BrakeType <> btUnspecified then   //: If BrakeType defined then
      BrakeType := SynSymbol.BrakeType             //:   Write this BreakType to local variable
    else                                           //: Else (if BrakeType not defined)
      if s[Len] in fTermSymbols then               //:   If last symbol is TermSymbol
        BrakeType := btAny                         //:     Write to BreakType: btAny
      else                                         //:   Else
        BrakeType := btTerm;                       //:     Write to BreakType: btTerm

    if SymbolList[CaseFunct(FirstChar)] = nil then //: If in SymbolList on FirstChar there is no nothing
    begin
      if Len = 1 then                              //:   If length of string of SynSymbol equal 1
                                                   //:     then write SynSymbol in this element of SimbolList
        SymbolList[CaseFunct(FirstChar)] := TSymbols.Create(FirstChar, SynSymbol, BrakeType)
      else begin                                   //:   Else (length of SynSymbol greate then 1)
                                                   //:     Write fDefaultSynSymbol (???) to this element | FirstChar
        SymbolList[CaseFunct(FirstChar)] := TSymbols.Create(FirstChar, fDefaultSynSymbol, BrakeType);
                                                   //:     and add SynSymbol to this element | All but without FirstChar
        TSymbols(SymbolList[CaseFunct(FirstChar)]).AddSymbol(StringCaseFunct(copy(s, 2, Len-1)), SynSymbol, BrakeType);
      end;
    end
    else begin                                     //: Else (if in SynSymbol exist something)
      if Len = 1 then
      else                                         //: If length of string SynSymbol greate then 1
                                                   //:   Add SynSymbol to this element | All but without FirstChar
        TSymbols(SymbolList[CaseFunct(FirstChar)]).AddSymbol(StringCaseFunct(copy(s, 2, Len-1)), SynSymbol, BrakeType);
    end;
  end;

{begin} //Vitalik 2004
  if fSynSets.Count > 0 then
    for i := 0 to 255 do
      for j := 0 to fSynSets.Count-1 do begin
        if TSynSet(fSynSets[j]).Enabled and (char(i) in TSynSet(fSynSets[j]).SymbSet) then
          if SymbolList[CaseFunct(char(i))] = nil then
            SymbolList[CaseFunct(char(i))] := TSymbols.Create(TSynSet(fSynSets[j]))
          else
            TSymbols(SymbolList[CaseFunct(char(i))]).AddSet(TSynSet(fSynSets[j]));
      end;
//          SymbolList[char(i)] := fSetSymbols;
//          TSetSymbols(SymbolList[char(i)]).AddSetfSetSymbols;
{end} //Vitalik 2004
 end;
  //Fill remaining table
  for i := 0 to 255 do
  if SymbolList[char(i)] = nil then
  begin
    if char(i) in fTermSymbols then
      SymbolList[char(i)] := fDefaultTermSymbol
    else
      SymbolList[char(i)] := fDefaultSymbols;
   end;

  fPrepared := true;
end;

procedure TSynRange.Reset;
//: Clear some properties of SynRange,
//: вызывается при очистке Clear, а также при Подготовке SynRang'a (Prepare)
//: Ресетится только если SynRange был уже подготовлен!
var
  i: integer;
begin
  if not fPrepared then
    exit;
  fDefaultSynSymbol.Free;
  fDefaultTermSymbol.Free;
  fDefaultSymbols.Free;
  for i := 0 to 255 do
    SymbolList[char(i)] := nil; //maybe need to free???
  for i := 0 to fSynRanges.Count-1 do
    TSynRange( fSynRanges[i] ).Reset;
  ClearList(fSynSymbols);
  fPrepared := False;
end;

procedure TSynRange.Clear;
//: Clear primary properties of SynRang, call in creating new rools
var
  i: integer;
begin
//!!!!!!!!!!!!!!!!!!!!!! Нужно еще очищать или удалять OpenSymbol и CloseSymbol !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  Reset;  //: Reser Range (clear some properties)
  for i := 0 to fSynRanges.Count-1 do   //: Clear all sub-ranges
    TSynRange(fSynRanges[i]).Clear;
  ClearList(fSynRanges);
  ClearList(fSynSymbols);
  ClearList(fSynKeyLists);
  ClearList(fSynSets);
end;

function TSynRange.FindRange(const Name: string): TSynRange;
Var
  I: integer;
begin
  Result := nil;
  if fOwner = nil then Exit;

  for I := 0 to fOwner.RangeCount - 1 do
    if SameText(Name, fOwner.Ranges[I].Name) then
    begin
      Result := fOwner.Ranges[I];
      Exit;
    end;
end;

procedure TSynRange.SetColorForChilds; //Vitalik 2004
var
  i: integer;
begin
  for i := 0 to RangeCount-1 do begin
    if Ranges[i].Attribs.ParentForeground then begin
      Ranges[i].Attribs.Foreground := Attribs.Foreground;
      Ranges[i].Attribs.OldColorForeground := Attribs.Foreground;
    end;
    if Ranges[i].Attribs.ParentBackground then begin
      Ranges[i].Attribs.Background := Attribs.Background;
      Ranges[i].Attribs.OldColorBackground := Attribs.Background;
    end;
    Ranges[i].SetColorForChilds;
  end;
  for i := 0 to KeyListCount-1 do begin
    if KeyLists[i].Attribs.ParentForeground then
      KeyLists[i].Attribs.Foreground := Attribs.Foreground;
    if KeyLists[i].Attribs.ParentBackground then
      KeyLists[i].Attribs.Background := Attribs.Background;
  end;
  for i := 0 to SetCount-1 do begin
    if Sets[i].Attribs.ParentForeground then
      Sets[i].Attribs.Foreground := Attribs.Foreground;
    if Sets[i].Attribs.ParentBackground then
      Sets[i].Attribs.Background := Attribs.Background;
  end;
end;

//==== TSynRangeLink =========================================================
constructor TSynRangeLink.Create(aRange: TSynRange);
begin
  inherited Create;
  Range := aRange;
  Parent := nil;
end;

//////////////////////////////////////////////////////////////////////////////
     //  LLL       OOOOO    AAAAA   DDDDDD   IIIII  NN    N   GGGGGG  //
     //   L       O     O  A     A   D    D    I    NNN   N  G        //
     //   L       O     O  AAAAAAA   D    D    I    N NNN N  G   GGG  //
     //   L    L  O     O  A     A   D    D    I    N   NNN  G     G  //
     //  LLLLLLL   OOOOO   A     A  DDDDDD   IIIII  N    NN   GGGGGG  //
//////////////////////////////////////////////////////////////////////////////
procedure TSynKeyList.LoadFromXml(xml: TDOMNode);
var
  I, J: Integer;
  ChildNode: TDOMNode;
  Key, Value, LowValue: string;
//  OldAttribs: {TSynHighlighter}TSynAttributes;
begin
  if xml = nil then Exit;
  if not SameText(xml.NodeName, 'Keywords') then xml:= xml.FindNode('Keywords');
  if xml = nil then raise Exception.Create(ClassName + '.LoadFromXml - no keywords to load!');

  for I := 0 to Int32(xml.Attributes.Length) - 1 do
  begin
    Key := xml.Attributes[I].NodeName;
    Value := xml.Attributes[I].NodeValue;
    LowValue := LowerCase(Value);
    if SameText('Name', Key) then Name := Value else
    if SameText('Enabled', Key) then Enabled := (LowValue = 'true') else
    if SameText('Attributes', Key) then
      Attribs.LoadFromString(Value) else
    if SameText('Style', Key) then begin
      Style := Value;
      if Styles <> nil then //begin
//          OldAttribs := Attribs;
        Attribs := Styles.GetStyleDef(Value, Attribs);
//          if OldAttribs <> Attribs then
//            Attribs.UseStyle := True;
{        if (Attribs = DefaultAttr) or (Attribs = nil) then
        Attribs := DefaultAttri;}
//        end;
    end else
//    Attribs := fStyles.GetStyleDef(getAttrValue('style', xml), defaultattr);
  end;

  KeyList.BeginUpdate;
  KeyList.Clear;
  try
    for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
    begin
      ChildNode:= xml.ChildNodes.Item[J];
      if SameText('Word', ChildNode.NodeName) then
        if (ChildNode.Attributes.Length > 0) and SameText('Value', ChildNode.Attributes[0].NodeName) then
          KeyList.Add(ChildNode.Attributes[0].NodeValue);
    end;
  finally
    KeyList.EndUpdate;
  end;
end;

procedure TSynSet.LoadFromXml(xml: TDOMNode);
var
  i: integer;
  Key, Value, LowValue: string;
//  OldAttribs: {TSynHighlighter}TSynAttributes;
begin
  if xml = nil then Exit;
  if not SameText(xml.NodeName, 'Set') then xml:= xml.FindNode('Set');
  if xml = nil then raise Exception.Create(ClassName + '.LoadFromXml - no set to load!');

  for i := 0 to Int32(xml.Attributes.Length) - 1 do
  begin
    Key := xml.Attributes[i].NodeName;
    Value := xml.Attributes[i].NodeValue;
    LowValue := LowerCase(Value);         {ind := 0;}
    if SameText('Name', Key) then Name := Value else
    if SameText('Enabled', Key) then Enabled := (LowValue = 'true') else
    if SameText('Attributes', Key) then
      Attribs.LoadFromString(Value) else
    if SameText('Style', Key) then begin
      Style := Value;
      if Styles <> nil then //begin
//          OldAttribs := Attribs;
        Attribs := Styles.GetStyleDef(Value, Attribs);
//          if OldAttribs <> Attribs then
//            Attribs.UseStyle := True;
{        if (Attribs = DefaultAttr) or (Attribs = nil) then
        Attribs := DefaultAttri;}
//        end;
    end else
    if SameText('Symbols', Key) then SymbSet := StrToSet(Value)
//    Attribs := fStyles.GetStyleDef(getAttrValue('style', xml), defaultattr);
  end;
end;

procedure TSynRange.LoadFromXml(xml: TDOMNode);
var
  I, J: Integer;
  ChildNode: TDOMNode;
  NewSynRange: TSynRange;
  NewSynKeyList: TSynKeyList;
  NewSynSet: TSynSet;
  Key, Value, LowValue: string;
//  OldAttribs: {TSynHighlighter}TSynAttributes;
begin
  if xml = nil then Exit;
  if not SameText(xml.NodeName, 'Range') then xml:= xml.FindNode('Range');
  if xml = nil then raise Exception.Create(ClassName + '.LoadFromXml - no range to load!');

//    Clear;            //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Вставить нормальный Clear в KeyList и SynSet !!!!!!!!!!!!!!!!!!
  Enabled := True;   CaseSensitive := False;
  for i := 0 to Int32(xml.Attributes.Length) - 1 do
  begin
    Key := xml.Attributes[i].NodeName;
    Value := xml.Attributes[i].NodeValue;
    LowValue := LowerCase(Value);
    if SameText('Name', Key) then Name := Value else
    if SameText('Enabled', Key) then Enabled := (LowValue = 'true') else
    if SameText('Attributes', Key) then
      Attribs.LoadFromString(Value) else
    if SameText('Style', Key) then begin
      Style := Value;
      if Styles <> nil then //begin
//          OldAttribs := Attribs;
        Attribs := Styles.GetStyleDef(Value, Attribs);
//          if OldAttribs <> Attribs then
//            Attribs.UseStyle := True;
{        if (Attribs = DefaultAttr) or (Attribs = nil) then
        Attribs := DefaultAttri;}
//        end;
    end else
    if SameText('CaseSensitive', Key) then CaseSensitive := (LowValue = 'true') else
    if SameText('Delimiters', Key) then
//      if SameText(GetAttrValue('spaces', xml), 'true') then
//        TermSymbols := String2Set(xml.CurContent) + [#32, #9, #13, #10] else
      TermSymbols := StrToSet(Value)  // CloseOnTerm := true;
  end;

  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode:= xml.ChildNodes.Item[J];
    if SameText('Rule', ChildNode.NodeName) then
    for i := 0 to Int32(ChildNode.Attributes.Length) - 1 do begin
      Key := ChildNode.Attributes[i].NodeName;
      Value := ChildNode.Attributes[i].NodeValue;
        LowValue := LowerCase(Value);
        if SameText('Enabled', Key) then Enabled := (LowValue = 'true') else
        if SameText('OpenSymbol', Key) then fRule.fOpenSymbol.Symbol := Value else
        if SameText('OpenSymbolFinishOnEol', Key) then
          if LowValue = 'true' then fRule.fOpenSymbol.Symbol := fRule.fOpenSymbol.Symbol + #0 else else
        if SameText('OpenSymbolStartLine', Key) then with fRule.fOpenSymbol do
          if LowValue = 'true' then     StartLine := slFirst else
          if LowValue = 'nonspace' then StartLine := slFirstNonSpace else
                                        StartLine := slNotFirst else
        if SameText('OpenSymbolPartOfTerm', Key) then with fRule.fOpenSymbol do
          if LowValue = 'true' then begin  StartType := stAny;  BrakeType := btAny; end else
          if LowValue = 'left' then begin  StartType := stAny;  BrakeType := btTerm; end else
          if LowValue = 'right' then begin StartType := stTerm; BrakeType := btAny; end else
                                     begin StartType := stTerm; BrakeType := btTerm; end else
        if SameText('CloseSymbol', Key) then fRule.fCloseSymbol.Symbol := Value else
        if SameText('CloseSymbolFinishOnEol', Key) then
          if LowValue = 'true' then fRule.fCloseSymbol.Symbol := fRule.fCloseSymbol.Symbol + #0 else else
        if SameText('CloseSymbolStartLine', Key) then with fRule.fCloseSymbol do
          if LowValue = 'true' then     StartLine := slFirst else
          if LowValue = 'nonspace' then StartLine := slFirstNonSpace else
                                        StartLine := slNotFirst else
        if SameText('CloseSymbolPartOfTerm', Key) then with fRule.fCloseSymbol do
          if LowValue = 'true' then begin  StartType := stAny;  BrakeType := btAny; end else
          if LowValue = 'left' then begin  StartType := stAny;  BrakeType := btTerm; end else
          if LowValue = 'right' then begin StartType := stTerm; BrakeType := btAny; end else
                                     begin StartType := stTerm; BrakeType := btTerm; end else
        if SameText('CloseOnTerm',    Key) then fRule.fCloseOnTerm    := (LowValue = 'true') else
        if SameText('CloseOnEol',     Key) then fRule.fCloseOnEol     := (LowValue = 'true') else
        if SameText('AllowPredClose', Key) then fRule.fAllowPredClose := (LowValue = 'true') else
      end else
    if SameText('Range', ChildNode.NodeName) then begin
      NewSynRange := TSynRange.Create;
      NewSynRange.Styles := Styles;
      AddRange(NewSynRange);
      NewSynRange.LoadFromXml(ChildNode);
    end else
    if SameText('Keywords', ChildNode.NodeName) then begin
      NewSynKeyList := TSynKeyList.Create;
      NewSynKeyList.Styles := Styles;
      AddKeyList(NewSynKeyList);
      NewSynKeyList.LoadFromXml(ChildNode);
    end else
    if SameText('Set', ChildNode.NodeName) then begin
      NewSynSet := TSynSet.Create;
      NewSynSet.Styles := Styles;
      AddSet(NewSynSet);
      NewSynSet.LoadFromXml(ChildNode);
    end
{      else if SameText(xml.CurName, 'TextStyle') then
    begin
      DefaultAttri := fStyles.getStyleDef(xml.CurContent, defaultAttr);
      if (NumberAttri = DefaultAttr) or (NumberAttri = nil) then
        NumberAttri := DefaultAttri;
    end
    else if SameText(xml.CurName, 'NumberStyle') then
      NumberAttri := fStyles.getStyleDef(xml.CurContent, defaultAttr);}
  end;
end;

//////////////////////////////////////////////////////////////////////////////
        //   SSSSSS   AAAAAA   V       V  IIIII  NN    N   GGGGGGG  //
        //  S        A      A  V       V    I    NNN   N  G         //
        //   SSSSS   AAAAAAAA  VV     VV    I    N NNN N  G    GGG  //
        //        S  A      A    VV VV      I    N   NNN  G      G  //
        //  SSSSSS   A      A      V      IIIII  N    NN   GGGGGGG  //
//////////////////////////////////////////////////////////////////////////////

procedure TSynKeyList.SaveToStream(StreamWriter: TStreamWriter; Ind: integer);
var
  i: integer;
begin
  with StreamWriter do begin
    WriteTag(Ind, 'Keywords');
    WriteParam('Name', Name);
    WriteBoolParam('Enabled', Enabled, True);
    Attribs.SaveToStream(StreamWriter);    
    WriteParam('Style', Style);
    WriteString(CloseStartTag + EOL);
    for i := 0 to KeyList.Count-1 do begin
      WriteTag(Ind+2, 'Word');
      WriteParam('Value', KeyList[i], CloseEmptyTag);
    end;
    WriteTag(Ind, '/Keywords', True);
  end;
end;

procedure TSynSet.SaveToStream(StreamWriter: TStreamWriter; Ind: integer);
begin
  with StreamWriter do begin
    WriteTag(Ind, 'Set');
    WriteParam('Name', Name);
    WriteBoolParam('Enabled', Enabled, True);
    Attribs.SaveToStream(StreamWriter);    
    WriteParam('Style', Style);
    WriteParam('Symbols', SetToStr(SymbSet), CloseEmptyTag);
{    if S.StartType = stAny then
      if S.BrakeType = btAny then InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'True')
      else InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'Left')
    else
      if S.BrakeType = btAny then InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'Right')
      else InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'False');}
  end;
end;

procedure TSynRange.SaveToStream(StreamWriter: TStreamWriter; Ind: integer);
var
  i: integer;
begin
  with StreamWriter do begin
    WriteTag(Ind, 'Range');
    WriteParam('Name', Name);
    WriteBoolParam('Enabled', Enabled, True);
    Attribs.SaveToStream(StreamWriter);    
    WriteParam('Style', Style);
    WriteBoolParam('CaseSensitive', CaseSensitive, False);
    WriteString(EOL + Indent(Ind + Length('<Range')));
    WriteParam('Delimiters', SetToStr(TermSymbols), CloseStartTag);
    for i := 0 to 0 do begin
      WriteTag(Ind+2, 'Rule');
      with fRule.fOpenSymbol do begin
        if Length(Symbol) > 0 then
          if Symbol[Length(Symbol)] = #0 then begin
            WriteParam('OpenSymbol', copy(Symbol, 1, Length(Symbol) - 1));
            WriteParam('OpenSymbolFinishOnEol', 'True');
          end else begin
            WriteParam('OpenSymbol', Symbol);
            {WriteParam('OpenSymbolFinishOnEol', 'False');}
          end;
        if StartLine = slFirst         then WriteParam('OpenSymbolStartLine', 'True') else
        if StartLine = slFirstNonSpace then WriteParam('OpenSymbolStartLine', 'NonSpace');
                                       //else WriteParam('OpenSymbolStartLine', 'False');
        if StartType = stAny then
          if BrakeType = btAny then //WriteParam('OpenSymbolPartOfTerm', 'True')
                               else WriteParam('OpenSymbolPartOfTerm', 'Left') else
          if BrakeType = btAny then WriteParam('OpenSymbolPartOfTerm', 'Right')
                               else WriteParam('OpenSymbolPartOfTerm', 'False');
      end;
      with fRule.fCloseSymbol do begin
        if Length(Symbol) > 0 then
          if Symbol[Length(Symbol)] = #0 then begin
            WriteParam('CloseSymbol', copy(Symbol, 1, Length(Symbol) - 1));
            WriteParam('CloseSymbolFinishOnEol', 'True');
          end else begin
            WriteParam('CloseSymbol', Symbol);
            {WriteParam('CloseSymbolFinishOnEol', 'False');}
          end;
        if StartLine = slFirst         then WriteParam('CloseSymbolStartLine', 'True') else
        if StartLine = slFirstNonSpace then WriteParam('CloseSymbolStartLine', 'NonSpace');
                                       //else WriteParam('CloseSymbolStartLine', 'False');
        if StartType = stAny then
          if BrakeType = btAny then //WriteParam('CloseSymbolPartOfTerm', 'True')
                               else WriteParam('CloseSymbolPartOfTerm', 'Left') else
          if BrakeType = btAny then WriteParam('CloseSymbolPartOfTerm', 'Right')
                               else WriteParam('CloseSymbolPartOfTerm', 'False');
      end;
      WriteBoolParam('CloseOnTerm',    fRule.fCloseOnTerm,    False);
      WriteBoolParam('CloseOnEol',     fRule.fCloseOnEol,     False);
      WriteBoolParam('AllowPredClose', fRule.fAllowPredClose, False);
      WriteString(CloseEmptyTag + EOL);
    end;
    for i := 0 to KeyListCount-1 do KeyLists[i].SaveToStream(StreamWriter, Ind+2);
    for i := 0 to SetCount-1     do Sets[i].SaveToStream(StreamWriter, Ind+2);
    for i := 0 to RangeCount-1   do Ranges[i].SaveToStream(StreamWriter, Ind+2);
    WriteTag(Ind, '/Range', True);
  end;
end;

function ReadValue(ANode: TDOMNode): String;
begin
  if Assigned(ANode.FirstChild) then
    Result:= ANode.FirstChild.NodeValue
  else
    Result:= EmptyStr;
end;

function Verify(tag: string; xml: TDOMNode): boolean; overload;
begin
  Result := SameText(xml.NodeName, tag);
end;

procedure LoadAttri(curRule: TSynRule; xml: TDOMNode);
var
  J: Integer;
  ChildNode: TDOMNode;
begin
//  inc(CurRule.ind);
  CurRule.Attribs{ByIndex[0]}.ParentForeground := False;
  CurRule.Attribs{ByIndex[0]}.ParentBackground := False;
  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode:= xml.ChildNodes.Item[J];
    if Verify('Back',ChildNode) then begin
      CurRule.Attribs.Background := StrToIntDef(ReadValue(ChildNode), $FFFFFF);
      CurRule.Attribs.OldColorBackground := CurRule.Attribs.Background;
    end else
    if Verify('Fore',ChildNode) then begin
      CurRule.Attribs.Foreground := StrToIntDef(ReadValue(ChildNode), 0);
      CurRule.Attribs.OldColorForeground := CurRule.Attribs.Foreground;
    end else
    if Verify('Style',ChildNode) then
      CurRule.Attribs.Style := StrToFontStyle(ReadValue(ChildNode))
    else if Verify('ParentForeground',ChildNode) then
      CurRule.Attribs.ParentForeground := LowerCase(ReadValue(ChildNode)) = 'true'
    else if Verify('ParentBackground',ChildNode) then
      CurRule.Attribs.ParentBackground := LowerCase(ReadValue(ChildNode)) = 'true';
  end;
end;

procedure TSynKeyList.LoadHglFromXml(xml: TDOMNode; SchCount,SchIndex: integer);
var
  J: Integer;
  ChildNode: TDOMNode;
  TempSchIndex: integer;
begin
//  if curKw = nil then Exit;
  if xml = nil then Exit;

  if ( SameText('KW'{'Keywords'}, xml.NodeName) ) then
    begin
      if xml.Attributes.Length > 0 then Name := xml.Attributes[0].NodeValue
//    Attribs := fStyles.GetStyleDef(getAttrValue('style', xml), defaultattr);
    end
  else
    raise Exception.Create(ClassName + '.LoadFromXml - no keywords to load!');

//  ClearAttributes();
//  for i := 0 to SchCount-1 do
//    AddAttribute();
//  ind := -1;
  TempSchIndex := SchIndex;

  KeyList.BeginUpdate;
  KeyList.Clear;
  try
    for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
    begin
      ChildNode:= xml.ChildNodes.Item[J];
      if SameText(ChildNode.NodeName, 'Attri') or SameText(ChildNode.NodeName, 'Def') then begin
        if TempSchIndex >= 0 then LoadAttri(self, ChildNode);
        dec(TempSchIndex);
      end else
      if Verify('Enabled',ChildNode) then
        Enabled := LowerCase(ReadValue(ChildNode)) = 'true'
      else
      if Verify('W',ChildNode) then begin
        KeyList.Add(ReadValue(ChildNode));
      end;
    end;
  finally
    ind := SchIndex;
    KeyList.EndUpdate;
  end;
end;

procedure TSynSet.LoadHglFromXml(xml: TDOMNode; SchCount,SchIndex: integer);
var
  J: Integer;
  ChildNode: TDOMNode;
  TempSchIndex: integer;
begin
//  if curSet = nil then Exit;
  if xml = nil then Exit;

  if ( SameText('Set', xml.NodeName) ) then
    begin
      if xml.Attributes.Length > 0 then Name := xml.Attributes[0].NodeValue
//    Attribs := fStyles.GetStyleDef(getAttrValue('style', xml), defaultattr);
    end
  else
    raise Exception.Create(ClassName + '.LoadFromXml - no set to load!');

{  ClearAttributes();
  for i := 0 to SchCount-1 do
    AddAttribute();
  ind := -1;}
  TempSchIndex := SchIndex;

  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode:= xml.ChildNodes.Item[J];
    if SameText(ChildNode.NodeName, 'Attri') or SameText(ChildNode.NodeName, 'Def') then begin
      if TempSchIndex >= 0 then LoadAttri(self, ChildNode);
      dec(TempSchIndex);
    end else
    if Verify('Enabled',ChildNode) then
      Enabled := LowerCase(ReadValue(ChildNode)) = 'true'
    else
    if Verify('S',ChildNode) then
      SymbSet := StrToSet(ReadValue(ChildNode));
  end;

  ind := SchIndex;
end;

procedure TSynRange.LoadHglFromXml(xml: TDOMNode; SchCount, SchIndex: integer);
var
  NewSynRange: TSynRange;
  NewSynKeyList: TSynKeyList;
  NewSynSet: TSynSet;
  S: string;
  TempSchIndex: integer;
  J: Integer;
  ChildNode: TDOMNode;
begin
  fRule.fOpenSymbol.BrakeType := btAny;
  if SameText(xml.NodeName, 'Range') then
  begin
    if xml.Attributes.Length > 0 then
      S:= xml.Attributes[0].NodeValue;
    if S <> '' then
      Name := S;
  end else
    raise Exception.Create(ClassName + '.LoadFromXml - no range to load!');

{  ClearAttributes();
  for i := 0 to SchCount-1 do
    AddAttribute();
  ind := -1;}
  TempSchIndex := SchIndex;

  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode:= xml.ChildNodes.Item[J];
    if Verify('Enabled', ChildNode) then
      Enabled := LowerCase(ReadValue(ChildNode)) = 'true'
    else
    if Verify('CaseSensitive', ChildNode) then
      CaseSensitive := LowerCase(ReadValue(ChildNode)) = 'true'
    else
    if Verify('OpenSymbol', ChildNode) then
      fRule.fOpenSymbol.Symbol := ReadValue(ChildNode)
    else
    if Verify('CloseSymbol', ChildNode) then
      fRule.fCloseSymbol.Symbol := ReadValue(ChildNode)
    else
    if Verify('OpenSymbolFinishOnEol', ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then
        fRule.fOpenSymbol.Symbol := fRule.fOpenSymbol.Symbol + #0
      else
    else
    if Verify('CloseSymbolFinishOnEol',ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then
        fRule.fCloseSymbol.Symbol := fRule.fCloseSymbol.Symbol + #0
      else
    else
    if Verify('CloseOnTerm',ChildNode) then
      fRule.fCloseOnTerm := LowerCase(ReadValue(ChildNode)) = 'true'
    else
    if Verify('CloseOnEol',ChildNode) then
      fRule.fCloseOnEol := LowerCase(ReadValue(ChildNode)) = 'true'
    else
    if Verify('AllowPredClose',ChildNode) then
      fRule.fAllowPredClose := LowerCase(ReadValue(ChildNode)) = 'true'
    else
    if Verify('OpenSymbolStartLine',ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then
        fRule.fOpenSymbol.StartLine := slFirst
      else if LowerCase(ReadValue(ChildNode)) = 'nonspace' then
        fRule.fOpenSymbol.StartLine := slFirstNonSpace
      else
        fRule.fOpenSymbol.StartLine := slNotFirst
    else
    if Verify('CloseSymbolStartLine',ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then
        fRule.fCloseSymbol.StartLine := slFirst
      else if LowerCase(ReadValue(ChildNode)) = 'nonspace' then
        fRule.fCloseSymbol.StartLine := slFirstNonSpace
      else
        fRule.fCloseSymbol.StartLine := slNotFirst
    else
    if Verify('AnyTerm',ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then
        fRule.fOpenSymbol.BrakeType := btAny
      else
        fRule.fOpenSymbol.BrakeType := btTerm
//        if StrToBoolDef(ReadValue(ChildNode), false) then
//          fRule.fOpenSymbol.BrakeType := btTerm;
    else
    if Verify('OpenSymbolPartOfTerm',ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then begin
        fRule.fOpenSymbol.StartType := stAny;
        fRule.fOpenSymbol.BrakeType := btAny;
      end else if LowerCase(ReadValue(ChildNode)) = 'left' then begin
        fRule.fOpenSymbol.StartType := stAny;
        fRule.fOpenSymbol.BrakeType := btTerm;
      end else if LowerCase(ReadValue(ChildNode)) = 'right' then begin
        fRule.fOpenSymbol.StartType := stTerm;
        fRule.fOpenSymbol.BrakeType := btAny;
      end else begin
        fRule.fOpenSymbol.StartType := stTerm;
        fRule.fOpenSymbol.BrakeType := btTerm;
      end
    else
    if Verify('CloseSymbolPartOfTerm',ChildNode) then
      if LowerCase(ReadValue(ChildNode)) = 'true' then begin
        fRule.fCloseSymbol.StartType := stAny;
        fRule.fCloseSymbol.BrakeType := btAny;
      end else if LowerCase(ReadValue(ChildNode)) = 'left' then begin
        fRule.fCloseSymbol.StartType := stAny;
        fRule.fCloseSymbol.BrakeType := btTerm;
      end else if LowerCase(ReadValue(ChildNode)) = 'right' then begin
        fRule.fCloseSymbol.StartType := stTerm;
        fRule.fCloseSymbol.BrakeType := btAny;
      end else begin
        fRule.fCloseSymbol.StartType := stTerm;
        fRule.fCloseSymbol.BrakeType := btTerm;
      end
    else
    if Verify('DelimiterChars',ChildNode) then
//        if SameText(GetAttrValue('spaces', xml), 'true') then
//          TermSymbols := String2Set(ReadValue(ChildNode)) + [#32, #9, #13, #10]
//        else
      if Assigned(ChildNode.FirstChild) then
        TermSymbols := StrToSet(ReadValue(ChildNode))
      else
//        CloseOnTerm := true;
    else
{      else if SameText(xml.CurName, 'TextStyle') then
    begin
      DefaultAttri := fStyles.getStyleDef(ReadValue(ChildNode), defaultAttr);
      if (NumberAttri = DefaultAttr) or (NumberAttri = nil) then
        NumberAttri := DefaultAttri;
    end
    else if SameText(xml.CurName, 'NumberStyle') then
      NumberAttri := fStyles.getStyleDef(ReadValue(ChildNode), defaultAttr);}
    if SameText(ChildNode.NodeName, 'Attri') or SameText(ChildNode.NodeName, 'Def') then begin
      if TempSchIndex >= 0 then LoadAttri(self, ChildNode);
      dec(TempSchIndex);
    end else
    if SameText(ChildNode.NodeName, 'Range') then begin
      NewSynRange := TSynRange.Create;
      AddRange(NewSynRange);
      NewSynRange.LoadHglFromXml(ChildNode, SchCount, SchIndex);
    end else
    if SameText(ChildNode.NodeName, 'KW') then begin
      NewSynKeyList := TSynKeyList.Create;
      AddKeyList(NewSynKeyList);
      NewSynKeyList.LoadHglFromXml(ChildNode, SchCount, SchIndex);
    end else
    if SameText(ChildNode.NodeName, 'Set') then begin
      NewSynSet := TSynSet.Create;
      AddSet(NewSynSet);
      NewSynSet.LoadHglFromXml(ChildNode, SchCount, SchIndex);
    end;
  end;
  ind := SchIndex;
end;

procedure TSynRange.LoadHglFromStream(aSrc: TStream);
var
  I, J: Integer;
  xml: TXMLDocument = nil;
  SchCount, SchIndex: integer;
  ChildNode, ChildNode2: TDOMNode;
begin
  try
    SchCount := 0;   SchIndex := -1;
    ReadXMLFile(xml, aSrc);

    for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
    begin
      ChildNode:= xml.ChildNodes.Item[J];
      if Verify('SchemeIndex', ChildNode) then
        SchIndex := StrToInt(ReadValue(ChildNode))
      else if Verify('Schemes', ChildNode) then
      begin
        for I:= 0 to Int32(ChildNode.ChildNodes.Count) - 1 do begin
        ChildNode2:= ChildNode.ChildNodes.Item[I];
          if Verify('S', ChildNode2) then
            inc(SchCount);
        end
      end
      else if SameText(ChildNode.NodeName, 'Range') then
        LoadHglFromXml(ChildNode, SchCount, SchIndex);
    end;
  finally
    xml.Free;
  end;
end;

end.

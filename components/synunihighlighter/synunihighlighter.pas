{-------------------------------------------------------------------------------
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
of this file under either the MPL or the GPL.}

{
@abstract(Provides a universal highlighter for SynEdit)
@authors(Fantasist [walking_in_the_sky@yahoo.com],Vit [nevzorov@yahoo.com])
@created(2003)
@lastmod(2003-01-29)
}

(******************************************************************************
Authors: Fantasist (Kirill Burtsev walking_in_the_sky@yahoo.com)
         Vit (Vitaly Nevzorov nevzorov@yahoo.com); 
Official Site: www.delphist.com
With all questions, please visit www.delphist.com/forum

Contributors:

Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
  Initially adapted for use with Lazarus and FPC - 2003-06-12
  Changes can be found by searching for: ////TL


******************************************************************************)

unit SynUniHighlighter;

{$I SynEdit.inc}

interface

uses
  SysUtils,
  ////TL Windows,
  Graphics,
  Classes, FileUtil, LazUTF8Classes,
  SynEditTypes,
  GraphType, ////TL 2003-06-11: Added for TFontStyles
  SynEditHighlighter;

Const
  _Root='Root';
  _NewRange='New';

type
  TVersionType=(vtInternalTest, vtBeta, vtRelease);

  TAuthorInfo=record
    Name:string;
    Email:string;
    Web:string;
    Copyright:string;
    Company:string;
    Remark:string;
  end;

  TVerInfo=record
    Version:Integer;
    Revision:Integer;
    VersionType:TVersionType;
    ReleaseDate:TDateTime;
  end;

  THighInfo=record
    Name:string;
    FileTypeName:string;
    Layout:string;
  end;

  TInfo=record
    Author:TAuthorInfo;
    Version:TVerInfo;
    General:THighInfo;
    History:TStringList;
    Sample:TStringlist;
  End;

  TSynRange=class;

  TSymbBrakeType=(btUnspecified,btAny,btTerm);

  TSynSymbol=class
  private
    Attr:TSynHighlighterAttributes;
    fOpenRule:TSynRange;
    FBrakeType:TSymbBrakeType;
  public
    Symbol:string;
    property BrakeType:TSymbBrakeType read FBrakeType write FBrakeType;
    property Attributes:TSynHighlighterAttributes read Attr write Attr;
    constructor Create(s:string;attribs:TSynHighlighterAttributes); virtual;
    destructor Destroy; override;
  end;

  TSynSymbolGroup=class
    Attribs:TSynHighlighterAttributes;
    KeywordsList:TStringList;
    GroupName:string;
    Name:string;
    constructor Create(s:string; attr:TSynHighlighterAttributes);
    destructor Destroy; override;
  end;

  TSymbRangeSet=record
    RangeValue:Integer;
    IncludeSymbols:boolean;
  end;

  PSymbRangeSet=^TSymbRangeSet;


  SymbolsSet=set of char;

  TSymbolList=class;

  TSymbolNode=class
    c:char;
    BrakeType:TSymbBrakeType;
    NextSymbs:TSymbolList;
    tkSynSymbol:TSynSymbol;
    constructor Create(AC:char; SynSymbol:TSynSymbol; ABrakeType:TSymbBrakeType); overload; virtual;
    constructor Create(AC:char); overload;
    destructor Destroy; override;
  end;

  TSymbolList=class
    SymbList:TList;
    function FindSymbol(c:char):TSymbolNode;
    procedure AddSymbol(symb:TSymbolNode);
    procedure SetSymbolNode(Index:Integer;Value:TSymbolNode);
    function GetSymbolNode(Index:integer):TSymbolNode;
    function GetCount:integer;
    property Nodes[index:integer]:TSymbolNode read GetSymbolNode write SetSymbolNode;
    property Count:Integer read GetCount;
    constructor Create(); virtual;
    destructor Destroy; override;
  end;

  TSynUniSyn = class;

  TAbstractSymbol=class
    function GetToken(parser:TSynUniSyn; var tkSynSymbol:TSynSymbol):boolean; virtual; abstract;
  end;

  TSymbols=class(TAbstractSymbol)
    HeadNode:TSymbolNode;
    function GetToken(parser:TSynUniSyn; var tkSynSymbol:TSynSymbol):boolean; override;
    procedure AddSymbol(s:string; tkSynSymbol:TSynSymbol; ABrakeType: TSymbBrakeType);
    function FindSymbol(s:string):TSymbolNode;
    constructor Create(c:char; tkSynSymbol:TSynSymbol;ABrakeType:TSymbBrakeType); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TDefaultSymbols=class(TAbstractSymbol)
     tkSynSymbol:TSynSymbol;
     constructor Create(SynSymb:TSynSymbol); reintroduce; virtual;
     destructor Destroy; override;
     ////TL Duplicate identifier TkSynSymbol... changed to TkSynSymbol1
     function GetToken(parser:TSynUniSyn; var tkSynSymbol1:TSynSymbol):boolean; override;
  end;

  TDefaultTermSymbols=class(TAbstractSymbol)
     tkSynSymbol:TSynSymbol;
     constructor Create(SynSymb:TSynSymbol); virtual;
     ////TL Duplicate identifier TkSynSymbol... changed to TkSynSymbol1
     function GetToken(parser:TSynUniSyn; var tkSynSymbol1:TSynSymbol):boolean; override;
     destructor Destroy; override;
  end;


  TNumberSymbols=class(TAbstractSymbol)
     tkSynSymbol:TSynSymbol;
     constructor Create(SynSymbol:TSynSymbol); virtual;
     ////TL Duplicate identifier TkSynSymbol... changed to TkSynSymbol1
     function GetToken(parser:TSynUniSyn; var tkSynSymbol1:TSynSymbol):boolean; override;
     destructor Destroy; override;
  end;

  TClosingSymbolSet=record
    Symbol:TSynSymbol;
    AllowPredClose:boolean;
  end;
  PClosingSymbolSet=^TClosingSymbolSet;

  TSynRange=class
  private
    fCloseSymbol:TSynSymbol;
    fOpenSymbol: TSynSymbol;
    fCloseOnTerm:boolean;
    fCloseOnEol:boolean;
    FCaseSensitive:boolean;
    fOwner:TSynRange;
//    fClosingSymbols:array of TClosingSymbolSet;
    fClosingSymbol:TClosingSymbolSet;

    fSynSymbols:TList;
    fSynRanges:TList;
    fSymbolGroups:TList;

    fDefaultSynSymbol:TSynSymbol;
    fNumberSymbol:TNumberSymbols;
    fDefaultSymbols:TDefaultSymbols;
    fDefaultTermSymbol:TDefaultTermSymbols;

    fDefaultAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fAttribs:TList;

    fTermSymbols:SymbolsSet;
    SymbolList: array[char] of TAbstractSymbol;

    CaseFunct:function (c:char):char;
    StringCaseFunct:function (const s:string):string;
    fPrepared:boolean;
    FName: string;
  private
    function GetSynSymbol(Index:Integer):TSynSymbol;
    function GetSynRange(Index:Integer):TSynRange;
    function GetSynSymbolGroup(Index:Integer):TSynSymbolGroup;
    function GetRangeCount:Integer;
    function GetSymbolCount:Integer;
    function GetSymbolGroupCount:Integer;

    function GetCaseSensitive: boolean;
    procedure SetCaseSensitive(const Value: boolean);
  public
    ////TL Get rid of default, optional parameters... we'll be explicit in the calls
    ////TL constructor Create(OpenSymbs:string='';CloseSymbs:string=''); virtual;
    constructor Create(OpenSymbs:string; CloseSymbs:string); virtual;
    destructor Destroy; override;

    procedure AddSymbolGroup(SymbolGroup:TSynSymbolGroup);
    procedure AddSymbol(NewSymb:TSynSymbol);
    procedure AddRange(NewSymb:TSynRange);
    function GetSymbol(s:string):TSynSymbol;
    function FindSymbol(s:string):TSynSymbol;
    function FindSymbolOwner(Symbol:TSynSymbol):TSynSymbolGroup;

    procedure DeleteRange(index:integer); overload;
    procedure DeleteRange(SynRange:TSynRange); overload;
    procedure DeleteSymbolGroup(index:integer); overload;
    procedure DeleteSymbolGroup(SymbolGroup:TSynSymbolGroup); overload;
    ////TL FPC errored with duplicate id... changed to Name1 in the following 2 functions
    function AddNewAttribs(Name1:String):TSynHighlighterAttributes;
    function AttribsByName(Name1:string):TSynHighlighterAttributes;
    function AddAttribs(Attri:TSynHighlighterAttributes):integer;
    procedure DeleteAttribs(Idx:integer); overload;
    ////TL FPC errored with duplicate id... changed to Name1
    procedure DeleteAttribs(Name1:string); overload;

    procedure Prepare(Owner:TSynRange);
    procedure Reset;
    procedure Clear;

    Procedure LoadFromStream(aSrc: TStream);
  public
    property TermSymbols:SymbolsSet read fTermSymbols write fTermSymbols;
    property OpenSymbol:TSynSymbol read fOpenSymbol;
    property CloseSymbol:TSynSymbol read fCloseSymbol;
    property CloseOnTerm:boolean read fCloseOnTerm write fCloseOnTerm;
    property CloseOnEol:boolean read fCloseOnEol write fCloseOnEol;

    property Ranges[Index:integer]:TSynRange read GetSynRange;
    property RangeCount:integer read GetRangeCount;
    property Symbols[Index:integer]:TSynSymbol read GetSynSymbol;
    property SymbolCount:integer read GetSymbolCount;
    property SymbolGroups[Index:integer]:TSynSymbolGroup read GetSynSymbolGroup;
    property SymbolGroupCount:Integer read GetSymbolGroupCount;

    property NumberAttri: TSynHighlighterAttributes read fNumberAttri;
    property DefaultAttri: TSynHighlighterAttributes read fDefaultAttri;

    property CaseSensitive:boolean read GetCaseSensitive write SetCaseSensitive;
    property Prepared:boolean read fPrepared;
    property Parent:TSynRange read fOwner;
    property Name:string read FName write FName;
  end;


  TSynUniSyn = class(TSynCustomHighlighter)
  private
    procedure ReadSyntax(Reader: TReader);
    procedure WriteSyntax(Writer: TWriter);
  protected
    fMainRules:TSynRange;

    fEol:boolean;
    fPrEol:boolean;
    fTrueLine: PChar;
    fLine: PChar;
    fLineNumber: Integer;
    Run: LongInt;
    fStringLen: Integer;
    fTokenPos: Integer;

    fCurrToken:TSynSymbol;

    fCurrentRule:TSynRange;
    fSymbols:TSymbols;
    SymbolList: array[char] of TAbstractSymbol;

    fPrepared:boolean;
    procedure SpaceProc;
    procedure NullProc;
    function GetIdentChars: TSynIdentChars; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetSampleSource: string; override;
    procedure SetSampleSource(Value: string); override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEOL: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    ////TL Added the following 3 lines... and the implementation procedure
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override; ////TL: Added 2003-06-11

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: Integer;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;              // DJLP 2000-08-09
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override; ////TL: Replaced line below 2003-06-12
    ////TL replaced by line above procedure SetLine(NewValue: string; LineNumber: Integer); ////TL override;
    procedure SetRange(Value: Pointer); override;
    procedure Reset;
    procedure Clear;
    procedure Prepare;
    Procedure CreateStandardRules;
    Procedure LoadFromStream(aSrc: TStream);
    Procedure SaveToStream(aDst: TStream);
    Procedure LoadFromFile(FileName:string);
    Procedure SaveToFile(FileName:string);
  public
    Info:TInfo;
    property MainRules:TSynRange read fMainRules;
  end;

  TNodeType=(ntRange, ntRootRange, ntKeyWords, ntNone);

Function String2Set(s:string):SymbolsSet;
Function Set2String(s:SymbolsSet):string;
procedure BuildXMLIndexes(xmlInfoTags: TStringList);

const
  DefaultTermSymbols: SymbolsSet = ['*','/','+','-','=','\','|','&','(',')',
    '[',']','{','}','`','~','!','@',',','$','%','^','?',':',';','''','"','.',
    '>','<','#'];

Procedure Register;

implementation

uses
  SynEditStrConst;

procedure BuildXMLIndexes(xmlInfoTags: TStringList);
begin
  xmlInfoTags.Add('AnyTerm');
  xmlInfoTags.Add('Attri');
  xmlInfoTags.Add('Author');
  xmlInfoTags.Add('Back');
  xmlInfoTags.Add('CaseSensitive');
  xmlInfoTags.Add('CloseOnEol');
  xmlInfoTags.Add('CloseOnTerm');
  xmlInfoTags.Add('CloseSymbol');
  xmlInfoTags.Add('Company');
  xmlInfoTags.Add('Copyright');
  xmlInfoTags.Add('Date');
  xmlInfoTags.Add('Def');
  xmlInfoTags.Add('DelimiterChars');
  xmlInfoTags.Add('Email');
  xmlInfoTags.Add('FileTypeName');
  xmlInfoTags.Add('Fore');
  xmlInfoTags.Add('General');
  xmlInfoTags.Add('H');
  xmlInfoTags.Add('History');
  xmlInfoTags.Add('Info');
  xmlInfoTags.Add('KW');
  xmlInfoTags.Add('Layout');
  xmlInfoTags.Add('Name');
  xmlInfoTags.Add('Num');
  xmlInfoTags.Add('OpenSymbol');
  xmlInfoTags.Add('Range');
  xmlInfoTags.Add('Remark');
  xmlInfoTags.Add('Revision');
  xmlInfoTags.Add('S');
  xmlInfoTags.Add('Sample');
  xmlInfoTags.Add('Style');
  xmlInfoTags.Add('Type');
  xmlInfoTags.Add('UniHighlighter');
  xmlInfoTags.Add('Version');
  xmlInfoTags.Add('W');
  xmlInfoTags.Add('Web');
end;

const
  ////TL: Duplicate members: #32=' '... FPC is right... why did Delphi allow it?
  //////////////AbsoluteTermSymbols:SymbolsSet=[' ',#13,#0,#10,#32];
  AbsoluteTermSymbols:SymbolsSet=[' ',#13,#0,#10];

  tkNoRange=1;
  tkNoRangeChange=0;
//  srNoRangeChange:TSymbRangeSet=(RangeValue:tkNoRangeChange;IncludeSymbols:true;);

  xitAnyTerm=0;
  xitAttri=1;
  xitAuthor=2;
  xitBack=3;
  xitCaseSensitive=4;
  xitCloseOnEol=5;
  xitCloseOnTerm=6;
  xitCloseSymbol=7;
  xitCompany=8;
  xitCopyright=9;
  xitDate=10;
  xitDef=11;
  xitDelimiterChars=12;
  xitEmail=13;
  xitFileTypeName=14;
  xitFore=15;
  xitGeneral=16;
  xitH=17;
  xitHistory=18;
  xitInfo=19;
  xitKW=20;
  xitLayout=21;
  xitName=22;
  xitNum=23;
  xitOpenSymbol=24;
  xitRange=25;
  xitRemark=26;
  xitRevision=27;
  xitS=28;
  xitSample=29;
  xitStyle=30;
  xitType=31;
  xitUniHighlighter=32;
  xitVersion=33;
  xitW=34;
  xitWeb=35;

Function String2Set(s:string):SymbolsSet;
  var i:integer;
begin
  result:=[];
  for i:=1 to length(s) do Result:=Result+[s[i]];
end;

Function Set2String(s:SymbolsSet):string;
  var b:byte;
begin
  Result:='';
  for b:=1 to 255 do
    if (chr(b) in s) and (not (chr(b) in AbsoluteTermSymbols)) then
      Result:=Result+chr(b);
end;

Function String2Fs(Style: string):TFontStyles;
begin
  Result := [];
  if Pos('B', Style) > 0 then
    Include( Result, fsBold );
  if Pos('I', Style) > 0 then
    Include( Result, fsItalic );
  if Pos('U', Style) > 0 then
    Include( Result, fsUnderline );
  if Pos('S', Style) > 0 then
    Include( Result, fsStrikeOut );
end;

Function Fs2String(Style:TFontStyles):string;
begin
  Result:='';
  if fsBold in Style then Result:=Result+'B';
  if fsItalic in Style then Result:=Result+'I';
  if fsUnderline in Style then Result:=Result+'U';
  if fsStrikeOut in Style then Result:=Result+'S';
end;

procedure FreeList(var List:TList);
var
 i:integer;
begin
  if List=nil then exit;

  for i:=0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Free;
  List:=nil;
end;

procedure ClearList(List:TList);
var
 i:integer;
begin
  if List=nil then exit;

  for i:=0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function CaseNone(c:char):char;
begin
  Result:=c;
end;

function StringCaseNone(const s:string):string;
begin
  Result:=s;
end;

(*
function CloseRangeComp(CloseRange:PSymbRangeSet;RangeCount:Integer; Range:Integer; var IncludeSymbol:boolean):boolean;
var
 i:integer;
begin
 for i:=1 to RangeCount do
 begin
   if CloseRange^.RangeValue=Range then
   begin
     Result:=True;
     IncludeSymbol:=CloseRange^.IncludeSymbols;
     exit;
   end;
   inc(CloseRange);
 end;
 Result:=False;
end;
*)

{ TSynSymbolGroup }

constructor TSynSymbolGroup.Create(s: string;
  attr: TSynHighlighterAttributes);
begin
  Attribs:=attr;
  KeywordsList:=TStringList.Create;
  KeywordsList.Text:=s;
end;

destructor TSynSymbolGroup.Destroy;
begin
  KeywordsList.Free;
  inherited;
end;


{ TSynSymbol }

constructor TSynSymbol.Create(s: string;
  attribs: TSynHighlighterAttributes);
begin
 attr:=attribs;
 Symbol:=s;
 fOpenRule:=nil;
 BrakeType:=btUnspecified;
// fClosing:=false;
end;

destructor TSynSymbol.Destroy;
begin
  inherited;
end;

{ TSynRange }

procedure TSynRange.AddRange(NewSymb: TSynRange);
begin
  fSynRanges.Add(NewSymb);
end;

procedure TSynRange.AddSymbol(NewSymb: TSynSymbol);
var
 SynSym:TSynSymbol;
begin
  SynSym:=FindSymbol(NewSymb.Symbol);
  if SynSym<>nil then
  begin
    fSynSymbols.Remove(SynSym);
    SynSym.Free;
  end;
  fSynSymbols.Add(NewSymb);
end;

procedure TSynRange.AddSymbolGroup(SymbolGroup: TSynSymbolGroup);
begin
  fSymbolGroups.Add(SymbolGroup);
end;

constructor TSynRange.Create(OpenSymbs:string;CloseSymbs:string);
begin
  fOpenSymbol:=TSynSymbol.Create(OpenSymbs,nil);
  fCloseSymbol:=TSynSymbol.Create(CloseSymbs,nil);

  fDefaultAttri :=TSynHighlighterAttributes.Create(SYNS_AttrDefaultPackage);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);

  FillChar(SymbolList,sizeof(SymbolList),0);
  ////TL Added @ prefix
  CaseFunct:=@CaseNone;
  StringCaseFunct:=@StringCaseNone;

  fPrepared:=false;
  fCloseOnTerm:=false;
  fCloseOnEol:=false;

  fAttribs:=TList.Create;
  fSymbolGroups:=TList.Create;
  fSynSymbols:=TList.Create;
  fSynRanges:=TList.Create;
  fTermSymbols:=DefaultTermSymbols;
end;

destructor TSynRange.Destroy;
begin
//  Reset;
  fOpenSymbol.Free;
  fCloseSymbol.Free;
  FreeList(fSymbolGroups);
  FreeList(fSynSymbols);
  FreeList(fSynRanges);
  FreeList(fAttribs);
  inherited;
end;

function TSynRange.FindSymbol(s: string): TSynSymbol;
var
 i:integer;
begin
  Result:=nil;
  for i:=0 to fSynSymbols.Count-1 do
   if TSynSymbol(fSynSymbols.Items[i]).Symbol=s then
   begin
     Result:=TSynSymbol(fSynSymbols.Items[i]);
     exit;
   end;
end;

function TSynRange.FindSymbolOwner(Symbol: TSynSymbol): TSynSymbolGroup;
var
 i,j:integer;
begin
  Result:=nil;
  for i:=0 to fSymbolGroups.Count-1 do
    if TSynSymbolGroup(fSymbolGroups[i]).KeywordsList.Find(Symbol.Symbol,j) then
    begin
      Result:=TSynSymbolGroup(fSymbolGroups[i]);
      exit;
    end;
end;

function TSynRange.GetRangeCount: Integer;
begin
  Result:=fSynRanges.Count;
end;

function TSynRange.GetSymbol(s: string): TSynSymbol;
begin
  Result:=FindSymbol(s);
end;

function TSynRange.GetSymbolCount: Integer;
begin
 Result:=fSynSymbols.Count;
end;

function TSynRange.GetSymbolGroupCount: Integer;
begin
 Result:=fSymbolGroups.Count;
end;

function TSynRange.GetSynRange(Index: Integer): TSynRange;
begin
 Result:=TSynRange(fSynRanges[Index]);
end;

function TSynRange.GetSynSymbol(Index: Integer): TSynSymbol;
begin
 Result:=TSynSymbol(fSynSymbols[Index]);
end;

function TSynRange.GetSynSymbolGroup(Index: Integer): TSynSymbolGroup;
begin
 Result:=TSynSymbolGroup(fSymbolGroups[Index]);
end;

procedure TSynRange.Prepare(Owner:TSynRange);
var
 i,j:integer;
 SynSymbol:TSynSymbol;
 s:string;
 FirstChar:char;
 BrakeType:TSymbBrakeType;

 procedure SortSymbolList(List:TList);
 var
   i:integer;
   fin:boolean;
 begin
   fin:=False;
   while not fin do
   begin
     fin:=True;
     for i:=0 to List.Count-2 do
       if TSynSymbol(List[i]).Symbol>TSynSymbol(List[i+1]).Symbol then
       begin
         List.Exchange(i,i+1);
         fin:=False;
       end;
   end;
 end;

 function SafeInsertSymbol(Symb:TSynSymbol; Rules:TSynRange; Attribs:TSynHighlighterAttributes):TSynSymbol;
 begin
   Result:=Rules.FindSymbol(Symb.Symbol);
   if Result=nil then
   begin
     Result:=TSynSymbol.Create(Symb.Symbol,Symb.Attr);
     Result.BrakeType:=Symb.BrakeType;
     Rules.AddSymbol(Result);
   end;
   if Result.Attr=nil then
     Result.Attr:=Attribs;
 end;

begin
 Reset;
 fOwner:=Owner;

 fDefaultSynSymbol:=TSynSymbol.Create('',fDefaultAttri);
 fDefaultTermSymbol:=TDefaultTermSymbols.Create(TSynSymbol.Create('',fDefaultAttri));
 fDefaultSymbols:=TDefaultSymbols.Create(TSynSymbol.Create('',fDefaultAttri));
 fNumberSymbol:=TNumberSymbols.Create(TSynSymbol.Create('',fNumberAttri));
 fTermSymbols:=fTermSymbols+AbsoluteTermSymbols;

 //Add all keywords in Symbol list.
 for i:=0 to fSymbolGroups.Count-1 do
   for j:=0 to TSynSymbolGroup(fSymbolGroups[i]).KeywordsList.Count-1 do
     AddSymbol(TSynSymbol.Create(TSynSymbolGroup(fSymbolGroups[i]).KeywordsList[j],TSynSymbolGroup(fSymbolGroups[i]).Attribs));

 //Assign range opening and closing symbols and Prepare range rules.
 for i:=0 to fSynRanges.Count-1 do
 begin
   //Assign range opening symbol
   SynSymbol:=SafeInsertSymbol(TSynRange(fSynRanges[i]).fOpenSymbol,self,TSynRange(fSynRanges[i]).fDefaultAttri);
   SynSymbol.fOpenRule:=TSynRange(fSynRanges[i]);

   //Assing range closing symbols
   SynSymbol:=SafeInsertSymbol(TSynRange(fSynRanges[i]).fCloseSymbol,TSynRange(fSynRanges[i]),TSynRange(fSynRanges[i]).fDefaultAttri);
   TSynRange(fSynRanges[i]).fClosingSymbol.Symbol:=SynSymbol;

   TSynRange(fSynRanges[i]).Prepare(Self);
 end;

 //Build tokens table

 SortSymbolList(fSynSymbols);
 for i:=0 to fSynSymbols.Count-1 do
 begin
   SynSymbol:=TSynSymbol(fSynSymbols[i]);
   if Length(SynSymbol.Symbol)<1 then
     continue;
   s:=SynSymbol.Symbol;
   FirstChar:=s[1];
   if SynSymbol.BrakeType<>btUnspecified then
     BrakeType:=SynSymbol.BrakeType
   else
     if s[Length(s)] in fTermSymbols then
       BrakeType:=btAny
     else
       BrakeType:=btTerm;
   if SymbolList[CaseFunct(FirstChar)]=nil then
   begin
     if Length(s)=1 then
       SymbolList[CaseFunct(FirstChar)]:=TSymbols.Create(FirstChar,SynSymbol,BrakeType)
     else
     begin
       SymbolList[CaseFunct(FirstChar)]:=TSymbols.Create(FirstChar,fDefaultSynSymbol,BrakeType);
       TSymbols(SymbolList[CaseFunct(FirstChar)]).AddSymbol(StringCaseFunct(copy(s,2,Length(s)-1)),SynSymbol,BrakeType);
     end;
   end
   else
   begin
     if Length(s)=1 then
     else
       TSymbols(SymbolList[CaseFunct(FirstChar)]).AddSymbol(StringCaseFunct(copy(s,2,Length(s)-1)),SynSymbol,BrakeType);
   end;
 end;

 //Fill remaining table
 for i:=0 to 255 do
 if SymbolList[char(i)]=nil then
 begin
  if  char(i) in fTermSymbols then
    SymbolList[char(i)]:=fDefaultTermSymbol
  else
  if char(i) in ['0'..'9'] then
    SymbolList[char(i)]:=fNumberSymbol
  else
    SymbolList[char(i)]:=fDefaultSymbols;
 end;

 fPrepared:=true;
end;

function TSynRange.GetCaseSensitive: boolean;
begin
  Result:=FCaseSensitive;//@CaseFunct=@UpCase;
end;

procedure TSynRange.SetCaseSensitive(const Value: boolean);
begin
 FCaseSensitive:=Value;
 if Value then // DiBo33 Removed not so correct function is set
 begin
   ////TL Added @ prefix
   CaseFunct:=@UpCase;
   StringCaseFunct:=@UpperCase;
 end
 else
 begin
   CaseFunct:=@CaseNone;
   StringCaseFunct:=@StringCaseNone;
 end;
end;


function TSynRange.AddAttribs(Attri: TSynHighlighterAttributes): integer;
begin
 Result:=fAttribs.Add(Attri);
end;

function TSynRange.AddNewAttribs(Name1: String): TSynHighlighterAttributes;
begin
 Result:=TSynHighlighterAttributes.Create(Name1);
 fAttribs.Add(Result);
end;

function TSynRange.AttribsByName(Name1: string): TSynHighlighterAttributes;
var
 i:integer;
begin
 Result:=nil;
 for i:=0 to fAttribs.Count-1 do
   if TSynHighlighterAttributes(fAttribs[i]).Name=Name1 then
   begin
     Result:=TSynHighlighterAttributes(fAttribs[i]);
     exit;
   end;
end;

procedure TSynRange.DeleteAttribs(Idx: integer);
begin
 TSynHighlighterAttributes(fAttribs[Idx]).Free;
 fAttribs.Delete(Idx);
end;

procedure TSynRange.DeleteAttribs(Name1:string);
var
  p:TSynHighlighterAttributes;
begin
  p:=AttribsByName(Name1);
  p.Free;
  fAttribs.Remove(p);
end;

procedure TSynRange.Reset;
var
 i:integer;
begin
 if not fPrepared then
   exit;
 fDefaultSynSymbol.Free;
 fDefaultTermSymbol.Free;
 fDefaultSymbols.Free;
 fNumberSymbol.Free;

 for i:=0 to 255 do
   SymbolList[char(i)]:=nil;

 for i:=0 to fSynRanges.Count-1 do
   TSynRange( fSynRanges[i] ).Reset;


 ClearList(fSynSymbols);

 fPrepared:=False;
end;

procedure TSynRange.Clear;
var
 i:integer;
begin
 Reset;
 for i:=0 to fSynRanges.Count-1 do
   TSynRange(fSynRanges[i]).Clear;

 ClearList(fSynRanges);
 ClearList(fSynSymbols);
 ClearList(fSymbolGroups);
 ClearList(fAttribs);
end;

procedure TSynRange.LoadFromStream(aSrc: TStream);
var
  buf,sav:PChar;
  BufSize:Integer;
  CurTagIndex:Integer;
  LineNumber:integer;
  Param:string;
  xmlInfoTags:TStringList;

  ////TL Guess FPC doesn't support optional parameters. Never used this in Delphi or Kylix.
  ////TL I'm going to remove them and explicitly state the parms in each call.
  ////TL function GetNextTag(var Idx:Integer; var TagParam:string; IgnoreUnknown:boolean=false):boolean;
  function GetNextTag(var Idx:Integer; var TagParam:string; IgnoreUnknown:boolean):boolean;
  var
   s:string;
   sPos:PChar;
  begin
    Idx:=-1;
    Result:=True;
    TagParam:='';
    while buf^<>'<' do
    begin
      if buf^=#0 then exit;
      if buf^=#13 then Inc(LineNumber);
      Inc(buf);
    end;
    Inc(buf);
    while (buf^=' ') or (buf^=#32) do
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber))
      else Inc(buf);

    if buf^='/' then
    begin
      Result:=False;
      inc(buf);
    end;
    sPos:=buf;
    while (buf^<>#32) and (buf^<>'>') do
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber))
      else Inc(buf);
    SetLength(s,Cardinal(buf)-Cardinal(sPos));
    move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));

    if (not xmlInfoTags.Find(s,Idx)) then
     if (not IgnoreUnknown) then
       raise Exception.Create('Tag "'+s+'" is unknown (line '+IntToStr(LineNumber) +')')
     else
     begin
       Idx:=-1; Result:=True;
       exit;
     end;

    while buf^<>'>' do
    begin
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber));
      if buf^='"' then
      begin
        inc(buf);
        sPos:=buf;
        while (buf^<>'"') do
          if buf^=#0 then begin Result:=False; exit; end
          else Inc(buf);
        SetLength(TagParam,Cardinal(buf)-Cardinal(sPos));
        move(sPos^,pointer(TagParam)^,Cardinal(buf)-Cardinal(sPos));
      end;
      Inc(buf);
    end;
    Inc(buf);
  end;

  function GetReplacement:string;
  var
    sPos:PChar;
  begin
    Result:='';
    sPos:=buf;
    inc(buf);
    if buf^='l' then begin
      Inc(buf);
      if buf^='t' then begin
        Inc(buf);
        if buf^=';' then
          Result:='<';
      end;
    end
    else
    if buf^='g' then begin
      Inc(buf);
      if buf^='t' then begin
        Inc(buf);
        if buf^=';' then
          Result:='>';
      end;
    end
    else
    if buf^='q' then begin
      Inc(buf);
      if buf^='t' then begin
        Inc(buf);
        if buf^=';' then
          Result:='"';
      end;
    end
    else
    if buf^='a' then begin
      Inc(buf);
      if buf^='m' then begin
        Inc(buf);
        if buf^='p' then begin
          Inc(buf);
          if buf^=';' then
            Result:='&';
        end;
      end;
    end;
    if Result='' then
    begin
      Dec(buf);
      SetLength(Result,Cardinal(buf)-Cardinal(sPos));
      Move(sPos^,pointer(Result)^,Cardinal(buf)-Cardinal(sPos));
    end
    else
     Inc(buf);
  end;

  function GetData(TagIndex:integer):string;
  var
    s:string;
    sPos:PChar;
    idx:Integer;
  begin
    Result:='';
    sPos:=buf;
    while buf^<>'<' do
    begin
      if buf^='&' then
      begin
        SetLength(s,Cardinal(buf)-Cardinal(sPos));
        move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));
        Result:=Result+s+GetReplacement;
        sPos:=buf;
      end
      else
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber))
      else Inc(buf);
    end;
    SetLength(s,Cardinal(buf)-Cardinal(sPos));
    Move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));
    Result:=Result+s;
    ////TL added the third parameter
    if (GetNextTag(idx,s,false)) or (idx<>CurTagIndex) then
       raise Exception.Create('Close tag: /'+xmlInfoTags[idx]+' is not found. Line '+IntToStr(LineNumber));
  end;

  procedure ReadInfo;

    procedure ReadGeneral;
    begin
    ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param, false) do
      begin
        case CurTagIndex of
           xitName: GetData(xitName);
           xitFileTypeName: GetData(xitFileTypeName);
           xitLayout: GetData(xitLayout);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitGeneral then
         raise Exception.Create('Unexpected tag: /'+xmlInfoTags[curTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadVersion;
      function GetType(s:string):TVersionType;
      begin
        if s='Beta' then Result:=vtBeta else
        if s='Release' then Result:=vtRelease else
            Result:=vtInternalTest;
      end;
    begin
    ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param, false) do
      begin
        case CurTagIndex of
            xitVersion: GetData(xitVersion);
            xitRevision: GetData(xitRevision);
            xitDate: GetData(xitDate);
            xitType: GetData(xitType);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitVersion then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadAuthor;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
            xitName: GetData(xitName);
            xitEmail: GetData(xitEmail);
            xitWeb: GetData(xitWeb);
            xitCopyright: GetData(xitCopyright);
            xitCompany: GetData(xitCompany);
            xitRemark: GetData(xitRemark);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitAuthor then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadHistroy;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
            xitH: GetData(xitH);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitHistory then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadSample;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
           xitS: GetData(xitS);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitSample then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

  begin
    ////TL added the third parameter
    while GetNextTag(CurTagIndex,Param,false) do
    begin
      case CurTagIndex of
        xitGeneral: ReadGeneral;
        xitVersion: ReadVersion;
        xitAuthor: ReadAuthor;
        xitHistory:  ReadHistroy;
        xitSample: ReadSample;
      else
        raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex<>xitInfo then
      raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
  end;

  procedure ReadKW(SymbGr:TSynSymbolGroup);

    procedure ReadAttri;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
          xitBack: SymbGr.Attribs.Background:=strtointdef(GetData(xitBack),0);
          xitFore: SymbGr.Attribs.Foreground:=strtointdef(GetData(xitFore),0);
          xitStyle:SymbGr.Attribs.Style:=String2Fs(GetData(xitStyle));
        else
           raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitAttri then
        raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

  begin
    ////TL added the third parameter
    while GetNextTag(CurTagIndex,Param,false) do
    begin
      case CurTagIndex of
        xitAttri: ReadAttri;
        xitW: SymbGr.KeywordsList.Add(GetData(xitW));
      else
         raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex<>xitKW then
      raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
  end;


  procedure ReadRange(CurRange:TSynRange);
    var
      NewRange:TSynRange;
      NewSymbolGroup:TSynSymbolGroup;

    procedure ReadDef;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
          xitBack: CurRange.DefaultAttri.Background:=strtointdef(GetData(xitBack),0);
          xitFore: CurRange.DefaultAttri.Foreground:=strtointdef(GetData(xitFore),0);
          xitStyle: CurRange.DefaultAttri.Style:=String2Fs( GetData(xitStyle) );
        else
           raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitDef then
        raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadNum;
    begin
      ////TL added the third parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
          xitBack: CurRange.NumberAttri.Background:=strtointdef(GetData(xitBack),0);
          xitFore: CurRange.NumberAttri.Foreground:=strtointdef(GetData(xitFore),0);
          xitStyle: CurRange.DefaultAttri.Style:=String2Fs(GetData(xitStyle));
        else
           raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitNum then
        raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

  begin
    ////TL added the third parameter
    while GetNextTag(CurTagIndex,Param,false) do
    begin
      case CurTagIndex of
        xitDef: ReadDef;
        xitOpenSymbol: CurRange.OpenSymbol.Symbol:=GetData(xitOpenSymbol);
        xitCloseSymbol: CurRange.CloseSymbol.Symbol:=GetData(xitCloseSymbol);
        xitCloseOnTerm: CurRange.CloseOnTerm:=lowercase(GetData(xitCloseOnTerm))='true';
        xitCloseOnEol: CurRange.CloseOnEol:=lowercase(GetData(xitCloseOnEol))='true';
        xitAnyTerm: if Lowercase(GetData(xitAnyTerm))='true' then
                      CurRange.OpenSymbol.BrakeType:=btAny
                    else
                      CurRange.OpenSymbol.BrakeType:=btTerm;
        xitDelimiterChars: CurRange.TermSymbols:=String2Set(GetData(xitDelimiterChars));
        xitNum: ReadNum;
        xitCaseSensitive: CurRange.CaseSensitive:=lowercase(GetData(xitCaseSensitive))='true';
        xitKW: begin
                 NewSymbolGroup:=TSynSymbolGroup.Create('',CurRange.AddNewAttribs('unknown'));
                 NewSymbolGroup.Name:=Param;
                 CurRange.AddSymbolGroup(NewSymbolGroup);
                 ReadKW(NewSymbolGroup);
               end;
        xitRange:
                 begin
                    ////TL added two empty string parameters to explicitly match the
                    ////TL modified declaration.
                    NewRange:=TSynRange.Create('','');
                    NewRange.Name:=Param;
                    CurRange.AddRange(NewRange);
                    ReadRange(NewRange);
                 end;
      else
        raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex<>xitRange then
      raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
  end;

begin
  Clear;

  try
    BufSize:= aSrc.Size;
    GetMem(buf,BufSize);
    aSrc.ReadBuffer(buf^,BufSize);
  except
    FreeMem(buf);
    raise;
  end;
  sav:=buf;
  LineNumber:=0;

  xmlInfoTags := nil;
  try
    xmlInfoTags := TStringList.Create;
    BuildXMLIndexes( xmlInfoTags );
    ////TL added the third parameter
    if (not GetNextTag(CurTagIndex,Param,false)) or
    (CurTagIndex<>xitUniHighlighter) then
      raise exception.Create('Highlighter header tag ("<UniHighlighter>") is not found.');

    while GetNextTag(CurTagIndex,Param,True) do
    begin
      case CurTagIndex of
        xitInfo: ReadInfo;
        xitRange: ReadRange(self);
        xitCopyRight: GetData(xitCopyRight);
      end;
    end;
    if CurTagIndex<>xitUniHighlighter then
      raise Exception.Create('Closing tag: /'+xmlInfoTags[xitUniHighlighter]+' was not found. Line '+IntToStr(LineNumber));
  finally
    FreeMem(sav);
    xmlInfoTags.Free;
  end;
end;

{ TSymbolList }

procedure TSymbolList.AddSymbol(symb: TSymbolNode);
begin
 SymbList.Add(symb);
end;

constructor TSymbolList.Create;
begin
 SymbList:=TList.Create;
end;

destructor TSymbolList.Destroy;
begin
 FreeList(SymbList);
 inherited;
end;

function TSymbolList.FindSymbol(c: char): TSymbolNode;
var
 i:integer;
begin
 Result:=nil;
 for i:=0 to SymbList.Count-1 do
   if TSymbolNode(SymbList[i]).c=c then
   begin
     Result:=TSymbolNode(SymbList[i]);
     break;
   end;
end;

function TSymbolList.GetCount: integer;
begin
 Result:=SymbList.Count
end;

function TSymbolList.GetSymbolNode(Index: integer): TSymbolNode;
begin
  Result:=TSymbolNode(SymbList[index]);
end;

procedure TSymbolList.SetSymbolNode(Index: Integer; Value: TSymbolNode);
begin
 if Index<SymbList.Count then
   TSymbolNode(SymbList[index]).Free;
 SymbList[index]:=Value;
end;

{ TSymbols }

procedure TSymbols.AddSymbol(s:string; tkSynSymbol:TSynSymbol; ABrakeType: TSymbBrakeType);
var
  i:integer;
  l:integer;
  Node:TSymbolNode;
  SList:TSymbolList;
begin
  SList:=HeadNode.NextSymbs;
  Node:=nil;
  l:=Length(s);
  for i:=1 to l do
  begin
    Node:=SList.FindSymbol(s[i]);
    if Node=nil then
    begin
      Node:=TSymbolNode.Create(s[i]);
      SList.AddSymbol(Node);
    end;
    SList:=Node.NextSymbs;
  end;
  Node.BrakeType:=ABrakeType;
  Node.tkSynSymbol:=tkSynSymbol;
end;

constructor TSymbols.Create(c: char; tkSynSymbol: TSynSymbol;
  ABrakeType: TSymbBrakeType);
begin
 HeadNode:=TSymbolNode.Create(c,tkSynSymbol,ABrakeType);
end;

destructor TSymbols.Destroy;
begin
  HeadNode.Free;
  inherited;
end;

function TSymbols.FindSymbol(s:string): TSymbolNode;
var
  i:integer;
  l:integer;
  Node,prvNode:TSymbolNode;
begin
  Node:=HeadNode;
  l:=Length(s);
  for i:=1 to l do
  begin
    prvNode:=Node.NextSymbs.FindSymbol(s[i]);
    if prvNode=nil then
      break;
    Node:=prvNode;
  end;
  Result:=Node;
end;

function TSymbols.GetToken(parser: TSynUniSyn; var tkSynSymbol:TSynSymbol):boolean;
var
 Node,nxtNode:TSymbolNode;
begin
 Result:=false;
 Node:=HeadNode;
 nxtNode:=nil;

 while (Node.NextSymbs.Count>0) and (parser.fLine[parser.Run]<>#0) do
 begin
   inc(parser.Run);
   nxtNode:=Node.NextSymbs.FindSymbol(parser.fLine[parser.Run]);
   if nxtNode=nil then
     break;
   Node:=nxtNode;
 end;

 if Node.tkSynSymbol=nil then
  exit;

 if (nxtNode=nil) and (Node.NextSymbs.Count>0) then
    dec(parser.Run);

 if parser.fLine[parser.Run]<>#0 then
   inc(parser.Run);

 if Node.BrakeType=btAny then
 begin
   Result:=True;
   tkSynSymbol:=Node.tkSynSymbol;
   exit;
 end;

 if parser.fLine[parser.Run] in parser.fCurrentRule.fTermSymbols then
 begin
   Result:=True;
   tkSynSymbol:=Node.tkSynSymbol;
 end;

end;

{ TSymbolNode }

constructor TSymbolNode.Create(AC: char; SynSymbol:TSynSymbol;
  ABrakeType: TSymbBrakeType);
begin
 c:=AC;
 NextSymbs:=TSymbolList.Create;
 BrakeType:=ABrakeType;
 tkSynSymbol:=SynSymbol;
end;

constructor TSymbolNode.Create(AC: char);
begin
 c:=AC;
 NextSymbs:=TSymbolList.Create;
 tkSynSymbol:=nil;
end;

destructor TSymbolNode.Destroy;
begin
  NextSymbs.Free;
  inherited;
end;

{ TDefaultSymbols }

constructor TDefaultSymbols.Create(SynSymb:TSynSymbol);
begin
 tkSynSymbol:=SynSymb;
end;

destructor TDefaultSymbols.Destroy;
begin
 tkSynSymbol.Free;
 inherited;
end;


function TDefaultSymbols.GetToken(parser:TSynUniSyn; var tkSynSymbol1:TSynSymbol): boolean;
begin
  inc(parser.Run);
  Result:=False;
end;

{ TNumberSymbols }

constructor TNumberSymbols.Create(SynSymbol: TSynSymbol);
begin
  tkSynSymbol:=SynSymbol;
end;

destructor TNumberSymbols.Destroy;
begin
  tkSynSymbol.Free;
  inherited;
end;

function TNumberSymbols.GetToken(parser:TSynUniSyn; var tkSynSymbol1:TSynSymbol): boolean;
begin
 repeat
   Inc(Parser.Run);
 until not (parser.fLine[parser.Run] in ['0'..'9']);
 if parser.fLine[parser.Run] in parser.fCurrentRule.fTermSymbols then
 begin
   Result:=True;
   tkSynSymbol1:=self.tkSynSymbol;
 end
 else
   Result:=false;
end;

{ TDefaultTermSymbols }

constructor TDefaultTermSymbols.Create(SynSymb:TSynSymbol);
begin
 tkSynSymbol:=SynSymb;
end;

destructor TDefaultTermSymbols.Destroy;
begin
 tkSynSymbol.Free;
 inherited;
end;

function TDefaultTermSymbols.GetToken(parser: TSynUniSyn;var tkSynSymbol1:TSynSymbol): boolean;
begin
  if parser.fLine[parser.Run]<>#0 then
     Inc(parser.Run);
  tkSynSymbol1:=self.tkSynSymbol;
  Result:=True;
end;

{ TSynUniSyn }

constructor TSynUniSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Info.History:=TStringList.Create;
  Info.Sample:=TStringList.Create;
  fPrepared:=false;
  ////TL added two empty string parameters to match the
  ////TL modified declaration.
  fMainRules:=TSynRange.Create('','');
  fMainRules.Name:=_Root;
  fEol:=false;
  fPrEol:=false;
  fCurrentRule:=fMainRules;
end;

destructor TSynUniSyn.Destroy;
begin
 fMainRules.Free;
 Info.History.Free;
 Info.Sample.Free;
 inherited;
end;

////TL Replaced to reflect declaration: procedure TSynUniSyn.SetLine(NewValue: string; LineNumber: Integer);
procedure TSynUniSyn.SetLine(const NewValue: String; LineNumber:Integer);
var
 l,i:integer;
begin
  if not fCurrentRule.fPrepared then
    Prepare;
  fTrueLine := PChar(NewValue);
  l:=Length(NewValue);
  GetMem(fLine,l+1);
  for i:=0 to l do
    fLine[i]:=fCurrentRule.CaseFunct(fTrueLine[i]);
  Run := 0;
  fTokenPos:=0;
  fLineNumber := LineNumber;
  fEol:=false;
  fPrEol:=false;
  Next;
end;

procedure TSynUniSyn.SpaceProc;
begin
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

{begin}                                                                         // DJLP 2000-08-09
function TSynUniSyn.IsKeyword(const AKeyword: string): boolean;
begin
  Result := fSymbols.FindSymbol(AKeyword) <> nil;
end;
{end}                                                                           // DJLP 2000-08-09

procedure TSynUniSyn.Next;
begin
  if fPrEol then
  begin
    if (fCurrentRule.fCloseOnEol) or (fCurrentRule.fCloseOnTerm) then
      fCurrentRule:=fCurrentRule.fOwner;
    fEol:=True;
    exit;
  end;

  fTokenPos := Run;
  if (fCurrentRule.fCloseOnTerm) and (fLine[Run] in fCurrentRule.fTermSymbols) then
    fCurrentRule:=fCurrentRule.fOwner;

  if not fCurrentRule.SymbolList[fLine[Run]].GetToken(self,fCurrToken) then
  begin
    fCurrToken:=fCurrentRule.fDefaultSynSymbol;
    while not (fLine[Run] in fCurrentRule.fTermSymbols) do
      inc(Run);
  end
  else
  if fCurrentRule.fClosingSymbol.Symbol=fCurrToken then
    fCurrentRule:=fCurrentRule.fOwner
  else
  if fCurrToken.fOpenRule<>nil then
    fCurrentRule:=fCurrToken.fOpenRule;

  if fLine[Run]=#0 then
    fPrEol:=True;
end;

function TSynUniSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
 case Index of
    SYN_ATTR_COMMENT: Result := fCurrentRule.fDefaultAttri;
    SYN_ATTR_IDENTIFIER: Result := fCurrentRule.fDefaultAttri;
    SYN_ATTR_KEYWORD: Result := fCurrentRule.fDefaultAttri;
    SYN_ATTR_STRING: Result := fCurrentRule.fDefaultAttri;
    SYN_ATTR_WHITESPACE: Result := fCurrentRule.fDefaultAttri;
    else Result := nil;
  end;
end;

function TSynUniSyn.GetEOL: Boolean;
begin
  Result := fEol;
end;

function TSynUniSyn.GetRange: Pointer;
begin
  Result := fCurrentRule;
end;

function TSynUniSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Setstring(Result, (FTrueLine + fTokenPos), Len);
end;

////TL 2003-06-12: Added the following to satisfy abstract method override
procedure TSynUniSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynUniSyn.GetTokenID: Integer;
begin
  Result :=1;// CODE_REVIEW fCurrToken.ID;
end;

function TSynUniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
 Result:=fCurrToken.Attr;
end;

function TSynUniSyn.GetTokenKind: integer;
begin
  Result :=1;// CODE_REVIEW   fCurrToken.ID;
end;

function TSynUniSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynUniSyn.ResetRange;
begin
  fCurrentRule := fMainRules;
end;

procedure TSynUniSyn.SetRange(Value: Pointer);
begin
  fCurrentRule:=TSynRange(Value);
end;

class function TSynUniSyn.GetLanguageName: string;
begin
  Result := 'UniLenguage';
end;

procedure TSynUniSyn.Clear;
begin
  MainRules.Clear;
end;

procedure TSynUniSyn.CreateStandardRules;
var r:TSynRange;
    kw:TSynSymbolGroup;

begin
  self.MainRules.Clear;
  self.MainRules.DefaultAttri.Foreground:=clBlack;
  self.MainRules.DefaultAttri.Background:=clWhite;
  self.MainRules.NumberAttri.Foreground:=clMaroon;
  self.MainRules.NumberAttri.Background:=clWhite;
  self.MainRules.CaseSensitive:=false;

  r:=TSynRange.Create('''','''');
  r.Name:='Strings ''..''';
  r.DefaultAttri.Foreground:=clRed;
  r.DefaultAttri.Background:=clWhite;
  r.NumberAttri.Foreground:=clRed;
  r.NumberAttri.Background:=clWhite;
  r.CaseSensitive:=false;
  r.OpenSymbol.BrakeType:=btAny;
  self.MainRules.AddRange(r);

  r:=TSynRange.Create('"','"');
  r.Name:='Strings ".."';
  r.DefaultAttri.Foreground:=clRed;
  r.DefaultAttri.Background:=clWhite;
  r.NumberAttri.Foreground:=clRed;
  r.NumberAttri.Background:=clWhite;
  r.CaseSensitive:=false;
  r.OpenSymbol.BrakeType:=btAny;
  self.MainRules.AddRange(r);

  r:=TSynRange.Create('{','}');
  r.Name:='Remarks {..}';
  r.DefaultAttri.Foreground:=clNavy;
  r.DefaultAttri.Background:=clWhite;
  r.NumberAttri.Foreground:=clNavy;
  r.NumberAttri.Background:=clWhite;
  r.CaseSensitive:=false;
  r.OpenSymbol.BrakeType:=btAny;
  self.MainRules.AddRange(r);

  r:=TSynRange.Create('(*','*)');
  r.Name:='Remarks (*..*)';
  r.DefaultAttri.Foreground:=clNavy;
  r.DefaultAttri.Background:=clWhite;
  r.NumberAttri.Foreground:=clNavy;
  r.NumberAttri.Background:=clWhite;
  r.CaseSensitive:=false;
  r.OpenSymbol.BrakeType:=btAny;
  self.MainRules.AddRange(r);

  r:=TSynRange.Create('/*','*/');
  r.Name:='Remarks /*..*/';
  r.DefaultAttri.Foreground:=clNavy;
  r.DefaultAttri.Background:=clWhite;
  r.NumberAttri.Foreground:=clNavy;
  r.NumberAttri.Background:=clWhite;
  r.CaseSensitive:=false;
  r.OpenSymbol.BrakeType:=btAny;
  self.MainRules.AddRange(r);

  kw:=TSynSymbolGroup.Create('',TSynHighlighterAttributes.Create('unknown'));
  kw.Name:='Key words';
  kw.Attribs.Foreground:=clGreen;
  kw.Attribs.Background:=clWhite;
  self.MainRules.AddSymbolGroup(kw);
end;

procedure TSynUniSyn.Prepare;
begin
 fMainRules.Prepare(fMainRules);
end;


procedure TSynUniSyn.NullProc;
begin
// fEol:=True;
end;

procedure TSynUniSyn.Reset;
begin
 fMainRules.Reset;
end;

procedure TSynUniSyn.SaveToStream(aDst: TStream);

  procedure WriteString(const aStr: string);
  begin
    aDst.Write( aStr[1], Length(aStr) );
    aDst.Write( #10#13, 1 );
  end;

  Function Indent(i:integer):string;
  begin
    SetLength( Result, i );
    FillChar( Result[1], i, #32 );
  end;

  Function GetValidValue(Value:string):string;
  begin
    Value:=StringReplace(Value,'&','&amp;',[rfReplaceAll, rfIgnoreCase]);
    Value:=StringReplace(Value,'<','&lt;',[rfReplaceAll, rfIgnoreCase]);
    Value:=StringReplace(Value,'"','&qt;',[rfReplaceAll, rfIgnoreCase]);
    Result:=StringReplace(Value,'>','&gt;',[rfReplaceAll, rfIgnoreCase]);
  end;

  Procedure InsertTag(Ind:integer; Name:string; Value:string);
  begin
    WriteString( Format('%s<%s>%s</%s>',[Indent(Ind),Name,GetValidValue(Value),Name]) );
  end;

  ////TL Removed the default, optional parameters...
  ////TL and will explicitly state them in calls.
  ////TL Procedure OpenTag(Ind:integer; Name:string;Param:string='';ParamValue:string='');
  Procedure OpenTag(Ind:integer; Name:string;Param:string;ParamValue:string);
  begin
    if Param='' then
      WriteString(Format('%s<%s>',[Indent(Ind),Name]))
    else
      WriteString(Format('%s<%s %s="%s">',[Indent(Ind),Name, Param, GetValidValue(ParamValue)]));
  end;

  Procedure SaveColor(MainTag:string; Ind, Fore, Back:integer; Style:TFontStyles);
  begin
    ////TL Add missing null parameters
    OpenTag(Ind, MainTag,'','');
    InsertTag(Ind+1, 'Back', Inttostr(Back));
    InsertTag(Ind+1, 'Fore', Inttostr(Fore));
    InsertTag(Ind+1, 'Style', Fs2String(Style));
    ////TL Add missing null parameters
    OpenTag(Ind, '/'+MainTag,'','');
  end;

  Procedure SaveKWGroup(Ind:integer;G:TSynSymbolGroup);
    var i:integer;
  begin
    OpenTag(Ind, 'KW', 'Name', G.Name);
    SaveColor('Attri',Ind+1, G.Attribs.Foreground, G.Attribs.Background, G.Attribs.Style);
    For i:=0 to G.KeywordsList.Count-1 do InsertTag(Ind+1, 'W', G.KeywordsList[i]);
    ////TL Add missing null parameters
    OpenTag(Ind, '/KW','','');
  end;

  Procedure SaveRange(Ind:integer;R:TSynRange);
  var i:integer;
    Procedure InsertTagBool(Ind:integer; Name:string; Value:Boolean);
    begin
      if Value then
        WriteString(Format('%s<%s>True</%s>',[Indent(Ind),Name,Name]))
      else
        WriteString(Format('%s<%s>False</%s>',[Indent(Ind),Name,Name]))
    end;

    Procedure SaveRangeColor(Ind:integer;R:TSynRange);
    begin
      SaveColor('Def', Ind, R.DefaultAttri.Foreground, R.DefaultAttri.Background, R.DefaultAttri.Style);
      SaveColor('Num', Ind, R.NumberAttri.Foreground, R.NumberAttri.Background, R.NumberAttri.Style);
    end;

  begin
    OpenTag(Ind, 'Range', 'Name',R.Name);
    SaveRangeColor(Ind, R);
    InsertTag(Ind, 'OpenSymbol', R.OpenSymbol.Symbol);
    InsertTag(Ind, 'CloseSymbol', R.CloseSymbol.Symbol);

    InsertTag(Ind, 'DelimiterChars', Set2String(R.TermSymbols));
    if R.OpenSymbol.BrakeType=btAny then
      InsertTag(Ind, 'AnyTerm', 'True')
    else
      InsertTag(Ind, 'AnyTerm', 'False');
    InsertTagBool(Ind, 'CloseOnTerm', R.CloseOnTerm);
    InsertTagBool(Ind, 'CloseOnEol', R.CloseOnEol);
    InsertTagBool(Ind, 'CaseSensitive', R.CaseSensitive);
    For i:=0 to R.SymbolGroupCount-1 do SaveKWGroup(Ind, R.SymbolGroups[i]);
    For i:=0 to R.RangeCount-1 do SaveRange(Ind+1, R.Ranges[i]);
    ////TL Add missing null parameters
    OpenTag(Ind, '/Range','','');
  end;

  Procedure SaveInfo;
    var i:integer;
  begin
    ////TL Add missing null parameters to all the OpenTag calls
    OpenTag(1, 'Info','','');

    OpenTag(2, 'General','','');
    InsertTag(3, 'Name', info.General.Name);
    InsertTag(3, 'FileTypeName', info.General.FileTypeName);
    InsertTag(3, 'Layout', info.General.Layout);
    OpenTag(2, '/General','','');

    OpenTag(2, 'Author','','');
    InsertTag(3, 'Name', info.Author.Name);
    InsertTag(3, 'Email', info.Author.Email);
    InsertTag(3, 'Web', info.Author.Web);
    InsertTag(3, 'Copyright', info.Author.Copyright);
    InsertTag(3, 'Company', info.Author.Company);
    InsertTag(3, 'Remark', info.Author.Remark);
    OpenTag(2, '/Author','','');

    OpenTag(2, 'Version','','');
    InsertTag(3, 'Version', inttostr(Info.Version.Version));
    InsertTag(3, 'Revision', inttostr(Info.Version.Revision));
    InsertTag(3, 'Date', floattostr(Info.Version.ReleaseDate));
    case Info.Version.VersionType of
      vtInternalTest: InsertTag(3, 'Type', 'Internal Test');
      vtBeta: InsertTag(3, 'Type', 'Beta');
      vtRelease: InsertTag(3, 'Type', 'Release');
    end;
    OpenTag(2, '/Version','','');

    OpenTag(2, 'History','','');
    for i:=0 to Info.history.count-1 do InsertTag(3, 'H', Info.history[i]);
    OpenTag(2, '/History','','');

    OpenTag(2, 'Sample','','');
    for i:=0 to Info.Sample.count-1 do InsertTag(3, 'S', Info.Sample[i]);
    OpenTag(2, '/Sample','','');

    OpenTag(1, '/Info','','');
  end;

begin
  OpenTag(0, 'UniHighlighter','','');
  OpenTag(1, 'ImportantInfo','','');
  WriteString(Indent(2)+'******* Please read carefully *************************');
  WriteString(Indent(2)+'* Please, make any changes in this file very carefuly!*');
  WriteString(Indent(2)+'* It is much more convinient to use native designer!  *');
  WriteString(Indent(2)+'*******************************************************');
  OpenTag(1, '/ImportantInfo','','');
  SaveInfo;
  SaveRange(1, self.MainRules);
  InsertTag(1, 'CopyRight','Rule file for UniHighlighter Delphi component (Copyright(C) Fantasist(walking_in_the_sky@yahoo.com), Vit(nevzorov@yahoo.com), 2002)');
  OpenTag(0, '/UniHighlighter','','');
end;


procedure TSynUniSyn.LoadFromStream(aSrc: TStream);
{  type
    TParserState=(psTagWaiting,psTagReading,psDataReading);

  type TParsingSection=(psNone, psInitialized, psMainRange, psRange);
   Range(Name="") OpenSymbol CloseSymbol  KW(name="")


 <Info>
  <General>
   <Name>Oemsetup Script</Name>
   <FileTypeName>INF</FileTypeName>
   <Layout>Standard</Layout>
  </General>
  <Author>
   <Name>Vitaly Nevzorov</Name>
   <Email>nevzorov@yahoo.com</Email>
   <Web>www.delphist.com</Web>
   <Copyright>Copyright (c) Vitaly Nevzorov, 2002</Copyright>
   <Company>N/A</Company>
   <Remark>Created based on UltraEdit:oemsetup.txt</Remark>
  </Author>
  <Version>
   <Version>1</Version>
   <Revision>0</Revision>
   <Date>37612.555991169</Date>
   <Type>Beta</Type>
  </Version>
  <History>
  </History>
  <Sample>
   <S></S>
  </Sample>
 </Info>

  var f:textfile;
      s, rest:string;
      R:TSynRange;
      c:TComponent;
      ParsingSection:TParsingSection;

  Function isEqual(const s1,s2:string):boolean;
  begin
    result:=trim(lowercase(s1))=trim(lowercase(s2));
  end;

  Function isPresent(str, substr:string):boolean;
  begin
    str:=trim(lowercase(str));
    substr:=trim(lowercase(substr));
    str:=copy(str,1,length(substr));
    Result:=str=substr;
  end;

  Procedure LookForInitialization(s:string);
  begin
    if isPresent(s,'<UniHighlighter>') then
      begin
        Clear;
        ParsingSection:=psInitialized;
      end;
  end;

  Function GetValue(Str:string):string;
  var temp:string;
  begin
    temp:=copy(str,pos('>',str)+1,length(str));
    result:=copy(temp,1,pos('<',temp)-1);
    result:=StringReplace(result,'&qt;','"',[rfReplaceAll, rfIgnoreCase]);
    result:=StringReplace(result,'&lt;','<',[rfReplaceAll, rfIgnoreCase]);
    result:=StringReplace(result,'&gt;','>',[rfReplaceAll, rfIgnoreCase]);
    result:=StringReplace(result,'&amp;','&',[rfReplaceAll, rfIgnoreCase]);
  end;

  Function GetName(Str:string):string;
  var temp:string;
  begin
    temp:=copy(str,pos('"',str)+1,length(str));
    result:=copy(temp,1,pos('"',temp)-1);
    result:=StringReplace(result,'&qt;','"',[rfReplaceAll, rfIgnoreCase]);
    result:=StringReplace(result,'&lt;','<',[rfReplaceAll, rfIgnoreCase]);
    result:=StringReplace(result,'&gt;','>',[rfReplaceAll, rfIgnoreCase]);
    result:=StringReplace(result,'&amp;','&',[rfReplaceAll, rfIgnoreCase]);
  end;

  Procedure LoadRange(R:TSynRange);
    var rr:TSynRange;
        kw:TSynSymbolGroup;
    label 1;
  begin
    R.TermSymbols:=AbsoluteTermSymbols+DefaultTermSymbols;
    R.OpenSymbol.BrakeType:=btAny;
    repeat
      1: readln(f,s);
      if eof(f) then exit;
      if isPresent(s,'<Range Name="') then
        begin
          rr:=TSynRange.create;
          rr.Name:=GetName(s);
          r.AddRange(rr);
          LoadRange(rr);
          goto 1;
        end;

      if isPresent(s,'<Def>') then
        repeat
          readln(f,s);
          if eof(f) then exit;
          if isPresent(s,'<Back>') then R.DefaultAttri.Background:=strtointdef(GetValue(s),$FFFFFF);
          if isPresent(s,'<Fore>') then R.DefaultAttri.Foreground:=strtointdef(GetValue(s),0);
          if isPresent(s,'<Style>') then ;
        until isPresent(s,'</Def>');

      if isPresent(s,'<Num>') then
        repeat
          readln(f,s);
          if eof(f) then exit;
          if isPresent(s,'<Back>') then R.NumberAttri.Background:=strtointdef(GetValue(s),$FFFFFF);
          if isPresent(s,'<Fore>') then R.NumberAttri.Foreground:=strtointdef(GetValue(s),0);
          if isPresent(s,'<Style>') then ;
        until isPresent(s,'</Num>');

      if isPresent(s,'<DelimiterChars>') then R.TermSymbols:=String2Set(GetValue(s));

      if isPresent(s,'<OpenSymbol>') then R.OpenSymbol.Symbol:=GetValue(s);
      if isPresent(s,'<CloseSymbol>') then R.CloseSymbol.Symbol:=GetValue(s);
      if isPresent(s,'<CloseOnTerm>') then R.CloseOnTerm:=Lowercase(GetValue(s))='true';
      if isPresent(s,'<CloseOnEol>') then R.CloseOnEol:=Lowercase(GetValue(s))='true';
      if isPresent(s,'<CaseSensitive>') then R.CaseSensitive:=Lowercase(GetValue(s))='true';
      if (isPresent(s,'<AnyTerm>')) and (R.Name<>_Root) then
        begin
          if Lowercase(GetValue(s))='true' then
            R.OpenSymbol.BrakeType:=btAny
          else
            R.OpenSymbol.BrakeType:=btTerm;
        end;

      if isPresent(s,'<KW Name="') then
        begin
          kw:=TSynSymbolGroup.Create('',TSynHighlighterAttributes.Create('unknown'));
          kw.Name:=GetName(s);
          R.AddSymbolGroup(kw);
          repeat
            readln(f,s);
            if eof(f) then exit;
            if isPresent(s,'<Attri>') then
              repeat
                readln(f,s);
                if eof(f) then exit;
                if isPresent(s,'<Back>') then Kw.Attribs.Background:=strtointdef(GetValue(s),$FFFFFF);
                if isPresent(s,'<Fore>') then Kw.Attribs.Foreground:=strtointdef(GetValue(s),0);
                if isPresent(s,'<Style>') then ;
              until isPresent(s,'</Attri>');
            if isPresent(s,'<W>') then KW.KeywordsList.add(GetValue(s));
          until isPresent(s,'</KW>');
        end;
    Until isPresent(s,'</Range>');
  end;

  Procedure LoadInfo;
    var i:integer;
  begin
    info.history.Clear;
    info.Sample.Clear;
    if isPresent(s,'<Info>') then
      repeat
        readln(f,s);
        if eof(f) then exit;

        if isPresent(s,'<General>') then
          repeat
            readln(f,s);
            if eof(f) then exit;
            if isPresent(s,'<Name>') then info.General.Name:=GetValue(s);
            if isPresent(s,'<FileTypeName>') then info.General.FileTypeName:=GetValue(s);
            if isPresent(s,'<Layout>') then info.General.Layout:=GetValue(s);
          until isPresent(s,'</General>');

        if isPresent(s,'<Author>') then
          repeat
            readln(f,s);
            if eof(f) then exit;
            if isPresent(s,'<Name>') then info.Author.Name:=GetValue(s);
            if isPresent(s,'<Email>') then info.Author.Email:=GetValue(s);
            if isPresent(s,'<Web>') then info.Author.Web:=GetValue(s);
            if isPresent(s,'<Copyright>') then info.Author.Copyright:=GetValue(s);
            if isPresent(s,'<Company>') then info.Author.Company:=GetValue(s);
            if isPresent(s,'<Remark>') then info.Author.Remark:=GetValue(s);
          until isPresent(s,'</Author>');

        if isPresent(s,'<Version>') then
          repeat
            readln(f,s);
            if eof(f) then exit;
            if isPresent(s,'<Version>') then info.Version.Version:=strtointdef(GetValue(s),0);
            if isPresent(s,'<Revision>') then info.Version.Revision:=strtointdef(GetValue(s),0);
            if isPresent(s,'<Date>') then info.Version.ReleaseDate:=strtointdef(GetValue(s),0);
            if isPresent(s,'<Type>') then
              begin
                if GetValue(s)='Beta' then info.Version.VersionType:=vtBeta else
                  if GetValue(s)='Release' then info.Version.VersionType:=vtRelease else
                    info.Version.VersionType:=vtInternalTest;
              end;
          until isPresent(s,'</Version>');

        if isPresent(s,'<History>') then
          repeat
            readln(f,s);
            if eof(f) then exit;
            if isPresent(s,'<H>') then info.history.add(GetValue(s));
          until isPresent(s,'</History>');

        if isPresent(s,'<Sample>') then
          repeat
            readln(f,s);
            if eof(f) then exit;
            if isPresent(s,'<S>') then info.Sample.add(GetValue(s));
          until isPresent(s,'</Sample>');
      until isPresent(s,'</Info>');
  end;


  Procedure LookForMainRange(s:string);
  begin
    if isPresent(s,'<Info>') then LoadInfo;
    if isPresent(s,'<Range Name="Root">') then
      begin
        ParsingSection:=psMainRange;
        R:=self.MainRules;
        R.Name:=_Root;
        LoadRange(R);
      end;
  end;

}
var
  buf,sav:PChar;
  BufSize:Integer;
  CurTagIndex:Integer;
  LineNumber:integer;
  Param:string;
  xmlInfoTags:TStringList;


  function GetReplacement:string;
  var
    sPos:PChar;
  begin
    Result:='';
    sPos:=buf;
    inc(buf);
    if buf^='l' then begin
      Inc(buf);
      if buf^='t' then begin
        Inc(buf);
        if buf^=';' then
          Result:='<';
      end;
    end
    else
    if buf^='g' then begin
      Inc(buf);
      if buf^='t' then begin
        Inc(buf);
        if buf^=';' then
          Result:='>';
      end;
    end
    else
    if buf^='q' then begin
      Inc(buf);
      if buf^='t' then begin
        Inc(buf);
        if buf^=';' then
          Result:='"';
      end;
    end
    else
    if buf^='a' then begin
      Inc(buf);
      if buf^='m' then begin
        Inc(buf);
        if buf^='p' then begin
          Inc(buf);
          if buf^=';' then
            Result:='&';
        end;
      end;
    end;
    if Result='' then
    begin
      Dec(buf);
      SetLength(Result,Cardinal(buf)-Cardinal(sPos));
      Move(sPos^,pointer(Result)^,Cardinal(buf)-Cardinal(sPos));
    end
    else
     Inc(buf);
  end;
  ////TL Removed optional default parameter assignment... fixed in the calls
  ////TL function GetNextTag(var Idx:Integer; var TagParam:string; IgnoreUnknown:boolean=false):boolean;
  function GetNextTag(var Idx:Integer; var TagParam:string; IgnoreUnknown:boolean):boolean;
  var
   s,stmp:string;
   sPos:PChar;
  begin
    Idx:=-1;
    Result:=True;
    TagParam:='';
    while buf^<>'<' do
    begin
      if buf^=#0 then exit;
      if buf^=#13 then Inc(LineNumber);
      Inc(buf);
    end;
    Inc(buf);
    while (buf^=#32) do
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber))
      else Inc(buf);

    if buf^='/' then
    begin
      Result:=False;
      inc(buf);
    end;
    sPos:=buf;
    while (buf^<>#32) and (buf^<>'>') do
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber))
      else Inc(buf);
    SetLength(s,Cardinal(buf)-Cardinal(sPos));
    move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));

    if (not xmlInfoTags.Find(s,Idx)) then
     if (not IgnoreUnknown) then
       raise Exception.Create('Tag "'+s+'" is unknown (line '+IntToStr(LineNumber) +')')
     else
     begin
       Idx:=-1; Result:=True;
       exit;
     end;

    while buf^<>'>' do
    begin
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber));
      if buf^='"' then
      begin
        inc(buf);
        sPos:=buf;
        stmp:='';
        while (buf^<>'"') do
          if buf^=#0 then begin Result:=False; exit; end
          else if buf^='&' then
          begin
            SetLength(s,Cardinal(buf)-Cardinal(sPos));
            move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));
            stmp:=stmp+s+GetReplacement;
            sPos:=buf;
          end
          else Inc(buf);
        SetLength(s,Cardinal(buf)-Cardinal(sPos));
        move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));
        TagParam:=stmp+s;
      end;
      Inc(buf);
    end;
    Inc(buf);
  end;


  function GetData(TagIndex:integer):string;
  var
    s:string;
    sPos:PChar;
    idx:Integer;
  begin
    Result:='';
    sPos:=buf;
    while buf^<>'<' do
    begin
      if buf^='&' then
      begin
        SetLength(s,Cardinal(buf)-Cardinal(sPos));
        move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));
        Result:=Result+s+GetReplacement;
        sPos:=buf;
      end
      else
      if (buf^=#0) or (buf^=#13) then
         raise Exception.Create('Unexpected end of line. Line '+IntToStr(LineNumber))
      else Inc(buf);
    end;
    SetLength(s,Cardinal(buf)-Cardinal(sPos));
    Move(sPos^,pointer(s)^,Cardinal(buf)-Cardinal(sPos));
    Result:=Result+s;
    ////TL Added 3rd parameter
    if (GetNextTag(idx,s,false)) or (idx<>CurTagIndex) then
       raise Exception.Create('Close tag: /'+xmlInfoTags[idx]+' is not found. Line '+IntToStr(LineNumber));
  end;

  procedure ReadInfo;

    procedure ReadGeneral;
    begin
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
           xitName: info.General.Name:=GetData(xitName);
           xitFileTypeName: info.General.FileTypeName:=GetData(xitFileTypeName);
           xitLayout: info.General.Layout:=GetData(xitLayout);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitGeneral then
         raise Exception.Create('Unexpected tag: /'+xmlInfoTags[curTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadVersion;
      function GetType(s:string):TVersionType;
      begin
        if s='Beta' then Result:=vtBeta else
        if s='Release' then Result:=vtRelease else
            Result:=vtInternalTest;
      end;
    begin
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
            xitVersion: info.Version.Version:=strtointdef(GetData(xitVersion),0);
            xitRevision: info.Version.Revision:=strtointdef(GetData(xitRevision),0);
            xitDate: info.Version.ReleaseDate:=strtointdef(GetData(xitDate),0);
            xitType: info.Version.VersionType:=GetType(GetData(xitType));
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitVersion then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadAuthor;
    begin
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
            xitName: info.Author.Name:=GetData(xitName);
            xitEmail: info.Author.Email:=GetData(xitEmail);
            xitWeb: info.Author.Web:=GetData(xitWeb);
            xitCopyright: info.Author.Copyright:=GetData(xitCopyright);
            xitCompany: info.Author.Company:=GetData(xitCompany);
            xitRemark: info.Author.Remark:=GetData(xitRemark);
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitAuthor then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadHistroy;
    begin
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
            xitH: info.history.add(GetData(xitH));
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitHistory then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadSample;
    begin
      info.Sample.Clear;
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
           xitS: info.Sample.Add(GetData(xitS));
        else
          raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitSample then
          raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

  begin
    ////TL Added 3rd parameter
    while GetNextTag(CurTagIndex,Param,false) do
    begin
      case CurTagIndex of
        xitGeneral: ReadGeneral;
        xitVersion: ReadVersion;
        xitAuthor: ReadAuthor;
        xitHistory:  ReadHistroy;
        xitSample: ReadSample;
      else
        raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex<>xitInfo then
      raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
  end;

  procedure ReadKW(SymbGr:TSynSymbolGroup);

    procedure ReadAttri;
    begin
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
          xitBack: SymbGr.Attribs.Background:=strtointdef(GetData(xitBack),0);
          xitFore: SymbGr.Attribs.Foreground:=strtointdef(GetData(xitFore),0);
          xitStyle: SymbGr.Attribs.Style:=String2Fs(GetData(xitStyle));
        else
           raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitAttri then
        raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

  begin
    ////TL Added 3rd parameter
    while GetNextTag(CurTagIndex,Param,false) do
    begin
      case CurTagIndex of
        xitAttri: ReadAttri;
        xitW: SymbGr.KeywordsList.Add(GetData(xitW));
      else
         raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex<>xitKW then
      raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
  end;

  procedure ReadRange(CurRange:TSynRange);
    var
      NewRange:TSynRange;
      NewSymbolGroup:TSynSymbolGroup;

    procedure ReadDef;
    begin
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
          xitBack: CurRange.DefaultAttri.Background:=strtointdef(GetData(xitBack),0);
          xitFore: CurRange.DefaultAttri.Foreground:=strtointdef(GetData(xitFore),0);
          xitStyle: CurRange.DefaultAttri.Style := String2Fs(GetData(xitStyle));
        else
           raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitDef then
        raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

    procedure ReadNum;
    begin
      ////TL Added 3rd parameter
      while GetNextTag(CurTagIndex,Param,false) do
      begin
        case CurTagIndex of
          xitBack: CurRange.NumberAttri.Background:=strtointdef(GetData(xitBack),0);
          xitFore: CurRange.NumberAttri.Foreground:=strtointdef(GetData(xitFore),0);
          xitStyle: CurRange.DefaultAttri.Style:=String2Fs(GetData(xitStyle));
        else
           raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
        end;
      end;
      if CurTagIndex<>xitNum then
        raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
    end;

  begin
    ////TL Added 3rd parameter
    while GetNextTag(CurTagIndex,Param,false) do
    begin
      case CurTagIndex of
        xitDef: ReadDef;
        xitOpenSymbol: CurRange.OpenSymbol.Symbol:=GetData(xitOpenSymbol);
        xitCloseSymbol: CurRange.CloseSymbol.Symbol:=GetData(xitCloseSymbol);
        xitCloseOnTerm: CurRange.CloseOnTerm:=lowercase(GetData(xitCloseOnTerm))='true';
        xitCloseOnEol: CurRange.CloseOnEol:=lowercase(GetData(xitCloseOnEol))='true';
        xitAnyTerm: if Lowercase(GetData(xitAnyTerm))='true' then
                      CurRange.OpenSymbol.BrakeType:=btAny
                    else
                      CurRange.OpenSymbol.BrakeType:=btTerm;
        xitDelimiterChars: CurRange.TermSymbols:=String2Set(GetData(xitDelimiterChars));
        xitNum: ReadNum;
        xitCaseSensitive: CurRange.CaseSensitive:=lowercase(GetData(xitCaseSensitive))='true';
        xitKW: begin
                 NewSymbolGroup:=TSynSymbolGroup.Create('',CurRange.AddNewAttribs('unknown'));
                 NewSymbolGroup.Name:=Param;
                 CurRange.AddSymbolGroup(NewSymbolGroup);
                 ReadKW(NewSymbolGroup);
               end;
        xitRange:
                 begin
                    ////TL added two default null string parameters
                    NewRange:=TSynRange.Create('','');
                    NewRange.Name:=Param;
                    CurRange.AddRange(NewRange);
                    ReadRange(NewRange);
                 end;
      else
        raise Exception.Create('Unexpected tag: '+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
      end;
    end;
    if CurTagIndex<>xitRange then
      raise Exception.Create('Unexpected tag: /'+xmlInfoTags[CurTagIndex]+' line '+IntToStr(LineNumber));
  end;

begin
  Clear;

  try
    BufSize:= aSrc.Size;
    GetMem(buf,BufSize);
    aSrc.ReadBuffer(buf^,BufSize);
  except
    FreeMem(buf);
    raise;
  end;
  sav:=buf;
  LineNumber:=0;

  xmlInfoTags := nil;
  try
    xmlInfoTags := TStringList.Create;
    BuildXMLIndexes( xmlInfoTags );
    ////TL Added 3rd parameter
    if (not GetNextTag(CurTagIndex,Param,false)) or (CurTagIndex<>xitUniHighlighter) then
      raise exception.Create('Highlighter header tag ("<UniHighlighter>") was not found.');

    while GetNextTag(CurTagIndex,Param,True) do
    begin
      case CurTagIndex of
        xitInfo: ReadInfo;
        xitRange:
               begin
                  self.MainRules.Name:=Param;
                  ReadRange(self.MainRules);
               end;
        xitCopyRight: GetData(xitCopyRight);
      end;
    end;
    if CurTagIndex<>xitUniHighlighter then
      raise Exception.Create('Closing tag: /'+xmlInfoTags[xitUniHighlighter]+' was not found. Line '+IntToStr(LineNumber));
  finally
    FreeMem(sav);
    xmlInfoTags.Free;
  end;

{  ParsingSection:=psNone;
  While not eof(f) do
    begin
      readln(f,s);
      Case ParsingSection of
         psNone: LookForInitialization(s);
           psInitialized: LookForMainRange(s);
          End;
        End;
      closefile(f);
      if R=nil then raise exception.Create('Highlighter is not valid. Check structure!');
    end;}

  DefHighlightChange( Self );
end;

procedure TSynRange.DeleteRange(SynRange: TSynRange);
begin
  fSynRanges.Remove(SynRange);
  SynRange.Free;
end;

procedure TSynRange.DeleteRange(index: integer);
begin
 TSynRange(fSynRanges[index]).Free;
 fSynRanges.Delete(index);
end;

procedure TSynRange.DeleteSymbolGroup(SymbolGroup: TSynSymbolGroup);
begin
  fSymbolGroups.Remove(SymbolGroup);
  SymbolGroup.Free;
end;

procedure TSynRange.DeleteSymbolGroup(index: integer);
begin
 TSynSymbolGroup(fSymbolGroups[index]).Free;
 fSymbolGroups.Delete(index);
end;

procedure TSynUniSyn.LoadFromFile(FileName: string);
var
  F: TFileStreamUTF8;
begin
  if FileName = '' then
    raise exception.Create('FileName is empty');
  F:=TFileStreamUTF8.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream( F );
  finally
    F.Free;
  end;
end;

procedure TSynUniSyn.SaveToFile(FileName: string);
var
  F: TFileStreamUTF8;
begin
  if FileName = '' then
    raise exception.Create('FileName is empty');
  F:=TFileStreamUTF8.Create(FileName,fmOpenWrite or fmShareDenyNone);
  try
    SaveToStream( F );
  finally
    F.Free;
  end;
end;

procedure TSynUniSyn.DefineProperties(Filer: TFiler);
var
  iHasData: boolean;
begin
  inherited;
  if Filer.Ancestor <> nil then
  begin
    iHasData := True;
  end
  else
    iHasData := MainRules.RangeCount > 0;
  ////TL Added 2 @ prefixes
  Filer.DefineProperty( 'Syntax', @ReadSyntax, @WriteSyntax, iHasData );
end;

procedure TSynUniSyn.ReadSyntax(Reader: TReader);
var
  iBuffer: TStringStream;
begin
  iBuffer := nil;
  try
    iBuffer := TStringStream.Create( Reader.ReadString );
    iBuffer.Position := 0;
    LoadFromStream( iBuffer );
  finally
    iBuffer.Free;
  end;
end;

procedure TSynUniSyn.WriteSyntax(Writer: TWriter);
var
  iBuffer: TStringStream;
begin
  iBuffer := TStringStream.Create( '' );
  try
    SaveToStream( iBuffer );
    iBuffer.Position := 0;
    Writer.WriteString( iBuffer.DataString );
  finally
    iBuffer.Free;
  end;
end;

function TSynUniSyn.GetIdentChars: TSynIdentChars;
begin
  Result := [#32..#255] - fCurrentRule.TermSymbols;
end;

function TSynUniSyn.GetSampleSource: string;
begin
  Result := Info.Sample.Text;
end;

procedure TSynUniSyn.SetSampleSource(Value: string);
begin
  Info.Sample.Text := Value;
end;

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynUniSyn]);
end;

initialization
  RegisterPlaceableHighlighter(TSynUniSyn);

end.



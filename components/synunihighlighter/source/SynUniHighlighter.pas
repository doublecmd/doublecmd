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

(******************************************************************************
Authors: Fantasist (Kirill Burtsev walking_in_the_sky@yahoo.com)
         Vit (Vitaly Nevzorov nevzorov@yahoo.com)
         Vitalik (Vitaly Lyapota vetal-x@mail.ru)
Official Site: www.delphist.com
With all questions, please visit www.delphist.com/forum
******************************************************************************)

unit SynUniHighlighter;

{$mode delphi}

interface

uses
  SysUtils, Classes, Graphics,
  SynEditTypes, SynEditHighlighter, SynUniClasses, SynUniRules, Laz2_DOM;

Const
  _Root = 'Root';
  _New = 'New';

type

  { TSynUniSyn }

  TSynUniSyn = class(TSynCustomHighlighter)
  private
    procedure ReadSyntax(Reader: TReader);
    procedure WriteSyntax(Writer: TWriter);
  protected
    fMainRules: TSynRange;
    fEol: boolean;
    fPrEol: boolean;
    fLine: PChar;
    fTrueLine: String;
    fLineNumber: Integer;
    Run: LongInt;
    fTokenPos: Integer;
    fCurrToken: TSynSymbol;
    fCurrentRule: TSynRange;
    SymbolList: array[char] of TAbstractSymbol; //???
    fPrepared: boolean;

    fSchemes: TStringList; //Vitalik 2004
    fSchemeIndex: integer; //Vitalik 2004

    fImportFormats: TList;

    procedure SpaceProc;
    procedure NullProc;
    function GetIdentChars: TSynIdentChars; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetSampleSource: string; override;
    procedure SetSampleSource(Value: string); override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override; {Abstract}
    function GetEol: Boolean; override; {Abstract}
    function GetRange: Pointer; override;
    function GetToken: string; override; {Abstract}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override; {Abstract}
    function GetTokenAttribute: TSynHighlighterAttributes; override; {Abstract}
    function GetTokenID: Integer;
    function GetTokenKind: integer; override; {Abstract}
    function GetTokenPos: Integer; override; {Abstract}
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override; {Abstract}
    procedure ResetRange; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override; {Abstract}
    procedure SetRange(Value: Pointer); override;
    procedure Reset;
    procedure Clear;
    procedure Prepare;
    procedure CreateStandardRules;

    procedure ReadSchemes(xml: TDOMNode);
    procedure ReadInfo(xml: TDOMNode);
    procedure LoadHglFromXml(xml: TDOMNode);
    procedure LoadHglFromStream(Stream: TStream);
    procedure LoadHglFromFile(FileName: string);

    procedure SaveHglToStream(Stream: TStream);
    procedure SaveHglToFile(FileName: string);

    procedure LoadFromXml(xml: TDOMNode);
    procedure LoadFromStream(Stream: TStream; FreeStream: boolean = True);
    procedure LoadFromFile(FileName: string);

    function GetAsStream: TMemoryStream;
    procedure SaveToStream(Stream: TStream; Rule: TSynRule = nil);
    procedure SaveToFile(FileName: string; Rule: TSynRule = nil);

  public
    Info: TSynInfo;
    Styles: TSynUniStyles;
    SchemeFileName: string;
    SchemeName: string;
    property MainRules: TSynRange read fMainRules;
    property SchemesList: TStringList read fSchemes write fSchemes; //Vitalik 2004
    property SchemeIndex: integer read fSchemeIndex write fSchemeIndex; //Vitalik 2004
  end;

implementation

uses
  LazUTF8Classes, Laz2_XMLRead;

const
  SYNS_AttrTest = 'Test';

//==== TSynUniSyn ============================================================
constructor TSynUniSyn.Create(AOwner: TComponent);
var
  fTestAttri: TSynHighlighterAttributes;
begin
  inherited Create(AOwner);
  Info := TSynInfo.Create;
  Info.History := TStringList.Create;
  Info.Sample := TStringList.Create;
  fPrepared := False;

  //Вот так вот нужно все атрибуты будет добавлять! Потому как нужно еще и обработать [Underline + Italic]
  fTestAttri := TSynHighLighterAttributes.Create(SYNS_AttrTest);
  fTestAttri.Style := [fsUnderline, fsItalic];
  fTestAttri.Foreground := clBlue;
  fTestAttri.Background := clSilver;
  AddAttribute(fTestAttri);

  fSchemes := TStringList.Create;
  fSchemeIndex := -1;

  fMainRules := TSynRange.Create;
  MainRules.Name := _Root;
  fEol := False;
  fPrEol := False;
  fCurrentRule := MainRules;
//  AddNewScheme('Noname');
  fImportFormats := TList.Create;
end;

destructor TSynUniSyn.Destroy;
//: Destructor of TSynUniSyn
begin
  MainRules.Free;
  Info.History.Free;
  Info.Sample.Free;
  Info.Free;
  fSchemes.Free;
  fImportFormats.Free;
  inherited;
end;

procedure TSynUniSyn.SetLine(const NewValue: string; LineNumber: Integer);
//: Set current line in SynEdit for highlighting
  function HaveNodeAnyStart(Node: TSymbolNode): boolean;
  var
    i: integer;
  begin
    Result := False;
    if Node.StartType = stAny then
    begin
      Result := True;
      Exit;
    end;
    for i := 0 to Node.NextSymbs.Count-1 do
      if (Node.NextSymbs.Nodes[i].StartType = stAny) or HaveNodeAnyStart(Node.NextSymbs.Nodes[i]) then
      begin
        Result := True;
        Exit;
      end
  end;

var i: integer;
begin
  if LineNumber = 1 then begin
    MainRules.ResetParents(MainRules);
    MainRules.ClearParsingFields();
  end;
  if not fCurrentRule.Prepared then begin //: If current Range isn't prepared,
    Prepare;                              //: then prepare it and its sub-ranges
(*{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{
!!!!!!! Это я писал и это зачем-то нужно !!!!!!!!!!!!!!!!!!!!!!!!*)
  for i := 0 to 255 do
    if (SymbolList[char(i)] <> nil) {temp}and (TSymbols(SymbolList[char(i)]).HeadNode <> nil){/temp} then
      fCurrentRule.HasNodeAnyStart[char(i)] := HaveNodeAnyStart(TSymbols(SymbolList[fCurrentRule.CaseFunct(char(i))]).HeadNode);
(*}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}*)
  end;
{begin} //Vitalik 2004
{was:
  fTrueLine := PChar(NewValue);
  l := Length(NewValue);
  ReallocMem(fLine, l+1);
  for i := 0 to l do
    fLine[i] := fCurrentRule.CaseFunct(fTrueLine[i]);
}
  fTrueLine := NewValue;
  fLine := PChar(NewValue);     //: Current string of SynEdit
{end} //Vitalik 2004
  Run := 0;                     //: Set Position of "parser" at the first char of string
  fTokenPos := 0;               //: Set Position of current token at the first char of string
  fLineNumber := LineNumber;    //: Number of current line in SynEdit
  fEol := False;                //: ???
  fPrEol := False;              //: ???
  Next;                         //: Find first token in the line
end;

procedure TSynUniSyn.Next;
//: Goes to the next token and open/close ranges
var
  ParentCycle, CurrentParent: TSynRange;
  RangeLink: TSynRangeLink;
  isFindSame: boolean;
begin
  if fPrEol then //: if it was end of line then
  begin            //: if current range close on end of line then
    if (fCurrentRule.fRule.fCloseOnEol) or (fCurrentRule.fRule.fCloseOnTerm) then begin
      if fCurrentRule.OpenCount > 0 then
        fCurrentRule.OpenCount := fCurrentRule.OpenCount - 1
      else
        if fCurrentRule.ParentBackup <> nil then
          fCurrentRule.Parent := fCurrentRule.ParentBackup;
      if fCurrentRule.fRule.fAllowPredClose then begin
        fCurrentRule := fCurrentRule.Parent;
        while (fCurrentRule.fRule.fCloseOnEol) or (fCurrentRule.fRule.fCloseOnTerm) do
          fCurrentRule := fCurrentRule.Parent;
      end else
        fCurrentRule := fCurrentRule.Parent;
    end;
    fEol := True;    //: ???
    Exit;
  end;

  fTokenPos := Run; //: Start of cf current token is end of previsious
  //: if current range close on delimeter and current symbol is delimeter then
  if (fCurrentRule.fRule.fCloseOnTerm) and (fLine[Run] in fCurrentRule.fTermSymbols) then begin
    if fCurrentRule.OpenCount > 0 then
      fCurrentRule.OpenCount := fCurrentRule.OpenCount - 1
    else
      if fCurrentRule.ParentBackup <> nil then
        fCurrentRule.Parent := fCurrentRule.ParentBackup;
    if fCurrentRule.fRule.fAllowPredClose then begin
      fCurrentRule := fCurrentRule.Parent;
      while (fCurrentRule.fRule.fCloseOnTerm) do
        fCurrentRule := fCurrentRule.Parent;
    end
    else
      fCurrentRule := fCurrentRule.Parent;
  end;

  //: if we can't find token from current position:
  if not fCurrentRule.SymbolList[fCurrentRule.CaseFunct(fLine[Run])].GetToken(fCurrentRule, fLine, Run, fCurrToken) then //Vitalik 2004
  begin
    fCurrToken := fCurrentRule.fDefaultSynSymbol; //: Current token is just default symbol
    while not ((fLine[Run] in fCurrentRule.fTermSymbols) or fCurrentRule.HasNodeAnyStart[fCurrentRule.CaseFunct(fLine[Run])]) do
      inc(Run);   //: goes to the first non-delimeter symbol
  end
  else //: else (we find token!)
  if (fCurrentRule.fClosingSymbol = fCurrToken) then begin //: if current token close current range
//  if (fCurrentRule.fClosingSymbol <> nil) and (fCurrentRule.fClosingSymbol.Symbol = fCurrToken.Symbol) then
    if fCurrentRule.OpenCount > 0 then
      fCurrentRule.OpenCount := fCurrentRule.OpenCount - 1
    else
      if fCurrentRule.ParentBackup <> nil then
        fCurrentRule.Parent := fCurrentRule.ParentBackup;
    if fCurrentRule.fRule.fAllowPredClose then begin
      fCurrentRule := fCurrentRule.Parent;
      while (fCurrentRule.fClosingSymbol <> nil) and (fCurrentRule.fClosingSymbol.Symbol = fCurrToken.Symbol) do
        fCurrentRule := fCurrentRule.Parent;
    end else
      fCurrentRule := fCurrentRule.Parent
  end else
  if fCurrToken.fOpenRule <> nil then begin //: else if current token open range then
    CurrentParent := fCurrentRule;
    if fCurrToken.fOpenRule is TSynRangeLink then begin
      RangeLink := TSynRangeLink(fCurrToken.fOpenRule);
      fCurrentRule := RangeLink.Range;
      ParentCycle := CurrentParent;
      isFindSame := False;
      while ParentCycle <> nil do begin // Ищем есть ли у тек. правила такой же родитель
        if ParentCycle = fCurrentRule then begin
          if RangeLink.Range.OpenCount = 0 then begin // Первое открытие вложенного в себя правила.
            fCurrentRule.ParentBackup := RangeLink.Range.Parent;
            fCurrentRule.Parent := CurrentParent;
            RangeLink.Range.OpenCount := 1;
          end else begin
            RangeLink.Range.OpenCount := RangeLink.Range.OpenCount + 1;
          end;
          isFindSame := True;
          break;
        end;
        ParentCycle := ParentCycle.Parent;
      end;
     if not isFindSame then begin
{        fCurrentRule.ParentBackup := RangeLink.Range.Parent;
        fCurrentRule.Parent := CurrentParent;
        RangeLink.Range.OpenCount := 1;
//      fCurrentRule.Parent := RangeLink.Parent;}
      end
    end
    else if fCurrToken.fOpenRule is TSynRange then begin
      fCurrentRule := TSynRange(fCurrToken.fOpenRule);  //: open range
      fCurrentRule.Parent := CurrentParent;
    end;
  end;

  if fLine[Run] = #0 then //: If end of line
    fPrEol := True;         //: ???

end;

procedure TSynUniSyn.SpaceProc;
//! Never used!!! SSS
begin
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

function TSynUniSyn.IsKeyword(const aKeyword: string): boolean;
//! Never used!!!! ??? SSS
begin
  // Result := fSymbols.FindSymbol(aKeyword) <> nil;
end;

function TSynUniSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
//: Returns default attribute
//: Неопнятно зачем это нужно, но функция предка - абстрактная (может что-нить с ней сделать...)
begin
  case Index of
    SYN_ATTR_COMMENT:    Result := fCurrentRule.Attribs;
    SYN_ATTR_IDENTIFIER: Result := fCurrentRule.Attribs;
    SYN_ATTR_KEYWORD:    Result := fCurrentRule.Attribs;
    SYN_ATTR_STRING:     Result := fCurrentRule.Attribs;
    SYN_ATTR_WHITESPACE: Result := fCurrentRule.Attribs;
  else
    Result := nil;
  end;
end;

function TSynUniSyn.GetEol: Boolean;
begin
  Result := fEol;
end;

function TSynUniSyn.GetRange: Pointer;
//: Returns current Range
begin
  Result := fCurrentRule;
end;

function TSynUniSyn.GetToken: string;
//: Returns current token (string from fTokenPos to Run)
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Setstring(Result, (fLine + fTokenPos), Len);                                  
end;

procedure TSynUniSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := PAnsiChar(fTrueLine) + fTokenPos;
end;

function TSynUniSyn.GetTokenID: Integer;
//: Return ID of current token
//: ??? Оставлена для непонятной совместимости? Нигде же не вызывается и не используется!
//: Можено что-нить с ней сделать...
begin
  Result := 1; //# CODE_REVIEW fCurrToken.ID;
end;

function TSynUniSyn.GetTokenAttribute: TSynHighlighterAttributes;
//: Returns attribute of current token
begin
//  fCurrToken.Attr.Style := fCurrToken.Attr.Style + [fsUnderline];
//  if GetEol then
//    Result := nil
    Result := fCurrToken.Attributes;
end;

function TSynUniSyn.GetTokenKind: integer;
//~ Можно в Kind у токена fCurrToken хранить что это ?: слово или Range или Set
begin
  Result := 1; //# CODE_REVIEW   fCurrToken.ID;
end;

function TSynUniSyn.GetTokenPos: Integer;
//: Returns position of current token
begin
  Result := fTokenPos;
end;

procedure TSynUniSyn.ResetRange;
//: Reset current range to MainRules
begin
  fCurrentRule := MainRules;
end;

procedure TSynUniSyn.SetRange(Value: Pointer);
//: Set current range
begin
  fCurrentRule := TSynRange(Value);
end;

class function TSynUniSyn.GetLanguageName: string;
begin
  Result := 'UniLanguage';
end;

procedure TSynUniSyn.Clear;
begin
  MainRules.Clear;
  Info.Clear;
end;

procedure TSynUniSyn.CreateStandardRules;
//: Create sample rools
var r: TSynRange;
    kw: TSynKeyList;
begin
  self.MainRules.Clear;
  self.MainRules.Attribs.Foreground := clBlack;
  self.MainRules.Attribs.Background := clWhite;
  self.MainRules.CaseSensitive := False;

  r := TSynRange.Create('''', '''');
  r.Name := 'Strings ''..''';
  r.Attribs.Foreground := clRed;
  r.Attribs.Background := clWhite;
  r.CaseSensitive := False;
  r.fRule.fOpenSymbol.BrakeType := btAny;
  self.MainRules.AddRange(r);

  r := TSynRange.Create('"', '"');
  r.Name := 'Strings ".."';
  r.Attribs.Foreground := clRed;
  r.Attribs.Background := clWhite;
  r.CaseSensitive := False;
  r.fRule.fOpenSymbol.BrakeType := btAny;
  self.MainRules.AddRange(r);

  r := TSynRange.Create('{', '}');
  r.Name := 'Remarks {..}';
  r.Attribs.Foreground := clNavy;
  r.Attribs.Background := clWhite;
  r.CaseSensitive := False;
  r.fRule.fOpenSymbol.BrakeType := btAny;
  self.MainRules.AddRange(r);

  r := TSynRange.Create('(*', '*)');
  r.Name := 'Remarks (*..*)';
  r.Attribs.Foreground := clNavy;
  r.Attribs.Background := clWhite;
  r.CaseSensitive := False;
  r.fRule.fOpenSymbol.BrakeType := btAny;
  self.MainRules.AddRange(r);

  r := TSynRange.Create('/*', '*/');
  r.Name := 'Remarks /*..*/';
  r.Attribs.Foreground := clNavy;
  r.Attribs.Background := clWhite;
  r.CaseSensitive := False;
  r.fRule.fOpenSymbol.BrakeType := btAny;
  self.MainRules.AddRange(r);

  kw := TSynKeyList.Create('');
  kw.Name := 'Key words';
  kw.Attribs.Foreground := clGreen;
  kw.Attribs.Background := clWhite;
  self.MainRules.AddKeyList(kw);
end;

procedure TSynUniSyn.Prepare;
//: Prepare of SynUniSyn is Prepare of SynUniSyn.fMailRules
  function HaveNodeAnyStart(Node: TSymbolNode): boolean;
  var
    i: integer;
  begin
    Result := False;
    if Node.StartType = stAny then
    begin
      Result := True;
      Exit;
    end;
    for i := 0 to Node.NextSymbs.Count-1 do
      if (Node.NextSymbs.Nodes[i].StartType = stAny) or HaveNodeAnyStart(Node.NextSymbs.Nodes[i]) then
      begin
        Result := True;
        Exit;
      end
  end;

var i: integer;
begin
  MainRules.Prepare(MainRules);
//  for i := 0 to 255 do
//  if (MainRules.SymbolList[char(i)] <> MainRules.fDefaultTermSymbol) and (MainRules.SymbolList[char(i)] <> MainRules.fDefaultSymbols) then
//    MessageBox(0,PChar(TSymbols(MainRules.SymbolList[char(i)]).HeadNode.tkSynSymbol.Symbol),'1',0);
  for i := 0 to 255 do
//    if (MainRules.SymbolList[char(i)] <> nil) {temp}and (TSymbols(MainRules.SymbolList[char(i)]).HeadNode <> nil){/temp} then
    if (MainRules.SymbolList[char(i)] <> MainRules.fDefaultTermSymbol) and (MainRules.SymbolList[char(i)] <> MainRules.fDefaultSymbols) and (TSymbols(MainRules.SymbolList[char(i)]).HeadNode <> nil) then
      MainRules.HasNodeAnyStart[char(i)] := HaveNodeAnyStart(TSymbols(MainRules.SymbolList[MainRules.CaseFunct(char(i))]).HeadNode);
end;

procedure TSynUniSyn.NullProc;
//: Never used!!! SSS ???
begin
//  fEol := True;
end;

procedure TSynUniSyn.Reset;
//: Reset of SynUniSyn is Reset of SynUniSyn.MainRules
begin
  MainRules.Reset;
end;

procedure TSynUniSyn.DefineProperties(Filer: TFiler);
//! Never used ????
var
  iHasData: boolean;
begin
  inherited;
  if Filer.Ancestor <> nil then
    iHasData := True
  else
    iHasData := MainRules.RangeCount > 0;
  Filer.DefineProperty( 'Syntax', ReadSyntax, WriteSyntax, {True}iHasData );
end;

procedure TSynUniSyn.ReadSyntax(Reader: TReader);
//: This is some metods for reading ??? ??? ???
var
  iBuffer: TStringStream;
begin
//  iBuffer := nil;
//  try
    iBuffer := TStringStream.Create( Reader.ReadString );
    iBuffer.Position := 0;
    LoadFromStream( iBuffer );
//  finally
//    iBuffer.Free;
//  end;
end;

procedure TSynUniSyn.WriteSyntax(Writer: TWriter);
//: This is some metods for writing ??? ??? ???
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
//: Return IdentChars - hmm... What for ??? word selection?
begin
  Result := [#32..#255] - fCurrentRule.TermSymbols;
end;

function TSynUniSyn.GetSampleSource: string;
//: Get sample text
begin
  Result := Info.Sample.Text;
end;

procedure TSynUniSyn.SetSampleSource(Value: string);
//: Set sample text
begin
  Info.Sample.Text := Value;
end;

procedure TSynUniSyn.LoadFromXml(xml: TDOMNode);
var
  i, J, K: integer;
  ChildNode1,
  ChildNode2: TDOMNode;
  Key, Value: string;
begin
  Clear;
  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode1:= xml.ChildNodes.Item[J];
    if SameText(ChildNode1.NodeName, 'UniHighlighter') then
      if ChildNode1.Attributes.Length = 0 then
      begin
        LoadHglFromXml(ChildNode1);
        Exit;
      end
      else begin
      for I := 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
      begin
        ChildNode2:= ChildNode1.ChildNodes.Item[I];
        if SameText(ChildNode2.NodeName, 'Info') then
          Info.LoadFromXml(ChildNode2)
        else
        if SameText(ChildNode2.NodeName, 'Scheme') then begin
          SchemeFileName := '';   SchemeName := '';
          for K := 0 to Int32(ChildNode2.Attributes.Length) - 1 do begin
            Key := ChildNode2.Attributes[K].NodeName;
            Value := ChildNode2.Attributes[K].NodeValue;
            if SameText('File', Key) then SchemeFileName := Value else
            if SameText('Name', Key) then SchemeName := Value;
          end;
          if FileExists(SchemeFileName) then begin
            if Styles <> nil then
              Styles.Free;
            Styles := TSynUniStyles.Create;
            Styles.FileName := SchemeFileName;
            Styles.Load;
          end;
        end else
        if SameText(ChildNode2.NodeName, 'Range') then begin
  //        fMainRules.SetStyles(fStyles);
          fMainRules.Styles := Styles;
          fMainRules.LoadFromXml(ChildNode2);
          Break;
        end
      end;
    end;
  end
end;

procedure TSynUniSyn.LoadFromStream(Stream: TStream; FreeStream: boolean);
var
  Len: Integer;
  Temp: PAnsiChar;
  Target: PAnsiChar;
  Source: PAnsiChar;
  Finish: PAnsiChar;
  Memory: PAnsiChar;
  Xml: TXMLDocument;
  TargetStream: TMemoryStream;
begin
  TargetStream:= TMemoryStream.Create;
  try
    Len:= Stream.Size;
    Source:= GetMem(Len);
    TargetStream.SetSize(Len * 2);
    Stream.ReadBuffer(Source^, Len);
    Temp:= Source;
    Memory:= Source;
    Finish:= Temp + Len - 4;
    Target:= TargetStream.Memory;
    // Convert '&qt;' to '&quot;'
    while (Temp < Finish) do
    begin
      if (Temp^ <> '&') then
        Inc(Temp, 1)
      else if ((Temp + 1)^ <> 'q') then
        Inc(Temp, 2)
      else if ((Temp + 2)^ <> 't') then
        Inc(Temp, 3)
      else begin
        Len:= (Temp - Source) + 2;
        Move(Source^, Target^, Len);
        Inc(Temp, 4);
        Inc(Target, Len);
        Move('uot;', Target^, 4);
        Inc(Target, 4);
        Source:= Temp;
      end;
    end;
    Len:= (Temp - Source) + 4;
    Move(Source^, Target^, Len);
    Inc(Target, Len);
    TargetStream.SetSize(Target - TargetStream.Memory);
    try
      TargetStream.Position:= 0;
      ReadXMLFile(Xml, TargetStream);
      LoadFromXml(Xml);
    finally
      DefHighlightChange(Self);
    end;
  finally
    TargetStream.Free;
    if FreeStream then Stream.Free;
    if (Memory <> nil) then FreeMem(Memory);
  end;
end;

procedure TSynUniSyn.LoadFromFile(FileName: string);
begin
  LoadFromStream(TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyNone));
end;

procedure TSynUniSyn.SaveToStream(Stream: TStream; Rule: TSynRule);
var
  StreamWriter: TStreamWriter;
begin
  StreamWriter := TStreamWriter.Create(Stream);
  with StreamWriter do begin
    WriteTag(0, 'UniHighlighter');
    WriteParam('version', '1.8', CloseStartTag);
    Info.SaveToStream(StreamWriter, 2);
    WriteTag(2, 'Scheme');
    WriteParam('File', SchemeFileName);
    WriteParam('Name', SchemeName, CloseEmptyTag);
    if Rule = nil then
      MainRules.SaveToStream(StreamWriter, 2)
    else
      Rule.SaveToStream(StreamWriter, 2);
    WriteTag(0, '/UniHighlighter', True);
  end;
  StreamWriter.Free;
end;

function TSynUniSyn.GetAsStream: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(Result);
end;

procedure TSynUniSyn.SaveToFile(FileName: string; Rule: TSynRule);
var
  F: TFileStream;
begin
  if FileName = '' then
    raise exception.Create(ClassName + '.SaveToFile - FileName is empty');
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F, Rule)
  finally
    F.Free;
  end;
end;

function ReadValue(ANode: TDOMNode): String;
begin
  if Assigned(ANode.FirstChild) then
    Result:= ANode.FirstChild.NodeValue
  else
    Result:= EmptyStr;
end;

procedure TSynUniSyn.SaveHglToStream(Stream: TStream);
//: Save Highlighter to stream
  procedure WriteString(const aStr: string);
  begin
    Stream.Write( aStr[1], Length(aStr) );
    Stream.Write( #10#13, 1 );
  end;

  function Indent(i: integer): string;
  begin
    SetLength( Result, i );
    FillChar( Result[1], i, #32 );
  end;

  function GetValidValue(Value: string): string;
  begin
    Value := StringReplace(Value, '&', '&amp;', [rfReplaceAll, rfIgnoreCase]);
    Value := StringReplace(Value, '<', '&lt;', [rfReplaceAll, rfIgnoreCase]);
    Value := StringReplace(Value, '"', '&quot;', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Value, '>', '&gt;', [rfReplaceAll, rfIgnoreCase]);
  end;

  procedure InsertTag(Ind: integer; Name: string; Value: string);
  begin
    WriteString( Format('%s<%s>%s</%s>', [Indent(Ind), Name, GetValidValue(Value), Name]) );
  end;

  procedure OpenTag(Ind: integer; Name: string; Param: string = ''; ParamValue: string = '');
  begin
    if Param = '' then
      WriteString(Format('%s<%s>', [Indent(Ind), Name]))
    else
      WriteString(Format('%s<%s %s="%s">', [Indent(Ind), Name, Param, GetValidValue(ParamValue)]));
  end;

  procedure SaveColor(MainTag: string; Ind, Fore, Back: integer; Style: TFontStyles; PFore, PBack: boolean);
    procedure InsertTagBool(Ind: integer; Name: string; Value: Boolean);
    begin
      if Value then
        WriteString(Format('%s<%s>True</%s>', [Indent(Ind), Name, Name]))
      else
        WriteString(Format('%s<%s>False</%s>', [Indent(Ind), Name, Name]))
    end;
  begin
    OpenTag(Ind, MainTag);
    InsertTag(Ind+1, 'Back', Inttostr(Back));
    InsertTag(Ind+1, 'Fore', Inttostr(Fore));
    InsertTag(Ind+1, 'Style', FontStyleToStr(Style));
    InsertTagBool(Ind+1, 'ParentForeground', PFore);
    InsertTagBool(Ind+1, 'ParentBackground', PBack);
    OpenTag(Ind, '/'+MainTag);
  end;

  procedure SaveKWGroup(Ind: integer; G: TSynKeyList);
  var i: integer;
    procedure InsertTagBool(Ind: integer; Name: string; Value: Boolean);
    begin
      if Value then
        WriteString(Format('%s<%s>True</%s>', [Indent(Ind), Name, Name]))
      else
        WriteString(Format('%s<%s>False</%s>', [Indent(Ind), Name, Name]))
    end;
  begin
    OpenTag(Ind, 'KW', 'Name', G.Name);
    for i := 0 to fSchemes.Count-1 do begin
      G.ind := i;
      SaveColor('Attri', Ind+1, G.Attribs.Foreground, G.Attribs.Background, G.Attribs.Style, G.Attribs.ParentForeground, G.Attribs.ParentBackground);
    end;
    G.ind := fSchemeIndex;
	InsertTagBool(Ind+1, 'Enabled', G.Enabled);
    For i := 0 to G.KeyList.Count-1 do InsertTag(Ind+1, 'W', G.KeyList[i]);
    OpenTag(Ind, '/KW');
  end;

  procedure SaveSet(Ind: integer; S: TSynSet);
  var i: integer;
    procedure InsertTagBool(Ind: integer; Name: string; Value: Boolean);
    begin
      if Value then
        WriteString(Format('%s<%s>True</%s>', [Indent(Ind), Name, Name]))
      else
        WriteString(Format('%s<%s>False</%s>', [Indent(Ind), Name, Name]))
    end;
  begin
    OpenTag(Ind, 'Set', 'Name', S.Name);
    for i := 0 to fSchemes.Count-1 do begin
      S.ind := i;
      SaveColor('Attri', Ind+1, S.Attribs.Foreground, S.Attribs.Background, S.Attribs.Style, S.Attribs.ParentForeground, S.Attribs.ParentBackground);
    end;
    S.ind := fSchemeIndex;
    InsertTagBool(Ind+1, 'Enabled', S.Enabled);
{    if S.StartType = stAny then
      if S.BrakeType = btAny then
        InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'True')
      else
        InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'Left')
    else
      if S.BrakeType = btAny then
        InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'Right')
      else
        InsertTag(Ind+1, 'SymbolSetPartOfTerm', 'False');}

    InsertTag(Ind+1, 'S', SetToStr(S.SymbSet));
    OpenTag(Ind, '/Set');
  end;

  procedure SaveRange(Ind: integer; R: TSynRange);
  var i: integer;
    procedure InsertTagBool(Ind: integer; Name: string; Value: Boolean);
    begin
      if Value then
        WriteString(Format('%s<%s>True</%s>', [Indent(Ind), Name, Name]))
      else
        WriteString(Format('%s<%s>False</%s>', [Indent(Ind), Name, Name]))
    end;

  begin
    OpenTag(Ind, 'Range', 'Name', R.Name);
    for i := 0 to fSchemes.Count-1 do begin
      R.ind := i;
      SaveColor('Attri', Ind, R.Attribs.Foreground, R.Attribs.Background, R.Attribs.Style, R.Attribs.ParentForeground, R.Attribs.ParentBackground);
    end;
    R.ind := fSchemeIndex;
    InsertTagBool(Ind, 'Enabled', R.Enabled);
    if (Length(R.fRule.fOpenSymbol.Symbol) > 0) and (R.fRule.fOpenSymbol.Symbol[Length(R.fRule.fOpenSymbol.Symbol)] = #0) then begin
       InsertTag(Ind, 'OpenSymbol', copy(R.fRule.fOpenSymbol.Symbol,1,Length(R.fRule.fOpenSymbol.Symbol)-1));
       InsertTagBool(Ind, 'OpenSymbolFinishOnEol', true);
    end else begin
       InsertTag(Ind, 'OpenSymbol', R.fRule.fOpenSymbol.Symbol);
       InsertTagBool(Ind, 'OpenSymbolFinishOnEol', false);
    end;

    if (Length(R.fRule.fCloseSymbol.Symbol) > 0) and (R.fRule.fCloseSymbol.Symbol[Length(R.fRule.fCloseSymbol.Symbol)] = #0) then begin
       InsertTag(Ind, 'CloseSymbol', copy(R.fRule.fCloseSymbol.Symbol,1,Length(R.fRule.fCloseSymbol.Symbol)-1));
       InsertTagBool(Ind, 'CloseSymbolFinishOnEol', true);
    end else begin
       InsertTag(Ind, 'CloseSymbol', R.fRule.fCloseSymbol.Symbol);
       InsertTagBool(Ind, 'CloseSymbolFinishOnEol', false);
    end;
    if R.fRule.fOpenSymbol.StartLine = slFirst then
      InsertTag(Ind, 'OpenSymbolStartLine', 'True')
      else if R.fRule.fOpenSymbol.StartLine = slFirstNonSpace then
        InsertTag(Ind, 'OpenSymbolStartLine', 'NonSpace')
        else
          InsertTag(Ind, 'OpenSymbolStartLine', 'False');
    if R.fRule.fCloseSymbol.StartLine = slFirst then
      InsertTag(Ind, 'CloseSymbolStartLine', 'True')
      else if R.fRule.fCloseSymbol.StartLine = slFirstNonSpace then
        InsertTag(Ind, 'CloseSymbolStartLine', 'NonSpace')
        else
          InsertTag(Ind, 'CloseSymbolStartLine', 'False');
    InsertTag(Ind, 'DelimiterChars', SetToStr(R.TermSymbols));

    if R.fRule.fOpenSymbol.StartType = stAny then
      if R.fRule.fOpenSymbol.BrakeType = btAny then
        InsertTag(Ind, 'OpenSymbolPartOfTerm', 'True')
      else
        InsertTag(Ind, 'OpenSymbolPartOfTerm', 'Left')
    else
      if R.fRule.fOpenSymbol.BrakeType = btAny then
        InsertTag(Ind, 'OpenSymbolPartOfTerm', 'Right')
      else
        InsertTag(Ind, 'OpenSymbolPartOfTerm', 'False');
    if R.fRule.fCloseSymbol.StartType = stAny then
      if R.fRule.fCloseSymbol.BrakeType = btAny then
        InsertTag(Ind, 'CloseSymbolPartOfTerm', 'True')
      else
        InsertTag(Ind, 'CloseSymbolPartOfTerm', 'Left')
    else
      if R.fRule.fCloseSymbol.BrakeType = btAny then
        InsertTag(Ind, 'CloseSymbolPartOfTerm', 'Right')
      else
        InsertTag(Ind, 'CloseSymbolPartOfTerm', 'False');

    InsertTagBool(Ind, 'CloseOnTerm', R.fRule.fCloseOnTerm);
    InsertTagBool(Ind, 'CloseOnEol', R.fRule.fCloseOnEol);
    InsertTagBool(Ind, 'AllowPredClose', R.fRule.fAllowPredClose);
    InsertTagBool(Ind, 'CaseSensitive', R.CaseSensitive);
    For i := 0 to R.KeyListCount-1 do SaveKWGroup(Ind, R.KeyLists[i]);
    For i := 0 to R.SetCount-1 do SaveSet(Ind, R.Sets[i]);
    For i := 0 to R.RangeCount-1 do SaveRange(Ind+1, R.Ranges[i]);
    OpenTag(Ind, '/Range');
  end;

  procedure SaveInfo;
    var i: integer;
  begin
    OpenTag(1, 'Info');

    OpenTag(2, 'General');
    InsertTag(3, 'Name', info.General.Name);
    InsertTag(3, 'FileTypeName', info.General.Extensions);
//    InsertTag(3, 'Layout', info.General.Layout);
    OpenTag(2, '/General');

    OpenTag(2, 'Author');
    InsertTag(3, 'Name', Info.Author.Name);
    InsertTag(3, 'Email', Info.Author.Email);
    InsertTag(3, 'Web', Info.Author.Web);
    InsertTag(3, 'Copyright', Info.Author.Copyright);
    InsertTag(3, 'Company', Info.Author.Company);
    InsertTag(3, 'Remark', Info.Author.Remark);
    OpenTag(2, '/Author');

    OpenTag(2, 'Version');
    InsertTag(3, 'Version', IntToStr(Info.Version.Version));
    InsertTag(3, 'Revision', IntToStr(Info.Version.Revision));
    InsertTag(3, 'Date', FloatToStr(Info.Version.ReleaseDate));
    case Info.Version.VersionType of
      vtInternalTest: InsertTag(3, 'Type', 'Internal Test');
      vtBeta: InsertTag(3, 'Type', 'Beta');
      vtRelease: InsertTag(3, 'Type', 'Release');
    end;
    OpenTag(2, '/Version');

    OpenTag(2, 'History');
    for i := 0 to Info.history.count-1 do InsertTag(3, 'H', Info.history[i]);
    OpenTag(2, '/History');

    OpenTag(2, 'Sample');
    for i := 0 to Info.Sample.count-1 do InsertTag(3, 'S', Info.Sample[i]);
    OpenTag(2, '/Sample');

    OpenTag(1, '/Info');
  end;

  procedure SaveSchemes;
  var
    i: integer;
  begin
    InsertTag(1, 'SchemeIndex', IntToStr(fSchemeIndex));
    OpenTag(1, 'Schemes');
    for i := 0 to self.SchemesList.Count-1 do
      InsertTag(2, 'S', fSchemes.Strings[i]);
    OpenTag(1, '/Schemes');
  end;

begin
  OpenTag(0, 'UniHighlighter');
  OpenTag(1, 'ImportantInfo');
  WriteString(Indent(2)+'******* Please read carefully *************************');
  WriteString(Indent(2)+'* Please, make any changes in this file very carefuly!*');
  WriteString(Indent(2)+'* It is much more convenient to use native designer!  *');
  WriteString(Indent(2)+'*******************************************************');
  OpenTag(1, '/ImportantInfo');
  SaveInfo;
  SaveSchemes;
  SaveRange(1, self.MainRules);
  OpenTag(0, '/UniHighlighter');
end;

procedure TSynUniSyn.LoadHglFromXml(xml: TDOMNode);
var
  J: Integer;
  ChildNode: TDOMNode;
begin
  Clear;
  SchemeIndex := 0;
  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode:= xml.ChildNodes.Item[J];
    if SameText(ChildNode.NodeName, 'Info') then
      ReadInfo(ChildNode)
    else if SameText(ChildNode.NodeName, 'SchemeIndex') then
      SchemeIndex := StrToInt(ReadValue(ChildNode))
    else if SameText(ChildNode.NodeName, 'Schemes') then
      ReadSchemes(ChildNode)
    else if SameText(ChildNode.NodeName, 'Range') then
    begin
      fMainRules.LoadHglFromXml(ChildNode, fSchemes.Count, SchemeIndex);
    end
  end
end;

procedure TSynUniSyn.ReadInfo(xml: TDOMNode);
var
  I, J: Integer;
  ChildNode1, ChildNode2: TDOMNode;
  AFormatSettings: TFormatSettings;
begin
  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode1:= xml.ChildNodes.Item[J];
    if ChildNode1.NodeName = 'General' then
      begin
        for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
        begin
          ChildNode2 := ChildNode1.ChildNodes.Item[I];
          if ChildNode2.NodeName = 'Name' then Info.General.Name:= ReadValue(ChildNode2)
          else if ChildNode2.NodeName = 'FileTypeName' then Info.General.Extensions := ReadValue(ChildNode2)
          // else if ChildNode2.NodeName = 'Layout' then Info.General.Layout := ReadValue(ChildNode2);
        end;
      end
    else if ChildNode1.NodeName = 'Author' then
      begin
        for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
        begin
          ChildNode2 := ChildNode1.ChildNodes.Item[I];
          if ChildNode2.NodeName = 'Name' then Info.Author.Name:= ReadValue(ChildNode2)
          else if ChildNode2.NodeName = 'Email' then Info.Author.Email:= ReadValue(ChildNode2)
          else if ChildNode2.NodeName = 'Web' then Info.Author.Web:= ReadValue(ChildNode2)
          else if ChildNode2.NodeName = 'Copyright' then Info.Author.Copyright:= ReadValue(ChildNode2)
          else if ChildNode2.NodeName = 'Company' then Info.Author.Company:= ReadValue(ChildNode2)
          else if ChildNode2.NodeName = 'Remark' then Info.Author.Remark:= ReadValue(ChildNode2)
        end;
      end
    else if ChildNode1.NodeName = 'Version' then
      begin
        for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
        begin
          ChildNode2 := ChildNode1.ChildNodes.Item[I];
          if ChildNode2.NodeName = 'Version' then Info.Version.Version := StrToIntDef(ReadValue(ChildNode2), 0)
          else if ChildNode2.NodeName = 'Revision' then Info.Version.Revision := StrToIntDef(ReadValue(ChildNode2), 0)
          else if ChildNode2.NodeName = 'Date' then
          {
            try
              AFormatSettings:= DefaultFormatSettings;
              Info.Version.ReleaseDate := StrToFloat(ReadValue(ChildNode2), AFormatSettings);
            except
              AFormatSettings.DecimalSeparator := '.';
              try
                Info.Version.ReleaseDate := StrToFloat(ReadValue(ChildNode2), AFormatSettings);
              except
                // Ignore
              end;
            end
          }
          else if ChildNode2.NodeName = 'Type' then
            begin
              if ReadValue(ChildNode2) = 'Beta' then Info.Version.VersionType := vtBeta
              else if ReadValue(ChildNode2) = 'Release' then Info.Version.VersionType := vtRelease
              else Info.Version.VersionType := vtInternalTest
            end
          end;
        end
    else if ChildNode1.NodeName = 'History' then
      begin
        Info.History.Clear;
        for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
        begin
          ChildNode2 := ChildNode1.ChildNodes.Item[I];
          if ChildNode2.NodeName = 'H' then
            Info.History.Add(ReadValue(ChildNode2));
        end;
      end
    else if ChildNode1.NodeName = 'Sample' then
      begin
        Info.Sample.Clear;
        for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
        begin
          ChildNode2 := ChildNode1.ChildNodes.Item[I];
          if ChildNode2.NodeName = 'S' then
            Info.Sample.Add(ReadValue(ChildNode2));
        end;
      end;
  end;
end;

procedure TSynUniSyn.ReadSchemes(xml: TDOMNode);
var
  J: Integer;
  ChildNode: TDOMNode;
begin
  if fSchemes <> nil then begin
    fSchemes.Clear();
    //MainRules.ClearAttributes();
  end
  else
    raise Exception.Create(ClassName + '.ReadSchemes - property Schemes not initialized.');

  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode:= xml.ChildNodes.Item[J];
    if ChildNode.NodeName = 'S' then
      fSchemes.Add(ReadValue(ChildNode));
  end;
end;

procedure TSynUniSyn.LoadHglFromFile(FileName: string);
//: Load Highlighter'a from file
var
  F: TFileStreamUTF8;
begin
  if not FileExists(FileName) then
    raise Exception.Create(ClassName + '.LoadHglFromFile - "'+FileName+'" does not exists.');
  F := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadHglFromStream( F );
  finally
    F.Free;
  end;
end;

procedure TSynUniSyn.SaveHglToFile(FileName: string);
//: Save Highlighter to file
var
  F: TFileStreamUTF8;
begin
  if FileName = '' then
    raise exception.Create(ClassName + '.SaveHglToFile - FileName is empty');
  F := TFileStreamUTF8.Create(FileName, fmCreate);
  try
    SaveHglToStream( F );
  finally
    F.Free;
  end;
end;

procedure TSynUniSyn.LoadHglFromStream(Stream: TStream);
var
  xml: TXMLDocument = nil;
begin
  try
    ReadXMLFile(xml, Stream);
    LoadHglFromXml(xml);
  finally
    xml.Free;
  end;
  DefHighlightChange( Self );
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynUniSyn);
{$ENDIF}

end.

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

unit SynUniClasses;

interface

uses
  SysUtils, Graphics,
  Classes, SynEditHighlighter, Contnrs, Laz2_DOM;

type
  TSymbSet  = set of char;

  TSynInfo      = class;
  TStreamWriter = class;
  TSynSymbol    = class;
  TSymbolNode   = class;
  TSymbolList   = class;
  TSynRule      = class;

  TVersionType = (vtInternalTest, vtBeta, vtRelease);

  TAuthorInfo = record
    Name:      string;     Email:   string;     Web:    string;
    Copyright: string;     Company: string;     Remark: string;
  end;

  TVerInfo = record
    Version:     integer;          Revision:    integer;
    VersionType: TVersionType;     ReleaseDate: TDateTime;
  end;

  THighInfo = record
    Name:       string;    Extensions: string; Other: Boolean
  end;

  TSynInfo = class //Vitalik 2004
    Author:  TAuthorInfo;
    Version: TVerInfo;
    General: THighInfo;
    History: TStringList;
    Sample:  TStringlist;
    constructor Create();
    procedure Clear();
    procedure LoadFromXml(xml: TDOMNode);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream; Ind: integer = 0); overload;
    procedure SaveToStream(StreamWriter: TStreamWriter; Ind: integer = 0); overload;
  end;

  TSynEditProperties = class //Vitalik 2004

  end;

  TSymbStartType = (stUnspecified, stAny, stTerm); //Vitalik 2004
  TSymbBrakeType = (btUnspecified, btAny, btTerm);
  TSymbStartLine = (slNotFirst, slFirst, slFirstNonSpace); //Vitalik 2004

  TStreamWriter = class //Vitalik 2004
    Stream: TStream;
    constructor Create(aStream: TStream);
    procedure WriteString(const Str: string);
    procedure InsertTag(Ind: integer; Name: string; Value: string);
    procedure WriteTag(Ind: integer; Name: string; EndLine: boolean = False);
    procedure WriteParam(Key, Value: string; CloseTag: string = '');
    procedure WriteBoolParam(Key: string; Value, Default: boolean; CloseTag: string = '');
  end;

  TSynAttributes = class (TSynHighlighterAttributes) //Vitalik 2004
  public
//    UseStyle: boolean;
    OldColorForeground: TColor;
    OldColorBackground: TColor;
    ParentForeground: boolean;
    ParentBackground: boolean;
    constructor Create(Name: string);
//    destructor Destroy(); override;
    procedure LoadFromString(Value: string);
    procedure SaveToStream(StreamWriter: TStreamWriter);
  end;

  TAbstractRule = class;

  TSynSymbol = class
  public
    Symbol: string;
    fOpenRule: TAbstractRule;
    StartType: TSymbStartType; //Vitalik 2004
    BrakeType: TSymbBrakeType;
    StartLine: TSymbStartLine; //Vitalik 2004
    Attributes: TSynHighlighterAttributes;
    constructor Create(st: string; Attribs: TSynHighlighterAttributes); virtual;
    destructor Destroy(); override;
  end;

  TSymbolNode = class
    ch: char;
    BrakeType: TSymbBrakeType;
    StartType: TSymbStartType; //Vitalik 2004
    NextSymbs: TSymbolList;
    tkSynSymbol: TSynSymbol;
    constructor Create(AC: char; SynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType); overload; virtual;
    constructor Create(AC: char); overload;
    destructor Destroy(); override;
  end;

  TSymbolList = class
    SymbList: TList; //Vitalik 2004
    procedure AddSymbol(symb: TSymbolNode);
    procedure SetSymbolNode(Index: Integer; Value: TSymbolNode);
    function  FindSymbol(ch: char): TSymbolNode;
    function  GetSymbolNode(Index: integer): TSymbolNode;
    function  GetCount(): integer;
    property  Nodes[index: integer]: TSymbolNode read GetSymbolNode write SetSymbolNode;
    property  Count: Integer read GetCount;
    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  TSynUniStyles = class (TObjectList)
  public
    FileName: string;
    constructor Create();
    destructor Destroy(); override;
    function GetStyle(const Name: string): {TSynHighlighter}TSynAttributes;
    function GetStyleDef(const Name: string; const Def: {TSynHighlighter}TSynAttributes): {TSynHighlighter}TSynAttributes;
    procedure AddStyle(Name: string; Foreground, Background: TColor; FontStyle: TFontStyles);
    procedure ListStylesNames(const AList: TStrings);
    function GetStylesAsXML(): string;
    procedure Load();
    procedure Save();
  end;

  TAbstractRule = class //Vitalik 2004
    Enabled: boolean;
    constructor Create();
  end;

  TSynRule = class(TAbstractRule) //Vitalik 2004
  public
    Ind: integer; //temp
    Name: string;
    Attribs: TSynAttributes;
    Style: string;
    Styles: TSynUniStyles;
    constructor Create();
    destructor Destroy(); override;
    procedure LoadFromXml(xml: TDOMNode); virtual; abstract;
    procedure LoadFromStream(aSrc: TStream);
    procedure LoadFromFile(FileName: string);
    function  GetAsStream(): TMemoryStream;
    procedure SaveToStream(Stream: TStream; Ind: integer = 0); overload;
    procedure SaveToStream(StreamWriter: TStreamWriter; Ind: integer = 0); overload; virtual; abstract;
  end;

function StrToSet(st: string): TSymbSet;
function SetToStr(st: TSymbSet): string;
function StrToFontStyle(Style: string): TFontStyles;
function FontStyleToStr(Style: TFontStyles): string;
procedure FreeList(var List: TList);
procedure ClearList(List: TList);
function Indent(i: integer): string;

const
  AbsoluteTermSymbols: TSymbSet = [#0, #9, #10, #13, #32];
  EOL = #13#10;
  CloseEmptyTag = '/>';
  CloseStartTag = '>';

implementation

uses
  Laz2_XMLRead;

function StrToSet(st: string): TSymbSet;
var i: integer;
begin
  result := [];
  for i := 1 to length(st) do Result := Result + [st[i]];
end;

function SetToStr(st: TSymbSet): string;
var b: byte;
begin
  Result := '';
  for b := 1 to 255 do
    if (chr(b) in st) and (not (chr(b) in AbsoluteTermSymbols)) then
      Result := Result+chr(b);
end;

function StrToFontStyle(Style: string): TFontStyles;
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

function FontStyleToStr(Style: TFontStyles): string;
begin
  Result := '';
  if fsBold in Style then Result := Result + 'B';
  if fsItalic in Style then Result := Result + 'I';
  if fsUnderline in Style then Result := Result + 'U';
  if fsStrikeOut in Style then Result := Result + 'S';
end;

procedure FreeList(var List: TList);
var i: integer;
begin
  if List = nil then exit;
  for i := 0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Free;
  List := nil;
end;

procedure ClearList(List: TList);
var i: integer;
begin
  if List = nil then exit;
  for i := 0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Clear;
end;

//==== TInfo =================================================================
constructor TSynInfo.Create();
begin
  inherited;
end;

procedure TSynInfo.Clear();
begin
  General.Other       := False;
  General.Name        := '';
  General.Extensions  := '';
  Author.Name         := '';
  Author.Email        := '';
  Author.Web          := '';
  Author.Copyright    := '';
  Author.Company      := '';
  Author.Remark       := '';
  Version.Version     := 0;
  Version.Revision    := 0;
  Version.ReleaseDate := 0;
  Version.VersionType := vtInternalTest;
  History.Clear;
  Sample.Clear;
end;

function ReadValue(ANode: TDOMNode): String;
begin
  if Assigned(ANode.FirstChild) then
    Result:= ANode.FirstChild.NodeValue
  else
    Result:= EmptyStr;
end;

procedure TSynInfo.LoadFromXml(xml: TDOMNode);
var
  i, J: integer;
  Key, Value: string;
  ChildNode1, ChildNode2: TDOMNode;
begin
  for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
  begin
    ChildNode1:= xml.ChildNodes.Item[J];
    if SameText('General', ChildNode1.NodeName) then
      for i := 0 to Int32(ChildNode1.Attributes.Length) - 1 do begin
        Key := ChildNode1.Attributes[i].NodeName; Value := ChildNode1.Attributes[i].NodeValue;
        if SameText('Name', Key)       then General.Name       := Value else
        if SameText('Extensions', Key) then General.Extensions := Value else
        if SameText('Other', Key) then General.Other := StrToBoolDef(Value, False)
      end else
    if SameText('Author', ChildNode1.NodeName) then
      for i := 0 to Int32(ChildNode1.Attributes.Length) - 1 do begin
        Key := ChildNode1.Attributes[i].NodeName; Value := ChildNode1.Attributes[i].NodeValue;
        if SameText('Name', Key)      then Author.Name      := Value else
        if SameText('Email', Key)     then Author.Email     := Value else
        if SameText('Web', Key)       then Author.Web       := Value else
        if SameText('Copyright', Key) then Author.Copyright := Value else
        if SameText('Company', Key)   then Author.Company   := Value else
        if SameText('Remark', Key)    then Author.Remark    := Value else
      end else
    if SameText('Version', ChildNode1.NodeName) then
      for i := 0 to Int32(ChildNode1.Attributes.Length) - 1 do begin
        Key := ChildNode1.Attributes[i].NodeName; Value := ChildNode1.Attributes[i].NodeValue;
        if SameText('Version', Key)  then Version.Version  := StrToIntDef(Value, 0) else
        if SameText('Revision', Key) then Version.Revision := StrToIntDef(Value, 0) else
        if SameText('Date', Key) then
          try
            Value := StringReplace(Value, ',', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]); // Since no one ever call something like "GetFormatSettings", "DefaultFormatSettings" still hold the default values.
            Value := StringReplace(Value, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]); // Just in case there is something we did not think about.
            Version.ReleaseDate := StrToFloat(Value, DefaultFormatSettings);
          except
            // Ignore
          end
        else if SameText('Type', Key) then
          if Value = 'Beta'    then Version.VersionType := vtBeta else
          if Value = 'Release' then Version.VersionType := vtRelease else
                                    Version.VersionType := vtInternalTest
      end else
    if SameText('History', ChildNode1.NodeName) then begin
      History.Clear; Sample.Clear;
      for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
      begin
        ChildNode2 := ChildNode1.ChildNodes.Item[I];
        if ChildNode2.NodeName = 'H' then
          History.Add(ReadValue(ChildNode2));
      end;
    end else
    if SameText('Sample', ChildNode1.NodeName) then begin
      Sample.Clear;
      for I:= 0 to Int32(ChildNode1.ChildNodes.Count) - 1 do
      begin
        ChildNode2 := ChildNode1.ChildNodes.Item[I];
        if ChildNode2.NodeName = 'S' then
          Sample.Add(ReadValue(ChildNode2));
      end;
    end;
  end;
end;

procedure TSynInfo.LoadFromStream(Stream: TStream);
var
  xml: TXMLDocument = nil;
begin
  try
    ReadXMLFile(xml, Stream);
    LoadFromXml(xml);
  finally
    xml.Free;
  end;
end;

procedure TSynInfo.SaveToStream(Stream: TStream; Ind: integer);
var
  StreamWriter: TStreamWriter;
begin
  StreamWriter := TStreamWriter.Create(Stream);
  SaveToStream(StreamWriter, Ind);
  StreamWriter.Free;
end;

procedure TSynInfo.SaveToStream(StreamWriter: TStreamWriter; Ind: integer);
var
  i: integer;
begin
  with StreamWriter do begin
    WriteTag(Ind, 'Info', True);

    WriteTag(Ind+2, 'General');
    WriteParam('Name',       General.Name);
    WriteParam('Extensions', General.Extensions, CloseEmptyTag);
    WriteParam('Other',      BoolToStr(General.Other), CloseEmptyTag);

    WriteTag(Ind+2, 'Author');
    WriteParam('Name',      Author.Name);
    WriteParam('Email',     Author.Email);
    WriteParam('Web',       Author.Web);
    WriteParam('Copyright', Author.Copyright);
    WriteParam('Company',   Author.Company);
    WriteParam('Remark',    Author.Remark, CloseEmptyTag);

    WriteTag(Ind+2, 'Version');
    WriteParam('Version',  IntToStr(Version.Version));
    WriteParam('Revision', IntToStr(Version.Revision));
    WriteParam('Date',     FloatToStr(Version.ReleaseDate), CloseEmptyTag);
{    case Version.VersionType of
      vtInternalTest: WriteParam('Type', 'Internal Test');
      vtBeta: WriteParam('Type', 'Beta');
      vtRelease: WriteParam('Type', 'Release');
    end;}

    WriteTag(Ind+2, 'History', True);
    for i := 0 to History.Count-1 do InsertTag(Ind+4, 'H', History[i]);
    WriteTag(Ind+2, '/History', True);

    WriteTag(Ind+2, 'Sample', True);
    for i := 0 to Sample.Count-1 do InsertTag(Ind+4, 'S', Sample[i]);
    WriteTag(Ind+2, '/Sample', True);

    WriteTag(Ind, '/Info', True);
  end;
end;
//==== TStreamWriter =========================================================
  function Indent(i: integer): string;
  begin
    SetLength(Result, i);
//    if i > 0 then !!!!!!!!!!!!!!!!!!!!!!!!!
{To prevent error...}
{$IFDEF FPC}
    if i > 0 then
{$ENDIF}
    FillChar(Result[1], i, #32);
  end;

  function GetValidValue(Value: string): string;
  begin
    Value := StringReplace(Value, '&', '&amp;', [rfReplaceAll, rfIgnoreCase]);
    Value := StringReplace(Value, '<', '&lt;', [rfReplaceAll, rfIgnoreCase]);
    Value := StringReplace(Value, '"', '&quot;', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Value, '>', '&gt;', [rfReplaceAll, rfIgnoreCase]);
  end;
  
  constructor TStreamWriter.Create(aStream: TStream);
  begin
    Stream := aStream;
  end;

  procedure TStreamWriter.WriteString(const Str: string);
  begin
    Stream.Write(Str[1], Length(Str));
  end;

  procedure TStreamWriter.InsertTag(Ind: integer; Name: string; Value: string);
  begin
    WriteString(Format('%s<%s>%s</%s>'+EOL, [Indent(Ind), Name, GetValidValue(Value), Name]));
  end;

{  procedure OpenTag(Ind: integer; Name: string; Param: string = ''; ParamValue: string = '');
  begin
    if Param = '' then
      WriteString(Format('%s<%s>', [Indent(Ind), Name]))
    else
      WriteString(Format('%s<%s %s="%s">', [Indent(Ind), Name, Param, GetValidValue(ParamValue)]));
  end;}

  procedure TStreamWriter.WriteTag(Ind: integer; Name: string; EndLine: boolean = False);
  begin
    WriteString(Format('%s<%s', [Indent(Ind), Name]));
    if EndLine then WriteString('>' + EOL);
  end;

{  procedure SaveColor(MainTag: string; Ind, Fore, Back: integer; Style: TFontStyles; PFore, PBack: boolean);
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
    InsertTag(Ind+1, 'Style', Fs2String(Style));
    InsertTagBool(Ind+1, 'ParentForeground', PFore);
    InsertTagBool(Ind+1, 'ParentBackground', PBack);
    OpenTag(Ind, '/'+MainTag);
  end;}

  procedure TStreamWriter.WriteParam(Key, Value: string; CloseTag: string = '');
  begin
    WriteString(Format(' %s="%s"', [Key, GetValidValue(Value)]));
    if CloseTag <> '' then WriteString(CloseTag + EOL);
  end;

  procedure TStreamWriter.WriteBoolParam(Key: string; Value, Default: boolean; CloseTag: string = '');
  begin
    If Value <> Default then
      WriteParam(Key, BoolToStr(Value,True), CloseTag);
  end;

//==== TAttributes ===========================================================
constructor TSynAttributes.Create(Name: String);
begin
//  Std := TSynHighlighterAttributes.Create(SYNS_AttrDefaultPackage);
  inherited Create(Name{SYNS_AttrDefaultPackage});
//  UseStyle := False;
end;

{destructor TSynAttributes.Destroy;
//var xml: TXMLParser;
begin
//  if not UseStyle then
    //Std.Free;
//  xml := TXMLParser.Create;
//  xml.Standalone
  inherited;
end;
}
procedure TSynAttributes.LoadFromString(Value: string);
begin
  ParentForeground := False;
  ParentBackground := False;
  Foreground := StrToIntDef(Copy(Value, 1, pos(',',Value)-1), 0);
  OldColorForeground := Foreground;
  Background := StrToIntDef(Copy(Value, pos(',',Value)+1, pos(';',Value)-pos(',',Value)-1), $FFFFFF);
  OldColorBackground := Background;
  ParentForeground := LowerCase(Copy(Value, pos(';',Value)+1, pos(':',Value)-pos(';',Value)-1)) = 'true';
  ParentBackground := LowerCase(Copy(Value, pos(':',Value)+1, pos('.',Value)-pos(':',Value)-1)) = 'true';
  Style := StrToFontStyle(Copy(Value, pos('.',Value)+1, Length(Value)-pos('.',Value)));
//  '12345,0;true:false.'
{  Std.Background := StrToIntDef(Value, $FFFFFF);
  OldColorBackground := Std.Background;
  Std.Foreground := StrToIntDef(Value, 0);
  OldColorForeground := Std.Foreground;
  Std.Style := String2Fs(Value)
  ParentForeground := LoweValue = 'true'
  ParentBackground := LowerValue = 'true';}
end;

procedure TSynAttributes.SaveToStream(StreamWriter: TStreamWriter);
begin
  with StreamWriter do
    WriteParam('Attributes', IntToStr(Foreground)+','+IntToStr(Background)+';'+
                             BoolToStr(ParentForeground,True)+':'+
                             BoolToStr(ParentBackground,True)+'.'+
                             FontStyleToStr(Style));
end;

//==== TSynSymbol ============================================================
constructor TSynSymbol.Create(st: string; Attribs: TSynHighlighterAttributes);
//: Constructor of TSynSymbol
begin
  Attributes := Attribs;
  Symbol := st;
  fOpenRule := nil;
  StartLine := slNotFirst;
  StartType := stUnspecified;
  BrakeType := btUnspecified;
end;

destructor TSynSymbol.Destroy;
//: Destructor of TSynSymbol
begin
  inherited;
end;

//==== TSymbolNode ===========================================================
constructor TSymbolNode.Create(AC: char; SynSymbol: TSynSymbol;
  ABrakeType: TSymbBrakeType);
begin
  ch := AC;
  NextSymbs := TSymbolList.Create;
  BrakeType := ABrakeType;
  StartType := SynSymbol.StartType;
  tkSynSymbol := SynSymbol;
end;

constructor TSymbolNode.Create(AC: char);
begin
  ch := AC;
  NextSymbs := TSymbolList.Create;
  tkSynSymbol := nil;
end;

destructor TSymbolNode.Destroy;
//: Destructor of TSymbolNode
begin
  NextSymbs.Free;
  inherited;
end;

//==== TSymbolList ===========================================================
procedure TSymbolList.AddSymbol(symb: TSymbolNode);
//: Add Node to SymbolList
begin
  SymbList.Add(symb);
end;

constructor TSymbolList.Create;
//: Constructor of TSymbolList
begin
  SymbList := TList.Create;
end;

destructor TSymbolList.Destroy;
//: Destructor of TSymbolList
begin
  FreeList(SymbList);
  inherited;
end;

function TSymbolList.FindSymbol(ch: char): TSymbolNode;
//: Find Node in SymbolList by char
var
  i: integer;
begin
  Result := nil;
  for i := 0 to SymbList.Count-1 do
    if TSymbolNode(SymbList[i]).ch = ch then
    begin
      Result := TSymbolNode(SymbList[i]);
      break;
    end;
end;

function TSymbolList.GetCount: integer;
//: Return Node count in SymbolList
begin
  Result := SymbList.Count
end;

function TSymbolList.GetSymbolNode(Index: integer): TSymbolNode;
//: Return Node in SymbolList by index
begin
  Result := TSymbolNode(SymbList[index]);
end;

procedure TSymbolList.SetSymbolNode(Index: Integer; Value: TSymbolNode);
//: Set Node in SymbolList bt index
begin
  if Index < SymbList.Count then
    TSymbolNode(SymbList[index]).Free;
  SymbList[index] := Value;
end;

constructor TAbstractRule.Create();
begin
  Enabled := True;
end;

//==== TSynRule ==============================================================
{function TSynRule.GetAttribs: TAttributes;
begin
  if (Ind < 0) or (Ind >= AttribsList.Count) then
    raise Exception.CreateFmt ('Invalid index: %d', [Ind]);
  Result := TAttributes(AttribsList[Ind]);
end;

function TSynRule.GetAttribsByIndex(Index: integer): TAttributes;
begin
  if (Index < 0) or (Index >= AttribsList.Count) then
    raise Exception.CreateFmt ('Invalid index: %d', [Ind]);
  Result := TAttributes(AttribsList[Index]);
end;
}
constructor TSynRule.Create;
begin
  inherited;
  ind := -1;
  Attribs := TSynAttributes.Create('unknown');
//  AttribsList := TList.Create;
end;

destructor TSynRule.Destroy;
begin
//  FreeList(AttribsList);
  Attribs.Free;
  inherited Destroy;
end;

{function TSynRule.AddAttribute(): integer;
var
  i: integer;
begin
  ind := AttribsList.Add(TAttributes.Create);
  Attribs.ParentForeground := True;
  Attribs.ParentBackground := True;
  Attribs.Std.Foreground := clBlack;
  Attribs.Std.Background := clWhite;
  Attribs.OldColorForeground := Attribs.Std.Foreground;
  Attribs.OldColorBackground := Attribs.Std.Background;
  Attribs.Std.Style := [];
  Result := ind;

  if self is TSynRange then
    with self as TSynRange do begin
      for i := 0 to RangeCount-1 do
        Ranges[i].AddAttribute();
      for i := 0 to KeyListCount-1 do
        KeyLists[i].AddAttribute();
      for i := 0 to SetCount-1 do
        Sets[i].AddAttribute();
    end;
end;

procedure TSynRule.DeleteAttributes(Index: integer);
var
  i: integer;
begin
  AttribsList.Delete(Index);
  if AttribsList.Count = Index then
    ind := Index-1
  else
    ind := Index;
  if self is TSynRange then
    with self as TSynRange do begin
      for i := 0 to RangeCount-1 do
        Ranges[i].DeleteAttributes(Index);
      for i := 0 to KeyListCount-1 do
        KeyLists[i].DeleteAttributes(Index);
      for i := 0 to SetCount-1 do
        Sets[i].DeleteAttributes(Index);
    end;
end;

procedure TSynRule.ClearAttributes();
var
  i: integer;
begin
  ClearList(AttribsList);
  ind := -1;

  if self is TSynRange then
    with self as TSynRange do begin
      for i := 0 to RangeCount-1 do
        Ranges[i].ClearAttributes();
      for i := 0 to KeyListCount-1 do
        KeyLists[i].ClearAttributes();
      for i := 0 to SetCount-1 do
        Sets[i].ClearAttributes();
    end;
end;

procedure TSynRule.SetAttributesIndex(Index: integer);
var
  i: integer;
begin
  ind := Index;

  if self is TSynRange then
    with self as TSynRange do begin
      for i := 0 to RangeCount-1 do
        Ranges[i].SetAttributesIndex(Index);
      for i := 0 to KeyListCount-1 do
        KeyLists[i].SetAttributesIndex(Index);
      for i := 0 to SetCount-1 do
        Sets[i].SetAttributesIndex(Index);
    end;
end;}

function TSynRule.GetAsStream: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(Result);
end;

procedure TSynRule.SaveToStream(Stream: TStream; Ind: integer = 0);
var
  StreamWriter: TStreamWriter;
begin
  StreamWriter := TStreamWriter.Create(Stream);
  SaveToStream(StreamWriter, Ind);
  StreamWriter.Free;
end;

procedure TSynRule.LoadFromStream(aSrc: TStream);
var
  I: Integer;
  ChildNode: TDOMNode;
  TagName: UnicodeString;
  xml: TXMLDocument = nil;
begin
  if ClassName = 'TSynRange'   then TagName := 'Range' else
  if ClassName = 'TSynKeyList' then TagName := 'Keywords' else
  if ClassName = 'TSynSet'     then TagName := 'Set' else
    raise Exception.Create(ClassName + '.LoadFromStream - Unknown rule to load!');
  try
    ReadXMLFile(xml, aSrc);
    for I:= 0 to Int32(xml.ChildNodes.Count) - 1 do
    begin
      ChildNode:= xml.ChildNodes.Item[I];
      if SameText(ChildNode.NodeName, TagName) then
        LoadFromXml(ChildNode);
    end;
  finally
    xml.Free;
  end;
end;

procedure TSynRule.LoadFromFile(FileName: string);
var
  xml: TXMLDocument = nil;
begin
  if not FileExists(FileName) then
    raise Exception.Create(ClassName + '.LoadFromFile - "'+FileName+'" does not exists.');

  try
    ReadXMLFile(xml, FileName);
    LoadFromXml(xml);
  finally
    xml.Free;
  end;
end;

//==== TSynUniStyles =========================================================
constructor TSynUniStyles.Create;
begin
  Self.OwnsObjects := True;
end;

destructor TSynUniStyles.Destroy;
begin
  inherited;
end;

function TSynUniStyles.GetStyle(const Name: string): {TSynHighlighter}TSynAttributes;
begin
  Result := GetStyleDef(Name, nil);
end;

function TSynUniStyles.GetStyleDef(const Name: string;
  const Def: {TSynHighlighter}TSynAttributes): {TSynHighlighter}TSynAttributes;
var
  i: integer;
begin
  Result := Def;
  for i := 0 to Self.Count-1 do
    if SameText({TSynHighlighter}TSynAttributes(Self.Items[i]).Name, Name) then begin
      Result := {TSynHighlighter}TSynAttributes(Self.Items[i]);
      Exit;
    end;
end;

procedure TSynUniStyles.AddStyle(Name: string; Foreground, Background: TColor;
  FontStyle: TFontStyles);
var
  Atr: {TSynHighlighter}TSynAttributes;
begin
  Atr := {TSynHighlighter}TSynAttributes.Create(Name);
  Atr.Foreground := Foreground;
  Atr.Background := Background;
  Atr.Style := FontStyle;
  Self.Add(Atr);
end;

procedure TSynUniStyles.ListStylesNames(const AList: TStrings);
var
  i: integer;
begin
  aList.BeginUpdate;
  try
    aList.Clear;
    for i := 0 to Self.Count-1 do
      aList.Add({TSynHighlighter}TSynAttributes(Self.Items[i]).Name);
  finally
    aList.EndUpdate;
  end;
end;

function TSynUniStyles.GetStylesAsXML: string;
var
  i: integer;
begin
//  Result:= '<?xml version="1.0" encoding="ISO-8859-1"?>'#13#10#13#10';
  Result := '<Schemes>'#13#10;
  Result := Result + '  <Scheme Name="Default">'#13#10;
  for i := 0 to Self.Count-1 do
    with {TSynHighlighter}TSynAttributes(Self.Items[I]) do
      Result := Result + '    <Style Name="' + Name +
                         '" Fg="' + IntToStr(Foreground) +
                         '" Bg="' + IntToStr(Background) +
                         '" FontStyle="' + FontStyleToStr(Style) + '/>'#13#10;
  Result := Result + '  </Scheme>'#13#10 + '</Schemes>';
end;

procedure TSynUniStyles.Load;
var
  xml: TXMLDocument = nil;
  i, J: integer;
  Name, FontStyle, Key, Value: string;
  Foreground, Background: TColor;
  Style: {TSynHighlighter}TSynAttributes;
  ChildNode: TDOMNode;
begin
  if not FileExists(FileName) then
    raise Exception.Create(ClassName + '.Load - "' + FileName + '" does not exists.');

  Clear;
  try
    ReadXMLFile(xml, FileName);
    for J := 0 to Int32(xml.ChildNodes.Count) - 1 do
    begin
      ChildNode:= xml.ChildNodes.Item[J];
      if SameText(ChildNode.NodeName, 'Style') then begin
        Name := '';
        Foreground := clLime;
        Background := clFuchsia;
        FontStyle := '';
        for i := 0 to Int32(ChildNode.Attributes.Length) - 1 do begin
          Key := ChildNode.Attributes[i].NodeName;
          Value := ChildNode.Attributes[i].NodeValue;
          if SameText('Name', Key) then Name := Value else
          if SameText('Foreground', Key) then Foreground := StrToIntDef(Value, clBlack) else
          if SameText('Background', Key) then Background := StrToIntDef(Value, clWhite) else
          if SameText('FontStyle', Key) then FontStyle := Value else
        end;
        Style := Self.GetStyle(Name);
        if Style <> nil then begin
          Style.Foreground := Foreground;
          Style.Background := Background;
          Style.Style := StrToFontStyle(FontStyle);
        end else
          Self.AddStyle(Name, Foreground, Background, StrToFontStyle(FontStyle));
      end;
    end;
  finally
    FreeAndNil(xml);
  end;
end;

procedure TSynUniStyles.Save;
var
  S: string;
begin
  if not FileExists(FileName) then
    raise Exception.Create(ClassName + '.Save - "' + FileName + '" does not exists.');

  S := Self.GetStylesAsXML;
  with TFileStream.Create(FileName, fmOpenWrite) do
    try
      Write(Pointer(S)^, length(S));
    finally
      Free;
    end;
end;

end.

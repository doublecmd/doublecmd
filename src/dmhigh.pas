unit dmHigh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, SynEdit, DCStringHashListUtf8, LCLVersion,
  SynEditHighlighter, SynHighlighterJava, SynHighlighterXML, SynHighlighterLFM,
  SynHighlighterPHP, SynHighlighterSQL, SynHighlighterCss, SynHighlighterPython,
  SynHighlighterVB, SynHighlighterLua, SynUniHighlighter,
  uHighlighters, uColors, fpJson;

const
  HighlighterConfig = 'highlighters.xml';

type

  { TdmHighl }

  TdmHighl = class(TComponent)
  private
    FTemp: Boolean;
    procedure LoadColors(AConfig: TJSONObject);
    procedure SaveColors(AConfig: TJSONObject);
    procedure LoadUniColors(AConfig: TJSONObject);
    procedure SaveUniColors(AConfig: TJSONObject);
  private
    procedure CreateHighlighters;
    procedure LoadUniHighlighters;
    function GetSyn(Index: Integer): TSynCustomHighlighter;
    function GetSyn(AClass: TSynCustomHighlighterClass): TSynCustomHighlighter;
    procedure CopyHighlighter(SourceHighlighter, TargetHighlighter: TSynCustomHighlighter);
  public
    SynHighlighterList: TStringList;
    SynHighlighterHashList: TStringHashListUtf8;
  public
    constructor Create(AOwner: TComponent; ATemp: Boolean); overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TdmHighl;
    procedure CopyFrom(ASource: TdmHighl);
    function GetHighlighter(SynEdit: TCustomSynEdit; const sExtension: string): TSynCustomHighlighter;
    procedure SetHighlighter(SynEdit: TCustomSynEdit; Highlighter: TSynCustomHighlighter);
    property Highlighters[Index: Integer]: TSynCustomHighlighter read GetSyn;
    property SynPlainTextHighlighter: TSynCustomHighlighter index 0 read GetSyn;
  end;

  { THighlighters }

  THighlighters = class
  private
    FStyle: Integer;
    FStyles: array[0..Pred(THEME_COUNT)] of TdmHighl;
  public
    constructor Create;
    procedure UpdateStyle;
    procedure LoadDefaults;
    function Current: TdmHighl;
    procedure Load(const FileName: String); overload;
    procedure Save(const FileName: String); overload;
    procedure LoadColors(AConfig: TJSONObject); overload;
    procedure SaveColors(AConfig: TJSONObject); overload;
  end;

function dmHighl: TdmHighl; inline;

var
  gHighlighters: THighlighters;

implementation

uses
  Graphics, SynEditTypes, SynUniClasses, FileUtil, uHighlighterProcs, DCXmlConfig,
  LCLType, DCJsonConfig, uGlobsPaths, DCClassesUtf8, DCOSUtils, DCStrUtils, uLng,
  uGlobs, uSysFolders, SynUniRules;

const
  ConfigVersion = 2;

const
  DEFAULT_HIGHLIGHTERS: array[0..19] of TSynCustomHighlighterClass =
  (
    TSynPlainTextHighlighter,
    TSynXMLSyn, TSynPerlSynEx, TSynPythonSyn, TSynUNIXShellScriptSynEx,
    TSynBatSynEx, TSynCppSynEx, TSynCssSyn, TSynDiffSynEx,
    TSynHTMLSynEx, TSynIniSynEx, TSynJavaSyn, TSynLFMSyn,
    TSynPasSynEx, TSynPHPSyn, TSynPoSynEx, TSynSQLSyn,
    TSynTeXSynEx, TSynVBSyn, TSynLuaSyn
  );

function SynHighlighterSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if CompareStr(List[Index1], rsSynLangPlainText) = 0 then
    Result:= -1
  else if CompareStr(List[Index2], rsSynLangPlainText) = 0 then
    Result:=  1
  else
    Result:= CompareStr(List[Index1], List[Index2]);
end;

function dmHighl: TdmHighl;
begin
  Result:= gHighlighters.Current;
end;

{ TdmHighl }

procedure TdmHighl.LoadColors(AConfig: TJSONObject);
var
  I, J, K: Integer;
  AName, AValue: String;
  AValues: TStringArray;
  AList, Attributes: TJSONArray;
  AItem, AttributeNode: TJSONObject;
  Highlighter: TSynCustomHighlighter;
  Attribute: TSynHighlighterAttributes;
begin
  if AConfig.Find('Highlighters', AList) then
  begin
    for I:= 0 to AList.Count - 1 do
    begin
      AItem:= AList.Objects[I];
      AName:= AItem.Get('Name', EmptyStr);
      Highlighter:= TSynCustomHighlighter(SynHighlighterHashList.Data[AName]);
      if Assigned(Highlighter) and (not (Highlighter is TSynUniSyn)) then
      begin
        Attributes:= AItem.Get('Attributes', TJSONArray(nil));
        if Assigned(Attributes) and (Attributes.Count > 0) then
        begin
          for J:= 0 to Attributes.Count - 1 do
          begin
            AttributeNode:= Attributes.Objects[J];
            AName:= AttributeNode.Get('Name', EmptyStr);
            AValue:= AttributeNode.Get('Value', EmptyStr);
            AValues:= AValue.Split(['|'], TStringSplitOptions.ExcludeEmpty);

            if (Length(AName) = 0) or (Length(AValues) <> 7) then
              Continue;

            for K:= 0 to Highlighter.AttrCount - 1 do
            begin
              Attribute:= Highlighter.Attribute[K];
              if SameText(Attribute.StoredName, AName) then
              begin
                Attribute.Background := TColor(StrToIntDef(AValues[0], Integer(Attribute.Background)));
                Attribute.Foreground := TColor(StrToIntDef(AValues[1],  Integer(Attribute.Foreground)));
                Attribute.FrameColor := TColor(StrToIntDef(AValues[2], Integer(Attribute.FrameColor)));
                Attribute.FrameStyle := TSynLineStyle(StrToIntDef(AValues[3], Integer(Attribute.FrameStyle)));
                Attribute.FrameEdges := TSynFrameEdges(StrToIntDef(AValues[4], Integer(Attribute.FrameEdges)));
                Attribute.IntegerStyle := StrToIntDef(AValues[5], Attribute.IntegerStyle);
                Attribute.IntegerStyleMask  := StrToIntDef(AValues[6],  Attribute.IntegerStyleMask);
                Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TdmHighl.SaveColors(AConfig: TJSONObject);
var
  I, J: Integer;
  AValue: String;
  AttributeNode: TJSONObject;
  HighlighterNode: TJSONObject;
  AList, Attributes: TJSONArray;
  Highlighter: TSynCustomHighlighter;
  Attribute: TSynHighlighterAttributes;
begin
  if AConfig.Find('Highlighters', AList) then
    AList.Clear
  else begin
    AList:= TJSONArray.Create;
    AConfig.Add('Highlighters', AList);
  end;

  for I := 0 to SynHighlighterList.Count - 1 do
  begin
    if SynHighlighterList.Objects[I] is TSynUniSyn then Continue;
    Highlighter:= TSynCustomHighlighter(SynHighlighterList.Objects[I]);

    HighlighterNode:= TJSONObject.Create;
    HighlighterNode.Add('Name', Highlighter.LanguageName);

    Attributes:= TJSONArray.Create;

    for J:= 0 to Highlighter.AttrCount - 1 do
    begin
      Attribute:= Highlighter.Attribute[J];
      AttributeNode:= TJSONObject.Create;
      AttributeNode.Add('Name', Attribute.StoredName);

      AValue:= '$' + HexStr(Attribute.Background, 8) + '|' +
               '$' + HexStr(Attribute.Foreground, 8) + '|' +
               '$' + HexStr(Attribute.FrameColor, 8) + '|' +
               IntToStr(Integer(Attribute.FrameStyle)) + '|' +
               IntToStr(Integer(Attribute.FrameEdges)) + '|' +
               IntToStr(Attribute.IntegerStyle) + '|' +
               IntToStr(Attribute.IntegerStyleMask);

      AttributeNode.Add('Value', AValue);

      Attributes.Add(AttributeNode);
    end;
    HighlighterNode.Add('Attributes', Attributes);
    AList.Add(HighlighterNode);
  end;
end;

procedure TdmHighl.LoadUniColors(AConfig: TJSONObject);
var
  I: Integer;
  AName: String;
  AList: TJSONArray;
  AItem: TJSONObject;
  ARanges: TJSONArray;
  Highlighter: TSynCustomHighlighter;

  function LoadRule(AList: TJSONArray; SymbRule: TSynRule): TJSONObject;
  var
    J: Integer;
    AValue: String;
  begin
    for J:= 0 to AList.Count - 1 do
    begin
      Result:= AList.Objects[J];
      AName:= Result.Get('Name', EmptyStr);

      if SameStr(AName, SymbRule.Name) then
      begin
        AValue:= Result.Get('Attributes', EmptyStr);
        if Length(AValue) > 0 then
        begin
          SymbRule.Attribs.LoadFromString(AValue);
        end;
        Exit;
      end;
    end;
    Result:= nil;
  end;

  procedure LoadRange(ARanges: TJSONArray; ARange: TSynRange);
  var
    Index: Integer;
    AList: TJSONArray;
    ARule: TJSONObject;
  begin
    ARule:= LoadRule(ARanges, ARange);

    if (ARule = nil) then Exit;

    if (ARange.SetCount > 0) then
    begin
      AList:= ARule.Get('Sets', TJSONArray(nil));
      if Assigned(AList) and (AList.Count > 0) then
      begin
        for Index:= 0 to ARange.SetCount - 1 do
        begin
          LoadRule(AList, ARange.Sets[Index]);
        end;
      end;
    end;
    if (ARange.KeyListCount > 0) then
    begin
      AList:= ARule.Get('KeyLists', TJSONArray(nil));
      if Assigned(AList) and (AList.Count > 0) then
      begin
        for Index:= 0 to ARange.KeyListCount - 1 do
        begin
          LoadRule(AList, ARange.KeyLists[Index]);
        end;
      end;
    end;
    if (ARange.RangeCount > 0) then
    begin
      AList:= ARule.Get('Ranges', TJSONArray(nil));
      if Assigned(AList) and (AList.Count > 0) then
      begin
        for Index:= 0 to ARange.RangeCount - 1 do
        begin
          LoadRange(AList, ARange.Ranges[Index]);
        end;
      end;
    end;
  end;

begin
  if AConfig.Find('UniHighlighters', AList) then
  begin
    for I:= 0 to AList.Count - 1 do
    begin
      AItem:= AList.Objects[I];
      AName:= AItem.Get('Name', EmptyStr);
      Highlighter:= TSynCustomHighlighter(SynHighlighterHashList.Data[AName]);
      if Assigned(Highlighter) and (Highlighter is TSynUniSyn) then
      begin
        with TSynUniSyn(Highlighter) do
        begin
          ARanges:= AItem.Get('Ranges', TJSONArray(nil));
          if Assigned(ARanges) and (ARanges.Count > 0) then
          begin
            LoadRange(ARanges, MainRules);
          end;
        end;
      end;
    end;
  end;
end;

procedure TdmHighl.SaveUniColors(AConfig: TJSONObject);
var
  I: Integer;
  SynUniSyn: TSynUniSyn;

  procedure SaveRule(ARules: TJSONArray; SymbRule: TSynRule);
  var
    ARule: TJSONObject;
  begin
    ARule:= TJSONObject.Create;
    ARule.Add('Name', SymbRule.Name);
    ARule.Add('Attributes', SymbRule.Attribs.ToString);
    ARules.Add(ARule);
  end;

  procedure SaveRange(ARules: TJSONArray; ARange: TSynRange);
  var
    Index: Integer;
    ARule: TJSONObject;
    Sets, KeyLists, Ranges: TJSONArray;
  begin
    ARule:= TJSONObject.Create;

    ARule.Add('Name', ARange.Name);
    ARule.Add('Attributes', ARange.Attribs.ToString);

    if (ARange.SetCount > 0) then
    begin
      Sets:= TJSONArray.Create;
      for Index:= 0 to ARange.SetCount - 1 do
      begin
        SaveRule(Sets, ARange.Sets[Index]);
      end;
      ARule.Add('Sets', Sets);
    end;

    if (ARange.KeyListCount > 0) then
    begin
      KeyLists:= TJSONArray.Create;
      for Index:= 0 to ARange.KeyListCount - 1 do
      begin
        SaveRule(KeyLists, ARange.KeyLists[Index]);
      end;
      ARule.Add('KeyLists', KeyLists);
    end;

    if (ARange.RangeCount > 0) then
    begin
      Ranges:= TJSONArray.Create;
      for Index:= 0 to ARange.RangeCount - 1 do
      begin
        SaveRange(Ranges, ARange.Ranges[Index]);
      end;
      ARule.Add('Ranges', Ranges);
    end;

    ARules.Add(ARule);
  end;

  procedure SaveHighlighter(AList: TJSONArray);
  var
    AItem: TJSONObject;
    ARules: TJSONArray;
  begin
    AItem:= TJSONObject.Create;
    AItem.Add('Name', SynUniSyn.Info.General.Name);

    ARules:= TJSONArray.Create;
    SaveRange(ARules, SynUniSyn.MainRules);
    AItem.Add('Ranges', ARules);

    AList.Add(AItem);
  end;

var
  AList: TJSONArray;
begin
  if AConfig.Find('UniHighlighters', AList) then
    AList.Clear
  else begin
    AList:= TJSONArray.Create;
    AConfig.Add('UniHighlighters', AList);
  end;

  for I := 0 to SynHighlighterList.Count - 1 do
  begin
    if SynHighlighterList.Objects[I] is TSynUniSyn then
    begin
      SynUniSyn:= TSynUniSyn(SynHighlighterList.Objects[I]);
      //if SynUniSyn.Tag < 0 then
      begin
        SaveHighlighter(AList);
        SynUniSyn.Tag:= 0;
      end;
    end;
  end;
end;

procedure TdmHighl.LoadUniHighlighters;
var
  AName: String;
  I, Index: Integer;
  AList: TStringList;
  AFileName: String = '';
  ACache: TStringListEx;
  HighLighter: TSynCustomHighlighter;
begin
  ACache:= TStringListEx.Create;
  ACache.CaseSensitive:= FileNameCaseSensitive;
  if not gUseConfigInProgramDir then begin
    AFileName:= IncludeTrailingBackslash(GetAppDataDir) + 'highlighters' + ';';
  end;
  AList:= FindAllFiles(AFileName + gpHighPath, '*.hgl');
  for I:= 0 to AList.Count - 1 do
  begin
    AFileName:= ExtractFileName(AList[I]);
    if ACache.IndexOf(AFileName) < 0 then
    begin
      HighLighter:= TSynUniSyn.Create(Self);
      try
        TSynUniSyn(HighLighter).LoadFromFile(AList[I]);
        AName:= TSynUniSyn(HighLighter).Info.General.Name;
        Index:= SynHighlighterList.IndexOf(AName);
        if (Index < 0) then
          SynHighlighterList.AddObject(AName, Highlighter)
        else begin
          // Add duplicate external highlighter
          if SynHighlighterList.Objects[Index] is TSynUniSyn then
          begin
            AName:= AName + ' #' + IntToStr(I);
            SynHighlighterList.AddObject(AName, Highlighter);
          end
          // Replace built-in highlighter
          else begin
            SynHighlighterList.Objects[Index].Free;
            SynHighlighterList.Objects[Index]:= Highlighter;
          end;
        end;
        ACache.Add(AFileName);
      except
        FreeAndNil(HighLighter);
      end;
    end;
  end;
  AList.Free;
  ACache.Free;
end;

function TdmHighl.GetSyn(Index: Integer): TSynCustomHighlighter;
begin
  Result:= TSynCustomHighlighter(SynHighlighterList.Objects[Index]);
end;

function TdmHighl.GetSyn(AClass: TSynCustomHighlighterClass): TSynCustomHighlighter;
var
  Index: Integer;
begin
  for Index:= 0 to SynHighlighterList.Count - 1 do
  begin
    if SynHighlighterList.Objects[Index] is AClass then
      Exit(TSynCustomHighlighter(SynHighlighterList.Objects[Index]));
  end;
  Result:= nil;
end;

procedure TdmHighl.CreateHighlighters;
var
  I: Integer;
  Highlighter: TSynCustomHighlighter;
begin
  for I:= 0 to High(DEFAULT_HIGHLIGHTERS) do
  begin
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
    Highlighter:= DEFAULT_HIGHLIGHTERS[I].Create(Self);
    {$POP}
    Highlighter.Tag:= PtrInt(I <> 0);
    SynHighlighterList.AddObject(HighLighter.LanguageName, HighLighter);
  end;

  LoadUniHighlighters;

  for I:= 0 to SynHighlighterList.Count - 1 do
  begin
    HighLighter:= TSynCustomHighlighter(SynHighlighterList.Objects[I]);
    SynHighlighterHashList.Add(HighLighter.LanguageName, HighLighter);
    if not (Highlighter is TSynUniSyn) then
    begin
      with HighLighter.AddSpecialAttribute(rsSynDefaultText, SYNS_XML_DefaultText) do
      begin
        Background:= clWindow;
        Foreground:= clWindowText;
      end;
    end;
  end;
  SynHighlighterList.CustomSort(@SynHighlighterSortCompare);
end;

constructor TdmHighl.Create(AOwner: TComponent; ATemp: Boolean);
begin
  FTemp:= ATemp;
  SynHighlighterList:= TStringList.Create;
  SynHighlighterHashList:= TStringHashListUtf8.Create(True);
  if not FTemp then CreateHighlighters;
  inherited Create(AOwner);
end;

destructor TdmHighl.Destroy;
begin
  inherited Destroy;
  SynHighlighterList.Free;
  SynHighlighterHashList.Free;
end;

procedure TdmHighl.CopyHighlighter(SourceHighlighter, TargetHighlighter: TSynCustomHighlighter);
var
  J: Integer;
begin
  if SourceHighlighter is TSynUniSyn then
    TSynUniSyn(TargetHighlighter).Assign(TSynUniSyn(SourceHighlighter))
  else begin
    TargetHighlighter.Tag:= SourceHighlighter.Tag;
    TargetHighlighter.DefaultFilter:= SourceHighlighter.DefaultFilter;
    for J:= 0 to SourceHighlighter.AttrCount - 1 do
    begin
      TargetHighlighter.Attribute[J].Assign(SourceHighlighter.Attribute[J]);
    end;
  end;
end;

procedure TdmHighl.Assign(Source: TPersistent);
var
  I: Integer;
  Highl: TdmHighl absolute Source;
begin
  for I:= 0 to SynHighlighterList.Count - 1 do
  begin
    CopyHighlighter(TSynCustomHighlighter(Highl.SynHighlighterList.Objects[I]),
                    TSynCustomHighlighter(SynHighlighterList.Objects[I])
                   );
  end;
end;

function TdmHighl.Clone: TdmHighl;
var
  I: Integer;
  AClass: TSynCustomHighlighterClass;
  Highlighter: TSynCustomHighlighter;
  SourceHighlighter: TSynCustomHighlighter;
begin
  Result:= TdmHighl.Create(Application, True);

  for I:= 0 to SynHighlighterList.Count - 1 do
  begin
    SourceHighlighter:= TSynCustomHighlighter(SynHighlighterList.Objects[I]);
    AClass:= TSynCustomHighlighterClass(SourceHighlighter.ClassType);
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
    Highlighter:= AClass.Create(Result);
    {$POP}
    if not (Highlighter is TSynUniSyn) then
    begin
      with HighLighter.AddSpecialAttribute(rsSynDefaultText, SYNS_XML_DefaultText) do
      begin
        Background:= clWindow;
        Foreground:= clWindowText;
      end;
    end;
    CopyHighlighter(SourceHighlighter, Highlighter);
    Result.SynHighlighterHashList.Add(Highlighter.LanguageName, HighLighter);
    Result.SynHighlighterList.AddObject(Highlighter.LanguageName, Highlighter);
  end;
end;

procedure TdmHighl.CopyFrom(ASource: TdmHighl);
var
  I: Integer;
  TargetHighlighter,
  SourceHighlighter: TSynCustomHighlighter;
begin
  for I:= 0 to ASource.SynHighlighterList.Count - 1 do
  begin
    TargetHighlighter:= Self.Highlighters[I];
    SourceHighlighter:= ASource.Highlighters[I];

    TargetHighlighter.Tag:= SourceHighlighter.Tag;
    TargetHighlighter.DefaultFilter:= SourceHighlighter.DefaultFilter;
  end;
end;

function TdmHighl.GetHighlighter(SynEdit: TCustomSynEdit;
  const sExtension: string): TSynCustomHighlighter;
var
  Extension: String;
begin
  Result:= GetHighlighterFromFileExt(SynHighlighterList, sExtension);
  // Determine file type by content
  if (Result = nil) and (SynEdit.Lines.Count > 0) then
  begin
    Extension:= SynEdit.Lines[0];
    if StrBegins(Extension, '<?xml') then
      Result:= GetSyn(TSynXMLSyn)
    else if StrBegins(Extension, '#!') then
    begin
      // Unix shell script
      if (Pos('sh', Extension) > 0) then
        Result:= GetSyn(TSynUNIXShellScriptSynEx)
      // Python script
      else if (Pos('python', Extension) > 0) then
        Result:= GetSyn(TSynPythonSyn)
      // Perl script
      else if (Pos('perl', Extension) > 0) then
        Result:= GetSyn(TSynPerlSynEx);
    end;
  end;
  // Default syntax highlighter
  if (Result = nil) then Result:= SynPlainTextHighlighter;
end;

procedure TdmHighl.SetHighlighter(SynEdit: TCustomSynEdit; Highlighter: TSynCustomHighlighter);
var
  I: LongInt;
  Attribute: TSynHighlighterAttributes;
begin
  if (Highlighter is TSynPlainTextHighlighter) then
    SynEdit.Highlighter:= nil
  else begin
    SynEdit.Highlighter:= Highlighter;
  end;

  if Highlighter is TSynUniSyn then
    Attribute:= TSynUniSyn(Highlighter).MainRules.Attribs
  else begin
    I:= Highlighter.AttrCount - 1;
    repeat
      Attribute:= Highlighter.Attribute[I];
      Dec(I);
    until (I < 0) or SameText(Attribute.StoredName, SYNS_XML_DefaultText);
  end;

  SynEdit.Color:= Attribute.Background;
  SynEdit.Font.Color:= Attribute.Foreground;
end;

{ THighlighters }

constructor THighlighters.Create;
begin
  FStyle := TColorThemes.StyleIndex;

  FStyles[FStyle]:= TdmHighl.Create(Application, False);
  FStyles[Abs(FStyle - 1)]:= FStyles[FStyle].Clone;

  LoadDefaults;
end;

procedure THighlighters.UpdateStyle;
var
  ANewStyle: Integer;
begin
  ANewStyle := TColorThemes.StyleIndex;

  if FStyle <> ANewStyle then
  begin
    // Synchronize common options
    FStyles[ANewStyle].CopyFrom(FStyles[FStyle]);
    FStyle := ANewStyle;
  end;
end;

procedure THighlighters.LoadDefaults;
var
  ARoot: TJSONObject;
  AStream: TResourceStream;
begin
  AStream:= TResourceStream.Create(HInstance, 'HIGHLIGHTERS', RT_RCDATA);
  try
    ARoot:= GetJSON(AStream, True) as TJSONObject;
    try
      LoadColors(ARoot);
    finally
      ARoot.Free;
    end;
  finally
    AStream.Free;
  end;
  if mbFileExists(gpHighPath + 'highlighters.json') then
  try
    with TJsonConfig.Create do
    try
      LoadFromFile(gpHighPath + 'highlighters.json');
      LoadColors(Root);
    finally
      Free;
    end;
  except
   // Ignore
  end;
end;

function THighlighters.Current: TdmHighl;
begin
  Result:= FStyles[FStyle];
end;

procedure THighlighters.LoadColors(AConfig: TJSONObject);
var
  AName: String;
  Index: Integer;
  Theme: TJSONObject;
  Themes: TJSONArray;
begin
  Themes:= AConfig.Get('Styles', TJSONArray(nil));

  if Assigned(Themes) then
  begin
    for Index:= 0 to Themes.Count - 1 do
    begin
      Theme:= Themes.Objects[Index];
      AName:= Theme.Get('Name', EmptyStr);
      if (AName = LIGHT_THEME) then
      begin
        FStyles[0].LoadColors(Theme);
        FStyles[0].LoadUniColors(Theme);
      end
      else if (AName = DARK_THEME) then
      begin
        FStyles[1].LoadColors(Theme);
        FStyles[1].LoadUniColors(Theme);
      end
    end;
   end;
end;

procedure THighlighters.SaveColors(AConfig: TJSONObject);
var
  AName: String;
  Index: Integer;
  Theme: TJSONObject;
  Themes: TJSONArray;
begin
  if not AConfig.Find('Styles', Themes) then
  begin
    Themes:= TJSONArray.Create;
    AConfig.Add('Styles', Themes);
  end;

  if Assigned(Themes) then
  begin
    for Index:= 0 to Themes.Count - 1 do
    begin
      Theme:= Themes.Objects[Index];
      AName:= Theme.Get('Name', EmptyStr);
      if (AName = LIGHT_THEME) then
      begin
        FStyles[0].SaveColors(Theme);
        FStyles[0].SaveUniColors(Theme);
      end
      else if (AName = DARK_THEME) then
      begin
        FStyles[1].SaveColors(Theme);
        FStyles[1].SaveUniColors(Theme);
      end
    end;
  end;
end;

procedure THighlighters.Load(const FileName: String);
var
  J: LongInt;
  AVersion: Integer;
  Config: TXmlConfig;
  Highlighter: TSynCustomHighlighter;
  LanguageName, AttributeName: String;
  Attribute: TSynHighlighterAttributes;
  Root, FormNode, AttributeNode: TXmlNode;
begin
  Config := TXmlConfig.Create(FileName, True);
  try
    Root := Config.FindNode(Config.RootNode, 'Highlighters');
    if Assigned(Root) then
    begin
      AVersion := Config.GetAttr(Root, 'Version', ConfigVersion);
      FormNode := Config.FindNode(Root, 'Highlighter');
      if Assigned(FormNode) then
      begin
        while Assigned(FormNode) do
        begin
          LanguageName:= Config.GetAttr(FormNode, 'Name', EmptyStr);
          Highlighter:= TSynCustomHighlighter(Current.SynHighlighterHashList.Data[LanguageName]);
          if Assigned(Highlighter) then
          begin
            Highlighter.Tag := Config.GetAttr(FormNode, 'Tag', 1);
            Highlighter.DefaultFilter:= Config.GetValue(FormNode, 'DefaultFilter', Highlighter.DefaultFilter);
            // Import colors from old format
            if AVersion < 2 then
            begin
              AttributeNode := Config.FindNode(FormNode, 'Attribute');
              if Assigned(AttributeNode) then
              begin
                while Assigned(AttributeNode) do
                begin
                  AttributeName:= Config.GetAttr(AttributeNode, 'Name', EmptyStr);;
                  for J:= 0 to Highlighter.AttrCount - 1 do
                  begin
                    Attribute:= Highlighter.Attribute[J];
                    if SameText(Attribute.StoredName, AttributeName) or SameText(Attribute.Name, AttributeName) then
                    begin
                      Attribute.Style      := TFontStyles(Config.GetValue(AttributeNode, 'Style', Integer(Attribute.Style)));
                      Attribute.StyleMask  := TFontStyles(Config.GetValue(AttributeNode, 'StyleMask', Integer(Attribute.StyleMask)));
                      Attribute.Foreground := TColor(Config.GetValue(AttributeNode, 'Foreground', Integer(Attribute.Foreground)));
                      Attribute.Background := TColor(Config.GetValue(AttributeNode, 'Background', Integer(Attribute.Background)));
                      Attribute.FrameColor := TColor(Config.GetValue(AttributeNode, 'FrameColor', Integer(Attribute.FrameColor)));
                      Attribute.FrameStyle := TSynLineStyle(Config.GetValue(AttributeNode, 'FrameStyle', Integer(Attribute.FrameStyle)));
                      Attribute.FrameEdges := TSynFrameEdges(Config.GetValue(AttributeNode, 'FrameEdges', Integer(Attribute.FrameEdges)));
                      Break;
                    end;
                  end;
                  AttributeNode := AttributeNode.NextSibling;
                end;
              end;
            end;
          end;
          FormNode := FormNode.NextSibling;
        end;
      end;
    end;
  finally
    Config.Free;
  end;
end;

procedure THighlighters.Save(const FileName: String);
var
  I: LongInt;
  Config: TXmlConfig;
  Root, FormNode: TXmlNode;
  Highlighter: TSynCustomHighlighter;
begin
  Config := TXmlConfig.Create;
  try
    Config.FileName := FileName;
    Root := Config.FindNode(Config.RootNode, 'Highlighters', True);
    Config.ClearNode(Root);
    Config.SetAttr(Root, 'Version', ConfigVersion);
    with Current do
    begin
      for I := 0 to SynHighlighterList.Count - 1 do
      begin
        Highlighter := Highlighters[I];
        FormNode := Config.AddNode(Root, 'Highlighter');
        Config.SetAttr(FormNode, 'Tag', Highlighter.Tag);
        Config.SetAttr(FormNode, 'Name', Highlighter.LanguageName);
        Config.SetValue(FormNode, 'DefaultFilter', Highlighter.DefaultFilter);
      end;
    end;
    Config.Save;
  finally
    Config.Free;
  end;
end;

end.


unit dmHigh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, SynEdit, StringHashList, LCLVersion,
  SynEditHighlighter, SynHighlighterPas, SynHighlighterCPP, SynHighlighterJava,
  SynHighlighterHTML, SynHighlighterXML, SynHighlighterLFM,
  SynHighlighterUNIXShellScript, SynHighlighterPHP, SynHighlighterTeX,
  SynHighlighterSQL, SynHighlighterPerl, SynHighlighterCss,
  SynHighlighterPython, SynHighlighterDiff, SynHighlighterVB, SynHighlighterBat,
  SynHighlighterIni;


const
  HighlighterConfig = 'highlighters.xml';
  SYNS_XML_DefaultText = 'Default text';

type

  { TSynPlainTextHighlighter }

  TSynPlainTextHighlighter = class(TSynCustomHighlighter)
  public
    class function GetLanguageName: string; override;
  end;

  { TdmHighl }

  TdmHighl = class(TDataModule)
    SynBatSyn1: TSynBatSyn;
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynDiffSyn1: TSynDiffSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynIniSyn1: TSynIniSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynPasSyn1: TSynPasSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynTeXSyn1: TSynTeXSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynVBSyn1: TSynVBSyn;
    SynXMLSyn1: TSynXMLSyn;
    procedure dmHighlCreate(Sender: TObject);
    procedure dmHighlDestroy(Sender: TObject);
  private
    FTemp: Boolean;
    FChanged: Boolean;
    procedure ImportFromOldFormat;
  public
    SynHighlighterList: TStringList;
    SynHighlighterHashList: TStringHashList;
    SynPlainTextHighlighter: TSynPlainTextHighlighter;
    function GetSampleSource(Highlighter: TSynCustomHighlighter): string;
  public
    constructor Create(AOwner: TComponent; ATemp: Boolean); overload;
    procedure Assign(Source: TPersistent); override;
    function LoadFromFile(const FileName: UTF8String): Boolean;
    function SaveToFile(const FileName: UTF8String): Boolean;
    function GetHighlighterByExt(const sExtension: string): TSynCustomHighlighter;
    procedure SetHighlighter(SynEdit: TCustomSynEdit; Highlighter: TSynCustomHighlighter);
    property Changed: Boolean read FChanged write FChanged;
  end;

var
  dmHighl: TdmHighl;

implementation

{$R *.lfm}

uses
  Graphics, SynEditTypes, uHighlighterProcs, uXMLConfig, uGlobsPaths,
  uClassesEx, uOSUtils, uLng
{$IF lcl_fullversion >= 093100}
  , SynHighlighterPo
{$ENDIF}
  ;

const
  csDefaultName = 'editor.col';

function SynHighlighterSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if CompareStr(List[Index1], rsSynLangPlainText) = 0 then
    Result:= -1
  else if CompareStr(List[Index2], rsSynLangPlainText) = 0 then
    Result:=  1
  else
    Result:= CompareStr(List[Index1], List[Index2]);
end;

{ TSynPlainTextHighlighter }

class function TSynPlainTextHighlighter.GetLanguageName: string;
begin
  Result:= rsSynLangPlainText;
end;

{ TdmHighl }

procedure TdmHighl.dmHighlCreate(Sender: TObject);
var
  I: LongWord;
  HighLighter: TSynCustomHighlighter;
  Attribute: TSynHighlighterAttributes;
begin
  SynHighlighterList:= TStringList.Create;
  SynHighlighterHashList:= TStringHashList.Create(True);
  SynPlainTextHighlighter:= TSynPlainTextHighlighter.Create(Self);
{$IF lcl_fullversion >= 093100}
  TSynPoSyn.Create(Self).Tag:= 1; // Will be destroyed by owner
{$ENDIF}
  GetHighlighters(Self, SynHighlighterList, False);
  for I:= 0 to SynHighlighterList.Count - 1 do
  begin
    HighLighter:= TSynCustomHighlighter(SynHighlighterList.Objects[I]);
    SynHighlighterHashList.Add(HighLighter.LanguageName, HighLighter);
    with HighLighter.AddSpecialAttribute(rsSynDefaultText, SYNS_XML_DefaultText) do
    begin
      Features:= [hafBackColor, hafForeColor];
      Background:= clWindow;
      Foreground:= clWindowText;
    end;
  end;
  SynHighlighterList.CustomSort(@SynHighlighterSortCompare);
  if (FTemp = False) then
  begin
    if not mbFileExists(gpCfgDir + csDefaultName) then
      LoadFromFile(gpCfgDir + HighlighterConfig)
    else
      begin
        ImportFromOldFormat;
        SaveToFile(gpCfgDir + HighlighterConfig);
        mbRenameFile(gpCfgDir + csDefaultName, gpCfgDir + csDefaultName + '.obsolete');
      end;
  end;
end;

procedure TdmHighl.dmHighlDestroy(Sender: TObject);
begin
  if FChanged and (FTemp = False) then
    SaveToFile(gpCfgDir + HighlighterConfig);
  SynHighlighterList.Free;
  SynHighlighterHashList.Free;
  SynPlainTextHighlighter.Free;
end;

procedure TdmHighl.ImportFromOldFormat;
var
  I: Integer = 0;
  J, K: Integer;
  aFile: TStringListEx;
  s, sValue: UTF8String;
  Highlighter: TSynCustomHighlighter;
  Attribute: TSynHighlighterAttributes;
begin
  aFile:= TStringListEx.Create;
  try
    aFile.LoadFromFile(gpCfgDir + csDefaultName);
    while I < aFile.Count do
    begin;
      s:= Trim(aFile[I]);
      Inc(I, 1);
      if s = '' then Continue;
      if s[1] = '#' then Continue;
      if s[1] <> '[' then Continue;
      Inc(I, 3);
      sValue:= Copy(s, 2, Length(s) - 2);
      for J:= 0 to SynHighlighterList.Count - 1 do
      begin
        Highlighter:= TSynCustomHighlighter(SynHighlighterList.Objects[J]);
        for K:= 0 to Highlighter.AttrCount - 1 do
        begin
          Attribute:= Highlighter.Attribute[K];
          if SameText(sValue, Attribute.StoredName) then
          begin
            Attribute.Background := TColor(StrToIntDef(aFile.ValueFromIndex[I - 3], 0));
            Attribute.Foreground := TColor(StrToIntDef(aFile.ValueFromIndex[I - 2], 0));
            Attribute.Style      := TFontStyles(StrToIntDef(aFile.ValueFromIndex[I - 1], 0));
            Break;
          end;
        end;
      end;
    end;
  finally
    aFile.Free;
  end;
end;

function TdmHighl.GetSampleSource(Highlighter: TSynCustomHighlighter): string;
begin
  if (Highlighter  is TSynPlainTextHighlighter) then
    Result :=
      'Double Commander is a cross platform open source file manager'#13 +
      'with two panels side by side. It is inspired by Total Commander'#13 +
      'and features some new ideas.'#13
  else if (Highlighter  is TSynPasSyn) then
    Result :=
      '{ Comment }'#13 +
      '{$R- compiler directive}'#13 +
      'procedure TForm1.Button1Click(Sender: TObject);'#13 +
      'var  // Delphi Comment'#13 +
      '  Number, I, X: Integer;'#13 +
      'begin'#13 +
      '  Number := 12345 * (2 + 9) // << Matching Brackets ;'#13 +
      '  Caption := ''The number is '' + IntToStr(Number);'#13 +
      '  asm'#13 + '    MOV AX,1234h'#13 +
      '    MOV Number,AX'#13 +
      '  end;'#13 +
      '  case ModalResult of'#13+
      '    mrOK: inc(X);'#13+
      '    mrCancel, mrIgnore: dec(X);'#13+
      '  end;'#13+
      '  ListBox1.Items.Add(IntToStr(X));'#13 +
      'end;'#13 + #13
  else if (Highlighter  is TSynCppSyn) then
    Result :=
      '/* Comment */'#13 + '#include <stdio.h>'#13 +
      '#include <stdlib.h>'#13 + #13 +
      'static char line_buf[LINE_BUF];'#13 + #13 +
      'int main(int argc,char **argv){'#13 + '  FILE *file;'#13 +
      '  line_buf[0]=0;'#13 + '  printf("\n");'#13 +
      '  return 0;'#13 + '}'#13 + ''#13 + #13
  else if (Highlighter  is TSynDiffSyn) then
    Result :=
      '*** /a/file'#13#10 +
      '--- /b/file'#13#10 +
      '***************'#13#10 +
      '*** 2,5 ****'#13#10 +
      '--- 2,5 ----'#13#10 +
      '  context'#13#10 +
      '- removed'#13#10 +
      '! Changed'#13#10 +
      '+ added'#13#10 +
      '  context'#13#10
  else if (Highlighter  is TSynHTMLSyn) then
    Result :=
      '<html>'#13 + '<title>Lazarus Sample source for html</title>'#13 +
      '<body bgcolor=#ffffff background="bg.jpg">'#13 +
      '<!-- Comment -->'#13 + '<img src="lazarus.jpg">'#13 +
      '<p>'#13 + '  Some Text'#13 +
      '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'#13 + '</p>'#13 +
      '<invalid_tag>'#13 + '<!-- Text Block -->'#13 +
      '</body>'#13 + '</html>'#13 + #13
  else if (Highlighter  is TSynPerlSyn) then
    Result :=
      '#!/usr/bin/perl'#13 + '# Perl sample code'#13 +
      ''#13 + '$i = "10";'#13 + 'print "$ENV{PATH}\n";'#13 +
      '($i =~ /\d+/) || die "Error\n";'#13 + ''#13 +
      '# Text Block'#13 + ''#13 + #13
  else if (Highlighter  is TSynUNIXShellScriptSyn) then
    Result :=
      '#!/bin/bash'#13#13 +
      '# Bash syntax highlighting'#13#10 + 'set -x'#13#10 +
      'set -e'#13#10 +
      'Usage="Usage: $0 devel|stable"'#13#10 +
      'FPCVersion=$1'#13#10 +
      'for ver in devel stable; do'#13#10 +
      '  if [ "x$FPCVersion" = "x$ver" ]; then'#13#10 +
      '  fi'#13#10 + 'done'#13#10 +
      '# Text Block'#13#10 + #13#10
  else
    Result:= EmptyStr;
end;

constructor TdmHighl.Create(AOwner: TComponent; ATemp: Boolean);
begin
  inherited Create(AOwner);
  FTemp:= ATemp;
end;

procedure TdmHighl.Assign(Source: TPersistent);
var
  I: LongWord;
  Highl: TdmHighl absolute Source;
  Highlighter: TSynCustomHighlighter;

  procedure CopyAttributes(SourceHighlighter, TargetHighlighter: TSynCustomHighlighter);
  var
    J: LongWord;
  begin
    TargetHighlighter.Tag:= SourceHighlighter.Tag;
    for J:= 0 to SourceHighlighter.AttrCount - 1 do
    begin
      TargetHighlighter.Attribute[J].Background:= SourceHighlighter.Attribute[J].Background;
      TargetHighlighter.Attribute[J].Foreground:= SourceHighlighter.Attribute[J].Foreground;
      TargetHighlighter.Attribute[J].FrameColor:= SourceHighlighter.Attribute[J].FrameColor;
      TargetHighlighter.Attribute[J].FrameStyle:= SourceHighlighter.Attribute[J].FrameStyle;
      TargetHighlighter.Attribute[J].FrameEdges:= SourceHighlighter.Attribute[J].FrameEdges;
      TargetHighlighter.Attribute[J].Style     := SourceHighlighter.Attribute[J].Style;
      TargetHighlighter.Attribute[J].StyleMask := SourceHighlighter.Attribute[J].StyleMask;
    end;
  end;

begin
  FChanged:= True;
  for I:= 0 to SynHighlighterList.Count - 1 do
  begin
    CopyAttributes(TSynCustomHighlighter(Highl.SynHighlighterList.Objects[I]),
                   TSynCustomHighlighter(SynHighlighterList.Objects[I])
                  );
  end;
end;

function TdmHighl.LoadFromFile(const FileName: UTF8String): Boolean;
var
  I, J: LongInt;
  Config: TXmlConfig = nil;
  Root, FormNode, AttributeNode: TXmlNode;
  Highlighter: TSynCustomHighlighter;
  Attribute: TSynHighlighterAttributes;
  LanguageName,
  AttributeName  : String;
begin
  try
    Result:= True;
    try
      Config:= TXmlConfig.Create(FileName);
      Root := Config.FindNode(Config.RootNode, 'Highlighters');
      if Assigned(Root) then
      begin
        FormNode := Config.FindNode(Root, 'Highlighter');
        if Assigned(FormNode) then
        begin
          FormNode:= FormNode.FirstChild;
          while Assigned(FormNode) do
          begin
            LanguageName:= Config.GetAttr(FormNode, 'Name', EmptyStr);
            Highlighter:= TSynCustomHighlighter(SynHighlighterHashList.Data[LanguageName]);
            if Assigned(Highlighter) then
            begin
              Highlighter.Tag := Config.GetAttr(FormNode, 'Tag', 1);
              AttributeNode := Config.FindNode(FormNode, 'Attribute');
              if Assigned(AttributeNode) then
              begin
                AttributeNode := AttributeNode.FirstChild;
                while Assigned(AttributeNode) do
                begin
                  AttributeName:= Config.GetAttr(AttributeNode, 'Name', EmptyStr);;
                  for J:= 0 to Highlighter.AttrCount - 1 do
                  begin
                    Attribute:= Highlighter.Attribute[J];
                    if SameText(Attribute.StoredName, AttributeName) then
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
            FormNode := FormNode.NextSibling;
          end;
        end;
      end;
    except
      Result:= False;
    end;
  finally
    Config.Free;
  end;
end;

function TdmHighl.SaveToFile(const FileName: UTF8String): Boolean;
var
  I: LongInt;
  Config: TXmlConfig;
  Root, FormNode, AttributeNode: TXmlNode;
  Attribute: TSynHighlighterAttributes;

  procedure SaveHighlighter(Highlighter: TSynCustomHighlighter);
  var
    J: LongWord;
  begin
    FormNode := Config.AddNode(Root, 'Highlighter');
    Config.SetAttr(FormNode, 'Tag', Highlighter.Tag);
    Config.SetAttr(FormNode, 'Name', Highlighter.LanguageName);
    for J:= 0 to Highlighter.AttrCount - 1 do
    begin
      Attribute:= Highlighter.Attribute[J];
      AttributeNode := Config.AddNode(FormNode, 'Attribute');
      Config.SetAttr(AttributeNode, 'Name', Attribute.StoredName);
      Config.SetValue(AttributeNode, 'Style', Integer(Attribute.Style));
      Config.SetValue(AttributeNode, 'StyleMask', Integer(Attribute.StyleMask));
      Config.SetValue(AttributeNode, 'Foreground', Integer(Attribute.Foreground));
      Config.SetValue(AttributeNode, 'Background', Integer(Attribute.Background));
      Config.SetValue(AttributeNode, 'FrameColor', Integer(Attribute.FrameColor));
      Config.SetValue(AttributeNode, 'FrameStyle', Integer(Attribute.FrameStyle));
      Config.SetValue(AttributeNode, 'FrameEdges', Integer(Attribute.FrameEdges));
    end;
  end;

begin
  Result:= True;
  Config := TXmlConfig.Create;
  try
    Config.FileName := FileName;
    Root := Config.FindNode(Config.RootNode, 'Highlighters', True);
    Config.ClearNode(Root);
    Config.SetAttr(Root, 'Version', 1);
    try
      for I := 0 to SynHighlighterList.Count - 1 do
      begin
        SaveHighlighter(TSynCustomHighlighter(SynHighlighterList.Objects[I]));
      end;
      Config.Save;
    except
      Result:= False;
    end;
  finally
    Config.Free;
  end;
end;

function TdmHighl.GetHighlighterByExt(const sExtension: string): TSynCustomHighlighter;
begin
  Result:=GetHighlighterFromFileExt(SynHighlighterList, sExtension);
end;

procedure TdmHighl.SetHighlighter(SynEdit: TCustomSynEdit; Highlighter: TSynCustomHighlighter);
var
  I: LongInt;
  Attribute: TSynHighlighterAttributes;
begin
  if (Highlighter = nil) then Exit;
  if (Highlighter is TSynPlainTextHighlighter) then
    SynEdit.Highlighter:= nil
  else
    SynEdit.Highlighter:= Highlighter;
  I:= Highlighter.AttrCount - 1;
  repeat
    Attribute:= Highlighter.Attribute[I];
    Dec(I);
  until (I < 0) or SameText(Attribute.StoredName, SYNS_XML_DefaultText);
  SynEdit.Color:= Attribute.Background;
  SynEdit.Font.Color:= Attribute.Foreground;
end;

end.


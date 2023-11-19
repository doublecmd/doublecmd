unit uHighlighters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterPas, SynHighlighterCPP,
  SynHighlighterHTML, SynHighlighterUNIXShellScript, SynHighlighterPerl,
  SynHighlighterDiff, SynHighlighterPo, SynHighlighterIni, SynHighlighterBat,
  SynHighlighterTeX;

const
  SYNS_XML_DefaultText = 'Default text';

type

  { TSynBatSynEx }

  TSynBatSynEx = class(TSynBatSyn)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynCppSynEx }

  TSynCppSynEx = class(TSynCppSyn)
  protected
    function GetSampleSource: string; override;
  end;

  { TSynDiffSynEx }

  TSynDiffSynEx = class(TSynDiffSyn)
  protected
    function GetSampleSource: string; override;
    function GetDefaultFilter: string; override;
  end;

  { TSynHTMLSynEx }

  TSynHTMLSynEx = class(TSynHTMLSyn)
  protected
    function GetSampleSource: string; override;
  end;

  { TSynIniSynEx }

  TSynIniSynEx = class(TSynIniSyn)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynPasSynEx }

  TSynPasSynEx = class(TSynPasSyn)
  protected
    function GetSampleSource: string; override;
    function GetDefaultFilter: string; override;
  end;

  { TSynPerlSynEx }

  TSynPerlSynEx = class(TSynPerlSyn)
  protected
    function GetSampleSource: string; override;
  end;

  { TSynPoSynEx }

  TSynPoSynEx = class(TSynPoSyn)
  protected
    function GetDefaultFilter: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynTeXSynEx }

  TSynTeXSynEx = class(TSynTeXSyn)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynUNIXShellScriptSynEx }

  TSynUNIXShellScriptSynEx = class(TSynUNIXShellScriptSyn)
  protected
    function GetSampleSource: string; override;
  end;

  { TSynPlainTextHighlighter }

  TSynPlainTextHighlighter = class(TSynCustomHighlighter)
  protected
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  end;

  { TSynCustomHighlighterHelper }

  TSynCustomHighlighterHelper = class helper for TSynCustomHighlighter
  public
    function LanguageName: String;
    function Other: Boolean;
  end;

  TSynHighlighterAttrFeature =
    ( hafBackColor, hafForeColor, hafFrameColor,
      hafStyle, hafStyleMask,
      hafFrameStyle, hafFrameEdges
    );
  TSynHighlighterAttrFeatures = set of TSynHighlighterAttrFeature;

  { TSynHighlighterAttributesHelper }

  TSynHighlighterAttributesHelper = class helper for TSynHighlighterAttributes
  private
    function GetFeatures: TSynHighlighterAttrFeatures;
  public
    property Features: TSynHighlighterAttrFeatures read GetFeatures;
  end;

implementation

uses
  SynEditStrConst, SynUniHighlighter, SynUniClasses, uLng;

{ TSynBatSynEx }

constructor TSynBatSynEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommentAttri.StoredName    := SYNS_XML_AttrComment;
  IdentifierAttri.StoredName := SYNS_XML_AttrIdentifier;
  KeyAttri.StoredName        := SYNS_XML_AttrKey;
  NumberAttri.StoredName     := SYNS_XML_AttrNumber;
  SpaceAttri.StoredName      := SYNS_XML_AttrSpace;
  VariableAttri.StoredName   := SYNS_XML_AttrVariable;
end;

{ TSynCppSynEx }

function TSynCppSynEx.GetSampleSource: string;
begin
  Result :=
    '/* Comment */'#13 + '#include <stdio.h>'#13 +
    '#include <stdlib.h>'#13 + #13 +
    'static char line_buf[LINE_BUF];'#13 + #13 +
    'int main(int argc,char **argv){'#13 + '  FILE *file;'#13 +
    '  line_buf[0]=0;'#13 + '  printf("\n");'#13 +
    '  return 0;'#13 + '}'#13 + ''#13 + #13;
end;

{ TSynDiffSynEx }

function TSynDiffSynEx.GetSampleSource: string;
begin
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
    '  context'#13#10;
end;

function TSynDiffSynEx.GetDefaultFilter: string;
begin
  Result:= 'Difference Files (*.diff,*.patch)|*.diff;*.patch';
end;

{ TSynHTMLSynEx }

function TSynHTMLSynEx.GetSampleSource: string;
begin
  Result :=
    '<html>'#13 + '<title>Lazarus Sample source for html</title>'#13 +
    '<body bgcolor=#ffffff background="bg.jpg">'#13 +
    '<!-- Comment -->'#13 + '<img src="lazarus.jpg">'#13 +
    '<p>'#13 + '  Some Text'#13 +
    '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'#13 + '</p>'#13 +
    '<invalid_tag>'#13 + '<!-- Text Block -->'#13 +
    '</body>'#13 + '</html>'#13 + #13;
end;

{ TSynIniSynEx }

constructor TSynIniSynEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommentAttri.StoredName := SYNS_XML_AttrComment;
  TextAttri.StoredName    := SYNS_XML_AttrText;
  SectionAttri.StoredName := SYNS_XML_AttrSection;
  KeyAttri.StoredName     := SYNS_XML_AttrKey;
  NumberAttri.StoredName  := SYNS_XML_AttrNumber;
  SpaceAttri.StoredName   := SYNS_XML_AttrSpace;
  StringAttri.StoredName  := SYNS_XML_AttrString;
  SymbolAttri.StoredName  := SYNS_XML_AttrSymbol;
end;

{ TSynPasSynEx }

function TSynPasSynEx.GetSampleSource: string;
begin
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
    'end;'#13 + #13;
end;

function TSynPasSynEx.GetDefaultFilter: string;
begin
  Result:= 'Pascal Files (*.pas,*.dpr,*.dpk,*.inc,*.pp,*.lpr)|*.pas;*.dpr;*.dpk;*.inc;*.pp;*.lpr';
end;

{ TSynPerlSynEx }

function TSynPerlSynEx.GetSampleSource: string;
begin
  Result :=
    '#!/usr/bin/perl'#13 + '# Perl sample code'#13 +
    ''#13 + '$i = "10";'#13 + 'print "$ENV{PATH}\n";'#13 +
    '($i =~ /\d+/) || die "Error\n";'#13 + ''#13 +
    '# Text Block'#13 + ''#13 + #13;
end;

{ TSynPoSynEx }

function TSynPoSynEx.GetDefaultFilter: string;
begin
  Result:= 'Po Files (*.po,*.pot)|*.po;*.pot';
end;

constructor TSynPoSynEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommentAttri.StoredName := SYNS_XML_AttrComment;
  TextAttri.StoredName    := SYNS_XML_AttrText;
  KeyAttri.StoredName     := SYNS_XML_AttrKey;
end;

{ TSynTeXSynEx }

constructor TSynTeXSynEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommentAttri.StoredName         := SYNS_XML_AttrComment;
  TextAttri.StoredName            := SYNS_XML_AttrText;
  MathmodeAttri.StoredName        := SYNS_XML_AttrMathmode;
  SpaceAttri.StoredName           := SYNS_XML_AttrSpace;
  ControlSequenceAttri.StoredName := SYNS_XML_AttrTexCommand;
  BracketAttri.StoredName         := SYNS_XML_AttrSquareBracket;
  BraceAttri.StoredName           := SYNS_XML_AttrRoundBracket;
end;

{ TSynUNIXShellScriptSynEx }

function TSynUNIXShellScriptSynEx.GetSampleSource: string;
begin
  Result :=
    '#!/bin/bash'#13#13 +
    '# Bash syntax highlighting'#13#10 + 'set -x'#13#10 +
    'set -e'#13#10 +
    'Usage="Usage: $0 devel|stable"'#13#10 +
    'FPCVersion=$1'#13#10 +
    'for ver in devel stable; do'#13#10 +
    '  if [ "x$FPCVersion" = "x$ver" ]; then'#13#10 +
    '  fi'#13#10 + 'done'#13#10 +
    '# Text Block'#13#10 + #13#10;
end;

{ TSynPlainTextHighlighter }

function TSynPlainTextHighlighter.GetSampleSource: string;
begin
  Result :=
    'Double Commander is a cross platform open source file manager'#13 +
    'with two panels side by side. It is inspired by Total Commander'#13 +
    'and features some new ideas.'#13;
end;

class function TSynPlainTextHighlighter.GetLanguageName: string;
begin
  Result:= rsSynLangPlainText;
end;

{ TSynCustomHighlighterHelper }

function TSynCustomHighlighterHelper.LanguageName: String;
begin
  if Self is TSynUniSyn then
    Result:= TSynUniSyn(Self).Info.General.Name
  else
    Result:= Self.GetLanguageName;
end;

function TSynCustomHighlighterHelper.Other: Boolean;
begin
  if Self is TSynUniSyn then
    Result:= TSynUniSyn(Self).Info.General.Other
  else
    Result:= False;
end;

{ TSynHighlighterAttributesHelper }

function TSynHighlighterAttributesHelper.GetFeatures: TSynHighlighterAttrFeatures;
begin
  if SameText(StoredName, SYNS_XML_DefaultText) then
    Result:= [hafBackColor, hafForeColor]
  else begin
    if Self is TSynAttributes then
      Result:= [hafBackColor, hafForeColor, hafStyle]
    else
      Result:= [hafBackColor, hafForeColor, hafFrameColor, hafStyle, hafFrameStyle, hafFrameEdges];
  end;
end;

end.


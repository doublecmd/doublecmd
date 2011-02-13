unit dmHigh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, SynEditHighlighter,
  SynHighlighterPas, SynHighlighterCPP, SynHighlighterJava, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterLFM, SynHighlighterUNIXShellScript,
  SynHighlighterPHP, SynHighlighterTeX, SynHighlighterSQL,
  SynHighlighterPerl, SynHighlighterCss, SynHighlighterPython,
  SynHighlighterDiff, SynHighlighterVB, SynHighlighterBat, SynHighlighterIni;

type

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
    slHighLighters:TStringList;
  public
    { public declarations }
    function GetHighlighterByExt(const sExtension: string): TSynCustomHighlighter;
  end;

var
  dmHighl: TdmHighl;

implementation

{$R *.lfm}

uses
  uHighlighterProcs;

{ TdmHighl }

procedure TdmHighl.dmHighlCreate(Sender: TObject);
begin
  slHighLighters:=TStringList.Create;
  GetHighlighters(self,slHighLighters, False);
end;

procedure TdmHighl.dmHighlDestroy(Sender: TObject);
begin
  if assigned(slHighLighters) then
    FreeAndNil(slHighLighters);
end;

function TdmHighl.GetHighlighterByExt(const sExtension: string): TSynCustomHighlighter;
begin
  Result:=GetHighlighterFromFileExt(slHighLighters, sExtension);
end;

end.


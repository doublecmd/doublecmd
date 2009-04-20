unit dmHigh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs,
  SynEditHighlighter, SynHighlighterPas,
  SynHighlighterCPP, SynHighlighterJava, SynHighlighterHTML, SynHighlighterXML,
  SynHighlighterLFM, SynHighlighterUNIXShellScript, SynHighlighterPHP,
  SynHighlighterTeX, SynHighlighterSQL, SynHighlighterMulti, SynHighlighterPerl,
  SynHighlighterCss;

type
  TdmHighl = class(TDataModule)
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynLFMSyn1: TSynLFMSyn;
    SynMultiSyn1: TSynMultiSyn;
    SynPasSyn1: TSynPasSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynTeXSyn1: TSynTeXSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
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

initialization
  {$I dmhigh.lrs}

end.


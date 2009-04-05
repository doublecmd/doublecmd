unit uSearchTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, udsxplugin, uClassesEx;

type
  TSearchTemplate = class
  private
    FTemplateName: UTF8String;
    FSearchAttr: TSearchAttrRecord;
  public
    property SearchRecord: TSearchAttrRecord read FSearchAttr write FSearchAttr;
    property TemplateName: UTF8String read FTemplateName write FTemplateName;
  end;

  { TSearchTemplateList }

  TSearchTemplateList = class(TList)
  private
    function GetTemplate(Index: Integer): TSearchTemplate;
  public
    function Add(SearchTemplate: TSearchTemplate): Integer;
    procedure Delete(Index: Integer);
    procedure LoadFromIni(IniFile: TIniFileEx);
    procedure SaveToIni(IniFile: TIniFileEx);
    property Templates[Index: Integer]: TSearchTemplate read GetTemplate;
  end;

implementation

{ TSearchTemplateList }

function TSearchTemplateList.GetTemplate(Index: Integer): TSearchTemplate;
begin
  Result:= TSearchTemplate(Items[Index]);
end;

function TSearchTemplateList.Add(SearchTemplate: TSearchTemplate): Integer;
begin
  Result:= inherited Add(SearchTemplate);
end;

procedure TSearchTemplateList.Delete(Index: Integer);
begin
  Templates[Index].Free;
  Delete(Index);
end;

procedure TSearchTemplateList.LoadFromIni(IniFile: TIniFileEx);
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
    with Templates[I].SearchRecord do
    begin
      IniFile.WriteString('SearchTemplates', 'rFileMask', rFileMask);
    end;
end;

procedure TSearchTemplateList.SaveToIni(IniFile: TIniFileEx);
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
    with Templates[I].SearchRecord do
    begin
      IniFile.WriteString('SearchTemplates', 'rFileMask', rFileMask);
    end;
end;

end.


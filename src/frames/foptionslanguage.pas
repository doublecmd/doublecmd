unit fOptionsLanguage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsLanguage }

  TfrmOptionsLanguage = class(TOptionsEditor)
    lngList: TListBox;
  private
    procedure FillLngListBox;
  protected
    procedure Init; override;
    procedure Done; override;
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end;

implementation

{$R *.lfm}

uses
  uDebug, uFindEx, uTypes, uGlobs, uGlobsPaths, uLng;

{ TfrmOptionsLanguage }

procedure TfrmOptionsLanguage.FillLngListBox;
var
  fr: TSearchRecEx;
  iIndex: Integer;
  sLangName: String;
begin
  lngList.Clear;
  DCDebug('Language dir: ' + gpLngDir);
  if FindFirstEx(gpLngDir+'*.po', faAnyFile, fr)<>0 then
  begin
    FindCloseEx(fr);
    Exit;
  end;
  repeat
    sLangName := GetLanguageName(gpLngDir + fr.Name);
    lngList.Items.Add(Format('%s = (%s)', [fr.Name, sLangName]));
  until FindNextEx(fr)<>0;

  FindCloseEx(fr);

  iIndex:=lngList.Items.IndexOfName(gPOFileName + #32);
  if iIndex>=0 then
    lngList.Selected[iIndex]:=True;
end;

procedure TfrmOptionsLanguage.Init;
begin
end;

procedure TfrmOptionsLanguage.Done;
begin
end;

procedure TfrmOptionsLanguage.Load;
begin
  FillLngListBox;
end;

function TfrmOptionsLanguage.Save: TOptionsEditorSaveFlags;
var
  SelectedPOFileName: String;
begin
  Result := [];
  if lngList.ItemIndex > -1 then
  begin
    SelectedPOFileName := Trim(lngList.Items.Names[lngList.ItemIndex]);
    if SelectedPOFileName <> gPOFileName then
      Include(Result, oesfNeedsRestart);
    gPOFileName := SelectedPOFileName;
  end;
end;

initialization
  RegisterOptionsEditor(optedLanguage, TfrmOptionsLanguage);

end.


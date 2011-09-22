{
   Double Commander
   -------------------------------------------------------------------------
   Language options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

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
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
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

class function TfrmOptionsLanguage.GetIconIndex: Integer;
begin
  Result := 0;
end;

class function TfrmOptionsLanguage.GetTitle: String;
begin
  Result := rsOptionsEditorLanguage;
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

end.


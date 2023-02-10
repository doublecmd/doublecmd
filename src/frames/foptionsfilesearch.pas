{
   Double Commander
   -------------------------------------------------------------------------
   File search options page

   Copyright (C) 2006-2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit fOptionsFileSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Spin, ExtCtrls, KASComboBox, DividerBevel,
  fOptionsFrame;

type

  { TfrmOptionsFileSearch }

  TfrmOptionsFileSearch = class(TOptionsEditor)
    cbInitiallyClearFileMask: TCheckBox;
    cbNewSearchFilters: TComboBoxAutoWidth;
    cbShowMenuBarInFindFiles: TCheckBox;
    cbPartialNameSearch: TCheckBox;
    cbSearchDefaultTemplate: TComboBoxAutoWidth;
    dbTextSearch: TDividerBevel;
    gbFileSearch: TGroupBox;
    lblNewSearchFilters: TLabel;
    lblSearchDefaultTemplate: TLabel;
    rbUseMmapInSearch: TRadioButton;
    rbUseStreamInSearch: TRadioButton;
  private
    FLoading: Boolean;
    procedure FillTemplatesList(ListItems: TStrings);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng;

{ TfrmOptionsFileSearch }

class function TfrmOptionsFileSearch.GetIconIndex: Integer;
begin
  Result := 41;
end;

class function TfrmOptionsFileSearch.GetTitle: String;
begin
  Result := rsOptionsEditorFileSearch;
end;

procedure TfrmOptionsFileSearch.Init;
begin
  FillTemplatesList(cbSearchDefaultTemplate.Items);
  ParseLineToList(rsNewSearchClearFilterOptions, cbNewSearchFilters.Items);
end;

procedure TfrmOptionsFileSearch.FillTemplatesList(ListItems: TStrings);
begin
  gSearchTemplateList.LoadToStringList(ListItems);
  ListItems.Insert(0, rsOptHotkeysNoHotkey);
end;

procedure TfrmOptionsFileSearch.Load;
begin
  FLoading := True;

  rbUseMmapInSearch.Checked        := gUseMmapInSearch;
  cbPartialNameSearch.Checked      := gPartialNameSearch;
  cbInitiallyClearFileMask.Checked := gInitiallyClearFileMask;
  cbNewSearchFilters.ItemIndex     := integer(gNewSearchClearFiltersAction);
  cbShowMenuBarInFindFiles.Checked := gShowMenuBarInFindFiles;

  cbSearchDefaultTemplate.ItemIndex   := cbSearchDefaultTemplate.Items.IndexOf(gSearchDefaultTemplate);
  if cbSearchDefaultTemplate.ItemIndex < 0 then cbSearchDefaultTemplate.ItemIndex := 0;

  FLoading := False;
end;

function TfrmOptionsFileSearch.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gUseMmapInSearch        := rbUseMmapInSearch.Checked;
  gPartialNameSearch      := cbPartialNameSearch.Checked;
  gInitiallyClearFileMask := cbInitiallyClearFileMask.Checked;
  gNewSearchClearFiltersAction := TFiltersOnNewSearch(cbNewSearchFilters.ItemIndex);
  gShowMenuBarInFindFiles := cbShowMenuBarInFindFiles.Checked;

  if cbSearchDefaultTemplate.ItemIndex > 0 then
    gSearchDefaultTemplate:= cbSearchDefaultTemplate.Text
  else begin
    gSearchDefaultTemplate:= EmptyStr;
  end;
end;

constructor TfrmOptionsFileSearch.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLoading := False;
end;

end.


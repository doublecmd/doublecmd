{
   Double Commander
   -------------------------------------------------------------------------
   Quick search/filter options page

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

unit fOptionsQuickSearchFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, fOptionsFrame;

type

  { TfrmOptionsQuickSearchFilter }

  TfrmOptionsQuickSearchFilter = class(TOptionsEditor)
    cbExactBeginning: TCheckBox;
    cbExactEnding: TCheckBox;
    cgpOptions: TCheckGroup;
    gbExactNameMatch: TGroupBox;
    rgpSearchCase: TRadioGroup;
    rgpSearchItems: TRadioGroup;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng, fQuickSearch;

const
  OPTION_AUTOHIDE_POSITION = 0;

{ TfrmOptionsQuickSearchFilter }

class function TfrmOptionsQuickSearchFilter.GetIconIndex: Integer;
begin
  Result := 12;
end;

class function TfrmOptionsQuickSearchFilter.GetTitle: String;
begin
  Result := rsOptionsEditorQuickSearch;
end;

procedure TfrmOptionsQuickSearchFilter.Init;
begin
  // Copy localized strings to each combo box.
  ParseLineToList(rsOptSearchItems, rgpSearchItems.Items);
  ParseLineToList(rsOptSearchCase, rgpSearchCase.Items);
  ParseLineToList(rsOptSearchOpt, cgpOptions.Items);
end;

procedure TfrmOptionsQuickSearchFilter.Load;
begin
  cbExactBeginning.Checked := qsmBeginning in gQuickSearchOptions.Match;
  cbExactEnding.Checked := qsmEnding in gQuickSearchOptions.Match;
  rgpSearchItems.ItemIndex := Integer(gQuickSearchOptions.Items);
  rgpSearchCase.ItemIndex := Integer(gQuickSearchOptions.SearchCase);
  cgpOptions.Checked[OPTION_AUTOHIDE_POSITION] := gQuickFilterAutoHide;
end;

function TfrmOptionsQuickSearchFilter.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  if cbExactBeginning.Checked then
    Include(gQuickSearchOptions.Match, qsmBeginning)
  else
    Exclude(gQuickSearchOptions.Match, qsmBeginning);

  if cbExactEnding.Checked then
    Include(gQuickSearchOptions.Match, qsmEnding)
  else
    Exclude(gQuickSearchOptions.Match, qsmEnding);

  gQuickSearchOptions.Items := TQuickSearchItems(rgpSearchItems.ItemIndex);
  gQuickSearchOptions.SearchCase := TQuickSearchCase(rgpSearchCase.ItemIndex);
  gQuickFilterAutoHide := cgpOptions.Checked[OPTION_AUTOHIDE_POSITION];
end;

end.


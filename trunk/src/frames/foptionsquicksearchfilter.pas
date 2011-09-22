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
    grpQuickSearchFilterKeys: TGroupBox;
    lblQuickFilter: TLabel;
    lblQuickSearch: TLabel;
    pnlQuickFilter: TPanel;
    pnlQuickSearch: TPanel;
    rgpSearchCase: TRadioGroup;
    rgpSearchItems: TRadioGroup;
    rbAltLetterQF: TRadioButton;
    rbAltLetterQS: TRadioButton;
    rbCtrlAltLetterQF: TRadioButton;
    rbCtrlAltLetterQS: TRadioButton;
    rbLetterQF: TRadioButton;
    rbLetterQS: TRadioButton;
    rbNoneQF: TRadioButton;
    rbNoneQS: TRadioButton;
    procedure rbQuickSearchFilterKeyChange(Sender: TObject);
  protected
    procedure Init; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  uGlobs, uLng, fQuickSearch;

const
  OPTION_AUTOHIDE_POSITION = 0;

{ TfrmOptionsQuickSearchFilter }

procedure TfrmOptionsQuickSearchFilter.rbQuickSearchFilterKeyChange(Sender: TObject);
begin
  rbCtrlAltLetterQF.Enabled := not rbCtrlAltLetterQS.Checked;
  rbAltLetterQF.Enabled     := not rbAltLetterQS.Checked;
  rbLetterQF.Enabled        := not rbLetterQS.Checked;
  rbCtrlAltLetterQS.Enabled := not rbCtrlAltLetterQF.Checked;
  rbAltLetterQS.Enabled     := not rbAltLetterQF.Checked;
  rbLetterQS.Enabled        := not rbLetterQF.Checked;
end;

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
  // Set QuickFilter radio buttons captions same as QuickSearch.
  rbCtrlAltLetterQF.Caption := rbCtrlAltLetterQS.Caption;
  rbAltLetterQF.Caption     := rbAltLetterQS.Caption;
  rbLetterQF.Caption        := rbLetterQS.Caption;
  rbNoneQF.Caption          := rbNoneQS.Caption;
end;

procedure TfrmOptionsQuickSearchFilter.Load;
begin
  if gQuickSearch then
    begin
      if (gQuickSearchMode = [ssCtrl, ssAlt]) then
        rbCtrlAltLetterQS.Checked := True
      else if (gQuickSearchMode = [ssAlt]) then
        rbAltLetterQS.Checked := True
      else if gQuickSearchMode = [] then
        rbLetterQS.Checked := True;
    end
  else
    rbNoneQS.Checked := True;

  if gQuickFilter then
    begin
      if (gQuickFilterMode = [ssCtrl, ssAlt]) then
        rbCtrlAltLetterQF.Checked := True
      else if (gQuickFilterMode = [ssAlt]) then
        rbAltLetterQF.Checked := True
      else if gQuickFilterMode = [] then
        rbLetterQF.Checked := True;
    end
  else
    rbNoneQF.Checked := True;

  cbExactBeginning.Checked := qsmBeginning in gQuickSearchOptions.Match;
  cbExactEnding.Checked := qsmEnding in gQuickSearchOptions.Match;
  rgpSearchItems.ItemIndex := Integer(gQuickSearchOptions.Items);
  rgpSearchCase.ItemIndex := Integer(gQuickSearchOptions.SearchCase);
  cgpOptions.Checked[OPTION_AUTOHIDE_POSITION] := gQuickFilterAutoHide;
end;

function TfrmOptionsQuickSearchFilter.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gQuickSearch := not rbNoneQS.Checked;

  if rbCtrlAltLetterQS.Checked then
    gQuickSearchMode := [ssCtrl, ssAlt];
  if rbAltLetterQS.Checked then
    gQuickSearchMode := [ssAlt];
  if rbLetterQS.Checked then
    gQuickSearchMode := [];

  gQuickFilter := not rbNoneQF.Checked;

  if rbCtrlAltLetterQF.Checked then
    gQuickFilterMode := [ssCtrl, ssAlt];
  if rbAltLetterQF.Checked then
    gQuickFilterMode := [ssAlt];
  if rbLetterQF.Checked then
    gQuickFilterMode := [];

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


{
  Search & Replace dialog
  for lazarus converted from SynEdit by
  Radek Cervinka, radek.cervinka@centrum.cz

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU General Public License along with
  this program; if not, write to the Free Software Foundation, Inc., 59 Temple
  Place - Suite 330, Boston, MA 02111-1307, USA.



based on SynEdit demo, original license:

-------------------------------------------------------------------------------

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dlgSearchText.pas, released 2000-06-23.

The Original Code is part of the SearchReplaceDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: dlgSearchText.pas,v 1.3 2002/08/01 05:44:05 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit fEditSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type

  { TfrmEditSearchReplace }

  TfrmEditSearchReplace = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbSearchText: TComboBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchRegExp: TCheckBox;
    cbReplaceText: TComboBox;
    gbSearchOptions: TGroupBox;
    lblReplaceWith: TLabel;
    lblSearchFor: TLabel;
    rgSearchDirection: TRadioGroup;
    procedure btnOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    function GetSearchBackwards: boolean;
    function GetSearchCaseSensitive: boolean;
    function GetSearchFromCursor: boolean;
    function GetSearchInSelection: boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: boolean;
    function GetSearchRegExp: boolean;
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    procedure SetSearchBackwards(Value: boolean);
    procedure SetSearchCaseSensitive(Value: boolean);
    procedure SetSearchFromCursor(Value: boolean);
    procedure SetSearchInSelection(Value: boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: boolean);
    procedure SetSearchRegExp(Value: boolean);
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
  public
    constructor Create(AOwner: TComponent; AReplace: Boolean); reintroduce;
    property SearchBackwards: boolean read GetSearchBackwards
      write SetSearchBackwards;
    property SearchCaseSensitive: boolean read GetSearchCaseSensitive
      write SetSearchCaseSensitive;
    property SearchFromCursor: boolean read GetSearchFromCursor
      write SetSearchFromCursor;
    property SearchInSelectionOnly: boolean read GetSearchInSelection
      write SetSearchInSelection;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory
      write SetSearchTextHistory;
    property SearchWholeWords: boolean read GetSearchWholeWords
      write SetSearchWholeWords;
    property SearchRegExp: boolean read GetSearchRegExp
      write SetSearchRegExp;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory
      write SetReplaceTextHistory;
  end;

implementation

uses
  uLng, uDCUtils;

{ TfrmEditSearchReplace }

procedure TfrmEditSearchReplace.btnOKClick(Sender: TObject);
begin
  InsertFirstItem(cbSearchText.Text, cbSearchText.Items);
  ModalResult := mrOK
end;

procedure TfrmEditSearchReplace.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrOK then
    InsertFirstItem(cbReplaceText.Text, cbReplaceText.Items);
end;

procedure TfrmEditSearchReplace.FormShow(Sender: TObject);
begin
  if cbSearchText.Text = EmptyStr then
    begin
      if cbSearchText.Items.Count > 0 then
        cbSearchText.Text:= cbSearchText.Items[0];
    end;
  cbSearchText.SelectAll;
end;

function TfrmEditSearchReplace.GetSearchBackwards: boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TfrmEditSearchReplace.GetSearchCaseSensitive: boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TfrmEditSearchReplace.GetSearchFromCursor: boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TfrmEditSearchReplace.GetSearchInSelection: boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TfrmEditSearchReplace.GetSearchText: string;
begin
  Result := cbSearchText.Text;
end;

function TfrmEditSearchReplace.GetSearchTextHistory: string;
var
  i: integer;
begin
  for i:= cbSearchText.Items.Count - 1 downto 25 do
    cbSearchText.Items.Delete(i);
  Result:=cbSearchText.Items.CommaText;
end;

function TfrmEditSearchReplace.GetSearchWholeWords: boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;

function TfrmEditSearchReplace.GetSearchRegExp: boolean;
begin
  Result:= cbSearchRegExp.Checked;
end;

function TfrmEditSearchReplace.GetReplaceText: string;
begin
  Result := cbReplaceText.Text;
end;

function TfrmEditSearchReplace.GetReplaceTextHistory: string;
var
  i: integer;
begin
  for i:= cbSearchText.Items.Count - 1 downto 25 do
    cbReplaceText.Items.Delete(i);
  Result:=cbReplaceText.Items.CommaText;
end;

procedure TfrmEditSearchReplace.SetSearchBackwards(Value: boolean);
begin
  rgSearchDirection.ItemIndex := Ord(Value);
end;

procedure TfrmEditSearchReplace.SetSearchCaseSensitive(Value: boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TfrmEditSearchReplace.SetSearchFromCursor(Value: boolean);
begin
  cbSearchFromCursor.Checked := Value;
end;

procedure TfrmEditSearchReplace.SetSearchInSelection(Value: boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TfrmEditSearchReplace.SetSearchText(Value: string);
begin
  cbSearchText.Text := Value;
end;

procedure TfrmEditSearchReplace.SetSearchTextHistory(Value: string);
begin
  cbSearchText.Items.CommaText := Value;
end;

procedure TfrmEditSearchReplace.SetSearchWholeWords(Value: boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

procedure TfrmEditSearchReplace.SetSearchRegExp(Value: boolean);
begin
  cbSearchRegExp.Checked:= Value;
end;

procedure TfrmEditSearchReplace.SetReplaceText(Value: string);
begin
  cbReplaceText.Items.CommaText := Value;
end;

procedure TfrmEditSearchReplace.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;
end;

constructor TfrmEditSearchReplace.Create(AOwner: TComponent; AReplace: Boolean);
begin
  inherited Create(AOwner);
  if AReplace then
    begin
      Caption:= rsEditSearchReplace;
      lblReplaceWith.Visible:= True;
      cbReplaceText.Visible:= True;
    end
  else
    begin
      Caption:= rsEditSearchCaption;
      lblReplaceWith.Visible:= False;
      cbReplaceText.Visible:= False;
      Height:= Height - cbReplaceText.Height;
    end;
  rgSearchDirection.Items.Strings[0]:= rsEditSearchFrw;
  rgSearchDirection.Items.Strings[1]:= rsEditSearchBack;
end;

initialization
  {$I feditsearch.lrs}

end.


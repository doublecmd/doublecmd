{
  Search & Replace dialogs
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
  LResources,
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Buttons;

type

  TfrmEditSearch = class(TForm)
  protected
    lblSearchFor: TLabel;
    cbSearchText: TComboBox;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    rgSearchDirection: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
{    procedure cbSearchTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);}
  protected
    function GetSearchBackwards: boolean;
    function GetSearchCaseSensitive: boolean;
    function GetSearchFromCursor: boolean;
    function GetSearchInSelection: boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: boolean;
    procedure SetSearchBackwards(Value: boolean);
    procedure SetSearchCaseSensitive(Value: boolean);
    procedure SetSearchFromCursor(Value: boolean);
    procedure SetSearchInSelection(Value: boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: boolean);
    
    procedure CreateDialog(AOwner: TForm); virtual;
    
  public
    constructor Create(AOwner:TComponent);  override;
  
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
//    procedure LoadLng; override;
  end;

  TfrmEditSearchReplace = class(TfrmEditSearch)
  protected
    lblReplaceWith: TLabel;
    cbReplaceText: TComboBox;
  protected
    procedure CreateDialog(AOwner: TForm); override;
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
  public
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory
      write SetReplaceTextHistory;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  end;

{  TSearchDialog= Class(TCommonDialog)
  protected
    Ffrm: TfrmEditSearch;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function Execute:Boolean; override;
  end;
  
  TReplaceDialog = Class (TSearchDialog)
  public
    function Execute:Boolean; override;
  end;}
  


implementation



uses
{  uLng,} lclType;

{procedure TfrmEditSearch.LoadLng;
begin
  Caption:=lngGetString(clngEditSearch);
  lblSearchFor.Caption:= lngGetString(clngEditSearchFor);
  rgSearchDirection.Items.Clear;
  rgSearchDirection.Items.Add(lngGetString(clngEditSearchFrw));
  rgSearchDirection.Items.Add(lngGetString(clngEditSearchBack));
  cbSearchCaseSensitive.Caption:= lngGetString(clngEditSearchCase);
  cbSearchWholeWords.Caption:= lngGetString(clngEditSearchWholeWord);
  cbSearchFromCursor.Caption :=  lngGetString(clngEditSearchCaret);
  cbSearchSelectedOnly.Caption:=  lngGetString(clngEditSearchSelect);
end;


}

const

  cEditSearchCaption='Search';
  cEditSearchForLbl='&Search for:';
  cEditSearchFrw='&Forward';
  cEditSearchBack='&Backward';
  cEditSearchCase='C&ase sensitivity';
  cEditSearchWholeWord ='&Whole words only';
  cEditSearchCaret ='Search from &caret';
  cEditSearchSelect ='Selected &text only';
  cEditSearchOptions ='Option';
  cEditSearchDirection = 'Direction';
  cEditSearchReplace ='Replace';
  cEditSearchReplaceWith ='&Replace with:';

function TfrmEditSearch.GetSearchBackwards: boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TfrmEditSearch.GetSearchCaseSensitive: boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TfrmEditSearch.GetSearchFromCursor: boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TfrmEditSearch.GetSearchInSelection: boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TfrmEditSearch.GetSearchText: string;
begin
  Result := cbSearchText.Text;
end;

function TfrmEditSearch.GetSearchTextHistory: string;
var
  i: integer;
begin
  for i:= cbSearchText.Items.Count - 1 downto 25 do
    cbSearchText.Items.Delete(i);
  Result:=cbSearchText.Items.CommaText;
end;

function TfrmEditSearch.GetSearchWholeWords: boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;

procedure TfrmEditSearch.SetSearchBackwards(Value: boolean);
begin
  rgSearchDirection.ItemIndex := Ord(Value);
end;

procedure TfrmEditSearch.SetSearchCaseSensitive(Value: boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TfrmEditSearch.SetSearchFromCursor(Value: boolean);
begin
  cbSearchFromCursor.Checked := Value;
end;

procedure TfrmEditSearch.SetSearchInSelection(Value: boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TfrmEditSearch.SetSearchText(Value: string);
begin
  cbSearchText.Text := Value;
end;

procedure TfrmEditSearch.SetSearchTextHistory(Value: string);
begin
  cbSearchText.Items.CommaText := Value;
end;

procedure TfrmEditSearch.SetSearchWholeWords(Value: boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

procedure TfrmEditSearch.CreateDialog(AOwner: TForm);
begin
  SetInitialBounds(367, 203, 332, 170);

  BorderStyle := bsDialog;
  Position := poMainFormCenter;

  lblSearchFor:= TLabel.Create(AOwner);
  lblSearchFor.Parent:=AOwner;
  lblSearchFor.SetBounds(16, 8, 90, 17);

  cbSearchText:= TComboBox.Create(AOwner);
  cbSearchText.Parent:=AOwner;
  cbSearchText.SetBounds(112, 8, 208, 24 );

  gbSearchOptions :=TGroupBox.Create(AOwner);
  gbSearchOptions.Parent:=AOwner;
  gbSearchOptions.SetBounds(8, 32, 154, 127);

  
  cbSearchCaseSensitive := TCheckBox.Create(gbSearchOptions);
  cbSearchCaseSensitive.Parent:=gbSearchOptions;
  
  cbSearchCaseSensitive.SetBounds(0,0 ,142, 20);
  
  cbSearchWholeWords := TCheckBox.Create(gbSearchOptions);
  cbSearchWholeWords.Parent:=gbSearchOptions;
  cbSearchWholeWords.SetBounds(0,20 ,142, 20);

  
  cbSearchFromCursor := TCheckBox.Create(gbSearchOptions);
  cbSearchFromCursor.Parent:=gbSearchOptions;
  cbSearchFromCursor.SetBounds(0,65 ,142, 20);
  
  cbSearchSelectedOnly := TCheckBox.Create(gbSearchOptions);
  cbSearchSelectedOnly.Parent:=gbSearchOptions;
  cbSearchSelectedOnly.SetBounds(0, 41 ,142, 20);
  
  rgSearchDirection := TRadioGroup.Create(AOwner);
  rgSearchDirection.Parent:=AOwner;
  rgSearchDirection.SetBounds(170, 32, 154, 72);
  
  btnOK := TButton.Create(AOwner);
  btnOK.Parent:=AOwner;
  btnOK.Left:=170;
  btnOK.Top:=136;
  btnOK.Height := 32;
  btnOK.Caption:='OK'; // TODO: change this
  btnOK.OnClick:=@btnOKClick;

  btnCancel:= TButton.Create(AOwner);
  btnCancel.Parent:=AOwner;
  btnCancel.Top:=136;
  btnCancel.Left:=249;
  btnCancel.Height := 32;
  btnCancel.Cancel:=True;
  btnCancel.ModalResult:=mrCancel;
  btnCancel.Caption:='Cancel'; // TODO: change this
  
  
  Caption:= cEditSearchCaption;
  
  lblSearchFor.Caption:= cEditSearchForLbl;
  rgSearchDirection.Items.Add(cEditSearchFrw);
  rgSearchDirection.Items.Add(cEditSearchBack);
  cbSearchCaseSensitive.Caption:= cEditSearchCase;
  cbSearchWholeWords.Caption:= cEditSearchWholeWord;
  cbSearchFromCursor.Caption :=  cEditSearchCaret;
  cbSearchSelectedOnly.Caption:=  cEditSearchSelect;
  
  gbSearchOptions.Caption:=cEditSearchOptions;
  rgSearchDirection.Caption:=CEditSearchDirection;
end;

constructor TfrmEditSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateDialog(Self);
end;


procedure TfrmEditSearch.btnOKClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  s := cbSearchText.Text;
  if s <> '' then begin
    i := cbSearchText.Items.IndexOf(s);
    if i > -1 then begin
      cbSearchText.Items.Delete(i);
      cbSearchText.Items.Insert(0, s);
      cbSearchText.Text := s;
    end else
      cbSearchText.Items.Insert(0, s);
  end;
  ModalResult := mrOK
end;
{
procedure TfrmEditSearch.cbSearchTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_Down) and (cbSearchText.Items.Count>0) then
  begin
    cbSearchText.DroppedDown:=True;
  end;
end;
}
{ TSearchDialog }
{
constructor TSearchDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Ffrm:=nil;
end;

destructor TSearchDialog.Destroy;
begin
  if assigned(Ffrm) then
    FreeAndNil(Ffrm);
  inherited Destroy;
end;

function TSearchDialog.Execute:Boolean;
begin
  if not assigned(Ffrm) then
    Ffrm:=TfrmEditSearch.Create(Self);
  if assigned(FDialogCreated) then
    FDialogCreated(Ffrm);
  Result:=Ffrm.ShowModal=mrOK;
end;
}
{ TfrmEditSearchReplace }

procedure TfrmEditSearchReplace.CreateDialog(AOwner: TForm);
begin
  inherited CreateDialog(AOwner); // create search dialog

  BorderStyle := bsDialog;
  Position := poMainFormCenter;

  Height:=Height+40;
  gbSearchOptions.Top:=gbSearchOptions.Top+30;
  rgSearchDirection.Top:=rgSearchDirection.Top+30;
  lblReplaceWith:= TLabel.Create(AOwner);
  lblReplaceWith.Parent:=AOwner;
  lblReplaceWith.SetBounds(16, 35, 90, 17);

  cbReplaceText:= TComboBox.Create(AOwner);
  cbReplaceText.Parent:=AOwner;
  cbReplaceText.SetBounds(112, 35, 208, 24 );

  btnOK.Top:=btnOK.Top+30;
  btnOK.Height := 32;
  btnCancel.Top:=btnCancel.Top+30;
  btnCancel.Height := 32;

  Caption:=cEditSearchReplace;
  lblReplaceWith.Caption:=cEditSearchReplaceWith;
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

procedure TfrmEditSearchReplace.SetReplaceText(Value: string);
begin
  cbReplaceText.Items.CommaText := Value;
end;

procedure TfrmEditSearchReplace.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;

end;

procedure TfrmEditSearchReplace.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  s: string;
  i: integer;
begin
  inherited;
  if ModalResult = mrOK then begin
    s := cbReplaceText.Text;
    if s <> '' then begin
      i := cbReplaceText.Items.IndexOf(s);
      if i > -1 then begin
        cbReplaceText.Items.Delete(i);
        cbReplaceText.Items.Insert(0, s);
        cbReplaceText.Text := s;
      end else
        cbReplaceText.Items.Insert(0, s);
    end;
  end;
end;

{ TReplaceDialog }
{
function TReplaceDialog.Execute: Boolean;
begin
  if not assigned(Ffrm) then
    Ffrm:=TfrmEditSearchReplace.Create(Self);
  if assigned(FDialogCreated) then
    FDialogCreated(Ffrm);
  Result:=Ffrm.ShowModal=mrOK;
end;
}
end.

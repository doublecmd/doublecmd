{
   Double Commander
   -------------------------------------------------------------------------
   Search & Replace dialog

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2022 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fEditSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons, ButtonPanel,
  SynEdit, SynEditTypes, uOSForms, DCClassesUtf8;

type
  { TEditSearchOptions }

  TEditSearchOptions = record
    SearchText: String;
    ReplaceText: String;
    Flags: TSynSearchOptions;
  end;

  { TEditSearchDialogOption }
  //Not only it helps to show what we want to offer to user, it will help to determine the default
  //When used as parameters of function, place on required.
  //When used as a returned value, we'll include the status of all.
  TEditSearchDialogOption = set of (eswoCaseSensitiveChecked, eswoCaseSensitiveUnchecked,
                                    eswoWholeWordChecked, eswoWholeWordUnchecked,
                                    eswoSelectedTextChecked, eswoSelectedTextUnchecked,
                                    eswoSearchFromCursorChecked, eswoSearchFromCursorUnchecked,
                                    eswoRegularExpressChecked, eswoRegularExpressUnchecked,
                                    eswoDirectionDisabled, eswoDirectionEnabledForward, eswoDirectionEnabledBackward);

  { TfrmEditSearchReplace }

  TfrmEditSearchReplace = class(TModalForm)
    ButtonPanel: TButtonPanel;
    cbSearchText: TComboBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchRegExp: TCheckBox;
    cbReplaceText: TComboBox;
    cbMultiLine: TCheckBox;
    gbSearchOptions: TGroupBox;
    lblReplaceWith: TCheckBox;
    lblSearchFor: TLabel;
    rgSearchDirection: TRadioGroup;
    procedure btnOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblReplaceWithChange(Sender: TObject);
    procedure RequestAlign(Data: PtrInt);
  private
    function GetSearchOptions: TEditSearchOptions;
    procedure SetSearchOptions(AValue: TEditSearchOptions);
    function GetTextSearchOptions: UIntPtr;
  public
    constructor Create(AOwner: TComponent; AReplace: TCheckBoxState); reintroduce;
    property SearchOptions: TEditSearchOptions read GetSearchOptions write SetSearchOptions;
  end;

  function GetSimpleSearchAndReplaceString(AOwner: TComponent; OptionAllowed: TEditSearchDialogOption; var sSearchText: string; var sReplaceText: string; var OptionsToReturn:TEditSearchDialogOption; PastSearchList:TStringListEx; PastReplaceList:TStringListEx):boolean;

  procedure DoSearchReplaceText(AEditor: TCustomSynEdit; AReplace, ABackwards: Boolean; AOptions: TEditSearchOptions);
  procedure ShowSearchReplaceDialog(AOwner: TComponent; AEditor: TCustomSynEdit; AReplace: TCheckBoxState; var AOptions: TEditSearchOptions);

implementation

{$R *.lfm}

uses
  Math, Graphics, uGlobs, uLng, uDCUtils, uFindFiles, uShowMsg;

function GetSimpleSearchAndReplaceString(AOwner:TComponent; OptionAllowed:TEditSearchDialogOption; var sSearchText:string; var sReplaceText:string; var OptionsToReturn:TEditSearchDialogOption; PastSearchList:TStringListEx; PastReplaceList:TStringListEx):boolean;
var
  dlg: TfrmEditSearchReplace;
begin
  result:=FALSE;
  OptionsToReturn:=[];

  dlg := TfrmEditSearchReplace.Create(AOwner, cbChecked);
  try
    with dlg do
    begin
      //1. Let's enable to options host wanted to offer to user
      cbSearchCaseSensitive.Enabled := ((eswoCaseSensitiveChecked in OptionAllowed) OR (eswoCaseSensitiveUnchecked in OptionAllowed));
      cbSearchWholeWords.Enabled := ((eswoWholeWordChecked in OptionAllowed) OR (eswoWholeWordUnchecked in OptionAllowed));
      cbSearchSelectedOnly.Enabled := ((eswoSelectedTextChecked in OptionAllowed) OR (eswoSelectedTextUnchecked in OptionAllowed));
      cbSearchFromCursor.Enabled := ((eswoSearchFromCursorChecked in OptionAllowed) OR (eswoSearchFromCursorUnchecked in OptionAllowed));
      cbSearchRegExp.Enabled := ((eswoRegularExpressChecked in OptionAllowed) OR (eswoRegularExpressUnchecked in OptionAllowed));
      rgSearchDirection.Enabled := ((eswoDirectionEnabledForward in OptionAllowed) OR (eswoDirectionEnabledBackward in OptionAllowed));

      //2. Let's set the option to their default according to what host wants to offer
      cbSearchCaseSensitive.Checked := (eswoCaseSensitiveChecked in OptionAllowed);
      cbSearchWholeWords.Checked := (eswoWholeWordChecked in OptionAllowed);
      cbSearchSelectedOnly.Checked := (eswoSelectedTextChecked in OptionAllowed);
      cbSearchFromCursor.Checked := (eswoSearchFromCursorChecked in OptionAllowed);
      cbSearchRegExp.Checked := (eswoRegularExpressChecked in OptionAllowed);
      rgSearchDirection.ItemIndex:=ifthen((eswoDirectionEnabledBackward in OptionAllowed),1,0);

      //3. Setup the SEARCH info
      if sSearchText='' then sSearchText:=rsEditSearchCaption;
      cbSearchText.Items.Assign(PastSearchList);
      cbSearchText.Text:= sSearchText;

      //4. Setup the REPLACE info
      if sReplaceText='' then sReplaceText:=rsEditSearchReplace;
      cbReplaceText.Items.Assign(PastReplaceList);
      cbReplaceText.Text:=sReplaceText;

      //5. Get feedback from user
      if ShowModal=mrOk then
      begin
        //6. Let's set the options wanted by the user
        if cbSearchCaseSensitive.Enabled then
          if cbSearchCaseSensitive.Checked then OptionsToReturn:=OptionsToReturn+[eswoCaseSensitiveChecked] else OptionsToReturn:=OptionsToReturn+[eswoCaseSensitiveUnchecked];
        if cbSearchWholeWords.Enabled then
          if cbSearchWholeWords.Checked then OptionsToReturn:=OptionsToReturn+[eswoWholeWordChecked] else OptionsToReturn:=OptionsToReturn+[eswoWholeWordUnchecked];
        if cbSearchSelectedOnly.Enabled then
          if cbSearchSelectedOnly.Checked then OptionsToReturn:=OptionsToReturn+[eswoSelectedTextChecked] else OptionsToReturn:=OptionsToReturn+[eswoSelectedTextUnchecked];
        if cbSearchFromCursor.Enabled then
          if cbSearchFromCursor.Checked then OptionsToReturn:=OptionsToReturn+[eswoSearchFromCursorChecked] else OptionsToReturn:=OptionsToReturn+[eswoSearchFromCursorUnchecked];
        if cbSearchRegExp.Enabled then
          if cbSearchRegExp.Checked then OptionsToReturn:=OptionsToReturn+[eswoRegularExpressChecked] else OptionsToReturn:=OptionsToReturn+[eswoRegularExpressUnchecked];
        if rgSearchDirection.Enabled then
          if rgSearchDirection.ItemIndex=1 then OptionsToReturn:=OptionsToReturn+[eswoDirectionEnabledBackward] else OptionsToReturn:=OptionsToReturn+[eswoDirectionEnabledForward];

        //7. Let's set our history
        PastSearchList.Assign(cbSearchText.Items);
        PastReplaceList.Assign(cbReplaceText.Items);

        //8. And FINALLY, our valuable text to search we wanted to replace!
        sSearchText:=cbSearchText.Text;
        sReplaceText:=cbReplaceText.Text;
        result:=((sSearchText<>sReplaceText) AND (sSearchText<>''));
      end;
    end;

  finally
    FreeAndNil(Dlg);
  end;
end;

procedure DoSearchReplaceText(AEditor: TCustomSynEdit; AReplace,
  ABackwards: Boolean; AOptions: TEditSearchOptions);
var
  Flags: TSynSearchOptions;
begin
  Flags := AOptions.Flags;

  if ABackwards then
    Include(Flags, ssoBackwards)
  else begin
    Exclude(Flags, ssoBackwards);
  end;

  if AReplace then begin
    Flags += [ssoPrompt, ssoReplace, ssoReplaceAll];
  end;

  try
    if AEditor.SearchReplace(AOptions.SearchText, AOptions.ReplaceText, Flags) = 0 then
    begin
      if ssoBackwards in Flags then
        AEditor.BlockEnd := AEditor.BlockBegin
      else begin
        AEditor.BlockBegin := AEditor.BlockEnd;
      end;
      AEditor.CaretXY := AEditor.BlockBegin;
      msgOK(Format(rsViewNotFound, ['"' + AOptions.SearchText + '"']));
    end;
  except
    on E: Exception do msgError(E.Message);
  end;
end;

procedure ShowSearchReplaceDialog(AOwner: TComponent; AEditor: TCustomSynEdit;
  AReplace: TCheckBoxState; var AOptions: TEditSearchOptions);
var
  Options: TEditSearchOptions;
begin
  with TfrmEditSearchReplace.Create(AOwner, AReplace) do
  try
    Options := AOptions;

    if AEditor.SelAvail and (AEditor.BlockBegin.Y <> AEditor.BlockEnd.Y) then
      Options.Flags += [ssoSelectedOnly];

    // If something is selected then search for that text
    if AEditor.SelAvail and (AEditor.BlockBegin.Y = AEditor.BlockEnd.Y) then
      Options.SearchText := AEditor.SelText
    else begin
      Options.SearchText := AEditor.GetWordAtRowCol(AEditor.CaretXY);
    end;

    cbSearchText.Items.Text := glsSearchHistory.Text;
    cbReplaceText.Items.Text := glsReplaceHistory.Text;

    // Assign search options
    SearchOptions := Options;

    if ShowModal = mrOK then
    begin
      AOptions := SearchOptions;
      glsSearchHistory.Assign(cbSearchText.Items);
      glsReplaceHistory.Assign(cbReplaceText.Items);
      if AOptions.SearchText <> '' then
      begin
        DoSearchReplaceText(AEditor, AReplace = cbChecked, ssoBackwards in AOptions.Flags, AOptions);
        AOptions.Flags -= [ssoEntireScope];
        gFirstTextSearch := False;
      end;
    end;
  finally
    Free;
  end;
end;

{ TfrmEditSearchReplace }

procedure TfrmEditSearchReplace.btnOKClick(Sender: TObject);
begin
  InsertFirstItem(cbSearchText.Text, cbSearchText, GetTextSearchOptions);
  ModalResult := mrOK
end;

procedure TfrmEditSearchReplace.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrOK then
    InsertFirstItem(cbReplaceText.Text, cbReplaceText, GetTextSearchOptions);
end;

procedure TfrmEditSearchReplace.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);
end;

procedure TfrmEditSearchReplace.FormShow(Sender: TObject);
begin
  if cbSearchText.Text = EmptyStr then
    begin
      if cbSearchText.Items.Count > 0 then
        cbSearchText.Text:= cbSearchText.Items[0];
    end;
  cbSearchText.SelectAll;

  // Fixes AutoSize under Qt
  Application.QueueAsyncCall(@RequestAlign, 0);
end;

procedure TfrmEditSearchReplace.lblReplaceWithChange(Sender: TObject);
begin
  if lblReplaceWith.Checked then
    Caption:= rsEditSearchReplace
  else begin
    Caption:= rsEditSearchCaption;
  end;
  cbReplaceText.Enabled := lblReplaceWith.Checked;
end;

procedure TfrmEditSearchReplace.RequestAlign(Data: PtrInt);
begin
  Width := Width + 1;
  Width := Width - 1;
end;

function TfrmEditSearchReplace.GetSearchOptions: TEditSearchOptions;
begin
  Result.SearchText:= cbSearchText.Text;
  Result.ReplaceText := cbReplaceText.Text;

  Result.Flags := [];
  if cbSearchCaseSensitive.Checked then
    Result.Flags += [ssoMatchCase];
  if cbSearchWholeWords.Checked then
    Result.Flags += [ssoWholeWord];
  if cbSearchSelectedOnly.Checked then
    Result.Flags += [ssoSelectedOnly];
  if not cbSearchFromCursor.Checked then
    Result.Flags += [ssoEntireScope];
  if cbSearchRegExp.Checked then
    Result.Flags += [ssoRegExpr];
  if cbMultiLine.Checked then
    Result.Flags += [ssoRegExprMultiLine];
  if rgSearchDirection.ItemIndex = 1 then
    Result.Flags += [ssoBackwards];
end;

procedure TfrmEditSearchReplace.SetSearchOptions(AValue: TEditSearchOptions);
begin
  cbSearchText.Text := AValue.SearchText;
  cbReplaceText.Text := AValue.ReplaceText;
  with AValue do
  begin
    cbSearchCaseSensitive.Checked := ssoMatchCase in Flags;
    cbSearchWholeWords.Checked := ssoWholeWord in Flags;
    cbSearchSelectedOnly.Checked := ssoSelectedOnly in Flags;
    cbSearchFromCursor.Checked := not (ssoEntireScope in Flags);
    cbSearchRegExp.Checked := ssoRegExpr in Flags;
    cbMultiLine.Checked := ssoRegExprMultiLine in Flags;
    rgSearchDirection.ItemIndex := Ord(ssoBackwards in Flags);
  end;
end;

function TfrmEditSearchReplace.GetTextSearchOptions: UIntPtr;
var
  Options: TTextSearchOptions absolute Result;
begin
  Result:= 0;
  if cbSearchCaseSensitive.Checked then
    Include(Options, tsoMatchCase);
  if cbSearchRegExp.Checked then
    Include(Options, tsoRegExpr);
end;

constructor TfrmEditSearchReplace.Create(AOwner: TComponent; AReplace: TCheckBoxState);
begin
  inherited Create(AOwner);

  lblReplaceWith.Visible:= (AReplace <> cbGrayed);
  cbReplaceText.Visible:= (AReplace <> cbGrayed);

  cbReplaceText.Enabled := (AReplace = cbChecked);
  lblReplaceWith.Checked := (AReplace = cbChecked);

  if (AReplace = cbChecked) then
    Caption:= rsEditSearchReplace
  else begin
    Caption:= rsEditSearchCaption;
  end;

  rgSearchDirection.Items.Strings[0]:= rsEditSearchFrw;
  rgSearchDirection.Items.Strings[1]:= rsEditSearchBack;
end;

end.


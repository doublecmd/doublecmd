{
    Double Commander
    -------------------------------------------------------------------------
    Support for popup menu to help to enter percent variable parameters.

    The idea here is:
      -Have something to help user who wants to use "%..." variable to have a quick hint built-in the application instead of having to seach in help of doc files.
      -Next to an edit box where we could type in "%...", have a speed button that would popup a menu where user sees to possible percent variables available.
      -User sees what he could use, select one and then it would type in the edit box the select "%...".
      -This unit is to build that popup instead of having it in different unit.
      -It creates the popup only the first time use click on "%" button.
      -If in the main session use again a "%" button, the popup menu is already created and almost ready.
      -"Almost", because we simply need to re-assign the possible different target edit box.

    Copyright (C) 2015-2019  Alexander Koblov (alexx2000@mail.ru)

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

unit uVariableMenuSupport;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, Menus, StdCtrls,

  //DC
  dmHelpManager;
type
  TVariableMenuItem = class(TMenuItem)
  private
    FSubstitutionText: string;
  public
    constructor MyCreate(TheOwner: TComponent; ParamCaption, ParamSubstitutionText: string);
    procedure VariableHelperClick(Sender: TObject);
    procedure HelpOnVariablesClick(Sender: TObject);
  end;

  TPercentVariablePopupMenu = class(TPopUpMenu)
  private
    FAssociatedComponent: TComponent;
    procedure PopulateMenuWithVariableHelper;
  public
    constructor Create(AOnwer: TComponent); override;
    property AssociatedTComponent: TComponent read FAssociatedComponent write FAssociatedComponent;
  end;

procedure BringPercentVariablePopupMenu(AComponent: TComponent);

implementation

uses
  //Lazarus, Free-Pascal, etc.
  EditBtn, SysUtils, Dialogs,

  //DC
  uLng;
var
  PercentVariablePopupMenu: TPercentVariablePopupMenu = nil;

{ TPercentVariablePopupMenu.Create }
constructor TPercentVariablePopupMenu.Create(AOnwer: TComponent);
begin
  inherited Create(AOnwer);
  FAssociatedComponent := nil;
  PopulateMenuWithVariableHelper;
end;

{ TPercentVariablePopupMenu.PopulateMenuWithVariableHelper }
procedure TPercentVariablePopupMenu.PopulateMenuWithVariableHelper;
type
  tHelperMenuDispatcher = set of (hmdNothing, hmdSeparator, hmdListLevel);

  tFunctionHelper = record
    sLetter: string;
    sDescription: string;
    HelperMenuDispatcher: tHelperMenuDispatcher;
  end;

  tFirstSubLevelHelper = record
    sLetter: string;
    sDescription: string;
    HelperMenuDispatcher: tHelperMenuDispatcher;
  end;

const
  NbOfFunctions = 12;
  FunctionHelper: array[1..NbOfFunctions] of tFunctionHelper = (
    (sLetter: 'f'; sDescription: rsVarOnlyFilename; HelperMenuDispatcher: []),
    (sLetter: 'd'; sDescription: rsVarPath; HelperMenuDispatcher: []),
    (sLetter: 'z'; sDescription: rsVarLastDirOfPath; HelperMenuDispatcher: []),
    (sLetter: 'p'; sDescription: rsVarFullPath; HelperMenuDispatcher: []),
    (sLetter: 'o'; sDescription: rsVarFilenameNoExt; HelperMenuDispatcher: []),
    (sLetter: 'e'; sDescription: rsVarOnlyExtension; HelperMenuDispatcher: []),
    (sLetter: 'v'; sDescription: rsVarRelativePathAndFilename; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: 'D'; sDescription: rsVarCurrentPath; HelperMenuDispatcher: []),
    (sLetter: 'Z'; sDescription: rsVarLastDirCurrentPath; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: 'L'; sDescription: rsVarListFullFilename; HelperMenuDispatcher: [hmdListLevel]),
    (sLetter: 'F'; sDescription: rsVarListFilename; HelperMenuDispatcher: [hmdListLevel]),
    (sLetter: 'R'; sDescription: rsVarListRelativeFilename; HelperMenuDispatcher: [hmdListLevel, hmdSeparator]));

  NbOfSubListLevel = 4;
  SubListLevelHelper: array[1..NbOfSubListLevel] of tFirstSubLevelHelper = (
    (sLetter: 'U'; sDescription: rsVarListInUTF8; HelperMenuDispatcher: []),
    (sLetter: 'W'; sDescription: rsVarListInUTF16; HelperMenuDispatcher: []),
    (sLetter: 'UQ'; sDescription: rsVarListInUTF8Quoted; HelperMenuDispatcher: []),
    (sLetter: 'WQ'; sDescription: rsVarListInUTF16Quoted; HelperMenuDispatcher: []));

  NbOfSubLevel = 6;
  SubLevelHelper: array[1..NbOfSubLevel] of tFirstSubLevelHelper = (
    (sLetter: 's'; sDescription: rsVarSourcePanel; HelperMenuDispatcher: []),
    (sLetter: 't'; sDescription: rsVarTargetPanel; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: 'l'; sDescription: rsVarLeftPanel; HelperMenuDispatcher: []),
    (sLetter: 'r'; sDescription: rsVarRightPanel; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: 'b'; sDescription: rsVarBothPanelLeftToRight; HelperMenuDispatcher: []),
    (sLetter: 'p'; sDescription: rsVarBothPanelActiveToInactive; HelperMenuDispatcher: []));

  NbOfSubLevelExamples = 15;
  SubLevelHelperExamples: array[1..NbOfSubLevelExamples] of tFirstSubLevelHelper = (
    (sLetter: '%?'; sDescription: rsVarShowCommandPrior; HelperMenuDispatcher: []),
    (sLetter: '%%'; sDescription: rsVarPercentSign; HelperMenuDispatcher: []),
    (sLetter: '%#'; sDescription: rsVarPercentChangeToPound; HelperMenuDispatcher: []),
    (sLetter: '#%'; sDescription: rsVarPoundChangeToPercent; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: '%"0'; sDescription: rsVarWillNotBeQuoted; HelperMenuDispatcher: []),
    (sLetter: '%"1'; sDescription: rsVarWillBeQuoted; HelperMenuDispatcher: []),
    (sLetter: '%/0'; sDescription: rsVarWillNotHaveEndingDelimiter; HelperMenuDispatcher: []),
    (sLetter: '%/1'; sDescription: rsVarWillHaveEndingDelimiter; HelperMenuDispatcher: []),
    (sLetter: '%t0'; sDescription: rsVarWillNotDoInTerminal; HelperMenuDispatcher: []),
    (sLetter: '%t1'; sDescription: rsVarWillDoInTerminal; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: rsVarSimpleMessage; sDescription: rsVarSimpleShowMessage; HelperMenuDispatcher: []),
    (sLetter: rsVarPromptUserForParam; sDescription: rsVarInputParam; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: '%f{-a }'; sDescription: rsVarPrependElement; HelperMenuDispatcher: []),
    (sLetter: '%f{[}{]} '; sDescription: rsVarEncloseElement; HelperMenuDispatcher: []),
    (sLetter: '%pr2'; sDescription: rsVarSecondElementRightPanel; HelperMenuDispatcher: []));

var
  miMainTree, miSubTree, miSubListTree: TVariableMenuItem;
  iFunction, iSubLevel, iSubListLevel: integer;

  procedure InsertSeparatorInMainMenu;
  begin
    miMainTree := TVariableMenuItem.MyCreate(Self, '-', '');
    Self.Items.Add(miMainTree);
  end;

  procedure InsertSeparatorInSubMenu;
  begin
    miSubTree := TVariableMenuItem.MyCreate(Self, '-', '');
    miMainTree.Add(miSubTree);
  end;

  procedure InsertSeparatorInSubListMenu;
  begin
    miSubTree := TVariableMenuItem.MyCreate(Self, '-', '');
    miSubListTree.Add(miSubTree);
  end;

begin
  //Add the automatic helper
  for iFunction := 1 to NbOfFunctions do
  begin
    miMainTree := TVariableMenuItem.MyCreate(Self, '%' + FunctionHelper[iFunction].sLetter + ' - ' + FunctionHelper[iFunction].sDescription, '');
    TPopupMenu(Self).Items.Add(miMainTree);

    miSubTree := TVariableMenuItem.MyCreate(Self, '%' + FunctionHelper[iFunction].sLetter + ' - ' + FunctionHelper[iFunction].sDescription, '%' + FunctionHelper[iFunction].sLetter);
    miMainTree.Add(miSubTree);
    InsertSeparatorInSubMenu;

    for iSubLevel := 1 to NbOfSubLevel do
    begin
      miSubTree := TVariableMenuItem.MyCreate(Self, '%' + FunctionHelper[iFunction].sLetter + SubLevelHelper[iSubLevel].sLetter + ' - ' + '...' + SubLevelHelper[iSubLevel].sDescription, '%' + FunctionHelper[iFunction].sLetter + SubLevelHelper[iSubLevel].sLetter);
      miMainTree.Add(miSubTree);
      if hmdSeparator in SubLevelHelper[iSubLevel].HelperMenuDispatcher then InsertSeparatorInSubMenu;
    end;

    if hmdListLevel in FunctionHelper[iFunction].HelperMenuDispatcher then
    begin
      InsertSeparatorInSubMenu;

      for iSubListLevel := 1 to NbOfSubListLevel do
      begin
        miSubListTree := TVariableMenuItem.MyCreate(Self, '%' + FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + ' - ' + '...' + SubListLevelHelper[iSubListLevel].sDescription + '...', '');
        miMainTree.Add(miSubListTree);

        miSubTree := TVariableMenuItem.MyCreate(Self, '%' + FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + ' - ' + SubListLevelHelper[iSubListLevel].sDescription, '%' + FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter);
        miSubListTree.Add(miSubTree);
        InsertSeparatorInSubListMenu;

        for iSubLevel := 1 to NbOfSubLevel do
        begin
          miSubTree := TVariableMenuItem.MyCreate(Self, '%' + FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + SubLevelHelper[iSubLevel].sLetter + ' - ' + '...' + SubLevelHelper[iSubLevel].sDescription,
            '%' + FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + SubLevelHelper[iSubLevel].sLetter);

          miSubListTree.Add(miSubTree);
          if hmdSeparator in SubLevelHelper[iSubLevel].HelperMenuDispatcher then InsertSeparatorInSubListMenu;
        end;
      end;
    end;

    if hmdSeparator in FunctionHelper[iFunction].HelperMenuDispatcher then InsertSeparatorInMainMenu;
  end;

  //Add the more complex-not-so-complex other examples
  miMainTree := TVariableMenuItem.MyCreate(Self, rsVarOtherExamples, '');
  TPopupMenu(Self).Items.Add(miMainTree);
  for iSubLevel := 1 to NbOfSubLevelExamples do
  begin
    miSubTree := TVariableMenuItem.MyCreate(Self, SubLevelHelperExamples[iSubLevel].sLetter + ' - ' + SubLevelHelperExamples[iSubLevel].sDescription, SubLevelHelperExamples[iSubLevel].sLetter);
    miMainTree.Add(miSubTree);
    if hmdSeparator in SubLevelHelperExamples[iSubLevel].HelperMenuDispatcher then InsertSeparatorInSubMenu;
  end;

  //Add link for the help at the end
  InsertSeparatorInMainMenu;
  miMainTree := TVariableMenuItem.MyCreate(Self, rsVarHelpWith, '');

  TPopupMenu(Self).Items.Add(miMainTree);
end;

{ TVariableMenuItem.MyCreate }
constructor TVariableMenuItem.MyCreate(TheOwner: TComponent; ParamCaption, ParamSubstitutionText: string);
begin
  inherited Create(TheOwner);

  Caption := ParamCaption;
  if ParamCaption <> rsVarHelpWith then
  begin
    if ParamSubstitutionText <> '' then
    begin
      FSubstitutionText := ParamSubstitutionText;
      OnClick := @VariableHelperClick;
    end;
  end
  else
  begin
    OnClick := @HelpOnVariablesClick;
  end;
end;

{ TVariableMenuItem.VariableHelperClick }
//Our intention:
//-If something is selected, we replace what's selected by the helper string
//-If nothing is selected, we insert our helper string at the current cursor pos
//-If nothing is there at all, we add, simply
//Since "TDirectoryEdit" is not a descendant of "TCustomEdit", we need to treat it separately.
procedure TVariableMenuItem.VariableHelperClick(Sender: TObject);
begin
  if TPercentVariablePopupMenu(Owner).FAssociatedComponent.ClassNameIs('TDirectoryEdit') then
    TDirectoryEdit(TPercentVariablePopupMenu(Owner).FAssociatedComponent).SelText := FSubstitutionText
  else
    TCustomEdit(TPercentVariablePopupMenu(Owner).FAssociatedComponent).SelText := FSubstitutionText;
end;

{ TVariableMenuItem.HelpOnVariablesClick }
procedure TVariableMenuItem.HelpOnVariablesClick(Sender: TObject);
begin
  ShowHelpForKeywordWithAnchor('/variables.html');
end;

{ BringPercentVariablePopupMenu }
procedure BringPercentVariablePopupMenu(AComponent: TComponent);
begin
  if PercentVariablePopupMenu = nil then PercentVariablePopupMenu := TPercentVariablePopupMenu.Create(nil);
  PercentVariablePopupMenu.AssociatedTComponent := AComponent;
  PercentVariablePopupMenu.PopUp;
end;

initialization
  //JEDI code formatter doesn't like a "finalization" section without prior an "initialization" one...

finalization
  if PercentVariablePopupMenu <> nil then FreeAndNil(PercentVariablePopupMenu);

end.

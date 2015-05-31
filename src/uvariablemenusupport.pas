{
    Double Commander
    -------------------------------------------------------------------------
    Support for popup menu to help to enter variable parameters.

	Copyright (C) 2015  Alexander Koblov (alexx2000@mail.ru)

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

unit uVariableMenuSupport;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, Menus, StdCtrls,

  //DC
  dmHelpManager;
type
  TSupportForVariableHelperMenu = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    procedure PopulateMenuWithVariableHelper(pmToPopulate: TComponent; ParamAssociatedComponent: TComponent);
  end;

  TVariableMenuItem = class(TMenuItem)
  private
    FAssociatedTComponent: TComponent;
    FSubstitutionText: string;
  public
    constructor MyCreate(TheOwner: TComponent; ParamTComponent: TComponent; ParamCaption, ParamSubstitutionText: string);
    procedure VariableHelperClick(Sender: TObject);
    procedure HelpOnVariablesClick(Sender: TObject);
  end;

procedure LoadVariableMenuSupport;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  SysUtils, Dialogs,

  //DC
  uGlobs, uLng;
procedure LoadVariableMenuSupport;
begin
  gSupportForVariableHelperMenu := TSupportForVariableHelperMenu.Create;
end;

{ TVariableMenuItem.MyCreate }
constructor TVariableMenuItem.MyCreate(TheOwner: TComponent; ParamTComponent: TComponent; ParamCaption, ParamSubstitutionText: string);
begin
  inherited Create(TheOwner);

  Caption := ParamCaption;
  if ParamCaption <> rsVarHelpWith then
  begin
    if ParamSubstitutionText <> '' then
    begin
      FAssociatedTComponent := ParamTComponent;
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
procedure TVariableMenuItem.VariableHelperClick(Sender: TObject);
begin
  TCustomEdit(FAssociatedTComponent).SelText := FSubstitutionText;
end;

{ TVariableMenuItem.HelpOnVariablesClick }
procedure TVariableMenuItem.HelpOnVariablesClick(Sender: TObject);
begin
  ShowHelpForKeywordWithAnchor(PathDelim + 'Variables.html');
end;

{ TSupportForVariableHelperMenu.Create }
constructor TSupportForVariableHelperMenu.Create;
begin
  inherited Create;
end;

{ TSupportForVariableHelperMenu.Destroy }
destructor TSupportForVariableHelperMenu.Destroy;
begin
  inherited Destroy;
end;

{ TSupportForVariableHelperMenu.PopulateMenuWithVariableHelper }
procedure TSupportForVariableHelperMenu.PopulateMenuWithVariableHelper(pmToPopulate: TComponent; ParamAssociatedComponent: TComponent);
type
  tFunctionHelper = record
    sLetter: string;
    sDescription: string;
    bAddSeparator: boolean;
  end;

  tFirstSubLevelHelper = record
    sLetter: string;
    sDescription: string;
    bAddSeparator: boolean;
  end;

const
  NbOfFunctions = 10;
  FunctionHelper: array[1..NbOfFunctions] of tFunctionHelper = (
    (sLetter: 'f'; sDescription: rsVarOnlyFilename; bAddSeparator: False),
    (sLetter: 'd'; sDescription: rsVarPath; bAddSeparator: False),
    (sLetter: 'p'; sDescription: rsVarFullPath; bAddSeparator: False),
    (sLetter: 'o'; sDescription: rsVarFilenameNoExt; bAddSeparator: False),
    (sLetter: 'e'; sDescription: rsVarOnlyExtension; bAddSeparator: False),
    (sLetter: 'v'; sDescription: rsVarRelativePathAndFilename; bAddSeparator: True),
    (sLetter: 'D'; sDescription: rsVarCurrentPath; bAddSeparator: True),
    (sLetter: 'L'; sDescription: rsVarListFullFilename; bAddSeparator: False),
    (sLetter: 'F'; sDescription: rsVarListFilename; bAddSeparator: False),
    (sLetter: 'R'; sDescription: rsVarListRelativeFilename; bAddSeparator: False));

  NbOfSubLevel = 6;
  SubLevelHelper: array[1..NbOfSubLevel] of tFirstSubLevelHelper = (
    (sLetter: 's'; sDescription: rsVarSourcePanel; bAddSeparator: False),
    (sLetter: 't'; sDescription: rsVarTargetPanel; bAddSeparator: True),
    (sLetter: 'l'; sDescription: rsVarLeftPanel; bAddSeparator: False),
    (sLetter: 'r'; sDescription: rsVarRightPanel; bAddSeparator: True),
    (sLetter: 'b'; sDescription: rsVarBothPanelLeftToRight; bAddSeparator: False),
    (sLetter: 'p'; sDescription: rsVarBothPanelActiveToInactive; bAddSeparator: False));

  NbOfSubLevelExamples = 15;
  SubLevelHelperExamples: array[1..NbOfSubLevelExamples] of tFirstSubLevelHelper = (
    (sLetter: '%?'; sDescription: rsVarShowCommandPrior; bAddSeparator: False),
    (sLetter: '%%'; sDescription: rsVarPercentSign; bAddSeparator: False),
    (sLetter: '%#'; sDescription: rsVarPercentChangeToPound; bAddSeparator: False),
    (sLetter: '#%'; sDescription: rsVarPoundChangeToPercent; bAddSeparator: True),
    (sLetter: '%"0'; sDescription: rsVarWillNotBeQuoted; bAddSeparator: False),
    (sLetter: '%"1'; sDescription: rsVarWillBeQuoted; bAddSeparator: False),
    (sLetter: '%/0'; sDescription: rsVarWillNotHaveEndingDelimiter; bAddSeparator: False),
    (sLetter: '%/1'; sDescription: rsVarWillHaveEndingDelimiter; bAddSeparator: False),
    (sLetter: '%t0'; sDescription: rsVarWillNotDoInTerminal; bAddSeparator: False),
    (sLetter: '%t1'; sDescription: rsVarWillDoInTerminal; bAddSeparator: True),
    (sLetter: rsVarSimpleMessage; sDescription: rsVarSimpleShowMessage; bAddSeparator: False),
    (sLetter: rsVarPromptUserForParam; sDescription: rsVarInputParam; bAddSeparator: True),
    (sLetter: '%f{-a }'; sDescription: rsVarPrependElement; bAddSeparator: False),
    (sLetter: '%f{[}{]} '; sDescription: rsVarEncloseElement; bAddSeparator: False),
    (sLetter: '%pr2'; sDescription: rsVarSecondElementRightPanel; bAddSeparator: False));

var
  miMainTree, miSubTree: TVariableMenuItem;
  iFunction, iSubLevel: integer;

  procedure InsertSeparatorInMainMenu;
  begin
    miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, '-', '');
    TPopupMenu(pmToPopulate).Items.Add(miMainTree);
  end;

  procedure InsertSeparatorInSubMenu;
  begin
    miSubTree := TVariableMenuItem.MyCreate(miMainTree, nil, '-', '');
    miMainTree.Add(miSubTree);
  end;

begin
  //Add the automatic helper
  for iFunction := 1 to NbOfFunctions do
  begin
    miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, '%' + FunctionHelper[iFunction].sLetter + ' - ' + FunctionHelper[iFunction].sDescription, '');
    TPopupMenu(pmToPopulate).Items.Add(miMainTree);

    miSubTree := TVariableMenuItem.MyCreate(miMainTree, ParamAssociatedComponent, '%' + FunctionHelper[iFunction].sLetter + ' - ' + FunctionHelper[iFunction].sDescription, '%' + FunctionHelper[iFunction].sLetter);
    miMainTree.Add(miSubTree);

    InsertSeparatorInSubMenu;

    for iSubLevel := 1 to NbOfSubLevel do
    begin
      miSubTree := TVariableMenuItem.MyCreate(miMainTree, ParamAssociatedComponent, '%' + FunctionHelper[iFunction].sLetter + SubLevelHelper[iSubLevel].sLetter + ' - ' + '...' + SubLevelHelper[iSubLevel].sDescription, '%' + FunctionHelper[iFunction].sLetter + SubLevelHelper[iSubLevel].sLetter);
      miMainTree.Add(miSubTree);
      if SubLevelHelper[iSubLevel].bAddSeparator then
        InsertSeparatorInSubMenu;
    end;

    if (FunctionHelper[iFunction].bAddSeparator) then
      InsertSeparatorInMainMenu;
  end;

  //Add the more complex-not-so-complex other examples
  miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, rsVarOtherExamples, '');
  TPopupMenu(pmToPopulate).Items.Add(miMainTree);
  for iSubLevel := 1 to NbOfSubLevelExamples do
  begin
    miSubTree := TVariableMenuItem.MyCreate(miMainTree, ParamAssociatedComponent, SubLevelHelperExamples[iSubLevel].sLetter + ' - ' + SubLevelHelperExamples[iSubLevel].sDescription, SubLevelHelperExamples[iSubLevel].sLetter);
    miMainTree.Add(miSubTree);

    if SubLevelHelperExamples[iSubLevel].bAddSeparator then
      InsertSeparatorInSubMenu;
  end;

  //Add link for the help at the end
  InsertSeparatorInMainMenu;
  miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, rsVarHelpWith, '');

  TPopupMenu(pmToPopulate).Items.Add(miMainTree);
end;

end.

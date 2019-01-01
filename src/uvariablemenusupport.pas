{
    Double Commander
    -------------------------------------------------------------------------
    Support for popup menu to help to enter variable parameters.

    Copyright (C) 2015-2018  Alexander Koblov (alexx2000@mail.ru)

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
  ShowHelpForKeywordWithAnchor('/variables.html');
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
  NbOfFunctions = 10;
  FunctionHelper: array[1..NbOfFunctions] of tFunctionHelper = (
    (sLetter: 'f'; sDescription: rsVarOnlyFilename; HelperMenuDispatcher: []),
    (sLetter: 'd'; sDescription: rsVarPath; HelperMenuDispatcher: []),
    (sLetter: 'p'; sDescription: rsVarFullPath; HelperMenuDispatcher: []),
    (sLetter: 'o'; sDescription: rsVarFilenameNoExt; HelperMenuDispatcher: []),
    (sLetter: 'e'; sDescription: rsVarOnlyExtension; HelperMenuDispatcher: []),
    (sLetter: 'v'; sDescription: rsVarRelativePathAndFilename; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: 'D'; sDescription: rsVarCurrentPath; HelperMenuDispatcher: [hmdSeparator]),
    (sLetter: 'L'; sDescription: rsVarListFullFilename; HelperMenuDispatcher: [hmdListLevel]),
    (sLetter: 'F'; sDescription: rsVarListFilename; HelperMenuDispatcher: [hmdListLevel]),
    (sLetter: 'R'; sDescription: rsVarListRelativeFilename; HelperMenuDispatcher: [hmdListLevel,hmdSeparator]));

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
    miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, '-', '');
    TPopupMenu(pmToPopulate).Items.Add(miMainTree);
  end;

  procedure InsertSeparatorInSubMenu;
  begin
    miSubTree := TVariableMenuItem.MyCreate(miMainTree, nil, '-', '');
    miMainTree.Add(miSubTree);
  end;

  procedure InsertSeparatorInSubListMenu;
  begin
    miSubTree := TVariableMenuItem.MyCreate(miSubListTree, nil, '-', '');
    miSubListTree.Add(miSubTree);
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
      if hmdSeparator in SubLevelHelper[iSubLevel].HelperMenuDispatcher then InsertSeparatorInSubMenu;
    end;

    if hmdListLevel in FunctionHelper[iFunction].HelperMenuDispatcher then
    begin
      InsertSeparatorInSubMenu;

      for iSubListLevel:=1 to NbOfSubListLevel do
      begin
        miSubListTree := TVariableMenuItem.MyCreate(miMainTree, ParamAssociatedComponent, '%' +FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + ' - ' + '...' + SubListLevelHelper[iSubListLevel].sDescription + '...', '');
        miMainTree.Add(miSubListTree);

        miSubTree := TVariableMenuItem.MyCreate(miSubListTree, ParamAssociatedComponent, '%' +FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + ' - ' + SubListLevelHelper[iSubListLevel].sDescription, '%' +FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter);
        miSubListTree.Add(miSubTree);
        InsertSeparatorInSubListMenu;

        for iSubLevel := 1 to NbOfSubLevel do
        begin
          miSubTree := TVariableMenuItem.MyCreate(miSubListTree, ParamAssociatedComponent, '%' +FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + SubLevelHelper[iSubLevel].sLetter + ' - ' + '...' + SubLevelHelper[iSubLevel].sDescription, '%' +FunctionHelper[iFunction].sLetter + SubListLevelHelper[iSubListLevel].sLetter + SubLevelHelper[iSubLevel].sLetter);
          miSubListTree.Add(miSubTree);
          if hmdSeparator in SubLevelHelper[iSubLevel].HelperMenuDispatcher then InsertSeparatorInSubListMenu;
        end;
      end;
    end;

    if hmdSeparator in FunctionHelper[iFunction].HelperMenuDispatcher then InsertSeparatorInMainMenu;
  end;

  //Add the more complex-not-so-complex other examples
  miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, rsVarOtherExamples, '');
  TPopupMenu(pmToPopulate).Items.Add(miMainTree);
  for iSubLevel := 1 to NbOfSubLevelExamples do
  begin
    miSubTree := TVariableMenuItem.MyCreate(miMainTree, ParamAssociatedComponent, SubLevelHelperExamples[iSubLevel].sLetter + ' - ' + SubLevelHelperExamples[iSubLevel].sDescription, SubLevelHelperExamples[iSubLevel].sLetter);
    miMainTree.Add(miSubTree);
    if hmdSeparator in SubLevelHelperExamples[iSubLevel].HelperMenuDispatcher then InsertSeparatorInSubMenu;
  end;

  //Add link for the help at the end
  InsertSeparatorInMainMenu;
  miMainTree := TVariableMenuItem.MyCreate(pmToPopulate, nil, rsVarHelpWith, '');

  TPopupMenu(pmToPopulate).Items.Add(miMainTree);
end;

end.

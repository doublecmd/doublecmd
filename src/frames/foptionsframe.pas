{
   Double Commander
   -------------------------------------------------------------------------
   Options frame page

   Copyright (C) 2006-2014  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, fgl;

type
  TOptionsEditorInitFlag = (oeifLoad);
  TOptionsEditorInitFlags = set of TOptionsEditorInitFlag;
  TOptionsEditorSaveFlag = (oesfNeedsRestart);
  TOptionsEditorSaveFlags = set of TOptionsEditorSaveFlag;

  TOptionsEditor = class;
  TOptionsEditorClass = class of TOptionsEditor;
  TOptionsEditorClassList = class;

  { IOptionsDialog }

  {$interfaces corba}
  IOptionsDialog = interface
    ['{E62AAF5E-74ED-49AB-93F2-DBE210BF6723}']
    procedure LoadSettings;
    function GetEditor(EditorClass: TOptionsEditorClass): TOptionsEditor;
  end;
  {$interfaces default}

  { TOptionsEditor }

  TOptionsEditor = class(TFrame)
  private
    FOptionsDialog: IOptionsDialog;
  protected
    procedure Init; virtual;
    procedure Done; virtual;
    procedure Load; virtual;
    function  Save: TOptionsEditorSaveFlags; virtual;
    property OptionsDialog: IOptionsDialog read FOptionsDialog;
  public
    destructor Destroy; override;

    class function GetIconIndex: Integer; virtual; abstract;
    class function GetTitle: String; virtual; abstract;
    class function IsEmpty: Boolean; virtual;

    procedure LoadSettings;
    function  SaveSettings: TOptionsEditorSaveFlags;
    procedure Init(AParent: TWinControl;
                   AOptionsDialog: IOptionsDialog;
                   Flags: TOptionsEditorInitFlags);
    function CanWeClose(var WillNeedUpdateWindowView:boolean):boolean; virtual;
  end;

  { TOptionsEditorRec }

  TOptionsEditorRec = class
  private
    FChildren: TOptionsEditorClassList;
    FEditorClass: TOptionsEditorClass;
    function GetChildren: TOptionsEditorClassList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Editor: TOptionsEditorClass): TOptionsEditorRec;
    function HasChildren: Boolean;
    property Children: TOptionsEditorClassList read GetChildren;
    property EditorClass: TOptionsEditorClass read FEditorClass write FEditorClass;
  end;

  { TBaseOptionsEditorClassList }

  TBaseOptionsEditorClassList = specialize TFPGObjectList<TOptionsEditorRec>;

  { TOptionsEditorClassList }

  TOptionsEditorClassList = class(TBaseOptionsEditorClassList)
  public
    function Add(Editor: TOptionsEditorClass): TOptionsEditorRec; overload;
  end;

var
  OptionsEditorClassList: TOptionsEditorClassList = nil;

implementation

uses
  fOptionsArchivers,
  fOptionsAutoRefresh,
  fOptionsBehavior,
  fOptionsColumnsView,
  fOptionsConfiguration,
  fOptionsCustomColumns,
  fOptionsDragDrop,
  fOptionsDrivesListButton,
  fOptionsFileOperations,
  fOptionsFilePanelsColors,
  fOptionsFileTypesColors,
  fOptionsFilesViews,
  fOptionsFonts,
  fOptionsGroups,
  fOptionsHotkeys,
  fOptionsIcons,
  fOptionsIgnoreList,
  fOptionsKeyboard,
  fOptionsLanguage,
  fOptionsLayout,
  fOptionsLog,
  fOptionsMisc,
  fOptionsMouse,
  fOptionsPlugins,
  fOptionsQuickSearchFilter,
  fOptionsTabs,
  fOptionsTerminal,
  fOptionsToolbar,
  fOptionsTools,
  fOptionsToolsDiffer,
  fOptionsEditorColors,
  fOptionsToolTips,
  fOptionsFileAssoc,
  fOptionsDirectoryHotlist;

{ TOptionsEditorRec }

function TOptionsEditorRec.GetChildren: TOptionsEditorClassList;
begin
  if not Assigned(FChildren) then
    FChildren := TOptionsEditorClassList.Create;
  Result := FChildren;
end;

constructor TOptionsEditorRec.Create;
begin
  FChildren := nil;
end;

destructor TOptionsEditorRec.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FChildren);
end;

function TOptionsEditorRec.Add(Editor: TOptionsEditorClass): TOptionsEditorRec;
begin
  Result := Children.Add(Editor);
end;

function TOptionsEditorRec.HasChildren: Boolean;
begin
  Result := Assigned(FChildren) and (FChildren.Count > 0);
end;

{ TOptionsEditorClassList }

function TOptionsEditorClassList.Add(Editor: TOptionsEditorClass): TOptionsEditorRec;
begin
  Result := TOptionsEditorRec.Create;
  Add(Result);
  Result.EditorClass:= Editor;
end;

{ TOptionsEditor }

procedure TOptionsEditor.Init;
begin
  // Empty.
end;

function TOptionsEditor.CanWeClose(var WillNeedUpdateWindowView:boolean): boolean;
begin
  result:=TRUE;
end;

procedure TOptionsEditor.Done;
begin
  // Empty.
end;

destructor TOptionsEditor.Destroy;
begin
  Done;
  inherited Destroy;
end;

class function TOptionsEditor.IsEmpty: Boolean;
begin
  Result := False;
end;

procedure TOptionsEditor.LoadSettings;
begin
  DisableAutoSizing;
  try
    Load;
  finally
    EnableAutoSizing;
  end;
end;

function TOptionsEditor.SaveSettings: TOptionsEditorSaveFlags;
begin
  Result := Save;
end;

procedure TOptionsEditor.Load;
begin
  // Empty.
end;

function TOptionsEditor.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
end;

procedure TOptionsEditor.Init(AParent: TWinControl;
                              AOptionsDialog: IOptionsDialog;
                              Flags: TOptionsEditorInitFlags);
begin
  DisableAutoSizing;
  try
    Parent := AParent;
    FOptionsDialog := AOptionsDialog;
    Init;
    if oeifLoad in Flags then
      LoadSettings;
  finally
    EnableAutoSizing;
  end;
end;

procedure MakeEditorsClassList;
var
  Main: TOptionsEditorClassList absolute OptionsEditorClassList;
  Colors,
  ColumnsView,
  FilesViews,
  Keyboard,
  Layout,
  Mouse,
  Tools,
  Editor: TOptionsEditorRec;
begin
  Main.Add(TfrmOptionsLanguage);
  Main.Add(TfrmOptionsBehavior);
  Tools := Main.Add(TOptionsToolsGroup);
  Tools.Add(TfrmOptionsViewer);
  Editor:= Tools.Add(TfrmOptionsEditor);
  Editor.Add(TfrmOptionsEditorColors);
  Tools.Add(TfrmOptionsDiffer);
  Tools.Add(TfrmOptionsTerminal);
  Main.Add(TfrmOptionsFonts);
  Colors := Main.Add(TOptionsColorsGroup);
  Colors.Add(TfrmOptionsFilePanelsColors);
  Colors.Add(TfrmOptionsFileTypesColors);
  Keyboard := Main.Add(TfrmOptionsKeyboard);
  Keyboard.Add(TfrmOptionsHotkeys);
  Mouse := Main.Add(TfrmOptionsMouse);
  Mouse.Add(TfrmOptionsDragDrop);
  FilesViews := Main.Add(TfrmOptionsFilesViews);
  ColumnsView := FilesViews.Add(TfrmOptionsColumnsView);
  ColumnsView.Add(TfrmOptionsCustomColumns);
  Main.Add(TfrmOptionsPlugins);
  Layout := Main.Add(TfrmOptionsLayout);
  Layout.Add(TfrmOptionsDrivesListButton);
  Main.Add(TfrmOptionsToolbar);
  Main.Add(TfrmOptionsFileOperations);
  Main.Add(TfrmOptionsTabs);
  Main.Add(TfrmOptionsLog);
  Main.Add(TfrmOptionsConfiguration);
  Main.Add(TfrmOptionsQuickSearchFilter);
  Main.Add(TfrmOptionsMisc);
  Main.Add(TfrmOptionsAutoRefresh);
  Main.Add(TfrmOptionsIcons);
  Main.Add(TfrmOptionsIgnoreList);
  Main.Add(TfrmOptionsArchivers);
  Main.Add(TfrmOptionsToolTips);
  Main.Add(TfrmOptionsFileAssoc);
  Main.Add(TfrmOptionsDirectoryHotlist);
end;

initialization
  OptionsEditorClassList:= TOptionsEditorClassList.Create;
  MakeEditorsClassList;

finalization
  if Assigned(OptionsEditorClassList) then
    FreeAndNil(OptionsEditorClassList);

end.


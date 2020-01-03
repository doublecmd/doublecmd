{
   Double Commander
   -------------------------------------------------------------------------
   Options frame page

   Copyright (C) 2006-2020  Alexander Koblov (alexx2000@mail.ru)

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

  // Regarding warning to user when attempting to exit configuration window or "option editor" without having saved a modified setting:
  // 1o) Immediately after having load the options in "LoadSettings", we will compute a signature,
  //     which is a CRC32 of the related settings and memorize it.
  // 2o) When will attempt to close options, we'll recalculate again this signature, if it is different,
  //     we know user has change something and will prompt user to validate if he wants to save, discard or cancel quit.
  // 3o) For many option editors, signature may be computed simply by validating actual controls of the window like
  //     checkboxes state, edit box, etc.
  // 4o) For others, we need to run specific computation like signature of list like hot directories list, favorites tabs list, etc.
  // 5o) For some computing the signature with controls should not be done.
  // 6o) Here is a list of function and procedure around that:
  // 7o)   "IsSignatureComputedFromAllWindowComponents": Function that may be overloaded by specific option editor to indicate
  //                                                     if we may compute signature of controls of the editor or not.
  //                                                     By default, it is the case.
  // 8o)   "ExtraOptionsSignature": Function that may overloaded by specifica option editor when signature must include extra element not present in controls of the editor like a list of structures, etc.
  //                                By default, nothing more is required.
  // 9o)   "ComputeCompleteOptionsSignature": Will first compute signature based on controls >IF< "IsSignatureComputedFromAllWindowComponents" is not invalidated by specific option editor
  //                                          Then will make progress that signature >IF< "ExtraOptionsSignature" has been overload by specific option editor.

  TOptionsEditor = class(TFrame)
  private
    FOptionsDialog: IOptionsDialog;
    FLastLoadedOptionSignature: dword;
  protected
    procedure Init; virtual;
    procedure Done; virtual;
    procedure Load; virtual;
    function  Save: TOptionsEditorSaveFlags; virtual;
    property OptionsDialog: IOptionsDialog read FOptionsDialog;
  public
    property LastLoadedOptionSignature:dword read FLastLoadedOptionSignature write FLastLoadedOptionSignature; // Let it public so Options Forms will be able to know what was initial signature.
    destructor Destroy; override;

    class function GetIconIndex: Integer; virtual; abstract;
    class function GetTitle: String; virtual; abstract;
    class function IsEmpty: Boolean; virtual;
    function IsSignatureComputedFromAllWindowComponents: Boolean; virtual;
    function ComputeCompleteOptionsSignature: dword;
    function ExtraOptionsSignature(CurrentSignature:dword):dword; virtual;
    function CanWeClose(var SaveFlags:TOptionsEditorSaveFlags; bForceClose:boolean=false):boolean; virtual;

    procedure LoadSettings;
    function  SaveSettings: TOptionsEditorSaveFlags;
    procedure Init(AParent: TWinControl;
                   AOptionsDialog: IOptionsDialog;
                   Flags: TOptionsEditorInitFlags);
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
  uLng,
  uComponentsSignature,
  uShowMsg,
  fOptions,
  fOptionsArchivers,
  fOptionsAutoRefresh,
  fOptionsBehavior,
  fOptionsBriefView,
  fOptionsColumnsView,
  fOptionsConfiguration,
  fOptionsCustomColumns,
  fOptionsDragDrop,
  fOptionsDrivesListButton,
  fOptionsTreeViewMenu,
  fOptionsTreeViewMenuColor,
  fOptionsFileOperations,
  fOptionsFileSearch,
  fOptionsMultiRename,
  fOptionsFilePanelsColors,
  fOptionsFileTypesColors,
  fOptionsFilesViews,
  fOptionsFilesViewsComplement,
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
  fOptionsPluginsGroup,
  fOptionsPluginsDSX,
  fOptionsPluginsWCX,
  fOptionsPluginsWDX,
  fOptionsPluginsWFX,
  fOptionsPluginsWLX,
  fOptionsQuickSearchFilter,
  fOptionsTabs,
  fOptionsFavoriteTabs,
  fOptionsTabsExtra,
  fOptionsTerminal,
  fOptionsToolbar,
  fOptionsToolbarExtra,
  fOptionsToolbarMiddle,
  fOptionsTools,
  fOptionsToolsEditor,
  fOptionsToolsDiffer,
  fOptionsEditorColors,
  fOptionsToolTips,
  fOptionsFileAssoc,
  fOptionsFileAssocExtra,
  fOptionsDirectoryHotlist,
  fOptionsDirectoryHotlistExtra;

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

{ TOptionsEditor.ExtraOptionsSignature }
function TOptionsEditor.ExtraOptionsSignature(CurrentSignature:dword):dword;
begin
  result := CurrentSignature;
end;

{ TOptionsEditor.ComputeCompleteOptionsSignature }
function TOptionsEditor.ComputeCompleteOptionsSignature:dword;
begin
  result := $000000;

  if IsSignatureComputedFromAllWindowComponents then
    result := ComputeSignatureBasedOnComponent(Self, result);

  result := ExtraOptionsSignature(result);
end;

{ TOptionsEditor.CanWeClose }
function TOptionsEditor.CanWeClose(var SaveFlags:TOptionsEditorSaveFlags; bForceClose:boolean=false):boolean;
var
  Answer: TMyMsgResult;
begin
  SaveFlags:=[];

  if bForceClose then
    result:=True
  else
    result := (FLastLoadedOptionSignature = ComputeCompleteOptionsSignature);

  if (not result) OR bForceClose then
  begin
    if bForceClose then
    begin
      Answer:=mmrYes;
    end
    else
    begin
      ShowOptions(Self.ClassName);
      Answer := MsgBox(Format(rsOptionsEditorOptionsChanged, [GetTitle]), [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
    end;

    case Answer of
      mmrYes:
      begin
        SaveFlags := SaveSettings;
        result := True;
      end;

      mmrNo: result := True;
      else
        result := False;
    end;
  end;
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

{ TOptionsEditor.IsSignatureComputedFromAllWindowComponents }
function TOptionsEditor.IsSignatureComputedFromAllWindowComponents: Boolean;
begin
  Result := True;
end;

procedure TOptionsEditor.LoadSettings;
begin
  DisableAutoSizing;
  try
    Load;
    FLastLoadedOptionSignature := ComputeCompleteOptionsSignature;
  finally
    EnableAutoSizing;
  end;
end;

function TOptionsEditor.SaveSettings: TOptionsEditorSaveFlags;
begin
  Result := Save;
  FLastLoadedOptionSignature := ComputeCompleteOptionsSignature;
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
  Colors, ColumnsView, FilesViews, Keyboard, Layout, Mouse, Tools, Editor,
  FileAssoc, ToolbarConfig, FileOperation, FolderTabs, Plugins,
  DirectoryHotlistConfig: TOptionsEditorRec;
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
  FilesViews.Add(TfrmOptionsFilesViewsComplement);
  FilesViews.Add(TfrmOptionsBriefView);
  ColumnsView := FilesViews.Add(TfrmOptionsColumnsView);
  ColumnsView.Add(TfrmOptionsCustomColumns);
  Plugins := Main.Add(TfrmOptionsPluginsGroup);
  Plugins.Add(TfrmOptionsPluginsDSX);
  Plugins.Add(TfrmOptionsPluginsWCX);
  Plugins.Add(TfrmOptionsPluginsWDX);
  Plugins.Add(TfrmOptionsPluginsWFX);
  Plugins.Add(TfrmOptionsPluginsWLX);
  Layout := Main.Add(TfrmOptionsLayout);
  Layout.Add(TfrmOptionsDrivesListButton);
  Layout.Add(TfrmOptionsTreeViewMenu);
  Layout.Add(TfrmOptionsTreeViewMenuColor);
  ToolbarConfig := Main.Add(TfrmOptionsToolbar);
  ToolbarConfig.Add(TfrmOptionsToolbarMiddle);
  ToolbarConfig.Add(TfrmOptionsToolbarExtra);
  FileOperation := Main.Add(TfrmOptionsFileOperations);
  FileOperation.Add(TfrmOptionsFileSearch);
  FileOperation.Add(TfrmOptionsMultiRename);
  FolderTabs := Main.Add(TfrmOptionsTabs);
  FolderTabs.Add(TfrmOptionsFavoriteTabs);
  FolderTabs.Add(TfrmOptionsTabsExtra);
  Main.Add(TfrmOptionsLog);
  Main.Add(TfrmOptionsConfiguration);
  Main.Add(TfrmOptionsQuickSearchFilter);
  Main.Add(TfrmOptionsMisc);
  Main.Add(TfrmOptionsAutoRefresh);
  Main.Add(TfrmOptionsIcons);
  Main.Add(TfrmOptionsIgnoreList);
  Main.Add(TfrmOptionsArchivers);
  Main.Add(TfrmOptionsToolTips);
  FileAssoc := Main.Add(TfrmOptionsFileAssoc);
  FileAssoc.Add(TfrmOptionsFileAssocExtra);
  DirectoryHotlistConfig := Main.Add(TfrmOptionsDirectoryHotlist);
  DirectoryHotlistConfig.Add(TfrmOptionsDirectoryHotlistExtra);
end;

initialization
  OptionsEditorClassList:= TOptionsEditorClassList.Create;
  MakeEditorsClassList;

finalization
  if Assigned(OptionsEditorClassList) then
    FreeAndNil(OptionsEditorClassList);

end.


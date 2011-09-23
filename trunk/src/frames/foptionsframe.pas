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

  TOptionsEditorClassList = class;

  { TOptionsEditor }

  TOptionsEditor = class(TFrame)
  protected
    procedure Init; virtual;
    procedure Done; virtual;
    procedure Load; virtual;
    function  Save: TOptionsEditorSaveFlags; virtual;
  public
    destructor Destroy; override;

    class function GetIconIndex: Integer; virtual; abstract;
    class function GetTitle: String; virtual; abstract;

    procedure LoadSettings;
    function  SaveSettings: TOptionsEditorSaveFlags;
    procedure Init(AParent: TWinControl; Flags: TOptionsEditorInitFlags);
  end;

  { TOptionsEditorClass }

  TOptionsEditorClass = class of TOptionsEditor;

  { TOptionsEditorRec }

  TOptionsEditorRec = class
  private
    FChildren: TOptionsEditorClassList;
    FEditorClass: TOptionsEditorClass;
    function GetChildren: TOptionsEditorClassList;
  public
    constructor Create;
    destructor Destroy; override;
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
  fOptionsColumns,
  fOptionsConfiguration,
  fOptionsFileOperations,
  fOptionsFilePanelsColors,
  fOptionsFileTypesColors,
  fOptionsFonts,
  fOptionsGroups,
  fOptionsHotkeys,
  fOptionsIcons,
  fOptionsIgnoreList,
  fOptionsLanguage,
  fOptionsLayout,
  fOptionsLog,
  fOptionsMisc,
  fOptionsPlugins,
  fOptionsQuickSearchFilter,
  fOptionsTabs,
  fOptionsTools,
  fOptionsToolTips;

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

procedure TOptionsEditor.Done;
begin
  // Empty.
end;

destructor TOptionsEditor.Destroy;
begin
  Done;
  inherited Destroy;
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

procedure TOptionsEditor.Init(AParent: TWinControl; Flags: TOptionsEditorInitFlags);
begin
  DisableAutoSizing;
  try
    Parent := AParent;
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
  Colors: TOptionsEditorRec;
begin
  Main.Add(TfrmOptionsLanguage);
  Main.Add(TfrmOptionsBehavior);
  Main.Add(TfrmOptionsTools);
  Main.Add(TfrmOptionsFonts);
  Colors := Main.Add(TOptionsColorsGroup);
  Colors.Children.Add(TfrmOptionsFilePanelsColors);
  Colors.Children.Add(TfrmOptionsFileTypesColors);
  Main.Add(TfrmOptionsHotkeys);
  Main.Add(TfrmOptionsPlugins);
  Main.Add(TfrmOptionsLayout);
  Main.Add(TfrmOptionsFileOperations);
  Main.Add(TfrmOptionsTabs);
  Main.Add(TfrmOptionsLog);
  Main.Add(TfrmOptionsConfiguration);
  Main.Add(TfrmOptionsQuickSearchFilter);
  Main.Add(TfrmOptionsColumns);
  Main.Add(TfrmOptionsMisc);
  Main.Add(TfrmOptionsAutoRefresh);
  Main.Add(TfrmOptionsIcons);
  Main.Add(TfrmOptionsIgnoreList);
  Main.Add(TfrmOptionsArchivers);
  Main.Add(TfrmOptionsToolTips);
end;

initialization
  OptionsEditorClassList:= TOptionsEditorClassList.Create;
  MakeEditorsClassList;

finalization
  if Assigned(OptionsEditorClassList) then
    FreeAndNil(OptionsEditorClassList);

end.


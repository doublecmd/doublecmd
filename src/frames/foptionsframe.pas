unit fOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fgl;

type

  TOptionsEditorType =
    (optedLanguage = 0,
     optedBehaviours,
     optedTools,
     optedFonts,
     optedColors,
     optedHotKeys,
     optedPlugins,
     optedLayout,
     optedFileOperations,
     optedFolderTabs,
     optedLog,
     optedConfiguration,
     optedQuickSearchFilter,
     optedColumns,
     optedMisc,
     optedAutoRefresh,
     optedIcons,
     optedIgnoreList,
     optedArchivers,
     optedTooltips);

  TOptionsEditorSaveFlag = (oesfNeedsRestart);
  TOptionsEditorSaveFlags = set of TOptionsEditorSaveFlag;

  { TOptionsEditor }

  TOptionsEditor = class(TFrame)
  protected
    procedure Init; virtual;
    procedure Done; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function GetTitle: String; virtual; abstract;
    procedure Load; virtual; abstract;
    function Save: TOptionsEditorSaveFlags; virtual; abstract;
  end;

  { TOptionsEditorClass }

  TOptionsEditorClass = class of TOptionsEditor;

  { TOptionsEditorRec }

  TOptionsEditorRec = class
    OptionsEditorType: TOptionsEditorType;
    OptionsEditorClass: TOptionsEditorClass;
  end;

  { TOptionsEditorList }

  TOptionsEditorList = specialize TFPGList<TOptionsEditor>;

  { TBaseOptionsEditorClassList }

  TBaseOptionsEditorClassList = specialize TFPGObjectList<TOptionsEditorRec>;

  { TOptionsEditorClassList }

  TOptionsEditorClassList = class(TBaseOptionsEditorClassList)
  public
    procedure Sort; overload;
  end;

  procedure RegisterOptionsEditor(AEditorType: TOptionsEditorType; AEditorClass: TOptionsEditorClass);

var
  OptionsEditorClassList: TOptionsEditorClassList = nil;

implementation

function CompareOptionsEditor(const Item1, Item2: TOptionsEditorRec): Integer;
begin
  if Item1.OptionsEditorType < Item2.OptionsEditorType then
    Result := -1
  else if Item1.OptionsEditorType > Item2.OptionsEditorType then
    Result := 1
  else
    Result := 0;
end;

{ TOptionsEditorClassList }

procedure TOptionsEditorClassList.Sort;
begin
  Sort(@CompareOptionsEditor);
end;

{ TOptionsEditor }

procedure TOptionsEditor.Init;
begin

end;

procedure TOptionsEditor.Done;
begin

end;

constructor TOptionsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Init;
end;

destructor TOptionsEditor.Destroy;
begin
  Done;
  inherited Destroy;
end;

procedure RegisterOptionsEditor(AEditorType: TOptionsEditorType; AEditorClass: TOptionsEditorClass);
var
  OptionsEditorRec: TOptionsEditorRec;
begin
  OptionsEditorRec:= TOptionsEditorRec.Create;
  OptionsEditorRec.OptionsEditorType:= AEditorType;
  OptionsEditorRec.OptionsEditorClass:= AEditorClass;
  OptionsEditorClassList.Add(OptionsEditorRec);
  OptionsEditorClassList.Sort;
end;

initialization
  OptionsEditorClassList:= TOptionsEditorClassList.Create;

finalization
  if Assigned(OptionsEditorClassList) then
    FreeAndNil(OptionsEditorClassList);

end.


unit fOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fgl;

type

  { TOptionsEditorClass }

  TOptionsEditorClass = class of TOptionsEditor;

  { TOptionsEditor }

  TOptionsEditor = class(TFrame)
  protected
    procedure Init; virtual;
    procedure Done; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
  end;

  { TOptionsEditorRec }

  TOptionsEditorRec = class
    PageIndex: LongInt;
    OptionsEditorClass: TOptionsEditorClass;
  end;

  { TOptionsEditorList }

  TOptionsEditorList = specialize TFPGList<TOptionsEditor>;

  { TOptionsEditorClassList }

  TOptionsEditorClassList = specialize TFPGObjectList<TOptionsEditorRec>;

  procedure RegisterOptionsEditor(APageIndex: LongInt; AEditorClass: TOptionsEditorClass);

var
  OptionsEditorClassList: TOptionsEditorClassList = nil;

implementation

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

procedure RegisterOptionsEditor(APageIndex: LongInt; AEditorClass: TOptionsEditorClass);
var
  OptionsEditorRec: TOptionsEditorRec;
begin
  OptionsEditorRec:= TOptionsEditorRec.Create;
  OptionsEditorRec.PageIndex:= APageIndex;
  OptionsEditorRec.OptionsEditorClass:= AEditorClass;
  OptionsEditorClassList.Add(OptionsEditorRec);
end;

initialization
  OptionsEditorClassList:= TOptionsEditorClassList.Create;

finalization
  if Assigned(OptionsEditorClassList) then
    FreeAndNil(OptionsEditorClassList);

end.


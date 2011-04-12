unit fOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fgl;

type

  { TAbstractOptionsEditor }

  TAbstractOptionsEditor = class(TFrame)
  protected
    procedure Init; virtual;
    procedure Done; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
  end;

  { TOptionsEditorList }

  TOptionsEditorList = specialize TFPGList<TAbstractOptionsEditor>;

implementation

{ TAbstractOptionsEditor }

procedure TAbstractOptionsEditor.Init;
begin

end;

procedure TAbstractOptionsEditor.Done;
begin

end;

constructor TAbstractOptionsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Init;
end;

destructor TAbstractOptionsEditor.Destroy;
begin
  Done;
  inherited Destroy;
end;

end.


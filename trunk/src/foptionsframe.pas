unit fOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fgl;

type

  { TAbstractOptionsEditor }

  TAbstractOptionsEditor = class(TFrame)
  protected
    procedure Init; virtual; abstract;
    procedure Done; virtual; abstract;
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


unit fWfxPluginCopyMoveOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uFileSourceOperationOptionsUI,
  uWfxPluginCopyOperation,
  uWfxPluginMoveOperation;

type

  { TWfxPluginCopyMoveOperationOptionsUI }

  TWfxPluginCopyMoveOperationOptionsUI = class(TFileSourceOperationOptionsUI)
    cmbFileExists: TComboBox;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
  private
    procedure SetOperationOptions(CopyOperation: TWfxPluginCopyOperation); overload;
    procedure SetOperationOptions(MoveOperation: TWfxPluginMoveOperation); overload;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveOptions; override;
    procedure SetOperationOptions(Operation: TObject); override;
  end;

  TWfxPluginCopyOperationOptionsUI = class(TWfxPluginCopyMoveOperationOptionsUI)
  end;

  TWfxPluginMoveOperationOptionsUI = class(TWfxPluginCopyMoveOperationOptionsUI)
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uFileSourceOperationOptions;

{ TWfxPluginCopyMoveOperationOptionsUI }

constructor TWfxPluginCopyMoveOperationOptionsUI.Create(AOwner: TComponent);
begin
  inherited;

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SaveOptions;
begin
  // TODO: Saving options for each file source operation separately.
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(Operation: TObject);
begin
  if Operation is TWfxPluginCopyOperation then
    SetOperationOptions(Operation as TWfxPluginCopyOperation)
  else if Operation is TWfxPluginMoveOperation then
    SetOperationOptions(Operation as TWfxPluginMoveOperation);
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(CopyOperation: TWfxPluginCopyOperation);
begin
  with CopyOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
end;

procedure TWfxPluginCopyMoveOperationOptionsUI.SetOperationOptions(MoveOperation: TWfxPluginMoveOperation);
begin
  with MoveOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
  end;
end;

end.

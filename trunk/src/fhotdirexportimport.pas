unit fhotdirexportimport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls;

type

  { Tfrmhotdirexportimport }

  Tfrmhotdirexportimport = class(TForm)
    btnSelectAll: TBitBtn;
    btnSelectionDone: TBitBtn;
    btnCancelImportation: TBitBtn;
    lblHintHoldControl: TLabel;
    lbHint: TLabel;
    tvDirectoryHotlistToExportImport: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmhotdirexportimport: Tfrmhotdirexportimport;

implementation

{$R *.lfm}

uses
  uGlobs;

{ Tfrmhotdirexportimport }

procedure Tfrmhotdirexportimport.FormCreate(Sender: TObject);
begin
  // Initialize property storage
  InitPropStorage(Self);
end;

end.


unit fhotdirimportexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { Tfrmhotdirimportexport }

  Tfrmhotdirimportexport = class(TForm)
    btnSelectAll: TBitBtn;
    btnSelectionDone: TBitBtn;
    btnCancelImportation: TBitBtn;
    lblHintHoldControl: TLabel;
    lbHint: TLabel;
    lsImportedHotDir: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    RememberItemHeight:longint;
  public
    { public declarations }
  end;

var
  frmhotdirimportexport: Tfrmhotdirimportexport;

implementation

{$R *.lfm}

uses
  uGlobs;

{ Tfrmhotdirimportexport }

procedure Tfrmhotdirimportexport.FormCreate(Sender: TObject);
begin
  // Initialize property storage
  InitPropStorage(Self);

  lsImportedHotDir.Items.Add('W');                  //All this to be able to know the itemheight.
  RememberItemHeight:=lsImportedHotDir.ItemHeight;   //It looks like in OwnerDrawFixed, the ItemHeight is reported as 0
  lsImportedHotDir.Clear;                             //So prior to make it in lbOwnerDrawFixed, we remember the normal itemheight
  lsImportedHotDir.Style:=lbOwnerDrawFixed;            //And for this, we needed to fill at leat one item...
end;

procedure Tfrmhotdirimportexport.FormResize(Sender: TObject);
var
  NumberOfColumn:integer;
begin
  NumberOfColumn:=lsImportedHotDir.Width div 300;
  if NumberOfColumn<1 then NumberOfColumn:=1;
  lsImportedHotDir.Columns:=NumberOfColumn;
end;

end.


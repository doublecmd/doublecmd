unit fHotDir;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmHotDir = class(TForm)
    lsHotDir: TListBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnADD: TBitBtn;
    btnDelete: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnADDClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadFromGlob;
    procedure SaveToGlob;
  end;


implementation

uses
  uGlobs, uLng;

procedure TfrmHotDir.LoadFromGlob;
begin
  lsHotDir.Clear;
  lsHotDir.Items.Assign(glsHotDir);
  if lsHotDir.Items.Count > 0 then
    lsHotDir.ItemIndex:= 0;
  btnDelete.Enabled:= (lsHotDir.Items.Count > 0);
end;


procedure TfrmHotDir.SaveToGlob;
begin
  glsHotDir.Assign(lsHotDir.Items);
end;


procedure TfrmHotDir.btnOKClick(Sender: TObject);
begin
  inherited;
  SaveToGlob;
end;

procedure TfrmHotDir.btnDeleteClick(Sender: TObject);
var
  iIndex:Integer;
begin
  inherited;
  if lsHotDir.ItemIndex=-1 then Exit;
  iIndex:=lsHotDir.ItemIndex;
  lsHotDir.Items.Delete(iIndex);
  if (iIndex>=lsHotDir.Items.Count-1) then
    iIndex:=lsHotDir.Items.Count-1;
  lsHotDir.ItemIndex:=iIndex;
  btnDelete.Enabled:= (lsHotDir.Items.Count>0);
end;

procedure TfrmHotDir.btnADDClick(Sender: TObject);
var
  sDir:String;
begin
  inherited;
  if SelectDirectory(rsSelectDir,'',sDir,False) then
    lsHotDir.ItemIndex:=lsHotDir.Items.Add(sDir+DirectorySeparator);
  btnDelete.Enabled:= (lsHotDir.Items.Count>0);
end;

initialization
 {$I fhotdir.lrs}
end.

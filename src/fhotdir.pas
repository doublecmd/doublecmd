unit fHotDir;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  
  { TfrmHotDir }

  TfrmHotDir = class(TForm)
    btnAddMan: TBitBtn;
    btnEdit: TBitBtn;
    lsHotDir: TListBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnADD: TBitBtn;
    btnDelete: TBitBtn;
    procedure btnAddManClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnADDClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lsHotDirMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lsHotDirMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lsHotDirMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    fPivotIndex: integer;
  public
    { Public declarations }
    procedure LoadFromGlob;
    procedure SaveToGlob;
  end;


implementation

uses
  uDCUtils, uGlobs, uLng;

procedure TfrmHotDir.LoadFromGlob;
begin
  lsHotDir.Clear;
  lsHotDir.Items.Assign(glsHotDir);
  if glsHotDir.Count > 0 then lsHotDir.ItemIndex:= 0;
  btnDelete.Enabled:= (lsHotDir.Items.Count > 0);
  btnEdit.Enabled:= (lsHotDir.Items.Count > 0);
end;


procedure TfrmHotDir.SaveToGlob;
begin
  glsHotDir.Assign(lsHotDir.Items);
end;


procedure TfrmHotDir.btnOKClick(Sender: TObject);
begin
  SaveToGlob;
end;

procedure TfrmHotDir.btnAddManClick(Sender: TObject);
var
  sDir: String;
begin
  if InputQuery(rsMsgManualAddHotDir, rsMsgManualHotDirQuery, sDir) then
    lsHotDir.ItemIndex:= lsHotDir.Items.Add(IncludeTrailingBackSlash(sDir));
  btnDelete.Enabled:= (lsHotDir.Items.Count > 0);
  btnEdit.Enabled:= (lsHotDir.Items.Count > 0);
end;

procedure TfrmHotDir.btnEditClick(Sender: TObject);
var
  sDir: String;
begin
  if lsHotDir.Items.Count < 1 then Exit;
  sDir:= lsHotDir.Items[lsHotDir.ItemIndex];
  if InputQuery(rsMsgManualEditHotDir, rsMsgManualHotDirQuery, sDir) then
    lsHotDir.Items[lsHotDir.ItemIndex]:= IncludeTrailingBackSlash(sDir);
  btnDelete.Enabled:= (lsHotDir.Items.Count > 0);
  btnEdit.Enabled:= (lsHotDir.Items.Count > 0);
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
  btnEdit.Enabled:= (lsHotDir.Items.Count>0);
end;

procedure TfrmHotDir.btnADDClick(Sender: TObject);
var
  sName,
  sPath: String;
begin
  if SelectDirectory(rsSelectDir, '', sPath, False) then
    begin
      sName:= GetLastDir(sPath);
      lsHotDir.ItemIndex:= lsHotDir.Items.Add(sName + '=' + IncludeTrailingPathDelimiter(sPath));
    end;
  btnDelete.Enabled:= (lsHotDir.Items.Count > 0);
  btnEdit.Enabled:= (lsHotDir.Items.Count > 0);
end;

procedure TfrmHotDir.FormCreate(Sender: TObject);
begin
  fPivotIndex := -1;
end;

procedure TfrmHotDir.lsHotDirMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fPivotIndex := lsHotDir.GetIndexAtXY(X,Y);
end;

procedure TfrmHotDir.lsHotDirMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewIndex: Integer;
begin
  if (fPivotIndex>=0) and (Y>=0) then
    begin
      NewIndex := lsHotDir.GetIndexAtXY(X,Y);
      if (NewIndex>=0) and (NewIndex<>fPivotIndex) then
        begin
          lsHotDir.Items.Exchange(NewIndex, fPivotIndex);
          if fPivotIndex=lsHotDir.ItemIndex then
            lsHotDir.ItemIndex := NewIndex;
          fPivotIndex := NewIndex;
        end;
    end;
end;

procedure TfrmHotDir.lsHotDirMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fPivotIndex := -1;
end;

initialization
 {$I fhotdir.lrs}
end.

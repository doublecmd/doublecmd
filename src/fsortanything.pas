{
    Double Commander
    -------------------------------------------------------------------------
    Form allowing user to sort a list of element via drag and drop

    Copyright (C) 2020 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fSortAnything;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,

  //DC
  uClassesEx;

type
  { TfrmSortAnything }
  TfrmSortAnything = class(TForm)
    lblSortAnything: TLabel;
    lbSortAnything: TListBox;
    pnlButtons: TPanel;
    btnSort: TBitBtn;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure lbSortAnythingDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure lbSortAnythingDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
  private
    IniPropStorage: TIniPropStorageEx;
  end;

var
  frmSortAnything: TfrmSortAnything;

function HaveUserSortThisList(const sWindowTitle: string; const slListToSort: TStringList): integer;

implementation

{$R *.lfm}


uses
  //Lazarus, Free-Pascal, etc.

  //DC
  uGlobs;

{ TfrmSortAnything }

{ TfrmSortAnything.FormCreate }
procedure TfrmSortAnything.FormCreate(Sender: TObject);
begin
  IniPropStorage := InitPropStorage(Self);
end;

{ TfrmSortAnything.btnSortClick }
procedure TfrmSortAnything.btnSortClick(Sender: TObject);
begin
  lbSortAnything.Sorted := True;
  lbSortAnything.Sorted := False; //Put it back to false so the drag'n drop is still functional after the sort.
end;

{ TfrmSortAnything.lbSortAnythingDragOver }
procedure TfrmSortAnything.lbSortAnythingDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

{ TfrmSortAnything.lbSortAnythingDragDrop }
// Key prodecure here that will let user do the drag and drop in the list to move item in the order he wants.
// Basically we first remove from the list the elements to be moved...
// ... and then we place them back to the correct destination.
// The key thing is to determine where will be this destination location based on current selection and target position.
procedure TfrmSortAnything.lbSortAnythingDragDrop(Sender, Source: TObject; X, Y: integer);
var
  iFirstSelection, iBeforeTarget, iSeeker, iDestIndex: integer;
  bMoveSelectionUp: boolean;
  slBuffer: TStringList;
begin
  iDestIndex := lbSortAnything.GetIndexAtXY(X, Y);

  if (iDestIndex >= 0) and (iDestIndex < lbSortAnything.Items.Count) then //Don't laught, apparently it's possible to get a iDestIndex=-1 if we move totally on top.
  begin
    //1o) Let's determine in which direction the move is taken place with hint about down move.
    iFirstSelection := -1;
    iBeforeTarget := 0;
    iSeeker := 0;
    while (iSeeker < lbSortAnything.Count) do
    begin
      if lbSortAnything.Selected[iSeeker] then
      begin
        if iFirstSelection = -1 then
          iFirstSelection := iSeeker;

        if iSeeker < iDestIndex then
          Inc(iBeforeTarget);
      end;

      Inc(iSeeker);
    end;
    bMoveSelectionUp := (iDestIndex <= iFirstSelection);

    if (iFirstSelection >= 0) then
    begin
      lbSortAnything.Items.BeginUpdate;
      try
        slBuffer := TStringList.Create;
        try
          //2o) Let's remove from the list the element that will be relocated.
          for iSeeker := pred(lbSortAnything.Items.Count) downto 0 do
          begin
            if lbSortAnything.Selected[iSeeker] then
            begin
              slBuffer.Insert(0, lbSortAnything.Items[iSeeker]);
              lbSortAnything.Items.Delete(iSeeker);
            end;
          end;

          //3o) If we're moving down, we need to readjust destination based on elements seen prior the destination.
          if not bMoveSelectionUp then
            iDestIndex := iDestIndex - pred(iBeforeTarget);

          //4o) Putting back elements in the list after move. It could be "inserted" or "added at the end" based on the result of move.
          if iDestIndex < lbSortAnything.Items.Count then
          begin
            for iSeeker := pred(slBuffer.Count) downto 0 do
            begin
              lbSortAnything.Items.Insert(iDestIndex, slBuffer.Strings[iSeeker]);
              lbSortAnything.Selected[iDestIndex] := True;
            end;
          end
          else
          begin
            for iSeeker := 0 to pred(slBuffer.Count) do
            begin
              lbSortAnything.Items.Add(slBuffer.Strings[iSeeker]);
              lbSortAnything.Selected[pred(lbSortAnything.Items.Count)] := True;
            end;
          end;
        finally
          lbSortAnything.Items.EndUpdate;
        end;
      finally
        slBuffer.Free;
      end;
    end;
  end;
end;

{ HaveUserSortThisList }
function HaveUserSortThisList(const sWindowTitle: string; const slListToSort: TStringList): integer;
var
  LocalfrmSortAnything: TfrmSortAnything;
begin
  Result := mrCancel;
  LocalfrmSortAnything := TfrmSortAnything.Create(Application.MainForm);
  try
    LocalfrmSortAnything.Caption := sWindowTitle;
    LocalfrmSortAnything.Icon := Application.MainForm.Icon;
    LocalfrmSortAnything.lbSortAnything.Items.Assign(slListToSort);
    Result := LocalfrmSortAnything.ShowModal;
    if Result = mrOk then
      slListToSort.Assign(LocalfrmSortAnything.lbSortAnything.Items);
  finally
    LocalfrmSortAnything.Free;
  end;
end;

end.


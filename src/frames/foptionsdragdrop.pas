{
   Double Commander
   -------------------------------------------------------------------------
   Drag&drop options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fOptionsDragDrop;

{$mode objfpc}{$H+}

interface

uses
  Controls, Classes, SysUtils, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsDragDrop }

  TfrmOptionsDragDrop = class(TOptionsEditor)
    cbShowConfirmationDialog: TCheckBox;
    cbDragAndDropAskFormatEachTime: TCheckBox;
    cbDragAndDropSaveUnicodeTextInUFT8: TCheckBox;
    cbDragAndDropTextAutoFilename: TCheckBox;
    gbTextDragAndDropRelatedOptions: TGroupBox;
    lblMostDesiredTextFormat1: TLabel;
    lblMostDesiredTextFormat2: TLabel;
    lblWarningForAskFormat: TLabel;
    lbMostDesiredTextFormat: TListBox;
    procedure lbMostDesiredTextFormatDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbMostDesiredTextFormatDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure GenericSomethingChanged(Sender: TObject);
  protected
    FModificationTookPlace: boolean;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure LoadDesiredOrderTextFormatList;
    procedure SaveDesiredOrderTextFormatList;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    function CanWeClose(var WillNeedUpdateWindowView: boolean): boolean; override;
  end;

procedure SortThisListAccordingToDragAndDropDesiredFormat(ListToSort:TStringList);

implementation

{$R *.lfm}

uses
  fOptions, uShowMsg, uGlobs, uLng;

{ TfrmOptionsDragDrop }

procedure TfrmOptionsDragDrop.Load;
begin
  cbShowConfirmationDialog.Checked := gShowDialogOnDragDrop;
  {$IFDEF MSWINDOWS}
  gbTextDragAndDropRelatedOptions.Visible:=TRUE;
  LoadDesiredOrderTextFormatList;
  cbDragAndDropAskFormatEachTime.Checked := gDragAndDropAskFormatEachTime;
  cbDragAndDropTextAutoFilename.Checked := gDragAndDropTextAutoFilename;
  cbDragAndDropSaveUnicodeTextInUFT8.Checked := gDragAndDropSaveUnicodeTextInUFT8;
  {$ENDIF}
  FModificationTookPlace := False;
end;

function TfrmOptionsDragDrop.Save: TOptionsEditorSaveFlags;
begin
  gShowDialogOnDragDrop := cbShowConfirmationDialog.Checked;
  {$IFDEF MSWINDOWS}
  SaveDesiredOrderTextFormatList;
  gDragAndDropAskFormatEachTime := cbDragAndDropAskFormatEachTime.Checked;
  gDragAndDropTextAutoFilename := cbDragAndDropTextAutoFilename.Checked;
  gDragAndDropSaveUnicodeTextInUFT8 := cbDragAndDropSaveUnicodeTextInUFT8.Checked;
  {$ENDIF}
  FModificationTookPlace := False;
  Result := [];
end;

class function TfrmOptionsDragDrop.GetIconIndex: Integer;
begin
  Result := 28;
end;

class function TfrmOptionsDragDrop.GetTitle: String;
begin
  Result := rsOptionsEditorDragAndDrop;
end;

procedure TfrmOptionsDragDrop.lbMostDesiredTextFormatDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbMostDesiredTextFormat) and (lbMostDesiredTextFormat.ItemIndex <> -1);
end;

procedure TfrmOptionsDragDrop.lbMostDesiredTextFormatDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SrcIndex, DestIndex: Integer;
begin
  SrcIndex := lbMostDesiredTextFormat.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbMostDesiredTextFormat.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lbMostDesiredTextFormat.Count) then
    DestIndex := lbMostDesiredTextFormat.Count - 1;

  lbMostDesiredTextFormat.Items.Move(SrcIndex, DestIndex);
  lbMostDesiredTextFormat.ItemIndex := DestIndex;
  FModificationTookPlace := True;
end;

procedure TfrmOptionsDragDrop.GenericSomethingChanged(Sender: TObject);
begin
  FModificationTookPlace := True;
end;

procedure TfrmOptionsDragDrop.LoadDesiredOrderTextFormatList;
var
  IndexDropTextFormat, ExpectedPosition, ActualPosition:integer;
  TempoString: string;
begin
  lbMostDesiredTextFormat.Clear;
  for IndexDropTextFormat:=0 to pred(NbOfDropTextFormat) do
    lbMostDesiredTextFormat.Items.Add(gDragAndDropDesiredTextFormat[IndexDropTextFormat].Name);

  for IndexDropTextFormat:=0 to pred(NbOfDropTextFormat) do
    begin
      ExpectedPosition:=gDragAndDropDesiredTextFormat[IndexDropTextFormat].DesireLevel;
      if (ExpectedPosition<0) OR (ExpectedPosition>pred(NbOfDropTextFormat)) then  ExpectedPosition:=pred(NbOfDropTextFormat);
      ActualPosition:=lbMostDesiredTextFormat.Items.IndexOf(gDragAndDropDesiredTextFormat[IndexDropTextFormat].Name);
      if (ActualPosition<>-1) AND (ExpectedPosition<>ActualPosition) then
      begin
        TempoString:=lbMostDesiredTextFormat.Items.Strings[ActualPosition];
        lbMostDesiredTextFormat.Items.Strings[ActualPosition]:=lbMostDesiredTextFormat.Items.Strings[ExpectedPosition];
        lbMostDesiredTextFormat.Items.Strings[ExpectedPosition]:=TempoString;
      end;
    end;
end;

procedure TfrmOptionsDragDrop.SaveDesiredOrderTextFormatList;
var
  IndexDropTextFormat, ActualPosition:integer;
  TempoString: string;
begin
  for IndexDropTextFormat:=0 to pred(NbOfDropTextFormat) do
  begin
    ActualPosition:=lbMostDesiredTextFormat.Items.IndexOf(gDragAndDropDesiredTextFormat[IndexDropTextFormat].Name);
    if (ActualPosition<>-1) then gDragAndDropDesiredTextFormat[IndexDropTextFormat].DesireLevel:=ActualPosition;
  end;
end;

function TfrmOptionsDragDrop.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := not FModificationTookPlace;

  if not Result then
  begin
    ShowOptions(TfrmOptionsDragDrop);
    Answer := MsgBox(rsMsgDragAndDropModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
    case Answer of
      mmrYes:
      begin
        Save;
        Result := True;
      end;

      mmrNo: Result := True;
      else
        Result := False;
    end;
  end;
end;


// Arrange the list in such way that the most desired format is on top.
// This routine is also used in "uOleDragDrop" for offering user's suggestion so the list is arranged according to user's desire
procedure SortThisListAccordingToDragAndDropDesiredFormat(ListToSort:TStringList);
  function GetDesireLevel(SearchingText:string):integer;
  var
    SearchingIndex:integer;
  begin
    result:=-1;
    SearchingIndex:=0;
    while (SearchingIndex<NbOfDropTextFormat) AND (result=-1) do
     begin
       if gDragAndDropDesiredTextFormat[SearchingIndex].Name=SearchingText then result:=gDragAndDropDesiredTextFormat[SearchingIndex].DesireLevel;
       inc(SearchingIndex);
     end;
  end;
var
  Index, InnerIndex, DesireLevelIndex, DesireLevelInnerIndex : integer;
  TempoString : string;
begin
  //It's a poor sort... But we don't have too many so we keep it simple.
  for Index:=0 to (ListToSort.Count-2) do
  begin
    for InnerIndex:=Index+1 to pred(ListToSort.Count) do
    begin
      DesireLevelIndex:=GetDesireLevel(ListToSort.Strings[Index]);
      DesireLevelInnerIndex:=GetDesireLevel(ListToSort.Strings[InnerIndex]);

      if (DesireLevelIndex>DesireLevelInnerIndex) AND (DesireLevelIndex<>-1) AND (DesireLevelInnerIndex<>-1) then
      begin
        TempoString:=ListToSort.Strings[Index];
        ListToSort.Strings[Index]:=ListToSort.Strings[InnerIndex];
        ListToSort.Strings[InnerIndex]:=TempoString;
      end;
    end;
  end;
end;

end.


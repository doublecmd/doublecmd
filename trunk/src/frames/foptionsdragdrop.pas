{
   Double Commander
   -------------------------------------------------------------------------
   Drag&drop options page

   Copyright (C) 2006-2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit fOptionsDragDrop;

{$mode objfpc}{$H+}

interface

uses
  Controls, Classes, SysUtils, StdCtrls,
  fOptionsFrame, Types;

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
    procedure lbMostDesiredTextFormatDragOver(Sender, Source: TObject; {%H-}X,
    {%H-}Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure lbMostDesiredTextFormatDragDrop(Sender, {%H-}Source: TObject; {%H-}X,
      Y: integer);
  protected
    slUserLanguageName, slLegacyName: TStringList;
    procedure Init; override;
    procedure Done; override;
    procedure Load; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
    function Save: TOptionsEditorSaveFlags; override;
    function GetUserNameFromLegacyName(sLegacyName: string): string;
    procedure LoadDesiredOrderTextFormatList;
    procedure SaveDesiredOrderTextFormatList;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
  end;

procedure SortThisListAccordingToDragAndDropDesiredFormat(ListToSort: TStringList);

implementation

{$R *.lfm}

uses
  DCStrUtils, crc, uGlobs, uLng;

{ TfrmOptionsDragDrop }

{ TfrmOptionsDragDrop.Init }
procedure TfrmOptionsDragDrop.Init;
var
  iFormat: integer;
begin
  slUserLanguageName := TStringList.Create;
  ParseLineToList(rsDragAndDropTextFormat, slUserLanguageName);

  slLegacyName := TStringList.Create;
  for iFormat := 0 to pred(NbOfDropTextFormat) do
    slLegacyName.Add(gDragAndDropDesiredTextFormat[iFormat].Name);
end;

{ TfrmOptionsDragDrop.Done }
procedure TfrmOptionsDragDrop.Done;
begin
  FreeAndNil(slUserLanguageName);
  FreeAndNil(slLegacyName);
end;

{ TfrmOptionsDragDrop.GetUserNameFromLegacyName }
function TfrmOptionsDragDrop.GetUserNameFromLegacyName(sLegacyName: string): string;
var
  iPos: integer;
begin
  Result := '???';
  iPos := slLegacyName.indexof(sLegacyName);
  if (iPos >= 0) and (iPos < NbOfDropTextFormat) then
    Result := slUserLanguageName.Strings[iPos];
end;

{ TfrmOptionsDragDrop.Load }
procedure TfrmOptionsDragDrop.Load;
begin
  cbShowConfirmationDialog.Checked := gShowDialogOnDragDrop;
  {$IFDEF MSWINDOWS}
  gbTextDragAndDropRelatedOptions.Visible := True;
  LoadDesiredOrderTextFormatList;
  cbDragAndDropAskFormatEachTime.Checked := gDragAndDropAskFormatEachTime;
  cbDragAndDropTextAutoFilename.Checked := gDragAndDropTextAutoFilename;
  cbDragAndDropSaveUnicodeTextInUFT8.Checked := gDragAndDropSaveUnicodeTextInUFT8;
  {$ENDIF}
end;

{ TfrmOptionsDragDrop.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsDragDrop.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  lbMostDesiredTextFormat.ItemIndex := -1; // Tricky pass but nothing was selected when we initially did the signature so let's unselect them all.
  Result := True;
end;

{ TfrmOptionsDragDrop.ExtraOptionsSignature }
function TfrmOptionsDragDrop.ExtraOptionsSignature(CurrentSignature: dword): dword;
var
  iFormat: integer;
begin
  Result := CurrentSignature;
  for iFormat := 0 to pred(lbMostDesiredTextFormat.Items.Count) do
    Result := crc32(Result, @lbMostDesiredTextFormat.Items.Strings[iFormat][1], length(lbMostDesiredTextFormat.Items.Strings[iFormat]));
end;

{ TfrmOptionsDragDrop.Save }
function TfrmOptionsDragDrop.Save: TOptionsEditorSaveFlags;
begin
  gShowDialogOnDragDrop := cbShowConfirmationDialog.Checked;
  {$IFDEF MSWINDOWS}
  SaveDesiredOrderTextFormatList;
  gDragAndDropAskFormatEachTime := cbDragAndDropAskFormatEachTime.Checked;
  gDragAndDropTextAutoFilename := cbDragAndDropTextAutoFilename.Checked;
  gDragAndDropSaveUnicodeTextInUFT8 := cbDragAndDropSaveUnicodeTextInUFT8.Checked;
  {$ENDIF}
  Result := [];
end;

class function TfrmOptionsDragDrop.GetIconIndex: integer;
begin
  Result := 28;
end;

class function TfrmOptionsDragDrop.GetTitle: string;
begin
  Result := rsOptionsEditorDragAndDrop;
end;

procedure TfrmOptionsDragDrop.lbMostDesiredTextFormatDragOver(Sender,
  Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := (Source = lbMostDesiredTextFormat) and (lbMostDesiredTextFormat.ItemIndex <> -1);
end;

procedure TfrmOptionsDragDrop.lbMostDesiredTextFormatDragDrop(Sender, Source: TObject; X, Y: integer);
var
  SrcIndex, DestIndex: integer;
begin
  SrcIndex := lbMostDesiredTextFormat.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbMostDesiredTextFormat.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lbMostDesiredTextFormat.Count) then
    DestIndex := lbMostDesiredTextFormat.Count - 1;

  lbMostDesiredTextFormat.Items.Move(SrcIndex, DestIndex);
  lbMostDesiredTextFormat.ItemIndex := DestIndex;
end;

{ TfrmOptionsDragDrop.LoadDesiredOrderTextFormatList }
procedure TfrmOptionsDragDrop.LoadDesiredOrderTextFormatList;
var
  IndexDropTextFormat, ExpectedPosition, ActualPosition: integer;
  TempoString: string;
begin
  lbMostDesiredTextFormat.Clear;
  for IndexDropTextFormat := 0 to pred(NbOfDropTextFormat) do
    lbMostDesiredTextFormat.Items.Add(gDragAndDropDesiredTextFormat[IndexDropTextFormat].Name);

  for IndexDropTextFormat := 0 to pred(NbOfDropTextFormat) do
  begin
    ExpectedPosition := gDragAndDropDesiredTextFormat[IndexDropTextFormat].DesireLevel;
    if (ExpectedPosition < 0) or (ExpectedPosition > pred(NbOfDropTextFormat)) then  ExpectedPosition := pred(NbOfDropTextFormat);
    ActualPosition := lbMostDesiredTextFormat.Items.IndexOf(gDragAndDropDesiredTextFormat[IndexDropTextFormat].Name);
    if (ActualPosition <> -1) and (ExpectedPosition <> ActualPosition) then
    begin
      TempoString := lbMostDesiredTextFormat.Items.Strings[ActualPosition];
      lbMostDesiredTextFormat.Items.Strings[ActualPosition] := lbMostDesiredTextFormat.Items.Strings[ExpectedPosition];
      lbMostDesiredTextFormat.Items.Strings[ExpectedPosition] := TempoString;
    end;
  end;

  // At the last minutes, we translate to user's language the format names
  for ActualPosition := 0 to pred(lbMostDesiredTextFormat.Items.Count) do
    lbMostDesiredTextFormat.Items.Strings[ActualPosition] := GetUserNameFromLegacyName(lbMostDesiredTextFormat.Items.Strings[ActualPosition]);

  lbMostDesiredTextFormat.ItemIndex := -1;
end;

procedure TfrmOptionsDragDrop.SaveDesiredOrderTextFormatList;
var
  IndexDropTextFormat, ActualPosition: integer;
begin
  for IndexDropTextFormat := 0 to pred(NbOfDropTextFormat) do
  begin
    ActualPosition := lbMostDesiredTextFormat.Items.IndexOf(GetUserNameFromLegacyName(gDragAndDropDesiredTextFormat[IndexDropTextFormat].Name));
    if (ActualPosition <> -1) then gDragAndDropDesiredTextFormat[IndexDropTextFormat].DesireLevel := ActualPosition;
  end;
end;

// Arrange the list in such way that the most desired format is on top.
// This routine is also used in "uOleDragDrop" for offering user's suggestion so the list is arranged according to user's desire
procedure SortThisListAccordingToDragAndDropDesiredFormat(ListToSort: TStringList);
  function GetDesireLevel(SearchingText: string): integer;
  var
    SearchingIndex: integer;
  begin
    Result := -1;
    SearchingIndex := 0;
    while (SearchingIndex < NbOfDropTextFormat) and (Result = -1) do
    begin
      if gDragAndDropDesiredTextFormat[SearchingIndex].Name = SearchingText then Result := gDragAndDropDesiredTextFormat[SearchingIndex].DesireLevel;
      Inc(SearchingIndex);
    end;
  end;
var
  Index, InnerIndex, DesireLevelIndex, DesireLevelInnerIndex: integer;
  TempoString: string;
begin
  //It's a poor sort... But we don't have too many so we keep it simple.
  for Index := 0 to (ListToSort.Count - 2) do
  begin
    for InnerIndex := Index + 1 to pred(ListToSort.Count) do
    begin
      DesireLevelIndex := GetDesireLevel(ListToSort.Strings[Index]);
      DesireLevelInnerIndex := GetDesireLevel(ListToSort.Strings[InnerIndex]);

      if (DesireLevelIndex > DesireLevelInnerIndex) and (DesireLevelIndex <> -1) and (DesireLevelInnerIndex <> -1) then
      begin
        TempoString := ListToSort.Strings[Index];
        ListToSort.Strings[Index] := ListToSort.Strings[InnerIndex];
        ListToSort.Strings[InnerIndex] := TempoString;
      end;
    end;
  end;
end;

end.


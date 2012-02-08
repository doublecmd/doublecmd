{
   Double Commander Components
   -------------------------------------------------------------------------
   Path edit class with auto complete feature

   Copyright (C) 2012  Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit KASPathEdit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ShellCtrls, LCLType;

type

  { TKASPathEdit }

  TKASPathEdit = class(TEdit)
  private
    FListBox: TListBox;
    FObjectTypes: TObjectTypes;
    FFileSortType: TFileSortType;
  private
    function GetAnchorControl: TControl;
    procedure SetAnchorControl(AControl: TControl);
    procedure AutoComplete(const Path: UTF8String);
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUpAfterInterface(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ListBox: TListBox read FListBox;
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property FileSortType: TFileSortType read FFileSortType write FFileSortType;
    property AnchorControl: TControl read GetAnchorControl write SetAnchorControl;
  end;

procedure Register;

implementation

uses
  LCLProc, Math;

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASPathEdit]);
end;

{ TKASPathEdit }

function TKASPathEdit.GetAnchorControl: TControl;
begin
  Result:= FListBox.AnchorSide[akTop].Control;
end;

procedure TKASPathEdit.SetAnchorControl(AControl: TControl);
begin
  if (AControl.Parent <> nil) then
  begin
    FListBox.Parent := AControl.Parent;
    FListBox.Anchors := [akTop, akLeft, akRight];
    FListBox.AnchorSide[akTop].Control := AControl;
    FListBox.AnchorSide[akTop].Side := asrBottom;
    FListBox.AnchorSide[akLeft].Control := AControl;
    FListBox.AnchorSide[akLeft].Side := asrLeft;
    FListBox.AnchorSide[akRight].Control := AControl;
    FListBox.AnchorSide[akRight].Side := asrRight;
  end;
end;

procedure TKASPathEdit.AutoComplete(const Path: UTF8String);
var
  I: LongWord;
  BasePath: UTF8String;
begin
  FListBox.Clear;
  if Pos(PathDelim, Path) > 0 then
  begin
    BasePath:= ExtractFilePath(Path);
    TCustomShellTreeView.GetFilesInDir(
                                       BasePath,
                                       ExtractFileName(Path) + '*',
                                       FObjectTypes,
                                       FListBox.Items,
                                       FFileSortType
                                       );
    if (FListBox.Items.Count > 0) then
    begin
      for I := 0 to FListBox.Items.Count - 1 do
      FListBox.Items[I] := BasePath + FListBox.Items[I];
    end;
  end;
  FListBox.Visible:= FListBox.Items.Count > 0;
end;

procedure TKASPathEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  SetAnchorControl(Self);
end;

procedure TKASPathEdit.DoExit;
begin
  FListBox.Visible:= False;
  inherited DoExit;
end;

procedure TKASPathEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE,
    VK_RETURN,
    VK_SELECT:
      begin
        FListBox.Visible := False;
      end;
    VK_UP:
      if FListBox.Visible then
      begin
        Key := 0;
        if FListBox.ItemIndex = -1 then
          FListBox.ItemIndex := FListBox.Items.Count - 1
        else if FListBox.ItemIndex - 1 < 0 then
          FListBox.ItemIndex := - 1
        else
          FListBox.ItemIndex := FListBox.ItemIndex - 1;

        if FListBox.ItemIndex >= 0 then
          Text := FListBox.Items[FListBox.ItemIndex]
        else
          Text := ExtractFilePath(Text);
        SelStart := UTF8Length(Text);
      end;
    VK_DOWN:
      if FListBox.Visible then
      begin
        Key := 0;
        if FListBox.ItemIndex + 1 >= FListBox.Items.Count then
          FListBox.ItemIndex := -1
        else if FListBox.ItemIndex = -1 then
          FListBox.ItemIndex := IfThen(FListBox.Items.Count > 0, 0, -1)
        else
          FListBox.ItemIndex := FListBox.ItemIndex + 1;

        if FListBox.ItemIndex >= 0 then
          Text := FListBox.Items[FListBox.ItemIndex]
        else
          Text := ExtractFilePath(Text);
        SelStart := UTF8Length(Text);
      end;
  end;
  inherited KeyDown(Key, Shift);
  {$IFDEF LCLGTK2}
  // Workaround for GTK2 - up and down arrows moving through controls.
  if Key in [VK_UP, VK_DOWN] then Key := 0;
  {$ENDIF}
end;

procedure TKASPathEdit.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  if not (Key in [VK_ESCAPE, VK_RETURN, VK_SELECT, VK_UP, VK_DOWN]) then
    AutoComplete(Text);
  inherited KeyDownAfterInterface(Key, Shift);
end;

constructor TKASPathEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FListBox := TListBox.Create(Self);
  FListBox.Visible := False;
  FListBox.TabStop := False;

  FFileSortType := fstFoldersFirst;
  FObjectTypes  := [otNonFolders, otFolders];
end;

end.

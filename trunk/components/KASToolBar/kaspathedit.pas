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
    FPanel: THintWindow;
    FListBox: TListBox;
    FKeyDown: Word;
    FAutoComplete: Boolean;
    FObjectTypes: TObjectTypes;
    FFileSortType: TFileSortType;
  private
    procedure AutoComplete(const Path: UTF8String);
    procedure SetObjectTypes(const AValue: TObjectTypes);
    procedure FormChangeBoundsEvent(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ShowListBox;
    procedure HideListBox;
  protected
{$IF DEFINED(LCLWIN32)}
    procedure CreateWnd; override;
{$ENDIF}
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUpAfterInterface(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes;
    property FileSortType: TFileSortType read FFileSortType write FFileSortType;
  end;

procedure Register;

implementation

uses
  LCLProc, Math
  {$IF DEFINED(LCLWIN32)}
  , ComObj
  {$ENDIF}
  ;

{$IF DEFINED(LCLWIN32)}

const
  SHACF_AUTOAPPEND_FORCE_ON  = $40000000;
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;
  SHACF_FILESYS_ONLY         = $00000010;
  SHACF_FILESYS_DIRS         = $00000020;

function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HRESULT; stdcall; external 'shlwapi.dll';

function SHAutoCompleteX(hwndEdit: HWND; ObjectTypes: TObjectTypes): Boolean;
var
  dwFlags: DWORD;
begin
  if (ObjectTypes = []) then Exit(False);
  dwFlags := SHACF_AUTOAPPEND_FORCE_ON or SHACF_AUTOSUGGEST_FORCE_ON;
  if (otNonFolders in ObjectTypes) then
    dwFlags := dwFlags or SHACF_FILESYS_ONLY
  else if (otFolders in ObjectTypes) then
    dwFlags := dwFlags or SHACF_FILESYS_DIRS;
  Result:= (SHAutoComplete(hwndEdit, dwFlags) = 0);
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASPathEdit]);
end;

{ TKASPathEdit }

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
      ShowListBox;
      // Make absolute file name
      for I:= 0 to FListBox.Items.Count - 1 do
      FListBox.Items[I]:= BasePath + FListBox.Items[I];
      // Calculate ListBox height
      with FListBox.ItemRect(0) do
      I:= Bottom - Top; // TListBox.ItemHeight sometimes don't work under GTK2
      with FListBox do
      begin
        if Items.Count = 1 then
          FPanel.ClientHeight:= Self.Height
        else
          FPanel.ClientHeight:= I * IfThen(Items.Count > 10, 11, Items.Count + 1);
      end;
    end;
  end;
  if (FListBox.Items.Count = 0) then HideListBox;
end;

procedure TKASPathEdit.SetObjectTypes(const AValue: TObjectTypes);
begin
  if FObjectTypes = AValue then Exit;
  FObjectTypes:= AValue;
{$IF DEFINED(LCLWIN32)}
  if HandleAllocated then RecreateWnd(Self);
  if FAutoComplete then
{$ENDIF}
  FAutoComplete:= (FObjectTypes <> []);
end;

procedure TKASPathEdit.FormChangeBoundsEvent(Sender: TObject);
begin
  HideListBox;
end;

procedure TKASPathEdit.ListBoxClick(Sender: TObject);
begin
  if FListBox.ItemIndex >= 0 then
  begin
    Text:= FListBox.Items[FListBox.ItemIndex];
    SelStart:= UTF8Length(Text);
    HideListBox;
    SetFocus;
  end;
end;

procedure TKASPathEdit.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FListBox.ItemIndex:= FListBox.ItemAtPos(Classes.Point(X, Y), True);
end;

procedure TKASPathEdit.ShowListBox;
begin
  if (FPanel = nil) then
  begin
    FPanel:= THintWindow.Create(Self);
    FPanel.Color:= clForm;
    FListBox.Parent:= FPanel;

    with Parent.ClientToScreen(CLasses.Point(Left, Top)) do
    begin
      FPanel.Left:= X;
      FPanel.Top:= Y + Height;
    end;

    FPanel.Width:= Width;
    FPanel.Visible:= True;

    GetParentForm(Self).AddHandlerOnChangeBounds(FormChangeBoundsEvent, True);
  end;
end;

procedure TKASPathEdit.HideListBox;
begin
  if (FPanel <> nil) then
  begin
    FPanel.Visible:= False;
    FListBox.Parent:= nil;
    FreeAndNil(FPanel);
    GetParentForm(Self).RemoveHandlerOnChangeBounds(FormChangeBoundsEvent);
  end;
end;

{$IF DEFINED(LCLWIN32)}

procedure TKASPathEdit.CreateWnd;
begin
  inherited CreateWnd;
  FAutoComplete:= not SHAutoCompleteX(Handle, FObjectTypes);
end;

{$ENDIF}

procedure TKASPathEdit.DoExit;
begin
  HideListBox;
  inherited DoExit;
end;

procedure TKASPathEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FKeyDown:= Key;
  case Key of
    VK_ESCAPE,
    VK_RETURN,
    VK_SELECT:
      begin
        HideListBox;
      end;
    VK_UP:
      if Assigned(FPanel) then
      begin
        Key:= 0;
        if FListBox.ItemIndex = -1 then
          FListBox.ItemIndex:= FListBox.Items.Count - 1
        else if FListBox.ItemIndex - 1 < 0 then
          FListBox.ItemIndex:= - 1
        else
          FListBox.ItemIndex:= FListBox.ItemIndex - 1;

        if FListBox.ItemIndex >= 0 then
          Text:= FListBox.Items[FListBox.ItemIndex]
        else
          Text:= ExtractFilePath(Text);
        SelStart:= UTF8Length(Text);
      end;
    VK_DOWN:
      if Assigned(FPanel) then
      begin
        Key:= 0;
        if FListBox.ItemIndex + 1 >= FListBox.Items.Count then
          FListBox.ItemIndex:= -1
        else if FListBox.ItemIndex = -1 then
          FListBox.ItemIndex:= IfThen(FListBox.Items.Count > 0, 0, -1)
        else
          FListBox.ItemIndex:= FListBox.ItemIndex + 1;

        if FListBox.ItemIndex >= 0 then
          Text:= FListBox.Items[FListBox.ItemIndex]
        else
          Text:= ExtractFilePath(Text);
        SelStart:= UTF8Length(Text);
      end;
  end;
  inherited KeyDown(Key, Shift);
{$IFDEF LCLGTK2}
  // Workaround for GTK2 - up and down arrows moving through controls.
  if Key in [VK_UP, VK_DOWN] then Key:= 0;
{$ENDIF}
end;

procedure TKASPathEdit.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  if (FKeyDown = Key) and FAutoComplete and not (Key in [VK_ESCAPE, VK_RETURN, VK_SELECT, VK_UP, VK_DOWN]) then
    AutoComplete(Text);
  inherited KeyUpAfterInterface(Key, Shift);
end;

constructor TKASPathEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FListBox:= TListBox.Create(Self);
  FListBox.TabStop:= False;
  FListBox.Align:= alClient;
  FListBox.ClickOnSelChange:= False;
  FListBox.OnClick:= ListBoxClick;
  FListBox.OnMouseMove:= ListBoxMouseMove;

  FAutoComplete:= True;
  FFileSortType:= fstFoldersFirst;
  FObjectTypes:= [otNonFolders, otFolders];
end;

end.

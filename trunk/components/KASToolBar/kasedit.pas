{
   File name: kasedit.pas

   Author:    Koblov Alexander (Alexx2000@mail.ru)

   Edit box for Linux with popup menu

   Copyright (C) 2006
   
   contributors:
   
  

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


unit KASEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus;

type

  { TKASEdit }

  TKASEdit = class(TEdit)
  private
    { Private declarations }
    fUndoText,
    fCutText,
    fCopyText,
    fPasteText,
    fDeleteText,
    fSelectAllText,
    fOldText : String;
    fChange : boolean;
    procedure TextUndo(Sender: TObject);
    procedure TextCut(Sender: TObject);
    procedure TextCopy(Sender: TObject);
    procedure TextPaste(Sender: TObject);
    procedure TextDelete(Sender: TObject);
    procedure TextSelectAll(Sender: TObject);
    procedure OnPopupMenu(Sender: TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
     constructor Create(TheOwner: TComponent); override;
     destructor Destroy; override;
     procedure CreateWnd; override;
  published
    { Published declarations }
    property UndoText : String read fUndoText write fUndoText;
    property CutText : String read fCutText write fCutText;
    property CopyText : String read fCopyText write fCopyText;
    property PasteText : String read fPasteText write fPasteText;
    property DeleteText : String read fDeleteText write fDeleteText;
    property SelectAllText : String read fSelectAllText write fSelectAllText;
    
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASEdit]);
end;

{ TKASEdit }

procedure TKASEdit.TextUndo(Sender: TObject);
begin
  if fChange then
     Text := fOldText;
end;

procedure TKASEdit.TextCut(Sender: TObject);
begin
fChange := true;
fOldText := Text;
CutToClipboard;
end;

procedure TKASEdit.TextCopy(Sender: TObject);
begin
CopyToClipboard;
end;

procedure TKASEdit.TextPaste(Sender: TObject);
begin
fChange := true;
fOldText := Text;
PasteFromClipboard;
end;

procedure TKASEdit.TextDelete(Sender: TObject);
begin
fChange := true;
fOldText := Text;
ClearSelection;
end;

procedure TKASEdit.TextSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TKASEdit.OnPopupMenu(Sender: TObject);
begin
   if SelLength = 0 then
      begin
      PopUpMenu.Items.Items[1].Enabled := false;
      PopUpMenu.Items.Items[2].Enabled := false;
      PopUpMenu.Items.Items[4].Enabled := false;
      end
   else
      begin
      PopUpMenu.Items.Items[1].Enabled := true;
      PopUpMenu.Items.Items[2].Enabled := true;
      PopUpMenu.Items.Items[4].Enabled := true;
      end;
   if fChange then
      PopUpMenu.Items.Items[0].Enabled := true
   else
      PopUpMenu.Items.Items[0].Enabled := false;

end;

constructor TKASEdit.Create(TheOwner: TComponent);
var
MenuItem : TMenuItem;
begin
  inherited Create(TheOwner);
  fChange := false;
  PopUpMenu := TPopUpMenu.Create(nil);
  PopUpMenu.OnPopup := @OnPopupMenu;
  PopUpMenu.AutoPopup := true;
  (*Undo Text*)
  MenuItem := TMenuItem.Create(PopUpMenu);
  //MenuItem.ShortCut := $405A; //Ctrl+Z
  MenuItem.OnClick := @TextUndo;
  PopUpMenu.Items.Add(MenuItem);
  (*Cut Text*)
  MenuItem := TMenuItem.Create(PopUpMenu);
  //MenuItem.ShortCut := $4058; //Ctrl+X
  MenuItem.OnClick := @TextCut;
  PopUpMenu.Items.Add(MenuItem);
  (*Copy Text*)
  MenuItem := TMenuItem.Create(PopUpMenu);
  //MenuItem.ShortCut := $4043; //Ctrl+C
  MenuItem.OnClick := @TextCopy;
  PopUpMenu.Items.Add(MenuItem);
  (*Paste Text*)
  MenuItem := TMenuItem.Create(PopUpMenu);
  //MenuItem.ShortCut := $4056; //Ctrl+V
  MenuItem.OnClick := @TextPaste;
  PopUpMenu.Items.Add(MenuItem);
  (*Delete Text*)
  MenuItem := TMenuItem.Create(PopUpMenu);
  MenuItem.OnClick := @TextDelete;
  PopUpMenu.Items.Add(MenuItem);
  (*Select All Text*)
  MenuItem := TMenuItem.Create(PopUpMenu);
  //MenuItem.ShortCut := $4041; //Ctrl+A
  MenuItem.OnClick := @TextSelectAll;
  PopUpMenu.Items.Add(MenuItem);
end;

destructor TKASEdit.Destroy;
var
I : byte;
begin
try
  for I := 0 to PopupMenu.Items.Count - 1 do
      PopUpMenu.Items.Items[0].Free;

  PopUpMenu.Items.Free;
finally
  inherited Destroy;
end;
end;

procedure TKASEdit.CreateWnd;
begin
  inherited CreateWnd;
   (*Caption Undo*)
   if fUndoText <> '' then
     PopUpMenu.Items.Items[0].Caption := fUndoText
   else
   PopUpMenu.Items.Items[0].Caption := 'Undo';
   
   (*Caption Cut*)
   if fCutText <> '' then
      PopUpMenu.Items.Items[1].Caption := fCutText
   else
      PopUpMenu.Items.Items[1].Caption := 'Cut';
      
   (*Caption Copy*)
   if fCopyText <> '' then
      PopUpMenu.Items.Items[2].Caption := fCopyText
   else
      PopUpMenu.Items.Items[2].Caption := 'Copy';
      
   (*Caption Paste*)
   if fPasteText <> '' then
      PopUpMenu.Items.Items[3].Caption := fPasteText
   else
      PopUpMenu.Items.Items[3].Caption := 'Paste';
      
   (*Caption Delete*)
   if fDeleteText <> '' then
      PopUpMenu.Items.Items[4].Caption := fDeleteText
   else
      PopUpMenu.Items.Items[4].Caption := 'Delete';
      
   (*Caption Select All*)
   if fSelectAllText <> '' then
      PopUpMenu.Items.Items[5].Caption := fSelectAllText
   else
      PopUpMenu.Items.Items[5].Caption := 'SelectAll';
end;

end.

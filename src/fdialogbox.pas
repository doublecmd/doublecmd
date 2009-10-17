{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains realization of Dialog API functions.

    Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit fDialogBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Types, DialogAPI;

type

  { TDialogBox }

  TDialogBox = class(TForm)
    DialogButton: TButton;
    DialogComboBox: TComboBox;
    DialogListBox: TListBox;
    DialogCheckBox: TCheckBox;    
    DialogGroupBox: TGroupBox;
    DialogLabel: TLabel;
    DialogEdit: TEdit;
    // Dialog events
    procedure DialogBoxShow(Sender: TObject);
    // Button events
    procedure ButtonClick(Sender: TObject);
    procedure ButtonEnter(Sender: TObject);
    procedure ButtonExit(Sender: TObject);
    procedure ButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // ComboBox events
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBoxDblClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxEnter(Sender: TObject);
    procedure ComboBoxExit(Sender: TObject);
    procedure ComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // Edit events
    procedure EditClick(Sender: TObject);
    procedure EditDblClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // ListBox events
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ListBoxEnter(Sender: TObject);
    procedure ListBoxExit(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fDlgProc: TDlgProc;
  public
    { public declarations }
  end; 

function InputBox(Caption, Prompt, DefaultText : PWideChar): PWideChar;stdcall;
function MessageBox(Text, Caption: PWideChar; Flags: Longint): Integer;stdcall;
function DialogBox(DlgData: PWideChar; DlgProc: TDlgProc): PtrUInt;stdcall;
function DialogBoxEx(lfmFileName: PWideChar; DlgProc: TDlgProc): PtrUInt;stdcall;
function SendDlgMsg(pDlg: PtrUInt; DlgItemName: PChar; Msg, wParam, lParam: PtrInt): PtrInt; stdcall;

implementation

uses
  uClassesEx;

function InputBox(Caption, Prompt, DefaultText: PWideChar): PWideChar;stdcall;
var
  sCaption,
  sPrompt,
  sDefaultText: UTF8String;
  wResult: WideString;
begin
  sCaption:= UTF8Encode(Caption);
  sPrompt:= UTF8Encode(Prompt);
  sDefaultText:= UTF8Encode(DefaultText);
  wResult:= Dialogs.InputBox(sCaption, sPrompt, sDefaultText);
  Result:= PWideChar(UTF8Decode(wResult));
end;

function MessageBox(Text, Caption: PWideChar; Flags: Longint): Integer;stdcall;
var
  sText,
  sCaption: String;
begin
  sText:= UTF8Encode(Text);
  sCaption:= UTF8Encode(Caption);
  Result:= Application.MessageBox(PChar(sText), PChar(sCaption), Flags);
end;

function DialogBox(DlgData: PWideChar; DlgProc: TDlgProc): PtrUInt;stdcall;
var
  DataString: UTF8String;
  LFMStream: TStringStream = nil;
  BinStream: TStringStream = nil;
  Dialog: TDialogBox = nil;
begin
  try
    DataString:= UTF8Encode(DlgData);

    LFMStream:= TStringStream.Create(DataString);
    BinStream:= TStringStream.Create('');
    LRSObjectTextToBinary(LFMStream, BinStream);

    LazarusResources.Add('TDialogBox','FORMDATA', BinStream.DataString);

    Dialog:= TDialogBox.Create(nil);
    with Dialog do
    begin
      fDlgProc:= DlgProc;
      ShowModal;
    end;
    Result:= PtrUInt(Dialog.DialogButton);

  finally
    if Assigned(Dialog) then
      FreeAndNil(Dialog);
    if Assigned(LFMStream) then
      FreeAndNil(LFMStream);
    if Assigned(BinStream) then
      FreeAndNil(BinStream);
  end;
end;

function DialogBoxEx(lfmFileName: PWideChar; DlgProc: TDlgProc): PtrUInt;stdcall;
var
  lfmStringList: TStringListEx;
  wDlgData: WideString;
begin
  try
    lfmStringList:= TStringListEx.Create;
    lfmStringList.LoadFromFile(UTF8Encode(lfmFileName));
    wDlgData:= lfmStringList.Text;
    Result:= DialogBox(PWideChar(wDlgData), DlgProc);
  finally
    FreeAndNil(lfmStringList);
  end;
end;

function SendDlgMsg(pDlg: PtrUInt; DlgItemName: PChar; Msg, wParam, lParam: PtrInt): PtrInt;stdcall;
var
  DialogBox: TDialogBox;
  Control: TControl;
  sText: String;
  wText: WideString;
  I: Integer;
  Rect: TRect;
  Key: Word;
begin
  DialogBox:= TDialogBox(Pointer(pDlg));
  // find component by name
  for I:= 0 to DialogBox.ComponentCount - 1 do
    begin
      Control:= TControl(DialogBox.Components[I]);
      if CompareText(Control.Name,DlgItemName) = 0 then Break;
    end;
  // process message
  case Msg of
  DM_CLOSE:
    begin
      DialogBox.Close;
    end;
  DM_ENABLE:
    begin
      Result:= PtrInt(Control.Enabled);
      if wParam <> -1 then
        Control.Enabled:= Boolean(wParam);
    end;
  DM_GETCHECK:
    begin
      if Control is TCheckBox then
        Result:= PtrInt((Control as TCheckBox).State);
      if Control is TRadioButton then
        Result := PtrInt((Control as TRadioButton).Checked);
    end;
  DM_GETDLGBOUNDS:
    begin
      Rect.Left:= DialogBox.Left;
      Rect.Top:= DialogBox.Top;
      Rect.Right:= DialogBox.Left + DialogBox.Width;
      Rect.Bottom:= DialogBox.Top + DialogBox.Height;
      Result:= PtrInt(@Rect);
    end;
  DM_GETDLGDATA:
    begin
      Result:= DialogBox.Tag;
    end;
  DM_GETDROPPEDDOWN:
    begin
      if Control is TComboBox then
        Result:= PtrInt((Control as TComboBox).DroppedDown);
    end;
  DM_GETITEMBOUNDS:
    begin
      Rect.Left:= Control.Left;
      Rect.Top:= Control.Top;
      Rect.Right:= Control.Left + Control.Width;
      Rect.Bottom:= Control.Top + Control.Height;
      Result:= PtrInt(@Rect);
    end;
  DM_GETITEMDATA:
    begin
      Result:= Control.Tag;
    end;
  DM_LISTADD:
    begin
      sText:= UTF8Encode(PChar(wParam));
      if Control is TComboBox then
         (Control as TComboBox).Items.AddObject(sText, TObject(Pointer(lParam)));
      if Control is TListBox then
        (Control as TListBox).Items.AddObject(sText, TObject(Pointer(lParam)));
    end;
  DM_LISTADDSTR:
    begin
      sText:= UTF8Encode(PChar(wParam));
      if Control is TComboBox then
         (Control as TComboBox).Items.Add(sText);
      if Control is TListBox then
        (Control as TListBox).Items.Add(sText);
    end;
  DM_LISTDELETE:
    begin
      if Control is TComboBox then
         (Control as TComboBox).Items.Delete(wParam);
      if Control is TListBox then
        (Control as TListBox).Items.Delete(wParam);
    end;
  DM_LISTINDEXOF:
    begin
      sText:= UTF8Encode(PWideChar(lParam));
      if Control is TComboBox then
        Result:= (Control as TComboBox).Items.IndexOf(sText);
      if Control is TListBox then
        Result:= (Control as TListBox).Items.IndexOf(sText);
    end;
  DM_LISTINSERT:
    begin
      sText:= UTF8Encode(PWideChar(lParam));
      if Control is TComboBox then
        (Control as TComboBox).Items.Insert(wParam, sText);
      if Control is TListBox then
        (Control as TListBox).Items.Insert(wParam, sText);
    end;
  DM_LISTGETCOUNT:
    begin
      if Control is TComboBox then
        Result:= (Control as TComboBox).Items.Count;
      if Control is TListBox then
        Result:= (Control as TListBox).Items.Count;
    end;
  DM_LISTGETDATA:
    begin
      if Control is TComboBox then
        Result:= PtrInt((Control as TComboBox).Items.Objects[wParam]);
      if Control is TListBox then
        Result:= PtrInt((Control as TListBox).Items.Objects[wParam]);
    end;
  DM_LISTGETITEM:
    begin
      if Control is TComboBox then
        sText:= (Control as TComboBox).Items[wParam];
      if Control is TListBox then
        sText:= (Control as TListBox).Items[wParam];
      wText:= UTF8Decode(sText);
      Result:= PtrInt(PWideChar(wText));
    end;
  DM_LISTGETITEMINDEX:
    begin
      Result:= -1;
      if Control is TComboBox then
        Result:= (Control as TComboBox).ItemIndex;
      if Control is TListBox then
        Result:= (Control as TListBox).ItemIndex;
    end;
  DM_LISTSETITEMINDEX:
    begin
      if Control is TComboBox then
        (Control as TComboBox).ItemIndex:= wParam;
      if Control is TListBox then
        (Control as TListBox).ItemIndex:= wParam;
    end;
  DM_LISTUPDATE :
    begin
      sText:= UTF8Encode(PWideChar(lParam));
      if Control is TComboBox then
        (Control as TComboBox).Items[wParam]:= sText;
      if Control is TListBox then
        (Control as TListBox).Items[wParam]:= sText;
    end;
  DM_GETTEXT:
    begin
      if Control is TButton then
        sText:= (Control as TButton).Caption;
      if Control is TComboBox then
        sText:= (Control as TComboBox).Text;
      if Control is TEdit then
        sText:= (Control as TEdit).Text;
      if Control is TGroupBox then
        sText:= (Control as TGroupBox).Caption;
      if Control is TLabel then
        sText:= (Control as TLabel).Caption;
      wText:= UTF8Decode(sText);
      Result:= PtrInt(PWideChar(wText));
    end;
  DM_KEYDOWN:
    begin
      Key:= wParam;
      DialogBox.KeyDown(Key, GetKeyShiftState);
      Result:= Key;
    end;
  DM_KEYUP:
    begin
      Key:= wParam;
      DialogBox.KeyUp(Key, GetKeyShiftState);
      Result:= Key;
    end;
  DM_REDRAW:
    begin
      DialogBox.Repaint;
    end;
  DM_SETCHECK:
    begin
      if Control is TCheckBox then
        begin
          Result:= PtrInt((Control as TCheckBox).State);
          (Control as TCheckBox).State:= TCheckBoxState(wParam)
        end;
      if Control is TRadioButton then
        begin
          Result := PtrInt((Control as TRadioButton).Checked);
          (Control as TRadioButton).Checked:= Boolean(wParam);
        end;
    end;
  DM_LISTSETDATA:
    begin
      if Control is TComboBox then
        (Control as TComboBox).Items.Objects[wParam]:= TObject(Pointer(lParam));
      if Control is TListBox then
        (Control as TListBox).Items.Objects[wParam]:= TObject(Pointer(lParam));
    end;
  DM_SETDLGBOUNDS:
    begin
      Rect:= PRect(wParam)^;
      DialogBox.Left:= Rect.Left;
      DialogBox.Top:= Rect.Top;
      DialogBox.Width:= Rect.Right - Rect.Left;
      DialogBox.Height:= Rect.Bottom - Rect.Top;
    end;
  DM_SETDLGDATA:
    begin
      Result:= DialogBox.Tag;
      DialogBox.Tag:= wParam;
    end;
  DM_SETDROPPEDDOWN:
    begin
      if Control is TComboBox then
        (Control as TComboBox).DroppedDown:= Boolean(wParam);
    end;
  DM_SETFOCUS:
    begin
      if Control.Visible then
        (Control as TWinControl).SetFocus;
    end;
  DM_SETITEMBOUNDS:
    begin
      Rect:= PRect(wParam)^;
      Control.Left:= Rect.Left;
      Control.Top:= Rect.Top;
      Control.Width:= Rect.Right - Rect.Left;
      Control.Height:= Rect.Bottom - Rect.Top;
    end;
  DM_SETITEMDATA:
    begin
      Control.Tag:= wParam;
    end;
  DM_SETMAXTEXTLENGTH:
    begin
      Result:= -1;
      if Control is TComboBox then
        begin
          Result:= (Control as TComboBox).MaxLength;
          (Control as TComboBox).MaxLength:= wParam;
        end;
      if Control is TEdit then
        begin
          Result:= (Control as TEdit).MaxLength;
          (Control as TEdit).MaxLength:= wParam;
        end;
    end;
  DM_SETTEXT:
    begin
      sText:= UTF8Encode(PWideChar(wParam));
      if Control is TButton then
        (Control as TButton).Caption:= sText;
      if Control is TComboBox then
        (Control as TComboBox).Text:= sText;
      if Control is TEdit then
        (Control as TEdit).Text:= sText;
      if Control is TGroupBox then
        (Control as TGroupBox).Caption:= sText;
      if Control is TLabel then
        (Control as TLabel).Caption:= sText;
    end;
  DM_SHOWDIALOG:
    begin
      if wParam = 0 then
        DialogBox.Hide;
      if wParam = 1 then
        DialogBox.Show;
    end;
  DM_SHOWITEM:
    begin
      Result:= PtrInt(Control.Visible);
      if wParam <> -1 then
        Control.Visible:= Boolean(wParam);
    end;
  end;
end;

{ TDialog }

procedure TDialogBox.DialogBoxShow(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_INITDIALOG,0,0);
end;

procedure TDialogBox.ButtonClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CLICK,0,0);
end;

procedure TDialogBox.ButtonEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ButtonExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ButtonKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYDOWN,Key,0);
end;

procedure TDialogBox.ButtonKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYUP,Key,0);
end;

procedure TDialogBox.ComboBoxClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CLICK,PtrInt((Sender as TComboBox).ItemIndex),0);
end;

procedure TDialogBox.ComboBoxDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_DBLCLICK,PtrInt((Sender as TComboBox).ItemIndex),0);
end;

procedure TDialogBox.ComboBoxChange(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TComboBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ComboBoxEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ComboBoxExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYDOWN,Key,0);
end;

procedure TDialogBox.ComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYUP,Key,0);
end;

procedure TDialogBox.EditClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CLICK,0,0);
end;

procedure TDialogBox.EditDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_DBLCLICK,0,0);
end;

procedure TDialogBox.EditChange(Sender: TObject);
var
  wText: WideString;
begin
  if Assigned(fDlgProc) then
    begin
      wText:= UTF8Decode((Sender as TEdit).Text);
      fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CHANGE, PtrInt(PWideChar(wText)),0);
    end;
end;

procedure TDialogBox.EditEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.EditExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYDOWN,Key,0);
end;

procedure TDialogBox.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYUP,Key,0);
end;

procedure TDialogBox.ListBoxClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CLICK, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxDblClick(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_DBLCLICK, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if Assigned(fDlgProc) then
    begin
      fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_CHANGE, PtrInt((Sender as TListBox).ItemIndex),0);
    end;
end;

procedure TDialogBox.ListBoxEnter(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_GOTFOCUS,0,0);
end;

procedure TDialogBox.ListBoxExit(Sender: TObject);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KILLFOCUS,0,0);
end;

procedure TDialogBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYDOWN,Key,0);
end;

procedure TDialogBox.ListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fDlgProc) then
    fDlgProc(PtrUInt(Pointer(Self)), PChar((Sender as TControl).Name), DN_KEYUP,Key,0);
end;

initialization
  {.$I fdialogbox.lrs}

end.


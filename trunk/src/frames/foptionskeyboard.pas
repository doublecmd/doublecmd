{
   Double Commander
   -------------------------------------------------------------------------
   Keyboard options page

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

unit fOptionsKeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsKeyboard }

  TfrmOptionsKeyboard = class(TOptionsEditor)
    cbLynxLike: TCheckBox;
    cbNoModifier: TComboBox;
    cbAlt: TComboBox;
    cbCtrlAlt: TComboBox;
    gbTyping: TGroupBox;
    lblNoModifier: TLabel;
    lblAlt: TLabel;
    lblCtrlAlt: TLabel;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

const
  KeyAction_None        = 0;
  KeyAction_CommandLine = 1;
  KeyAction_QuickSearch = 2;
  KeyAction_QuickFilter = 3;

{ TfrmOptionsKeyboard }

procedure TfrmOptionsKeyboard.Init;
begin
  // Copy localized strings to each combo box.
  cbAlt.Items.Assign(cbNoModifier.Items);
  cbCtrlAlt.Items.Assign(cbNoModifier.Items);
end;

procedure TfrmOptionsKeyboard.Load;
  procedure SetAction(ComboBox: TComboBox; KeyTypingAction: TKeyTypingAction);
  begin
    case KeyTypingAction of
      ktaNone:
        ComboBox.ItemIndex := KeyAction_None;
      ktaCommandLine:
        ComboBox.ItemIndex := KeyAction_CommandLine;
      ktaQuickSearch:
        ComboBox.ItemIndex := KeyAction_QuickSearch;
      ktaQuickFilter:
        ComboBox.ItemIndex := KeyAction_QuickFilter;
      else
        raise Exception.Create('Unknown TKeyTypingMode');
    end;
  end;
begin
  SetAction(cbNoModifier, gKeyTyping[ktmNone]);
  SetAction(cbAlt, gKeyTyping[ktmAlt]);
  SetAction(cbCtrlAlt, gKeyTyping[ktmCtrlAlt]);

  cbLynxLike.Checked := gLynxLike;
end;

function TfrmOptionsKeyboard.Save: TOptionsEditorSaveFlags;
  function GetAction(ComboBox: TComboBox): TKeyTypingAction;
  begin
    case ComboBox.ItemIndex of
      KeyAction_None:
        Result := ktaNone;
      KeyAction_CommandLine:
        Result := ktaCommandLine;
      KeyAction_QuickSearch:
        Result := ktaQuickSearch;
      KeyAction_QuickFilter:
        Result := ktaQuickFilter;
      else
        raise Exception.Create('Unknown action selected');
    end;
  end;
begin
  gKeyTyping[ktmNone]    := GetAction(cbNoModifier);
  gKeyTyping[ktmAlt]     := GetAction(cbAlt);
  gKeyTyping[ktmCtrlAlt] := GetAction(cbCtrlAlt);

  gLynxLike := cbLynxLike.Checked;
  Result := [];
end;

class function TfrmOptionsKeyboard.GetIconIndex: Integer;
begin
  Result := 26;
end;

class function TfrmOptionsKeyboard.GetTitle: String;
begin
  Result := rsOptionsEditorKeyboard;
end;

end.


{
   Double Commander
   -------------------------------------------------------------------------
   Terminal options page

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

unit fOptionsTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls, ExtCtrls, Buttons, Menus;

type

  { TfrmOptionsTerminal }

  TfrmOptionsTerminal = class(TOptionsEditor)
    gbRunInTerminalClose: TGroupBox;
    gbJustRunTerminal: TGroupBox;
    gbRunInTerminalStayOpen: TGroupBox;
    ledtRunInTermCloseCmd: TLabeledEdit;
    ledtRunTermCmd: TLabeledEdit;
    ledtRunTermParams: TLabeledEdit;
    ledtRunInTermStayOpenCmd: TLabeledEdit;
    ledtRunInTermCloseParams: TLabeledEdit;
    ledtRunInTermStayOpenParams: TLabeledEdit;
  protected
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

{ TfrmOptionsTerminal }

procedure TfrmOptionsTerminal.Load;
begin
  ledtRunInTermStayOpenCmd.Text := gRunInTermStayOpenCmd;
  ledtRunInTermStayOpenParams.Text := gRunInTermStayOpenParams;
  ledtRunInTermCloseCmd.Text := gRunInTermCloseCmd;
  ledtRunInTermCloseParams.Text := gRunInTermCloseParams;
  ledtRunTermCmd.Text := gRunTermCmd;
  ledtRunTermParams.Text := gRunTermParams;
end;

function TfrmOptionsTerminal.Save: TOptionsEditorSaveFlags;
begin
  gRunInTermStayOpenCmd := ledtRunInTermStayOpenCmd.Text;
  gRunInTermStayOpenParams := ledtRunInTermStayOpenParams.Text;
  gRunInTermCloseCmd := ledtRunInTermCloseCmd.Text;
  gRunInTermCloseParams := ledtRunInTermCloseParams.Text;
  gRunTermCmd := ledtRunTermCmd.Text;
  gRunTermParams := ledtRunTermParams.Text;
  Result := [];
end;

class function TfrmOptionsTerminal.GetIconIndex: Integer;
begin
  Result := 24;
end;

class function TfrmOptionsTerminal.GetTitle: String;
begin
  Result := rsOptionsEditorTerminal;
end;

end.


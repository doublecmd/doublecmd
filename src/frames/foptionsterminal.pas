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

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
  private
    FLastLoadedOptionSignature: dword;
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    function CanWeClose(var {%H-}WillNeedUpdateWindowView: boolean): boolean; override;
  end;

implementation

{$R *.lfm}

uses
  fOptions, uShowMsg, uComponentsSignature, uGlobs, uLng;

{ TfrmOptionsTerminal }

procedure TfrmOptionsTerminal.Load;
begin
  ledtRunInTermStayOpenCmd.Text := gRunInTermStayOpenCmd;
  ledtRunInTermStayOpenParams.Text := gRunInTermStayOpenParams;
  ledtRunInTermCloseCmd.Text := gRunInTermCloseCmd;
  ledtRunInTermCloseParams.Text := gRunInTermCloseParams;
  ledtRunTermCmd.Text := gRunTermCmd;
  ledtRunTermParams.Text := gRunTermParams;

  FLastLoadedOptionSignature := ComputeSignatureBasedOnComponent(Self, $00000000);
end;

function TfrmOptionsTerminal.Save: TOptionsEditorSaveFlags;
begin
  gRunInTermStayOpenCmd := ledtRunInTermStayOpenCmd.Text;
  gRunInTermStayOpenParams := ledtRunInTermStayOpenParams.Text;
  gRunInTermCloseCmd := ledtRunInTermCloseCmd.Text;
  gRunInTermCloseParams := ledtRunInTermCloseParams.Text;
  gRunTermCmd := ledtRunTermCmd.Text;
  gRunTermParams := ledtRunTermParams.Text;

  FLastLoadedOptionSignature := ComputeSignatureBasedOnComponent(Self, $00000000);
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

{ TfrmOptionsTerminal.CanWeClose }
function TfrmOptionsTerminal.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := (FLastLoadedOptionSignature = ComputeSignatureBasedOnComponent(Self, $00000000));

  if not Result then
  begin
    ShowOptions(TfrmOptionsTerminal);
    Answer := MsgBox(rsMsgTerminalOptionsModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
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


end.


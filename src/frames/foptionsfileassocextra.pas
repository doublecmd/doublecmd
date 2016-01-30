{
    Double Commander
    -------------------------------------------------------------------------
    Extra File Associations Configuration

	Copyright (C) 2016  Alexander Koblov (alexx2000@mail.ru)

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


unit fOptionsFileAssocExtra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls, ExtCtrls;

type

  { TfrmOptionsFileAssocExtra }

  TfrmOptionsFileAssocExtra = class(TOptionsEditor)
    cbOfferToAddToFileAssociations: TCheckBox;
    cbExecuteViaShell: TCheckBox;
    cbExtendedContextMenu: TCheckBox;
    cbOpenSystemWithTerminalClose: TCheckBox;
    cbOpenSystemWithTerminalStayOpen: TCheckBox;
    cbIncludeConfigFileAssoc: TCheckBox;
    gbExtendedContextMenuOptions: TGroupBox;
    procedure cbExtendedContextMenuChange(Sender: TObject);
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  private
    FLastLoadedOptionSignature: dword;
  public
    class function GetTitle: string; override;
    class function GetIconIndex: integer; override;
    function CanWeClose(var WillNeedUpdateWindowView: boolean): boolean; override;
  end;

implementation

{$R *.lfm}

uses
  fOptions, uShowMsg, uComponentsSignature, DCStrUtils, uGlobs, uLng;

{TfrmOptionsFileAssocExtra}

{ TfrmOptionsFileAssocExtra.GetTitle }
class function TfrmOptionsFileAssocExtra.GetTitle: string;
begin
  Result := rsOptionsEditorFileAssicExtra;
end;

{ TfrmOptionsFileAssocExtra.GetIconIndex }
class function TfrmOptionsFileAssocExtra.GetIconIndex: integer;
begin
  Result := 36;
end;

procedure TfrmOptionsFileAssocExtra.cbExtendedContextMenuChange(Sender: TObject);
begin
  gbExtendedContextMenuOptions.Enabled := TCheckbox(Sender).Checked;
end;

{ TfrmOptionsFileAssocExtra.Load }
procedure TfrmOptionsFileAssocExtra.Load;
begin
  cbOfferToAddToFileAssociations.Checked := gOfferToAddToFileAssociations;
  cbExtendedContextMenu.Checked := gExtendedContextMenu;
  cbOpenSystemWithTerminalStayOpen.Checked := gExecuteViaTerminalStayOpen;
  cbOpenSystemWithTerminalClose.Checked := gExecuteViaTerminalClose;
  cbExecuteViaShell.Checked := gOpenExecuteViaShell;
  cbIncludeConfigFileAssoc.Checked := gIncludeFileAssociation;
  cbExtendedContextMenuChange(cbExtendedContextMenu);
  FLastLoadedOptionSignature := ComputeSignatureBasedOnComponent(Self, $00000000);
end;

{ TfrmOptionsFileAssocExtra.Save }
function TfrmOptionsFileAssocExtra.Save: TOptionsEditorSaveFlags;
begin
  gOfferToAddToFileAssociations := cbOfferToAddToFileAssociations.Checked;
  gExtendedContextMenu := cbExtendedContextMenu.Checked;
  gExecuteViaTerminalStayOpen := cbOpenSystemWithTerminalStayOpen.Checked;
  gExecuteViaTerminalClose := cbOpenSystemWithTerminalClose.Checked;
  gOpenExecuteViaShell := cbExecuteViaShell.Checked;
  gIncludeFileAssociation := cbIncludeConfigFileAssoc.Checked;
  FLastLoadedOptionSignature := ComputeSignatureBasedOnComponent(Self, $00000000);
  Result := [];
end;

{ TfrmOptionsFileAssocExtra.CanWeClose }
function TfrmOptionsFileAssocExtra.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := (FLastLoadedOptionSignature = ComputeSignatureBasedOnComponent(Self, $00000000));

  if not Result then
  begin
    ShowOptions(TfrmOptionsFileAssocExtra);
    Answer := MsgBox(rsMsgFileAssociationsExtraModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
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

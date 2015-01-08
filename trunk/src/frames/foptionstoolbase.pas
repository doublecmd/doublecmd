{
   Double Commander
   -------------------------------------------------------------------------
   Base options page for external tools (Viewer, Editor, Differ)

   Copyright (C) 2006-2015  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsToolBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, EditBtn, Buttons, Menus,
  fOptionsFrame, uGlobs;

type

  { TfrmOptionsToolBase }

  TfrmOptionsToolBase = class(TOptionsEditor)
    btnRelativeToolPath: TSpeedButton;
    cbToolsKeepTerminalOpen: TCheckBox;
    cbToolsRunInTerminal: TCheckBox;
    cbToolsUseExternalProgram: TCheckBox;
    edtToolsParameters: TEdit;
    fneToolsPath: TFileNameEdit;
    lblToolsParameters: TLabel;
    lblToolsPath: TLabel;
    pmPathHelper: TPopupMenu;
    procedure btnRelativeToolPathClick(Sender: TObject);
    procedure cbToolsKeepTerminalOpenChange(Sender: TObject);
    procedure cbToolsRunInTerminalChange(Sender: TObject);
    procedure cbToolsUseExternalProgramChange(Sender: TObject);
    procedure edtToolsParametersChange(Sender: TObject);
    procedure fneToolsPathAcceptFileName(Sender: TObject; var Value: String);
    procedure fneToolsPathChange(Sender: TObject);
  private
    FExternalTool: TExternalTool;
    FExternalToolOptions: TExternalToolOptions;
    FOnUseExternalProgramChange: TNotifyEvent;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    property ExternalTool: TExternalTool read FExternalTool write FExternalTool;
    property OnUseExternalProgramChange: TNotifyEvent read FOnUseExternalProgramChange write FOnUseExternalProgramChange;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  uDCUtils, uSpecialDir;

{ TfrmOptionsToolBase }

procedure TfrmOptionsToolBase.cbToolsKeepTerminalOpenChange(Sender: TObject);
begin
  FExternalToolOptions.KeepTerminalOpen := cbToolsKeepTerminalOpen.Checked;
end;

procedure TfrmOptionsToolBase.btnRelativeToolPathClick(Sender: TObject);
begin
  fneToolsPath.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneToolsPath,pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmOptionsToolBase.cbToolsRunInTerminalChange(Sender: TObject);
begin
  cbToolsKeepTerminalOpen.Enabled := cbToolsRunInTerminal.Checked;
  FExternalToolOptions.RunInTerminal := cbToolsRunInTerminal.Checked;
end;

procedure TfrmOptionsToolBase.cbToolsUseExternalProgramChange(Sender: TObject);
begin
  lblToolsPath.Enabled            := cbToolsUseExternalProgram.Checked;
  fneToolsPath.Enabled            := cbToolsUseExternalProgram.Checked;
  btnRelativeToolPath.Enabled := cbToolsUseExternalProgram.Checked;
  lblToolsParameters.Enabled      := cbToolsUseExternalProgram.Checked;
  edtToolsParameters.Enabled      := cbToolsUseExternalProgram.Checked;
  cbToolsRunInTerminal.Enabled    := cbToolsUseExternalProgram.Checked;
  cbToolsKeepTerminalOpen.Enabled := cbToolsUseExternalProgram.Checked;

  FExternalToolOptions.Enabled := cbToolsUseExternalProgram.Checked;

  if Assigned(FOnUseExternalProgramChange) then
    FOnUseExternalProgramChange(Self);
end;

procedure TfrmOptionsToolBase.edtToolsParametersChange(Sender: TObject);
begin
  FExternalToolOptions.Parameters := edtToolsParameters.Text;
end;

procedure TfrmOptionsToolBase.fneToolsPathAcceptFileName(Sender: TObject; var Value: String);
begin
  Value := SetCmdDirAsEnvVar(Value);
{$IF DEFINED(LCLCARBON)}
  // OnChange don't called under Carbon when choose file name
  // from open dialog so assign path in this event.
  FExternalToolOptions.Path := Value;
{$ENDIF}
end;

procedure TfrmOptionsToolBase.fneToolsPathChange(Sender: TObject);
begin
  FExternalToolOptions.Path := fneToolsPath.FileName;
end;

procedure TfrmOptionsToolBase.Init;
begin
  // Enable/disable tools controls.
  cbToolsUseExternalProgramChange(nil);
end;

procedure TfrmOptionsToolBase.Load;
begin
  FExternalToolOptions := gExternalTools[FExternalTool];

  cbToolsUseExternalProgram.Checked := FExternalToolOptions.Enabled;
  fneToolsPath.FileName             := FExternalToolOptions.Path;
  edtToolsParameters.Text           := FExternalToolOptions.Parameters;
  cbToolsRunInTerminal.Checked      := FExternalToolOptions.RunInTerminal;
  cbToolsKeepTerminalOpen.Checked   := FExternalToolOptions.KeepTerminalOpen;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
end;

function TfrmOptionsToolBase.Save: TOptionsEditorSaveFlags;
begin
  gExternalTools[FExternalTool] := FExternalToolOptions;
  Result := [];
end;

constructor TfrmOptionsToolBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOnUseExternalProgramChange := nil;
end;

end.


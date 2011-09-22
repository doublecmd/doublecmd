{
   Double Commander
   -------------------------------------------------------------------------
   Configuration options page

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

unit fOptionsConfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fOptionsFrame, StdCtrls, Buttons;

type

  { TfrmOptionsConfiguration }

  TfrmOptionsConfiguration = class(TOptionsEditor)
    btnConfigApply: TBitBtn;
    btnConfigEdit: TBitBtn;
    cbCmdLineHistory: TCheckBox;
    cbDirHistory: TCheckBox;
    cbFileMaskHistory: TCheckBox;
    chkSaveConfiguration: TCheckBox;
    chkSearchReplaceHistory: TCheckBox;
    gbLocConfigFiles: TGroupBox;
    gbSaveOnExit: TGroupBox;
    lblCmdLineConfigDir: TLabel;
    rbProgramDir: TRadioButton;
    rbUserHomeDir: TRadioButton;
    procedure btnConfigApplyClick(Sender: TObject);
    procedure btnConfigEditClick(Sender: TObject);
    procedure chkSaveConfigurationChange(Sender: TObject);
  protected
    procedure Init; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  uGlobs, uGlobsPaths, uShowForm, uOSUtils, uLng;

{ TfrmOptionsConfiguration }

procedure TfrmOptionsConfiguration.btnConfigApplyClick(Sender: TObject);
begin
  gConfig.Load; // force reloading config from file
  LoadGlobs;
  //TODO: Options.LoadConfig;
  btnConfigApply.Enabled:= False;
end;

procedure TfrmOptionsConfiguration.btnConfigEditClick(Sender: TObject);
begin
  if Assigned(gIni) then
    ShowEditorByGlob(gpCfgDir + 'doublecmd.ini')
  else
    ShowEditorByGlob(gpCfgDir + 'doublecmd.xml');
  btnConfigApply.Enabled:= True;
end;

procedure TfrmOptionsConfiguration.chkSaveConfigurationChange(Sender: TObject);
begin
  cbDirHistory.Enabled:= chkSaveConfiguration.Checked;
  cbCmdLineHistory.Enabled:= chkSaveConfiguration.Checked;
  cbFileMaskHistory.Enabled:= chkSaveConfiguration.Checked;
  chkSearchReplaceHistory.Enabled:= chkSaveConfiguration.Checked;
end;

class function TfrmOptionsConfiguration.GetIconIndex: Integer;
begin
  Result := 11;
end;

class function TfrmOptionsConfiguration.GetTitle: String;
begin
  Result := rsOptionsEditorConfiguration;
end;

procedure TfrmOptionsConfiguration.Init;
begin
  if gpCmdLineCfgDir = '' then
  begin
    rbProgramDir.Caption:= rbProgramDir.Caption + ' - [' + IncludeTrailingPathDelimiter(gpGlobalCfgDir) + ']';
    rbUserHomeDir.Caption:= rbUserHomeDir.Caption + ' - [' + IncludeTrailingPathDelimiter(GetAppConfigDir) + ']';
  end
  else
  begin
    rbProgramDir.Visible := False;
    rbProgramDir.Enabled := False;
    rbUserHomeDir.Visible := False;
    rbUserHomeDir.Enabled := False;
    lblCmdLineConfigDir.Visible := True;
    lblCmdLineConfigDir.Caption := lblCmdLineConfigDir.Caption + ' - [' + IncludeTrailingPathDelimiter(gpCmdLineCfgDir) + ']';
  end;
end;

procedure TfrmOptionsConfiguration.Load;
begin
  if gUseConfigInProgramDirNew then
    rbProgramDir.Checked := True
  else
    rbUserHomeDir.Checked := True;
  chkSaveConfiguration.Checked:= gSaveConfiguration;
  chkSearchReplaceHistory.Checked:= gSaveSearchReplaceHistory;
  cbDirHistory.Checked := gSaveDirHistory;
  cbCmdLineHistory.Checked := gSaveCmdLineHistory;
  cbFileMaskHistory.Checked := gSaveFileMaskHistory;
end;

function TfrmOptionsConfiguration.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gUseConfigInProgramDirNew := rbProgramDir.Checked;
  gSaveConfiguration := chkSaveConfiguration.Checked;
  gSaveSearchReplaceHistory := chkSearchReplaceHistory.Checked;
  gSaveDirHistory := cbDirHistory.Checked;
  gSaveCmdLineHistory := cbCmdLineHistory.Checked;
  gSaveFileMaskHistory := cbFileMaskHistory.Checked;
end;

end.


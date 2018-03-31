{
   Double Commander
   -------------------------------------------------------------------------
   Configuration options page

   Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fOptionsConfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fOptionsFrame, StdCtrls, Buttons, ExtCtrls;

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
    edtHighlighters: TEdit;
    edtThumbCache: TEdit;
    edtIconThemes: TEdit;
    gbLocConfigFiles: TGroupBox;
    gbSaveOnExit: TGroupBox;
    gbDirectories: TGroupBox;
    lblIconThemes: TLabel;
    lblHighlighters: TLabel;
    lblThumbCache: TLabel;
    lblCmdLineConfigDir: TLabel;
    gbSortOrderConfigurationOption: TRadioGroup;
    rbProgramDir: TRadioButton;
    rbUserHomeDir: TRadioButton;
    procedure btnConfigApplyClick(Sender: TObject);
    procedure btnConfigEditClick(Sender: TObject);
    procedure chkSaveConfigurationChange(Sender: TObject);
    procedure gbSortOrderConfigurationOptionClick(Sender: TObject);
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
  Forms, DCStrUtils, uGlobs, uGlobsPaths, uShowForm, uOSUtils, uLng, fOptions;

{ TfrmOptionsConfiguration }

procedure TfrmOptionsConfiguration.btnConfigApplyClick(Sender: TObject);
begin
  if LoadConfig then // force reloading config from file
  begin
    LoadGlobs;
    OptionsDialog.LoadSettings;
    btnConfigApply.Enabled:= False;
  end
  else
  begin
    gSaveConfiguration := False;
    Application.Terminate;
  end;
end;

procedure TfrmOptionsConfiguration.btnConfigEditClick(Sender: TObject);
begin
  ShowEditorByGlob(gpCfgDir + 'doublecmd.xml');
  btnConfigApply.Enabled:= True;
end;

procedure TfrmOptionsConfiguration.chkSaveConfigurationChange(Sender: TObject);
begin
  cbDirHistory.Enabled:= chkSaveConfiguration.Checked;
  cbCmdLineHistory.Enabled:= chkSaveConfiguration.Checked;
  cbFileMaskHistory.Enabled:= chkSaveConfiguration.Checked;
  chkSearchReplaceHistory.Enabled := chkSaveConfiguration.Checked;
end;

procedure TfrmOptionsConfiguration.gbSortOrderConfigurationOptionClick(Sender: TObject);
begin
  //Exceptionnally for THIS setting, let's apply it immediately, even before quiting since the effect is... in the configuration area, just where we are at this moment!
  gSortOrderOfConfigurationOptionsTree := TSortConfigurationOptions(gbSortOrderConfigurationOption.ItemIndex);
  SortConfigurationOptionsOnLeftTree;
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
  ParseLineToList(rsOptConfigSortOrder, gbSortOrderConfigurationOption.Items);
end;

procedure TfrmOptionsConfiguration.Load;
begin
  if gUseConfigInProgramDirNew then
    rbProgramDir.Checked := True
  else
    rbUserHomeDir.Checked := True;

  edtThumbCache.Text:= gpThumbCacheDir;

  if not gUseConfigInProgramDir then begin
    edtIconThemes.Text:= IncludeTrailingBackslash(GetAppDataDir) + 'pixmaps' + PathSep;
  end;
  edtIconThemes.Text:= edtIconThemes.Text + ExcludeTrailingPathDelimiter(gpPixmapPath);

  if not gUseConfigInProgramDir then begin
    edtHighlighters.Text:= IncludeTrailingBackslash(GetAppDataDir) + 'highlighters' + PathSep;
  end;
  edtHighlighters.Text:= edtHighlighters.Text + ExcludeTrailingPathDelimiter(gpHighPath);

  chkSaveConfiguration.Checked:= gSaveConfiguration;
  chkSearchReplaceHistory.Checked:= gSaveSearchReplaceHistory;
  cbDirHistory.Checked := gSaveDirHistory;
  cbCmdLineHistory.Checked := gSaveCmdLineHistory;
  cbFileMaskHistory.Checked := gSaveFileMaskHistory;
  gbSortOrderConfigurationOption.ItemIndex:=Integer(gSortOrderOfConfigurationOptionsTree);
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
  gSortOrderOfConfigurationOptionsTree := TSortConfigurationOptions(gbSortOrderConfigurationOption.ItemIndex);
end;

end.


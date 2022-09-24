{
   Double Commander
   -------------------------------------------------------------------------
   Miscellaneous options page

   Copyright (C) 2006-2022 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsMisc;

{$mode objfpc}{$H+}

interface

uses
  EditBtn, Buttons, Menus,
  Classes, SysUtils, StdCtrls, Spin, ExtCtrls, DividerBevel, fOptionsFrame;

type

  { TfrmOptionsMisc }

  TfrmOptionsMisc = class(TOptionsEditor)
    btnThumbCompactCache: TButton;
    chkShowSplashForm: TCheckBox;
    chkDescCreateUnicode: TCheckBox;
    chkGoToRoot: TCheckBox;
    chkShowCurDirTitleBar: TCheckBox;
    chkThumbSave: TCheckBox;
    chkShowWarningMessages: TCheckBox;
    cmbDescDefaultEncoding: TComboBox;
    cmbDescCreateEncoding: TComboBox;
    cmbDefaultEncoding: TComboBox;
    dblThumbnails: TDividerBevel;
    gbExtended: TGroupBox;
    gbFileComments: TGroupBox;
    lblDefaultEncoding: TLabel;
    lblDescrDefaultEncoding: TLabel;
    lblThumbPixels: TLabel;
    lblThumbSize: TLabel;
    lblThumbSeparator: TLabel;
    speThumbWidth: TSpinEdit;
    speThumbHeight: TSpinEdit;
    gbTCExportImport: TGroupBox;
    lblTCExecutable: TLabel;
    fneTCExecutableFilename: TFileNameEdit;
    btnRelativeTCExecutableFile: TSpeedButton;
    lblTCConfig: TLabel;
    fneTCConfigFilename: TFileNameEdit;
    btnRelativeTCConfigFile: TSpeedButton;
    btnViewConfigFile: TSpeedButton;
    lblTCPathForTool: TLabel;
    edOutputPathForToolbar: TEdit;
    btnOutputPathForToolbar: TButton;
    btnRelativeOutputPathForToolbar: TSpeedButton;
    pmPathHelper: TPopupMenu;
    procedure btnThumbCompactCacheClick(Sender: TObject);
    procedure btnRelativeTCExecutableFileClick(Sender: TObject);
    procedure btnRelativeTCConfigFileClick(Sender: TObject);
    procedure btnViewConfigFileClick(Sender: TObject);
    procedure btnOutputPathForToolbarClick(Sender: TObject);
    procedure btnRelativeOutputPathForToolbarClick(Sender: TObject);
    procedure chkDescCreateUnicodeChange(Sender: TObject);
  private
    FSplashForm: Boolean;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

procedure BringUsToTCConfigurationPage;

implementation

{$R *.lfm}

uses
  fOptions, Forms, Dialogs, fMain, Controls,
  DCStrUtils, uDCUtils, uSpecialDir, uShowForm, uGlobs, uLng, uThumbnails,
  uConvEncoding, uEarlyConfig;

{ TfrmOptionsMisc }

class function TfrmOptionsMisc.GetIconIndex: Integer;
begin
  Result := 14;
end;

class function TfrmOptionsMisc.GetTitle: String;
begin
  Result := rsOptionsEditorMiscellaneous;
end;

procedure TfrmOptionsMisc.btnThumbCompactCacheClick(Sender: TObject);
begin
  TThumbnailManager.CompactCache;
end;

procedure TfrmOptionsMisc.Init;
var
  Index: Integer;
begin
  FSplashForm:= gSplashForm;

  GetSupportedEncodings(cmbDefaultEncoding.Items);
  for Index:= cmbDefaultEncoding.Items.Count - 1 downto 0 do
  begin
    if (not SingleByteEncoding(cmbDefaultEncoding.Items[Index])) then
      cmbDefaultEncoding.Items.Delete(Index);
  end;
  cmbDefaultEncoding.Items.Insert(0, UpperCase(EncodingNone));

  fneTCExecutableFilename.Filter := ParseLineToFileFilter([rsFilterExecutableFiles, '*.exe', rsFilterAnyFiles, '*.*']);
  fneTCConfigFilename.Filter := ParseLineToFileFilter([rsFilterIniConfigFiles, '*.ini', rsFilterAnyFiles, '*.*']);
end;

procedure TfrmOptionsMisc.Load;
var
  Index: Integer;
begin
  chkShowSplashForm.Checked      := gSplashForm;
  chkShowWarningMessages.Checked := gShowWarningMessages;
  chkThumbSave.Checked           := gThumbSave;
  speThumbWidth.Value            := gThumbSize.cx;
  speThumbHeight.Value           := gThumbSize.cy;
  chkGoToRoot.Checked            := gGoToRoot;
  chkShowCurDirTitleBar.Checked  := gShowCurDirTitleBar;

  Index:= cmbDefaultEncoding.Items.IndexOf(gDefaultTextEncoding);
  if (Index < 0) then
    cmbDefaultEncoding.ItemIndex:= 0
  else begin
    cmbDefaultEncoding.ItemIndex:= Index;
  end;

  {$IFDEF MSWINDOWS}
  gbTCExportImport.Visible:=True;
  fneTCExecutableFilename.FileName := gTotalCommanderExecutableFilename;
  fneTCConfigFilename.FileName := gTotalCommanderConfigFilename;
  edOutputPathForToolbar.Text := gTotalCommanderToolbarPath;
  fneTCExecutableFilename.DialogTitle := rsMsgLocateTCExecutable;
  fneTCConfigFilename.DialogTitle := rsMsgLocateTCConfiguation;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  {$ENDIF}

  case gDescReadEncoding of
    meOEM:  cmbDescDefaultEncoding.ItemIndex:= 0;
    meANSI: cmbDescDefaultEncoding.ItemIndex:= 1;
    meUTF8: cmbDescDefaultEncoding.ItemIndex:= 2;
    else    cmbDescDefaultEncoding.ItemIndex:= 2;
  end;

  case gDescWriteEncoding of
    meUTF8BOM: cmbDescCreateEncoding.ItemIndex:= 0;
    meUTF16LE: cmbDescCreateEncoding.ItemIndex:= 1;
    meUTF16BE: cmbDescCreateEncoding.ItemIndex:= 2;
    else       cmbDescCreateEncoding.ItemIndex:= 0;
  end;

  chkDescCreateUnicode.Checked:= gDescCreateUnicode;
  chkDescCreateUnicodeChange(chkDescCreateUnicode);
end;

function TfrmOptionsMisc.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  gSplashForm          := chkShowSplashForm.Checked;
  gShowWarningMessages := chkShowWarningMessages.Checked;
  gThumbSave           := chkThumbSave.Checked;
  gThumbSize.cx        := speThumbWidth.Value;
  gThumbSize.cy        := speThumbHeight.Value;
  gGoToRoot            := chkGoToRoot.Checked;
  gShowCurDirTitleBar  := chkShowCurDirTitleBar.Checked;
  gDefaultTextEncoding := cmbDefaultEncoding.Text;

  {$IFDEF MSWINDOWS}
  gTotalCommanderExecutableFilename := fneTCExecutableFilename.FileName;
  gTotalCommanderConfigFilename := fneTCConfigFilename.FileName;
  gTotalCommanderToolbarPath := edOutputPathForToolbar.Text;
  {$ENDIF}

  case cmbDescDefaultEncoding.ItemIndex of
    0: gDescReadEncoding:= meOEM;
    1: gDescReadEncoding:= meANSI;
    2: gDescReadEncoding:= meUTF8;
  end;

  case cmbDescCreateEncoding.ItemIndex of
    0: gDescWriteEncoding:= meUTF8BOM;
    1: gDescWriteEncoding:= meUTF16LE;
    2: gDescWriteEncoding:= meUTF16BE;
  end;

  gDescCreateUnicode:= chkDescCreateUnicode.Checked;

  if gSplashForm <> FSplashForm then
  try
    SaveEarlyConfig;
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

{ TfrmOptionsMisc.btnRelativeTCExecutableFileClick }
procedure TfrmOptionsMisc.btnRelativeTCExecutableFileClick(Sender: TObject);
begin
  fneTCExecutableFilename.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneTCExecutableFilename, pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsMisc.btnRelativeTCConfigFileClick }
procedure TfrmOptionsMisc.btnRelativeTCConfigFileClick(Sender: TObject);
begin
  fneTCConfigFilename.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneTCConfigFilename, pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsMisc.btnViewConfigFileClick }
procedure TfrmOptionsMisc.btnViewConfigFileClick(Sender: TObject);
begin
  ShowViewerByGlob(mbExpandFileName(fneTCConfigFilename.FileName));
end;

{ TfrmOptionsMisc.btnOutputPathForToolbarClick }
procedure TfrmOptionsMisc.btnOutputPathForToolbarClick(Sender: TObject);
var
  MaybeResultingOutputPath: string;
begin
  MaybeResultingOutputPath := edOutputPathForToolbar.Text;
  if MaybeResultingOutputPath = '' then
    MaybeResultingOutputPath := frmMain.ActiveFrame.CurrentPath;
  if SelectDirectory(rsSelectDir, MaybeResultingOutputPath, MaybeResultingOutputPath, False) then
    edOutputPathForToolbar.Text := MaybeResultingOutputPath;
end;

{ TfrmOptionsMisc.btnRelativeOutputPathForToolbarClick }
procedure TfrmOptionsMisc.btnRelativeOutputPathForToolbarClick(Sender: TObject);
begin
  edOutputPathForToolbar.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edOutputPathForToolbar, pfPATH);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmOptionsMisc.chkDescCreateUnicodeChange(Sender: TObject);
begin
  cmbDescCreateEncoding.Enabled:= chkDescCreateUnicode.Checked;
end;
procedure BringUsToTCConfigurationPage;
var
  Editor: TOptionsEditor;
  Options: IOptionsDialog;
begin
  Options := ShowOptions(TfrmOptionsMisc);
  Application.ProcessMessages;
  Editor := Options.GetEditor(TfrmOptionsMisc);
  Application.ProcessMessages;
  if Editor.CanFocus then
    Editor.SetFocus;
end;

end.


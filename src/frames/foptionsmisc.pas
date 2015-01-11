{
   Double Commander
   -------------------------------------------------------------------------
   Miscellaneous options page

   Copyright (C) 2006-2014  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsMisc;

{$mode objfpc}{$H+}

interface

uses
  EditBtn, Buttons, Menus,
  Classes, SysUtils, StdCtrls, Spin, DividerBevel, fOptionsFrame;

type

  { TfrmOptionsMisc }

  TfrmOptionsMisc = class(TOptionsEditor)
    btnThumbCompactCache: TButton;
    chkGoToRoot: TCheckBox;
    chkThumbSave: TCheckBox;
    chkShowWarningMessages: TCheckBox;
    dblThumbnails: TDividerBevel;
    gbExtended: TGroupBox;
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
    procedure GenericSomethingChanged(Sender: TObject);
  private
    FModificationTookPlace: Boolean;
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    function CanWeClose(var WillNeedUpdateWindowView: boolean): boolean; override;
  end;

procedure BringUsToTCConfigurationPage;

implementation

{$R *.lfm}

uses
  uShowMsg, fOptions, Forms, Dialogs, fMain, Controls, uSpecialDir, uShowForm,
  uGlobs, uLng, uThumbnails;

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

procedure TfrmOptionsMisc.Load;
begin
  chkShowWarningMessages.Checked := gShowWarningMessages;
  chkThumbSave.Checked           := gThumbSave;
  speThumbWidth.Value            := gThumbSize.cx;
  speThumbHeight.Value           := gThumbSize.cy;
  chkGoToRoot.Checked            := gGoToRoot;

  {$IFDEF MSWINDOWS}
  gbTCExportImport.Visible:=True;
  fneTCExecutableFilename.FileName := gTotalCommanderExecutableFilename;
  fneTCConfigFilename.FileName := gTotalCommanderConfigFilename;
  edOutputPathForToolbar.Text := gTotalCommanderToolbarPath;
  fneTCExecutableFilename.DialogTitle := rsMsgLocateTCExecutable;
  fneTCConfigFilename.DialogTitle := rsMsgLocateTCConfiguation;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper, mp_PATHHELPER, nil);
  {$ENDIF}
  FModificationTookPlace := False;
end;

function TfrmOptionsMisc.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  gShowWarningMessages := chkShowWarningMessages.Checked;
  gThumbSave           := chkThumbSave.Checked;
  gThumbSize.cx        := speThumbWidth.Value;
  gThumbSize.cy        := speThumbHeight.Value;
  gGoToRoot            := chkGoToRoot.Checked;
  {$IFDEF MSWINDOWS}
  gTotalCommanderExecutableFilename := fneTCExecutableFilename.FileName;
  gTotalCommanderConfigFilename := fneTCConfigFilename.FileName;
  gTotalCommanderToolbarPath := edOutputPathForToolbar.Text;
  {$ENDIF}
  FModificationTookPlace := False;
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
  ShowViewerByGlob(fneTCConfigFilename.FileName);
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

{ TfrmOptionsMisc.GenericSomethingChanged }
procedure TfrmOptionsMisc.GenericSomethingChanged(Sender: TObject);
begin
  FModificationTookPlace := True;
end;

{ TfrmOptionsMisc.CanWeClose }
function TfrmOptionsMisc.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := not FModificationTookPlace;

  if not Result then
  begin
    ShowOptions(TfrmOptionsMisc);
    Answer := MsgBox(rsMsgMiscellaneousModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
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


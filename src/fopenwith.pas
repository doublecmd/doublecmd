{
    Double Commander
    -------------------------------------------------------------------------
    Open with other application dialog

    Copyright (C) 2012  Alexander Koblov (alexx2000@mail.ru)

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

unit fOpenWith;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Buttons, ButtonPanel, ComCtrls, Menus, Types;

type

  { TfrmOpenWith }

  TfrmOpenWith = class(TForm)
    btnCommands: TSpeedButton;
    ButtonPanel: TButtonPanel;
    chkSaveAssociation: TCheckBox;
    chkCustomCommand: TCheckBox;
    chkUseAsDefault: TCheckBox;
    fneCommand: TFileNameEdit;
    ImageList: TImageList;
    lblMimeType: TLabel;
    miListOfURLs: TMenuItem;
    miSingleURL: TMenuItem;
    miListOfFiles: TMenuItem;
    miSingleFileName: TMenuItem;
    pnlOpenWith: TPanel;
    pmFieldCodes: TPopupMenu;
    tvApplications: TTreeView;
    procedure btnCommandsClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure chkCustomCommandChange(Sender: TObject);
    procedure chkSaveAssociationChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miFieldCodeClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tvApplicationsDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvApplicationsSelectionChanged(Sender: TObject);
  private
    FMimeType: UTF8String;
    FFileList: TStringList;
    procedure LoadApplicationList;
  public
    constructor Create(TheOwner: TComponent; AFileList: TStringList); reintroduce;
  end;

procedure ShowOpenWithDlg(const FileList: TStringList);

implementation

{$R *.lfm}

uses
  LCLProc, uOSUtils, uPixMapManager, uGlobs, uKeyFile, uMimeActions, uLng;

procedure ShowOpenWithDlg(const FileList: TStringList);
begin
  with TfrmOpenWith.Create(Application, FileList) do
  begin
    Show;
  end;
end;

{ TfrmOpenWith }

constructor TfrmOpenWith.Create(TheOwner: TComponent; AFileList: TStringList);
begin
  FFileList:= AFileList;
  inherited Create(TheOwner);
  InitPropStorage(Self);
end;

procedure TfrmOpenWith.FormCreate(Sender: TObject);
begin
  ImageList.Width:= gIconsSize;
  ImageList.Height:= gIconsSize;
  FMimeType:= GetFileMimeType(FFileList[0]);
  lblMimeType.Caption:= Format(lblMimeType.Caption, [FMimeType]);
  LoadApplicationList;
end;

procedure TfrmOpenWith.chkCustomCommandChange(Sender: TObject);
begin
  pnlOpenWith.Enabled:= chkCustomCommand.Checked;
end;

procedure TfrmOpenWith.chkSaveAssociationChange(Sender: TObject);
begin
  chkUseAsDefault.Enabled:= chkSaveAssociation.Checked;
end;

procedure TfrmOpenWith.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmOpenWith.btnCommandsClick(Sender: TObject);
begin
  pmFieldCodes.PopUp();
end;

procedure TfrmOpenWith.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOpenWith.FormDestroy(Sender: TObject);
begin
  FFileList.Free;
end;

procedure TfrmOpenWith.miFieldCodeClick(Sender: TObject);
begin
  fneCommand.Text:= fneCommand.Text + #32 + TMenuItem(Sender).Hint;
  fneCommand.SelStart:= UTF8Length(fneCommand.Text);
end;

procedure TfrmOpenWith.OKButtonClick(Sender: TObject);
var
  DesktopFile: PDesktopFileEntry;
  DesktopEntry: TDesktopFileEntry;
begin
  if chkCustomCommand.Checked then
    begin
      DesktopFile:= @DesktopEntry;
      DesktopEntry.MimeType:= FMimeType;
      DesktopEntry.ExecWithParams:= fneCommand.Text;
    end
  else if tvApplications.SelectionCount > 0 then
    begin
      DesktopFile:= PDesktopFileEntry(tvApplications.Selected.Data);
      fneCommand.Text:= DesktopFile^.DesktopFilePath;
    end;
  if Assigned(DesktopFile) then
  begin
    if chkSaveAssociation.Checked then
    begin
      if not AddDesktopEntry(FMimeType, fneCommand.Text, chkUseAsDefault.Checked) then
      begin
        MessageDlg(rsMsgErrSaveAssociation, mtError, [mbOK], 0);
      end;
    end;
    fneCommand.Text:= TranslateAppExecToCmdLine(DesktopFile, FFileList);
    ExecCmdFork(fneCommand.Text);
  end;
  Close;
end;

procedure TfrmOpenWith.tvApplicationsDeletion(Sender: TObject; Node: TTreeNode);
var
  DesktopFile: PDesktopFileEntry;
begin
  DesktopFile:= PDesktopFileEntry(Node.Data);
  Dispose(DesktopFile);
end;

procedure TfrmOpenWith.tvApplicationsSelectionChanged(Sender: TObject);
var
  DesktopFile: PDesktopFileEntry;
begin
  if tvApplications.SelectionCount > 0 then
  begin
    chkCustomCommand.Checked:= False;
    DesktopFile:= PDesktopFileEntry(tvApplications.Selected.Data);
    fneCommand.Text:= DesktopFile^.ExecWithParams;
  end;
end;

procedure TfrmOpenWith.LoadApplicationList;
const
  Folders: array [1..2] of UTF8String = ('/.local/share/applications',
                                         '/usr/share/applications');
var
  I, J: Integer;
  Bitmap: TBitmap;
  ImageIndex: PtrInt;
  TreeNode: TTreeNode;
  Applications: TStringList;
  DesktopFile: PDesktopFileEntry;
begin
  Folders[1]:= GetHomeDir + Folders[1];
  for I:= Low(Folders) to High(Folders) do
  begin
    Applications:= FindAllFiles(Folders[I], '*.desktop', True);
    for J:= 0 to Applications.Count - 1 do
    begin
      DesktopFile:= GetDesktopEntry(Applications[J]);
      if Assigned(DesktopFile) then
      begin
        if DesktopFile^.Hidden or (Pos('Screensaver', DesktopFile^.Categories) > 0) then
        begin
          Dispose(DesktopFile);
          Continue;
        end;
        TreeNode:= tvApplications.Items.AddChild(nil, DesktopFile^.DisplayName);
        TreeNode.Data:= DesktopFile;
        ImageIndex:= PixMapManager.GetIconByName(DesktopFile^.IconName);
        if ImageIndex >= 0 then
        begin
          Bitmap:= PixMapManager.GetBitmap(ImageIndex);
          if Assigned(Bitmap) then
          begin
            TreeNode.ImageIndex:= ImageList.Add(Bitmap, nil);
            TreeNode.SelectedIndex:= TreeNode.ImageIndex;
            TreeNode.StateIndex:= TreeNode.ImageIndex;
            Bitmap.Free;
          end;
        end;
      end;
    end;
    Applications.Free;
  end;
end;

end.


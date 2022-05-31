{
    Double Commander
    -------------------------------------------------------------------------
    Open with other application dialog

    Copyright (C) 2012-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit fOpenWith;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, EditBtn, Buttons, ButtonPanel, ComCtrls, Menus,
  Types;

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
    pnlFilter: TPanel;
    pnlOpenWith: TPanel;
    pmFieldCodes: TPopupMenu;
    tfeApplications: TTreeFilterEdit;
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
    FMimeType: String;
    FFileList: TStringList;
    procedure LoadApplicationList;
    function TreeNodeCompare(Node1, Node2: TTreeNode): Integer;
    procedure LoadBitmap(ANode: TTreeNode; const AName: String);
  public
    constructor Create(TheOwner: TComponent; AFileList: TStringList); reintroduce;
  end;

procedure ShowOpenWithDlg(TheOwner: TComponent; const FileList: TStringList);

implementation

{$R *.lfm}

uses
  LCLProc, DCStrUtils, uOSUtils, uPixMapManager, uGlobs, uMimeActions,
  uMimeType, uLng, LazUTF8, Math, uXdg, uGraphics;

const
  CATEGORY_OTHER = 11; // 'Other' category index

const
  // See https://specifications.freedesktop.org/menu-spec/latest
  CATEGORIES: array[0..12] of String =
  (
    'AudioVideo',
    'Audio',
    'Video',
    'Development',
    'Education',
    'Game',
    'Graphics',
    'Network',
    'Office',
    'Science',
    'Settings',
    'System',
    'Utility'
  );

procedure ShowOpenWithDlg(TheOwner: TComponent; const FileList: TStringList);
begin
  with TfrmOpenWith.Create(TheOwner, FileList) do
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
  with tvApplications do
  begin
    LoadBitmap(Items.AddChild(nil, rsOpenWithMultimedia), 'applications-multimedia');
    LoadBitmap(Items.AddChild(nil, rsOpenWithDevelopment), 'applications-development');
    LoadBitmap(Items.AddChild(nil, rsOpenWithEducation), 'applications-education');
    LoadBitmap(Items.AddChild(nil, rsOpenWithGames), 'applications-games');
    LoadBitmap(Items.AddChild(nil, rsOpenWithGraphics), 'applications-graphics');
    LoadBitmap(Items.AddChild(nil, rsOpenWithNetwork), 'applications-internet');
    LoadBitmap(Items.AddChild(nil, rsOpenWithOffice), 'applications-office');
    LoadBitmap(Items.AddChild(nil, rsOpenWithScience), 'applications-science');
    LoadBitmap(Items.AddChild(nil, rsOpenWithSettings), 'preferences-system');
    LoadBitmap(Items.AddChild(nil, rsOpenWithSystem), 'applications-system');
    LoadBitmap(Items.AddChild(nil, rsOpenWithUtility), 'applications-accessories');
    LoadBitmap(Items.AddChild(nil, rsOpenWithOther), 'applications-other');
  end;
  LoadApplicationList;
  tvApplications.CustomSort(@TreeNodeCompare);
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
  DesktopEntry: TDesktopFileEntry;
  DesktopFile: PDesktopFileEntry = nil;
begin
  if chkCustomCommand.Checked then
    begin
      DesktopFile:= @DesktopEntry;
      DesktopEntry.MimeType:= FMimeType;
      DesktopEntry.ExecWithParams:= fneCommand.Text;
    end
  else if tvApplications.SelectionCount > 0 then
    begin
      if Assigned(tvApplications.Selected.Data) then
      begin
        DesktopFile:= PDesktopFileEntry(tvApplications.Selected.Data);
        fneCommand.Text:= DesktopFile^.DesktopFilePath;
      end;
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
  if Assigned(Node.Data) then
  begin
    DesktopFile:= PDesktopFileEntry(Node.Data);
    Dispose(DesktopFile);
  end;
end;

procedure TfrmOpenWith.tvApplicationsSelectionChanged(Sender: TObject);
var
  DesktopFile: PDesktopFileEntry;
begin
  if tvApplications.SelectionCount > 0 then
  begin
    chkCustomCommand.Checked:= False;
    if (tvApplications.Selected.Data = nil) then
    begin
      DesktopFile:= nil;
      fneCommand.Text:= EmptyStr;
    end
    else begin
      DesktopFile:= PDesktopFileEntry(tvApplications.Selected.Data);
      fneCommand.Text:= DesktopFile^.ExecWithParams;
    end;
  end;
end;

procedure TfrmOpenWith.LoadApplicationList;
const
  APPS = 'applications';
var
  Folder: String;
  I, J, K: Integer;
  TreeNode: TTreeNode;
  Index, Count: Integer;
  DataDirs: TStringArray;
  DesktopFile: PDesktopFileEntry;
  Applications, Folders: TStringList;

  function GetCategoryIndex(const Category: String): Integer;
  var
    Index: Integer;
  begin
    Result:= CATEGORY_OTHER; // Default 'other' category
    for Index:= Low(CATEGORIES) to High(CATEGORIES) do
    begin
      if SameText(CATEGORIES[Index], Category) then
      begin
        if Index < 3 then
          Result:= 0
        else begin
          Result:= Index - 2;
        end;
        Break;
      end;
    end;
  end;

begin
  Folders:= TStringList.Create;
  Folders.CaseSensitive:= True;
  // $XDG_DATA_HOME
  Folder:= IncludeTrailingBackslash(GetUserDataDir) + APPS;
  if (Folders.IndexOf(Folder) < 0) then Folders.Add(Folder);
  // $XDG_DATA_DIRS
  DataDirs:= GetSystemDataDirs;
  for I:= Low(DataDirs) to High(DataDirs) do
  begin
    Folder:= IncludeTrailingBackslash(DataDirs[I]) + APPS;
    if (Folders.IndexOf(Folder) < 0) then Folders.Add(Folder);
  end;
  for I:= 0 to Folders.Count - 1 do
  begin
    Applications:= FindAllFiles(Folders[I], '*.desktop', True);
    for J:= 0 to Applications.Count - 1 do
    begin
      DesktopFile:= GetDesktopEntry(Applications[J]);
      if Assigned(DesktopFile) then
      begin
        if DesktopFile^.Hidden or Contains(DesktopFile^.Categories, 'Screensaver') then
        begin
          Dispose(DesktopFile);
          Continue;
        end;
        with DesktopFile^ do
        begin
          DesktopFilePath:= ExtractDirLevel(Folders[I] + PathDelim, DesktopFilePath);
          DesktopFilePath:= StringReplace(DesktopFilePath, PathDelim, '-', [rfReplaceAll]);
        end;

        // Determine application category
        Count:= Min(3, Length(DesktopFile^.Categories));
        if Count = 0 then
          Index:= CATEGORY_OTHER
        else begin
          for K:= 0 to Count - 1 do
          begin
            Index:= GetCategoryIndex(DesktopFile^.Categories[K]);
            if Index <> CATEGORY_OTHER then Break;
          end;
        end;

        TreeNode:= tvApplications.Items.TopLvlItems[Index];
        TreeNode:= tvApplications.Items.AddChild(TreeNode, DesktopFile^.DisplayName);
        TreeNode.Data:= DesktopFile;

        LoadBitmap(TreeNode, DesktopFile^.IconName);
      end;
    end;
    Applications.Free;
  end;
  Folders.Free;
  // Hide empty categories
  for Index:= 0 to tvApplications.Items.TopLvlCount - 1 do
  begin
    if tvApplications.Items.TopLvlItems[Index].Count = 0 then
      tvApplications.Items.TopLvlItems[Index].Visible:= False;
  end;
end;

function TfrmOpenWith.TreeNodeCompare(Node1, Node2: TTreeNode): Integer;
begin
  if SameText(Node1.Text, rsOpenWithOther) then
    Result:= +1
  else if SameText(Node2.Text, rsOpenWithOther) then
    Result:= -1
  else
    Result := LazUTF8.Utf8CompareStr(Node1.Text, Node2.Text);
end;

procedure TfrmOpenWith.LoadBitmap(ANode: TTreeNode; const AName: String);
var
  Bitmap: TBitmap;
  ImageIndex: PtrInt;
begin
  ImageIndex:= PixMapManager.GetIconByName(AName);
  if ImageIndex >= 0 then
  begin
    Bitmap:= PixMapManager.GetBitmap(ImageIndex);
    if Assigned(Bitmap) then
    begin
      BitmapCenter(Bitmap, ImageList.Width, ImageList.Height);
      ANode.ImageIndex:= ImageList.Add(Bitmap, nil);
      ANode.SelectedIndex:= ANode.ImageIndex;
      ANode.StateIndex:= ANode.ImageIndex;
      Bitmap.Free;
    end;
  end;
end;

end.


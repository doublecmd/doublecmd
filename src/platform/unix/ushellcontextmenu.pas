{
    Double Commander
    -------------------------------------------------------------------------
    Shell context menu implementation.

    Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)

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

unit uShellContextMenu;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Menus,
  uFile;

type

  { EContextMenuException }

  EContextMenuException = class(Exception);

  { TShellContextMenu }

  TShellContextMenu = class(TPopupMenu)
  private
    FFiles: TFiles;
    procedure ContextMenuSelect(Sender:TObject);
    procedure TemplateContextMenuSelect(Sender: TObject);
    procedure DriveContextMenuSelect(Sender:TObject);
    procedure OpenWithMenuItemSelect(Sender:TObject);
  public
    constructor Create(Owner: TWinControl; Path: UTF8String); reintroduce; overload;
    constructor Create(Owner: TWinControl; var Files : TFiles; Background: Boolean); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses
  LCLProc, Dialogs, IniFiles, Graphics, Unix, uTypes, uFindEx, uDCUtils,
  uOSUtils, uFileProcs, uShellExecute, uLng, uGlobs, uPixMapManager, uMyUnix,
  fMain, fFileProperties
  {$IFDEF LINUX}
  , uMimeActions
  {$ENDIF}
  ;

const
  sCmdVerbProperties = 'properties';

function GetGnomeTemplateMenu(out Items: TStringList): Boolean;
var
  userDirs: TStringList = nil;
  templateDir: UTF8String;
  searchRec: TSearchRecEx;
begin
  Result:= False;
  try
    userDirs:= TStringList.Create;
    userDirs.LoadFromFile(GetHomeDir + '.config/user-dirs.dirs');
    templateDir:= userDirs.Values['XDG_TEMPLATES_DIR'];
    if Length(templateDir) = 0 then Exit;
    templateDir:= IncludeTrailingPathDelimiter(mbExpandFileName(TrimQuotes(templateDir)));
    if mbDirectoryExists(templateDir) then
    begin
      if FindFirstEx(templateDir, faAnyFile, searchRec) = 0 then
      begin
        Items:= TStringList.Create;
        repeat
          // Skip directories
          if FPS_ISDIR(searchRec.Attr) then Continue;

          Items.Add(ExtractOnlyFileName(searchRec.Name) + '=' + templateDir + searchRec.Name);
        until FindNextEx(searchRec) <> 0;
        Result:= Items.Count > 0;
      end;
      FindCloseEx(searchRec);
    end;
  finally
    if Items.Count = 0 then
      FreeThenNil(Items);
    FreeThenNil(userDirs);
  end;
end;

function GetKdeTemplateMenu(out Items: TStringList): Boolean;
var
  I: Integer;
  desktopFile: TIniFile = nil;
  templateDir: array [0..1] of UTF8String;
  searchRec: TSearchRecEx;
  templateName,
  templatePath: UTF8String;
begin
  Result:= False;
  try
    templateDir[0]:= '/usr/share/templates';
    templateDir[1]:= GetHomeDir + '.kde/share/templates';
    for I:= Low(templateDir) to High(templateDir) do
    if mbDirectoryExists(templateDir[I]) then
    begin
      if FindFirstEx(templateDir[I] + PathDelim + '*.desktop', faAnyFile, searchRec) = 0 then
      begin
        Items:= TStringList.Create;
        repeat
          // Skip directories
          if FPS_ISDIR(searchRec.Attr) then Continue;

          try
            desktopFile:= TIniFile.Create(templateDir[I] + PathDelim + searchRec.Name);
            templateName:= desktopFile.ReadString('Desktop Entry', 'Name', EmptyStr);
            templatePath:= desktopFile.ReadString('Desktop Entry', 'URL', EmptyStr);
            templatePath:= GetAbsoluteFileName(templateDir[I] + PathDelim, templatePath);

            Items.Add(templateName + '=' + templatePath);
          finally
            FreeThenNil(desktopFile);
          end;
        until FindNextEx(searchRec) <> 0;
        Result:= Items.Count > 0;
      end;
      FindCloseEx(searchRec);
    end;
  finally
    if Items.Count = 0 then
      FreeThenNil(Items);
  end;
end;

function GetTemplateMenu(out Items: TStringList): Boolean;
begin
  case GetDesktopEnvironment of
  DE_KDE:
    Result:= GetKdeTemplateMenu(Items);
  else
    Result:= GetGnomeTemplateMenu(Items);
  end;
end;

(* handling user commands from context menu *)
procedure TShellContextMenu.ContextMenuSelect(Sender: TObject);
var
  sCmd: String;
begin
  // ShowMessage((Sender as TMenuItem).Hint);

  sCmd:= (Sender as TMenuItem).Hint;
  with frmMain.ActiveFrame do
  begin
    (*
    if (Pos('{!VFS}',sCmd)>0) and pnlFile.VFS.FindModule(ActiveDir + FileRecItem.sName) then
     begin
        pnlFile.LoadPanelVFS(@FileRecItem);
        Exit;
      end;
    *)

    if SameText(sCmd, sCmdVerbProperties) then
      ShowFileProperties(FileSource, FFiles);

    if not ProcessExtCommand(sCmd, CurrentPath) then
      frmMain.ExecCmd(sCmd);
  end;
end;

(* handling user commands from template context menu *)
procedure TShellContextMenu.TemplateContextMenuSelect(Sender: TObject);
var
  SelectedItem: TMenuItem;
  FileName: UTF8String;
begin
  // ShowMessage((Sender as TMenuItem).Hint);

  SelectedItem:= (Sender as TMenuItem);
  FileName:= SelectedItem.Caption;
  if InputQuery(rsMsgNewFile, rsMsgEnterName, FileName) then
    begin
      FileName:= FileName + ExtractFileExt(SelectedItem.Hint);
      if CopyFile(SelectedItem.Hint, frmMain.ActiveFrame.CurrentPath + FileName) then
        begin
          frmMain.ActiveFrame.Reload;
          frmMain.ActiveFrame.SetActiveFile(FileName);
        end;
    end;
end;

(* handling user commands from drive context menu *)
procedure TShellContextMenu.DriveContextMenuSelect(Sender:TObject);
var
  sExecCmd,
  sCmd, sPath: String;
  iPos: Integer;
begin
  // ShowMessage((Sender as TMenuItem).Hint);
  sCmd:= (Sender as TMenuItem).Hint;
  // mount drive
  iPos:= Pos('{!MOUNT}', sCmd);
  if iPos > 0 then
    sExecCmd:= 'mount ';
  // umount drive
  iPos:= Pos('{!UMOUNT}', sCmd);
  if iPos > 0 then
    sExecCmd:= 'umount ';
  // eject drive
  iPos:= Pos('{!EJECT}', sCmd);
  if iPos > 0 then
    sExecCmd:= 'eject ';
  // exit if command not found
  if sExecCmd = '' then Exit;
  // get path
  iPos:= Pos('}', sCmd);
  sPath:= Copy(sCmd, iPos + 1, Length(sCmd)-iPos);
  // execute command
  fpSystem(sExecCmd + sPath);
end;

procedure TShellContextMenu.OpenWithMenuItemSelect(Sender:TObject);
var
  ExecCmd: String;
begin
  ExecCmd := (Sender as TMenuItem).Hint;
  ExecCmdFork(ExecCmd);
end;

constructor TShellContextMenu.Create(Owner: TWinControl; Path: UTF8String);
var
  mi: TMenuItem;
begin
  inherited Create(Owner);

  mi:= TMenuItem.Create(Self);
  mi.Caption := rsMnuMount;
  if IsAvailable(Path) then
    begin
      mi.Enabled:= False;
    end
  else
    begin
      mi.Hint:= '{!MOUNT}' + Path;
      mi.OnClick:= Self.DriveContextMenuSelect;
    end;
  Self.Items.Add(mi);

  mi:=TMenuItem.Create(Self);
  mi.Caption:= rsMnuUmount;
  if not IsAvailable(Path) then
    begin
      mi.Enabled:= False;
    end
  else
    begin
      mi.Hint:= '{!UMOUNT}' + Path;
      mi.OnClick:= Self.DriveContextMenuSelect;
    end;
  Self.Items.Add(mi);

  mi:=TMenuItem.Create(Self);
  mi.Caption:= rsMnuEject;
  mi.Hint:= '{!EJECT}' + Path;
  mi.OnClick:= Self.DriveContextMenuSelect;
  Self.Items.Add(mi);
end;

constructor TShellContextMenu.Create(Owner: TWinControl; var Files: TFiles;
  Background: Boolean);
var
  aFile: TFile = nil;
  sl: TStringList = nil;
  I: Integer;
  bmpTemp: TBitmap;
  ImageIndex: PtrInt;
  sAct, sCmd: UTF8String;
  mi, miActions,
  miOpenWith, miSortBy: TMenuItem;
  FileNames: TStringList;
  DesktopEntries: TList = nil;
  AddActionsMenu: Boolean = False;
  AddOpenWithMenu: Boolean = False;
begin
  inherited Create(Owner);

  FFiles:= Files;

  try

    if not Background then
    begin

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actOpen;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Caption:='-';
    Self.Items.Add(mi);

    aFile := Files[0];
    if (Files.Count = 1) then
      begin
        miActions:=TMenuItem.Create(Self);
        miActions.Caption:= rsMnuActions;

        { Actions submenu }
        // Read actions from doublecmd.ext
        sl:=TStringList.Create;
        try
          if gExts.GetExtActions(aFile, sl) then
            begin
              AddActionsMenu := True;

              for I:= 0 to sl.Count - 1 do
                begin
                  sAct:= sl.Names[I];
                  if (CompareText('OPEN', sAct) = 0) or (CompareText('VIEW', sAct) = 0) or (CompareText('EDIT', sAct) = 0) then Continue;
                  sCmd:= sl.ValueFromIndex[I];
                  ReplaceExtCommand(sCmd, aFile, aFile.Path);
                  mi:= TMenuItem.Create(miActions);
                  mi.Caption:= sAct;
                  mi.Hint:= sCmd;
                  mi.OnClick:= Self.ContextMenuSelect; // handler
                  miActions.Add(mi);
                end;
            end;

          if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
            begin
              if sl.Count = 0 then
                AddActionsMenu := True
              else
                begin
                  // now add delimiter
                  mi:=TMenuItem.Create(miActions);
                  mi.Caption:='-';
                  miActions.Add(mi);
                end;

              // now add VIEW item
              mi:=TMenuItem.Create(miActions);
              mi.Caption:= rsMnuView;
              mi.Hint:= '{!VIEWER} ' + aFile.Path + aFile.Name;
              mi.OnClick:=Self.ContextMenuSelect; // handler
              miActions.Add(mi);

              // now add EDITconfigure item
              mi:=TMenuItem.Create(miActions);
              mi.Caption:= rsMnuEdit;
              mi.Hint:= '{!EDITOR} ' + aFile.Path + aFile.Name;
              mi.OnClick:=Self.ContextMenuSelect; // handler
              miActions.Add(mi);
            end;
        finally
          FreeAndNil(sl);
        end;

        if AddActionsMenu then
        begin
          //founded any commands
          Self.Items.Add(miActions);
       end;
       { /Actions submenu }
      end; // if count = 1

    {$IFDEF LINUX}
    //  Open with ...
    FileNames := TStringList.Create;
    try
      for i := 0 to Files.Count - 1 do
        FileNames.Add(Files[i].Path + Files[i].Name);

      DesktopEntries := GetDesktopEntries(FileNames);

      if Assigned(DesktopEntries) and (DesktopEntries.Count > 0) then
      begin
        miOpenWith := TMenuItem.Create(Self);
        miOpenWith.Caption := rsMnuOpenWith;
        Self.Items.Add(miOpenWith);
        AddOpenWithMenu := True;

        for i := 0 to DesktopEntries.Count - 1 do
        begin
          mi := TMenuItem.Create(miOpenWith);
          mi.Caption := PDesktopFileEntry(DesktopEntries[i])^.DisplayName;
          mi.Hint := PDesktopFileEntry(DesktopEntries[i])^.Exec;
          ImageIndex:= PixMapManager.GetIconByName(PDesktopFileEntry(DesktopEntries[i])^.IconName);
          if ImageIndex >= 0 then
            begin
              bmpTemp:= PixMapManager.GetBitmap(ImageIndex, clMenu);
              if Assigned(bmpTemp) then
                begin
                  mi.Bitmap.Assign(bmpTemp);
                  FreeAndNil(bmpTemp);
                end;
            end;
          mi.OnClick := Self.OpenWithMenuItemSelect;
          miOpenWith.Add(mi);
        end;
      end;

    finally
      FreeAndNil(FileNames);
      if Assigned(DesktopEntries) then
      begin
        for i := 0 to DesktopEntries.Count - 1 do
          Dispose(PDesktopFileEntry(DesktopEntries[i]));
        FreeAndNil(DesktopEntries);
      end;
    end;
    {$ENDIF}

    // Add separator after actions and openwith menu.
    if AddActionsMenu or AddOpenWithMenu then
    begin
      mi:=TMenuItem.Create(Self);
      mi.Caption:='-';
      Self.Items.Add(mi);
    end;

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actRename;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actCopy;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actDelete;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actRenameOnly;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Caption:='-';
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actCutToClipboard;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actCopyToClipboard;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actPasteFromClipboard;
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Caption:='-';
    Self.Items.Add(mi);

    mi:=TMenuItem.Create(Self);
    mi.Action := frmMain.actFileProperties;
    Self.Items.Add(mi);
    end
    else
      begin
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actRefresh;
        Self.Items.Add(mi);

        // Add "Sort by" submenu
        miSortBy := TMenuItem.Create(Self);
        miSortBy.Caption := rsMnuSortBy;
        Self.Items.Add(miSortBy);

        mi:=TMenuItem.Create(miSortBy);
        mi.Action := frmMain.actSortByName;
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(miSortBy);
        mi.Action := frmMain.actSortByExt;
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(miSortBy);
        mi.Action := frmMain.actSortBySize;
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(miSortBy);
        mi.Action := frmMain.actSortByDate;
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(miSortBy);
        mi.Action := frmMain.actSortByAttr;
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(miSortBy);
        mi.Caption := '-';
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(miSortBy);
        mi.Action := frmMain.actReverseOrder;
        miSortBy.Add(mi);

        mi:=TMenuItem.Create(Self);
        mi.Caption:='-';
        Self.Items.Add(mi);

        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actPasteFromClipboard;
        Self.Items.Add(mi);

        if GetTemplateMenu(sl) then
        begin
          mi:=TMenuItem.Create(Self);
          mi.Caption:='-';
          Self.Items.Add(mi);

          // Add "New" submenu
          miSortBy := TMenuItem.Create(Self);
          miSortBy.Caption := rsMnuNew;
          Self.Items.Add(miSortBy);

          for I:= 0 to sl.Count - 1 do
          begin
            mi:=TMenuItem.Create(miSortBy);
            mi.Caption:= sl.Names[I];
            mi.Hint:= sl.ValueFromIndex[I];
            mi.OnClick:= Self.TemplateContextMenuSelect;
            miSortBy.Add(mi);
          end;
          FreeThenNil(sl);
        end;

        mi:=TMenuItem.Create(Self);
        mi.Caption:='-';
        Self.Items.Add(mi);

        mi:=TMenuItem.Create(Self);
        mi.Caption:= frmMain.actFileProperties.Caption;
        mi.Hint:= sCmdVerbProperties;
        mi.OnClick:= Self.ContextMenuSelect;
        Self.Items.Add(mi);
      end;
  finally
    Files:= nil;
  end;
end;

destructor TShellContextMenu.Destroy;
begin
  FreeThenNil(FFiles);
  inherited Destroy;
end;

end.


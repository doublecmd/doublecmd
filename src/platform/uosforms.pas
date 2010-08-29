{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

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

unit uOSForms;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, ExtDlgs, uFile, uFileSource,
  {$IFDEF UNIX}
  Graphics, BaseUnix, Unix, fFileProperties, uPixMapManager;
  {$ELSE}
  FileUtil, Windows, ShlObj, uShlObjAdditional, JwaDbt, uMyWindows;
  {$ENDIF}

const
  sCmdVerbOpen = 'open';
  sCmdVerbRename = 'rename';
  sCmdVerbDelete = 'delete';
  sCmdVerbCut = 'cut';
  sCmdVerbCopy = 'copy';
  sCmdVerbPaste = 'paste';
  sCmdVerbLink = 'link';

type
  EContextMenuException = class(Exception);

{$IFDEF UNIX}
type
  TContextMenu = class(TPopupMenu)
    procedure ContextMenuSelect(Sender:TObject);
    procedure DriveContextMenuSelect(Sender:TObject);
    procedure OpenWithMenuItemSelect(Sender:TObject);
  end;
{$ENDIF}

{en
   Replace window procedure
   @param(Handle Window handle)
}
procedure SetMyWndProc(Handle : THandle);
{en
   Show file/folder properties dialog
   @param(Files List of files to show properties for)
}
procedure ShowFilePropertiesDialog(aFileSource: IFileSource; const Files: TFiles);
{en
   Show file/folder context menu
   @param(Owner Parent window)
   @param(Files List of files to show context menu for. It is freed by this function.)
   @param(X X coordinate)
   @param(Y Y coordinate)
}
procedure ShowContextMenu(Owner: TWinControl; var Files : TFiles; X, Y : Integer);
{en
   Show drive context menu
   @param(Owner Parent window)
   @param(sPath Path to drive)
   @param(X X coordinate)
   @param(Y Y coordinate)
}
procedure ShowDriveContextMenu(Owner: TWinControl; sPath: String; X, Y : Integer);
{en
   Show open icon dialog
   @param(Owner Owner)
   @param(sFileName Icon file name)
   @returns(The function returns @true if successful, @false otherwise)
}
function ShowOpenIconDialog(Owner: TCustomControl; var sFileName : String) : Boolean;

implementation

uses
  fMain, uOSUtils, uGlobs, uLng, uShellExecute
  {$IF DEFINED(MSWINDOWS)}
  , Dialogs, Graphics, comobj, uShellContextMenu, uFileSystemFileSource, uTotalCommander
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  , uFileSystemWatcher, inotify, uMimeActions
  {$ENDIF}
  ;

var
{$IFDEF MSWINDOWS}
  OldWProc: WNDPROC;
  ShellContextMenu: IContextMenu2 = nil;
{$ELSE}
  CM : TContextMenu = nil;
{$ENDIF}

{$IFDEF WIN64}
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall; external 'user32' name 'SetWindowLongPtrW';
{$ENDIF}

{$IFDEF MSWINDOWS}
function MyWndProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  case uiMsg of
    (* For working with submenu of context menu *)
    WM_INITMENUPOPUP,
    WM_DRAWITEM,
    WM_MENUCHAR,
    WM_MEASUREITEM:
      if Assigned(ShellContextMenu) then
        begin
          ShellContextMenu.HandleMenuMsg(uiMsg, wParam, lParam);
          Result := 0;
        end
      else
        Result := CallWindowProc(OldWProc, hWnd, uiMsg, wParam, lParam);
        
    WM_DEVICECHANGE:
      if (wParam = DBT_DEVICEARRIVAL) or (wParam = DBT_DEVICEREMOVECOMPLETE) then
        frmMain.UpdateDiskCount;
  else
    Result := CallWindowProc(OldWProc, hWnd, uiMsg, wParam, lParam);
  end; // case
end;
{$ENDIF}

procedure SetMyWndProc(Handle : THandle);
{$IFDEF MSWINDOWS}
begin
  OldWProc := WNDPROC(SetWindowLong(Handle, GWL_WNDPROC, LONG_PTR(@MyWndProc)));
  CreateTotalCommanderWindow(Handle);
end;
{$ELSE}
begin
  if fpGetUID = 0 then // if run under root
    frmMain.Caption:= frmMain.Caption + ' - ROOT PRIVILEGES';
end;
{$ENDIF}

{$IFDEF UNIX}
(* handling user commands from context menu *)
procedure TContextMenu.ContextMenuSelect(Sender: TObject);
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
    if not ProcessExtCommand(sCmd, CurrentPath) then
      frmMain.ExecCmd(sCmd);
  end;
end;

(* handling user commands from drive context menu *)
procedure TContextMenu.DriveContextMenuSelect(Sender:TObject);
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

procedure TContextMenu.OpenWithMenuItemSelect(Sender:TObject);
var
  ExecCmd: String;
begin
  ExecCmd := (Sender as TMenuItem).Hint;
  ExecCmdFork(ExecCmd);
end;
{$ENDIF}

procedure ShowContextMenu(Owner: TWinControl; var Files : TFiles; X, Y : Integer);
{$IFDEF MSWINDOWS}
const
  USER_CMD_ID = $1000;
var
  aFile: TFile;
  sl: TStringList = nil;
  i:Integer;
  sAct, sCmd: UTF8String;
  menu: HMENU = 0;
  hActionsSubMenu: HMENU = 0;
  cmd: UINT = 0;
  iCmd: Integer;
  cmici: TCMINVOKECOMMANDINFO;
  bHandled : Boolean = False;
  ZVerb: array[0..255] of char;
  sVerb : String;
begin

  try
    try
      if Files.Count = 0 then Exit;

      ShellContextMenu := TShellContextMenu.Create(Owner.Handle, Files, True);
      if Assigned(ShellContextMenu) then
      try
        menu := CreatePopupMenu;
        OleCheckUTF8(ShellContextMenu.QueryContextMenu(menu, 0, 1, USER_CMD_ID - 1, CMF_EXPLORE or CMF_CANRENAME));
        //------------------------------------------------------------------------------
        { Actions submenu }
        aFile := Files[0];
        if (Files.Count = 1) then
          begin
            hActionsSubMenu := CreatePopupMenu;

            // Read actions from doublecmd.ext
            sl:=TStringList.Create;

            if gExts.GetExtActions(aFile, sl) then
              begin
                for I:= 0 to sl.Count - 1 do
                  begin
                    sAct:= sl.Names[I];
                    if (CompareText('OPEN', sAct) = 0) or (CompareText('VIEW', sAct) = 0) or (CompareText('EDIT', sAct) = 0) then Continue;
                    InsertMenuItemEx(hActionsSubMenu,0, PWChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);
                  end;
              end;

            if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
              begin
                // Add separator if needed.
                if GetMenuItemCount(hActionsSubMenu) > 0 then
                  InsertMenuItemEx(hActionsSubMenu,0, nil, 0, 0, MFT_SEPARATOR);

                // now add VIEW item
                sCmd:= '{!VIEWER} ' + aFile.Path + aFile.Name;
                I := sl.Add(sCmd);
                InsertMenuItemEx(hActionsSubMenu,0, PWChar(UTF8Decode(rsMnuView)), 1, I + USER_CMD_ID, MFT_STRING);

                // now add EDIT item
                sCmd:= '{!EDITOR} ' + aFile.Path + aFile.Name;
                I := sl.Add(sCmd);
                InsertMenuItemEx(hActionsSubMenu,0, PWChar(UTF8Decode(rsMnuEdit)), 1, I + USER_CMD_ID, MFT_STRING);
              end;

            // Add Actions submenu if not empty.
            if GetMenuItemCount(hActionsSubMenu) > 0 then
              InsertMenuItemEx(menu, hActionsSubMenu, PWChar(UTF8Decode(rsMnuActions)), 0, 333, MFT_STRING)
          end;
        { /Actions submenu }
        //------------------------------------------------------------------------------
        cmd := UINT(TrackPopupMenu(menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, X, Y, 0, Owner.Handle, nil));
      finally
        if hActionsSubMenu <> 0 then
          DestroyMenu(hActionsSubMenu);
        if menu <> 0 then
          DestroyMenu(menu);
      end;

      if (cmd > 0) and (cmd < USER_CMD_ID) then
        begin
          iCmd := LongInt(Cmd) - 1;
          if Succeeded(ShellContextMenu.GetCommandString(iCmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb))) then
            begin
              sVerb := StrPas(ZVerb);

              if SameText(sVerb, sCmdVerbDelete) then
                begin
                  if ssShift in GetKeyShiftState then
                    Actions.cm_Delete('recyclesettingrev')
                  else
                    Actions.cm_Delete('recyclesetting');
                  bHandled := True;
                end
              else if SameText(sVerb, sCmdVerbRename) then
                begin
                  if Files.Count = 1 then
                    with Files[0] do
                    begin
                      if Name <> (ExtractFileDrive(Name)+PathDelim) then
                        frmMain.actRenameOnly.Execute
                      else  // change drive label
                        begin
                          sCmd:= mbGetVolumeLabel(Name, True);
                          if InputQuery(rsMsgSetVolumeLabel, rsMsgVolumeLabel, sCmd) then
                            mbSetVolumeLabel(Name, sCmd);
                        end;
                    end
                  else
                    frmMain.actRename.Execute;
                  bHandled := True;
                end
              else if SameText(sVerb, sCmdVerbOpen) then
                begin
                  if Files.Count = 1 then
                    with Files[0] do
                    begin
                      if IsDirectory or IsLinkToDirectory then
                        begin
                          if Name = '..' then
                            frmMain.ActiveFrame.ChangePathToParent(True)
                          else
                            frmMain.ActiveFrame.ChangePathToChild(Files[0]);
                          bHandled := True;
                        end; // is dir
                    end; // with
                end
              else if SameText(sVerb, sCmdVerbCut) then
                begin
                  frmMain.actCutToClipboard.Execute;
                  bHandled := True;
                end
              else if SameText(sVerb, sCmdVerbCopy) then
                begin
                  frmMain.actCopyToClipboard.Execute;
                  bHandled := True;
                end
              else if SameText(sVerb, sCmdVerbPaste) then
                begin
                  frmMain.actPasteFromClipboard.Execute;
                  bHandled := True;
                end;
            end;

          if not bHandled then
            begin
              FillChar(cmici, SizeOf(cmici), #0);
              with cmici do
              begin
                cbSize := sizeof(cmici);
                hwnd := Owner.Handle;
                lpVerb := PChar(cmd - 1);
                nShow := SW_NORMAL;
              end;
              OleCheckUTF8(ShellContextMenu.InvokeCommand(cmici));

              // Reload after possible changes on the filesystem.
              if SameText(sVerb, sCmdVerbLink) then
                frmMain.ActiveFrame.FileSource.Reload(frmMain.ActiveFrame.CurrentPath);
            end;

        end // if cmd > 0
      else if (cmd >= USER_CMD_ID) then // actions sub menu
        begin
          sCmd:= sl.Strings[cmd - USER_CMD_ID];
          sCmd:= Copy(sCmd, Pos('=', sCmd) + 1, Length(sCmd));
          ReplaceExtCommand(sCmd, aFile, aFile.Path);
          try
            with frmMain.ActiveFrame do
            begin
  (*
    VFS via another file source

              if (Pos('{!VFS}',sCmd)>0) and pnlFile.VFS.FindModule(CurrentPath + fri.sName) then
                begin
                  pnlFile.LoadPanelVFS(@fri);
                  Exit;
                end;
  *)
              if not ProcessExtCommand(sCmd, CurrentPath) then
                frmMain.ExecCmd(sCmd);
            end;
          finally
            bHandled:= True;
          end;
        end;
    finally
      ShellContextMenu := nil;
      FreeAndNil(Files);
      if Assigned(sl) then
        FreeAndNil(sl);
    end;

  except
    on e: EOleError do
      raise EContextMenuException.Create(e.Message);
  end;
end;
{$ELSE}
var
  aFile: TFile;
  sl: TStringList;
  I: Integer;
  bmpTemp: TBitmap;
  ImageIndex: PtrInt;
  sAct, sCmd: UTF8String;
  mi, miActions, miOpenWith: TMenuItem;
  FileNames: TStringList;
  DesktopEntries: TList = nil;
  AddActionsMenu: Boolean = False;
  AddOpenWithMenu: Boolean = False;
begin
  if Files.Count = 0 then
  begin
    FreeAndNil(Files);
    Exit;
  end;

  try
    if not Assigned(CM) then
      CM:= TContextMenu.Create(Owner)
    else
      CM.Items.Clear;

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actOpen;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Caption:='-';
    CM.Items.Add(mi);

    aFile := Files[0];
    if (Files.Count = 1) then
      begin
        miActions:=TMenuItem.Create(CM);
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
                  mi.OnClick:= CM.ContextMenuSelect; // handler
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
              mi.OnClick:=CM.ContextMenuSelect; // handler
              miActions.Add(mi);

              // now add EDITconfigure item
              mi:=TMenuItem.Create(miActions);
              mi.Caption:= rsMnuEdit;
              mi.Hint:= '{!EDITOR} ' + aFile.Path + aFile.Name;
              mi.OnClick:=CM.ContextMenuSelect; // handler
              miActions.Add(mi);
            end;
        finally
          FreeAndNil(sl);
        end;

        if AddActionsMenu then
        begin
          //founded any commands
          CM.Items.Add(miActions);
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
        miOpenWith := TMenuItem.Create(CM);
        miOpenWith.Caption := rsMnuOpenWith;
        CM.Items.Add(miOpenWith);
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
          mi.OnClick := CM.OpenWithMenuItemSelect;
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
      mi:=TMenuItem.Create(CM);
      mi.Caption:='-';
      CM.Items.Add(mi);
    end;

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actRename;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actCopy;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actDelete;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actRenameOnly;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Caption:='-';
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actCutToClipboard;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actCopyToClipboard;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actPasteFromClipboard;
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Caption:='-';
    CM.Items.Add(mi);

    mi:=TMenuItem.Create(CM);
    mi.Action := frmMain.actFileProperties;
    CM.Items.Add(mi);

    CM.PopUp(X, Y);

  finally
    FreeAndNil(Files);
  end;
end;
{$ENDIF}

procedure ShowDriveContextMenu(Owner: TWinControl; sPath: String; X, Y : Integer);
{$IFDEF MSWINDOWS}
var
  aFile: TFile;
  Files: TFiles;
  OldErrorMode: Word;
begin
  aFile := TFileSystemFileSource.CreateFile(EmptyStr);
  aFile.FullPath := sPath;
  Files:= TFiles.Create(EmptyStr); // free in ShowContextMenu
  Files.Add(aFile);
  OldErrorMode:= SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  ShowContextMenu(Owner, Files, X, Y);
  SetErrorMode(OldErrorMode);
end;
{$ELSE}
var
  mi: TMenuItem;
begin
  if not Assigned(CM) then
    CM:= TContextMenu.Create(Owner)
  else
    CM.Items.Clear;

  mi:= TMenuItem.Create(CM);
  mi.Caption := rsMnuMount;
  if IsAvailable(sPath) then
    begin
      mi.Enabled:= False;
    end
  else
    begin
      mi.Hint:= '{!MOUNT}' + sPath;
      mi.OnClick:= CM.DriveContextMenuSelect;
    end;
  CM.Items.Add(mi);

  mi:=TMenuItem.Create(CM);
  mi.Caption:= rsMnuUmount;
  if not IsAvailable(sPath) then
    begin
      mi.Enabled:= False;
    end
  else
    begin
      mi.Hint:= '{!UMOUNT}' + sPath;
      mi.OnClick:= CM.DriveContextMenuSelect;
    end;
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:= rsMnuEject;
  mi.Hint:= '{!EJECT}' + sPath;
  mi.OnClick:= CM.DriveContextMenuSelect;
  CM.Items.Add(mi);

  // show context menu
  CM.PopUp(X, Y);
end;
{$ENDIF}

(* Show file properties dialog *)
procedure ShowFilePropertiesDialog(aFileSource: IFileSource; const Files: TFiles);
{$IFDEF UNIX}
begin
  ShowFileProperties(aFileSource, Files);
end;
{$ELSE}
var
  cmici: TCMINVOKECOMMANDINFO;
  contMenu: IContextMenu2;
begin
  if Files.Count = 0 then Exit;

  try
    contMenu := TShellContextMenu.Create(frmMain.Handle, Files, False);
    if Assigned(contMenu) then
    begin
      FillChar(cmici, sizeof(cmici), #0);
      with cmici do
        begin
          cbSize := sizeof(cmici);
          hwnd := frmMain.Handle;
          lpVerb := 'properties';
          nShow := SW_SHOWNORMAL;
        end;

      OleCheckUTF8(contMenu.InvokeCommand(cmici));
    end;

  except
    on e: EOleError do
      raise EContextMenuException.Create(e.Message);
  end;
end;
{$ENDIF}

function ShowOpenIconDialog(Owner: TCustomControl; var sFileName : String) : Boolean;
var
  opdDialog : TOpenPictureDialog;
{$IFDEF MSWINDOWS}
  sFilter : String;
  iPos,
  iIconIndex: Integer;
  bAlreadyOpen : Boolean;
{$ENDIF}
begin
  opdDialog := nil;
{$IFDEF MSWINDOWS}
  sFilter := GraphicFilter(TGraphic)+'|'+ 'Programs and Libraries (*.exe;*.dll)|*.exe;*.dll'+'|'+
                       Format('All files (%s)|%s',[GetAllFilesMask, GetAllFilesMask]);
  bAlreadyOpen := False;
  iPos :=Pos(',', sFileName);
  if iPos <> 0 then
    begin
      iIconIndex := StrToIntDef(Copy(sFileName, iPos + 1, Length(sFileName) - iPos), 0);
      sFileName := Copy(sFileName, 1, iPos - 1);
    end
  else
    begin
      opdDialog := TOpenPictureDialog.Create(Owner);
      opdDialog.Filter:= sFilter;;
      Result:= opdDialog.Execute;
      sFileName := opdDialog.FileName;
      bAlreadyOpen := True;
    end;

  if FileIsExeLib(sFileName) then
    begin
      Result := SHChangeIconDialog(Owner.Handle, sFileName, iIconIndex);
      if Result then
        sFileName := sFileName + ',' + IntToStr(iIconIndex);
    end
  else if not bAlreadyOpen then
{$ENDIF}
    begin
      opdDialog := TOpenPictureDialog.Create(Owner);
{$IFDEF MSWINDOWS}
      opdDialog.Filter:= sFilter;
{$ENDIF}
      Result:= opdDialog.Execute;
      sFileName := opdDialog.FileName;
{$IFDEF MSWINDOWS}
      bAlreadyOpen := True;
{$ENDIF}
    end;
  if Assigned(opdDialog) then
    FreeAndNil(opdDialog);
end;

{$IFDEF LINUX}
var
  FileSystemWatcher: TFileSystemWatcher = nil;
type
  TFakeClass = class
    procedure OnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
  end;

procedure TFakeClass.OnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
var
  ev: pinotify_event absolute NotifyData;
begin
  if (ev^.mask = IN_DELETE) and (Pos('mtab', PChar(@ev^.name)) = 1) then
    frmMain.UpdateDiskCount;
end;

initialization
  FileSystemWatcher:= TFileSystemWatcher.Create(nil, '/etc', [wfFileNameChange]);
  FileSystemWatcher.OnWatcherNotifyEvent:= TFakeClass.OnWatcherNotifyEvent;
  FileSystemWatcher.Active:= True;
finalization
  if Assigned(FileSystemWatcher) then
    FreeAndNil(FileSystemWatcher);
{$ENDIF}

end.


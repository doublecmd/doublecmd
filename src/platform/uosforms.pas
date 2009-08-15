{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, uTypes, uFileList, Menus, Controls, Graphics, ExtDlgs, Dialogs,
  uFile,
  {$IFDEF UNIX}
  BaseUnix, Unix, fFileProperties;
  {$ELSE}
  FileUtil, Windows, ShlObj, ActiveX, uShlObjAdditional,
  uMyWindows, JwaShlGuid, JwaDbt, JwaWinUser;
  {$ENDIF}
const
  sCmdVerbOpen = 'open';
  sCmdVerbRename = 'rename';
  sCmdVerbDelete = 'delete';
  sCmdVerbCut = 'cut';
  sCmdVerbCopy = 'copy';
  sCmdVerbPaste = 'paste';

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
   @param(FileList List of files)
   @param(aPath Current file path)
}  
procedure ShowFilePropertiesDialog(const Files: TFiles; const aPath:String);
{en
   Show file/folder context menu
   @param(Owner Parent window)
   @param(FileList List of files. It is freed by this function.)
   @param(X X coordinate)
   @param(Y Y coordinate)
}
procedure ShowContextMenu(Owner: TWinControl; Files : TFiles; X, Y : Integer);
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
  LCLProc, fMain, uOSUtils, uGlobs, uLng, uDCUtils, uShellExecute
  {$IF DEFINED(MSWINDOWS)}
  , uFileSystemFile
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  , uFileSystemWatcher, inotify, uMimeActions
  {$ENDIF}
  ;

var
{$IFDEF MSWINDOWS}
  OldWProc: WNDPROC;
  ICM2: IContextMenu2 = nil;
{$ELSE}
  CM : TContextMenu = nil;
  FileItem: TFile;
{$ENDIF}

{$IFDEF WIN64}
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall; external 'user32' name 'SetWindowLongPtrW';
{$ENDIF}

{$IFDEF MSWINDOWS}
function MyWndProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  case uiMsg of
    (* For working with submenu of contex menu *)
    WM_INITMENUPOPUP,
    WM_DRAWITEM,
    WM_MENUCHAR,
    WM_MEASUREITEM:
      if Assigned(ICM2) then
        begin
          ICM2.HandleMenuMsg(uiMsg, wParam, lParam);
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
(*
  sCmd:= (Sender as TMenuItem).Hint;
  with frmMain.ActiveFrame do
  begin
    if (Pos('{!VFS}',sCmd)>0) and pnlFile.VFS.FindModule(ActiveDir + FileRecItem.sName) then
     begin
        pnlFile.LoadPanelVFS(@FileRecItem);
        Exit;
      end;
    if not ProcessExtCommand(sCmd, pnlFile.ActiveDir) then
      frmMain.ExecCmd(sCmd);
  end;
*)
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

{$IFDEF MSWINDOWS}
function InsertMenuItemEx(hMenu, SubMenu: HMENU; Caption: PWChar;
                         Position, ItemID,  ItemType : UINT): boolean;
var
  mi: TMenuItemInfoW;
begin
   with mi do
   begin
      cbSize := SizeOf(mi);
      fMask := MIIM_STATE or MIIM_TYPE or MIIM_SUBMENU or MIIM_ID;
      fType := ItemType;
      fState := MFS_ENABLED;
      wID := ItemID;
      hSubMenu := SubMenu;
      dwItemData := 0;
      dwTypeData := Caption;
      cch := SizeOf(Caption);
   end;
   Result := InsertMenuItemW(hMenu, Position, false, mi);
end;

function GetIContextMenu(Handle : THandle; Files : TFiles): IContextMenu;
type
  TPIDLArray = array[0..0] of PItemIDList;
  PPIDLArray = ^TPIDLArray;

var
  Folder,
  DesktopFolder: IShellFolder;
  PathPIDL,
  tmpPIDL: PItemIDList;
  malloc: IMalloc;
  S: WideString;
  List: PPIDLArray;
  I : Integer;
  pchEaten,
  dwAttributes: ULONG;
begin
  Result := nil;
  if not Succeeded(SHGetMalloc(malloc)) then Exit;
  if not Succeeded(SHGetDesktopFolder(DeskTopFolder)) then Exit;

  try
    List := malloc.Alloc(SizeOf(PItemIDList)*Files.Count);
    for I := 0 to Files.Count - 1 do
      begin
      //**********   if s <> sPath then
        S := UTF8Decode(Files[I].Path);
        
        OleCheck(DeskTopFolder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, PathPIDL, dwAttributes));
        try
          OleCheck(DeskTopFolder.BindToObject(PathPIDL, nil, IID_IShellFolder, Folder));
        finally
          malloc.Free(PathPIDL);
        end;
      //*****************

        S:=UTF8Decode(Files[I].Name);
        OleCheck(Folder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, tmpPIDL, dwAttributes));
        List^[i] := tmpPIDL;
      end;

    Folder.GetUIObjectOf(Handle, Files.Count, PItemIDList(List^), IID_IContextMenu, nil, Result);
  finally
    for I := 0 to Files.Count - 1 do
      malloc.Free(List^[i]);
    malloc.Free(List);
  end;
end;
{$ENDIF}

procedure ShowContextMenu(Owner: TWinControl; Files : TFiles; X, Y : Integer);
{$IFDEF MSWINDOWS}
var
  aFile: TFile;
  sl: TStringList = nil;
  i:Integer;
  sCmd:String;  
  contMenu: IContextMenu;
  menu: HMENU = 0;
  hActionsSubMenu: HMENU = 0;
  cmd: UINT = 0;
  iCmd: Integer;
  HR: HResult;
  cmici: TCMINVOKECOMMANDINFO;
  bHandled : Boolean;
  ZVerb: array[0..255] of char;
  sVerb : String;
begin

  try
    if Files.Count = 0 then Exit;

    contMenu := GetIContextMenu(Owner.Handle, Files);
    menu := CreatePopupMenu;
    try
      OleCheck( contMenu.QueryContextMenu(menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME) );
      contMenu.QueryInterface(IID_IContextMenu2, ICM2); // to handle submenus.
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
            //founded any commands
              InsertMenuItemEx(menu, hActionsSubMenu, PWChar(UTF8Decode(rsMnuActions)), 0, 333, MFT_STRING);
              for i:=0 to sl.Count-1 do
                begin
                  sCmd:=sl.Strings[i];
                  if pos('VIEW=',sCmd)>0 then Continue;  // view command is only for viewer
                  ReplaceExtCommand(sCmd, aFile, aFile.Path);

                  sCmd:= RemoveQuotation(sCmd);
                  InsertMenuItemEx(hActionsSubMenu,0, PWChar(UTF8Decode(sCmd)), 0, I + $1000, MFT_STRING);
                end;
            end;

          if not (aFile.IsDirectory or aFile.IsLinkToDirectory) then
            begin
              if sl.Count = 0 then
                InsertMenuItemEx(menu, hActionsSubMenu, PWChar(UTF8Decode(rsMnuActions)), 0, 333, MFT_STRING)
              else
                // now add delimiter
                InsertMenuItemEx(hActionsSubMenu,0, nil, 0, 0, MFT_SEPARATOR);

              // now add VIEW item
              sCmd:= '{!VIEWER}' + aFile.Path + aFile.Name;
              I := sl.Add(sCmd);
              InsertMenuItemEx(hActionsSubMenu,0, PWChar(UTF8Decode(sCmd)), 1, I + $1000, MFT_STRING);

              // now add EDITconfigure item
              sCmd:= '{!EDITOR}' + aFile.Path + aFile.Name;
              I := sl.Add(sCmd);
              InsertMenuItemEx(hActionsSubMenu,0, PWChar(UTF8Decode(sCmd)), 1, I + $1000, MFT_STRING);
            end;
        end;
      { /Actions submenu }
      //------------------------------------------------------------------------------
      cmd := UINT(TrackPopupMenu(menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, X, Y, 0, Owner.Handle, nil));
    finally
      if hActionsSubMenu <> 0 then
        DestroyMenu(hActionsSubMenu);
      if menu <> 0 then
        DestroyMenu(menu);
      ICM2 := nil;
    end;

    if (cmd > 0) and (cmd < $1000) then
      begin
        iCmd := LongInt(Cmd) - 1;
        HR := contMenu.GetCommandString(iCmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb));
        sVerb := StrPas(ZVerb);
        bHandled := False;

        if SameText(sVerb, sCmdVerbDelete) then
          begin
            frmMain.actDelete.Execute;
            bHandled := True;
          end
        else if SameText(sVerb, sCmdVerbRename) then
          begin
            if Files.Count = 1 then
              with Files[0] do
                begin
                  DebugLn(Name);
                  DebugLn(ExtractFileDrive(Name));
                  if Name <> (ExtractFileDrive(Name)+PathDelim) then
                    frmMain.RenameFile('')
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
{
  Do this via Actions not by directly using ActiveFrame.

                      if sName = '..' then
                        frmMain.ActiveFrame.pnlFile.cdUpLevel
                      else
                        frmMain.ActiveFrame.pnlFile.cdDownLevel(FileList.GetItem(0));
}
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
            OleCheck( contMenu.InvokeCommand(cmici) );
          end;

        if SameText(sVerb, sCmdVerbDelete) or SameText(sVerb, sCmdVerbPaste) then
          frmMain.ActiveFrame.Reload;

      end // if cmd > 0
    else if (cmd >= $1000) then // actions sub menu
      begin
        sCmd:= sl.Strings[cmd - $1000];
        ReplaceExtCommand(sCmd, aFile, aFile.Path);
        sCmd:= Copy(sCmd, pos('=',sCmd)+1, length(sCmd));
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
    FreeAndNil(Files);
    if Assigned(sl) then
      FreeAndNil(sl);
  end;
end;
{$ELSE}
var
  aFile: TFile;
  sl: TStringList;
  i: Integer;
  sCmd: String;
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
      FileItem:= aFile;
      miActions:=TMenuItem.Create(CM);
      miActions.Caption:= rsMnuActions;

      { Actions submenu }
      // Read actions from doublecmd.ext
      sl:=TStringList.Create;
      try
        if gExts.GetExtActions(aFile, sl) then
          begin
            AddActionsMenu := True;

            for i:=0 to sl.Count-1 do
              begin
                sCmd:=sl.Strings[i];
                if pos('VIEW=',sCmd)>0 then Continue;  // view command is only for viewer
                ReplaceExtCommand(sCmd, aFile, aFile.Path);
                mi:=TMenuItem.Create(miActions);
                mi.Caption:=RemoveQuotation(sCmd);
                mi.Hint:=Copy(sCmd, pos('=',sCmd)+1, length(sCmd));
                // length is bad, but in Copy is corrected
                mi.OnClick:=TContextMenu.ContextMenuSelect; // handler
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
            mi.Caption:='{!VIEWER}' + aFile.Path + aFile.Name;
            mi.Hint:=mi.Caption;
            mi.OnClick:=TContextMenu.ContextMenuSelect; // handler
            miActions.Add(mi);

            // now add EDITconfigure item
            mi:=TMenuItem.Create(miActions);
            mi.Caption:='{!EDITOR}' + aFile.Path + aFile.Name;
            mi.Hint:=mi.Caption;
            mi.OnClick:=TContextMenu.ContextMenuSelect; // handler
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

      //  Open with ...  (for now only for 1 selected file)
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
            mi.OnClick := TContextMenu.OpenWithMenuItemSelect;
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

      // Add separator after actions and openwith menu.
      if AddActionsMenu or AddOpenWithMenu then
      begin
        mi:=TMenuItem.Create(CM);
        mi.Caption:='-';
        CM.Items.Add(mi);
      end;
    end; // if count = 1

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

  Files.Free;
end;
{$ENDIF}

procedure ShowDriveContextMenu(Owner: TWinControl; sPath: String; X, Y : Integer);
{$IFDEF MSWINDOWS}
var
  aFile: TFileSystemFile;
  Files: TFiles;
  OldErrorMode: Word;
begin
  aFile := TFileSystemFile.Create;
  aFile.Name := sPath;
  Files:= TFiles.Create; // free in ShowContextMenu
  Files.Add(aFile);
  OldErrorMode:= SetErrorMode(SEM_NOOPENFILEERRORBOX);
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
      mi.OnClick:= TContextMenu.DriveContextMenuSelect;
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
      mi.OnClick:= TContextMenu.DriveContextMenuSelect;
    end;
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:= rsMnuEject;
  mi.Hint:= '{!EJECT}' + sPath;
  mi.OnClick:= TContextMenu.DriveContextMenuSelect;
  CM.Items.Add(mi);

  // show context menu
  CM.PopUp(X, Y);
end;
{$ENDIF}

(* Show file properties dialog *)
procedure ShowFilePropertiesDialog(const Files: TFiles; const aPath:String);
{$IFDEF UNIX}
begin
  { TODO: Fix Show File Properties under Linux
  ShowFileProperties(FileList, aPath);
  }
end;
{$ELSE}
var
  cmici: TCMINVOKECOMMANDINFO;
  contMenu: IContextMenu;
  fl : TFileList;
begin
  if Files.Count = 0 then Exit;

  contMenu := GetIContextMenu(frmMain.Handle, Files);

  FillChar(cmici, sizeof(cmici), #0);
  with cmici do
    begin
      cbSize := sizeof(cmici);
      hwnd := frmMain.Handle;
      lpVerb := 'properties';
      nShow := SW_SHOWNORMAL;
    end;

  OleCheck(contMenu.InvokeCommand(cmici));
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
  sFilter := GraphicFilter(TGraphic)+'|'+ 'Programs and Libraries(*.exe;*.dll)|*.exe;*.dll'+'|'+
                       Format('All files (%s)',[GetAllFilesMask]);
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


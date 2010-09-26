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
  Graphics, BaseUnix, Unix, fFileProperties;
  {$ELSE}
  FileUtil, Windows, ShlObj, ActiveX, uShlObjAdditional,
  JwaShlGuid, JwaDbt, uMyWindows;
  {$ENDIF}

const
  sCmdVerbOpen = 'open';
  sCmdVerbRename = 'rename';
  sCmdVerbDelete = 'delete';
  sCmdVerbCut = 'cut';
  sCmdVerbCopy = 'copy';
  sCmdVerbPaste = 'paste';
  sCmdVerbLink = 'link';
  sCmdVerbProperties = 'properties';
  sCmdVerbNewFolder = 'NewFolder';

type
  EContextMenuException = class(Exception);

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
procedure ShowContextMenu(Owner: TWinControl; var Files : TFiles; X, Y : Integer; Background: Boolean);
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
  LCLProc, Dialogs, fMain, uOSUtils, uGlobs, uLng, uShellExecute,
  uShellContextMenu, uFileSystemFileSource
  {$IF DEFINED(MSWINDOWS)}
  , Graphics, ComObj, uTotalCommander
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  , uFileSystemWatcher, inotify
  {$ENDIF}
  ;

var
{$IFDEF MSWINDOWS}
  OldWProc: WNDPROC;
  ICM2: IContextMenu2 = nil;
{$ELSE}
  ShellContextMenu : TShellContextMenu = nil;
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
  CreateTotalCommanderWindow(Handle);
end;
{$ELSE}
begin
  if fpGetUID = 0 then // if run under root
    frmMain.Caption:= frmMain.Caption + ' - ROOT PRIVILEGES';
end;
{$ENDIF}

procedure ShowContextMenu(Owner: TWinControl; var Files : TFiles; X, Y : Integer; Background: Boolean);
{$IFDEF MSWINDOWS}
const
  USER_CMD_ID = $1000;
var
  aFile: TFile;
  sl: TStringList = nil;
  i:Integer;
  sAct, sCmd: UTF8String;
  contMenu: IContextMenu;
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

      contMenu := GetShellContextMenu(Owner.Handle, Files, Background);
      if Assigned(contMenu) then
      try
        menu := CreatePopupMenu;
        OleCheckUTF8(contMenu.QueryContextMenu(menu, 0, 1, USER_CMD_ID - 1, CMF_EXPLORE or CMF_CANRENAME));
        contMenu.QueryInterface(IID_IContextMenu2, ICM2); // to handle submenus.
        //------------------------------------------------------------------------------

        aFile := Files[0];
        if Background then // Add "Background" context menu specific items
          begin
            sl:= TStringList.Create;

            // Add commands to root of context menu
            sCmd:= 'cm_Refresh';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(menu, 0, PWideChar(UTF8Decode(sAct)), I, I + USER_CMD_ID, MFT_STRING);
            // Add menu separator
            InsertMenuItemEx(menu, 0, nil, I + 1, 0, MFT_SEPARATOR);

            // Add "Sort by" submenu
            hActionsSubMenu := CreatePopupMenu;
            sCmd:= 'cm_ReverseOrder';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(hActionsSubMenu,0, PWideChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);
            // Add separator
            InsertMenuItemEx(hActionsSubMenu, 0, nil, 0, 0, MFT_SEPARATOR);
            sCmd:= 'cm_SortByAttr';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(hActionsSubMenu,0, PWideChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);
            sCmd:= 'cm_SortByDate';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(hActionsSubMenu,0, PWideChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);
            sCmd:= 'cm_SortBySize';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(hActionsSubMenu,0, PWideChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);
            sCmd:= 'cm_SortByExt';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(hActionsSubMenu,0, PWideChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);
            sCmd:= 'cm_SortByName';
            I:= sl.Add(sCmd);
            sAct:= Actions.GetCommandCaption(sCmd);
            InsertMenuItemEx(hActionsSubMenu,0, PWideChar(UTF8Decode(sAct)), 0, I + USER_CMD_ID, MFT_STRING);

            // Add submenu to context menu
            InsertMenuItemEx(menu, hActionsSubMenu, PWideChar(UTF8Decode(rsMnuSortBy)), 1, 333, MFT_STRING);
          end
        else if (Files.Count = 1) then  // Add "Actions" submenu
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
            begin
              // Insert Actions submenu before first separator
              iCmd:= GetMenuItemCount(menu) - 1;
              for I:= 0 to iCmd do
              begin
                if GetMenuItemType(menu, I, True) = MFT_SEPARATOR then
                  Break;
              end;
              InsertMenuItemEx(menu, hActionsSubMenu, PWideChar(UTF8Decode(rsMnuActions)), I, 333, MFT_STRING);
            end;
          end;
        { /Actions submenu }
        //------------------------------------------------------------------------------
        cmd := UINT(TrackPopupMenu(menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, X, Y, 0, Owner.Handle, nil));
      finally
        if hActionsSubMenu <> 0 then
          DestroyMenu(hActionsSubMenu);
        ICM2 := nil;
      end;

      if (cmd > 0) and (cmd < USER_CMD_ID) then
        begin
          iCmd := LongInt(Cmd) - 1;
          if Succeeded(contMenu.GetCommandString(iCmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb))) then
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
                end
              else if SameText(sVerb, sCmdVerbNewFolder) then
                begin
                  frmMain.actMakeDir.Execute;
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
              OleCheckUTF8(contMenu.InvokeCommand(cmici));

              // Reload after possible changes on the filesystem.
              if SameText(sVerb, sCmdVerbLink) then
                frmMain.ActiveFrame.FileSource.Reload(frmMain.ActiveFrame.CurrentPath);
            end;

        end // if cmd > 0
      else if (cmd >= USER_CMD_ID) then // actions sub menu
        begin
          sCmd:= sl.Strings[cmd - USER_CMD_ID];
          if Background then
            begin
              Actions.Execute(sCmd);
              bHandled:= True;
            end
          else
            begin
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
        end;
    finally
      if menu <> 0 then
        DestroyMenu(menu);
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
begin
  if Files.Count = 0 then
  begin
    FreeAndNil(Files);
    Exit;
  end;

  // Free previous created menu
  FreeThenNil(ShellContextMenu);
  // Create new context menu
  ShellContextMenu:= TShellContextMenu.Create(Owner, Files, Background);
  // Show context menu
  ShellContextMenu.PopUp(X, Y);
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
  ShowContextMenu(Owner, Files, X, Y, False);
  SetErrorMode(OldErrorMode);
end;
{$ELSE}
begin
  // Free previous created menu
  FreeThenNil(ShellContextMenu);
  // Create new context menu
  ShellContextMenu:= TShellContextMenu.Create(Owner, sPath);
  // show context menu
  ShellContextMenu.PopUp(X, Y);
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
  contMenu: IContextMenu;
begin
  if Files.Count = 0 then Exit;

  try
    contMenu := GetShellContextMenu(frmMain.Handle, Files, False);
    if Assigned(contMenu) then
    begin
      FillChar(cmici, sizeof(cmici), #0);
      with cmici do
        begin
          cbSize := sizeof(cmici);
          hwnd := frmMain.Handle;
          lpVerb := sCmdVerbProperties;
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

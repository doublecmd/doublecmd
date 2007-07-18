{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, uTypes, uFileList, Menus,
  {$IFDEF UNIX}
  fFileProperties;
  {$ELSE}
  Windows, Messages, ShellApi, ShlObj, ActiveX, uShlObjAdditional, JwaShlGuid;
  {$ENDIF}
const
  sCmdVerbOpen = 'open';
  sCmdVerbRename = 'rename';
  sCmdVerbDelete = 'delete';
  sCmdVerbPaste = 'paste';

type
  TContextMenu = class(TPopupMenu)
    procedure ContextMenuSelect(Sender:TObject);
  end;

procedure SetMyWndProc(Handle : THandle);
  
procedure ShowFilePropertiesDialog(FileList:TFileList; const aPath:String);
procedure ShowContextMenu(Handle : THandle; pfri : PFileRecItem; X, Y : Integer);

implementation

uses
  fMain, uVFSutil, uOSUtils, uExts, uGlobs;

var
{$IFDEF MSWINDOWS}
  OldWProc: WNDPROC;
  ICM2: IContextMenu2 = nil;
{$ELSE}
  CM : TContextMenu = nil;
{$ENDIF}

{$IFDEF MSWINDOWS}
function MyWndProc(hwnd: HWND; Msg, wParam, lParam: Cardinal): Cardinal; stdcall;
begin
  (* For working wuth submenu of contex menu *)
  if ((Msg = WM_INITMENUPOPUP) or (Msg = WM_DRAWITEM) or (Msg = WM_MENUCHAR)
    or (Msg = WM_MEASUREITEM)) and Assigned(ICM2) then
    begin
      ICM2.HandleMenuMsg(Msg, wParam, lParam);
      Result := 0;
    end
  else
    Result := CallWindowProc(OldWProc, hwnd, Msg, wParam, lParam);
end;
{$ENDIF}

procedure SetMyWndProc(Handle : THandle);
{$IFDEF MSWINDOWS}
begin
  OldWProc := WNDPROC(SetWindowLong(Handle, GWL_WNDPROC, Integer(@MyWndProc)));
end;
{$ELSE}
begin
end;
{$ENDIF}

(* handling user commands from context menu *)
procedure TContextMenu.ContextMenuSelect(Sender:TObject);
var
  sCmd:String;
begin
//  ShowMessage((Sender as TMenuItem).Hint);
  sCmd:=(Sender as TMenuItem).Hint;
  with frmMain.ActiveFrame do
  begin
    if Pos('{!VFS}',sCmd)>0 then
     begin
        pnlFile.LoadPanelVFS(PFileRecItem((Sender as TMenuItem).Tag));
        Exit;
      end;
    if not pnlFile.ProcessExtCommand(sCmd) then
      frmMain.ExecCmd(sCmd);
  end;
end;

procedure ShowContextMenu(Handle : THandle; pfri : PFileRecItem; X, Y : Integer);
var
  fri : TFileRecItem;
{$IFDEF MSWINDOWS}
  desktop: IShellFolder;
  mycomputer: IShellFolder;
  folder: IShellFolder;
  pidl: PItemIDList;
  malloc: IMalloc;
  chEaten: ULONG;
  dwAttributes: ULONG;
  contMenu: IContextMenu;
  menu: HMENU;
  cmd: UINT;
  iCmd: Integer;
  HR: HResult;
  cmici: CMINVOKECOMMANDINFO;
  pwPath,
  pwFileName : PWideChar;
  bHandled : Boolean;
  ZVerb: array[0..255] of char;
  sVerb : String;
{$ELSE}
  mi, miActions : TMenuItem;
  i:Integer;
  sCmd:String;
  sl: TStringList;
{$ENDIF}
begin
  fri := pfri^;
  if fri.sName = '..' then
    begin
      fri.sName := ExtractFileName(ExcludeTrailingPathDelimiter(fri.sPath));
      fri.sPath := LowDirLevel(fri.sPath);
    end;

{$IFDEF MSWINDOWS}
  OleCheck( SHGetMalloc(malloc) );
  OleCheck( SHGetDesktopFolder(desktop) );
  OleCheck( SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, pidl) );
  try
    OleCheck( desktop.BindToObject(pidl, nil, IShellFolder, mycomputer) );
  finally
    malloc.Free(pidl);
  end;
  dwAttributes := 0;
  pwPath := StringToOleStr(fri.sPath);

  OleCheck( mycomputer.ParseDisplayName(Handle, nil, pwPath, chEaten, pidl, dwAttributes) );
  try
    OleCheck( mycomputer.BindToObject(pidl, nil, IShellFolder, folder) );
  finally
    malloc.Free(pidl);
  end;
  dwAttributes := 0;
  pwFileName := StringToOleStr(fri.sName);
  
  OleCheck( folder.ParseDisplayName(Handle, nil, pwFileName, chEaten, pidl, dwAttributes) );
  try
    OleCheck( folder.GetUIObjectOf(Handle, 1, pidl, IID_IContextMenu, nil, contMenu) );
  finally
    malloc.Free(pidl);
  end;
  menu := CreatePopupMenu;
  try
    OleCheck( contMenu.QueryContextMenu(menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME) );
    AppendMenu(menu,0,0,'Test');
    contMenu.QueryInterface(IID_IContextMenu2, ICM2); //To handle submenus.
    cmd := UINT(TrackPopupMenu(menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, X, Y, 0, Handle, nil));
  finally
    DestroyMenu(menu);
    ICM2 := nil;
  end;
  
  if cmd > 0 then
    begin
      iCmd := LongInt(Cmd) - 1;
      HR := contMenu.GetCommandString(iCmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb));
      sVerb := StrPas(ZVerb);
      bHandled := False;

      if SameText(sVerb, sCmdVerbRename) then
        begin
          frmMain.RenameFile('');
          bHandled := True;
        end
      else if SameText(sVerb, sCmdVerbOpen) then
        begin
          if FPS_ISDIR(fri.iMode) or (fri.bLinkIsDir) then
            begin
              if pfri^.sName = '..' then
                frmMain.ActiveFrame.pnlFile.cdUpLevel
              else
                frmMain.ActiveFrame.pnlFile.cdDownLevel(@fri);
              bHandled := True;
            end;
        end;

      if not bHandled then
        begin
          FillChar(cmici, SizeOf(cmici), #0);
          with cmici do
          begin
            cbSize := sizeof(cmici);
            hwnd := Handle;
            lpVerb := PChar(cmd - 1);
            nShow := SW_NORMAL;
          end;
          OleCheck( contMenu.InvokeCommand(cmici) );
        end;
        
      if SameText(sVerb, sCmdVerbDelete) or SameText(sVerb, sCmdVerbPaste) then
        frmMain.ActiveFrame.RefreshPanel;

    end; // if cmd > 0
end;
{$ELSE}
  if not Assigned(CM) then
    CM := TContextMenu.Create(nil)
  else
    CM.Items.Clear;

  mi:=TMenuItem.Create(CM);
  mi.Caption:='Open...';
  mi.Hint := 'open';
  CM.Items.Add(mi);

  mi:=TMenuItem.Create(CM);
  mi.Caption:='-';
  CM.Items.Add(mi);
  
  miActions:=TMenuItem.Create(CM);
  miActions.Caption:='Actions';
  CM.Items.Add(miActions);
  
  { Actions submenu }
  // Read actions from doublecmd.ext
  sl:=TStringList.Create;
  try
    if FPS_ISDIR(fri.iMode) or (fri.bIsLink) then Exit;
    if gExts.GetExtCommands(lowercase(ExtractFileExt(fri.sName)),sl) then
      begin
      //founded any commands
        for i:=0 to sl.Count-1 do
        begin
          sCmd:=sl.Strings[i];
          if pos('VIEW=',sCmd)>0 then Continue;  // view command is only for viewer
          frmMain.ActiveFrame.pnlFile.ReplaceExtCommand(sCmd, @fri);
          mi:=TMenuItem.Create(miActions);
          mi.Caption:=sCmd;
          mi.Hint:=Copy(sCmd, pos('=',sCmd)+1, length(sCmd));
          // length is bad, but in Copy is corrected
          mi.OnClick:=TContextMenu.ContextMenuSelect; // handler
          mi.Tag:=Integer(@fri);
          miActions.Add(mi);
        end;

      end;
      // now add delimiter
    mi:=TMenuItem.Create(miActions);
    mi.Caption:='-';
    miActions.Add(mi);

    // now add VIEW item
    mi:=TMenuItem.Create(miActions);
    mi.Caption:='{!VIEWER}' + fri.sPath + fri.sName;
    mi.Hint:=mi.Caption;
    mi.OnClick:=TContextMenu.ContextMenuSelect; // handler
    miActions.Add(mi);

    // now add EDITconfigure item
    mi:=TMenuItem.Create(miActions);
    mi.Caption:='{!EDITOR}' + fri.sPath + fri.sName;
    mi.Hint:=mi.Caption;
    mi.OnClick:=TContextMenu.ContextMenuSelect; // handler
    miActions.Add(mi);
  finally
    FreeAndNil(sl);
  end;
  { /Actions submenu }

  mi:=TMenuItem.Create(CM);
  mi.Caption:='-';
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Cut';
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Copy';
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Delete';
  mi.Hint := 'actDelete';
  mi.OnClick:=TContextMenu.ContextMenuSelect;
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Rename';
  mi.Hint := 'actShiftF6';
  mi.OnClick:=TContextMenu.ContextMenuSelect;
  CM.Items.Add(mi);

  mi:=TMenuItem.Create(CM);
  mi.Caption:='-';
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Properties';
  mi.Hint := 'actFileProperties';
  mi.OnClick:=TContextMenu.ContextMenuSelect;
  CM.Items.Add(mi);
  
  CM.PopUp(X, Y);
end;
{$ENDIF}

(* Show file properties dialog *)
procedure ShowFilePropertiesDialog(FileList:TFileList; const aPath:String);
{$IFDEF UNIX}
begin
  ShowFileProperties(FileList, aPath);
end;
{$ELSE}
var
  SExInfo: TSHELLEXECUTEINFO;
  Error: LongInt;
  iCurrent : Integer;
  FName : String;

  (* Find first selected file *)
  function FindNextSelected:Boolean;
  var
    i:Integer;
  begin
    for i:=iCurrent to FileList.Count-1 do
    begin
      if FileList.GetItem(i)^.bSelected then
      begin
        iCurrent:=i;
        Result:=True;
        Exit;
      end;
    end;
    Result:=False;
  end;

begin
  iCurrent := 0;
  if FindNextSelected then
    begin
      FName := aPath + FileList.GetItem(iCurrent)^.sName;
      //DebugLN(FName);
      ZeroMemory(Addr(SExInfo),SizeOf(SExInfo));
      SExInfo.cbSize := SizeOf(SExInfo);
      SExInfo.lpFile := PChar(FName);
      SExInfo.lpVerb := 'properties';
      SExInfo.fMask := SEE_MASK_INVOKEIDLIST;
      ShellExecuteExA(Addr(SExInfo));
    end;
end;
{$ENDIF}

end.


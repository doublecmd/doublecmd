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
  SCmdVerbOpen = 'open';
  SCmdVerbRename = 'rename';
  SCmdVerbDelete = 'delete';
  SCmdVerbPaste = 'paste';

type
  TContexMenu = class(TPopupMenu)
    procedure ContexMenuSelect(Sender:TObject);
  end;

procedure SetMyWndProc(Handle : THandle);
  
procedure ShowFilePropertiesDialog(FileList:TFileList; const aPath:String);
procedure ShowContexMenu(Handle : THandle; pfri : PFileRecItem; X, Y : Integer);

implementation

uses
  fMain, uOSUtils, uExts, uGlobs;

var
{$IFDEF MSWINDOWS}
  OldWProc: WNDPROC;
  ICM2: IContextMenu2 = nil;
{$ELSE}
  CM : TContexMenu = nil;
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
procedure TContexMenu.ContexMenuSelect(Sender:TObject);
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

procedure ShowContexMenu(Handle : THandle; pfri : PFileRecItem; X, Y : Integer);
{$IFDEF MSWINDOWS}
var
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
  cmici: CMINVOKECOMMANDINFO;
  pwPath,
  pwFileName : PWideChar;
  bHandled : Boolean;
  sVerb : String;
begin
  OleCheck( SHGetMalloc(malloc) );
  OleCheck( SHGetDesktopFolder(desktop) );
  OleCheck( SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, pidl) );
  try
    OleCheck( desktop.BindToObject(pidl, nil, IShellFolder, mycomputer) );
  finally
    malloc.Free(pidl);
  end;
  dwAttributes := 0;
  pwPath := StringToOleStr(pfri^.sPath);

  OleCheck( mycomputer.ParseDisplayName(Handle, nil, pwPath, chEaten, pidl, dwAttributes) );
  try
    OleCheck( mycomputer.BindToObject(pidl, nil, IShellFolder, folder) );
  finally
    malloc.Free(pidl);
  end;
  dwAttributes := 0;
  pwFileName := StringToOleStr(pfri^.sName);
  
  OleCheck( folder.ParseDisplayName(Handle, nil, pwFileName, chEaten, pidl, dwAttributes) );
  try
    OleCheck( folder.GetUIObjectOf(Handle, 1, pidl, IContextMenu, nil, contMenu) );
  finally
    malloc.Free(pidl);
  end;
  menu := CreatePopupMenu;
  try
    OleCheck( contMenu.QueryContextMenu(menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME) );
    AppendMenu(menu,0,0,'Test');
    contMenu.QueryInterface(IID_IContextMenu2, ICM2); //To handle submenus.
    cmd := UINT(TrackPopupMenu(menu, TPM_RETURNCMD, X, Y, 0, Handle, nil));
  finally
    DestroyMenu(menu);
    ICM2 := nil;
  end;
  
  {bHandled := False;
  sVerb := StrPas(PChar(cmd - 1));

  if SameText(sVerb, SCmdVerbRename) then
  begin
    //EditText;
    bHandled := True;
  end
  else if SameText(sVerb, SCmdVerbOpen) then
  begin
    if FPS_ISDIR(pfri^.iMode) or (pfri^.bLinkIsDir) then
      begin
        frmMain.ActiveFrame.pnlFile.cdDownLevel(pfri);
      end;
  end;    }
  
  if {(not bHandled) and} (cmd > 0) then
    begin
      with cmici do
      begin
        cbSize := sizeof(cmici);
        fMask := 0;
        hwnd := Handle;
        lpVerb := PChar(cmd - 1);
        lpParameters := nil;
        lpDirectory := nil;
        nShow := SW_NORMAL;
      end;
      OleCheck( contMenu.InvokeCommand(cmici) );
    end
end;
{$ELSE}
var
  mi, miActions : TMenuItem;
  i:Integer;
  sCmd:String;
  sl: TStringList;
begin
  if not Assigned(CM) then
    CM := TContexMenu.Create(nil)
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
    if FPS_ISDIR(pfri^.iMode) or (pfri^.bIsLink) then Exit;
    if gExts.GetExtCommands(lowercase(ExtractFileExt(pfri^.sName)),sl) then
      begin
      //founded any commands
        for i:=0 to sl.Count-1 do
        begin
          sCmd:=sl.Strings[i];
          if pos('VIEW=',sCmd)>0 then Continue;  // view command is only for viewer
          frmMain.ActiveFrame.pnlFile.ReplaceExtCommand(sCmd, pfri);
          mi:=TMenuItem.Create(miActions);
          mi.Caption:=sCmd;
          mi.Hint:=Copy(sCmd, pos('=',sCmd)+1, length(sCmd));
          // length is bad, but in Copy is corrected
          mi.OnClick:=TContexMenu.ContexMenuSelect; // handler
          mi.Tag:=Integer(pfri);
          miActions.Add(mi);
        end;

      end;
      // now add delimiter
    mi:=TMenuItem.Create(miActions);
    mi.Caption:='-';
    miActions.Add(mi);

    // now add VIEW item
    mi:=TMenuItem.Create(miActions);
    mi.Caption:='{!VIEWER}' + pfri^.sPath + pfri^.sName;
    mi.Hint:=mi.Caption;
    mi.OnClick:=TContexMenu.ContexMenuSelect; // handler
    miActions.Add(mi);

    // now add EDITconfigure item
    mi:=TMenuItem.Create(miActions);
    mi.Caption:='{!EDITOR}' + pfri^.sPath + pfri^.sName;
    mi.Hint:=mi.Caption;
    mi.OnClick:=TContexMenu.ContexMenuSelect; // handler
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
  mi.OnClick:=TContexMenu.ContexMenuSelect;
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Rename';
  mi.Hint := 'actShiftF6';
  mi.OnClick:=TContexMenu.ContexMenuSelect;
  CM.Items.Add(mi);

  mi:=TMenuItem.Create(CM);
  mi.Caption:='-';
  CM.Items.Add(mi);
  
  mi:=TMenuItem.Create(CM);
  mi.Caption:='Properties';
  mi.Hint := 'actFileProperties';
  mi.OnClick:=TContexMenu.ContexMenuSelect;
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


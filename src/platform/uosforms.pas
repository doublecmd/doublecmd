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
  Classes, SysUtils, Menus, Controls, ExtDlgs, LCLType,
  uFile, uFileSource,
  {$IFDEF UNIX}
  Graphics, BaseUnix, Unix, fFileProperties;
  {$ELSE}
  FileUtil, Windows, ShlObj, uShlObjAdditional, JwaDbt;
  {$ENDIF}

{en
   Replace window procedure
   @param(Handle Window handle)
}
procedure SetMyWndProc(Handle : HWND);
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
{$ENDIF}
  ShellContextMenu : TShellContextMenu = nil;

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
      if Assigned(ShellContextMenu) and Assigned(ShellContextMenu.Menu) then
        begin
          ShellContextMenu.Menu.HandleMenuMsg(uiMsg, wParam, lParam);
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

procedure SetMyWndProc(Handle : HWND);
{$IFDEF MSWINDOWS}
begin
  {$PUSH}{$HINTS OFF}
  OldWProc := WNDPROC(SetWindowLong(Handle, GWL_WNDPROC, LONG_PTR(@MyWndProc)));
  {$POP}
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
begin
  if Files.Count = 0 then
  begin
    FreeAndNil(Files);
    Exit;
  end;

  try
    // Create new context menu
    ShellContextMenu:= TShellContextMenu.Create(Owner, Files, Background);
    // Show context menu
    ShellContextMenu.PopUp(X, Y);
  finally
    // Free created menu
    FreeThenNil(ShellContextMenu);
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


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
  Forms, Classes, SysUtils, Controls,
  uDrive, uFile, uFileSource;

{en
   Must be called on main form create
   @param(MainForm Main form)
}
procedure MainFormCreate(MainForm : TCustomForm);
{en
   Show file/folder properties dialog
   @param(Files List of files to show properties for)
}
procedure ShowFilePropertiesDialog(aFileSource: IFileSource; const Files: TFiles);
{en
   Show file/folder context menu
   @param(Parent Parent window)
   @param(Files List of files to show context menu for. It is freed by this function.)
   @param(X Screen X coordinate)
   @param(Y Screen Y coordinate)
   @param(CloseEvent Method called when popup menu is closed (optional))
}
procedure ShowContextMenu(Parent: TWinControl; var Files : TFiles; X, Y : Integer;
                          Background: Boolean; CloseEvent: TNotifyEvent);
{en
   Show drive context menu
   @param(Parent Parent window)
   @param(sPath Path to drive)
   @param(X Screen X coordinate)
   @param(Y Screen Y coordinate)
   @param(CloseEvent Method called when popup menu is closed (optional))
}
procedure ShowDriveContextMenu(Parent: TWinControl; ADrive: PDrive; X, Y : Integer;
                               CloseEvent: TNotifyEvent);
{en
   Show open icon dialog
   @param(Owner Owner)
   @param(sFileName Icon file name)
   @returns(The function returns @true if successful, @false otherwise)
}
function ShowOpenIconDialog(Owner: TCustomControl; var sFileName : String) : Boolean;

implementation

uses
  ExtDlgs, LCLProc, uShellContextMenu
  {$IF DEFINED(MSWINDOWS)}
  , Graphics, ComObj, fMain, uOSUtils, uFileSystemFileSource, uTotalCommander
  , FileUtil, Windows, ShlObj, uShlObjAdditional
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, fFileProperties
  {$ENDIF};

var
  ShellContextMenu : TShellContextMenu = nil;

procedure MainFormCreate(MainForm : TCustomForm);
{$IFDEF MSWINDOWS}
begin
  CreateTotalCommanderWindow(MainForm.Handle);
end;
{$ELSE}
begin
  if fpGetUID = 0 then // if run under root
    MainForm.Caption:= MainForm.Caption + ' - ROOT PRIVILEGES';
end;
{$ENDIF}

procedure ShowContextMenu(Parent: TWinControl; var Files : TFiles; X, Y : Integer;
                          Background: Boolean; CloseEvent: TNotifyEvent);
{$IFDEF MSWINDOWS}
begin
  if Files.Count = 0 then
  begin
    FreeAndNil(Files);
    Exit;
  end;

  try
    // Create new context menu
    ShellContextMenu:= TShellContextMenu.Create(Parent, Files, Background);
    ShellContextMenu.OnClose := CloseEvent;
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
  ShellContextMenu:= TShellContextMenu.Create(nil, Files, Background);
  ShellContextMenu.OnClose := CloseEvent;
  // Show context menu
  ShellContextMenu.PopUp(X, Y);
end;
{$ENDIF}

procedure ShowDriveContextMenu(Parent: TWinControl; ADrive: PDrive; X, Y : Integer;
                               CloseEvent: TNotifyEvent);
{$IFDEF MSWINDOWS}
var
  aFile: TFile;
  Files: TFiles;
begin
  aFile := TFileSystemFileSource.CreateFile(EmptyStr);
  aFile.FullPath := ADrive^.Path;
  aFile.Attributes := faFolder;
  Files:= TFiles.Create(EmptyStr); // free in ShowContextMenu
  Files.Add(aFile);
  ShowContextMenu(Parent, Files, X, Y, False, CloseEvent);
end;
{$ELSE}
begin
  // Free previous created menu
  FreeThenNil(ShellContextMenu);
  // Create new context menu
  ShellContextMenu:= TShellContextMenu.Create(nil, ADrive);
  ShellContextMenu.OnClose := CloseEvent;
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
      opdDialog.Filter:= sFilter;
      opdDialog.FileName := sFileName;
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
      opdDialog.FileName := sFileName;
      Result:= opdDialog.Execute;
      sFileName := opdDialog.FileName;
{$IFDEF MSWINDOWS}
      bAlreadyOpen := True;
{$ENDIF}
    end;
  if Assigned(opdDialog) then
    FreeAndNil(opdDialog);
end;

finalization
  FreeThenNil(ShellContextMenu);

end.


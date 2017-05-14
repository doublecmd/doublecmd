{
    Double Commander
    -------------------------------------------------------------------------
    Shell context menu implementation.

    Copyright (C) 2006-2015 Alexander Koblov (alexx2000@mail.ru)

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
{$IF (FPC_VERSION > 2) or ((FPC_VERSION = 2) and (FPC_RELEASE >= 5))}
{$POINTERMATH ON}
{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, uFile, Windows, ComObj, ShlObj, ActiveX,
  JwaShlGuid, uGlobs, uShlObjAdditional;

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

  { EContextMenuException }

  EContextMenuException = class(Exception);

  { TShellContextMenu }

  TShellContextMenu = class
  private
    FOnClose: TNotifyEvent;
    FParent: TWinControl;
    FFiles: TFiles;
    FBackground: boolean;
    FShellMenu1: IContextMenu;
    FShellMenu: HMENU;
    FUserWishForContextMenu: TUserWishForContextMenu;
  public
    constructor Create(Parent: TWinControl; var Files: TFiles; Background: boolean; UserWishForContextMenu: TUserWishForContextMenu = uwcmComplete); reintroduce;
    destructor Destroy; override;
    procedure PopUp(X, Y: integer);
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  procedure PasteFromClipboard(Parent: HWND; const Path: String);
  function GetShellContextMenu(Handle: HWND; Files: TFiles; Background: boolean): IContextMenu;

implementation

uses
  graphtype, intfgraphics, Graphics, uPixMapManager, Dialogs, uLng, uMyWindows,
  uShellExecute, fMain, uDCUtils, uFormCommands, DCOSUtils, uOSUtils, uShowMsg,
  uExts, uFileSystemFileSource, DCConvertEncoding, LazUTF8;

const
  USER_CMD_ID = $1000;

var
  OldWProc: WNDPROC = nil;
  ShellMenu2: IContextMenu2 = nil;
  ShellMenu3: IContextMenu3 = nil;
  ContextMenuDCIcon: Graphics.TBitmap = nil;
  ContextMenucm_FileAssoc: Graphics.TBitmap = nil;
  ContextMenucm_RunTerm: Graphics.TBitmap = nil;

function MyWndProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  case uiMsg of
    (* For working with submenu of context menu *)
    WM_INITMENUPOPUP,
    WM_DRAWITEM,
    WM_MENUCHAR,
    WM_MEASUREITEM:
      if Assigned(ShellMenu3) then
        ShellMenu3.HandleMenuMsg2(uiMsg, wParam, lParam, @Result)
      else if Assigned(ShellMenu2) then
      begin
        ShellMenu2.HandleMenuMsg(uiMsg, wParam, lParam);
        Result := 0;
      end
      else
        Result := CallWindowProc(OldWProc, hWnd, uiMsg, wParam, lParam);
    else
      Result := CallWindowProc(OldWProc, hWnd, uiMsg, wParam, lParam);
  end; // case
end;

function GetRecycleBinContextMenu(Handle: HWND): IContextMenu;
var
  PathPIDL: PItemIDList = nil;
  DesktopFolder: IShellFolder;
begin
  OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
  OleCheckUTF8(SHGetSpecialFolderLocation(Handle, CSIDL_BITBUCKET, PathPIDL));
  DesktopFolder.GetUIObjectOf(Handle, 1, PathPIDL, IID_IContextMenu, nil, Result);
end;

function GetForegroundContextMenu(Handle: HWND; Files: TFiles): IContextMenu;
type
  PPIDLArray = ^PItemIDList;

var
  Folder, DesktopFolder: IShellFolder;
  PathPIDL: PItemIDList = nil;
  tmpPIDL: PItemIDList = nil;
  S: WideString;
  List: PPIDLArray = nil;
  I: integer;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
begin
  Result := nil;

  OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
  try
    List := CoTaskMemAlloc(SizeOf(PItemIDList) * Files.Count);
    ZeroMemory(List, SizeOf(PItemIDList) * Files.Count);

    for I := 0 to Files.Count - 1 do
    begin
      if Files[I].Name = EmptyStr then
        S := EmptyStr
      else
        S := UTF8Decode(Files[I].Path);

      OleCheckUTF8(DeskTopFolder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, PathPIDL, dwAttributes));
      try
        OleCheckUTF8(DeskTopFolder.BindToObject(PathPIDL, nil, IID_IShellFolder, Folder));
      finally
        CoTaskMemFree(PathPIDL);
      end;

      if Files[I].Name = EmptyStr then
        S := UTF8Decode(Files[I].Path)
      else
        S := UTF8Decode(Files[I].Name);

      OleCheckUTF8(Folder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, tmpPIDL, dwAttributes));
      (List + i)^ := tmpPIDL;
    end;

    Folder.GetUIObjectOf(Handle, Files.Count, PItemIDList(List^), IID_IContextMenu, nil, Result);

  finally
    if Assigned(List) then
    begin
      for I := 0 to Files.Count - 1 do
        if Assigned((List + i)^) then
          CoTaskMemFree((List + i)^);
      CoTaskMemFree(List);
    end;

    Folder := nil;
    DesktopFolder := nil;
  end;
end;

function GetBackgroundContextMenu(Handle: HWND; Files: TFiles): IContextMenu;
var
  DesktopFolder, Folder: IShellFolder;
  wsFileName: WideString;
  PathPIDL: PItemIDList = nil;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
begin
  Result := nil;

  if Files.Count > 0 then
  begin
    wsFileName := UTF8Decode(Files[0].FullPath);
    OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
    try
      OleCheckUTF8(DesktopFolder.ParseDisplayName(Handle, nil, PWideChar(wsFileName), pchEaten, PathPIDL, dwAttributes));
      try
        OleCheckUTF8(DesktopFolder.BindToObject(PathPIDL, nil, IID_IShellFolder, Folder));
      finally
        CoTaskMemFree(PathPIDL);
      end;
      OleCheckUTF8(Folder.CreateViewObject(Handle, IID_IContextMenu, Result));
    finally
      Folder := nil;
      DesktopFolder := nil;
    end;
  end;
end;

function GetShellContextMenu(Handle: HWND; Files: TFiles; Background: boolean): IContextMenu; inline;
begin
  if Files = nil then
    Result := GetRecycleBinContextMenu(Handle)
  else if Background then
    Result := GetBackgroundContextMenu(Handle, Files)
  else
    Result := GetForegroundContextMenu(Handle, Files);
end;

type

  { TShellThread }

  TShellThread = class(TThread)
  private
    FParent: HWND;
    FVerb: ansistring;
    FShellMenu: IContextMenu;
  protected
    procedure Execute; override;
  public
    constructor Create(Parent: HWND; ShellMenu: IContextMenu; Verb: ansistring); reintroduce;
    destructor Destroy; override;
  end;

{ TShellThread }

procedure TShellThread.Execute;
var
  Result: HRESULT;
  cmici: TCMINVOKECOMMANDINFO;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    FillByte(cmici, SizeOf(cmici), 0);
    with cmici do
    begin
      cbSize := SizeOf(cmici);
      hwnd := FParent;
      lpVerb := PAnsiChar(FVerb);
      nShow := SW_NORMAL;
    end;
    Result := FShellMenu.InvokeCommand(cmici);
    if not (Succeeded(Result) or (Result = COPYENGINE_E_USER_CANCELLED) or (Result = HRESULT_ERROR_CANCELLED)) then
      msgError(Self, mbSysErrorMessage(Result));
  finally
    CoUninitialize;
  end;
end;

constructor TShellThread.Create(Parent: HWND; ShellMenu: IContextMenu; Verb: ansistring);
begin
  inherited Create(True);
  FVerb := Verb;
  FParent := Parent;
  FShellMenu := ShellMenu;
  FreeOnTerminate := True;
end;

destructor TShellThread.Destroy;
begin
  FShellMenu := nil;
  inherited Destroy;
end;

procedure CreateActionSubMenu(MenuWhereToAdd: HMenu; paramExtActionList: TExtActionList; aFile: TFile; bIncludeViewEdit: boolean);
const
  Always_Legacy_Action_Count = 2;
  DCIconRequired = True;
var
  I, iDummy: integer;
  sAct: String;
  iMenuPositionInsertion: integer = 0;
  Always_Expanded_Action_Count: integer = 0;
  liiSource: TLazIntfImage = nil;
  liiDestination: TLazIntfImage = nil;
  ImgFormatDescription: TRawImageDescription;
  bSeparatorAlreadyInserted: boolean;

  function GetMeTheBitmapForThis(ImageRequiredIndex: PtrInt): TBitmap;
  begin
    Result := Graphics.TBitmap.Create;
    Result.SetSize(gIconsSize, gIconsSize);
    Result.Transparent := True;
    Result.Canvas.Brush.Color := clMenu;
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.FillRect(0, 0, gIconsSize, gIconsSize);
    PixMapManager.DrawBitmap(ImageRequiredIndex, Result.Canvas, 0, 0);

    if Result.PixelFormat <> pf32bit then
    begin
      liiSource := Result.CreateIntfImage;
      liiDestination := TLazIntfImage.Create(gIconsSize, gIconsSize);
      try
        ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(gIconsSize, gIconsSize);
        liiDestination.DataDescription := ImgFormatDescription;
        liiDestination.CopyPixels(liiSource);
        Result.FreeImage;
        Result.PixelFormat := pf32bit;
        Result.LoadFromIntfImage(liiDestination);
      finally
        liiDestination.Free;
        liiSource.Free;
      end;
    end;
  end;

  procedure LocalInsertMenuSeparator;
  begin
    InsertMenuItemEx(MenuWhereToAdd, 0, nil, iMenuPositionInsertion, 0, MFT_SEPARATOR);
    Inc(iMenuPositionInsertion);
  end;

  procedure LocalInsertMenuItemExternal(MenuDispatcher: integer; BitmapProvided: TBitmap = nil);
  begin
    if BitmapProvided = nil then
      InsertMenuItemEx(MenuWhereToAdd, 0, PWChar(UTF8Decode(paramExtActionList.ExtActionCommand[MenuDispatcher].ActionName)), iMenuPositionInsertion, MenuDispatcher + USER_CMD_ID, MFT_STRING, paramExtActionList.ExtActionCommand[MenuDispatcher].IconBitmap)
    else
      InsertMenuItemEx(MenuWhereToAdd, 0, PWChar(UTF8Decode(paramExtActionList.ExtActionCommand[MenuDispatcher].ActionName)), iMenuPositionInsertion, MenuDispatcher + USER_CMD_ID, MFT_STRING, BitmapProvided);

    Inc(iMenuPositionInsertion);
  end;

begin
  // Read actions from "extassoc.xml"
  if not gExtendedContextMenu then
    gExts.GetExtActions(aFile, paramExtActionList, @iDummy, False)
  else
    gExts.GetExtActions(aFile, paramExtActionList, @iDummy, True);

  if not gExtendedContextMenu then
  begin
    // In non expanded context menu (legacy), the order of items is:
    // 1o) View (always)
    // 2o) Edit (always)
    // 3o) Custom action different then Open, View or Edit (if any, add also a separator just before)
    I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuView, '{!VIEWER}', QuoteStr(aFile.FullPath), ''));
    LocalInsertMenuItemExternal(I);

    I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuEdit, '{!EDITOR}', QuoteStr(aFile.FullPath), ''));
    LocalInsertMenuItemExternal(I);

    if paramExtActionList.Count > Always_Legacy_Action_Count then
    begin
      bSeparatorAlreadyInserted := false;

      for I := 0 to (pred(paramExtActionList.Count) - Always_Legacy_Action_Count) do
      begin
        sAct := paramExtActionList.ExtActionCommand[I].ActionName;
        if (CompareText('OPEN', sAct) <> 0) and (CompareText('VIEW', sAct) <> 0) and (CompareText('EDIT', sAct) <> 0) then
        begin
          if not bSeparatorAlreadyInserted then
          begin
            LocalInsertMenuSeparator;
            bSeparatorAlreadyInserted := true;
          end;
          LocalInsertMenuItemExternal(I);
        end;
      end;
    end;
  end
  else
  begin
    // In expanded context menu, the order of items is:
    // 1o) View (always, and if "external" is used, shows also the "internal" if user wants it.
    // 2o) Edit (always, and if "external" is used, shows also the "internal" if user wants it.
    // 3o) Custom actions, no matter is open, view or edit (if any, add also a separator just before).
    //     These will be shown in the same order as what they are configured in File Association.
    //     The routine "GetExtActions" has already placed them in the wanted order.
    //     Also, the routine "GetExtActions" has already included the menu separator ('-') between different "TExtAction".
    // 4o) We add the Execute via shell if user requested it.
    // 5o) We add the Execute via terminal if user requested it  (close and then stay open).
    // 6o) Still if user requested it, the shortcut run file association configuration, if user wanted it.
    //     A separator also prior that last action.

    // Let's prepare our icon for extended menu if not already prepaed during the session.
    if ContextMenuDCIcon = nil then
      ContextMenuDCIcon := GetMeTheBitmapForThis(gFiOwnDCIcon);
    if ContextMenucm_FileAssoc = nil then
      ContextMenucm_FileAssoc := GetMeTheBitmapForThis(PixMapManager.GetIconByName('cm_fileassoc'));
    if ContextMenucm_RunTerm = nil then
      ContextMenucm_RunTerm := GetMeTheBitmapForThis(PixMapManager.GetIconByName('cm_runterm'));

    // If the external generic viewer is configured, offer it.
    if gExternalTools[etViewer].Enabled then
    begin
      I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuView + ' (' + rsViewWithExternalViewer + ')', '{!VIEWER}', QuoteStr(aFile.FullPath), ''));
      LocalInsertMenuItemExternal(I);
      Inc(Always_Expanded_Action_Count);
    end;

    // Make sure we always shows our internal viewer
    I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuView + ' (' + rsViewWithInternalViewer + ')', '{!DC-VIEWER}', QuoteStr(aFile.FullPath), ''));
    LocalInsertMenuItemExternal(I, ContextMenuDCIcon);
    Inc(Always_Expanded_Action_Count);

    // If the external generic editor is configured, offer it.
    if gExternalTools[etEditor].Enabled then
    begin
      I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuEdit + ' (' + rsEditWithExternalEditor + ')', '{!EDITOR}', QuoteStr(aFile.FullPath), ''));
      LocalInsertMenuItemExternal(I);
      Inc(Always_Expanded_Action_Count);
    end;

    // Make sure we always shows our internal editor
    I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuEdit + ' (' + rsEditWithInternalEditor + ')', '{!DC-EDITOR}', QuoteStr(aFile.FullPath), ''));
    LocalInsertMenuItemExternal(I, ContextMenuDCIcon);
    Inc(Always_Expanded_Action_Count);

    // Now let's add the action button
    if paramExtActionList.Count > Always_Expanded_Action_Count then
    begin
      LocalInsertMenuSeparator;

      for I := 0 to (pred(paramExtActionList.Count) - Always_Expanded_Action_Count) do
      begin
        if paramExtActionList.ExtActionCommand[I].ActionName <> '-' then
        begin
          sAct := paramExtActionList.ExtActionCommand[I].ActionName;
          if (CompareText('OPEN', sAct) = 0) or (CompareText('VIEW', sAct) = 0) or (CompareText('EDIT', sAct) = 0) then
            sAct := sAct + ' (' + ExtractFilename(paramExtActionList.ExtActionCommand[I].CommandName) + ')';

          if paramExtActionList.ExtActionCommand[I].IconIndex <> -1 then
          begin
            paramExtActionList.ExtActionCommand[I].IconBitmap := Graphics.TBitmap.Create;
            paramExtActionList.ExtActionCommand[I].IconBitmap.SetSize(gIconsSize, gIconsSize);
            paramExtActionList.ExtActionCommand[I].IconBitmap.Transparent := True;
            paramExtActionList.ExtActionCommand[I].IconBitmap.Canvas.Brush.Color := clMenu;
            paramExtActionList.ExtActionCommand[I].IconBitmap.Canvas.Brush.Style := bsSolid;
            paramExtActionList.ExtActionCommand[I].IconBitmap.Canvas.FillRect(0, 0, gIconsSize, gIconsSize);
            PixMapManager.DrawBitmap(paramExtActionList.ExtActionCommand[I].IconIndex, paramExtActionList.ExtActionCommand[I].IconBitmap.Canvas, 0, 0);

            if paramExtActionList.ExtActionCommand[I].IconBitmap.PixelFormat <> pf32bit then
            begin
              liiSource := paramExtActionList.ExtActionCommand[I].IconBitmap.CreateIntfImage;
              liiDestination := TLazIntfImage.Create(gIconsSize, gIconsSize);
              try
                ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(gIconsSize, gIconsSize);
                liiDestination.DataDescription := ImgFormatDescription;
                liiDestination.CopyPixels(liiSource);
                paramExtActionList.ExtActionCommand[I].IconBitmap.FreeImage;
                paramExtActionList.ExtActionCommand[I].IconBitmap.PixelFormat := pf32bit;
                paramExtActionList.ExtActionCommand[I].IconBitmap.LoadFromIntfImage(liiDestination);
              finally
                liiDestination.Free;
                liiSource.Free;
              end;
            end;
          end;

          LocalInsertMenuItemExternal(I);
        end
        else
        begin
          LocalInsertMenuSeparator;
        end;
      end;
    end;

    if gOpenExecuteViaShell or gExecuteViaTerminalClose or gExecuteViaTerminalStayOpen then
      LocalInsertMenuSeparator;

    // now add various SHELL item
    if gOpenExecuteViaShell then
    begin
      I := paramExtActionList.Add(TExtActionCommand.Create(rsMnuOpen + ' (' + rsExecuteViaShell + ')', '{!SHELL}', QuoteStr(aFile.FullPath), ''));
      LocalInsertMenuItemExternal(I);
    end;

    if gExecuteViaTerminalClose then
    begin
      I := paramExtActionList.Add(TExtActionCommand.Create(rsExecuteViaTerminalClose, '{!TERMANDCLOSE}', QuoteStr(aFile.FullPath), ''));
      LocalInsertMenuItemExternal(I, ContextMenucm_RunTerm);
    end;

    if gExecuteViaTerminalStayOpen then
    begin
      I := paramExtActionList.Add(TExtActionCommand.Create(rsExecuteViaTerminalStayOpen, '{!TERMSTAYOPEN}', QuoteStr(aFile.FullPath), ''));
      LocalInsertMenuItemExternal(I, ContextMenucm_RunTerm);
    end;

    // Add shortcut to launch file association configuration screen
    if gIncludeFileAssociation then
    begin
      LocalInsertMenuSeparator;
      I := paramExtActionList.Add(TExtActionCommand.Create(rsConfigurationFileAssociation, 'cm_FileAssoc', '', ''));
      LocalInsertMenuItemExternal(I, ContextMenucm_FileAssoc);
    end;
  end;
end;

{ TShellContextMenu }

{ TShellContextMenu.Create }
constructor TShellContextMenu.Create(Parent: TWinControl; var Files: TFiles; Background: boolean; UserWishForContextMenu: TUserWishForContextMenu);
var
  UFlags: UINT = CMF_EXPLORE;
begin
  // Replace window procedure
  {$PUSH}{$HINTS OFF}
  OldWProc := WNDPROC(SetWindowLongPtr(Parent.Handle, GWL_WNDPROC, LONG_PTR(@MyWndProc)));
  {$POP}
  FParent := Parent;
  FFiles := Files;
  FBackground := Background;
  FShellMenu := 0;
  FUserWishForContextMenu := UserWishForContextMenu;
  if Assigned(Files) then begin
    UFlags := UFlags or CMF_CANRENAME;
  end;
  // Add extended verbs if shift key is down
  if (ssShift in GetKeyShiftState) then begin
    UFlags := UFlags or CMF_EXTENDEDVERBS;
  end;
  try
    try
      FShellMenu1 := GetShellContextMenu(Parent.Handle, Files, Background);
      if Assigned(FShellMenu1) then
      begin
        FShellMenu := CreatePopupMenu;

        if FUserWishForContextMenu = uwcmComplete then
          OleCheckUTF8(FShellMenu1.QueryContextMenu(FShellMenu, 0, 1, USER_CMD_ID - 1, UFlags));

        FShellMenu1.QueryInterface(IID_IContextMenu2, ShellMenu2); // to handle submenus.
        FShellMenu1.QueryInterface(IID_IContextMenu3, ShellMenu3); // to handle submenus.
      end;
    except
      on e: EOleError do
        raise EContextMenuException.Create(e.Message);
    end;
  finally
    Files := nil;
  end;
end;

destructor TShellContextMenu.Destroy;
begin
  // Restore window procedure
  {$PUSH}{$HINTS OFF}
  SetWindowLongPtr(FParent.Handle, GWL_WNDPROC, LONG_PTR(@OldWProc));
  {$POP}
  // Free global variables
  ShellMenu2 := nil;
  ShellMenu3 := nil;
  // Free internal objects
  FShellMenu1 := nil;
  FreeAndNil(FFiles);
  if FShellMenu <> 0 then
    DestroyMenu(FShellMenu);
  inherited Destroy;
end;

procedure TShellContextMenu.PopUp(X, Y: integer);
var
  aFile: TFile = nil;
  i: integer;
  hActionsSubMenu: HMENU = 0;
  cmd: UINT = 0;
  iCmd: integer;
  cmici: TCMInvokeCommandInfoEx;
  lpici: TCMINVOKECOMMANDINFO absolute cmici;
  bHandled: boolean = False;
  ZVerb: array[0..255] of AnsiChar;
  sVerb: string;
  Result: HRESULT;
  FormCommands: IFormCommands;
  InnerExtActionList: TExtActionList = nil;
  UserSelectedCommand: TExtActionCommand = nil;
  sVolumeLabel: string;
begin
  try
    try
      if Assigned(FShellMenu1) then
        try
          FormCommands := frmMain as IFormCommands;

          if Assigned(FFiles) then
          begin
            aFile := FFiles[0];
            if FBackground then // Add "Background" context menu specific items
            begin
              InnerExtActionList := TExtActionList.Create;

              // Add commands to root of context menu
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_Refresh'), 'cm_Refresh', '', ''));
              InsertMenuItemEx(FShellMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);

              // Add "Sort by" submenu
              hActionsSubMenu := CreatePopupMenu;
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_ReverseOrder'), 'cm_ReverseOrder', '', ''));
              InsertMenuItemEx(hActionsSubMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);

              // Add separator
              InsertMenuItemEx(hActionsSubMenu, 0, nil, 0, 0, MFT_SEPARATOR);

              // Add "Sort by" items
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_SortByAttr'), 'cm_SortByAttr', '', ''));
              InsertMenuItemEx(hActionsSubMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_SortByDate'), 'cm_SortByDate', '', ''));
              InsertMenuItemEx(hActionsSubMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_SortBySize'), 'cm_SortBySize', '', ''));
              InsertMenuItemEx(hActionsSubMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_SortByExt'), 'cm_SortByExt', '', ''));
              InsertMenuItemEx(hActionsSubMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_SortByName'), 'cm_SortByName', '', ''));
              InsertMenuItemEx(hActionsSubMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 0, I + USER_CMD_ID, MFT_STRING);

              // Add submenu to context menu
              InsertMenuItemEx(FShellMenu, hActionsSubMenu, PWideChar(UTF8Decode(rsMnuSortBy)), 1, 333, MFT_STRING);

              // Add menu separator
              InsertMenuItemEx(FShellMenu, 0, nil, 2, 0, MFT_SEPARATOR);

              // Add commands to root of context menu
              I := InnerExtActionList.Add(TExtActionCommand.Create(FormCommands.GetCommandCaption('cm_PasteFromClipboard'), 'cm_PasteFromClipboard', '', ''));
              InsertMenuItemEx(FShellMenu, 0, PWideChar(UTF8Decode(InnerExtActionList.ExtActionCommand[I].ActionName)), 3, I + USER_CMD_ID, MFT_STRING);

              // Add menu separator
              InsertMenuItemEx(FShellMenu, 0, nil, 4, 0, MFT_SEPARATOR);
            end
            else  // Add "Actions" submenu
            begin
              InnerExtActionList := TExtActionList.Create;

              if FUserWishForContextMenu = uwcmComplete then
              begin
                hActionsSubMenu := CreatePopupMenu;
                CreateActionSubMenu(hActionsSubMenu, InnerExtActionList, aFile, ((FFiles.Count = 1) and not (aFile.IsDirectory or aFile.IsLinkToDirectory)));
              end
              else
              begin
                CreateActionSubMenu(FShellMenu, InnerExtActionList, aFile, ((FFiles.Count = 1) and not (aFile.IsDirectory or aFile.IsLinkToDirectory)));
              end;

              // Add Actions submenu (Will never be empty, we always have View and Edit...)
              iCmd := GetMenuItemCount(FShellMenu) - 1;
              for I := 0 to iCmd do
              begin
                if GetMenuItemType(FShellMenu, I, True) = MFT_SEPARATOR then
                  Break;
              end;

              if FUserWishForContextMenu = uwcmComplete then
                InsertMenuItemEx(FShellMenu, hActionsSubMenu, PWideChar(UTF8Decode(rsMnuActions)), I, 333, MFT_STRING);
            end;
            { /Actions submenu }
          end;
          //------------------------------------------------------------------------------
          cmd := UINT(TrackPopupMenu(FShellMenu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD, X, Y, 0, FParent.Handle, nil));
        finally
          if hActionsSubMenu <> 0 then
            DestroyMenu(hActionsSubMenu);
        end;

      if (cmd > 0) and (cmd < USER_CMD_ID) then
      begin
        iCmd := longint(Cmd) - 1;
        if Succeeded(FShellMenu1.GetCommandString(iCmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb))) then
        begin
          sVerb := StrPas(ZVerb);

          if SameText(sVerb, sCmdVerbRename) then
          begin
            if FFiles.Count = 1 then
              with FFiles[0] do
              begin
                if not SameText(FullPath, ExtractFileDrive(FullPath) + PathDelim) then
                  frmMain.actRenameOnly.Execute
                else  // change drive label
                begin
                  sVolumeLabel := mbGetVolumeLabel(FullPath, True);
                  if InputQuery(rsMsgSetVolumeLabel, rsMsgVolumeLabel, sVolumeLabel) then
                    mbSetVolumeLabel(FullPath, sVolumeLabel);
                end;
              end
            else
              frmMain.actRename.Execute;
            bHandled := True;
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
          else if SameText(sVerb, sCmdVerbNewFolder) then
          begin
            frmMain.actMakeDir.Execute;
            bHandled := True;
          end
          else if SameText(sVerb, sCmdVerbPaste) or SameText(sVerb, sCmdVerbDelete) then
          begin
            TShellThread.Create(FParent.Handle, FShellMenu1, sVerb).Start;
            bHandled := True;
          end;
        end;

        if not bHandled then
        begin
          if FBackground then
            sVolumeLabel := FFiles[0].FullPath
          else begin
            sVolumeLabel := ExcludeTrailingBackslash(FFiles[0].Path);
          end;
          ZeroMemory(@cmici, SizeOf(cmici));
          with cmici do
          begin
            cbSize := SizeOf(cmici);
            hwnd := FParent.Handle;
            fMask := CMIC_MASK_UNICODE;
            {$PUSH}{$HINTS OFF}
            lpVerb  := PAnsiChar(PtrUInt(cmd - 1));
            {$POP}
            nShow := SW_NORMAL;
            lpDirectory := PAnsiChar(CeUtf8ToSys(sVolumeLabel));
            lpDirectoryW := PWideChar(UTF8ToUTF16(sVolumeLabel));
          end;

          Result := FShellMenu1.InvokeCommand(lpici);
          if not (Succeeded(Result) or (Result = COPYENGINE_E_USER_CANCELLED)) then
            OleErrorUTF8(Result);

          // Reload after possible changes on the filesystem.
          if SameText(sVerb, sCmdVerbLink) or SameText(sVerb, sCmdVerbDelete) then
            frmMain.ActiveFrame.FileSource.Reload(frmMain.ActiveFrame.CurrentPath);
        end;

      end // if cmd > 0
      else if (cmd >= USER_CMD_ID) then // actions sub menu
      begin
        if (cmd - USER_CMD_ID) < InnerExtActionList.Count then
          UserSelectedCommand := InnerExtActionList.ExtActionCommand[cmd - USER_CMD_ID].CloneExtAction;

        if FBackground then
        begin
          if SameText(UserSelectedCommand.CommandName, 'cm_PasteFromClipboard') then
            TShellThread.Create(FParent.Handle, FShellMenu1, sCmdVerbPaste).Start
          else
            FormCommands.ExecuteCommand(UserSelectedCommand.CommandName, []);
          bHandled := True;
        end
        else
        begin
          try
            with frmMain.ActiveFrame do
            begin
              try
                //For the %-Variable replacement that follows it might sounds incorrect to do it with "nil" instead of "aFile",
                //but original code was like that. It is useful, at least, when more than one file is selected so because of that,
                //it's pertinent and should be kept!
                ProcessExtCommandFork(UserSelectedCommand.CommandName, UserSelectedCommand.Params, UserSelectedCommand.StartPath, nil);
              except
                on e: EInvalidCommandLine do
                  MessageDlg(rsMsgErrorInContextMenuCommand, rsMsgInvalidCommandLine + ': ' + e.Message, mtError, [mbOK], 0);
              end;
            end;
          finally
            bHandled := True;
          end;
        end;
      end;
    finally
      if Assigned(InnerExtActionList) then
        FreeAndNil(InnerExtActionList);

      if Assigned(UserSelectedCommand) then
        FreeAndNil(UserSelectedCommand);

      if Assigned(ContextMenuDCIcon) then
        FreeAndNil(ContextMenuDCIcon);
    end;

  except
    on e: EOleError do
      raise EContextMenuException.Create(e.Message);
  end;

  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure PasteFromClipboard(Parent: HWND; const Path: String);
var
  AFile: TFile;
  Files: TFiles;
  ShellMenu: IContextMenu;
begin
  Files:= TFiles.Create(EmptyStr);
  try
    AFile := TFileSystemFileSource.CreateFile(EmptyStr);
    AFile.FullPath := Path;
    AFile.Attributes := faFolder;
    Files.Add(AFile);
    ShellMenu:= GetShellContextMenu(Parent, Files, True);
    if Assigned(ShellMenu) then begin
      TShellThread.Create(Parent, ShellMenu, sCmdVerbPaste).Start;
    end;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  FreeAndNil(Files);
end;

end.


{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit uOSForms;

{$mode delphi}{$H+}

interface

uses
  LCLType, Forms, Classes, SysUtils, Controls,
  uGlobs, uShellContextMenu, uDrive, uFile, uFileSource;

type

  { TAloneForm }

  TAloneForm = class(TForm)
  {$IF DEFINED(DARWIN) AND DEFINED(LCLQT)}
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
  {$ELSEIF DEFINED(LCLWIN32)}
  public
    constructor Create(TheOwner: TComponent); override;
  {$ENDIF}
  end;

  { TModalDialog }

  TModalDialog = class(TAloneForm)
  protected
    FParentWindow: HWND;
    procedure CloseModal;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure ExecuteModal; virtual;
    function ShowModal: Integer; override;
  end;

  { TModalForm }

  {$IF DEFINED(LCLWIN32)}
  TModalForm = class(TModalDialog);
  {$ELSE}
  TModalForm = class(TForm);
  {$ENDIF}

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
                          Background: Boolean; CloseEvent: TNotifyEvent; UserWishForContextMenu:TUserWishForContextMenu = uwcmComplete);
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
   Show trash context menu
   @param(Parent Parent window)
   @param(X Screen X coordinate)
   @param(Y Screen Y coordinate)
   @param(CloseEvent Method called when popup menu is closed (optional))
}
procedure ShowTrashContextMenu(Parent: TWinControl; X, Y : Integer;
                               CloseEvent: TNotifyEvent);
{en
   Show open icon dialog
   @param(Owner Owner)
   @param(sFileName Icon file name)
   @returns(The function returns @true if successful, @false otherwise)
}
function ShowOpenIconDialog(Owner: TCustomControl; var sFileName : String) : Boolean;

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
{en
   Show open with dialog
   @param(FileList List of files to open with)
}
procedure ShowOpenWithDialog(TheOwner: TComponent; const FileList: TStringList);
{$ENDIF}

function GetWindowHandle(AWindow: TWinControl): HWND;

implementation

uses
  ExtDlgs, LCLProc, Menus, Graphics, InterfaceBase, WSForms, LMessages, LCLIntf,
  uConnectionManager
  {$IF DEFINED(MSWINDOWS)}
  , ComObj, fMain, DCOSUtils, uOSUtils, uFileSystemFileSource
  , uTotalCommander, FileUtil, Windows, ShlObj, uShlObjAdditional
  , uWinNetFileSource, uVfsModule, uLng, uMyWindows, DCStrUtils
  , uListGetPreviewBitmap, uThumbnailProvider, uDCReadSVG, uFileSourceUtil
  , Dialogs, Clipbrd, uShowMsg, uDebug, JwaDbt
    {$IFDEF LCLQT5}
    , qt5, qtwidgets, uDarkStyle
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, fFileProperties, uJpegThumb
    {$IF NOT DEFINED(DARWIN)}
    , uDCReadSVG, uMagickWand, uGio, uGioFileSource, uVfsModule, uVideoThumb
    , uDCReadWebP, uFolderThumb
    {$ELSE}
    , MacOSAll, fMain, uQuickLook, uMyDarwin, uShowMsg, uLng
    {$ENDIF}
    {$IF NOT DEFINED(DARWIN)}
    , fOpenWith
    {$ENDIF}
    {$IF DEFINED(LCLQT) and not DEFINED(DARWIN)}
    , qt4, qtwidgets
    {$ENDIF}
    {$IF DEFINED(LCLGTK2)}
    , gtk2
    {$ENDIF}
  {$ENDIF};

{ TAloneForm }

{$IF DEFINED(DARWIN) AND DEFINED(LCLQT)}

var
  FMain, FBefore, FCurrent: TCustomForm;

procedure TAloneForm.DoClose(var CloseAction: TCloseAction);

  procedure TrySetFocus(Form: TCustomForm); inline;
  begin
    if Form.CanFocus then Form.SetFocus;
  end;

var
  psnFront, psnCurrent: ProcessSerialNumber;
begin
  inherited DoClose(CloseAction);
  if (GetCurrentProcess(psnCurrent) = noErr) and (GetFrontProcess(psnFront) = noErr) then
  begin
    // Check that our process is active
    if (psnCurrent.lowLongOfPSN = psnFront.lowLongOfPSN) and
       (psnCurrent.highLongOfPSN = psnFront.highLongOfPSN) then
    begin
      // Restore active form
      if (Screen.CustomFormIndex(FBefore) < 0) then
        TrySetFocus(FMain)
      else if (FBefore <> Self) then
        TrySetFocus(FBefore)
      else
        FBefore:= FMain;
    end;
  end;
end;

procedure ActiveFormChangedHandler(Self, Sender: TObject; Form: TCustomForm);
begin
  if (Form is TAloneForm) or (FMain = Form) then
  begin
    if FCurrent <> Form then
    begin
      FBefore:= FCurrent;
      FCurrent:= Form;
    end;
  end;
end;

{$ELSEIF DEFINED(LCLWIN32)}

constructor TAloneForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if ShowInTaskBar = stAlways then
    // Set window owner to zero, so it will be really standalone window
    SetWindowLong(Handle, GWL_HWNDPARENT, 0);
end;

{$ENDIF}

{ TModalDialog }

procedure TModalDialog.CloseModal;
var
  CloseAction: TCloseAction;
begin
  try
    CloseAction := caNone;
    if CloseQuery then
    begin
      CloseAction := caHide;
      DoClose(CloseAction);
    end;
    case CloseAction of
      caNone: ModalResult := 0;
      caFree: Release;
    end;
    { do not call widgetset CloseModal here, but in ShowModal to
      guarantee execution of it }
  except
    ModalResult := 0;
    Application.HandleException(Self);
  end;
end;

procedure TModalDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FParentWindow <> 0 then
  begin
    Params.Style := Params.Style or WS_POPUP;
    Params.WndParent := FParentWindow;
  end;
end;

procedure TModalDialog.ExecuteModal;
begin
  repeat
    { Delphi calls Application.HandleMessage
      But HandleMessage processes all pending events and then calls idle,
      which will wait for new messages. Under Win32 there is always a next
      message, so it works there. The LCL is OS independent, and so it uses
      a better way: }
    try
      WidgetSet.AppProcessMessages; // process all events
    except
      if Application.CaptureExceptions then
        Application.HandleException(Self)
      else
        raise;
    end;
    if Application.Terminated then
      ModalResult := mrCancel;
    if ModalResult <> 0 then
    begin
      CloseModal;
      if ModalResult <> 0 then Break;
    end;

    Application.Idle(true);
  until False;
end;

function TModalDialog.ShowModal: Integer;

  procedure RaiseShowModalImpossible;
  var
    s: String;
  begin
    DebugLn('TModalForm.ShowModal Visible=',dbgs(Visible),' Enabled=',dbgs(Enabled),
      ' fsModal=',dbgs(fsModal in FFormState),' MDIChild=',dbgs(FormStyle = fsMDIChild));
    s:='TCustomForm.ShowModal for '+DbgSName(Self)+' impossible, because';
    if Visible then
      s:=s+' already visible (hint for designer forms: set Visible property to false)';
    if not Enabled then
      s:=s+' not enabled';
    if fsModal in FFormState then
      s:=s+' already modal';
    if FormStyle = fsMDIChild then
      s:=s+' FormStyle=fsMDIChild';
    raise EInvalidOperation.Create(s);
  end;

var
  SavedFocusState: TFocusState;
  ActiveWindow: HWnd;
begin
  if Self = nil then
    raise EInvalidOperation.Create('TModalForm.ShowModal Self = nil');
  if Application.Terminated then
    ModalResult := 0;
  // Cancel drags
  DragManager.DragStop(false);
  // Close popupmenus
  if ActivePopupMenu <> nil then
    ActivePopupMenu.Close;
  if Visible or (not Enabled) or (FormStyle = fsMDIChild) then
    RaiseShowModalImpossible;
  // Kill capture when opening another dialog
  if GetCapture <> 0 then
    SendMessage(GetCapture, LM_CANCELMODE, 0, 0);
  ReleaseCapture;

  if Owner is TCustomForm then
    ActiveWindow := TCustomForm(Owner).Handle
  else begin
    ActiveWindow := GetActiveWindow;
  end;

  // If parent window is normal window then call inherited method
//  if GetWindowLong(ActiveWindow, GWL_HWNDPARENT) <> 0 then
//    Result:= inherited ShowModal
//  else
    begin
      Include(FFormState, fsModal);
      FParentWindow := ActiveWindow;
      SavedFocusState := SaveFocusState;
      Screen.MoveFormToFocusFront(Self);
      ModalResult := 0;

      try
        EnableWindow(FParentWindow, False);
        // If window already created then recreate it to force
        // call CreateParams with appropriate parent window
        if HandleAllocated then RecreateWnd(Self);
        Show;
        try
          EnableWindow(Handle, True);
          // Activate must happen after show
          Perform(CM_ACTIVATE, 0, 0);
          TWSCustomFormClass(WidgetSetClass).ShowModal(Self);

          ExecuteModal;

          Result := ModalResult;
          if HandleAllocated and (GetActiveWindow <> Handle) then
            ActiveWindow := 0;
        finally
          { Guarantee execution of widgetset CloseModal }
          TWSCustomFormClass(WidgetSetClass).CloseModal(Self);
          // Set our modalresult to mrCancel before hiding.
          if ModalResult = 0 then
            ModalResult := mrCancel;

          EnableWindow(FParentWindow, True);
          // Needs to be called only in ShowModal
          Perform(CM_DEACTIVATE, 0, 0);
          Exclude(FFormState, fsModal);
        end;
      finally
        RestoreFocusState(SavedFocusState);
        if LCLIntf.IsWindow(ActiveWindow) then
         SetActiveWindow(ActiveWindow);
        // Hide window when focus already changed back
        // to parent window to avoid blinking
        LCLIntf.ShowWindow(Handle, SW_HIDE);
        Visible := False;
      end;
    end;
end;

var
  ShellContextMenu : TShellContextMenu = nil;

{$IFDEF MSWINDOWS}

var
  OldWProc: WNDPROC;

function MyWndProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if (uiMsg = WM_SETTINGCHANGE) and (lParam <> 0) and (StrComp('Environment', {%H-}PAnsiChar(lParam)) = 0) then
  begin
    UpdateEnvironment;
    DCDebug('WM_SETTINGCHANGE:Environment');
  end;

  if (uiMsg = WM_DEVICECHANGE) and (wParam = DBT_DEVNODES_CHANGED) and (lParam = 0) then
  begin
    Screen.UpdateMonitors; // Refresh monitor list
    DCDebug('WM_DEVICECHANGE:DBT_DEVNODES_CHANGED');
  end;

  Result := CallWindowProc(OldWProc, hWnd, uiMsg, wParam, lParam);
end;

{$IF DEFINED(LCLWIN32)}
procedure ActivateHandler(Self, Sender: TObject);
var
  I: Integer = 0;
begin
  with Screen do
  begin
    while (I < CustomFormCount) and (((CustomFormsZOrdered[I] is TModalForm) and
          ((CustomFormsZOrdered[I] as TModalForm).FParentWindow <> 0)) or not
          (fsModal in CustomFormsZOrdered[I].FormState)) do
      Inc(I);
    // If modal form exists then activate it
    if (I >= 0) and (I < CustomFormCount) then
      CustomFormsZOrdered[I].BringToFront;
  end;
end;
{$ELSEIF DEFINED(LCLQT5)}
procedure ScreenFormEvent(Self, Sender: TObject; Form: TCustomForm);
var
  Handle: HWND;
begin
  Handle:= GetWindowHandle(Form);
  AllowDarkModeForWindow(Handle, True);
  RefreshTitleBarThemeColor(Handle);
end;
{$ENDIF}

procedure MenuHandler(Self, Sender: TObject);
var
  Ret: DWORD;
  Res: TNetResourceA;
  CDS: TConnectDlgStruct;
begin
  if (Sender as TMenuItem).Tag = 0 then
  begin
    ZeroMemory(@Res, SizeOf(TNetResourceA));
    Res.dwType := RESOURCETYPE_DISK;
    CDS.cbStructure := SizeOf(TConnectDlgStruct);
    CDS.hwndOwner := frmMain.Handle;
    CDS.lpConnRes := @Res;
    CDS.dwFlags := 0;
    Ret:= WNetConnectionDialog1(CDS);
    if Ret = NO_ERROR then
    begin
      SetFileSystemPath(frmMain.ActiveFrame, AnsiChar(Int64(CDS.dwDevNum) + Ord('a') - 1) + ':\');
    end
    else if Ret <> DWORD(-1) then begin
      MessageDlg(mbSysErrorMessage(Ret), mtError, [mbOK], 0);
    end;
  end
  else begin
    Ret:= WNetDisconnectDialog(fmain.frmMain.Handle, RESOURCETYPE_DISK);
    case Ret of
      NO_ERROR, DWORD(-1): ;
      else MessageDlg(mbSysErrorMessage(Ret), mtError, [mbOK], 0);
    end;
  end;
end;

procedure CopyNetNamesToClip(Self, Sender: TObject);
var
  I: Integer;
  sl: TStringList = nil;
  SelectedFiles: TFiles = nil;
begin
  SelectedFiles := frmMain.ActiveFrame.CloneSelectedOrActiveFiles;
  try
    if SelectedFiles.Count > 0 then
    begin
      sl := TStringList.Create;
      for I := 0 to SelectedFiles.Count - 1 do
      begin
        sl.Add(mbGetRemoteFileName(SelectedFiles[I].FullPath));
      end;

      Clipboard.Clear; // Prevent multiple formats in Clipboard (specially synedit)
      Clipboard.AsText := TrimRightLineEnding(sl.Text, sl.TextLineBreakStyle);
    end;

  finally
    FreeAndNil(sl);
    FreeAndNil(SelectedFiles);
  end;
end;

procedure CreateShortcut(Self, Sender: TObject);
var
  ShortcutName: String;
  SelectedFiles: TFiles;
begin
  if (not frmMain.ActiveFrame.FileSource.IsClass(TFileSystemFileSource)) or
     (not frmMain.NotActiveFrame.FileSource.IsClass(TFileSystemFileSource))then
  begin
    msgWarning(rsMsgErrNotSupported);
    Exit;
  end;

  SelectedFiles := frmMain.ActiveFrame.CloneSelectedOrActiveFiles;
  try
    if SelectedFiles.Count > 0 then
    begin
      ShortcutName:= frmMain.NotActiveFrame.CurrentPath + SelectedFiles[0].NameNoExt + '.lnk';
      if ShowInputQuery(rsMnuCreateShortcut, EmptyStr, ShortcutName) then
      begin
        if mbFileExists(ShortcutName) then
        begin
          if not msgYesNo(Format(rsMsgFileExistsRwrt, [WrapTextSimple(ShortcutName, 100)])) then
            Exit;
        end;
        try
          uMyWindows.CreateShortcut(SelectedFiles[0].FullPath, ShortcutName);
        except
          on E: Exception do msgError(E.Message);
        end;
      end;
    end;
  finally
    FreeAndNil(SelectedFiles);
  end;
end;
{$ENDIF}

{$IF DEFINED(LCLGTK2) or (DEFINED(LCLQT) and not DEFINED(DARWIN))}

procedure ScreenFormEvent(Self, Sender: TObject; Form: TCustomForm);
{$IF DEFINED(LCLGTK2)}
var
  ClassName: String;
begin
  ClassName:= Form.ClassName;
  gtk_window_set_role(PGtkWindow(Form.Handle), PAnsiChar(ClassName));
end;
{$ELSEIF DEFINED(LCLQT)}
var
  ClassName: WideString;
begin
  if not (Form is THintWindow) then
  begin
    ClassName:= Form.ClassName;
    QWidget_setWindowRole(QWidget_window(TQtWidget(Form.Handle).GetContainerWidget), @ClassName);
  end;
end;
{$ENDIF}

{$ENDIF}

{$IF DEFINED(DARWIN)}

procedure MenuHandler(Self, Sender: TObject);
var
  Address: String = '';
begin
  if ShowInputQuery(Application.Title, rsMsgURL, False, Address) then
    MountNetworkDrive(Address);
end;

{$ENDIF}

procedure MainFormCreate(MainForm : TCustomForm);
{$IFDEF MSWINDOWS}
var
  Handler: TMethod;
  MenuItem: TMenuItem;
begin
{$IF DEFINED(LCLWIN32)}
  Handler.Code:= @ActivateHandler;
  Handler.Data:= MainForm;
  // Setup application OnActivate handler
  Application.AddOnActivateHandler(TNotifyEvent(Handler), True);
  // Disable application button on taskbar
  with Widgetset do
  SetWindowLong(AppHandle, GWL_EXSTYLE, GetWindowLong(AppHandle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
{$ELSEIF DEFINED(LCLQT5)}
  if g_darkModeEnabled then
  begin
    Handler.Data:= MainForm;
    Handler.Code:= @ScreenFormEvent;
    Screen.AddHandlerFormVisibleChanged(TScreenFormEvent(Handler), True);
  end;
{$ENDIF}
  // Register network file source
  RegisterVirtualFileSource(rsVfsNetwork, TWinNetFileSource);
  if (IsUserAdmin = dupAccept) then // if run under administrator
    MainForm.Caption:= MainForm.Caption + ' - Administrator';

  // Add main window message handler
  {$PUSH}{$HINTS OFF}
  OldWProc := WNDPROC(SetWindowLongPtr(GetWindowHandle(Application.MainForm), GWL_WNDPROC, LONG_PTR(@MyWndProc)));
  {$POP}

  with frmMain do
  begin
    Handler.Code:= @MenuHandler;
    Handler.Data:= MainForm;

    MenuItem:= TMenuItem.Create(mnuMain);
    MenuItem.Caption:= '-';
    mnuNetwork.Add(MenuItem);

    MenuItem:= TMenuItem.Create(mnuMain);
    MenuItem.Caption:= rsMnuMapNetworkDrive;
    MenuItem.Tag:= 0;
    MenuItem.OnClick:= TNotifyEvent(Handler);
    mnuNetwork.Add(MenuItem);

    MenuItem:= TMenuItem.Create(mnuMain);
    MenuItem.Caption:= rsMnuDisconnectNetworkDrive;
    MenuItem.Tag:= 1;
    MenuItem.OnClick:= TNotifyEvent(Handler);
    mnuNetwork.Add(MenuItem);

    MenuItem:= TMenuItem.Create(mnuMain);
    MenuItem.Caption:= '-';
    mnuNetwork.Add(MenuItem);

    MenuItem:= TMenuItem.Create(mnuMain);
    MenuItem.Caption:= rsMnuCopyNetNamesToClip;
    Handler.Code:= @CopyNetNamesToClip;
    MenuItem.OnClick:= TNotifyEvent(Handler);
    mnuNetwork.Add(MenuItem);

    MenuItem:= TMenuItem.Create(mnuMain);
    MenuItem.Caption:= rsMnuCreateShortcut;
    Handler.Code:= @CreateShortcut;
    MenuItem.OnClick:= TNotifyEvent(Handler);
    mnuFiles.Insert(mnuFiles.IndexOf(miMakeDir) + 1, MenuItem);
  end;
end;
{$ELSE}
{$IF DEFINED(LCLQT) or DEFINED(LCLGTK2) or DEFINED(DARWIN)}
var
  Handler: TMethod;
{$ENDIF}
{$IF DEFINED(DARWIN)}
  MenuItem: TMenuItem;
{$ENDIF}
begin
  if fpGetUID = 0 then // if run under root
    MainForm.Caption:= MainForm.Caption + ' - ROOT PRIVILEGES';

  {$IF NOT DEFINED(DARWIN)}
  if HasGio then RegisterVirtualFileSource('GVfs', TGioFileSource, False);
  {$ENDIF}

  {$IF DEFINED(DARWIN) AND DEFINED(LCLQT)}
  FMain:= MainForm;
  Handler.Data:= MainForm;
  Handler.Code:= @ActiveFormChangedHandler;
  Screen.AddHandlerActiveFormChanged(TScreenFormEvent(Handler), True);
  {$ELSEIF DEFINED(LCLGTK2) or DEFINED(LCLQT)}
  Handler.Data:= MainForm;
  Handler.Code:= @ScreenFormEvent;
  ScreenFormEvent(MainForm, MainForm, MainForm);
  Screen.AddHandlerFormAdded(TScreenFormEvent(Handler), True);
  {$ENDIF}

  {$IF DEFINED(DARWIN)}
  if HasMountURL then
  begin
    with frmMain do
    begin
      Handler.Code:= @MenuHandler;
      Handler.Data:= MainForm;

      MenuItem:= TMenuItem.Create(mnuMain);
      MenuItem.Caption:= '-';
      mnuNetwork.Add(MenuItem);

      MenuItem:= TMenuItem.Create(mnuMain);
      MenuItem.Caption:= rsMnuMapNetworkDrive;
      MenuItem.OnClick:= TNotifyEvent(Handler);
      mnuNetwork.Add(MenuItem);
    end;
  end;
  {$ENDIF}
end;
{$ENDIF}

procedure ShowContextMenu(Parent: TWinControl; var Files : TFiles; X, Y : Integer;
                          Background: Boolean; CloseEvent: TNotifyEvent; UserWishForContextMenu:TUserWishForContextMenu = uwcmComplete);
{$IFDEF MSWINDOWS}
begin
  if Assigned(Files) and (Files.Count = 0) then
  begin
    FreeAndNil(Files);
    Exit;
  end;

  try
    // Create new context menu
    ShellContextMenu:= TShellContextMenu.Create(Parent, Files, Background, UserWishForContextMenu);
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
  ShellContextMenu:= TShellContextMenu.Create(nil, Files, Background, UserWishForContextMenu);
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
  if ADrive.DriveType = dtVirtual then
    ShowVirtualDriveMenu(ADrive, X, Y, CloseEvent)
  else begin
    aFile := TFileSystemFileSource.CreateFile(EmptyStr);
    aFile.FullPath := ADrive^.Path;
    aFile.Attributes := faFolder;
    Files:= TFiles.Create(EmptyStr); // free in ShowContextMenu
    Files.Add(aFile);
    ShowContextMenu(Parent, Files, X, Y, False, CloseEvent);
  end;
end;
{$ELSE}
begin
  if ADrive.DriveType = dtVirtual then
    ShowVirtualDriveMenu(ADrive, X, Y, CloseEvent)
  else
  begin
    // Free previous created menu
    FreeThenNil(ShellContextMenu);
    // Create new context menu
    ShellContextMenu:= TShellContextMenu.Create(nil, ADrive);
    ShellContextMenu.OnClose := CloseEvent;
    // show context menu
    ShellContextMenu.PopUp(X, Y);
  end;
end;
{$ENDIF}

procedure ShowTrashContextMenu(Parent: TWinControl; X, Y: Integer;
  CloseEvent: TNotifyEvent);
{$IFDEF MSWINDOWS}
var
  Files: TFiles = nil;
begin
  ShowContextMenu(Parent, Files, X, Y, False, CloseEvent);
end;
{$ELSE}
begin

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
  iPos, iIconIndex: Integer;
  bAlreadyOpen : Boolean;
  bFlagKeepGoing : Boolean = True;
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
      opdDialog.Filter := sFilter;
      opdDialog.InitialDir := ExtractFileDir(sFileName);
      opdDialog.FileName := sFileName;
      Result := opdDialog.Execute;
      if Result then sFileName := opdDialog.FileName else bFlagKeepGoing := False;
      bAlreadyOpen := True;
    end;

  if FileIsExeLib(sFileName) then
    begin
      if bFlagKeepGoing then
      begin
        Result := SHChangeIconDialog(Owner.Handle, sFileName, iIconIndex);
        if Result then sFileName := sFileName + ',' + IntToStr(iIconIndex);
      end;
    end
  else if not bAlreadyOpen then
{$ENDIF}
    begin
      opdDialog := TOpenPictureDialog.Create(Owner);
      opdDialog.InitialDir:=ExtractFileDir(sFileName);
{$IFDEF MSWINDOWS}
      opdDialog.Filter:= sFilter;
{$ENDIF}
      opdDialog.FileName := sFileName;
      Result:= opdDialog.Execute;
      sFileName := opdDialog.FileName;
    end;

  if Assigned(opdDialog) then
    FreeAndNil(opdDialog);
end;

function GetWindowHandle(AWindow: TWinControl): HWND;
{$IF DEFINED(MSWINDOWS) and DEFINED(LCLQT5)}
begin
  Result:= Windows.GetAncestor(HWND(QWidget_winId(TQtWidget(AWindow.Handle).GetContainerWidget)), GA_ROOT);
end;
{$ELSE}
begin
  Result:= AWindow.Handle;
end;
{$ENDIF}

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
procedure ShowOpenWithDialog(TheOwner: TComponent; const FileList: TStringList);
begin
  fOpenWith.ShowOpenWithDlg(TheOwner, FileList);
end;
{$ENDIF}

finalization
  FreeThenNil(ShellContextMenu);

end.


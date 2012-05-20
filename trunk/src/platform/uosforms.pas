{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2012  Koblov Alexander (Alexx2000@mail.ru)

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
  LCLType, Forms, Classes, SysUtils, Controls,
  uDrive, uFile, uFileSource;

type

  { TAloneForm }

  TAloneForm = class(TForm)
  {$IF DEFINED(LCLWIN32)}
  public
    constructor Create(TheOwner: TComponent); override;
  {$ENDIF}
  end;

  { TModalForm }

  TModalForm = class(TForm)
  {$IF DEFINED(LCLWIN32)}
  private
    FParentWindow: HWND;
    procedure CloseModal;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    function ShowModal: Integer; override;
  {$ENDIF}
  end;

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
  , Menus, Graphics, ComObj, fMain, DCOSUtils, uOSUtils, uFileSystemFileSource
  , uTotalCommander, InterfaceBase, FileUtil, Windows, ShlObj, uShlObjAdditional
  , uWinNetFileSource, uVfsModule, uLng, uMyWindows, LMessages, WSForms, LCLIntf
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, fFileProperties
  {$ENDIF};

{$IF DEFINED(LCLWIN32)}

{ TAloneForm }

constructor TAloneForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if ShowInTaskBar = stAlways then
    // Set window owner to zero, so it will be really standalone window
    SetWindowLong(Handle, GWL_HWNDPARENT, 0);
end;

{ TModalForm }

procedure TModalForm.CloseModal;
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

procedure TModalForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FParentWindow <> 0 then
  begin
    Params.Style := Params.Style or WS_POPUP;
    Params.WndParent := FParentWindow;
  end;
end;

function TModalForm.ShowModal: Integer;

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

  ActiveWindow := GetActiveWindow;
  // If parent window is normal window then call inherited method
  if GetWindowLong(ActiveWindow, GWL_HWNDPARENT) <> 0 then
    Result:= inherited ShowModal
  else
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
          // Activate must happen after show
          Perform(CM_ACTIVATE, 0, 0);
          TWSCustomFormClass(WidgetSetClass).ShowModal(Self);
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
        ShowWindow(Handle, SW_HIDE);
        Visible := False;
      end;
    end;
end;

{$ENDIF}

var
  ShellContextMenu : TShellContextMenu = nil;

{$IFDEF MSWINDOWS}
procedure ActivateHandler(Self, Sender: TObject);
var
  I: Integer = 0;
begin
  with Screen do
  begin
    while (I < CustomFormCount) and ((CustomFormsZOrdered[I] is TModalForm) or not
          (fsModal in CustomFormsZOrdered[I].FormState)) do
      Inc(I);
    // If modal form exists then activate it
    if (I >= 0) and (I < CustomFormCount) then
      CustomFormsZOrdered[I].BringToFront;
  end;
end;
{$ENDIF}

procedure MainFormCreate(MainForm : TCustomForm);
{$IFDEF MSWINDOWS}
var
  Handler: TMethod;
begin
  Handler.Code:= @ActivateHandler;
  Handler.Data:= MainForm;
  // Setup application OnActivate handler
  Application.AddOnActivateHandler(TNotifyEvent(Handler), True);
  // Disable application button on taskbar
  with Widgetset do
  SetWindowLong(AppHandle, GWL_EXSTYLE, GetWindowLong(AppHandle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  // Emulate Total Commander window
  CreateTotalCommanderWindow(MainForm.Handle);
  // Register network file source
  RegisterVirtualFileSource(rsVfsNetwork, TWinNetFileSource);
  if (Win32MajorVersion > 5) and IsUserAdmin then // if run under administrator
    MainForm.Caption:= MainForm.Caption + ' - Administrator';
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


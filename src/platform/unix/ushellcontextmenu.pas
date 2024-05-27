{
    Double Commander
    -------------------------------------------------------------------------
    Shell context menu implementation.

    Copyright (C) 2006-2022 Alexander Koblov (alexx2000@mail.ru)

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

unit uShellContextMenu;

{$mode delphi}{$H+}
{$IF DEFINED(DARWIN)}
{$modeswitch objectivec2}
{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, Menus,
  uGlobs, uFile, uDrive;

type

  { EContextMenuException }

  EContextMenuException = class(Exception);

  { TShellContextMenu }

  TShellContextMenu = class(TPopupMenu)
  private
    FFiles: TFiles;
    FDrive: TDrive;
    FUserWishForContextMenu: TUserWishForContextMenu;
    FMenuImageList: TImageList;
    procedure PackHereSelect(Sender: TObject);
    procedure ExtractHereSelect(Sender: TObject);
    procedure ContextMenuSelect(Sender: TObject);
    procedure StandardContextMenuSelect(Sender: TObject);
    procedure TemplateContextMenuSelect(Sender: TObject);
    procedure DriveMountSelect(Sender: TObject);
    procedure DriveUnmountSelect(Sender: TObject);
    procedure DriveEjectSelect(Sender: TObject);
    procedure OpenWithOtherSelect(Sender: TObject);
    procedure OpenWithMenuItemSelect(Sender: TObject);
  private
    procedure LeaveDrive;
    function FillOpenWithSubMenu: Boolean;
    {$IF DEFINED(DARWIN)}
    procedure FillServicesSubMenu;
    procedure SharingMenuItemSelect(Sender: TObject);
    {$ENDIF}
    procedure CreateActionSubMenu(MenuWhereToAdd:TComponent; aFile:TFile; bIncludeViewEdit:boolean);
  public
    constructor Create(Owner: TWinControl; ADrive: PDrive); reintroduce; overload;
    constructor Create(Owner: TWinControl; var Files : TFiles; Background: Boolean; UserWishForContextMenu: TUserWishForContextMenu = uwcmComplete); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses
  LCLProc, Dialogs, Graphics, uFindEx, uDCUtils, uShowMsg, uFileSystemFileSource,
  uOSUtils, uFileProcs, uShellExecute, uLng, uPixMapManager, uMyUnix, uOSForms,
  fMain, fFileProperties, DCOSUtils, DCStrUtils, uExts, uArchiveFileSourceUtil, uSysFolders
  {$IF DEFINED(DARWIN)}
  , LCLStrConsts, MacOSAll, CocoaAll, uMyDarwin
  {$ELSEIF NOT DEFINED(HAIKU)}
  , uKeyFile, uMimeActions
    {$IF DEFINED(LINUX)}
  , uRabbitVCS, uFlatpak
    {$ENDIF}
  {$ENDIF}
  ;

const
  sCmdVerbProperties = 'properties';

var
  // The "ContextMenuActionList" will hold the possible actions to do from the
  // context menu. Each "TMenuItem" associated with these actions will have the
  // the "tag" set to the matching "TExtActionCommand" in this "TextActionList"
  // list.
  ContextMenuActionList: TExtActionList = nil;


procedure addDelimiterMenuItem( menu:TMenuItem ); overload;
var
  item: TMenuItem;
begin
  item:= TMenuItem.Create( menu );
  item.Caption:= '-';
  menu.Add( item );
end;

procedure addDelimiterMenuItem( menu:TMenu ); overload;
var
  item: TMenuItem;
begin
  item:= TMenuItem.Create( menu );
  item.Caption:= '-';
  menu.Items.Add( item );
end;

{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}

function GetGnomeTemplateMenu(out Items: TStringList): Boolean;
var
  searchRec: TSearchRecEx;
  templateDir: String;
  bmpBitmap: TBitmap = nil;
  userDirs: TStringList = nil;
begin
  Result:= False;
  templateDir:= GetHomeDir + '/.config/user-dirs.dirs';
  if not mbFileExists(templateDir) then Exit;
  try
    Items:= nil;
    userDirs:= TStringList.Create;
    try
      userDirs.LoadFromFile(templateDir);
      templateDir:= userDirs.Values['XDG_TEMPLATES_DIR'];
    except
      Exit;
    end;
    if Length(templateDir) = 0 then Exit;
    templateDir:= TrimQuotes(templateDir);
    // Skip misconfigured template path
    if (ExcludeTrailingBackslash(templateDir) = '$HOME') then Exit;
    templateDir:= IncludeTrailingPathDelimiter(mbExpandFileName(templateDir));
    if mbDirectoryExists(templateDir) then
    begin
      if FindFirstEx(templateDir, 0, searchRec) = 0 then
      begin
        Items:= TStringList.Create;
        repeat
          // Skip directories
          if FPS_ISDIR(searchRec.Attr) then Continue;

          bmpBitmap:= PixMapManager.LoadBitmapEnhanced(templateDir + searchRec.Name, 16, True, clMenu);
          Items.AddObject(ExtractOnlyFileName(searchRec.Name) + '=' + templateDir + searchRec.Name, bmpBitmap);
        until FindNextEx(searchRec) <> 0;
        Result:= Items.Count > 0;
      end;
      FindCloseEx(searchRec);
    end;
  finally
    if Assigned(Items) and (Items.Count = 0) then
      FreeAndNil(Items);
    FreeAndNil(userDirs);
  end;
end;

function GetKdeTemplateMenu(out Items: TStringList): Boolean;
var
  I: Integer;
  bmpBitmap: TBitmap = nil;
  desktopFile: TKeyFile = nil;
  templateDir: array [0..1] of String;
  searchRec: TSearchRecEx;
  templateName, templateIcon,
  templatePath: String;
begin
  Result:= False;
  try
    Items:= nil;
    templateDir[0]:= '/usr/share/templates';
    templateDir[1]:= GetHomeDir + '/.kde/share/templates';
    for I:= Low(templateDir) to High(templateDir) do
    if mbDirectoryExists(templateDir[I]) then
    begin
      if FindFirstEx(templateDir[I] + PathDelim + '*.desktop', 0, searchRec) = 0 then
      begin
        if not Assigned(Items) then Items:= TStringList.Create;
        repeat
          // Skip directories
          if FPS_ISDIR(searchRec.Attr) then Continue;

          try
            desktopFile:= TKeyFile.Create(templateDir[I] + PathDelim + searchRec.Name);
            try
              templateName:= desktopFile.ReadLocaleString('Desktop Entry', 'Name', EmptyStr);
              templateIcon:= desktopFile.ReadString('Desktop Entry', 'Icon', EmptyStr);
              templatePath:= desktopFile.ReadString('Desktop Entry', 'URL', EmptyStr);
              templatePath:= GetAbsoluteFileName(templateDir[I] + PathDelim, templatePath);
              if not mbFileExists(templatePath) then Continue; // Skip the non-existent templates
              bmpBitmap:= PixMapManager.LoadBitmapEnhanced(templateIcon, 16, True, clMenu);
              Items.AddObject(templateName + '=' + templatePath, bmpBitmap);
            finally
              FreeAndNil(desktopFile);
            end;
          except
            // Skip
          end;
        until FindNextEx(searchRec) <> 0;
        Result:= Items.Count > 0;
      end;
      FindCloseEx(searchRec);
    end;
  finally
    if Assigned(Items) and (Items.Count = 0) then
      FreeAndNil(Items);
  end;
end;

{$ENDIF}

function GetTemplateMenu(out Items: TStringList): Boolean;
begin
{$IF DEFINED(DARWIN) OR DEFINED(HAIKU)}
  Result:= False;
{$ELSE}
  case GetDesktopEnvironment of
  DE_KDE:
    Result:= GetKdeTemplateMenu(Items);
  else
    Result:= GetGnomeTemplateMenu(Items);
  end;
  if Result then Items.Sort;
{$ENDIF}
end;

procedure TShellContextMenu.LeaveDrive;
begin
  if frmMain.ActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
  begin
    if IsInPath(FDrive.Path, frmMain.ActiveFrame.CurrentPath, True, True) then
    begin
      frmMain.ActiveFrame.CurrentPath:= GetHomeDir;
    end;
  end;
  if frmMain.NotActiveFrame.FileSource.IsClass(TFileSystemFileSource) then
  begin
    if IsInPath(FDrive.Path, frmMain.NotActiveFrame.CurrentPath, True, True) then
    begin
      frmMain.NotActiveFrame.CurrentPath:= GetHomeDir;
    end;
  end
end;

procedure TShellContextMenu.PackHereSelect(Sender: TObject);
begin
  frmMain.Commands.cm_PackFiles(['PackHere']);
end;

procedure TShellContextMenu.ExtractHereSelect(Sender: TObject);
begin
  frmMain.Commands.cm_ExtractFiles(['ExtractHere']);
end;

(* handling user commands from context menu *)
procedure TShellContextMenu.ContextMenuSelect(Sender: TObject);
var
  UserSelectedCommand: TExtActionCommand = nil;
begin
  with Sender as TComponent do
    UserSelectedCommand := ContextMenuActionList.ExtActionCommand[tag].CloneExtAction;

  try

    try
      //For the %-Variable replacement that follows it might sounds incorrect to do it with "nil" instead of "aFile",
      //but original code was like that. It is useful, at least, when more than one file is selected so because of that,
      //it's pertinent and should be kept!
      ProcessExtCommandFork(UserSelectedCommand.CommandName, UserSelectedCommand.Params, UserSelectedCommand.StartPath, nil);
    except
      on e: EInvalidCommandLine do
        MessageDlg(rsMsgErrorInContextMenuCommand, rsMsgInvalidCommandLine + ': ' + e.Message, mtError, [mbOK], 0);
    end;

  finally
    FreeAndNil(UserSelectedCommand);
  end;
end;

procedure TShellContextMenu.StandardContextMenuSelect(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  with frmMain.ActiveFrame do
  begin
    if SameText(MenuItem.Hint, sCmdVerbProperties) then
      ShowFilePropertiesDialog(FileSource, FFiles);
  end;
end;

(* handling user commands from template context menu *)
procedure TShellContextMenu.TemplateContextMenuSelect(Sender: TObject);
var
  FileName: String;
  SelectedItem: TMenuItem;
  AbsoluteTargetFileName: String;
begin
  // ShowMessage((Sender as TMenuItem).Hint);

  SelectedItem:= (Sender as TMenuItem);
  FileName:= SelectedItem.Caption;
  if InputQuery(rsMsgNewFile, rsMsgEnterName, FileName) then
    begin
      FileName:= FileName + ExtractFileExt(SelectedItem.Hint);
      AbsoluteTargetFileName:= frmMain.ActiveFrame.CurrentPath + FileName;
      if (not mbFileExists(AbsoluteTargetFileName)) or
         (msgYesNo(Format(rsMsgFileExistsRwrt, [FileName]))) then
      begin
        if CopyFile(SelectedItem.Hint, AbsoluteTargetFileName) then
        begin
          frmMain.ActiveFrame.Reload;
          frmMain.ActiveFrame.SetActiveFile(FileName);
        end;
      end;
    end;
end;

procedure TShellContextMenu.DriveMountSelect(Sender: TObject);
begin
  MountDrive(@FDrive);
end;

procedure TShellContextMenu.DriveUnmountSelect(Sender: TObject);
begin
  LeaveDrive;
  UnmountDrive(@FDrive);
end;

procedure TShellContextMenu.DriveEjectSelect(Sender: TObject);
begin
  LeaveDrive;
  EjectDrive(@FDrive);
end;

procedure TShellContextMenu.OpenWithOtherSelect(Sender: TObject);
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
var
  I: LongInt;
  FileNames: TStringList;
{$ENDIF}
begin
{$IF DEFINED(LINUX)}
  if DesktopEnv = DE_FLATPAK then
    FlatpakOpen(FFiles[0].FullPath, True)
  else
{$ENDIF}
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  begin
    FileNames := TStringList.Create;
    for I := 0 to FFiles.Count - 1 do
      FileNames.Add(FFiles[I].FullPath);
    ShowOpenWithDialog(frmMain, FileNames);
  end;
{$ENDIF}
end;

{$IF DEFINED(DARWIN)}
function OpenWithComparator(param1:id; param2:id; nouse: pointer): NSInteger; cdecl;
var
  fileManager: NSFileManager;
  string1: NSString;
  string2: NSString;
begin
  fileManager:= NSFileManager.defaultManager;
  string1:= fileManager.displayNameAtPath( param1.path );
  string2:= fileManager.displayNameAtPath( param2.path );
  Result:= string1.localizedStandardCompare( string2 );
end;

function filesToNSUrlArray( const files:TFiles ): NSArray;
var
  theArray: NSMutableArray;
  theFile: TFile;
  path: String;
  url: NSUrl;
begin
  theArray:= NSMutableArray.arrayWithCapacity( files.Count );
  for theFile in files.List do begin
    path:= theFile.FullPath;
    url:= NSUrl.fileURLWithPath( StringToNSString(path) );
    theArray.addObject( url );
  end;
  Result:= theArray;
end;

function getAppArrayFromFiles( const files:TFiles ): NSArray;
const
  ROLE_MASK = kLSRolesViewer or kLSRolesEditor or kLSRolesShell;
var
  theFile: TFile;
  path: String;
  url: NSUrl;

  appSet: NSMutableSet = nil;
  newSet: NSSet;
  appArray: NSMutableArray;
  newArray: NSArray;
  defaultAppUrl: NSUrl = nil;
begin
  Result:= nil;
  try
    for theFile in files.List do begin
      path:= theFile.FullPath;
      url:= NSUrl.fileURLWithPath( StringToNSString(path) );
      newArray:= NSArray( LSCopyApplicationURLsForURL(CFURLRef(url), ROLE_MASK) );
      newSet:= NSSet.setWithArray( newArray );
      if Assigned(appSet) then begin
        appSet.intersectSet( newSet );
      end else begin
        appSet:= NSMutableSet.alloc.initWithSet( newSet );
        if newArray.count > 0 then
          defaultAppUrl:= newArray.objectAtIndex(0);
      end;
      newArray.release;
    end;

    newArray:= NSArray.arrayWithArray( appSet.allObjects );
    newArray:= newArray.sortedArrayUsingFunction_context(
                     @OpenWithComparator, nil );

    appArray:= NSMutableArray.arrayWithArray( newArray );
    if appArray.containsObject(defaultAppUrl) then begin
      appArray.removeObject( defaultAppUrl );
      appArray.insertObject_atIndex( defaultAppUrl, 0 );
    end;
    Result:= appArray;
  finally
    appSet.release;
  end;
end;

// Context Menu / Open with / Other...
function getOtherAppFromDialog(): String;
var
  appDialog: TOpenDialog;
begin
  Result:= '';
  appDialog:= TOpenDialog.Create(nil);
  appDialog.DefaultExt:= 'app';
  appDialog.InitialDir:= '/Applications';
  appDialog.Filter:= rsOpenWithMacOSFilter;
  if appDialog.Execute and (NOT appDialog.FileName.IsEmpty) then begin
    Result:= appDialog.FileName;
  end;
  FreeAndNil( appDialog );
end;

procedure TShellContextMenu.OpenWithMenuItemSelect(Sender: TObject);
var
  appPath: String;
  launchParam: LSLaunchURLSpec;
begin
  appPath := (Sender as TMenuItem).Hint;

  if appPath.IsEmpty then begin
    appPath:= getOtherAppFromDialog;
    if appPath.IsEmpty then
      Exit;
  end;

  launchParam.appURL:= CFURLRef( NSUrl.fileURLWithPath(StringToNSString(appPath)) );
  launchParam.itemURLs:= CFArrayRef( filesToNSUrlArray(FFiles) );
  launchParam.launchFlags:= 0;
  launchParam.asyncRefCon:= nil;
  launchParam.passThruParams:= nil;
  LSOpenFromURLSpec( launchParam, nil );
end;

{$ELSE}

procedure TShellContextMenu.OpenWithMenuItemSelect(Sender: TObject);
var
  ExecCmd: String;
begin
  ExecCmd := (Sender as TMenuItem).Hint;

  if ExecCmd.IsEmpty then
    Exit;

  try
    ExecCmdFork(ExecCmd);
  except
    on e: EInvalidCommandLine do
      MessageDlg(rsMsgErrorInContextMenuCommand, rsMsgInvalidCommandLine + ': ' + e.Message, mtError, [mbOK], 0);
  end;
end;
{$ENDIF}

function TShellContextMenu.FillOpenWithSubMenu: Boolean;
{$IF DEFINED(DARWIN)}
var
  I: Integer;
  ImageIndex: PtrInt;
  bmpTemp: TBitmap = nil;
  mi, miOpenWith: TMenuItem;

  appArray: NSArray;
  appUrl: NSURL;
begin
  Result:= False;
  if FFiles.Count=0 then
    Exit;

  appArray:= getAppArrayFromFiles( FFiles );

  miOpenWith:= TMenuItem.Create(Self);
  miOpenWith.Caption:= rsMnuOpenWith;

  if Assigned(appArray) and (appArray.count>0) then begin
    FMenuImageList := TImageList.Create(nil);
    miOpenWith.SubMenuImages := FMenuImageList;

    for I:= 0 to appArray.count-1 do begin
      appUrl:= NSURL( appArray.objectAtIndex(I) );
      mi:= TMenuItem.Create( miOpenWith );
      mi.Caption:= NSFileManager.defaultManager.displayNameAtPath(appUrl.path).UTF8String;
      mi.Hint := appUrl.path.UTF8String;
      ImageIndex:= PixMapManager.GetApplicationBundleIcon(appUrl.path.UTF8String, -1);
      if ImageIndex >= 0 then begin
        bmpTemp:= PixMapManager.GetBitmap(ImageIndex);
        if Assigned(bmpTemp) then begin
          mi.ImageIndex:=FMenuImageList.Count;
          FMenuImageList.Add( bmpTemp , nil );
          FreeAndNil(bmpTemp);
        end;
      end;
      mi.OnClick := Self.OpenWithMenuItemSelect;
      miOpenWith.Add(mi);
      if (i=0) and (appArray.count>=2) then
        addDelimiterMenuItem( miOpenWith );
    end;
  end;

  // Other...
  addDelimiterMenuItem( miOpenWith );

  mi:= TMenuItem.Create(miOpenWith);
  mi.Caption:= rsMnuOpenWithOther;
  mi.OnClick := Self.OpenWithMenuItemSelect;
  miOpenWith.Add(mi);

  Self.Items.Add(miOpenWith);
  Result:= True;
end;
{$ELSEIF DEFINED(HAIKU)}
begin
  Result:= False;
end;
{$ELSE}
var
  I: LongInt;
  bmpTemp: TBitmap;
  FileNames: TStringList;
  Entry: PDesktopFileEntry;
  mi, miOpenWith: TMenuItem;
  DesktopEntries: TList = nil;
begin
  Result := True;
  FileNames := TStringList.Create;
  try
    miOpenWith := TMenuItem.Create(Self);
    miOpenWith.Caption := rsMnuOpenWith;
    Self.Items.Add(miOpenWith);

    for I := 0 to FFiles.Count - 1 do
      FileNames.Add(FFiles[I].FullPath);

    DesktopEntries := GetDesktopEntries(FileNames);

    if Assigned(DesktopEntries) and (DesktopEntries.Count > 0) then
    begin
      for I := 0 to DesktopEntries.Count - 1 do
      begin
        Entry := PDesktopFileEntry(DesktopEntries[I]);
        mi := TMenuItem.Create(miOpenWith);
        mi.Caption := Entry^.DisplayName;
        mi.Hint := Entry^.Exec;
        bmpTemp:= PixMapManager.LoadBitmapEnhanced(Entry^.IconName, 16, True, clMenu);
        if Assigned(bmpTemp) then
        begin
          mi.Bitmap.Assign(bmpTemp);
          FreeAndNil(bmpTemp);
        end;
        mi.OnClick := Self.OpenWithMenuItemSelect;
        miOpenWith.Add(mi);
      end;
      miOpenWith.AddSeparator;
    end;

    mi := TMenuItem.Create(miOpenWith);
    mi.Caption := rsMnuOpenWithOther;
    mi.OnClick := Self.OpenWithOtherSelect;
    miOpenWith.Add(mi);

{$IF DEFINED(LINUX)}
    FillRabbitMenu(Self, FileNames);
{$ENDIF}

  finally
    FreeAndNil(FileNames);
    if Assigned(DesktopEntries) then
    begin
      for I := 0 to DesktopEntries.Count - 1 do
        Dispose(PDesktopFileEntry(DesktopEntries[I]));
      FreeAndNil(DesktopEntries);
    end;
  end;
end;
{$ENDIF}


{$IF DEFINED(DARWIN)}
procedure TShellContextMenu.FillServicesSubMenu;
var
  mi: TMenuItem;
begin
  addDelimiterMenuItem( self );

  // attach Services Menu in TMacosServiceMenuHelper
  mi:=TMenuItem.Create(Self);
  mi.Caption:=LCLStrConsts.rsMacOSMenuServices;
  Self.Items.Add(mi);

  addDelimiterMenuItem( self );

  // add Sharing Menu
  // similar to MacOS 13, the Share MenuItem does not expand the submenu,
  // and the SharingServicePicker pops up after clicking Share MenuItem.
  mi:=TMenuItem.Create(Self);
  mi.Caption:= uLng.rsMenuMacOsShare;
  mi.OnClick:= self.SharingMenuItemSelect;
  Self.Items.Add(mi);
end;

procedure TShellContextMenu.SharingMenuItemSelect(Sender: TObject);
begin
  showMacOSSharingServiceMenu;
end;
{$ENDIF}


constructor TShellContextMenu.Create(Owner: TWinControl; ADrive: PDrive);
var
  mi: TMenuItem;
begin
  inherited Create(Owner);
  FDrive := ADrive^;

  mi := TMenuItem.Create(Self);
  if not ADrive^.IsMounted then
    begin
      if ADrive^.IsMediaAvailable then
        begin
          mi.Caption := rsMnuMount;
          mi.OnClick := Self.DriveMountSelect;
        end
      else
        begin
          mi.Caption := rsMnuNoMedia;
          mi.Enabled := False;
        end;
    end
  else
    begin
      {$IF not DEFINED(DARWIN)}
      mi.Caption := rsMnuUmount;
      mi.OnClick := Self.DriveUnmountSelect;
      {$ELSE}
      mi.Caption := rsMnuUmount + ' / ' + rsMnuEject;
      mi.OnClick := Self.DriveEjectSelect;
      {$ENDIF}
    end;
  Self.Items.Add(mi);

  {$IF not DEFINED(DARWIN)}
  if ADrive^.IsMediaEjectable then
    begin
      mi :=TMenuItem.Create(Self);
      mi.Caption := rsMnuEject;
      mi.OnClick := Self.DriveEjectSelect;
      Self.Items.Add(mi);
    end;
  {$ENDIF}
end;

{ TShellContextMenu.CreateActionSubMenu }
// Create the "Actions" menu/submenu.
procedure TShellContextMenu.CreateActionSubMenu(MenuWhereToAdd:TComponent; aFile:TFile; bIncludeViewEdit:boolean);
var
  mi: TMenuItem;
  I, iDummy:integer;
  sAct: String;
  iMenuPositionInsertion: integer =0;

  procedure AddMenuItemRightPlace;
  begin
    if MenuWhereToAdd is TMenuItem then
      TMenuItem(MenuWhereToAdd).Add(mi)
    else
      Self.Items.Add(mi);
    inc(iMenuPositionInsertion);
  end;

  procedure LocalInsertMenuSeparator;
  begin
    mi:=TMenuItem.Create(MenuWhereToAdd);
    mi.Caption:='-';
    AddMenuItemRightPlace;
  end;

  procedure LocalInsertMenuItem(CaptionMenu:string; MenuDispatcher:integer);
  begin
    mi := TMenuItem.Create(MenuWhereToAdd);
    mi.Caption := CaptionMenu;
    mi.Tag := MenuDispatcher;
    mi.OnClick:= Self.ContextMenuSelect;
    AddMenuItemRightPlace;
  end;

begin
  // Read actions from "extassoc.xml"
  if not gExtendedContextMenu then
    gExts.GetExtActions(aFile, ContextMenuActionList, @iDummy, False)
  else
    gExts.GetExtActions(aFile, ContextMenuActionList, @iDummy, True);

  if not gExtendedContextMenu then
  begin
    // In non expanded context menu (legacy), the order of items is:
    // 1o) Custom action different then Open, View or Edit
    // 2o) Add a separator in any action added above
    // 3o) View (always)
    // 4o) Edit (always)
    // note: In Windows flavor, this is not the same order but to respect initial DC legacy order, that was it.

    if ContextMenuActionList.Count > 0 then
    begin
      for I := 0 to pred(ContextMenuActionList.Count) do
      begin
        sAct := ContextMenuActionList.ExtActionCommand[I].ActionName;
        if (SysUtils.CompareText('OPEN', sAct) <> 0) and (SysUtils.CompareText('VIEW', sAct) <> 0) and (SysUtils.CompareText('EDIT', sAct) <> 0) then
          LocalInsertMenuItem(sAct,  I);
      end;
    end;

    if iMenuPositionInsertion>0 then //It cannot be just (ContextMenuActionList.Count>0) 'case if the list has just OPEN, VIEW or READ, we will have nothing and we don't want the separator.
      LocalInsertMenuSeparator;

    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsMnuView, '{!VIEWER}', QuoteStr(aFile.FullPath), ''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName, I);

    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsMnuEdit, '{!EDITOR}', QuoteStr(aFile.FullPath), ''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName, I);
  end
  else
  begin
    // In expanded context menu (legacy), the order of items is the following.
    // 1o) Custom actions, no matter is open, view or edit (if any, add also a separator just before).
    //     These will be shown in the same order as what they are configured in File Association.
    //     The routine "GetExtActions" has already placed them in the wanted order.
    //     Also, the routine "GetExtActions" has already included the menu separator ('-') between different "TExtAction".
    // 2o) Add a separator in any action added above
    // 3o) View (always, and if "external" is used, shows also the "internal" if user wants it.
    // 4o) Edit (always, and if "external" is used, shows also the "internal" if user wants it.
    // 5o) We add the Execute via shell if user requested it.
    // 6o) We add the Execute via terminal if user requested it  (close and then stay open).
    // 7o) Still if user requested it, the shortcut run file association configuration, if user wanted it.
    //     A separator also prior that last action.
    // note: In Windows flavor, this is not the same order but to respect initial DC legacy order, that was it.

    for I:= 0 to pred(ContextMenuActionList.Count) do
      begin
        if ContextMenuActionList.ExtActionCommand[I].ActionName<>'-' then
        begin
          sAct:= ContextMenuActionList.ExtActionCommand[I].ActionName;
          if (SysUtils.CompareText('OPEN', sAct) = 0) or (SysUtils.CompareText('VIEW', sAct) = 0) or (SysUtils.CompareText('EDIT', sAct) = 0) then
            sAct:=sAct+' ('+ExtractFilename(ContextMenuActionList.ExtActionCommand[I].CommandName)+')';
          LocalInsertMenuItem(sAct,I);
        end
        else
        begin
          LocalInsertMenuSeparator;
        end;
      end;

  // If the default context actions not hidden
  if gDefaultContextActions then
  begin
    if ContextMenuActionList.Count>0 then
       LocalInsertMenuSeparator;

    // If the external generic viewer is configured, offer it.
    if gExternalTools[etViewer].Enabled then
    begin
      I := ContextMenuActionList.Add(TExtActionCommand.Create(rsMnuView+' ('+rsViewWithExternalViewer+')','{!VIEWER}',QuoteStr(aFile.FullPath),''));
      LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
    end;

    // Make sure we always shows our internal viewer
    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsMnuView+' ('+rsViewWithInternalViewer+')','{!DC-VIEWER}',QuoteStr(aFile.FullPath),''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);

    // If the external generic editor is configured, offer it.
    if gExternalTools[etEditor].Enabled then
    begin
      I := ContextMenuActionList.Add(TExtActionCommand.Create(rsMnuEdit+' ('+rsEditWithExternalEditor+')','{!EDITOR}',QuoteStr(aFile.FullPath),''));
      LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
    end;

    // Make sure we always shows our internal editor
    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsMnuEdit+' ('+rsEditWithInternalEditor+')','{!DC-EDITOR}',QuoteStr(aFile.FullPath),''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
  end;

  if (gOpenExecuteViaShell or gExecuteViaTerminalClose or gExecuteViaTerminalStayOpen) and (ContextMenuActionList.Count>0) then
    LocalInsertMenuSeparator;

  // Execute via shell
  if gOpenExecuteViaShell then
  begin
    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsExecuteViaShell,'{!SHELL}',QuoteStr(aFile.FullPath),''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
  end;

  // Execute via terminal and close
  if gExecuteViaTerminalClose then
  begin
    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsExecuteViaTerminalClose,'{!TERMANDCLOSE}',QuoteStr(aFile.FullPath),''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
  end;

  // Execute via terminal and stay open
  if gExecuteViaTerminalStayOpen then
  begin
    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsExecuteViaTerminalStayOpen,'{!TERMSTAYOPEN}',QuoteStr(aFile.FullPath),''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
  end;

  // Add shortcut to launch file association cnfiguration screen
  if gIncludeFileAssociation then
  begin
    if ContextMenuActionList.Count>0 then
       LocalInsertMenuSeparator;

    I := ContextMenuActionList.Add(TExtActionCommand.Create(rsConfigurationFileAssociation,'cm_FileAssoc','',''));
    LocalInsertMenuItem(ContextMenuActionList.ExtActionCommand[I].ActionName,I);
  end;
  end;
end;


constructor TShellContextMenu.Create(Owner: TWinControl; var Files: TFiles; Background: Boolean; UserWishForContextMenu: TUserWishForContextMenu);
var
  I: Integer;
  aFile: TFile = nil;
  sl: TStringList = nil;
  mi, miActions, miSortBy: TMenuItem;
  AddActionsMenu: Boolean = False;
  AddOpenWithMenu: Boolean = False;
begin
  inherited Create(Owner);

  FFiles:= Files;
  FUserWishForContextMenu:= UserWishForContextMenu;

  try
    if ContextMenuActionList=nil then
      ContextMenuActionList:=TExtActionList.Create;
    ContextMenuActionList.Clear;

    if not Background then
    begin
      aFile := Files[0];

      // Add the "Open"
      if FUserWishForContextMenu = uwcmComplete then
      begin
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actShellExecute;
        Self.Items.Add(mi);
      end;

      // Add the "Actions" menu
      if FUserWishForContextMenu = uwcmComplete then
      begin
        miActions:=TMenuItem.Create(Self);
        miActions.Caption:= rsMnuActions;
        CreateActionSubMenu(miActions, aFile, ((FFiles.Count = 1) and not (aFile.IsDirectory or aFile.IsLinkToDirectory)));
        if miActions.Count>0 then
          Self.Items.Add(miActions)
        else
          miActions.Free;
      end
      else
      begin
        CreateActionSubMenu(Self, aFile, ((FFiles.Count = 1) and not (aFile.IsDirectory or aFile.IsLinkToDirectory)))
      end;

      if FUserWishForContextMenu = uwcmComplete then
      begin
        addDelimiterMenuItem( self );

        // Add "Open with" submenu if needed
        AddOpenWithMenu := FillOpenWithSubMenu;

        // Add "Services" menu if MacOS
        {$IF DEFINED(DARWIN)}
        FillServicesSubMenu;
        {$ENDIF}

        // Add delimiter menu
        addDelimiterMenuItem( self );

        // Add "Pack here..."
        mi:=TMenuItem.Create(Self);
        mi.Caption:= rsMnuPackHere;
        mi.OnClick:= Self.PackHereSelect;
        Self.Items.Add(mi);

        // Add "Extract here..."
        if FileIsArchive(aFile.FullPath) then
        begin
          mi:=TMenuItem.Create(Self);
          mi.Caption:= rsMnuExtractHere;
          mi.OnClick:= Self.ExtractHereSelect;
          Self.Items.Add(mi);
        end;

        // Add delimiter menu
        addDelimiterMenuItem( self );

        // Add "Move"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actRename;
        Self.Items.Add(mi);

        // Add "Copy"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actCopy;
        Self.Items.Add(mi);

        // Add "Delete"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actDelete;
        Self.Items.Add(mi);

        // Add "Rename"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actRenameOnly;
        Self.Items.Add(mi);

        addDelimiterMenuItem( self );

        // Add "Cut"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actCutToClipboard;
        Self.Items.Add(mi);

        // Add "Copy"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actCopyToClipboard;
        Self.Items.Add(mi);

        // Add "PAste"
        mi:=TMenuItem.Create(Self);
        mi.Action := frmMain.actPasteFromClipboard;
        Self.Items.Add(mi);

        addDelimiterMenuItem( self );

        // Add "Show file properties"
        mi:= TMenuItem.Create(Self);
        mi.Hint:= sCmdVerbProperties;
        mi.Caption:= frmMain.actFileProperties.Caption;
        mi.ShortCut:= frmMain.actFileProperties.ShortCut;
        mi.OnClick:= Self.StandardContextMenuSelect;
        Self.Items.Add(mi);
      end;
    end
    else
    begin
      mi:=TMenuItem.Create(Self);
      mi.Action := frmMain.actRefresh;
      Self.Items.Add(mi);

      // Add "Sort by" submenu
      miSortBy := TMenuItem.Create(Self);
      miSortBy.Caption := rsMnuSortBy;
      Self.Items.Add(miSortBy);

      mi:=TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actSortByName;
      miSortBy.Add(mi);

      mi:=TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actSortByExt;
      miSortBy.Add(mi);

      mi:=TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actSortBySize;
      miSortBy.Add(mi);

      mi:=TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actSortByDate;
      miSortBy.Add(mi);

      mi:=TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actSortByAttr;
      miSortBy.Add(mi);

      addDelimiterMenuItem( miSortBy );

      mi:=TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actReverseOrder;
      miSortBy.Add(mi);

      addDelimiterMenuItem( self );

      mi:=TMenuItem.Create(Self);
      mi.Action := frmMain.actPasteFromClipboard;
      Self.Items.Add(mi);

      addDelimiterMenuItem( self );

      // Add "New" submenu
      miSortBy := TMenuItem.Create(Self);
      miSortBy.Caption := rsMnuNew;
      Self.Items.Add(miSortBy);

      // Add "Create directory"
      mi:= TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actMakeDir;
      mi.Caption:= rsPropsFolder;
      miSortBy.Add(mi);

      // Add "Create file"
      mi:= TMenuItem.Create(miSortBy);
      mi.Action := frmMain.actEditNew;
      mi.Caption:= rsPropsFile;
      miSortBy.Add(mi);

      if GetTemplateMenu(sl) then
      begin
        addDelimiterMenuItem( miSortBy );

        for I:= 0 to sl.Count - 1 do
        begin
          mi:=TMenuItem.Create(miSortBy);
          mi.Caption:= sl.Names[I];
          mi.Hint:= sl.ValueFromIndex[I];
          mi.OnClick:= Self.TemplateContextMenuSelect;
          if Assigned(sl.Objects[I]) then
          begin
            mi.Bitmap.Assign(TBitmap(sl.Objects[I]));
            sl.Objects[I].Free;
            sl.Objects[I]:= nil;
          end;
          miSortBy.Add(mi);
        end;
        FreeAndNil(sl);
      end;

      addDelimiterMenuItem( self );

      mi:= TMenuItem.Create(Self);
      mi.Hint:= sCmdVerbProperties;
      mi.Caption:= frmMain.actFileProperties.Caption;
      mi.ShortCut:= frmMain.actFileProperties.ShortCut;
      mi.OnClick:= Self.StandardContextMenuSelect;
      Self.Items.Add(mi);
    end;
  finally
    Files:= nil;
  end;
end;

destructor TShellContextMenu.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FMenuImageList);
  inherited Destroy;
end;

end.


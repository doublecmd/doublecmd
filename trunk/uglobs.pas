{
   Double Commander
   ------------------------------------------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   Globals variables and some consts

   contributors:

   Copyright (C) 2006-2009 Alexander Koblov (Alexx2000@mail.ru)
   
   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   
   Copyright (C) 2008  Vitaly Zotov (vitalyzotov@mail.ru)
   
}

unit uGlobs;

interface
uses
  Classes, Controls, uExts, uColorExt, Graphics, uClassesEx, uWDXModule,
  uColumns,uhotkeymanger,uActs, uWFXModule, uWCXModule;

type
  TControlPosition = object
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    
    procedure Save(Control: TControl);
    procedure Restore(Control: TControl);
  end;

type
  { Log options }
  TLogOptions = set of (log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                        log_vfs_op, log_success, log_errors, log_info);
                        
var
  { For localization }
  gPOFileName,
  gHelpLang: String;
  
  { WDX plugins }
  gWdxPlugins:TWDXModuleList;
  { WCX plugins }
  gWCXPlugins: TWCXModuleList;
  { WFX plugins }
  gWFXPlugins: TWFXModuleList;
  
  { Columns Set }
  ColSet:TPanelColumnsList;
  
  { Layout page }
  gButtonBar,
  gToolBarFlat,
  gDriveBar1,
  gDriveBar2,
  gDriveBarFlat,
  gDriveMenuButton,
  gDirectoryTabs,
  gCurDir,
  gTabHeader,
  gStatusBar,
  gCmdLine,
  gLogWindow,
  gTermWindow,
  gKeyButtons,
  gInterfaceFlat : Boolean;
  
  gToolBarIconSize : Integer;
  
  gDirSortFirst:Boolean=True; // want to show dir first in panels
  gDirHistoryCount:Integer=30; // how many history we remember
  gShowSystemFiles:Boolean=True;
  gRunInTerm: String;
  gRunTerm: String;
  gCaseSensitiveSort:Boolean=True;
  gLynxLike:Boolean=True;
  gDirSelect:Boolean=True;

  gMouseSelectionEnabled: Boolean = True;
  gMouseSelectionButton: Integer = 0;
  
  glsHotDir:TStringListEx;
  glsDirHistory:TStringListEx;
  glsMaskHistory : TStringListEx;
  gCutTextToColWidth : Boolean;
  gSpaceMovesDown: Boolean;
  gScrollMode: Integer;
  gTrayIcon: Boolean;
  gShortFileSizeFormat:Boolean=True;

  gDateTimeFormat : String;
  
  gDriveBlackList: String;
  
  { Tools page }

  gUseExtEdit:Boolean=False;
  gUseExtView:Boolean=False;
  gUseExtDiff:Boolean=False;

  gExtEdit:String='emacs';
  gExtView:String='emacs';
  gExtDiff:String='gtk-diff';
  gLuaLib:String='/usr/lib/liblua5.1.so';
  gExts:TExts;
  gColorExt:TColorExt;

  gFontName: String;
  gFontSize: Integer;
  gFontStyle: TFontStyles;
  gEditorFontName: String;
  gEditorFontSize: Integer;
  gEditorFontStyle: TFontStyles;
  gViewerFontName: String;
  gViewerFontSize: Integer;
  gViewerFontStyle: TFontStyles;

  gViewerPos:TControlPosition;
  gEditorPos:TControlPosition;

  gViewerImageStretch: Boolean;
  
  { File panels color page }
  
  gBackColor, //Background color
  gBackColor2, //Background color 2
  gForeColor,  //text color
  gMarkColor,  // Mark color
  gCursorColor, //Cursor color
  gCursorText : TColor; //text color under cursor
  gUseInvertedSelection:boolean=false;
  
  gShowIcons : Boolean;
  gIconsSize,
  gNewIconsSize : Integer;
  gUseMmapInSearch : Boolean;
  gCustomDriveIcons : Boolean; // for use custom drive icons under windows
  
  { File operations page }

  gCopyBlockSize : Integer;
  gDropReadOnlyFlag : Boolean = True;
  gWipePassNumber: Integer;
  gProcessComments: Boolean;
  
  gRenameSelOnlyName:boolean;

  { Folder tabs page }

  gDirTabOptions,
  gDirTabLimit : Integer;
  
  { Log page }
  gLogFile : Boolean;
  gLogFileName : String;
  gLogOptions : TLogOptions = [log_cp_mv_ln, log_delete, log_dir_op, log_arc_op,
                               log_vfs_op, log_success, log_errors, log_info];

  { Configuration page }
  gUseIniInProgramDir,
  gSaveDirHistory,
  gSaveCmdLineHistory,
  gSaveFileMaskHistory : Boolean;
  
  {  Quick Search page}
  gQuickSearch : Boolean;
  gQuickSearchMode : TShiftState = [ssCtrl, ssAlt];
  gQuickSearchMatchBeginning,
  gQuickSearchMatchEnding : Boolean;

  { Misc page }
  gGridVertLine,
  gGridHorzLine,
  gShowWarningMessages: Boolean;
  
  {HotKey Manager}
  HotMan:THotKeyManager;
  
  {Actions}
  Actions:TActs;
  
const
  { Tabs options }
  tb_always_visible = 1;
  tb_multiple_lines = 2;
  tb_same_width = 4;
  tb_text_length_limit = 8;
  tb_confirm_close_all = 16;
  tb_close_on_dbl_click = 32;
  tb_open_new_in_foreground = 64;
  tb_open_new_near_current = 128;
  tb_show_asterisk_for_locked = 256;
  tb_activate_panel_on_click = 512;

function LoadGlobs : Boolean;
procedure SaveGlobs;
function LoadStringsFromFile(var list:TStringListEx; const sFileName:String):boolean;

procedure LoadDefaultHotkeyBindings;

procedure ResizeToScreen(Control:TControl; Width:integer=1024; Height:integer=768);

// for debugging only, can be removed
procedure dbgShowWindowPos(const pos: TControlPosition);

const
  cMaxStringItems=50;
  
var
  gIni:TIniFileEx = nil;

implementation
uses
   Forms, LCLProc, SysUtils, uGlobsPaths, uLng, uShowMsg, uFileProcs, uOSUtils;

procedure LoadDefaultHotkeyBindings;
begin
//  if not assigned(HotMan) then exit;
  With HotMan do
    begin
      AddHotKey('Alt+X','cm_Exit','','FrmMain','FrmMain');
      AddHotKey('F2','cm_RenameOnly','','FrmMain','FrmMain');
      AddHotKey('F3','cm_View','','FrmMain','FrmMain');
      AddHotKey('F4','cm_Edit','','FrmMain','FrmMain');
      AddHotKey('F5','cm_Copy','','FrmMain','FrmMain');
      AddHotKey('F6','cm_Rename','','FrmMain','FrmMain');
      AddHotKey('F7','cm_MakeDir','','FrmMain','FrmMain');
      AddHotKey('F1','cm_About','','FrmMain','FrmMain');
      AddHotKey('F9','cm_RunTerm','','FrmMain','FrmMain');
      AddHotKey('Ctrl+A','cm_MarkMarkAll','','FrmMain','FrmMain');
      AddHotKey('Ctrl+R','cm_Refresh','','FrmMain','FrmMain');
      AddHotKey('Alt+F7','cm_Search','','FrmMain','FrmMain');
      AddHotKey('Ctrl+D','cm_DirHotList','','FrmMain','FrmMain');
      AddHotKey('Ctrl+F3','cm_SortByName','','FrmMain','FrmMain');
      AddHotKey('Ctrl+F4','cm_SortByExt','','FrmMain','FrmMain');
      AddHotKey('Ctrl+F6','cm_SortBySize','','FrmMain','FrmMain');
      AddHotKey('Ctrl+F5','cm_SortByDate','','FrmMain','FrmMain');
      AddHotKey('Ctrl+M','cm_MultiRename','','FrmMain','FrmMain');
      AddHotKey('Shift+F5','cm_CopySamePanel','','FrmMain','FrmMain');
      AddHotKey('Shift+F6','cm_RenameOnly','','FrmMain','FrmMain');
      AddHotKey('Shift+F4','cm_EditNew','','FrmMain','FrmMain');
      AddHotKey('Ctrl+H','cm_DirHistory','','FrmMain','FrmMain');
      AddHotKey('Ctrl+7','cm_ShowCmdLineHistory','','FrmMain','FrmMain');
      AddHotKey('Ctrl+L','cm_CalculateSpace','','FrmMain','FrmMain');
      AddHotKey('Alt+Enter','cm_FileProperties','','FrmMain','FrmMain');
      AddHotKey('Ctrl+T','cm_NewTab','','FrmMain','FrmMain');
      AddHotKey('Ctrl+W','cm_RemoveTab','','FrmMain','FrmMain');
      AddHotKey('Ctrl+Z','cm_EditComment','','FrmMain','FrmMain');
      AddHotKey('Alt+F5','cm_PackFiles','','FrmMain','FrmMain');
      AddHotKey('Alt+F9','cm_ExtractFiles','','FrmMain','FrmMain');
      AddHotKey('Alt+F1','cm_LeftOpenDrives','','FrmMain','FrmMain');
      AddHotKey('Alt+F2','cm_RightOpenDrives','','FrmMain','FrmMain');
      AddHotKey('Ctrl+P','cm_AddPathToCmdLine','','FrmMain','FrmMain');
      AddHotKey('Shift+F2','cm_FocusCmdLine','','FrmMain','FrmMain');
      AddHotKey('Ctrl+Left','cm_TransferLeft','','FrmMain','FrmMain');
      AddHotKey('Ctrl+Right','cm_TransferRight','','FrmMain','FrmMain');
      AddHotKey('Ctrl+PgDn','cm_OpenArchive','','FrmMain','FrmMain');
      AddHotKey('Ctrl+S','cm_QuickSearch','','FrmMain','FrmMain');
      AddHotKey('Ctrl+Alt+X','cm_CopyNamesToClip','','FrmMain','FrmMain');
      AddHotKey('Ctrl+Alt+C','cm_CopyFullNamesToClip','','FrmMain','FrmMain');
      AddHotKey('Alt+Z','cm_TargetEqualSource','','FrmMain','FrmMain');
      AddHotKey('Ctrl+U','cm_Exchange','','FrmMain','FrmMain');
      AddHotKey('Alt+Del','cm_Wipe','','FrmMain','FrmMain');
      AddHotKey('Ctrl+Tab','cm_NextTab','','FrmMain','FrmMain');
      AddHotKey('Shift+Ctrl+Tab','cm_PrevTab','','FrmMain','FrmMain');
      AddHotKey('Ctrl+X','cm_CutToClipboard','','FrmMain','FrmMain');
      AddHotKey('Ctrl+C','cm_CopyToClipboard','','FrmMain','FrmMain');
      AddHotKey('Ctrl+V','cm_PasteFromClipboard','','FrmMain','FrmMain');
//      AddHotKey('','cm_','','FrmMain','FrmMain');
    end;
end;

procedure ResizeToScreen(Control:TControl; Width:integer=1024; Height:integer=768);
var SWidth, SHeight,
     PersW, PersH,
     NewW, NewH :Integer;
begin
  SWidth:=Screen.DesktopWidth;
  SHeight:=Screen.DesktopHeight;

  if (SWidth=Width) and (SHeight=Height) then exit;

  PersW:=round((SWidth*100)/Width);
  PersH:=round((SHeight*100)/Height);

  NewW:=round((Control.Width*PersW)/100);
  NewH:=round((Control.Height*PersH)/100);

  Control.Width:=NewW;
  Control.Height:=NewH;
end;

// for debugging only, can be removed
procedure dbgShowWindowPos(const pos: TControlPosition);
begin
  DebugLn('TWindowPos');
  DebugLn('Left: ', IntToStr(pos.Left));
  DebugLn('Top:  ', IntToStr(pos.Top));
  DebugLn('Width: ', IntToStr(pos.Width));
  DebugLn('Height: ', IntToStr(pos.Height));
  DebugLn('END');
end;

procedure TControlPosition.Save(Control: TControl);
begin
  Left := Control.Left;
  Top := Control.Top;
  Width := Control.Width;
  Height := Control.Height;
end;

procedure TControlPosition.Restore(Control: TControl);
begin
  Control.Left := Left;
  Control.Top := Top;
  Control.Width := Width;
  Control.Height := Height;
  // Resize window for screen size if need
  ResizeToScreen(Control);
end;

procedure LoadWindowPos(var pos:TControlPosition; sPrefix:String);
begin
  pos.Left:=gIni.ReadInteger('Configuration', sPrefix+'left',50);
  pos.Top:=gIni.ReadInteger('Configuration', sPrefix+'top',50);
  pos.Width:= gIni.ReadInteger('Configuration', sPrefix+'width',300);
  pos.Height:= gIni.ReadInteger('Configuration', sPrefix+'height',400);
end;

procedure SaveWindowPos(pos: TControlPosition; sPrefix:String);
begin
  gIni.WriteInteger('Configuration', sPrefix+'left', pos.Left);
  gIni.WriteInteger('Configuration', sPrefix+'top', pos.Top);
  gIni.WriteInteger('Configuration', sPrefix+'width', pos.Width);
  gIni.WriteInteger('Configuration', sPrefix+'height', pos.Height);
end;

procedure InitGlobs;
begin
  { Create default configuration files if need }
  // main ini file
  if not mbFileExists(gpIniDir + 'doublecmd.ini') then
    CopyFile(gpCfgDir + 'doublecmd.ini', gpIniDir + 'doublecmd.ini');
  // toolbar file
  if not mbFileExists(gpIniDir + 'default.bar') then
    CopyFile(gpCfgDir + 'default.bar', gpIniDir + 'default.bar');
  // extension file
  if not mbFileExists(gpIniDir + 'doublecmd.ext') then
    CopyFile(gpCfgDir + 'doublecmd.ext.example', gpIniDir + 'doublecmd.ext.example');
  // pixmaps file
  if not mbFileExists(gpIniDir + 'pixmaps.txt') then
    CopyFile(gpCfgDir + 'pixmaps.txt', gpIniDir + 'pixmaps.txt');
  // editor highlight file1
  if not mbFileExists(gpIniDir + 'editor.col') then
    CopyFile(gpCfgDir + 'editor.col', gpIniDir + 'editor.col');
  // editor highlight file2
  if not mbFileExists(gpIniDir + 'twilight.col') then
    CopyFile(gpCfgDir + 'twilight.col', gpIniDir + 'twilight.col');
	
  gIni := TIniFileEx.Create(gpIniDir + 'doublecmd.ini');
  gExts := TExts.Create;
  gColorExt := TColorExt.Create;
  glsHotDir := TStringListEx.Create;
  glsDirHistory := TStringListEx.Create;
  glsMaskHistory := TStringListEx.Create;
  
  HotMan:=THotKeyManager.Create;
  Actions:=TActs.Create;
end;

procedure DeInitGlobs;
begin
  if assigned(gColorExt) then
    FreeAndNil(gColorExt);
  if assigned(glsDirHistory) then
    FreeAndNil(glsDirHistory);
  if assigned(glsHotDir) then
    FreeAndNil(glsHotDir);
  if Assigned(glsMaskHistory) then
    FreeAndNil(glsMaskHistory);
  if Assigned(gExts) then
    FreeAndNil(gExts);
  if Assigned(gIni) then
    FreeAndNil(gIni);
  if Assigned(gWdxPlugins) then
    gWdxPlugins.Free;
  if Assigned(gWCXPlugins) then
    FreeAndNil(gWCXPlugins);
  if Assigned(gWFXPlugins) then
    FreeAndNil(gWFXPlugins);
  if Assigned(ColSet) then
    ColSet.Free;
  if Assigned(HotMan) then
    HotMan.Free;
  if Assigned(Actions) then
    Actions.Free;
end;

function LoadGlobs : Boolean;
var
  Ini: TIniFileEx;
begin
  Result := False;
  DebugLn('Loading configuration...');
  InitGlobs;

  { Load location of configuration files }
  Ini := TIniFileEx.Create(gpCfgDir + 'doublecmd.ini', fmOpenRead);
  gUseIniInProgramDir := Ini.ReadBool('Configuration', 'UseIniInProgramDir', False);
  Ini.Free;
  
  { Layout page }
  
  gButtonBar := gIni.ReadBool('Layout', 'ButtonBar', True);
  gToolBarFlat := gIni.ReadBool('ButtonBar', 'FlatIcons', True);
  gToolBarIconSize := gIni.ReadInteger('ButtonBar', 'ButtonHeight', 16);
  gDriveBar1 := gIni.ReadBool('Layout', 'DriveBar1', True);
  gDriveBar2 := gIni.ReadBool('Layout', 'DriveBar2', True);
  gDriveBarFlat := gIni.ReadBool('Layout', 'DriveBarFlat', True);
  gDriveMenuButton := gIni.ReadBool('Layout', 'DriveMenuButton', True);
  gDirectoryTabs := gIni.ReadBool('Layout', 'DirectoryTabs', True);
  gCurDir := gIni.ReadBool('Layout', 'CurDir', True);
  gTabHeader := gIni.ReadBool('Layout', 'TabHeader', True);
  gStatusBar := gIni.ReadBool('Layout', 'StatusBar', True);
  gCmdLine := gIni.ReadBool('Layout', 'CmdLine', True);
  gLogWindow := gIni.ReadBool('Layout', 'LogWindow', True);
  gTermWindow := gIni.ReadBool('Layout', 'TermWindow', False);
  gKeyButtons := gIni.ReadBool('Layout', 'KeyButtons', True);
  gInterfaceFlat := gIni.ReadBool('Layout', 'InterfaceFlat', True);
  
  gShowSystemFiles := gIni.ReadBool('Configuration', 'ShowSystemFiles', False);
  gPOFileName := gIni.ReadString('Configuration', 'Language', '?');
  gRunInTerm := gIni.ReadString('Configuration', 'RunInTerm', RunInTerm);
  gCaseSensitiveSort := gIni.ReadBool('Configuration', 'CaseSensitiveSort', False);
  gLynxLike := gIni.ReadBool('Configuration', 'LynxLike', True);
  gDirSelect := gIni.ReadBool('Configuration', 'DirSelect', True);
  glsHotDir.CommaText := gIni.ReadString('Configuration', 'HotDir', '');
  gShortFileSizeFormat := gIni.ReadBool('Configuration', 'ShortFileSizeFormat', True);
  gScrollMode := gIni.ReadInteger('Configuration', 'ScrollMode', 0);
  gTrayIcon := gIni.ReadBool('Configuration', 'TrayIcon', False);
  gDateTimeFormat := gIni.ReadString('Configuration', 'DateTimeFormat', 'dd.mm.yy');
  gDriveBlackList:= gIni.ReadString('Configuration', 'DriveBlackList', '');
  gSpaceMovesDown := gIni.ReadBool('Configuration', 'SpaceMovesDown', False);

  gMouseSelectionEnabled:= gIni.ReadBool('Configuration', 'MouseSelectionEnabled', True);
  gMouseSelectionButton := gIni.ReadInteger('Configuration', 'MouseSelectionButton', 0);

  gDirTabOptions := gIni.ReadInteger('Configuration', 'DirTabOptions', 273);
  gDirTabLimit :=  gIni.ReadInteger('Configuration', 'DirTabLimit', 32);

  gUseExtEdit := gIni.ReadBool('Configuration', 'UseExtEdit', False);
  gUseExtView := gIni.ReadBool('Configuration', 'UseExtView', False);
  gUseExtDiff := gIni.ReadBool('Configuration', 'UseExtDiff', False);

  gExtEdit := gIni.ReadString('Configuration', 'ExtEdit', '');
  gExtView := gIni.ReadString('Configuration', 'ExtView', '');
  gExtDiff := gIni.ReadString('Configuration', 'ExtDiff', '');
  gRunTerm := gIni.ReadString('Configuration', 'RunTerm', RunTerm);

  gLuaLib:=gIni.ReadString('Configuration', 'LuaLib', gLuaLib);

  
  { Fonts }
  gFontName:=gIni.ReadString('Configuration', 'Font.Name', 'default');
  DebugLn('gFontName:',gFontName);
  gEditorFontName:=gIni.ReadString('Editor', 'Font.Name', MonoSpaceFont);
  DebugLn('gEditorFontName:',gEditorFontName);
  gViewerFontName:=gIni.ReadString('Viewer', 'Font.Name', MonoSpaceFont);
  DebugLn('gViewerEditorFontName:',gViewerFontName);
  gFontSize:=gIni.ReadInteger('Configuration', 'Font.Size', 10);
  gEditorFontSize:=gIni.ReadInteger('Editor', 'Font.Size', 14);
  gViewerFontSize:=gIni.ReadInteger('Viewer', 'Font.Size', 14);
  gFontStyle := TFontStyles(gIni.ReadInteger('Configuration', 'Font.Style', 1));
  gEditorFontStyle:= TFontStyles(gIni.ReadInteger('Editor', 'Font.Style', 0));
  gViewerFontStyle:= TFontStyles(gIni.ReadInteger('Viewer', 'Font.Style', 0));
  { Colors }
  gForeColor  := gIni.ReadInteger('Colors', 'ForeColor', clDefault);
  gBackColor := gIni.ReadInteger('Colors', 'BackColor', clWhite);
  gBackColor2 := gIni.ReadInteger('Colors', 'BackColor2', clWhite);
  gMarkColor := gIni.ReadInteger('Colors', 'MarkColor', clRed);
  gCursorColor := gIni.ReadInteger('Colors', 'CursorColor', clHighlight);
  gCursorText := gIni.ReadInteger('Colors', 'CursorText', clHighlightText);
  gUseInvertedSelection:= gIni.ReadBool('Colors', 'UseInvertedSelection', false);
  { File operations }
  gCopyBlockSize := gIni.ReadInteger('Configuration', 'CopyBlockSize', 65536);
  gDropReadOnlyFlag := gIni.ReadBool('Configuration', 'DropReadOnlyFlag', True);
  gUseMmapInSearch := gIni.ReadBool('Configuration', 'UseMmapInSearch', False);
  gWipePassNumber:= gIni.ReadInteger('Configuration', 'WipePassNumber', 1);
  gProcessComments := gIni.ReadBool('Configuration', 'ProcessComments', True);
  gRenameSelOnlyName:= gIni.ReadBool('Configuration', 'RenameSelOnlyName', false);
  { Log }
  gLogFile := gIni.ReadBool('Configuration', 'LogFile', True);
  gLogFileName := gIni.ReadString('Configuration', 'LogFileName', gpIniDir + 'doublecmd.log');
  gLogOptions := TLogOptions(gIni.ReadInteger('Configuration', 'LogOptions', Integer(gLogOptions)));
  { Configuration page }
  gSaveDirHistory := gIni.ReadBool('Configuration', 'SaveDirHistory', True);
  gSaveCmdLineHistory := gIni.ReadBool('Configuration', 'SaveCmdLineHistory', True);
  gSaveFileMaskHistory := gIni.ReadBool('Configuration', 'SaveFileMaskHistory', True);
  { Quick Search page}
  gQuickSearch := gIni.ReadBool('Configuration', 'QuickSearch', True);
  gQuickSearchMode := TShiftState(gIni.ReadInteger('Configuration', 'QuickSearchMode', Integer(gQuickSearchMode)));
  gQuickSearchMatchBeginning := gIni.ReadBool('Configuration', 'QuickSearchMatchBeginning', True);
  gQuickSearchMatchEnding := gIni.ReadBool('Configuration', 'QuickSearchMatchEnding', True);
  { Misc page }
  gGridVertLine:= gIni.ReadBool('Configuration', 'GridVertLine', False);
  gGridHorzLine:= gIni.ReadBool('Configuration', 'GridHorzLine', False);
  gShowWarningMessages := gIni.ReadBool('Configuration', 'ShowWarningMessages', True);

  gShowIcons := gIni.ReadBool('Configuration', 'ShowIcons', True);
  gIconsSize := gIni.ReadInteger('Configuration', 'IconsSize', 16);
  gNewIconsSize:= gIconsSize;
  gCustomDriveIcons := gIni.ReadBool('Configuration', 'CustomDriveIcons', False);

  gCutTextToColWidth := gIni.ReadBool('Configuration', 'CutTextToColWidth', False);

  gViewerImageStretch:=  gIni.ReadBool('Viewer', 'Image.Stretch', False);

  if mbFileExists(gpIniDir + 'doublecmd.ext') then
    gExts.LoadFromFile(gpIniDir + 'doublecmd.ext');

  if mbFileExists(gpIniDir + 'dirhistory.txt') then
    LoadStringsFromFile(glsDirHistory,gpIniDir + 'dirhistory.txt');

  if mbFileExists(gpIniDir + 'maskhistory.txt') then
    LoadStringsFromFile(glsMaskHistory, gpIniDir + 'maskhistory.txt');

  gColorExt.Load;

  DebugLn('Loading viewer position...');
  LoadWindowPos(gViewerPos, 'Viewer.');
  DebugLn('Loading editor position...');
  LoadWindowPos(gEditorPos, 'Editor.');

  { Localization }
  DoLoadLng;
  msgLoadLng;
  Result := True;
  
  {Wdx Plugins and columns}
  //---------------------
  gWdxPlugins:=TWDXModuleList.Create;
  gWdxPlugins.Load(gIni);
  ColSet:=TPanelColumnsList.Create;
  ColSet.Load(gIni);
  //---------------------
  { WCX plugins }
  gWCXPlugins:= TWCXModuleList.Create;
  gWCXPlugins.Load(gIni);
  { WFX plugins }
  gWFXPlugins:= TWFXModuleList.Create;
  gWFXPlugins.Load(gIni);
end;

function LoadStringsFromFile(var list:TStringListEx; const sFileName:String):boolean;
var
  i:Integer;
begin
  Assert(list <> nil,'LoadStringsFromFile: list=nil');
  list.Clear;
  Result:=False;
  if not mbFileExists(sFileName) then Exit;
  list.LoadFromFile(sFileName);
  for i:=list.Count-1 downto 0 do
    if i>cMaxStringItems then
      list.Delete(i)
    else
      Break;
  Result:=True;
end;


procedure SaveGlobs;
var
  Ini: TIniFileEx;
begin
  if gSaveDirHistory then
    glsDirHistory.SaveToFile(gpIniDir + 'dirhistory.txt');
  if gSaveFileMaskHistory then
    glsMaskHistory.SaveToFile(gpIniDir + 'maskhistory.txt');
	
  { Layout page }

  gIni.WriteBool('Layout', 'ButtonBar', gButtonBar);
  gIni.WriteBool('ButtonBar', 'FlatIcons', gToolBarFlat);
  gIni.WriteInteger('ButtonBar', 'ButtonHeight', gToolBarIconSize);
  gIni.WriteBool('Layout', 'DriveBar1', gDriveBar1);
  gIni.WriteBool('Layout', 'DriveBar2', gDriveBar2);
  gIni.WriteBool('Layout', 'DriveBarFlat', gDriveBarFlat);
  gIni.WriteBool('Layout', 'DriveMenuButton', gDriveMenuButton);
  gIni.WriteBool('Layout', 'DirectoryTabs', gDirectoryTabs);
  gIni.WriteBool('Layout', 'CurDir', gCurDir);
  gIni.WriteBool('Layout', 'TabHeader', gTabHeader);
  gIni.WriteBool('Layout', 'StatusBar', gStatusBar);
  gIni.WriteBool('Layout', 'CmdLine', gCmdLine);
  gIni.WriteBool('Layout', 'LogWindow', gLogWindow);
  gIni.WriteBool('Layout', 'TermWindow', gTermWindow);
  gIni.WriteBool('Layout', 'KeyButtons', gKeyButtons);
  gIni.WriteBool('Layout', 'InterfaceFlat', gInterfaceFlat);

  gIni.WriteBool('Configuration', 'ShowSystemFiles', gShowSystemFiles);
  gIni.WriteString('Configuration', 'Language', gPOFileName);
  gIni.WriteString('Configuration', 'RunInTerm', gRunInTerm);
  gIni.WriteBool('Configuration', 'CaseSensitiveSort', gCaseSensitiveSort);
  gIni.WriteBool('Configuration', 'LynxLike', gLynxLike);
  gIni.WriteBool('Configuration', 'DirSelect', gDirSelect);
  gIni.WriteString('Configuration', 'HotDir', glsHotDir.CommaText);
  gIni.WriteBool('Configuration', 'ShortFileSizeFormat', gShortFileSizeFormat);
  gIni.WriteInteger('Configuration', 'ScrollMode', gScrollMode);
  gIni.WriteBool('Configuration', 'TrayIcon', gTrayIcon);
  gIni.WriteString('Configuration', 'DateTimeFormat', gDateTimeFormat);
  gIni.WriteString('Configuration', 'DriveBlackList', gDriveBlackList);
  gIni.WriteBool('Configuration', 'SpaceMovesDown', gSpaceMovesDown);
  
  gIni.WriteBool('Configuration', 'MouseSelectionEnabled', gMouseSelectionEnabled);
  gIni.WriteInteger('Configuration', 'MouseSelectionButton', gMouseSelectionButton);

  gIni.WriteInteger('Configuration', 'DirTabOptions', gDirTabOptions);
  gIni.WriteInteger('Configuration', 'DirTabLimit', gDirTabLimit);

  gIni.WriteBool('Configuration', 'UseExtEdit', gUseExtEdit);
  gIni.WriteBool('Configuration', 'UseExtView', gUseExtView);
  gIni.WriteBool('Configuration', 'UseExtDiff', gUseExtDiff);

  gIni.WriteString('Configuration', 'ExtEdit', gExtEdit);
  gIni.WriteString('Configuration', 'ExtView', gExtView);
  gIni.WriteString('Configuration', 'ExtDiff', gExtDiff);
  gIni.WriteString('Configuration', 'RunTerm', gRunTerm);

  gIni.WriteString('Configuration', 'LuaLib', gLuaLib);
  
  { Fonts }
  gIni.WriteString('Configuration', 'Font.Name', gFontName);
  gIni.WriteString('Editor', 'Font.Name', gEditorFontName);
  gIni.WriteString('Viewer', 'Font.Name', gViewerFontName);

  gIni.WriteInteger('Configuration', 'Font.Size', gFontSize);
  gIni.WriteInteger('Editor', 'Font.Size', gEditorFontSize);
  gIni.WriteInteger('Viewer', 'Font.Size', gViewerFontSize);

  gIni.WriteInteger('Configuration', 'Font.Style', Integer(gFontStyle));
  gIni.WriteInteger('Editor', 'Font.Style', Integer(gEditorFontStyle));
  gIni.WriteInteger('Viewer', 'Font.Style', Integer(gViewerFontStyle));
  { Colors }
  gIni.WriteInteger('Colors', 'ForeColor', gForeColor);
  gIni.WriteInteger('Colors', 'BackColor', gBackColor);
  gIni.WriteInteger('Colors', 'BackColor2', gBackColor2);
  gIni.WriteInteger('Colors', 'MarkColor', gMarkColor);
  gIni.WriteInteger('Colors', 'CursorColor', gCursorColor);
  gIni.WriteInteger('Colors', 'CursorText', gCursorText);
  gIni.WriteBool('Colors', 'UseInvertedSelection', gUseInvertedSelection);
  

  { File operations }
  gIni.WriteInteger('Configuration', 'CopyBlockSize', gCopyBlockSize);
  gIni.WriteBool('Configuration', 'DropReadOnlyFlag', gDropReadOnlyFlag);
  gIni.WriteBool('Configuration', 'UseMmapInSearch', gUseMmapInSearch);
  gIni.WriteInteger('Configuration', 'WipePassNumber', gWipePassNumber);
  gIni.WriteBool('Configuration', 'ProcessComments', gProcessComments);
  gIni.WriteBool('Configuration', 'RenameSelOnlyName', gRenameSelOnlyName);
  { Log }
  gIni.WriteBool('Configuration', 'LogFile', gLogFile);
  gIni.WriteString('Configuration', 'LogFileName', gLogFileName);
  gIni.WriteInteger('Configuration', 'LogOptions', Integer(gLogOptions));
  { Configuration page }
  gIni.WriteBool('Configuration', 'SaveDirHistory', gSaveDirHistory);
  gIni.WriteBool('Configuration', 'SaveCmdLineHistory', gSaveCmdLineHistory);
  gIni.WriteBool('Configuration', 'SaveFileMaskHistory', gSaveFileMaskHistory);
  { Quick Search page}
  gIni.WriteBool('Configuration', 'QuickSearch', gQuickSearch);
  gIni.WriteInteger('Configuration', 'QuickSearchMode', Integer(gQuickSearchMode));
  gIni.WriteBool('Configuration', 'QuickSearchMatchBeginning', gQuickSearchMatchBeginning);
  gIni.WriteBool('Configuration', 'QuickSearchMatchEnding', gQuickSearchMatchEnding);
  { Misc page }
  gIni.WriteBool('Configuration', 'GridVertLine', gGridVertLine);
  gIni.WriteBool('Configuration', 'GridHorzLine', gGridHorzLine);
  gIni.WriteBool('Configuration', 'ShowWarningMessages', gShowWarningMessages);

  gIni.WriteBool('Configuration', 'ShowIcons', gShowIcons);
  gIni.WriteInteger('Configuration', 'IconsSize', gNewIconsSize);
  
  gIni.WriteBool('Configuration', 'CutTextToColWidth', gCutTextToColWidth);

  gIni.WriteBool('Viewer', 'Image.Stretch', gViewerImageStretch);

  SaveWindowPos(gViewerPos, 'Viewer.');
  SaveWindowPos(gEditorPos, 'Editor.');
  
  gExts.SaveToFile(gpIniDir + 'doublecmd.ext');
  gColorExt.Save;
  
  //TODO: Save hotkeys
  //HotMan.Save();

  { Plugins }
  gWdxPlugins.Save(gIni);
  gWFXPlugins.Save(gIni);
  gWCXPlugins.Save(gIni);

  { Save location of configuration files }
  try
    Ini:= TIniFileEx.Create(gpCfgDir + 'doublecmd.ini');
    Ini.WriteBool('Configuration', 'UseIniInProgramDir', gUseIniInProgramDir);
  finally
    Ini.Free;
  end;
end;

initialization

finalization
  DeInitGlobs;
end.

{
   Double Commander
   ------------------------------------------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   Globals variables and some consts

   contributors:

   Copyright (C) 2006-2008 Alexander Koblov (Alexx2000@mail.ru)
}

unit uGlobs;

interface
uses
  Classes, Controls, uExts, uColorExt, Graphics, IniFiles, uWDXModule, uColumns;

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
  gPOFileName : String;
  
  { WDX plugins }
  WdxPlugins:TWDXModuleList;
  
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
  gKeyButtons,
  gInterfaceFlat : Boolean;
  
  gToolBarIconSize : Integer;
  
  gDirSortFirst:Boolean=True; // want to show dir first in panels
  gDirHistoryCount:Integer=30; // how many history we remember
  gShowSystemFiles:Boolean=True;
  gTerm:String='/usr/X11R6/bin/xterm -e bash -i -c %s';
  gRunTerm:String='/usr/X11R6/bin/xterm';
  gCaseSensitiveSort:Boolean=True;
  gLynxLike:Boolean=True;
  gDirSelect:Boolean=True;
  gMouseSelectionEnabled:Boolean=True;
  glsHotDir:TStringList;
  glsDirHistory:TStringList;
  glsMaskHistory : TStringList;
  gCutTextToColWidth : Boolean;
  gScrollMode: Integer;

  gShortFileSizeFormat:Boolean=True;

  gDateTimeFormat : String;
  
  { Tools page }

  gUseExtEdit:Boolean=False;
  gUseExtView:Boolean=False;
  gUseExtDiff:Boolean=False;

  gExtEdit:String='emacs';
  gExtView:String='emacs';
  gExtDiff:String='gtk-diff';
  gExts:TExts;
  gColorExt:TColorExt;

  gFontName:String;
  gFontSize:Integer;
  gFontWeight : Byte;
  gEditorFontName:String;
  gEditorSize:Integer;
  gViewerFontName:String;
  gViewerSize:Integer;

  gViewerPos:TControlPosition;
  gEditorPos:TControlPosition;
  
  { File panels color page }
  
  gBackColor, //Background color
  gBackColor2, //Background color 2
  gForeColor,  //text color
  gMarkColor,  // Mark color
  gCursorColor, //Cursor color
  gCursorText : TColor; //text color under cursor
  
  gShowIcons : Boolean;
  gIconsSize : Integer;
  gUseMmapInSearch : Boolean;
  gCustomDriveIcons : Boolean; // for use custom drive icons under windows
  
  { File operations page }

  gCopyBlockSize : Integer;
  gDropReadOnlyFlag : Boolean = True;

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
function LoadStringsFromFile(var list:TStringList; const sFileName:String):boolean;

// for debugging only, can be removed
procedure dbgShowWindowPos(const pos: TControlPosition);

const
  cMaxStringItems=50;
  
var
  gIni:TIniFile = nil;

implementation
uses
   LCLProc, SysUtils, uGlobsPaths, uLng, uShowMsg, uFileProcs, uOSUtils;

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
  { Load location of configuration files }
  gIni := TIniFile.Create(gpCfgDir + 'doublecmd.ini');
  gUseIniInProgramDir := gIni.ReadBool('Configuration', 'UseIniInProgramDir', True);
  gIni.Free;

  { Create default configuration files if need }
  // main ini file
  if not FileExists(gpIniDir + 'doublecmd.ini') then
    CopyFile(gpCfgDir + 'doublecmd.ini', gpIniDir + 'doublecmd.ini');
  // toolbar file
  if not FileExists(gpIniDir + 'default.bar') then
    CopyFile(gpCfgDir + 'default.bar', gpIniDir + 'default.bar');
  // extension file
  if not FileExists(gpIniDir + 'doublecmd.ext') then
    CopyFile(gpCfgDir + 'doublecmd.ext', gpIniDir + 'doublecmd.ext');
  // pixmaps file
  if not FileExists(gpIniDir + 'pixmaps.txt') then
    CopyFile(gpCfgDir + 'pixmaps.txt', gpIniDir + 'pixmaps.txt');
  // editor highlight file1
  if not FileExists(gpIniDir + 'editor.col') then
    CopyFile(gpCfgDir + 'editor.col', gpIniDir + 'editor.col');
  // editor highlight file2
  if not FileExists(gpIniDir + 'twilight.col') then
    CopyFile(gpCfgDir + 'twilight.col', gpIniDir + 'twilight.col');
	
  gIni := TIniFile.Create(gpIniDir + 'doublecmd.ini');
  gExts := TExts.Create;
  gColorExt := TColorExt.Create;
  glsHotDir := TStringList.Create;
  glsDirHistory := TStringList.Create;
  glsMaskHistory := TStringList.Create;
  //---------------------
  WdxPlugins:=TWDXModuleList.Create;
  WdxPlugins.Load(gIni);
  //---------------------
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

  { Save location of configuration files }
  gIni := TIniFile.Create(gpCfgDir + 'doublecmd.ini');
  gIni.WriteBool('Configuration', 'UseIniInProgramDir', gUseIniInProgramDir);
  gIni.Free;
  
  WdxPlugins.Free;
end;

function LoadGlobs : Boolean;
begin
  Result := False;
  DebugLn('Loading configuration...');
  InitGlobs;
  
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
  gKeyButtons := gIni.ReadBool('Layout', 'KeyButtons', True);
  gInterfaceFlat := gIni.ReadBool('Layout', 'InterfaceFlat', True);
  
  gShowSystemFiles := gIni.ReadBool('Configuration', 'ShowSystemFiles', False);
  gPOFileName := gIni.ReadString('Configuration', 'Language', '');
  gTerm := gIni.ReadString('Configuration', 'Term', gTerm);
  gCaseSensitiveSort := gIni.ReadBool('Configuration', 'CaseSensitiveSort', False);
  gLynxLike := gIni.ReadBool('Configuration', 'LynxLike', True);
  gDirSelect := gIni.ReadBool('Configuration', 'DirSelect', True);
  glsHotDir.CommaText := gIni.ReadString('Configuration', 'HotDir', '');
  gShortFileSizeFormat := gIni.ReadBool('Configuration', 'ShortFileSizeFormat', True);
  gScrollMode := gIni.ReadInteger('Configuration', 'ScrollMode', 0);
  gDateTimeFormat := gIni.ReadString('Configuration', 'DateTimeFormat', 'dd.mm.yy');

  gDirTabOptions := gIni.ReadInteger('Configuration', 'DirTabOptions', 0);
  gDirTabLimit :=  gIni.ReadInteger('Configuration', 'DirTabLimit', 32);

  gUseExtEdit := gIni.ReadBool('Configuration', 'UseExtEdit', False);
  gUseExtView := gIni.ReadBool('Configuration', 'UseExtView', False);
  gUseExtDiff := gIni.ReadBool('Configuration', 'UseExtDiff', False);

  gExtEdit := gIni.ReadString('Configuration', 'ExtEdit', '');
  gExtView := gIni.ReadString('Configuration', 'ExtView', '');
  gExtDiff := gIni.ReadString('Configuration', 'ExtDiff', '');
  gRunTerm := gIni.ReadString('Configuration', 'RunTerm', gRunTerm);
  { Fonts }
  gFontName:=gIni.ReadString('Configuration', 'FontName', 'default');
  gFontWeight := gIni.ReadInteger('Configuration', 'FontWeight', 1);
  DebugLn('gFontName:',gFontName);
  gEditorFontName:=gIni.ReadString('Configuration', 'FontEditorName', 'default');
  DebugLn('gEditorFontName:',gEditorFontName);
  gViewerFontName:=gIni.ReadString('Configuration', 'FontViewerName', 'default');
  DebugLn('gViewerEditorFontName:',gViewerFontName);
  gFontSize:=gIni.ReadInteger('Configuration', 'FontSize', 10);
  gEditorSize:=gIni.ReadInteger('Configuration', 'EditorSize', 14);
  gViewerSize:=gIni.ReadInteger('Configuration', 'ViewerSize', 14);
  { Colors }
  gForeColor  := gIni.ReadInteger('Colors', 'ForeColor', clBlack);
  gBackColor := gIni.ReadInteger('Colors', 'BackColor', clWhite);
  gBackColor2 := gIni.ReadInteger('Colors', 'BackColor2', clWhite);
  gMarkColor := gIni.ReadInteger('Colors', 'MarkColor', clRed);
  gCursorColor := gIni.ReadInteger('Colors', 'CursorColor', clHighlight);
  gCursorText := gIni.ReadInteger('Colors', 'CursorText', clHighlightedText);
  { File operations }
  gCopyBlockSize := gIni.ReadInteger('Configuration', 'CopyBlockSize', 16384);
  gDropReadOnlyFlag := gIni.ReadBool('Configuration', 'DropReadOnlyFlag', True);
  gUseMmapInSearch := gIni.ReadBool('Configuration', 'UseMmapInSearch', False);
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
  
  gShowIcons := gIni.ReadBool('Configuration', 'ShowIcons', True);
  gIconsSize := gIni.ReadInteger('Configuration', 'IconsSize', 16);

  gCustomDriveIcons := gIni.ReadBool('Configuration', 'CustomDriveIcons', False);

  gCutTextToColWidth := gIni.ReadBool('Configuration', 'CutTextToColWidth', False);

  if FileExists(gpIniDir + 'doublecmd.ext') then
    gExts.LoadFromFile(gpIniDir + 'doublecmd.ext');

  if FileExists(gpIniDir + 'dirhistory.txt') then
    LoadStringsFromFile(glsDirHistory,gpIniDir + 'dirhistory.txt');

  if FileExists(gpIniDir + 'maskhistory.txt') then
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
end;

function LoadStringsFromFile(var list:TStringList; const sFileName:String):boolean;
var
  i:Integer;
begin
  Assert(list <> nil,'LoadStringsFromFile: list=nil');
  list.Clear;
  Result:=False;
  if not FileExists(sFileName) then Exit;
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
  x:Integer;
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
  gIni.WriteBool('Layout', 'KeyButtons', gKeyButtons);
  gIni.WriteBool('Layout', 'InterfaceFlat', gInterfaceFlat);

  gIni.WriteBool('Configuration', 'ShowSystemFiles', gShowSystemFiles);
  gIni.WriteString('Configuration', 'Language', gPOFileName);
  gIni.WriteString('Configuration', 'Term', gTerm);
  gIni.WriteBool('Configuration', 'CaseSensitiveSort', gCaseSensitiveSort);
  gIni.WriteBool('Configuration', 'LynxLike', gLynxLike);
  gIni.WriteBool('Configuration', 'DirSelect', gDirSelect);
  gIni.WriteString('Configuration', 'HotDir', glsHotDir.CommaText);
  gIni.WriteBool('Configuration', 'ShortFileSizeFormat', gShortFileSizeFormat);
  gIni.WriteInteger('Configuration', 'ScrollMode', gScrollMode);
  gIni.WriteString('Configuration', 'DateTimeFormat', gDateTimeFormat);

  gIni.WriteInteger('Configuration', 'DirTabOptions', gDirTabOptions);
  gIni.WriteInteger('Configuration', 'DirTabLimit', gDirTabLimit);

  gIni.WriteBool('Configuration', 'UseExtEdit', gUseExtEdit);
  gIni.WriteBool('Configuration', 'UseExtView', gUseExtView);
  gIni.WriteBool('Configuration', 'UseExtDiff', gUseExtDiff);

  gIni.WriteString('Configuration', 'ExtEdit', gExtEdit);
  gIni.WriteString('Configuration', 'ExtView', gExtView);
  gIni.WriteString('Configuration', 'ExtDiff', gExtDiff);
  gIni.WriteString('Configuration', 'RunTerm', gRunTerm);
  { Fonts }
  gIni.WriteString('Configuration', 'FontName', gFontName);
  gIni.WriteInteger('Configuration', 'FontWeight', gFontWeight);
  gIni.WriteString('Configuration', 'FontEditorName', gEditorFontName);
  gIni.WriteString('Configuration', 'FontViewerName', gViewerFontName);

  gIni.WriteInteger('Configuration', 'FontSize', gFontSize);
  gIni.WriteInteger('Configuration', 'EditorSize', gEditorSize);
  gIni.WriteInteger('Configuration', 'ViewerSize', gViewerSize);
  { Colors }
  gIni.WriteInteger('Colors', 'ForeColor', gForeColor);
  gIni.WriteInteger('Colors', 'BackColor', gBackColor);
  gIni.WriteInteger('Colors', 'BackColor2', gBackColor2);
  gIni.WriteInteger('Colors', 'MarkColor', gMarkColor);
  gIni.WriteInteger('Colors', 'CursorColor', gCursorColor);
  gIni.WriteInteger('Colors', 'CursorText', gCursorText);
  { File operations }
  gIni.WriteInteger('Configuration', 'CopyBlockSize', gCopyBlockSize);
  gIni.WriteBool('Configuration', 'DropReadOnlyFlag', gDropReadOnlyFlag);
  gIni.WriteBool('Configuration', 'UseMmapInSearch', gUseMmapInSearch);
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

  gIni.WriteBool('Configuration', 'ShowIcons', gShowIcons);
  gIni.WriteInteger('Configuration', 'IconsSize', gIconsSize);
  
  gIni.WriteBool('Configuration', 'CutTextToColWidth', gCutTextToColWidth);

  SaveWindowPos(gViewerPos, 'Viewer.');
  SaveWindowPos(gEditorPos, 'Editor.');
  
  gExts.SaveToFile(gpIniDir + 'doublecmd.ext');
  gColorExt.Save;
  
  DeInitGlobs;
end;

initialization

finalization

end.

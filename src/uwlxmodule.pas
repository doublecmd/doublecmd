{
   Double Commander
   -------------------------------------------------------------------------
   WLX-API implementation (TC WLX-API v2.0).

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit uWlxModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, uDetectStr, uWlxPrototypes, WlxPlugin,
  DCClassesUtf8, uDCUtils, LCLProc, LCLType, DCXmlConfig
  {$IFDEF LCLWIN32}
  , Windows
  {$ENDIF}
  {$IFDEF LCLGTK}
  , gtk, glib, gdk, gtkproc
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , gtk2, glib2, gtk2proc
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4, qtwidgets
  // The Qt widgetset must be used to load plugins on qt
  {$ENDIF}
  {$IFDEF LCLQT5}
  , qt5, qtwidgets
  {$ENDIF}
  ;

type

  { TWlxModule }

  TWlxModule = class
  protected
    // a) Mandatory (must be implemented)
    ListLoad: TListLoad;
    // b) Optional (must NOT be implemented if unsupported!)
    ListLoadNext: TListLoadNext;
    ListCloseWindow: TListCloseWindow;
    ListGetDetectString: TListGetDetectString;
    ListSearchText: TListSearchText;
    ListSearchDialog: TListSearchDialog;
    ListSendCommand: TListSendCommand;
    ListPrint: TListPrint;
    ListNotificationReceived: TListNotificationReceived;
    ListSetDefaultParams: TListSetDefaultParams;
    ListGetPreviewBitmap: TListGetPreviewBitmap;
    // c) Unicode
    ListLoadW: TListLoadW;
    ListLoadNextW: TListLoadNextW;
    ListSearchTextW: TListSearchTextW;
    ListPrintW: TListPrintW;
    ListGetPreviewBitmapW: TListGetPreviewBitmapW;
  private
    FModuleHandle: TLibHandle;  // Handle to .DLL or .so
    FParser: TParserControl;
    FPluginWindow: HWND;
    function GetCanPreview: Boolean;
    function GetCanPrint: Boolean;
    function GIsLoaded: Boolean;
  public
    Name: String;
    FileName: String;
    DetectStr: String;
    pShowFlags: Integer;
    Enabled: Boolean;
    //---------------------
    constructor Create;
    destructor Destroy; override;
    //---------------------
    function LoadModule: Boolean;
    procedure UnloadModule;
    //---------------------
    function CallListLoad(ParentWin: HWND; FileToLoad: String; ShowFlags: Integer): HWND;
    function CallListLoadNext(ParentWin: HWND; FileToLoad: String; ShowFlags: Integer): Integer;
    function CallListGetDetectString: String;
    procedure CallListSetDefaultParams;
    procedure CallListCloseWindow;
    function CallListGetPreviewBitmap(FileToLoad: String; Width, Height: Integer; ContentBuf: String): HBITMAP;
    function CallListNotificationReceived(Msg, wParam, lParam: Integer): Integer;
    function CallListPrint(FileToPrint, DefPrinter: String; PrintFlags: Integer; var Margins: trect): Integer;
    function CallListSearchDialog(FindNext: Integer): Integer;
    function CallListSearchText(SearchString: String; SearchParameter: Integer): Integer;
    function CallListSendCommand(Command, Parameter: Integer): Integer;
    //---------------------
    function FileParamVSDetectStr(AFileName: String; bForce: Boolean): Boolean;
    //---------------------
    procedure SetFocus;
    procedure ResizeWindow(aRect: TRect);
    //---------------------
    property IsLoaded: Boolean read GIsLoaded;
    property ModuleHandle: TLibHandle read FModuleHandle write FModuleHandle;
    property CanPreview: Boolean read GetCanPreview;
    property PluginWindow: HWND read FPluginWindow;
    property CanPrint: Boolean read GetCanPrint;
  end;

  { TWLXModuleList }

  TWLXModuleList = class
  private
    Flist: TStringList;
    function GetCount: Integer;
  public
    //---------------------
    constructor Create;
    destructor Destroy; override;
    //---------------------
    procedure Clear;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    function ComputeSignature(seed: dword): dword;
    procedure DeleteItem(Index: Integer);
    //---------------------
    function Add(Item: TWlxModule): Integer; overload;
    function Add(FileName: String): Integer; overload;
    function Add(AName, FileName, DetectStr: String): Integer; overload;
    //---------------------
    procedure Assign(OtherList: TWLXModuleList);
    function IndexOfName(const AName: string): Integer;
    //---------------------
    function IsLoaded(AName: String): Boolean; overload;
    function IsLoaded(Index: Integer): Boolean; overload;
    function LoadModule(AName: String): Boolean; overload;
    function LoadModule(Index: Integer): Boolean; overload;
    //---------------------
    function GetWlxModule(Index: Integer): TWlxModule; overload;
    function GetWlxModule(AName: String): TWlxModule; overload;
    //---------------------
    //---------------------
    //property WlxList:TStringList read Flist;
    property Count: Integer read GetCount;
  end;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  FileUtil,

  //DC
  uComponentsSignature, uDebug, DCOSUtils, DCConvertEncoding, uOSUtils,
  uGlobsPaths, uGlobs;

const
  WlxIniFileName = 'wlx.ini';

{$IF DEFINED(LCLWIN32)}
var
  WindowProcAtom: PWideChar;

function PluginProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WindowProc: WNDPROC;
begin
  if Msg = WM_KEYDOWN then
  begin
    PostMessage(GetParent(hWnd), Msg, wParam, lParam);
  end;
  WindowProc := WNDPROC(GetPropW(hWnd, WindowProcAtom));
  if Assigned(WindowProc) then
    Result := CallWindowProc(WindowProc, hWnd, Msg, wParam, lParam)
  else
    Result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;
{$ENDIF}

procedure WlxPrepareContainer(var {%H-}ParentWin: HWND);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  ParentWin := HWND(GetFixedWidget(Pointer(ParentWin)));
{$ELSEIF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  ParentWin := HWND(TQtWidget(ParentWin).GetContainerWidget);
{$ENDIF}
end;

{ TWlxModule }

function TWlxModule.GIsLoaded: Boolean;
begin
  Result := FModuleHandle <> 0;
end;

function TWlxModule.GetCanPrint: Boolean;
begin
  Result := Assigned(ListPrint) or Assigned(ListPrintW);
end;

function TWlxModule.GetCanPreview: Boolean;
begin
  Result:= Assigned(ListGetPreviewBitmap) or Assigned(ListGetPreviewBitmapW);
end;

constructor TWlxModule.Create;
begin
  Enabled := True;
  FParser := TParserControl.Create;
end;

destructor TWlxModule.Destroy;
begin
{$IF NOT DEFINED(LCLWIN32)}
  if GIsLoaded then
    UnloadModule;
{$ENDIF}
  if Assigned(FParser) then
    FParser.Free;

  inherited Destroy;
end;

function TWlxModule.LoadModule: Boolean;
begin
  // DCDebug('WLXM LoadModule entered');
  FModuleHandle := mbLoadLibrary(mbExpandFileName(Self.FileName));
  Result := (FModuleHandle <> NilHandle);
  if FModuleHandle = NilHandle then Exit;
  { Mandatory }
  ListLoad := TListLoad(GetProcAddress(FModuleHandle, 'ListLoad'));
  { Optional }
  ListLoadNext := TListLoadNext(GetProcAddress(FModuleHandle, 'ListLoadNext'));
  ListCloseWindow := TListCloseWindow(GetProcAddress(FModuleHandle, 'ListCloseWindow'));
  ListGetDetectString := TListGetDetectString(GetProcAddress(FModuleHandle, 'ListGetDetectString'));
  ListSearchText := TListSearchText(GetProcAddress(FModuleHandle, 'ListSearchText'));
  ListSearchDialog := TListSearchDialog(GetProcAddress(FModuleHandle, 'ListSearchDialog'));
  ListSendCommand := TListSendCommand(GetProcAddress(FModuleHandle, 'ListSendCommand'));
  ListPrint := TListPrint(GetProcAddress(FModuleHandle, 'ListPrint'));
  ListNotificationReceived := TListNotificationReceived(GetProcAddress(FModuleHandle, 'ListNotificationReceived'));
  ListSetDefaultParams := TListSetDefaultParams(GetProcAddress(FModuleHandle, 'ListSetDefaultParams'));
  ListGetPreviewBitmap := TListGetPreviewBitmap(GetProcAddress(FModuleHandle, 'ListGetPreviewBitmap'));
  { Unicode }
  ListLoadW := TListLoadW(GetProcAddress(FModuleHandle, 'ListLoadW'));
  ListLoadNextW := TListLoadNextW(GetProcAddress(FModuleHandle, 'ListLoadNextW'));
  ListSearchTextW := TListSearchTextW(GetProcAddress(FModuleHandle, 'ListSearchTextW'));
  ListPrintW := TListPrintW(GetProcAddress(FModuleHandle, 'ListPrintW'));
  ListGetPreviewBitmapW := TListGetPreviewBitmapW(GetProcAddress(FModuleHandle, 'ListGetPreviewBitmapW'));
  // ListSetDefaultParams must be called immediately after loading the DLL, before ListLoad.
  CallListSetDefaultParams;
  // DCDebug('WLXM LoadModule Leaved');
end;

procedure TWlxModule.UnloadModule;
begin
{$IF NOT (DEFINED(LCLQT) or DEFINED(LCLQT5) or DEFINED(LCLGTK2))}
{$IF (not DEFINED(LINUX)) or ((FPC_VERSION > 2) or ((FPC_VERSION=2) and (FPC_RELEASE >= 5)))}
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
{$ENDIF}
  FModuleHandle := 0;
  { Mandatory }
  ListLoad := nil;
  { Optional }
  ListLoadNext := nil;
  ListCloseWindow := nil;
  ListGetDetectString := nil;
  ListSearchText := nil;
  ListSearchDialog := nil;
  ListSendCommand := nil;
  ListPrint := nil;
  ListNotificationReceived := nil;
  ListSetDefaultParams := nil;
  ListGetPreviewBitmap := nil;
  { Unicode }
  ListLoadW := nil;
  ListLoadNextW := nil;
  ListSearchTextW := nil;
  ListPrintW := nil;
  ListGetPreviewBitmapW := nil;
{$ENDIF}
end;

function TWlxModule.CallListLoad(ParentWin: HWND; FileToLoad: String; ShowFlags: Integer): HWND;
begin
  WlxPrepareContainer(ParentWin);

  if Assigned(ListLoadW) then
    FPluginWindow := ListLoadW(ParentWin, PWideChar(UTF8Decode(FileToLoad)), ShowFlags)
  else if Assigned(ListLoad) then
    FPluginWindow := ListLoad(ParentWin, PAnsiChar(CeUtf8ToSys(FileToLoad)), ShowFlags)
  else
    Exit(wlxInvalidHandle);

{$IF DEFINED(LCLWIN32)}
  // Subclass plugin window to catch some hotkeys like 'n' or 'p'.
  Result := SetWindowLongPtr(FPluginWindow, GWL_WNDPROC, LONG_PTR(@PluginProc));
  Windows.SetPropW(FPluginWindow, WindowProcAtom, Result);
{$ENDIF}

  Result := FPluginWindow;
end;

function TWlxModule.CallListLoadNext(ParentWin: HWND; FileToLoad: String; ShowFlags: Integer): Integer;
begin
  WlxPrepareContainer(ParentWin);

  if Assigned(ListLoadNextW) then
    Result := ListLoadNextW(ParentWin, FPluginWindow, PWideChar(UTF8Decode(FileToLoad)), ShowFlags)
  else if Assigned(ListLoadNext) then
    Result := ListLoadNext(ParentWin, FPluginWindow, PAnsiChar(CeUtf8ToSys(FileToLoad)), ShowFlags)
  else
    Result := LISTPLUGIN_ERROR;
end;

procedure TWlxModule.CallListCloseWindow;
begin
  //  DCDebug('Try to call ListCloseWindow');
  try
{$IF DEFINED(LCLWIN32)}
    SetWindowLongPtr(FPluginWindow, GWL_WNDPROC, RemovePropW(FPluginWindow, WindowProcAtom));
{$ENDIF}
    if Assigned(ListCloseWindow) then
      ListCloseWindow(FPluginWindow)
{$IF DEFINED(LCLWIN32)}
    else DestroyWindow(FPluginWindow)
{$ELSEIF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
    else gtk_widget_destroy(PGtkWidget(FPluginWindow));
{$ELSEIF DEFINED(LCLQT) or DEFINED(LCLQT5)}
    else QWidget_Destroy(QWidgetH(FPluginWindow));
{$ENDIF}
  finally
    FPluginWindow := 0;
  end;
  //  DCDebug('Call ListCloseWindow success');
end;

function TWlxModule.CallListGetDetectString: String;
const
  MAX_LEN = 2048; // See listplugin.hlp for details
begin
  if not Assigned(ListGetDetectString) then
    Result := EmptyStr
  else begin
    Result := StringOfChar(#0, MAX_LEN);
    ListGetDetectString(PAnsiChar(Result), MAX_LEN);
    Result := PAnsiChar(Result);
  end;
end;

function TWlxModule.CallListSearchText(SearchString: String; SearchParameter: Integer): Integer;
begin
  if Assigned(ListSearchTextW) then
    Result := ListSearchTextW(FPluginWindow, PWideChar(UTF8Decode(SearchString)), SearchParameter)
  else if Assigned(ListSearchText) then
    Result := ListSearchText(FPluginWindow, PAnsiChar(CeUtf8ToSys(SearchString)), SearchParameter)
  else
    Result := LISTPLUGIN_ERROR;
end;

function TWlxModule.CallListSearchDialog(FindNext: Integer): Integer;
begin
  if Assigned(ListSearchDialog) then
  begin
    Result := ListSearchDialog(FPluginWindow, FindNext);
  end
  else
    Result := LISTPLUGIN_ERROR;
end;

function TWlxModule.CallListSendCommand(Command, Parameter: Integer): Integer;
begin
  if Assigned(ListSendCommand) then
  begin
    Result := ListSendCommand(FPluginWindow, Command, Parameter);
  end
  else
    Result := LISTPLUGIN_ERROR;
end;

function TWlxModule.FileParamVSDetectStr(AFileName: String; bForce: Boolean): Boolean;
begin
  if not Enabled then Exit(False);
  FParser.IsForce:= bForce;
  FParser.DetectStr := Self.DetectStr;
  DCDebug('DetectStr = ' + FParser.DetectStr);
  DCDebug('AFileName = ' + AFileName);
  Result := FParser.TestFileResult(AFileName);
end;

procedure TWlxModule.SetFocus;
begin
  {$IF DEFINED(LCLWIN32)}
  Windows.SetFocus(FPluginWindow);
  {$ELSEIF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  QWidget_setFocus(QWidgetH(FPluginWindow));
  {$ELSEIF DEFINED(LCLGTK2)}
  gtk_widget_grab_focus(PGtkWidget(FPluginWindow));
  {$ENDIF}
end;

procedure TWlxModule.ResizeWindow(aRect: TRect);
begin
  //ToDo: Implement for other widgetsets
  with aRect do
  begin
    {$IF DEFINED(LCLWIN32)}
    MoveWindow(FPluginWindow, Left, Top, Right - Left, Bottom - Top, True);
    {$ELSEIF DEFINED(LCLQT) or DEFINED(LCLQT5)}
    QWidget_move(QWidgetH(FPluginWindow), Left, Top);
    QWidget_resize(QWidgetH(FPluginWindow), Right - Left, Bottom - Top);
    {$ELSEIF DEFINED(LCLGTK2)}
    gtk_widget_set_uposition(PGtkWidget(FPluginWindow), Left, -1);
    gtk_widget_set_usize(PGtkWidget(FPluginWindow), Right - Left, Bottom - Top);
    {$ENDIF}
  end;
end;

function TWlxModule.CallListPrint(FileToPrint, DefPrinter: String;
  PrintFlags: Integer; var Margins: trect): Integer;
begin
  if Assigned(ListPrintW) then
    Result := ListPrintW(FPluginWindow, PWideChar(UTF8Decode(FileToPrint)),
      PWideChar(UTF8Decode(DefPrinter)), PrintFlags, Margins)
  else if Assigned(ListPrint) then
    Result := ListPrint(FPluginWindow, PAnsiChar(CeUtf8ToSys(FileToPrint)), PAnsiChar(CeUtf8ToSys(DefPrinter)),
      PrintFlags, Margins)
  else
    Result := LISTPLUGIN_ERROR;
end;

function TWlxModule.CallListNotificationReceived(Msg, wParam, lParam: Integer): Integer;
begin
  if Assigned(ListNotificationReceived) then
  begin
    Result := ListNotificationReceived(FPluginWindow, Msg, wParam, lParam);
  end;
end;

procedure TWlxModule.CallListSetDefaultParams;
var
  dps: TListDefaultParamStruct;
begin
  if Assigned(ListSetDefaultParams) then
  begin
    dps.DefaultIniName := mbFileNameToSysEnc(gpCfgDir + WlxIniFileName);
    dps.PluginInterfaceVersionHi := 2;
    dps.PluginInterfaceVersionLow := 0;
    dps.Size := SizeOf(TListDefaultParamStruct);
    ListSetDefaultParams(@dps);
  end;
end;

function TWlxModule.CallListGetPreviewBitmap(FileToLoad: String; Width, Height: Integer; ContentBuf: String): HBITMAP;
begin
  if Assigned(ListGetPreviewBitmapW) then
    Result := ListGetPreviewBitmapW(PWideChar(UTF8Decode(FileToLoad)), Width, Height, PByte(ContentBuf), Length(ContentBuf))
  else if Assigned(ListGetPreviewBitmap) then
    Result := ListGetPreviewBitmap(PAnsiChar(CeUtf8ToSys(FileToLoad)), Width, Height, PByte(ContentBuf), Length(ContentBuf))
  else
    Result := 0;
end;

{ TWLXModuleList }

function TWLXModuleList.GetCount: Integer;
begin
  if Assigned(Flist) then
    Result := Flist.Count
  else
    Result := 0;
end;

constructor TWLXModuleList.Create;
begin
  Flist := TStringList.Create;
end;

destructor TWLXModuleList.Destroy;
begin
  Clear;
  FreeAndNil(Flist);

  inherited Destroy;
end;

procedure TWLXModuleList.Clear;
begin
  while Flist.Count > 0 do
  begin
    TWlxModule(Flist.Objects[0]).Free;
    Flist.Delete(0);
  end;
end;

procedure TWLXModuleList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TWLXModuleList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TWLXModuleList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  AName, APath: String;
  AWlxModule: TWlxModule;
begin
  Clear;

  ANode := ANode.FindNode('WlxPlugins');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('WlxPlugin') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', AName) and
           AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          AWlxModule := TWlxModule.Create;
          Flist.AddObject(UpCase(AName), AWlxModule);
          AWlxModule.Name := AName;
          AWlxModule.FileName := APath;
          AWlxModule.DetectStr := AConfig.GetValue(ANode, 'DetectString', '');
          AWlxModule.Enabled:= AConfig.GetAttr(ANode, 'Enabled', True);
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWLXModuleList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  i: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'WlxPlugins', True);
  AConfig.ClearNode(ANode);

  for i := 0 to Flist.Count - 1 do
  begin
    SubNode := AConfig.AddNode(ANode, 'WlxPlugin');
    AConfig.SetAttr(SubNode, 'Enabled', TWlxModule(Flist.Objects[I]).Enabled);
    AConfig.AddValue(SubNode, 'Name', TWlxModule(Flist.Objects[I]).Name);
    AConfig.AddValue(SubNode, 'Path', TWlxModule(Flist.Objects[I]).FileName);
    AConfig.AddValue(SubNode, 'DetectString', TWlxModule(Flist.Objects[I]).DetectStr);
  end;
end;

{ TWLXModuleList.ComputeSignature }
function TWLXModuleList.ComputeSignature(seed: dword): dword;
var
  iIndex: integer;
begin
  result := seed;
  for iIndex := 0 to pred(Count) do
  begin
    result := ComputeSignatureBoolean(result, TWlxModule(Flist.Objects[iIndex]).Enabled);
    result := ComputeSignatureString(result, TWlxModule(Flist.Objects[iIndex]).Name);
    result := ComputeSignatureString(result, TWlxModule(Flist.Objects[iIndex]).FileName);
    result := ComputeSignatureString(result, TWlxModule(Flist.Objects[iIndex]).DetectStr);
  end;
end;

procedure TWLXModuleList.DeleteItem(Index: Integer);
begin
  if (Index > -1) and (Index < Flist.Count) then
  begin
    TWlxModule(Flist.Objects[Index]).Free;
    Flist.Delete(Index);
  end;
end;

function TWLXModuleList.Add(Item: TWlxModule): Integer;
begin
  Result := Flist.AddObject(UpCase(item.Name), Item);
end;

function TWLXModuleList.Add(FileName: String): Integer;
var
  s: String;
begin
  //    DCDebug('WLXLIST Add entered');
  s := ExtractFileName(FileName);
  if pos('.', s) > 0 then
    Delete(s, pos('.', s), length(s));
  Result := Flist.AddObject(UpCase(s), TWlxModule.Create);
  TWlxModule(Flist.Objects[Result]).Name := s;
  TWlxModule(Flist.Objects[Result]).FileName := FileName;
  if TWlxModule(Flist.Objects[Result]).LoadModule then
  begin
    TWlxModule(Flist.Objects[Result]).DetectStr :=
    TWlxModule(Flist.Objects[Result]).CallListGetDetectString;
    TWlxModule(Flist.Objects[Result]).UnloadModule;
  end;
  //    DCDebug('WLXLIST ADD Leaved');
end;

function TWLXModuleList.Add(AName, FileName, DetectStr: String): Integer;
begin
  Result := Flist.AddObject(UpCase(AName), TWlxModule.Create);
  TWlxModule(Flist.Objects[Result]).Name := AName;
  TWlxModule(Flist.Objects[Result]).DetectStr := DetectStr;
  TWlxModule(Flist.Objects[Result]).FileName := FileName;
end;

procedure TWLXModuleList.Assign(OtherList: TWLXModuleList);
var
  I, J: Integer;
begin
  Clear;
  for I := 0 to OtherList.Flist.Count - 1 do
  begin
    with TWlxModule(OtherList.Flist.Objects[I]) do
    begin
      J:= Add(Name, FileName, DetectStr);
      GetWlxModule(J).Enabled:= Enabled;
    end;
  end;
end;

function TWLXModuleList.IndexOfName(const AName: string): Integer;
begin
  Result := Flist.IndexOf(UpCase(AName));
end;

function TWLXModuleList.IsLoaded(AName: String): Boolean;
var
  x: Integer;
begin
  x := Flist.IndexOf(AName);
  if x = -1 then
    Result := False
  else
  begin
    Result := GetWlxModule(x).IsLoaded;
  end;
end;

function TWLXModuleList.IsLoaded(Index: Integer): Boolean;
begin
  Result := GetWlxModule(Index).IsLoaded;
end;

function TWLXModuleList.LoadModule(AName: String): Boolean;
var
  x: Integer;
begin
  x := Flist.IndexOf(UpCase(AName));
  if x = -1 then
    Result := False
  else
  begin
    Result := GetWlxModule(x).LoadModule;
  end;
end;

function TWLXModuleList.LoadModule(Index: Integer): Boolean;
begin
  Result := GetWlxModule(Index).LoadModule;
end;

function TWLXModuleList.GetWlxModule(Index: Integer): TWlxModule;
begin
  Result := TWlxModule(Flist.Objects[Index]);
end;

function TWLXModuleList.GetWlxModule(AName: String): TWlxModule;
var
  tmp: Integer;
begin
  tmp := Flist.IndexOf(upcase(AName));
  if tmp > -1 then
    Result := TWlxModule(Flist.Objects[tmp]);
end;

{$IF DEFINED(LCLWIN32)}{$WARNINGS OFF}
initialization
  WindowProcAtom := Pointer(GlobalAddAtomW('Double Commander'));
finalization
  Windows.GlobalDeleteAtom(ATOM(WindowProcAtom));
{$ENDIF}

end.

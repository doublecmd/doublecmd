{
   Double Commander
   -------------------------------------------------------------------------
   WLX-API implementation (TC WLX-API v2.0).

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

   contributors:

   Copyright (C) 2009-2013 Alexander Koblov (alexx2000@mail.ru)

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
  , gtk, glib, gdk
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , gtk2, glib2
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4, qtwidgets
  // The Qt widgetset must be used to load plugins on qt
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
    FForce: Boolean;
    FParser: TParserControl;
    FPluginWindow: HWND;
    function GetCanPrint: Boolean;
    function GIsLoaded: Boolean;
  public
    Name: String;
    FileName: String;
    DetectStr: String;
    pShowFlags: Integer;
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
    function CallListGetPreviewBitmap(FileToLoad: String; Width, Height: Integer; contentbuf: String): hbitmap;
    function CallListNotificationReceived(Msg, wParam, lParam: Integer): Integer;
    function CallListPrint(FileToPrint, DefPrinter: String; PrintFlags: Integer; var Margins: trect): Integer;
    function CallListSearchDialog(FindNext: Integer): Integer;
    function CallListSearchText(SearchString: String; SearchParameter: Integer): Integer;
    function CallListSendCommand(Command, Parameter: Integer): Integer;
    //---------------------
    function FileParamVSDetectStr(AFileName: String): Boolean;
    //---------------------
    procedure ResizeWindow(aRect: TRect);
    //---------------------
    property IsLoaded: Boolean read GIsLoaded;
    property ModuleHandle: TLibHandle read FModuleHandle write FModuleHandle;
    property Force: Boolean read FForce write FForce;
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
    procedure Load(Ini: TIniFileEx); overload;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(Ini: TIniFileEx); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure DeleteItem(Index: Integer);
    //---------------------
    function Add(Item: TWlxModule): Integer; overload;
    function Add(FileName: String): Integer; overload;
    function Add(AName, FileName, DetectStr: String): Integer; overload;
    //---------------------
    procedure Assign(OtherList: TWLXModuleList);
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

function WlxPrepareContainer(Ahandle: HWND; revert: Boolean = False): Boolean;

implementation

uses
  FileUtil, uDebug, DCOSUtils, uOSUtils, uGlobsPaths, uGlobs;

const
  WlxIniFileName = 'wlx.ini';

function WlxPrepareContainer(Ahandle: HWND; revert: Boolean = False): Boolean;
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
var
  lst: PGList;
{$ENDIF}
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if not revert then
  begin
    //Hide controls from our gtk container
    lst := gtk_container_children(GTK_CONTAINER(PGtkwidget(AHandle)));
    if lst <> nil then
    begin
      gtk_widget_hide(PGtkWidget(lst^.Data));
      Result := True;
    end
    else
      Result := False;
    Exit;
  end
  else
  begin
    //Show controls from our gtk container
    lst := gtk_container_children(GTK_CONTAINER(PGtkwidget(AHandle)));
    if lst <> nil then
    begin
      gtk_widget_show(PGtkWidget(lst^.Data));
      Result := True;
    end
    else
      Result := False;
    Exit;
  end;
{$ENDIF}
  Result := True;

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

constructor TWlxModule.Create;
begin
  FParser := TParserControl.Create;
end;

destructor TWlxModule.Destroy;
begin
  if GIsLoaded then
    UnloadModule;
  if Assigned(FParser) then
    FParser.Free;

  inherited Destroy;
end;

function TWlxModule.LoadModule: Boolean;
begin
  // DCDebug('WLXM LoadModule entered');
  FModuleHandle := mbLoadLibrary(Self.FileName);
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
  // DCDebug('WLXM LoadModule Leaved');
end;

procedure TWlxModule.UnloadModule;
begin
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
end;

function TWlxModule.CallListLoad(ParentWin: HWND; FileToLoad: String; ShowFlags: Integer): HWND;
begin
  {$IFDEF LCLQT}
  ParentWin := HWND(TQtWidget(ParentWin).GetContainerWidget);
  {$ENDIF}

  if Assigned(ListLoadW) then
    FPluginWindow := ListLoadW(ParentWin, PWideChar(UTF8Decode(FileToLoad)), ShowFlags)
  else if Assigned(ListLoad) then
    FPluginWindow := ListLoad(ParentWin, PAnsiChar(UTF8ToSys(FileToLoad)), ShowFlags)
  else
    Exit(wlxInvalidHandle);

  Result := FPluginWindow;
end;

function TWlxModule.CallListLoadNext(ParentWin: HWND; FileToLoad: String; ShowFlags: Integer): Integer;
begin
  {$IFDEF LCLQT}
  ParentWin := HWND(TQtWidget(ParentWin).GetContainerWidget);
  {$ENDIF}

  if Assigned(ListLoadNextW) then
    Result := ListLoadNextW(ParentWin, FPluginWindow, PWideChar(UTF8Decode(FileToLoad)), ShowFlags)
  else if Assigned(ListLoadNext) then
    Result := ListLoadNext(ParentWin, FPluginWindow, PAnsiChar(UTF8ToSys(FileToLoad)), ShowFlags)
  else
    Result := LISTPLUGIN_ERROR;
end;

procedure TWlxModule.CallListCloseWindow;
begin
  //  DCDebug('Try to call ListCloseWindow');
  try
    if Assigned(ListCloseWindow) then
      ListCloseWindow(FPluginWindow)
{$IF DEFINED(LCLWIN32)}
    else
      DestroyWindow(FPluginWindow)
{$ENDIF}
  finally
    FPluginWindow := 0;
  end;
  //  DCDebug('Call ListCloseWindow success');
end;

function TWlxModule.CallListGetDetectString: String;
begin
  //  DCDebug('GetDetectstr Entered');
  if Assigned(ListGetDetectString) then
  begin
    SetLength(Result, MAX_PATH); Result[1] := #0;
    ListGetDetectString(PAnsiChar(Result), MAX_PATH);
    Result := PAnsiChar(Result);
  end
  else
    Result := EmptyStr;
  //  DCDebug('GetDetectStr Leaved');
end;

function TWlxModule.CallListSearchText(SearchString: String; SearchParameter: Integer): Integer;
begin
  if Assigned(ListSearchTextW) then
    Result := ListSearchTextW(FPluginWindow, PWideChar(UTF8Decode(SearchString)), SearchParameter)
  else if Assigned(ListSearchText) then
    Result := ListSearchText(FPluginWindow, PAnsiChar(UTF8ToSys(SearchString)), SearchParameter)
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

function TWlxModule.FileParamVSDetectStr(AFileName: String): Boolean;
begin
  FParser.DetectStr := Self.DetectStr;
  DCDebug('DetectStr = ' + FParser.DetectStr);
  DCDebug('AFileName = ' + AFileName);
  Result := FParser.TestFileResult(AFileName);
end;

procedure TWlxModule.ResizeWindow(aRect: TRect);
begin
  //ToDo: Implement for other widgetsets
  with aRect do
  begin
    {$IF DEFINED(LCLWIN32)}
    MoveWindow(FPluginWindow, Left, Top, Right - Left, Bottom - Top, True);
    {$ELSEIF DEFINED(LCLQT)}
    QWidget_move(QWidgetH(FPluginWindow), Left, Top);
    QWidget_resize(QWidgetH(FPluginWindow), Right - Left, Bottom - Top);
    {$ELSEIF DEFINED(LCLGTK2)}
    gtk_widget_set_uposition(PGtkWidget(FPluginWindow), Left, -1);
    gtk_widget_set_usize(PGtkWidget(FPluginWindow), Right - Left, Bottom - Top);
    {$ENDIF}
  end;
end;

function TWlxModule.CallListPrint(FileToPrint, DefPrinter: String; PrintFlags: Integer; var Margins: trect): Integer;
begin
  if Assigned(ListPrintW) then
    Result := ListPrintW(FPluginWindow, PWideChar(UTF8Decode(FileToPrint)),
      PWideChar(UTF8Decode(DefPrinter)), PrintFlags, Margins)
  else if Assigned(ListPrint) then
    Result := ListPrint(FPluginWindow, PAnsiChar(UTF8ToSys(FileToPrint)), PAnsiChar(UTF8ToSys(DefPrinter)),
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

function TWlxModule.CallListGetPreviewBitmap(FileToLoad: String; Width, Height: Integer; contentbuf: String): hbitmap;
begin
  if Assigned(ListGetPreviewBitmapW) then
    Result := ListGetPreviewBitmapW(PWideChar(UTF8Decode(FileToLoad)), Width, Height,
      PChar(contentbuf), length(contentbuf))
  else if Assigned(ListGetPreviewBitmap) then
    Result := ListGetPreviewBitmap(PAnsiChar(UTF8ToSys(FileToLoad)), Width, Height,
      PChar(contentbuf), length(contentbuf));
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

procedure TWLXModuleList.Load(Ini: TIniFileEx);
var
  xCount, I: Integer;
  tmp: String;
begin
  Self.Clear;
  xCount := Ini.ReadInteger('Lister Plugins', 'PluginCount', 0);
  if xCount = 0 then
    Exit;

  for i := 0 to xCount - 1 do
  begin
    tmp := Ini.ReadString('Lister Plugins', 'Plugin' + IntToStr(I + 1) + 'Name', '');
    Flist.AddObject(UpCase(tmp), TWlxModule.Create);
    TWlxModule(Flist.Objects[I]).Name := tmp;
    TWlxModule(Flist.Objects[I]).DetectStr :=
      Ini.ReadString('Lister Plugins', 'Plugin' + IntToStr(I + 1) + 'Detect', '');
    TWlxModule(Flist.Objects[I]).FileName :=
      GetCmdDirFromEnvVar(Ini.ReadString('Lister Plugins', 'Plugin' + IntToStr(I + 1) + 'Path', ''));
  end;
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
          AWlxModule.FileName := GetCmdDirFromEnvVar(APath);
          AWlxModule.DetectStr := AConfig.GetValue(ANode, 'DetectString', '');
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWLXModuleList.Save(Ini: TIniFileEx);
var
  i: Integer;
begin
  Ini.EraseSection('Lister Plugins');
  Ini.WriteInteger('Lister Plugins', 'PluginCount', Flist.Count);
  for i := 0 to Flist.Count - 1 do
  begin
    Ini.WriteString('Lister Plugins', 'Plugin' + IntToStr(I + 1) + 'Name',
      TWlxModule(Flist.Objects[I]).Name);
    Ini.WriteString('Lister Plugins', 'Plugin' + IntToStr(I + 1) + 'Detect',
      TWlxModule(Flist.Objects[I]).DetectStr);
    Ini.WriteString('Lister Plugins', 'Plugin' + IntToStr(I + 1) + 'Path',
      SetCmdDirAsEnvVar(TWlxModule(Flist.Objects[I]).FileName));
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
    AConfig.AddValue(SubNode, 'Name', TWlxModule(Flist.Objects[I]).Name);
    AConfig.AddValue(SubNode, 'Path', SetCmdDirAsEnvVar(TWlxModule(Flist.Objects[I]).FileName));
    AConfig.AddValue(SubNode, 'DetectString', TWlxModule(Flist.Objects[I]).DetectStr);
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
  i: Integer;
begin
  Clear;
  for i := 0 to OtherList.Flist.Count - 1 do
  begin
    with TWlxModule(OtherList.Flist.Objects[I]) do
      Add(Name, FileName, DetectStr);
  end;
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

function TWLXModuleList.GetWLxModule(Index: Integer): TWlxModule;
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

end.

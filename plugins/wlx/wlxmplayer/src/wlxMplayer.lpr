{
   WlxMplayer
   -------------------------------------------------------------------------
   This is WLX (Lister) plugin for Double Commander.

   Copyright (C) 2008 Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008-2026 Alexander Koblov (alexx2000@mail.ru)

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

library wlxMplayer;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  {$IFNDEF HEAPTRC}
  cmem,
  {$ENDIF}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  x,
  {$IFDEF LCLGTK2} gtk2, gdk2, glib2, gdk2x, {$ENDIF}
  {$IFDEF LCLGTK3} LazGtk3, LazGdk3, LazGlib2, LazGObject2, {$ENDIF}
  {$IFDEF LCLQT5} qt5, {$ENDIF}
  {$IFDEF LCLQT6} qt6, {$ENDIF}
  Process,
  Math,
  WlxPlugin,
  DCOSUtils;

{$R *.res}

{$IF DEFINED(LCLGTK3)}
function gdk_x11_window_get_type: TGType; cdecl; external LazGdk3_library;
function gdk_x11_window_get_xid(window: PGdkWindow): TWindow; cdecl; external LazGdk3_library;
{$ENDIF}

function CheckParent(ParentWin: HWND): Boolean;
{$IF DEFINED(LCLQT5) OR DEFINED(LCLQT6)}
var
  APlatform: WideString;
{$ENDIF}
begin
{$IF DEFINED(LCLGTK2)}
  if not GTK_IS_WIDGET(Pointer(ParentWin)) then
  begin
    WriteLn('wlxMplayer: invalid application widgetset');
    Exit(False);
  end;
  // Property 'expand' since GTK 3.0
  if (g_object_class_find_property(G_OBJECT_GET_CLASS(Pointer(ParentWin)), 'expand') <> nil) then
  begin
    WriteLn('wlxMplayer: invalid GTK version');
    Exit(False);
  end;
{$ELSEIF DEFINED(LCLGTK3)}
  if not g_type_check_instance_is_a(PGTypeInstance(ParentWin), gtk_widget_get_type) then
  begin
    WriteLn('wlxMplayer: invalid application widgetset');
    Exit(False);
  end;
  // Property 'expand' since GTK 3.0
  if (g_object_class_find_property(PGObjectClass(PGTypeInstance(ParentWin)^.g_class), 'expand') = nil) then
  begin
    WriteLn('wlxMplayer: invalid GTK version');
    Exit(False);
  end;
  // Plugin is X11 only
  gtk_widget_realize(PGtkWidget(ParentWin));
  if not g_type_check_instance_is_a(PGTypeInstance(gtk_widget_get_window(PGtkWidget(ParentWin))), gdk_x11_window_get_type) then
  begin
    WriteLn('wlxMplayer: invalid window system');
    Exit(False);
  end;
{$ELSEIF DEFINED(LCLQT5) OR DEFINED(LCLQT6)}
  QGuiApplication_platformName(@APlatform);
  // Plugin is X11 only
  if (APlatform <> 'xcb') then
  begin
    WriteLn('wlxMplayer: invalid window system - ', APlatform);
    Exit(False);
  end;
{$ENDIF}
  Result:= True;
end;

type

  { TMPlayer }

  TMPlayer = class(TThread)
  private
    Fxid: TWindow;       // X window handle
    FWidget: HWND;       // the integrable widget
    FFileName: String;   // filename
    FProcess: TProcess;  // mplayer's process
    FPlayerPath: String; // path to mplayer
  protected
    procedure Execute; override;
  public
    constructor Create(const APlayerPath, AFileName: String);
    destructor Destroy; override;
    procedure SetParentWidget(AWidget: HWND);
  end;

{ TMPlayer }

constructor TMPlayer.Create(const APlayerPath, AFileName: String);
begin
  inherited Create(True);
  FFileName:= AFilename;
  FPlayerPath:= APlayerPath;
  WriteLn('wlxMplayer: ', FPlayerPath);
end;

destructor TMPlayer.Destroy;
begin
  if FProcess.Running then
  begin
    FProcess.Terminate(0);
  end;
  FProcess.Free;
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5) or DEFINED(LCLQT6)}
  QWidget_Destroy(QWidgetH(FWidget));
{$ELSE}
  gtk_widget_destroy(PGtkWidget(FWidget));
{$ENDIF}
  inherited Destroy;
end;

procedure TMPlayer.SetParentWidget(AWidget: HWND);
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5) or DEFINED(LCLQT6)}
begin
  FWidget:= HWND(QWidget_create(QWidgetH(AWidget)));
  QWidget_show(QWidgetH(FWidget));
  Fxid:= QWidget_winId(QWidgetH(FWidget));
end;
{$ELSE}
var
  widget,
  mySocket: PGtkWidget;
begin
  widget:= PGtkWidget(AWidget);
  mySocket:= gtk_socket_new;

  gtk_container_add(PGtkContainer(widget), mySocket);

  gtk_widget_show(mySocket);
  gtk_widget_show(widget);

  gtk_widget_realize(mySocket);

{$IF DEFINED(LCLGTK2)}
  Fxid:= GDK_WINDOW_XID(mySocket^.window);
{$ELSEIF DEFINED(LCLGTK3)}
  Fxid:= gdk_x11_window_get_xid(mySocket^.window);
{$ENDIF}
  FWidget:= HWND(mySocket);
end;
{$ENDIF}

procedure TMPlayer.Execute;
begin
  FProcess:= TProcess.Create(nil);
  FProcess.Options:= FProcess.Options + [poWaitOnExit, poNoConsole];
  FProcess.Executable:= FPlayerPath;
  FProcess.Parameters.Add('-wid');
  FProcess.Parameters.Add(IntToStr(Fxid));
  FProcess.Parameters.Add(FFileName);
  FProcess.Execute;
end;

{ Plugin main part }

var
  List: TStringList;

function ListLoad(ParentWin: HWND; FileToLoad: PAnsiChar; ShowFlags: Integer): HWND; dcpcall;
var
  Player: TMPlayer;
  PlayerPath: String = 'mplayer';
begin
  if not CheckParent(ParentWin) then
    Exit(wlxInvalidHandle);

  if not FindInSystemPath(PlayerPath) then
    Exit(wlxInvalidHandle);

  Player:= TMPlayer.Create(PlayerPath, String(FileToLoad));
  Player.SetParentWidget(ParentWin);

  // Create list if none
  if not Assigned(List) then
  begin
    List:= TStringList.Create;
    List.OwnsObjects:= True;
  end;
  // Add to list new plugin window
  List.AddObject(IntToStr(Player.FWidget), Player);

  Result:= Player.FWidget;

  Player.Start;
end;

procedure ListCloseWindow(ListWin: HWND); dcpcall;
var
  S: String;
  Index: Integer;
begin
  if Assigned(List) then
  begin
    S:= IntToStr(ListWin);
    Index:= List.IndexOf(S);
    if Index > -1 then List.Delete(Index);
    // Free list if it has zero items
    if List.Count = 0 then FreeAndNil(List);
  end;
end;

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); dcpcall;
begin
  StrLCopy(DetectString, '(EXT="AVI")|(EXT="MKV")|(EXT="FLV")|(EXT="MPG")|(EXT="MPEG")|(EXT="MP4")|(EXT="VOB")', MaxLen);
end;

exports
  ListLoad,
  ListCloseWindow,
  ListGetDetectString;

end.

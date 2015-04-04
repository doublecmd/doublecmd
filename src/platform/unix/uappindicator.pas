{
    Double Commander
    -------------------------------------------------------------------------
    Application indicator support.

    Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

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

unit uAppIndicator;

{$mode delphi}

interface

uses
  ExtCtrls;

procedure RegisterAppIndicator(const ATrayIcon: TCustomTrayIcon);

implementation

uses
  DynLibs, WSLCLClasses, Glib2, Gtk2, Gtk2WSExtCtrls, DCOSUtils, uMyUnix;

type
  TAppIndicatorCategory = (
	APP_INDICATOR_CATEGORY_APPLICATION_STATUS,
	APP_INDICATOR_CATEGORY_COMMUNICATIONS,
	APP_INDICATOR_CATEGORY_SYSTEM_SERVICES,
	APP_INDICATOR_CATEGORY_HARDWARE,
	APP_INDICATOR_CATEGORY_OTHER
  );

  TAppIndicatorStatus = (
	APP_INDICATOR_STATUS_PASSIVE,
	APP_INDICATOR_STATUS_ACTIVE,
	APP_INDICATOR_STATUS_ATTENTION
  );

  PAppIndicator = ^TAppIndicator;
  TAppIndicator = record end;

type

  { TGtk2WSCustomTrayIconEx }

  TGtk2WSCustomTrayIconEx = class(TGtk2WSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
  end;

var
  PopupMenu: PGtkWidget;
  AppInd: PAppIndicator = nil;

var
  app_indicator_new: function(const id, icon_name: Pgchar; category: TAppIndicatorCategory): PAppIndicator; cdecl;
  app_indicator_set_status: procedure(self: PAppIndicator; status: TAppIndicatorStatus); cdecl;
  app_indicator_set_menu: procedure(self: PAppIndicator; menu: PGtkWidget); cdecl;

{ TGtk2WSCustomTrayIconEx }

class function TGtk2WSCustomTrayIconEx.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result:= inherited Hide(ATrayIcon);
  if Assigned(AppInd) then app_indicator_set_status(AppInd, APP_INDICATOR_STATUS_PASSIVE);
end;

class function TGtk2WSCustomTrayIconEx.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result:= inherited Show(ATrayIcon);
  if Assigned(AppInd) then app_indicator_set_status(AppInd, APP_INDICATOR_STATUS_ACTIVE);
end;

function Load: Boolean;
var
  libapp: TLibHandle;
begin
  libapp:= LoadLibrary('libappindicator.so.1');
  Result:= libapp <> NilHandle;
  if Result then
  try
    @app_indicator_new:= SafeGetProcAddress(libapp, 'app_indicator_new');
    @app_indicator_set_menu:= SafeGetProcAddress(libapp, 'app_indicator_set_menu');
    @app_indicator_set_status:= SafeGetProcAddress(libapp, 'app_indicator_set_status');
  except
    Result:= False;
    FreeLibrary(libapp);
  end;
end;

procedure RegisterAppIndicator(const ATrayIcon: TCustomTrayIcon);
var
  Index: Integer;
  MenuItem: PGtkWidget;
  WidgetSetClass: ^TWSLCLComponentClass;
begin
  if fpSystemStatus('pgrep unity > /dev/null 2>&1') = 0 then
  begin
    if not Load then Exit;

    // Replace tray icon widgetset class
    WidgetSetClass:= @ATrayIcon.WidgetSetClass;
    WidgetSetClass^:= TGtk2WSCustomTrayIconEx;

    // Create and fill popup menu
    PopupMenu:= gtk_menu_new();
    for Index:= 0 to ATrayIcon.PopupMenu.Items.Count - 1 do
    begin
      MenuItem:= PGtkWidget(ATrayIcon.PopupMenu.Items[Index].Handle);
      gtk_menu_shell_append(GTK_MENU_SHELL(PopupMenu), MenuItem);
    end;

    // Create application indicator
    AppInd:= app_indicator_new('doublecmd', 'doublecmd', APP_INDICATOR_CATEGORY_APPLICATION_STATUS);
    if Assigned(AppInd) then app_indicator_set_menu(AppInd, PopupMenu);
  end;
end;

end.

library SimpleWlx;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  Classes,
  SysUtils,
{$IF DEFINED(LCLGTK2)}
  gtk2, gdk2, glib2,
{$ELSEIF DEFINED(LCLGTK3)}
  LazGtk3,
{$ENDIF}
  WlxPlugin;

function ListLoad(ParentWin: HWND; FileToLoad: PAnsiChar; ShowFlags: Integer): HWND; dcpcall;
var
  GButton1, Gbutton2: PGtkWidget;
  GFix: PGtkWidget absolute Result;
  AParent: PGtkWidget absolute ParentWin;
begin
{$IFDEF LCLGTK2}
  // gFix:= gtk_fixed_new;
  gFix:= gtk_vbox_new(True, 5);
{$ELSE}
  gFix:= gtk_layout_new(nil, nil);
{$ENDIF}
  gtk_container_add(PGtkContainer(AParent), gFix);
  gtk_widget_show(gFix);

  GButton1:= gtk_button_new_with_label('Yehoo1');
  gtk_container_add(PGtkContainer(GFix), GButton1);
{$IFDEF LCLGTK2}
  gtk_widget_set_usize(GButton1, 90, 48);
  // gtk_widget_set_uposition(GButton1, 30, 10);
{$ELSE}
  GButton1^.set_size_request(90, 48);
  PGtkLayout(GFix)^.move(GButton1, 30, 10);
{$ENDIF}
  gtk_widget_show(GButton1);

  Gbutton2:= gtk_button_new_with_label('Yehoo2');
  gtk_container_add(PGtkContainer(GFix), Gbutton2);
{$IFDEF LCLGTK2}
  gtk_widget_set_usize(GButton2, 90, 64);
  // gtk_widget_set_uposition(GButton2, 50, 74);
{$ELSE}
  GButton2^.set_size_request(90, 64);
  PGtkLayout(GFix)^.move(GButton2, 50, 74);
{$ENDIF}
  gtk_widget_show(Gbutton2);
end;

procedure ListCloseWindow(ListWin: HWND); dcpcall;
var
  AWidget: PGtkWidget absolute ListWin;
begin
  gtk_widget_destroy(AWidget);
end;

exports
  ListLoad,
  ListCloseWindow;

end.


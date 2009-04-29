library SimpleWlx;

{$mode objfpc}{$H+}

{$DEFINE GTK2}

uses
  Classes,
  sysutils,
  {$IFDEF GTK}
    gtk,gdk,glib,
  {$ENDIF}
  {$IFDEF GTK2}
    gtk2,gdk2,glib2,
  {$ENDIF}
  WLXPlugin;
  
 var List:TStringList;

 //Custom class contains info for plugin windows
 type

 { TPlugInfo }

 TPlugInfo = class
        private
         fControls:TStringList;
        public
        fFileToLoad:string;
        fShowFlags:integer;
        //etc
        constructor Create;
        destructor Destroy; override;
        function AddControl(AItem:PGtkWidget):integer;
      end;

 { TPlugInfo }

 constructor TPlugInfo.Create;
 begin
  fControls:=TStringlist.Create;
 end;

destructor TPlugInfo.Destroy;
begin
  while fControls.Count>0 do
  begin
    gtk_widget_destroy(PGtkWidget(fControls.Objects[0]));
    fControls.Delete(0);
  end;
  inherited Destroy;
end;

function TPlugInfo.AddControl(AItem: PGtkWidget): integer;
begin
  fControls.AddObject(inttostr(Integer(AItem)),TObject(AItem));
end;


function ListLoad(ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle; stdcall;
var GFix,GButton1,Gbutton2:PGtkWidget;

   lst:PGlist;
begin
  lst:=gtk_container_children(GTK_CONTAINER(PGtkwidget(AWidget)));
  if lst=nil then exit;
//     gFix:=gtk_fixed_new;
     gFix:=gtk_vbox_new(true,5);
     gtk_container_add(GTK_CONTAINER(PGtkWidget(ParentWin)),gFix);
     gtk_widget_show(gFix);

     GButton1:=gtk_button_new_with_label('Yehoo1');
     gtk_container_add(GTK_CONTAINER(GFix),GButton1);
     gtk_widget_set_usize(GButton1,10,10);
//     gtk_widget_set_uposition(GButton1,30,10);
     gtk_widget_show(GButton1);

     Gbutton2:=gtk_button_new_with_label('Yehoo2');
     gtk_container_add(GTK_CONTAINER(GFix),Gbutton2 );
     gtk_widget_set_usize(GButton2,20,20);
    // gtk_widget_set_uposition(GButton2,50,50);
     gtk_widget_show(Gbutton2);
     
   //Create list if none
   if not assigned(List) then
       List:=TStringList.Create;

   //add to list new plugin window and it's info
   List.AddObject(IntToStr(integer(GFix)),TPlugInfo.Create);
   with TPlugInfo(List.Objects[List.Count-1]) do
     begin
       fFileToLoad:=FileToLoad;
       fShowFlags:=ShowFlags;
       AddControl(GFix);
     end;

Result:=integer(GFix);
end;

procedure ListCloseWindow(ListWin:thandle); stdcall;
 var Index:integer; s:string;
begin



 if assigned(List) then
   begin
     writeln('ListCloseWindow quit, List Item count: '+inttostr(List.Count));
     s:=IntToStr(ListWin);
     Index:=List.IndexOf(s);
     if Index>-1 then
       begin
         TPlugInfo(List.Objects[index]).Free;
         List.Delete(Index);
         writeln('List item n: '+inttostr(Index)+' Deleted');
       end;

     //Free list if it has zero items
     If List.Count=0 then  List.Free;
   end;

end;

exports
       ListLoad,
       ListCloseWindow;

begin
end.


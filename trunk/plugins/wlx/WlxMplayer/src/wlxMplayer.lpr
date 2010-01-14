{
   WlxMplayer
   -------------------------------------------------------------------------
   This is WLX (Lister) plugin for Double Commander.

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Class TExProcess used in plugin was written by Anton Rjeshevsky.
   Gtk2 and Qt support were added by Koblov Alexander (Alexx2000@mail.ru)

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

library wlxMplayer;

{$mode objfpc}{$H+}

{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2) or DEFINED(LCLQT))}
{$DEFINE LCLGTK2}
{$ENDIF}

uses
   {$IFDEF UNIX}
   cthreads,
   {$ENDIF}
  Classes,
  sysutils,
  x,
  {$IFDEF LCLGTK} gtk, gdk, glib, {$ENDIF}
  {$IFDEF LCLGTK2} gtk2, gdk2, glib2, gdk2x, {$ENDIF}
  {$IFDEF LCLQT} qt4, {$ENDIF}
  process,
  math,
  WLXPlugin;
  


type
{ TExProcess }

   TExProcess = class
  protected
    p: TProcess;
    s: string;
    function _GetExitStatus(): integer;
  public
    RezList:TStringList;
    constructor Create(commandline: string);
    procedure Execute;
    destructor Destroy;
    procedure OnReadLn(str: string);
    property ExitStatus: integer read _GetExitStatus;
  end;

const buf_len = 3000;


{ TExProcess }

function TExProcess._GetExitStatus(): integer;
begin
  Result:=p.ExitStatus;
end;

constructor TExProcess.Create(commandline: string);
begin
  RezList:=TStringList.Create;
  s:='';
  p:=TProcess.Create(nil);
  p.CommandLine:=commandline;
   p.Options:=[poUsePipes,poNoConsole];
end;

procedure TExProcess.Execute;
var
  buf: string;
  i, j, c, n: integer;
begin
  p.Execute;
  repeat
    SetLength(buf, buf_len);
    SetLength(buf, p.output.Read(buf[1], length(buf))); //waits for the process output
     // cut the incoming stream to lines:
    s:=s + buf; //add to the accumulator
    repeat //detect the line breaks and cut.
      i:=Pos(#13, s);
      j:=Pos(#10, s);
      if i=0 then i:=j;
      if j=0 then j:=i;
      if j = 0 then Break; //there are no complete lines yet.
      OnReadLn(Copy(s, 1, min(i, j) - 1)); //return the line without the CR/LF characters
      s:=Copy(s, max(i, j) + 1, length(s) - max(i, j)); //remove the line from accumulator
    until false;
  until buf = '';
  if s <> '' then OnReadLn(s);
end;

destructor TExProcess.Destroy;
begin
  RezList.Free;
  p.Free;
end;

procedure TExProcess.OnReadLn(str: string);
begin
 RezList.Add(str);
end;


type

//Class implementing mplayer control
{ TMPlayer }

TMPlayer=class(TThread)
        public
          //---------------------
          hWidget:THandle;	//the integrable widget
          fileName:string;        //filename
          xid:TWindow;		//X window handle
          pr:TProcess;            //mplayer's process
          pmplayer:string;        //path to mplayer
          //---------------------
          constructor Create(AFilename:String);
           destructor destroy; override;
          procedure SetParentWidget(AWidget:thandle);
        protected
          procedure Execute; override;
        private

     end;

{ TMPlayer }

constructor TMPlayer.Create(AFilename:String);
var pf:TExProcess;
begin
  inherited Create(true);
  filename:='"'+AFilename+'"';
  pf:=TExProcess.Create('which mplayer');
  pf.Execute;
  pmplayer:=pf.RezList[0]+' ';
  pf.Free;
  writeln('PLUGIN : found mplayer in :' + pmplayer);
end;

destructor TMPlayer.destroy;
begin
  if pr.Running then
    pr.Terminate(0);
  pr.Free;
  inherited destroy;
end;

procedure TMPlayer.SetParentWidget(AWidget: THandle);
{$IFDEF LCLQT}
begin
  xid:= QWidget_winId(QWidgetH(AWidget));
  hWidget:= AWidget;
end;
{$ELSE}
var
   widget,
   mySocket:PGtkWidget;	//the socket
   lst:PGlist;
begin
  lst:=gtk_container_children(GTK_CONTAINER(PGtkwidget(AWidget)));
  if lst=nil then exit;
  widget := PGtkWidget(lst^.data);
  //gtk_vbox_new(FALSE,0);
   mySocket := gtk_socket_new();
  gtk_container_add (GTK_CONTAINER(widget), mySocket);

  gtk_widget_show(mySocket);
  gtk_widget_show(widget);

//*****
  //gtk_container_add (GTK_CONTAINER (PGtkWidget(Awidget)), widget);
  gtk_widget_realize(mySocket);
  //gtk_widget_hide(PGtkWidget(AWidget));
{$IFDEF LCLGTK}
  xid:=(PGdkWindowPrivate(widget^.window))^.xwindow;
{$ENDIF}
{$IFDEF LCLGTK2}
  xid:=GDK_WINDOW_XID(widget^.window);
{$ENDIF}
  hWidget:= THandle(widget);
end;
{$ENDIF}


procedure TMPlayer.Execute;
begin
   pr:=TProcess.Create(nil);
   pr.Options := Pr.Options + [poWaitOnExit,poNoConsole{,poUsePipes}]; //mplayer stops if poUsePipes used.
   pr.CommandLine:=pmplayer+fileName+' -wid '+IntToStr(xid);
   pr.Execute;
end;



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
        function AddControl(AItem: TMPlayer):integer;
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
    TMPlayer(fControls.Objects[0]).Free;
    fControls.Delete(0);
  end;
  inherited Destroy;
end;

function TPlugInfo.AddControl(AItem: TMPlayer): integer;
begin
  fControls.AddObject(inttostr(Integer(AItem)),TObject(AItem));
end;

{Plugin main part}

 var List:TStringList;

function ListLoad(ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle; stdcall;
var p:TMPlayer;
begin
   p:=TMPlayer.Create(string(FileToLoad));
   p.SetParentWidget(ParentWin);
   
   //Create list if none
   if not assigned(List) then
       List:=TStringList.Create;

   //add to list new plugin window and it's info
   List.AddObject(IntToStr(integer(p.hWidget)),TPlugInfo.Create);
   with TPlugInfo(List.Objects[List.Count-1]) do
     begin
       fFileToLoad:=FileToLoad;
       fShowFlags:=ShowFlags;
       AddControl(p);
     end;
     
Result:=integer(p.hWidget);

p.Resume;

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


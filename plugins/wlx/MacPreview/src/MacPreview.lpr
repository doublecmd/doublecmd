{
   Double commander
   -------------------------------------------------------------------------
   MacOS preview plugin

   Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2022 Rich Chang (rich2014.git@outlook.com)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

library MacPreview;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  SysUtils, WlxPlugin, CocoaAll, QuickLookUI;

type
  TQLPItem = objcclass(NSObject, QLPreviewItemProtocol)
    url: NSURL;
    function previewItemURL: NSURL;
  end;

function TQLPItem.previewItemURL: NSURL;
begin
  Result:= url;
end;

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

procedure setFilepath( view:QLPreviewView; filepath:String );
var
  item: TQLPItem;
begin
  if filepath=EmptyStr then begin
    item:= nil;
  end else begin
    item:= TQLPItem.alloc.init;
    item.url:= NSURL.fileURLWithPath( StringToNSString(filepath) );
  end;
  view.setPreviewItem( item );
end;

function ListLoad( ParentWin:THandle; FileToLoad:pchar; {%H-}ShowFlags{%H+}:integer):THandle; cdecl;
var
  view: QLPreviewView;
begin
  view:= QLPreviewView.alloc.init;
  view.setShouldCloseWithWindow( false );
  NSView(ParentWin).addSubview( view );
  setFilepath( view, FileToLoad );
  Result:= THandle(view);
end;

function ListLoadNext( {%H-}ParentWin{%H+},PluginWin:THandle; FileToLoad:pchar; {%H-}ShowFlags{%H+}:integer):integer; cdecl;
begin
  setFilepath( QLPreviewView(PluginWin), FileToLoad );
  Result:= LISTPLUGIN_OK;
end;

procedure ListCloseWindow(ListWin:THandle); cdecl;
begin
  QLPreviewView(ListWin).close;
  QLPreviewView(ListWin).removeFromSuperview;
end;

procedure ListGetDetectString(DetectString:pchar;maxlen:integer); cdecl;
begin
  StrLCopy(DetectString, '(EXT!="")', MaxLen);
end;

exports
  ListLoad,
  ListLoadNext,
  ListCloseWindow,
  ListGetDetectString;

end.


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
    function previewItemURL: NSURL;
  private
    url: NSURL;
  public
    ext: ShortString;
    procedure initPath( path: pchar); message 'initPath:';
  end;

// copy from uMyDarwin
function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

// copy from DCStrUtils
function ExtractOnlyFileExt(const FileName: string): string;
var
  I : LongInt;
  SOF : Boolean;
  EndSep : Set of Char;
begin
  Result := EmptyStr;
  I := Length(FileName);
  EndSep:= AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
  begin
    SOF:= (I = 1) or (FileName[I - 1] in AllowDirectorySeparators);
    if (not SOF) or FirstDotAtFileNameStartIsExtension then
      Result := Copy(FileName, I + 1, MaxInt)
  end;
end;

function TQLPItem.previewItemURL: NSURL;
begin
  Result:= url;
end;

procedure TQLPItem.initPath( path: pchar );
begin
  url:= NSURL.fileURLWithPath( StringToNSString(path) );
  ext:= UpperCase( ExtractOnlyFileExt(path) );
end;

procedure setFilepath( view:QLPreviewView; filepath:String );
var
  item: TQLPItem;
begin
  if filepath=EmptyStr then begin
    item:= nil;
  end else begin
    item:= TQLPItem.alloc.init;
    item.initPath( pchar(filepath) );
  end;
  view.setPreviewItem( item );
end;

function ListLoad( ParentWin:THandle; FileToLoad:pchar; {%H-}ShowFlags:integer):THandle; cdecl;
var
  view: QLPreviewView;
begin
  view:= QLPreviewView.alloc.init;
  view.setShouldCloseWithWindow( false );
  NSView(ParentWin).addSubview( view );
  setFilepath( view, FileToLoad );
  Result:= THandle( view );
end;

function isExtChanged( view: QLPreviewView; FileToLoad:pchar ): boolean;
var
  item: TQLPItem;
  newExt: ShortString;
begin
  item:= {%H-}TQLPItem( view.previewItem );
  newExt:= upperCase( ExtractOnlyFileExt( FileToLoad ) );
  Result:= item.ext<>newExt;
end;

function ListLoadNext( {%H-}ParentWin,PluginWin:THandle; FileToLoad:pchar; {%H-}ShowFlags:integer):integer; cdecl;
var
  view: QLPreviewView;
begin
  view:= QLPreviewView(PluginWin);

  // workaround for the bug of MacOS Quick Look:
  // when previewing different types of files continuously, occasionally exceptions occur.
  // such as previewing a large .pas file immediately after previewing a pdf file.
  // empty the original preview file first can solve such problems.
  if isExtChanged(view,FileToLoad) then setFilepath(view,EmptyStr);

  setFilepath( view, FileToLoad );
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


{
   Double commander
   -------------------------------------------------------------------------
   MacOS preview plugin

   Copyright (C) 2022-2024 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2022-2024 Rich Chang (rich2014.git@outlook.com)

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
  SysUtils, WlxPlugin, QuickLookUI, CFPropertyList,
  MacOSAll, CocoaAll, UniformTypeIdentifiers;

const
  NSAppKitVersionNumber11_0  = 2022;

type
  TQLPItem = objcclass(NSObject, QLPreviewItemProtocol)
    function previewItemURL: NSURL;
  private
    url: NSURL;
  public
    ext: ShortString;
    procedure initPath( path: pchar); message 'initPath:';
  end;

  DCQLPreview = objcclass(QLPreviewView)
    function acceptsFirstResponder: ObjCBOOL; override;
  end;

var
  QLContentTypes: NSMutableArray;
  QLContentUTTypes: NSMutableArray;
  DisabledExtensions: NSMutableSet;

const
  PluginKitPath = '/usr/bin/pluginkit';
  QLPreviewExtensionPoint = 'com.apple.quicklook.preview';

  // Types a Quick Look preview extension may declare that would make MacPreview
  // claim (nearly) every file, or plain text DC's own viewer handles better.
  ExtensionExcludeList: array[0..6] of PAnsiChar =
    (
      'public.item',
      'public.data',
      'public.content',
      'public.text',
      'public.plain-text',
      'public.json',
      'public.xml'
    );

  ExcludeList: array[0..2] of PAnsiChar =
    (
      'public.plain-text',
      'public.json',
      'public.xml'
    );

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

procedure AddContentType(AType: NSString);
var
  anObject: id;
begin
  QLContentTypes.addObject(AType);
  if NSAppKitVersionNumber >= NSAppKitVersionNumber11_0 then begin
    anObject:= UTType.typeWithIdentifier(AType);
    if Assigned(anObject) then QLContentUTTypes.addObject(anObject);
  end;
end;

function CheckContentType(const Name: String; FileType: NSString): Boolean;
var
  Index: Integer;
begin
  // Special case
  if (Name = 'Text.qlgenerator') then
  begin
    for Index:= 0 to High(ExcludeList) do
    begin
      if StrComp(FileType.UTF8String, ExcludeList[Index]) = 0 then
        Exit(False)
    end;
  end;
  Result:= True;
end;

procedure ParseFile(const Path, Name: String);
var
  Data: NSData;
  I, J: Integer;
  Dict: NSDictionary;
  FileType: NSString;
  DocumentTypes, ContentTypes: NSArray;
begin
  Data:= NSData.dataWithContentsOfFile(StringToNSString(Path + Name + '/Contents/Info.plist'));
  if Assigned(Data) then
  begin
    Dict:= NSDictionary(NSPropertyListSerialization.propertyListWithData_options_format_error(Data, NSPropertyListImmutable, nil, nil));
    if Assigned(Dict) then
    begin
      DocumentTypes:= NSArray(Dict.valueForKey(StringToNSString('CFBundleDocumentTypes')));
      if Assigned(DocumentTypes) then
      begin
        for I:= 0 to Integer(DocumentTypes.count) - 1 do
        begin
          Dict:= NSDictionary(DocumentTypes.objectAtIndex(I));
          ContentTypes:= NSArray(Dict.valueForKey(StringToNSString('LSItemContentTypes')));
          if Assigned(ContentTypes) then
          begin
            for J:= 0 to Integer(ContentTypes.count - 1) do
            begin
              FileType:= NSString(ContentTypes.objectAtIndex(J));
              if CheckContentType(Name, FileType) then AddContentType(FileType);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure ParseFolder(const Path: String);
var
  FindData: TSearchRec;
begin
  if FindFirst(Path + '*.qlgenerator', faDirectory, FindData) = 0 then
  begin
    repeat
       ParseFile(Path, FindData.Name);
    until FindNext(FindData) <> 0;
    FindClose(FindData);
  end;
end;

function IsExtensionExcluded(FileType: NSString): Boolean;
var
  Index: Integer;
begin
  for Index:= 0 to High(ExtensionExcludeList) do
  begin
    if StrComp(FileType.UTF8String, ExtensionExcludeList[Index]) = 0 then
      Exit(True);
  end;
  Result:= False;
end;

// Paths of preview extensions the user switched off (System Settings, or
// "pluginkit -e ignore"). pluginkit prints them with a leading '-' and the
// path in the last tab-separated column; if the tool is unavailable nothing
// is excluded.
procedure LoadDisabledExtensions;
var
  I: Integer;
  Task: NSTask;
  Pipe: NSPipe;
  Output, Line: NSString;
  Lines: NSArray;
  Arguments: NSMutableArray;
  Tab: NSRange;
begin
  DisabledExtensions:= NSMutableSet.alloc.init;
  if not NSFileManager.defaultManager.isExecutableFileAtPath(StringToNSString(PluginKitPath)) then
    Exit;

  Arguments:= NSMutableArray.array_;
  Arguments.addObject(StringToNSString('-m'));
  Arguments.addObject(StringToNSString('-v'));
  Arguments.addObject(StringToNSString('-p'));
  Arguments.addObject(StringToNSString(QLPreviewExtensionPoint));

  Pipe:= NSPipe.pipe;
  Task:= NSTask.alloc.init;
  Task.setLaunchPath(StringToNSString(PluginKitPath));
  Task.setArguments(Arguments);
  Task.setStandardOutput(Pipe);
  Task.setStandardError(NSFileHandle.fileHandleWithNullDevice);
  Task.launch;
  Output:= NSString.alloc.initWithData_encoding(Pipe.fileHandleForReading.readDataToEndOfFile, NSUTF8StringEncoding);
  Task.waitUntilExit;
  Task.release;
  if not Assigned(Output) then Exit;

  Lines:= Output.componentsSeparatedByString(StringToNSString(#10));
  for I:= 0 to Integer(Lines.count) - 1 do
  begin
    Line:= NSString(Lines.objectAtIndex(I));
    if not Line.hasPrefix(StringToNSString('-')) then Continue;
    Tab:= Line.rangeOfString_options(StringToNSString(#9), NSBackwardsSearch);
    if Tab.location <> NSNotFound then
      DisabledExtensions.addObject(Line.substringFromIndex(Tab.location + 1).stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceCharacterSet));
  end;
  Output.release;
end;

// Modern Quick Look previewers are app extensions (.appex) rather than
// .qlgenerator bundles; their supported types are listed under
// NSExtension/NSExtensionAttributes/QLSupportedContentTypes.
procedure ParseExtension(const BundlePath: String);
var
  I: Integer;
  Data: NSData;
  Item: id;
  Dict, Extension, Attributes: NSDictionary;
  ExtensionPoint: NSString;
  ContentTypes: NSArray;
begin
  if DisabledExtensions.containsObject(StringToNSString(BundlePath)) then Exit;
  Data:= NSData.dataWithContentsOfFile(StringToNSString(BundlePath + '/Contents/Info.plist'));
  if not Assigned(Data) then Exit;
  Dict:= NSDictionary(NSPropertyListSerialization.propertyListWithData_options_format_error(Data, NSPropertyListImmutable, nil, nil));
  if not (Assigned(Dict) and Dict.isKindOfClass(NSDictionary)) then Exit;

  Extension:= NSDictionary(Dict.valueForKey(StringToNSString('NSExtension')));
  if not (Assigned(Extension) and Extension.isKindOfClass(NSDictionary)) then Exit;
  ExtensionPoint:= NSString(Extension.valueForKey(StringToNSString('NSExtensionPointIdentifier')));
  if not (Assigned(ExtensionPoint) and ExtensionPoint.isKindOfClass(NSString)) then Exit;
  if StrComp(ExtensionPoint.UTF8String, QLPreviewExtensionPoint) <> 0 then Exit;

  Attributes:= NSDictionary(Extension.valueForKey(StringToNSString('NSExtensionAttributes')));
  if not (Assigned(Attributes) and Attributes.isKindOfClass(NSDictionary)) then Exit;
  ContentTypes:= NSArray(Attributes.valueForKey(StringToNSString('QLSupportedContentTypes')));
  if not (Assigned(ContentTypes) and ContentTypes.isKindOfClass(NSArray)) then Exit;

  for I:= 0 to Integer(ContentTypes.count) - 1 do
  begin
    Item:= ContentTypes.objectAtIndex(I);
    if Item.isKindOfClass(NSString) and not IsExtensionExcluded(NSString(Item)) then
      AddContentType(NSString(Item));
  end;
end;

procedure ParseExtensionFolder(const Path: String);
var
  FindData: TSearchRec;
begin
  if FindFirst(Path + '*.appex', faDirectory, FindData) = 0 then
  begin
    repeat
       ParseExtension(Path + FindData.Name);
    until FindNext(FindData) <> 0;
    FindClose(FindData);
  end;
end;

// Extensions shipped inside applications: <Apps>/*.app/Contents/PlugIns/*.appex
procedure ParseAppPlugIns(const AppsPath: String);
var
  FindData: TSearchRec;
begin
  if FindFirst(AppsPath + '*.app', faDirectory, FindData) = 0 then
  begin
    repeat
       ParseExtensionFolder(AppsPath + FindData.Name + '/Contents/PlugIns/');
    until FindNext(FindData) <> 0;
    FindClose(FindData);
  end;
end;

function CheckFile_oldMacOS(const FileName: String): Boolean;
var
  Index: Integer;
  QLType: NSString;
  FileType: NSString;
begin
  FileType:= NSWorkspace.sharedWorkspace.typeOfFile_error(StringToNSString(FileName), nil);
  if (FileType = nil) then
  begin
    Result:= False;
  end
  else begin
    for Index:= 0 to QLContentTypes.Count - 1 do
    begin
      QLType:= NSString(QLContentTypes.objectAtIndex(Index));
      // Direct comparison
      if (FileType.compare(QLType) = NSOrderedSame) then
        Exit(True);
      // Conforms checking
      if UTTypeConformsTo(CFStringRef(FileType), CFStringRef(QLType)) then
        Exit(True);
    end;
    Result:= False;
  end;
end;

function CheckFile_newMacOS(const FileName: String): Boolean;
var
  Index: Integer;
  FileExt: String;
  QLUTType: UTType;
  QLType: NSString;
  FileType: NSString;
  FileUTType: UTType;
begin
  FileExt:= ExtractOnlyFileExt(FileName);
  FileUTType:= UTType.typeWithFilenameExtension(StringToNSString(FileExt));
  if (FileUTType = nil) then
  begin
    Result:= False;
  end
  else begin
    FileType:= FileUTType.identifier;
    // Direct comparison
    for Index:= 0 to QLContentTypes.Count - 1 do
    begin
      QLType:= NSString(QLContentTypes.objectAtIndex(Index));
      if (FileType.compare(QLType) = NSOrderedSame) then
        Exit(True);
    end;
    // Conforms checking
    for Index:= 0 to QLContentUTTypes.Count - 1 do
    begin
      QLUTType:= UTType(QLContentUTTypes.objectAtIndex(Index));
      if FileUTType.conformsToType(QLUTType) then
        Exit(True);
    end;
    Result:= False;
  end;
end;

function CheckFile(const FileName: String): Boolean;
begin
  if NSAppKitVersionNumber >= NSAppKitVersionNumber11_0 then
    Result:= CheckFile_newMacOS( FileName )
  else
    Result:= CheckFile_oldMacOS( FileName );
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

function DCQLPreview.acceptsFirstResponder: ObjCBOOL;
begin
  Result:= false;
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
  item.release;
end;

function ListLoad( ParentWin:THandle; FileToLoad:pchar; {%H-}ShowFlags:integer):THandle; cdecl;
var
  view: QLPreviewView;
begin
  if not CheckFile(FileToLoad) then
    Exit(wlxInvalidHandle);

  view:= DCQLPreview.alloc.init;
  view.setAutostarts( true );
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
  if not CheckFile(FileToLoad) then
    Exit(LISTPLUGIN_ERROR);

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

procedure ListSetDefaultParams(dps: PListDefaultParamStruct); cdecl;
begin
  QLContentTypes:= NSMutableArray.alloc.init;
  QLContentUTTypes:= NSMutableArray.alloc.init;
  ParseFolder('/Library/QuickLook/');
  ParseFolder('/System/Library/QuickLook/');
  ParseFolder(IncludeTrailingBackslash(GetUserDir) + 'Library/QuickLook/');
  LoadDisabledExtensions;
  ParseExtensionFolder('/Library/QuickLook/');
  ParseExtensionFolder(IncludeTrailingBackslash(GetUserDir) + 'Library/QuickLook/');
  ParseAppPlugIns('/Applications/');
  ParseAppPlugIns(IncludeTrailingBackslash(GetUserDir) + 'Applications/');
  DisabledExtensions.release;
end;

procedure ListGetDetectString(DetectString:pchar;maxlen:integer); cdecl;
begin
  StrLCopy(DetectString, '(EXT!="")', MaxLen);
end;

exports
  ListLoad,
  ListLoadNext,
  ListCloseWindow,
  ListGetDetectString,
  ListSetDefaultParams;

end.


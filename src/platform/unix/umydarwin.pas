{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific DARWIN functions.

   Copyright (C) 2016-2017 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit uMyDarwin;

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, MacOSAll, CocoaAll;

function StringToNSString(const S: String): NSString;
function StringToCFStringRef(const S: String): CFStringRef;

function NSGetFolderPath(Folder: NSSearchPathDirectory): String;

function GetFileDescription(const FileName: String): String;
function MountNetworkDrive(const serverAddress: String): Boolean;

var
  HasMountURL: Boolean = False;

implementation

uses
  DynLibs;

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

function StringToCFStringRef(const S: String): CFStringRef;
begin
  Result:= CFStringCreateWithCString(nil, PAnsiChar(S), kCFStringEncodingUTF8);
end;

function NSGetFolderPath(Folder: NSSearchPathDirectory): String;
var
  Path: NSArray;
begin
  Path:= NSFileManager.defaultManager.URLsForDirectory_inDomains(Folder, NSUserDomainMask);
  if Path.count > 0 then
  begin
    Result:= IncludeTrailingBackslash(NSURL(Path.objectAtIndex(0)).path.UTF8String) + ApplicationName;
  end;
end;

function GetFileDescription(const FileName: String): String;
var
  Error: NSError;
  WS: NSWorkspace;
  FileType: NSString;
  FileNameRef: CFStringRef;
begin
  WS:= NSWorkspace.sharedWorkspace;
  FileNameRef:= StringToCFStringRef(FileName);
  if (FileNameRef = nil) then Exit(EmptyStr);
  FileType:= WS.typeOfFile_error(NSString(FileNameRef), @Error);
  if (FileType = nil) then
    Result:= Error.localizedDescription.UTF8String
  else begin
    Result:= WS.localizedDescriptionForType(FileType).UTF8String;
  end;
  CFRelease(FileNameRef);
end;

var
  NetFS: TLibHandle = NilHandle;
  CoreServices: TLibHandle = NilHandle;

var
  FSMountServerVolumeSync: function(url: CFURLRef; mountDir: CFURLRef; user: CFStringRef; password: CFStringRef;
    mountedVolumeRefNum: FSVolumeRefNumPtr; flags: OptionBits): OSStatus; stdcall;
  NetFSMountURLSync: function(_url: CFURLRef; _mountpath: CFURLRef; _user: CFStringRef; _passwd: CFStringRef;
    _open_options: CFMutableDictionaryRef; _mount_options: CFMutableDictionaryRef; _mountpoints: CFArrayRefPtr): Int32; cdecl;

function MountNetworkDrive(const serverAddress: String): Boolean;
var
  sharePath: NSURL;
  mountPoints: CFArrayRef = nil;
begin
  sharePath:= NSURL.URLWithString(StringToNSString(serverAddress));
  if Assigned(NetFSMountURLSync) then
    Result:= NetFSMountURLSync(CFURLRef(sharePath), nil, nil, nil, nil, nil, @mountPoints) = 0
  else begin
    Result:= FSMountServerVolumeSync(CFURLRef(sharePath), nil, nil, nil, nil, 0) = noErr;
  end;
end;

procedure Initialize;
begin
  NetFS:= LoadLibrary('/System/Library/Frameworks/NetFS.framework/NetFS');
  if (NetFS <> NilHandle) then
  begin
    @NetFSMountURLSync:= GetProcAddress(NetFS, 'NetFSMountURLSync');
  end;
  CoreServices:= LoadLibrary('/System/Library/Frameworks/CoreServices.framework/CoreServices');
  if (CoreServices <> NilHandle) then
  begin
    @FSMountServerVolumeSync:= GetProcAddress(CoreServices, 'FSMountServerVolumeSync');
  end;
  HasMountURL:= Assigned(NetFSMountURLSync) or Assigned(FSMountServerVolumeSync);
end;

procedure Finalize;
begin
  if (NetFS <> NilHandle) then FreeLibrary(NetFS);
  if (CoreServices <> NilHandle) then FreeLibrary(CoreServices);
end;

initialization
  Initialize;

finalization
  Finalize;

end.


unit uDarwinFile;

{$mode delphi}
{$modeswitch objectivec2}
{$modeswitch cblocks}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll, Cocoa_Extra, CocoaUtils,
  uDarwinUtil;

type
  
  { TDarwinFileUtil }

  TDarwinFileUtil = class
  private class var
    NetFS: TLibHandle;
    CoreServices: TLibHandle;
    FSMountServerVolumeSync: function(url: CFURLRef; mountDir: CFURLRef; user: CFStringRef; password: CFStringRef;
      mountedVolumeRefNum: FSVolumeRefNumPtr; flags: OptionBits): OSStatus; stdcall;
    NetFSMountURLSync: function(_url: CFURLRef; _mountpath: CFURLRef; _user: CFStringRef; _passwd: CFStringRef;
      _open_options: CFMutableDictionaryRef; _mount_options: CFMutableDictionaryRef; _mountpoints: CFArrayRefPtr): Int32; cdecl;
  public class var
    isMountSupported: Boolean;
  public
    class function mount( const serverAddress: String ): Boolean;
    class function unmountAndEject( const path: String ): Boolean;
    class function resolveAlias( const path: String ): String;
  public
    class function getDisplayName( const path: String ): String;
    class function getUniqueIcon( const path: String ): NSImage;
    class function getDescription( const path: String ): String;
  public
    class function getTempPath: String;
    class function getTerminalPath: String;
    class function getSpecifiedFolderPath( folder: NSSearchPathDirectory ): String;
  public
    class function dataWithContentsOfFile( const path: NSString; const tag: String ): NSData; overload;
    class function dataWithContentsOfFile( const path: String; const tag: String ): NSData; overload;
  end;

implementation

const
  ICON_SPECIAL_FOLDER_EXT_STRING = '.app;.musiclibrary;.imovielibrary;.tvlibrary;.photoslibrary;.theater;.saver;.xcode;.xcodeproj;.xcworkspace;.playground;.scptd;.action;.workflow;.prefpane;.appex;.kext;.xpc;.bundle;.qlgenerator;.mdimporter;.systemextension;.fcpbundle;.fcpxmld;';
  ICON_SPECIAL_PARENT_FOLDER_STRING = '/;/System;/Applications;/Volumes;/Users;~;~/Music;~/Pictures;~/Movies;';

var
  ICON_SPECIAL_FOLDER_EXT: NSString;
  ICON_SPECIAL_PARENT_FOLDER: NSString;

class function TDarwinFileUtil.mount(const serverAddress: String): Boolean;
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

type
  TUnmountManager = class
  public
    class function unmount( const path: String; const allPartitions: Boolean ): Boolean;
  private
    function doUnmount( const path: String; const allPartitions: Boolean ): Boolean;
    procedure onComplete( error: NSError ); cdecl;
  end;

class function TUnmountManager.unmount( const path: String; const allPartitions: Boolean ): Boolean;
var
  manager: TUnmountManager;
begin
  manager:= TUnmountManager.Create;
  Result:= manager.doUnmount( path, allPartitions );
  // free in TUnmountManager.onComplete();
end;

function TUnmountManager.doUnmount(const path: String; const allPartitions: Boolean): Boolean;
var
  url: NSURL;
  options: NSFileManagerUnmountOptions = 0;
begin
  url:= NSURL.fileURLWithPath( StringToNSString(path) );
  if allPartitions then
    options:= NSFileManagerUnmountAllPartitionsAndEjectDisk;
  NSFileManager.defaultManager.unmountVolumeAtURL_options_completionHandler( url, options, self.onComplete );
  sleep( 1000 );
  Result:= True;
end;

procedure TUnmountManager.onComplete( error: NSError ); cdecl;
begin
  if Assigned(error) then
    logDarwinError( 'TUnmountManager error when unmount', error );
  self.Free;
end;

class function TDarwinFileUtil.unmountAndEject(const path: String): Boolean;
begin
  Result:= TUnmountManager.unmount( path, True );
end;

class function TDarwinFileUtil.getDisplayName(const path: String): String;
var
  cocoaPath: NSString;
  displayName: NSString;
begin
  cocoaPath:= StringToNSString(path).stringByStandardizingPath;
  displayName:= NSFileManager.defaultManager.displayNameAtPath( cocoaPath );
  Result:= displayName.UTF8String;
end;

class function TDarwinFileUtil.getUniqueIcon(const path: String): NSImage;
  function hasUniqueIcon( const path: String ): Boolean;
  var
    pathRef: FSRef;
    catalogInfo: FSCatalogInfo;
    pFinderInfo: FileInfoPtr;
  begin
    FSPathMakeRef( pchar(path), pathRef, nil );
    FSGetCatalogInfo( pathRef, kFSCatInfoFinderInfo, @catalogInfo, nil, nil, nil );
    pFinderInfo:= FileInfoPtr( @catalogInfo.finderInfo );
    Result:= (pFinderInfo^.finderFlags and kHasCustomIcon) <> 0;
  end;

  function hasSpecialFolderExt( const path: String ): Boolean;
  var
    ext: NSString;
  begin
    ext:= StringToNSString(path).pathExtension.lowercaseString;
    ext:= NSSTR('.').stringByAppendingString(ext).stringByAppendingString(NSSTR(';'));
    Result:= ICON_SPECIAL_FOLDER_EXT.containsString( ext );
  end;

  function inSpecialParentFolder( const path: String ): Boolean;
  var
    parentPath: NSString;
  begin
    parentPath:= StringToNSString(path).stringByDeletingLastPathComponent;
    parentPath:= parentPath.stringByAppendingString(NSSTR(';'));
    Result:= ICON_SPECIAL_PARENT_FOLDER.containsString( parentPath );
  end;

begin
  Result:= nil;
  if hasUniqueIcon(path) or hasSpecialFolderExt(path) or inSpecialParentFolder(path) then
    Result:= NSWorkspace.sharedWorkspace.iconForFile( StringToNSString(path) );
end;

class function TDarwinFileUtil.getTempPath: String;
begin
  Result:= IncludeTrailingBackslash(NSTemporaryDirectory.UTF8String);
end;

class function TDarwinFileUtil.getTerminalPath(): String;
begin
  Result:= NSStringToString( NSWorkspace.sharedWorkspace.fullPathForApplication( NSStr('terminal') ) );
end;

class function TDarwinFileUtil.getSpecifiedFolderPath(folder: NSSearchPathDirectory
  ): String;
var
  Path: NSArray;
begin
  Path:= NSFileManager.defaultManager.URLsForDirectory_inDomains(folder, NSUserDomainMask);
  if Path.count > 0 then
  begin
    Result:= IncludeTrailingBackslash(NSURL(Path.objectAtIndex(0)).path.UTF8String) + ApplicationName;
  end;
end;

class function TDarwinFileUtil.dataWithContentsOfFile
  ( const path: NSString; const tag: String ): NSData;
var
  error: NSError = nil;
begin
  Result:= NSData.dataWithContentsOfFile_options_error( path, 0, @error );
  if error <> nil then
    logDarwinError( tag, error );
end;

class function TDarwinFileUtil.dataWithContentsOfFile(
  const path: String; const tag: String ): NSData;
begin
  Result:= TDarwinFileUtil.dataWithContentsOfFile( StringToNSString(path), tag );
end;

class function TDarwinFileUtil.getDescription(const path: String): String;
var
  error: NSError = nil;
  WS: NSWorkspace;
  FileType: NSString;
  FileNameRef: CFStringRef;
begin
  WS:= NSWorkspace.sharedWorkspace;
  FileNameRef:= StringToCFStringRef(path);
  if (FileNameRef = nil) then Exit(EmptyStr);
  FileType:= WS.typeOfFile_error(NSString(FileNameRef), @error);
  if (FileType = nil) then
    Result:= error.localizedDescription.UTF8String
  else begin
    Result:= WS.localizedDescriptionForType(FileType).UTF8String;
  end;
  CFRelease(FileNameRef);
end;

class function TDarwinFileUtil.resolveAlias(const path: String): String;
var
  ASource: NSURL;
  ATarget: NSURL;
  error: NSError = nil;
begin
  Result:= EmptyStr;
  ASource:= NSURL.fileURLWithPath(StringToNSString(path));
  ATarget:= NSURL.URLByResolvingAliasFileAtURL_options_error(
    ASource, NSURLBookmarkResolutionWithoutUI, @error );
  if Assigned(ATarget) then
    Result:= ATarget.fileSystemRepresentation
  else
    logDarwinError( 'TDarwinFileUtil.resolveAlias', error );
end;

procedure Initialize;
begin
  TDarwinFileUtil.NetFS:= LoadLibrary('/System/Library/Frameworks/NetFS.framework/NetFS');
  if (TDarwinFileUtil.NetFS <> NilHandle) then
  begin
    @TDarwinFileUtil.NetFSMountURLSync:= GetProcAddress(TDarwinFileUtil.NetFS, 'NetFSMountURLSync');
  end;
  TDarwinFileUtil.CoreServices:= LoadLibrary('/System/Library/Frameworks/CoreServices.framework/CoreServices');
  if (TDarwinFileUtil.CoreServices <> NilHandle) then
  begin
    @TDarwinFileUtil.FSMountServerVolumeSync:= GetProcAddress(TDarwinFileUtil.CoreServices, 'FSMountServerVolumeSync');
  end;
  TDarwinFileUtil.isMountSupported:= Assigned(TDarwinFileUtil.NetFSMountURLSync) or Assigned(TDarwinFileUtil.FSMountServerVolumeSync);

  ICON_SPECIAL_FOLDER_EXT:= StringToNSString( ICON_SPECIAL_FOLDER_EXT_STRING );
  ICON_SPECIAL_FOLDER_EXT.retain;
  ICON_SPECIAL_PARENT_FOLDER:= StringToNSString( ICON_SPECIAL_PARENT_FOLDER_STRING );
  ICON_SPECIAL_PARENT_FOLDER:= ICON_SPECIAL_PARENT_FOLDER.stringByReplacingOccurrencesOfString_withString( NSSTR('~'), NSHomeDirectory );
  ICON_SPECIAL_PARENT_FOLDER.retain;
end;

procedure Finalize;
begin
  if (TDarwinFileUtil.NetFS <> NilHandle) then FreeLibrary(TDarwinFileUtil.NetFS);
  if (TDarwinFileUtil.CoreServices <> NilHandle) then FreeLibrary(TDarwinFileUtil.CoreServices);
end;

initialization
  Initialize;

finalization
  Finalize;

end.


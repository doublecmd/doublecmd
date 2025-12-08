unit uDarwinFileUtil;

{$mode delphi}
{$modeswitch objectivec2}
{$modeswitch cblocks}

interface

uses
  Classes, SysUtils,
  uDebug, uLog,
  MacOSAll, CocoaAll, Cocoa_Extra, CocoaUtils;

function MountNetworkDrive(const serverAddress: String): Boolean;
function unmountAndEject(const path: String): Boolean;

var
  HasMountURL: Boolean = False;

implementation

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
  sharePath:= NSURL.URLWithString(StrToNSString(serverAddress));
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
  url:= NSURL.fileURLWithPath( StrToNSString(path) );
  if allPartitions then
    options:= NSFileManagerUnmountAllPartitionsAndEjectDisk;
  NSFileManager.defaultManager.unmountVolumeAtURL_options_completionHandler( url, options, self.onComplete );
  sleep( 1000 );
  Result:= True;
end;

procedure TUnmountManager.onComplete( error: NSError ); cdecl;
var
  msg: String;
begin
  if Assigned(error) then begin
    msg:= 'there is an error in TUnmountManager when unmount: ' + error.localizedDescription.UTF8String;
    DCDebug( msg );
    LogWrite( msg , lmtError );
  end;
  self.Free;
end;

function unmountAndEject(const path: String): Boolean;
begin
  Result:= TUnmountManager.unmount( path, True );
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


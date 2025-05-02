{
   Notes:
   1. contains only the most basic extensions to CocoaAll
   2. the purpose is to avoid dependence on LCL/Cocoa
}

unit uMiniCocoa;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}
{$linkframework Security}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll;

const
  NSAppKitVersionNumber11_0  = 2022;
  NSAppKitVersionNumber12_0  = 2113;
  NSAppKitVersionNumber13_0  = 2299;
  NSAppKitVersionNumber14_0  = 2487;

  // 11.0
  NSTableViewStyleAutomatic = 0;
  NSTableViewStyleFullWidth = 1;
  NSTableViewStyleInset = 2;
  NSTableViewStyleSourceList = 3;
  NSTableViewStylePlain = 4;

const
  NSJSONWritingWithoutEscapingSlashes = 1 shl 3;

const
  NSVisualEffectMaterialAppearanceBased = 0 deprecated;
  NSVisualEffectMaterialLight = 1 deprecated;
  NSVisualEffectMaterialDark = 2 deprecated;
  NSVisualEffectMaterialTitlebar = 3;
  NSVisualEffectMaterialSelection = 4;
  // 10.11
  NSVisualEffectMaterialMenu = 5;
  NSVisualEffectMaterialPopover = 6;
  NSVisualEffectMaterialSidebar =  7;
  NSVisualEffectMaterialMediumLight = 8 deprecated;
  NSVisualEffectMaterialUltraDark = 9 deprecated;
  // 10.14
  NSVisualEffectMaterialHeaderView = 10;
  NSVisualEffectMaterialSheet = 11;
  NSVisualEffectMaterialWindowBackground = 12;
  NSVisualEffectMaterialHUDWindow = 13;
  NSVisualEffectMaterialFullScreenUI = 15;
  NSVisualEffectMaterialToolTip = 17;
  NSVisualEffectMaterialContentBackground = 18;
  NSVisualEffectMaterialUnderWindowBackground = 21;
  NSVisualEffectMaterialUnderPageBackground = 22;

var
  kSecClassGenericPassword: NSString; cvar; external;

  kSecClass: NSString; cvar; external;
  kSecAttrService: NSString; cvar; external;
  kSecAttrAccount: NSString; cvar; external;
  kSecValueData: NSString; cvar; external;
  kSecAttrLabel: NSString; cvar; external;
  kSecMatchLimit: NSString; cvar; external;
  kSecMatchLimitOne: NSString; cvar; external;
  kSecReturnAttributes: NSString; cvar; external;
  kSecReturnData: NSString; cvar; external;
  kSecUseAuthenticationUI: NSString; cvar; external;
  kSecUseAuthenticationUISkip: NSString; cvar; external;
  kSecAttrAccessible: NSString; cvar; external;
  kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly: NSString; cvar; external;
  kSecAttrAccessibleAlways: NSString; cvar; external;
  kSecAttrAccessControl: NSString; cvar; external;

  function SecItemAdd( attributes: NSDictionary; result: CFTypeRefPtr ): OSStatus; external name '_SecItemAdd';
  function SecItemDelete( query: NSDictionary ): OSStatus; external name '_SecItemDelete';
  function SecItemCopyMatching( query: NSDictionary; result: CFTypeRefPtr ): OSStatus; external name '_SecItemCopyMatching';

const
  kSecAccessControlUserPresence = 1 shl 0;
  kSecAccessControlBiometryAny  = 1 shl 1;

type
  SecAccessControl = record end;
  SecAccessControlRef = ^SecAccessControl;
  SecAccessControlCreateFlags = NSUInteger;

  function SecAccessControlCreateWithFlags(allocator: CFAllocatorRef; protection: CFTypeRef; flags: SecAccessControlCreateFlags; error: CFErrorRefPtr ): SecAccessControlRef; external name '_SecAccessControlCreateWithFlags';

type

  TCocoaAppOnOpenURLNotify = procedure (const url: NSURL) of object;

  NSApplication_FIX = objccategory external (NSApplication)
    procedure setOpenURLObserver( onOpenURLObserver: TCocoaAppOnOpenURLNotify );
      message 'lclSetOpenURLObserver:';
  end;

  NSSplitView_FIX = objccategory external (NSSplitView)
    procedure insertArrangedSubview( view: NSView; index: NSInteger );
      message 'insertArrangedSubview:atIndex:';
  end;

  NSTableViewStyle = NSInteger;

  NSTableViewFix = objccategory external (NSTableView)
    procedure setStyle(newValue: NSTableViewStyle); message 'setStyle:';  // 11.0
  end;

  { NSFileRangeInputStream }

  NSFileRangeInputStream = objcclass( NSInputStream )
  private
    _fileStream: NSInputStream;
    _currentIndex: NSUInteger;
    _endIndex: NSUInteger;
  public
    function initWithFileAtPath_Range(path: NSString; range: NSRange): id; message 'initWithFileAtPath:range:';
    procedure dealloc; override;

    procedure open; override;
    procedure close; override;
    function read_maxLength(buffer: pbyte; len: NSUInteger): NSInteger; override;
    function hasBytesAvailable: ObjCBOOL; override;

    function getBuffer_length(buffer: pbyte; len: NSUIntegerPtr): ObjCBOOL; override;
    procedure setDelegate(newValue: NSStreamDelegateProtocol); override;
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); override;
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); override;
  end;

  function StringToNSString(const S: String): NSString;

type
  TNSDataArray = array of NSObject;

implementation

{ NSFileRangeInputStream }

function NSFileRangeInputStream.initWithFileAtPath_Range(path: NSString;
  range: NSRange): id;
begin
  Result:= Inherited init;
  _fileStream:= NSInputStream.alloc.initWithFileAtPath( path );
  _currentIndex:= range.location;
  _endIndex:= range.location + range.length;
end;

procedure NSFileRangeInputStream.dealloc;
begin
  _fileStream.release;
end;

procedure NSFileRangeInputStream.open;
begin
  _fileStream.open;
  _fileStream.setProperty_forKey(
    NSNumber.numberWithUnsignedInteger(_currentIndex),
    NSStreamFileCurrentOffsetKey );
end;

procedure NSFileRangeInputStream.close;
begin
  _fileStream.close;
end;

function NSFileRangeInputStream.read_maxLength(buffer: pbyte; len: NSUInteger
  ): NSInteger;
var
  realLength: NSUInteger;
begin
  realLength:= len;
  if _currentIndex + realLength > _endIndex then
    realLength:= _endIndex - _currentIndex;
  _currentIndex:= _currentIndex + realLength;
  Result:= _fileStream.read_maxLength(buffer, realLength);
end;

function NSFileRangeInputStream.hasBytesAvailable: ObjCBOOL;
begin
  Result:= _currentIndex < _endIndex;
end;

function NSFileRangeInputStream.getBuffer_length(buffer: pbyte;
  len: NSUIntegerPtr): ObjCBOOL;
begin
  Result:= False;
end;

procedure NSFileRangeInputStream.setDelegate(newValue: NSStreamDelegateProtocol
  );
begin
  _fileStream.setDelegate( newValue );
end;

procedure NSFileRangeInputStream.scheduleInRunLoop_forMode(aRunLoop: NSRunLoop;
  mode: NSString);
begin
  _fileStream.scheduleInRunLoop_forMode(aRunLoop, mode);
end;

procedure NSFileRangeInputStream.removeFromRunLoop_forMode(aRunLoop: NSRunLoop;
  mode: NSString);
begin
  _fileStream.removeFromRunLoop_forMode(aRunLoop, mode);
end;

// copy from uMyDarwin
function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

end.


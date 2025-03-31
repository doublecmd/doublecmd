{
   Notes:
   1. contains only the most basic extensions to CocoaAll
   2. the purpose is to avoid dependence on LCL/Cocoa
}

unit uMiniCocoa;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll;

type

  TCocoaAppOnOpenURLNotify = procedure (const url: NSURL) of object;

  NSApplication_FIX = objccategory external (NSApplication)
    procedure setOpenURLObserver( onOpenURLObserver: TCocoaAppOnOpenURLNotify );
      message 'lclSetOpenURLObserver:';
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


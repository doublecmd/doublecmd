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

  function StringToNSString(const S: String): NSString;

implementation

// copy from uMyDarwin
function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

end.


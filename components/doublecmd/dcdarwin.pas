unit DCDarwin;

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, BaseUnix, CocoaAll;

// MacOS File Utils
function MacosFileSetCreationTime( const path:String; const birthtime:time_t ): Boolean;

implementation

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

function MacosFileSetCreationTime( const path:String; const birthtime:time_t ): Boolean;
var
  attrs: NSMutableDictionary;
  nsPath: NSString;
begin
  attrs:= NSMutableDictionary.dictionaryWithCapacity( 1 );
  attrs.setValue_forKey( NSDate.dateWithTimeIntervalSince1970(birthtime), NSFileCreationDate );
  nsPath:= StringToNSString( path );
  Result:= NSFileManager.defaultManager.setAttributes_ofItemAtPath_error( attrs, nsPath, nil );
end;

end.


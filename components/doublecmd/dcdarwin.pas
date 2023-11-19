unit DCDarwin;

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, DCBasicTypes, CocoaAll;

// MacOS File Utils
function MacosFileSetCreationTime( const path:String; const birthtime:TFileTimeEx ): Boolean;

implementation

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

function MacosFileSetCreationTime( const path:String; const birthtime:TFileTimeEx ): Boolean;
var
  seconds: Double;
  attrs: NSMutableDictionary;
  nsPath: NSString;
begin
  Result:= true;
  if birthtime = TFileTimeExNull then exit;
  seconds:= birthtime.sec.ToDouble + birthtime.nanosec.ToDouble / (1000.0*1000.0*1000.0);
  attrs:= NSMutableDictionary.dictionaryWithCapacity( 1 );
  attrs.setValue_forKey( NSDate.dateWithTimeIntervalSince1970(seconds), NSFileCreationDate );
  nsPath:= StringToNSString( path );
  Result:= NSFileManager.defaultManager.setAttributes_ofItemAtPath_error( attrs, nsPath, nil );
end;

end.


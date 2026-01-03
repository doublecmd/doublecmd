unit uDarwinUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  uLog,
  MacOSAll, CocoaAll, CocoaUtils;

// Darwin Util Function
function StringToNSString(const S: String): NSString;
function StringToCFStringRef(const S: String): CFStringRef;
function NSArrayToList(const theArray:NSArray): TStringList;
function ListToNSArray(const list:TStrings): NSArray;
function ListToNSUrlArray(const list:TStrings): NSArray;
procedure logDarwinError(const tag: String; const error: NSError);
procedure logDarwinException(const tag: String; const e: Exception);

implementation

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

function StringToCFStringRef(const S: String): CFStringRef;
begin
  Result:= CFStringCreateWithCString(nil, PAnsiChar(S), kCFStringEncodingUTF8);
end;

function NSArrayToList(const theArray:NSArray): TStringList;
var
  i: Integer;
  list : TStringList;
begin
  list := TStringList.Create;
  for i := 0 to theArray.Count-1 do
  begin
    list.Add( NSStringToString( theArray.objectAtIndex(i) ) );
  end;
  Result := list;
end;

function ListToNSArray(const list:TStrings): NSArray;
var
  theArray: NSMutableArray;
  item: String;
begin
  theArray := NSMutableArray.arrayWithCapacity( list.Count );
  for item in list do begin
    theArray.addObject( StringToNSString(item) );
  end;
  Result := theArray;
end;

function ListToNSUrlArray(const list:TStrings): NSArray;
var
  theArray: NSMutableArray;
  item: String;
  url: NSUrl;
begin
  theArray:= NSMutableArray.arrayWithCapacity( list.Count );
  for item in list do begin
    url:= NSUrl.fileURLWithPath( StringToNSString(item) );
    theArray.addObject( url );
  end;
  Result:= theArray;
end;

procedure logDarwinError(const tag: String; const error: NSError);
begin
  if error <> nil then
    LogWrite( tag + ': ' + error.description.utf8String, lmtError );
end;

procedure logDarwinException(const tag: String; const e: Exception);
begin
  LogWrite( tag + ': ' + e.ToString, lmtError );
end;

function CFStringToStr(AString: CFStringRef): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result:= EmptyStr;
    Exit;
  end;
  // Try the quick way first
  Str:= CFStringGetCStringPtr(AString, kCFStringEncodingUTF8);
  if Str <> nil then
    Result:= PAnsiChar(Str)
  else begin
    // if that doesn't work this will
    StrRange.location:= 0;
    StrRange.length:= CFStringGetLength(AString);

    CFStringGetBytes(AString, StrRange, kCFStringEncodingUTF8,
                     Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
    begin
      CFStringGetBytes(AString, StrRange, kCFStringEncodingUTF8,
                       Ord('?'), False, @Result[1], StrSize, StrSize);
    end;
  end;
end;

end.


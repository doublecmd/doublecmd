unit FtpLng;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

var
  cAddConnection: String;
  cQuickConnection: String;

resourcestring
  rsAddConnection = 'Add connection';
  rsQuickConnection = 'Quick connection';

procedure TranslateResourceStrings;

implementation

uses
  FtpFunc;

function Translate(Name, Value: AnsiString; Hash: LongInt; Arg: Pointer): AnsiString;
var
  ALen: Integer;
begin
  with gStartupInfo do
  begin
    SetLength(Result, MaxSmallint);
    ALen:= TranslateString(Translation, PAnsiChar(Name), PAnsiChar(Value), PAnsiChar(Result), MaxSmallint);
    SetLength(Result, ALen);
  end;
end;

procedure TranslateResourceStrings;
begin
  if Assigned(gStartupInfo.Translation) then
  begin
    SetResourceStrings(@Translate, nil);
  end;
  cAddConnection:= '<' + rsAddConnection + '>';
  cQuickConnection:= '<' + rsQuickConnection + '>';
end;

end.


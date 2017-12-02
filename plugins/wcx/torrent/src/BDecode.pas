unit BDecode;

{======================================================================
BDecode.pas -- BitTorrent BDecoding Routines
Original Coding by Knowbuddy, 2003-03-19
======================================================================}

interface

uses
  SysUtils, Classes, Hashes, Contnrs;

type
  TISType = (tisString = 0, tisInt);
  TIntString = class(TObject)
  public
    StringPart: String;
    IntPart: Int64;
    ISType: TISType;
  end;
  function bdecodeStream(s: TStream): TObject;
  function bdecodeInt64(s: TStream): TIntString;
  function bdecodeHash(s: TStream): TObjectHash;
  function bdecodeString(s: TStream; i: Integer = 0): TIntString;
  function bdecodeList(s: TStream): TObjectList;
  function bin2hex(s: String; m: Integer = 999): String;

var
  hexchars: array[0..15] of Char = '0123456789abcdef';


implementation

function bin2hex(s: String; m: Integer = 999): String;
var
  i, j, k, l : Integer;
  r : array of Char;
begin
  l := Length(s);
  if(m < l) then l := m;
  SetLength(r,l * 2);
  for i := 1 to l do begin
    j := Ord(s[i]);
    k := (i - 1) * 2;
    r[k] := hexchars[j div 16];
    r[k+1] := hexchars[j mod 16];
  end;
  bin2hex := String(r);
end;

function bdecodeStream(s: TStream): TObject;
var
  r: TObject;
  c: Char;
  n: Integer;
begin
  n := s.Read(c, 1);
  if(n > 0) then begin
    case c of
    'd' : r:= bdecodeHash(s);
    'l' : r:= bdecodeList(s);
    'i' : r:= bdecodeInt64(s);
    '0'..'9' : r:= bdecodeString(s,StrToInt(c));
    else r := nil;
    end;
  end else begin
    r := nil;
  end;
  bdecodeStream := r;
end;

function bdecodeHash(s: TStream): TObjectHash;
var
  r: TObjectHash;
  o: TObject;
  n, st: Integer;
  c: Char;
  k, l: TIntString;
begin
  r := TObjectHash.Create();
  n := s.Read(c, 1);
  while((n > 0) and (c <> 'e') and (c >= '0') and (c <= '9')) do begin
    n := StrToInt(c);
    k := bdecodeString(s, n);
    if(k <> nil) then begin
      st := s.Position;
      o := bdecodeStream(s);
      if((o <> nil) and (k.StringPart <> '')) then r[k.StringPart] := o;
      if(k.StringPart = 'pieces') then begin
        k.StringPart:='pieces';
      end;
      if(k.StringPart = 'info') then begin
        l := TIntString.Create();
        l.IntPart := st;
        r['_info_start'] := l;
        l := TIntString.Create();
        l.IntPart := s.Position - st;
        r['_info_length'] := l;
      end;
    end;
    n := s.Read(c, 1);

  end;

  if ((c < '0') or (c > '9')) and (c <> 'e') then
    bdecodeHash:= nil
  else  bdecodeHash := r;
end;

function bdecodeList(s: TStream): TObjectList;
var
  r: TObjectList;
  o: TObject;
  n: Integer;
  c: Char;
begin
  r := TObjectList.Create();
  n := s.Read(c, 1);
  while((n > 0) and (c <> 'e')) do begin
    s.Seek(-1, soFromCurrent);
    o := bdecodeStream(s);
    if(o <> nil) then r.Add(o);
    n := s.Read(c, 1);
  end;
  bdecodeList := r;
end;

function bdecodeString(s: TStream; i: Integer = 0): TIntString;
var
  r: TIntString;
  t: String;
  c: Char;
  n: Integer;
begin
  c := '0';
  n := s.Read(c, 1);
  while((n > 0) and (c >= '0') and (c <= '9')) do begin
    i := (i * 10) + StrToInt(c);
    n := s.Read(c, 1);
  end;
  SetLength(t, i);
  n:=s.Read(PChar(t)^, i);
  r := TIntString.Create();
  r.StringPart := t;
  r.ISType := tisString;

  bdecodeString := r;
end;

function bdecodeInt64(s: TStream): TIntString;
var
  r: TIntString;
  i: Int64;
  c: Char;
  n: Integer;
  neg: boolean;

begin
  i := 0;
  c := '0';
  neg:= false;
  repeat
    if c='-' then neg:= true else i := (i * 10) + StrToInt(c);
    n := s.Read(c, 1);
  until not ((n > 0) and (((c >= '0')and(c <= '9'))or(c = '-')));

  if neg then i:=-i;

  r := TIntString.Create();
  r.IntPart := i;
  r.ISType := tisInt;
  bdecodeInt64 := r;
end;

end.

unit uPinyin;

interface

type
  pinyinarray=array[0..20901] of word;

function PinyinMatch(a,b:UnicodeChar):boolean;

implementation

Uses sysutils;

var
  PINYINTABLE: pinyinarray;
  PINYINTABLELOADED: boolean = False;

procedure loadPinyinTable;
var
  f:file of pinyinarray;
begin
  if PINYINTABLELOADED then exit;
  Assign(f, ExtractFileDir(Paramstr(0)) + '/tcmatch.tbl');
  Reset(f);
  seek(f,0);
  Read(f, PINYINTABLE);
  close(f);
  PINYINTABLELOADED := True;
  writeln('code in loadPinyinTable');
end;

function PinyinMatch(a,b:UnicodeChar):boolean;
var
  i,code:word;
  j:byte;
begin
  loadPinyinTable;
  PinyinMatch := True;
  if a = b then exit;
  i := ord(a) - 19968;
  code := PINYINTABLE[i];
  j := code and 31;
  if(j > 0) and (j+96 = ord(b)) then exit;
  j := code >> 5 and 31;
  if(j > 0) and (j+96 = ord(b)) then exit;
  j := code >> 10 and 31;
  if(j > 0) and (j+96 = ord(b)) then exit;
  PinyinMatch := False;
end;

procedure test;
  var
    s:UnicodeString;
    c:UnicodeChar;
begin
  loadPinyinTable;
  s := UTF8Decode('一重庆');
  c := s[2];
  writeln(PinyinMatch(c, UnicodeChar('i')));
  writeln(PinyinMatch(c, UnicodeChar('v')));
  writeln(PinyinMatch(c, UnicodeChar('c')));
  writeln(PinyinMatch(c, UnicodeChar('`')));
end;

end.

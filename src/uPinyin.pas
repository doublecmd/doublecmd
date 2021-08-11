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
  tblpath:string;
begin
  if PINYINTABLELOADED then exit;
  tblpath := ExtractFilePath(Paramstr(0)) + 'tcmatch.tbl';
  if fileexists(tblpath) then
  begin
    Assign(f, tblpath);
    try
      Reset(f);
      seek(f,0);
      Read(f, PINYINTABLE);
      PINYINTABLELOADED := True;
    finally
      close(f);
    end;
  end;
end;

function PinyinMatch(a,b:UnicodeChar):boolean;
var
  i,code:word;
  j:byte;
begin
  loadPinyinTable;
  PinyinMatch := True;

  if a = b then exit;

  if PINYINTABLELOADED then
  begin
    if (Ord(a) >= 19968) and (Ord(a) < 40869) then
    begin
      i := Ord(a) - 19968;
      code := PINYINTABLE[i];
      j := code and 31;
      if(j > 0) and (j+96 = Ord(b)) then exit;
      j := code >> 5 and 31;
      if(j > 0) and (j+96 = Ord(b)) then exit;
      j := code >> 10 and 31;
      if(j > 0) and (j+96 = Ord(b)) then exit;
    end;
  end;

  PinyinMatch := False;
end;

end.

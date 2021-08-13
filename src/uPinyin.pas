unit uPinyin;

interface

type
  TPinyinArray = array[0..20901] of Word;

function PinyinMatch(a,b:UnicodeChar):boolean;

implementation

Uses sysutils;

var
  PINYINTABLE: TPinyinArray;
  PINYINTABLELOADED: boolean = False;

procedure loadPinyinTable;
var
  f: THandle;
  tblpath: String;
begin
  tblpath := ExtractFilePath(Paramstr(0)) + 'tcmatch.tbl';
  if FileExists(tblpath) then
  begin
    f:= FileOpen(tblpath, fmOpenRead or fmShareDenyNone);
    if (f <> feInvalidHandle) then
    begin
      if FileRead(f, PINYINTABLE, SizeOf(TPinyinArray)) = SizeOf(TPinyinArray) then
        PINYINTABLELOADED := True;
      FileClose(f);
    end;
  end;
end;

function PinyinMatch(a,b:UnicodeChar):boolean;
var
  i,code:word;
  j:byte;
begin
  PinyinMatch := True;

  if a = b then exit;

  if PINYINTABLELOADED then
  begin
    if (Ord(a) >= 19968) and (Ord(a) <= 40869) then
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

initialization
  loadPinyinTable;

end.

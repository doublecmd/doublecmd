unit deb_io;

interface

{$A-,I-}

uses
  deb_def;

function  deb_ReadHeader(var f : file; var header, lastheader : deb_Header) : Boolean;

implementation

uses
   SysUtils;

function deb_ReadHeader;
var
  tmp_buf    : array [0..259] of Char;
  tmp_str    : String[10];
  j          : Integer;
  loadlen    : Integer;
begin
  Result := False;
  loadlen:=size_deb_files;
  Seek(f, lastheader.pos+lastheader.size);
    if IOResult <> 0 then Exit;
  header.pos:=FilePos(f)+size_deb_files;
  BlockRead(f, tmp_buf, loadlen);
    if IOResult <> 0 then Exit;
  if tmp_buf[0] = #10 then begin  // other version DPKG - offset 1.
    Seek(f, lastheader.pos+lastheader.size+1);
      if IOResult <> 0 then Exit;
    header.pos:=FilePos(f)+size_deb_files;
    BlockRead(f, tmp_buf, loadlen);
      if IOResult <> 0 then Exit;
  end;
  SetLength(header.filename, 16);
  for j:=1 to 16 do header.filename[j]:=tmp_buf[j-1];
  header.filename:=Trim(header.filename);
  SetLength(tmp_str, 10);
  for j:=1 to 10 do tmp_str[j] := tmp_buf[j - 1 + 32+16];
  if tmp_str = '' then header.size:=0 else header.size:=StrToInt(Trim(tmp_str));
  SetLength(tmp_str, 12);
  for j:=1 to 12 do tmp_str[j] := tmp_buf[j - 1 + 16];
  if tmp_str = '' then header.Time:=0 else header.Time:=StrToInt(Trim(tmp_str));
  header.crc:=0;
  Result := True;
end;

end.

unit deb_io;

interface

{$A-,I-}

uses
  deb_def;

function  deb_ReadHeader(var f : file; var header, lastheader : deb_Header) : Boolean;

implementation

uses
   SysUtils, DCStrUtils, DCFileAttributes;

function deb_ReadHeader(var f : file; var header, lastheader : deb_Header) : Boolean;
var
  tmp_str    : String;
  loadlen    : Integer;
  tmp_buf    : array [0..259] of Char;
begin
  Result:= False;
  loadlen:= size_deb_files;
  // Skip last header
  Seek(f, lastheader.pos + lastheader.size);
  if IOResult <> 0 then Exit;
  // Read next header
  header.pos:= FilePos(f) + size_deb_files;
  BlockRead(f, {%H-}tmp_buf, loadlen);
  if IOResult <> 0 then Exit;
  // Other version DPKG - offset 1.
  if tmp_buf[0] = #10 then
  begin
    Seek(f, lastheader.pos + lastheader.size + 1);
    if IOResult <> 0 then Exit;
    header.pos:= FilePos(f) + size_deb_files;
    BlockRead(f, tmp_buf, loadlen);
    if IOResult <> 0 then Exit;
  end;
  // Read file name
  SetLength(header.filename, 16);
  Move(tmp_buf[0], header.filename[1], 16);
  header.filename:= Trim(header.filename);
  if (Length(header.filename) > 0) then
  begin
    loadlen:= Length(header.filename);
    if header.filename[loadlen] = '/' then
      SetLength(header.filename, loadlen - 1);
  end;
  // Read file time
  SetLength(tmp_str, 12);
  Move(tmp_buf[16], tmp_str[1], 12);
  header.Time:= StrToIntDef(Trim(tmp_str), 0);
  // Read file mode
  SetLength(tmp_str, 8);
  Move(tmp_buf[40], tmp_str[1], 8);
  tmp_str:= Trim(tmp_str);
  if Length(tmp_str) > 0 then
    header.Mode:= OctToDec(tmp_str)
  else begin
    header.Mode:= GENERIC_ATTRIBUTE_FILE;
  end;
  // Read file size
  SetLength(tmp_str, 10);
  Move(tmp_buf[48], tmp_str[1], 10);
  header.size:= StrToIntDef(Trim(tmp_str), 0);
  Result := True;
end;

end.

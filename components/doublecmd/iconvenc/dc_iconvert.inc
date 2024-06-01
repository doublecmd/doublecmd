function Iconvert(S: string; var Res: string; const FromEncoding, ToEncoding: string): cint;
var
  InLen, OutLen, Offset: size_t;
  Src, Dst: pchar;
  H: iconv_t;
  lerr: cint;
  iconvres: size_t;
begin
  H := iconv_open(PChar(ToEncoding), PChar(FromEncoding));
  if h=Iconv_t(-1) then
  begin
    Res := S;
    exit(-1);
  end;

  try
    InLen:=Length(s);
    outlen:=InLen;
    setlength(res,outlen);

    Src := PChar(S);
    Dst := PChar(Res);

    while InLen > 0 do
    begin
      iconvres := iconv(H, @Src, @InLen, @Dst, @OutLen);
      if iconvres = size_t(-1) then
      begin
        lerr := cerrno;
        if lerr = ESysEILSEQ then // unknown char, skip
          begin
            Inc(Src);
            Dec(InLen);
          end
        else
          if lerr = ESysE2BIG then
            begin
              Offset := Dst - PChar(Res);
              SetLength(Res, Length(Res)+InLen*2+5); // 5 is minimally one utf-8 char
              Dst := PChar(Res) + Offset;
              OutLen := Length(Res) - Offset;
            end
          else
            exit(-1)
      end;
    end;
  finally
    setlength(Res,Length(Res) - Outlen);
    iconv_close(H);
  end;

  Result := 0;
end;

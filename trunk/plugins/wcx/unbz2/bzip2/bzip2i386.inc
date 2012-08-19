{$ASMMODE intel}

{$define HAVE_DETRANSFORM}
{
procedure Tbzip2_decode_stream.detransform;

var a:cardinal;
    p,q,r:Pcardinal;

begin
  a:=0;
  p:=@tt^[0];
  q:=p+tt_count;
  while p<>q do
    begin
      r:=@tt^[cftab[p^ and $ff]];
      inc(cftab[p^ and $ff]);
      r^:=r^ or a;
      inc(a,256);
      inc(p);
    end;
end;
}

{const c:cardinal=0;

procedure mcount;external name 'mcount';}


procedure Tbzip2_decode_stream.detransform;assembler;

asm
{  mov edx,offset c
  call mcount}
  xor edx,edx
  lea ebx,[esi+Tbzip2_decode_stream.cftab]
  mov ecx,[esi+Tbzip2_decode_stream.tt_count]
  push esi
  push ebp
  mov esi,[esi+Tbzip2_decode_stream.tt]
  mov edi,esi
  lea ebp,[4*ecx+esi]
  jmp @a2
@a1:
  movzx eax,byte [esi]
  mov ecx,[ebx+4*eax]
  inc dword [ebx+4*eax]
  or [edi+4*ecx],edx
  add edx,$100
  add esi,4
@a2:
  cmp esi,ebp
  jne @a1
  pop ebp
  pop esi
end ['eax','ebx','ecx','edx','edi'];

unit bzip2;
{****************************************************************************

                             BZIP2 decompression unit

                        Copyright (C) 2002 by Daniel Mantione

This unit provides a decompression stream to decode .bz2 files. It is
inpired by Julian R. Seward's libbzip2 library and therefore you should
send credits to him and bug reports to me :)

This code is licensed under the same terms as the original libbz2 library,
which is decsribed in the file LICENSE. If you don't have this file, look
at http://www.freepascal.org for this bzip2 unit, the LICENSE file will
be included. In case of problems, contact the author.

E-mail addresses:

Daniel Mantione     <daniel.mantione@freepascal.org>
Julian R. Seward    <jseward@acm.org>

Please do not contact Julian about this Pascal library, he didn't wrote it.

****************************************************************************}
interface

{$goto on}

uses objects;

const max_groups=6;
      max_alpha_size=258;
      max_code_len=23;
      group_size=50;
      iter_count=4;
      max_selectors=2+(900000 div group_size);

const mtfa_size=4096;
      mtfl_size=16;

type  Tcardinal_array=array [0..899999] of cardinal;
      Pcardinal_array=^Tcardinal_array;

      Pcardinal=^cardinal;
      Thuffarray=array[0..max_alpha_size] of cardinal;
      Phuffarray=^Thuffarray;

      Tbzip2_decode_stream=object(Tstream)
        short:cardinal;
        readstream:Pstream;
        block_randomized:boolean;
        blocksize:byte;
        tt:Pcardinal_array;
        tt_count:cardinal;
        rle_run_left,rle_run_data:byte;
        nextrle:Pbyte;
        decode_available:cardinal;
        block_origin:cardinal;
        current_block:cardinal;
        read_data,bits_available:byte;
        inuse16:set of 0..15;
        inuse:set of 0..255;
        inuse_count:cardinal;
        seq_to_unseq:array[0..255] of byte;
        alphasize:cardinal;
        group_count,group_pos,gsel,gminlen:byte;
        group_no:cardinal;
        glimit,gperm,gbase:Phuffarray;
        selector_count:cardinal;
        selector,selector_mtf:array[0..max_selectors] of byte;
        len:array[0..max_groups,0..max_alpha_size] of byte;
        limit:array[0..max_groups,0..max_alpha_size] of cardinal;
        base:array[0..max_groups,0..max_alpha_size] of cardinal;
        perm:array[0..max_groups,0..max_alpha_size] of cardinal;
        minlens:array[0..max_groups] of byte;
        cftab:array[0..257] of cardinal;
        mtfbase:array[0..256 div mtfl_size-1] of cardinal;
        mtfa:array[0..mtfa_size-1] of byte;
        constructor init(Areadstream:Pstream);
        function get_bits(n:byte):byte;
        function get_boolean:boolean;
        function get_byte:byte;
        function get_cardinal24:cardinal;
        function get_cardinal:cardinal;
        procedure receive_mapping_table;
        procedure receive_selectors;
        procedure undo_mtf_values;
        procedure receive_coding_tables;
        procedure make_hufftab;
        procedure init_mtf;
        function get_mtf_value:cardinal;
        procedure move_mtf_block;
        procedure receive_mtf_values;
        procedure detransform;
        function decode_block:boolean;
        procedure read(var buf;count:Longint);virtual;
        procedure new_block;
        procedure consume_rle;inline;
        procedure rle_read(bufptr:Pbyte;var count:Longint);
        destructor done;virtual;
      end;

{A bzip2 stream starts with this:}
const bzip2_stream_magic='BZh';

{Error codes for stream errorinfo.}
const bzip2_bad_header_magic        =1;
      bzip2_bad_block_magic         =2;
      bzip2_endoffile               =3;
      bzip2_data_error              =4;

implementation

{$ifdef i386}
  {$i bzip2i386.inc}
{$endif}

procedure hb_create_decode_tables(var limit,base,perm:array of cardinal;
                                  var length:array of byte;
                                  minlen,maxlen:byte;alphasize:cardinal);

var pp,i,j,vec:cardinal;

begin
  pp:=0;
  for i:=minlen to maxlen do
    for j:=0 to alphasize-1 do
      if length[j]=i then
        begin
          perm[pp]:=j;
          inc(pp);
        end;
  for i:=0 to max_code_len-1 do
    begin
      base[i]:=0;
      limit[i]:=0;
    end;
  for i:=0 to alphasize-1 do
    inc(base[length[i]+1]);
  for i:=1 to max_code_len-1 do
    inc(base[i],base[i-1]);
  vec:=0;
  for i:=minlen to maxlen do
    begin
      inc(vec,base[i+1]-base[i]);
      limit[i]:=vec-1;
      vec:=vec shl 1;
    end;
  for i:=minlen+1 to maxlen do
    base[i]:=((limit[i-1]+1) shl 1)-base[i];
end;

{*****************************************************************************
                             Tbzip2_decode_stream
*****************************************************************************}

constructor Tbzip2_decode_stream.init(Areadstream:Pstream);

var magic:array[1..3] of char;
    c:char;

begin
  readstream:=Areadstream;
  {Read the magic.}
  readstream^.read(magic,sizeof(magic));
  if magic<>bzip2_stream_magic then
    begin
      error(stiniterror,bzip2_bad_header_magic);
      exit;
    end;
  {Read the block size and allocate the working array.}
  readstream^.read(c,1);
  blocksize:=byte(c)-byte('0');
  getmem(tt,blocksize*100000*sizeof(cardinal));
  decode_available:=high(decode_available);
end;

function Tbzip2_decode_stream.get_bits(n:byte):byte;

var data:byte;

begin
  if n>bits_available then
    begin
      readstream^.read(data,1);
      get_bits:=(read_data shr (8-n)) or data shr (8-(n-bits_available));
      read_data:=data shl (n-bits_available);
      inc(bits_available,8);
    end
  else
    begin
      get_bits:=read_data shr (8-n);
      read_data:=read_data shl n;
    end;
  dec(bits_available,n);
end;

function Tbzip2_decode_stream.get_boolean:boolean;

begin
  get_boolean:=boolean(get_bits(1));
end;

function Tbzip2_decode_stream.get_byte:byte;

begin
  get_byte:=get_bits(8);
end;

function Tbzip2_decode_stream.get_cardinal24:cardinal;

begin
  get_cardinal24:=get_bits(8) shl 16 or get_bits(8) shl 8 or get_bits(8);
end;


function Tbzip2_decode_stream.get_cardinal:cardinal;

begin
  get_cardinal:=get_bits(8) shl 24 or get_bits(8) shl 16 or get_bits(8) shl 8 or
                get_bits(8);
end;

procedure Tbzip2_decode_stream.receive_mapping_table;

{Receive the mapping table. To save space, the inuse set is stored in pieces
 of 16 bits. First 16 bits are stored which pieces of 16 bits are used, then
 the pieces follow.}

var i,j:byte;

begin
  inuse16:=[];
  {Receive the first 16 bits which tell which pieces are stored.}
  for i:=0 to 15 do
    if get_boolean then
      include(inuse16,i);

  {Receive the used pieces.}
  inuse:=[];
  inuse_count:=0;
  for i:=0 to 15 do
    if i in inuse16 then
      for j:=0 to 15 do
        if get_boolean then
          begin
            include(inuse,16*i+j);
            seq_to_unseq[inuse_count]:=16*i+j;
            inc(inuse_count);
          end;
{  system.write('Mapping table: ');
  for i:=0 to 255 do
    if i in inuse then
      system.write(i,' ');
  writeln;}
end;

procedure Tbzip2_decode_stream.receive_selectors;

{Receives the selectors.}

var i:cardinal;
    j:byte;

begin
  group_count:=get_bits(3);
  selector_count:=get_bits(8) shl 7 or get_bits(7);
  for i:=0 to selector_count-1 do
    begin
      j:=0;
      while get_boolean do
        begin
          inc(j);
          if j>5 then
            error(streaderror,bzip2_data_error);
        end;
      selector_mtf[i]:=j;
    end;
{  system.write('Selector_mtf: ');
  for i:=0 to selector_count-1 do
    system.write(selector_mtf[i],' ');
  writeln;}
end;

procedure Tbzip2_decode_stream.undo_mtf_values;

{Undo the MTF values for the selectors.}

var pos:array[0..max_groups] of byte;
    i:cardinal;
    v,tmp:byte;

begin
  for v:=0 to group_count-1 do
    pos[v]:=v;
  for i:=0 to selector_count-1 do
    begin
      v:=selector_mtf[i];
      tmp:=pos[v];
      while v<>0 do
        begin
          pos[v]:=pos[v-1];
          dec(v);
        end;
      pos[0]:=tmp;
      selector[i]:=tmp;
    end;
end;

procedure Tbzip2_decode_stream.receive_coding_tables;

var t,curr:byte;
    i:cardinal;

begin
  for t:=0 to group_count-1 do
    begin
      curr:=get_bits(5);
      for i:=0 to alphasize-1 do
        begin
          repeat
            if not(curr in [1..20]) then
              begin
                error(streaderror,bzip2_data_error);
                exit;
              end;
            if not get_boolean then
              break;
            if get_boolean then
              dec(curr)
            else
              inc(curr);
          until false;
          len[t,i]:=curr;
        end;
    end;
{  writeln('Coding tables:');
  for t:=0 to group_count-1 do
    begin
      for i:=0 to alphasize-1 do
        system.write(len[t,i],' ');
      writeln;
    end;}
end;

procedure Tbzip2_decode_stream.make_hufftab;

{Builds the Huffman tables.}

var i:cardinal;
    t,minlen,maxlen:byte;

begin
  for t:=0 to group_count-1 do
    begin
      minlen:=32;
      maxlen:=0;
      for i:=0 to alphasize-1 do
        begin
          if len[t,i]>maxlen then
            maxlen:=len[t,i];
          if len[t,i]<minlen then
            minlen:=len[t,i];
        end;
      hb_create_decode_tables(limit[t],base[t],perm[t],len[t],
                              minlen,maxlen,alphasize);
      minlens[t]:=minlen;
    end;
end;

procedure Tbzip2_decode_stream.init_mtf;

var i,j:byte;
    k:cardinal;

begin
  k:=mtfa_size-1;
  for i:=256 div mtfl_size-1 downto 0 do
    begin
      for j:=mtfl_size-1 downto 0 do
        begin
          mtfa[k]:=i*mtfl_size+j;
          dec(k);
        end;
      mtfbase[i]:=k+1;
    end;
end;

function Tbzip2_decode_stream.get_mtf_value:cardinal;

var zn:byte;
    zvec:cardinal;

begin
  if group_pos=0 then
    begin
      inc(group_no);
      group_pos:=group_size;
      gsel:=selector[group_no];
      gminlen:=minlens[gsel];
      glimit:=@limit[gsel];
      gperm:=@perm[gsel];
      gbase:=@base[gsel];
    end;
  dec(group_pos);
  zn:=gminlen;
  zvec:=get_bits(zn);
  while zvec>glimit^[zn] do
    begin
      inc(zn);
      zvec:=zvec shl 1 or byte(get_boolean);
    end;
  get_mtf_value:=gperm^[zvec-gbase^[zn]];
end;

procedure Tbzip2_decode_stream.move_mtf_block;

var i:byte;
    j,k:cardinal;

begin
  k:=MTFA_SIZE;
  for i:=256 div MTFL_SIZE-1 downto 0 do
    begin
      j:=mtfbase[i];
      Pcardinal(@mtfa[k- 4])^:=Pcardinal(@mtfa[j+12])^;
      Pcardinal(@mtfa[k- 8])^:=Pcardinal(@mtfa[j+ 8])^;
      Pcardinal(@mtfa[k-12])^:=Pcardinal(@mtfa[j+ 4])^;
      dec(k,16);
      Pcardinal(@mtfa[k   ])^:=Pcardinal(@mtfa[j   ])^;
      mtfbase[i]:=k;
    end;
end;

procedure Tbzip2_decode_stream.receive_mtf_values;

const run_a=0;
      run_b=1;

var t,next_sym:cardinal;
    es:cardinal;
    n:byte;
    nn,i:cardinal;
    p,q:Pbyte;
    u,v:Pcardinal;
    lno,off:cardinal;

begin
  group_no:=high(group_no);
  group_pos:=0;
  t:=0;
  for i:=0 to 257 do
    cftab[i]:=0;
  init_mtf;
  next_sym:=get_mtf_value;
  while next_sym<>inuse_count+1 do
    begin
{      writeln(t,'   ',next_sym);
      if t=22296 then
        t:=t;                    }
      if next_sym<=run_b then
        begin
          es:=0;
          n:=0;
          repeat
            inc(es,(next_sym+1) shl n);
            inc(n);
            next_sym:=get_mtf_value;
          until next_sym>run_b;
          n:=seq_to_unseq[mtfa[mtfbase[0]]];
          inc(cftab[n],es);
          if t+es>100000*blocksize then
            begin
              error(streaderror,bzip2_data_error);
              exit;
            end;
          while es>0 do
            begin
              tt^[t]:=n;
              dec(es);
              inc(t);
            end;
        end
      else
        begin
          nn:=next_sym-1;
          if nn<mtfl_size then
            begin
              {Avoid the costs of the general case.}
              p:=@mtfa[mtfbase[0]];
              q:=p+nn;
              n:=q^;
              repeat
                q^:=(q-1)^;
                dec(q);
              until q=p;
              q^:=n;
            end
          else
            begin
              {General case.}
              lno:=nn div MTFL_SIZE;
              off:=nn and (MTFL_SIZE-1);
              p:=@mtfa[mtfbase[lno]];
              q:=p+off;
              n:=q^;
              while(q<>p) do
                begin
                  q^:=(q-1)^;
                  dec(q);
                end;
              u:=@mtfbase;
              v:=u+lno;
              repeat
                mtfa[v^]:=mtfa[(v-1)^+MTFL_SIZE-1];
                dec(v);
                dec(v^);
              until v=u;
              mtfa[v^]:=n;
              if v^=0 then
                move_mtf_block;
            end;
          inc(cftab[seq_to_unseq[n]]);
          tt^[t]:=cardinal(seq_to_unseq[n]);
          inc(t);
          if t>100000*blocksize then
            begin
              error(streaderror,bzip2_data_error);
              exit;
            end;
          next_sym:=get_mtf_value;
        end;
    end;
    tt_count:=t;
  {Setup cftab to facilitate generation of T^(-1).}
  t:=0;
  for i:=0 to 256 do
    begin
      nn:=cftab[i];
      cftab[i]:=t;
{      writeln(i,' ',t);}
      inc(t,nn);
    end;
end;

{$ifndef HAVE_DETRANSFORM}

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

{$endif}

function Tbzip2_decode_stream.decode_block:boolean;

{Decode a new compressed block.}

var magic:array[1..6] of char;
    stored_blockcrc:cardinal;
    i:byte;

begin
  for i:=1 to 6 do
    magic[i]:=char(get_byte);
  if magic='1AY&SY' then
    begin
      inc(current_block);
{      writeln('Block ',current_block,': Header ok');}
      stored_blockcrc:=get_cardinal;
      block_randomized:=get_boolean;
      block_origin:=get_cardinal24;

      {Receive the mapping table.}
      receive_mapping_table;
      alphasize:=cardinal(inuse_count)+2;
{      writeln('Mapping table ok.');}

      {Receive the selectors.}
      receive_selectors;
      if status<>0 then
        exit;
{      writeln('Selectors ok.');}
      {Undo the MTF values for the selectors.}
      undo_mtf_values;
{      writeln('Undo mtf ok.');}
      {Receive the coding tables.}
      receive_coding_tables;
      if status<>0 then
        exit;
{      writeln('Coding tables ok');}
      {Build the Huffman tables.}
      make_hufftab;
{      writeln('Huffman ok.');}
      {Receive the MTF values.}
      receive_mtf_values;
{      writeln('MTF OK');}
      {Undo the Burrows Wheeler transformation.}
      detransform;
{      writeln('Detransform OK');}
      decode_available:=tt_count;
    end
  else
    begin
      if magic<>#$17'rE8P'#$90 then
        error(streaderror,bzip2_bad_block_magic);
      decode_block:=false;
    end;
end;

procedure Tbzip2_decode_stream.new_block;

begin
  if decode_block then
    nextrle:=@tt^[tt^[block_origin] shr 8]
  else
    begin
      error(streaderror,bzip2_endoffile);
      nextrle:=nil;
    end;
end;

procedure Tbzip2_decode_stream.consume_rle;inline;

{Make nextrle point to the next decoded byte. If nextrle did point to the last
 byte in the current block, decode the next block.}

begin
{  Pcardinal(nextrle)^:=Pcardinal(nextrle)^ shr 8;}
  nextrle:=@tt^[Pcardinal(nextrle)^ shr 8];
  dec(decode_available);
  if decode_available=0 then
    new_block;
end;

procedure Tbzip2_decode_stream.rle_read(bufptr:Pbyte;var count:Longint);

var rle_len:cardinal;
    data:byte;

label rle_write;

begin
  rle_len:=rle_run_left;
  data:=rle_run_data;
  if block_randomized then
    {Not yet implemented.}
    runerror(212)
  else
    begin
      if rle_len<>0 then
        {Speed is important. Instead of an if statement within the
         repeat loop use a goto outside the loop.}
        goto rle_write;
      repeat
        if decode_available=0 then
          break;
        rle_len:=1;
        data:=nextrle^;
        consume_rle;
        if (decode_available>0) and (data=nextrle^) then
          begin
            inc(rle_len);
            consume_rle;
            if (decode_available>0) and (data=nextrle^) then
              begin
                inc(rle_len);
                consume_rle;
                if (decode_available>0) and (data=nextrle^) then
                  begin
                    consume_rle;
                    inc(rle_len,nextrle^+1);
                    consume_rle;
                  end;
              end;
          end;
rle_write:
        repeat
            bufptr^:=data;
            inc(bufptr);
            dec(count);
            dec(rle_len);
        until (rle_len=0) or (count=0);
      until count=0;
      short:=count;
    end;
  rle_run_data:=data;
  rle_run_left:=rle_len;
end;

procedure Tbzip2_decode_stream.read(var buf;count:Longint);

var bufptr:Pbyte;

begin
  short:=0;
  bufptr:=@buf;
  if decode_available=high(decode_available) then
    begin
      {Initialize the rle process:
        - Decode a block
        - Initialize pointer.}
      if not decode_block then
        begin
          error(streaderror,bzip2_endoffile);
          nextrle:=nil;
        end;
      nextrle:=@tt^[tt^[block_origin] shr 8];
    end;
  rle_read(bufptr,count);
end;

destructor Tbzip2_decode_stream.done;

begin
  if tt<>nil then
    freemem(tt,blocksize*100000*sizeof(cardinal));
  inherited done;
end;

end.

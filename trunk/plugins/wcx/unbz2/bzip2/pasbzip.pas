program pasbzip;

uses objects,bzip2;

var infile,outfile:Tbufstream;
    decoder:Tbzip2_decode_stream;
    a:array[1..4096] of byte;
    i,readsize:cardinal;

begin
  assign(output,'pasbzip.out');
  rewrite(output);
  if paramcount<>1 then
    writeln('Usage: pasbunzip <file>')
  else
    begin
      infile.init(paramstr(1),stopenread,4096);
      outfile.init('OUTFILE',stcreate,4096);
      decoder.init(@infile);
      if decoder.status<>stok then
        writeln('Fout: ',decoder.status,' ',decoder.errorinfo);
      repeat
        readsize:=4096;
        decoder.read(a,readsize);
        dec(readsize,decoder.short);
        outfile.write(a,readsize);
      until decoder.status<>0;
      if decoder.status<>stok then
        writeln('Fout: ',decoder.status,' ',decoder.errorinfo);
      decoder.done;
      infile.done;
      outfile.done;
    end;
    close(output);
end.

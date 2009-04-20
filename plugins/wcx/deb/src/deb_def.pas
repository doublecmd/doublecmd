unit deb_def;

interface

type
  deb_Header = record
    filename : String[16];
    time     : longint;
    size     : longint;
    pos      : longint;
    crc      : longint;
  end;

const
  size_deb_files= 60;
  size_deb_signature = 72;

implementation
end.

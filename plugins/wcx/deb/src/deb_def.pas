unit deb_def;

interface

type
  deb_Header = record
    filename : String;
    time     : longint;
    size     : longint;
    mode     : longint;
    pos      : longint;
  end;

const
  size_deb_files= 60;
  size_deb_signature = 72;

implementation
end.

unit uLocalFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uRealFileSource;

type

  ILocalFileSource = interface(IRealFileSource)
  end;

  {en
     Base for classes of local file sources.
     Empty placeholder for now, allows to check
     whether a certain file source is local.
  }
  TLocalFileSource = class(TRealFileSource, ILocalFileSource)
  end;

implementation

end.


unit uRealFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource;

type

  IRealFileSource = interface(IFileSource)
  end;

  {en
     Base class for any real file source
     (filesystem, archive, network, ... - all sources able to produce real files).
  }
  TRealFileSource = class(TFileSource, IRealFileSource)
  end;

implementation

end.


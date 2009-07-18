unit uRealFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource;

type

  {en
     Base class for any real file source
     (filesystem, archive, network, ... - all sources able to produce real files).
  }
  TRealFileSource = class(TFileSource)
  end;

implementation

end.


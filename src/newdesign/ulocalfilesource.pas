unit uLocalFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uRealFileSource;

type

  {en
     Base for classes of local file sources.
     Empty placeholder for now, allows to check
     whether a certain file source is local.
  }
  TLocalFileSource = class(TRealFileSource)
  end;

implementation

end.


unit uLocalFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource;

type

  {en
     Base for classes of local file sources.
     Empty placeholder for now, allows to check
     whether a certain file source is local.
  }
  TLocalFileSource = class(TFileSource)
  end;

implementation

end.


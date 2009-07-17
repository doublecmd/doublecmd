unit uFileSourceOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TFileSourceOperation = class
  public
    procedure Execute; virtual abstract;
  end;

implementation

end.


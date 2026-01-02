unit uLocalFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uRealFileSource;

type

  ILocalFileSource = interface(IRealFileSource)
    ['{22F03840-42C2-0E62-2CCE-677794F64598}']
    procedure AddSearchPath( paths: TStringList );
  end;

  {en
     Base for classes of local file sources.
     Empty placeholder for now, allows to check
     whether a certain file source is local.
  }
  
  { TLocalFileSource }

  TLocalFileSource = class(TRealFileSource, ILocalFileSource)
    procedure AddSearchPath( paths: TStringList ); virtual;
  end;

implementation

{ TLocalFileSource }

procedure TLocalFileSource.AddSearchPath( paths: TStringList );
begin
end;

end.


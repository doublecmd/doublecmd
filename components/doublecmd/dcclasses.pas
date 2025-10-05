unit DCClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, NullStream;

type

  { TNullStreamEx }

  TNullStreamEx = class(TNullStream)
  public
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

implementation

{ TNullStreamEx }

function TNullStreamEx.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result:= Count;
  inherited Write(Buffer, Count);
end;

end.


unit ufactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperationTypes,
  uFileSourceListOperation,
  uFileSource;

type

  TFactory = class//(IFactory)
  // Creates an operation object specific to the file source.
  class function GetListOperationObject(FileSource: TFileSource)
                             : TFileSourceListOperation;
  end;

implementation

class function TFactory.GetListOperationObject(FileSource: TFileSource)
                             : TFileSourceListOperation;
var
  Operation: TObject;
begin
  Result := nil;
  Operation := FileSource.GetOperation(fsoList);
  if Operation is TFileSourceListOperation then
    Result := (Operation as TFileSourceListOperation)
  else
    raise Exception.Create('Incorrect operation type created');
end;

end.


unit uGioExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uFileSourceExecuteOperation;

type

  { TGioExecuteOperation }

  TGioExecuteOperation = class(TFileSourceExecuteOperation)
  public
    procedure MainExecute; override;
  end;

implementation

uses
  uGio;

procedure TGioExecuteOperation.MainExecute;
var
  RemoteName: UTF8String;
  iResult: LongInt;
begin
  if GioOpen(AbsolutePath) then
    FExecuteOperationResult:= fseorSuccess
  else begin
    FExecuteOperationResult:= fseorError;
  end;
end;

end.


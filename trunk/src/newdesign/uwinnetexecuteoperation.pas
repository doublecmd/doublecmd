unit uWinNetExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceExecuteOperation;

type

  { TWinNetExecuteOperation }

  TWinNetExecuteOperation = class(TFileSourceExecuteOperation)
  public
    procedure MainExecute; override;
  end;

implementation

uses
  uDCUtils;

procedure TWinNetExecuteOperation.MainExecute;
begin
  FExecuteOperationResult:= fseorSymLink;
  SymLinkPath:= IncludeFrontPathDelimiter(ExecutableFile.FullPath);
end;

end.


unit uStashFileSourceOperation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uFileSource,
  uFileSourceListOperation, uFileSourceCopyOperation,
  uStashFilesBackend;

type

  { TStashListOperation }

  TStashListOperation = class( TFileSourceListOperation )
  public
    procedure MainExecute; override;
  end;

  { TStashCopyInOperation }

  TStashCopyInOperation = class( TFileSourceCopyInOperation )
  public
    procedure MainExecute; override;
  end;

implementation

{ TStashListOperation }

procedure TStashListOperation.MainExecute;
begin
  FFiles:= stashFilesBackend.toFiles;
end;

{ TStashCopyInOperation }

procedure TStashCopyInOperation.MainExecute;
begin
  stashFilesBackend.addPaths( SourceFiles );
end;

end.


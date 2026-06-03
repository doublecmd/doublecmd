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
var
  i: Integer;
  f: TFile;
begin
  for i:= 0 to SourceFiles.Count-1 do begin
    f:= SourceFiles[i];
    stashFilesBackend.addPath( f.FullPath );
  end;
end;

end.


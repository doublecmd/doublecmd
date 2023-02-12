unit uFlatViewFileSource;

{$mode objfpc}{$H+}

interface

uses
  uMultiListFileSource,
  uFileSourceOperation,
  uSearchResultFileSource;

type

  { TFlatViewFileSource }

  TFlatViewFileSource = class(TSearchResultFileSource)
  public
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetRootDir(sPath : String): String; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
  end;

implementation

{ TFlatViewFileSource }

function TFlatViewFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result:= True;
end;

function TFlatViewFileSource.GetRootDir(sPath: String): String;
begin
  Result:= FileSource.GetRootDir(sPath);
end;

function TFlatViewFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result:= True;
end;

end.


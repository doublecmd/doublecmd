unit uSearchResultFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uMultiListFileSource,
  uFileSourceOperation,
  uFileSourceProperty;

type

  ISearchResultFileSource = interface(IMultiListFileSource)
    ['{5076D4C2-3AB8-4029-9318-0AF115F7FDDD}']
  end;

  {en
     File source for search results.
  }

  { TSearchResultFileSource }

  TSearchResultFileSource = class(TMultiListFileSource, ISearchResultFileSource)
  public
    constructor Create; override;

    function GetProperties: TFileSourceProperties; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

    function GetLocalName(var aFile: TFile): Boolean; override;
  end;

implementation

uses
  uSearchResultListOperation;

constructor TSearchResultFileSource.Create;
begin
  FCurrentAddress := 'SearchResult';
  inherited Create;
end;

function TSearchResultFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspLinksToLocalFiles];
end;

function TSearchResultFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  // Only Root dir allowed (for flat mode).
  Result := IsPathAtRoot(NewDir);
end;

function TSearchResultFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := TSearchResultListOperation.Create(Self, TargetPath);
end;

function TSearchResultFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  if (fspLinksToLocalFiles in FileSource.Properties) then
    Result:= FileSource.GetLocalName(aFile)
  else
    Result:= True;
end;

end.


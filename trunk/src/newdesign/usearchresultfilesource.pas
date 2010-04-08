unit uSearchResultFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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
  TSearchResultFileSource = class(TMultiListFileSource, ISearchResultFileSource)
  public
    constructor Create; override;

    function GetProperties: TFileSourceProperties; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
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

end.


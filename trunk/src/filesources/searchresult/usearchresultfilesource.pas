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

    function GetRootDir(sPath : String): String; override;
    function GetProperties: TFileSourceProperties; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    class function CreateFile(const APath: String): TFile; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

    function GetLocalName(var aFile: TFile): Boolean; override;
  end;

implementation

uses
  uFileSystemFileSource, uSearchResultListOperation, uLng;

function TSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:=  PathDelim + PathDelim + PathDelim + rsSearchResult + PathDelim;
end;

function TSearchResultFileSource.GetProperties: TFileSourceProperties;
begin
  Result := inherited GetProperties + [fspLinksToLocalFiles] - [fspNoneParent, fspListFlatView];
end;

function TSearchResultFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  // Only Root dir allowed (for flat mode).
  Result := IsPathAtRoot(NewDir);
end;

class function TSearchResultFileSource.CreateFile(const APath: String): TFile;
begin
  Result:= TFileSystemFileSource.CreateFile(APath);
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


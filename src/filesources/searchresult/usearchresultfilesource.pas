unit uSearchResultFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource, uFileSourceManager,
  uMultiListFileSource,
  uFileSourceOperationTypes,
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
    function GetProcessor: TFileSourceProcessor; override;

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

type

  { TSearchResultFileSourceProcessor }

  TSearchResultFileSourceProcessor = class( TDefaultFileSourceProcessor )
  private
    procedure consultMoveOperation( var params: TFileSourceConsultParams );
  public
    procedure consultOperation( var params: TFileSourceConsultParams ); override;
  end;

var
  searchResultFileSourceProcessor: TSearchResultFileSourceProcessor;

procedure TSearchResultFileSourceProcessor.consultMoveOperation( var params: TFileSourceConsultParams);
var
  searchResultFS: ISearchResultFileSource;
begin
  if params.currentFS <> params.sourceFS then
    Exit;

  searchResultFS:= params.currentFS as ISearchResultFileSource;
  params.sourceFS:= searchResultFS.FileSource;
end;

procedure TSearchResultFileSourceProcessor.consultOperation( var params: TFileSourceConsultParams);
begin
  case params.operationType of
    fsoMove:
      self.consultMoveOperation( params );
  end;
  Inherited;
end;

function TSearchResultFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= searchResultFileSourceProcessor;
end;

function TSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:=  PathDelim + PathDelim + PathDelim + rsSearchResult + PathDelim;
end;

function TSearchResultFileSource.GetProperties: TFileSourceProperties;
begin
  Result := inherited GetProperties - [fspNoneParent, fspListFlatView];
  if (fspDirectAccess in Result) then Result+= [fspLinksToLocalFiles];
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

initialization
  searchResultFileSourceProcessor:= TSearchResultFileSourceProcessor.Create;

finalization
  FreeAndNil( searchResultFileSourceProcessor );

end.


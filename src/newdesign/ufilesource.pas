unit uFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSourceProperty,
  uFileProperty,
  uFile;

type

  TFileSource = class(TObject)

  private

  protected
    FCurrentPath: String;    // Always includes trailing path delimiter.
    FCurrentAddress: String;

    {en
       Retrieves the full address of the file source
       (the CurrentPath is relative to this).
       This may be used for specifying address:
       - archive : path to archive
       - network : address of server
       etc.
    }
    function GetCurrentAddress: String; virtual;
    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;

    {en
       Returns all the properties supported by the file type of the given file source.
    }
    class function GetSupportedFileProperties: TFilePropertiesTypes; virtual abstract;

  public
    constructor Create; virtual;

    function Clone: TFileSource; virtual;
    procedure CloneTo(FileSource: TFileSource); virtual;

    // Retrieve operations permitted on the source.  = capabilities?
    class function GetOperationsTypes: TFileSourceOperationTypes; virtual abstract;

    // Returns a list of property types supported by this source for each file.
    class function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; virtual abstract;

    // Retrieve some properties of the file source.
    class function GetProperties: TFileSourceProperties; virtual abstract;

    // Retrieves a list of files.
    // This is the same as GetOperation(fsoList), executing it
    // and returning the result of Operation.ReleaseFiles.
    // Caller is responsible for freeing the result list.
    function GetFiles: TFiles; virtual abstract;

    // These functions create an operation object specific to the file source.
    function CreateListOperation: TFileSourceOperation; virtual abstract;
    function CreateCopyInOperation(SourceFileSource: TFileSource;
                                   SourceFiles: TFiles;
                                   TargetPath: String;
                                   RenameMask: String): TFileSourceOperation; virtual abstract;
    function CreateCopyOutOperation(TargetFileSource: TFileSource;
                                    SourceFiles: TFiles;
                                    TargetPath: String;
                                    RenameMask: String): TFileSourceOperation; virtual abstract;

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property CurrentAddress: String read GetCurrentAddress;
    property Properties: TFileSourceProperties read GetProperties;
    property SupportedFileProperties: TFilePropertiesTypes read GetSupportedFileProperties;

  end;

implementation

constructor TFileSource.Create;
begin
  if ClassType = TFileSource then
    raise Exception.Create('Cannot construct abstract class');
  inherited Create;
end;

function TFileSource.Clone: TFileSource;
begin
  Result := TFileSource.Create;
  CloneTo(Result);
end;

procedure TFileSource.CloneTo(FileSource: TFileSource);
begin
  if Assigned(FileSource) then
  begin
    FileSource.FCurrentPath := FCurrentPath;
    FileSource.FCurrentAddress := FCurrentAddress;
  end;
end;

function TFileSource.GetCurrentAddress: String;
begin
  Result := FCurrentAddress;
end;

function TFileSource.GetCurrentPath: String;
begin
  Result := FCurrentPath;
end;

procedure TFileSource.SetCurrentPath(NewPath: String);
begin
  if NewPath = '' then
    FCurrentPath := ''
  else
    FCurrentPath := IncludeTrailingPathDelimiter(NewPath);
end;

end.


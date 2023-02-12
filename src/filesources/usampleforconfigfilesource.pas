unit uSampleForConfigFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uLocalFileSource,
  uFileSourceOperation,
  uFileSourceProperty,
  uFileSourceOperationTypes,
  uFileProperty;

const
  SAMPLE_PATH = PathDelim+PathDelim+'DoubleCommander'+PathDelim;

type

  ISampleForConfigFileSource = interface(ILocalFileSource)
    ['{C7D75C6D-38B6-4038-B3C4-4BB200A6FF28}']
  end;

  {en
     File source for configuration purpose, just fake files.
  }

  { TSampleForConfigFileSource }

  TSampleForConfigFileSource = class(TLocalFileSource, ISampleForConfigFileSource)
  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    function GetRootDir(sPath : String): String; override;
    function GetProperties: TFileSourceProperties; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    class function CreateFile(const APath: String): TFile; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

    function GetLocalName(var aFile: TFile): Boolean; override;
  end;

implementation

uses
  uFileSystemFileSource, uSampleForConfigListOperation, uLng;

function TSampleForConfigFileSource.GetRootDir(sPath: String): String;
begin
  Result:=sPath;
end;

function TSampleForConfigFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual];
end;

function TSampleForConfigFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result := true;
end;

function TSampleForConfigFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := TSampleForConfigListOperation.Create(Self, TargetPath);
end;

function TSampleForConfigFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  Result:= True;
end;

function TSampleForConfigFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList];
end;

function TSampleForConfigFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [fpName, fpSize];
end;

class function TSampleForConfigFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    CreationTimeProperty := TFileCreationDateTimeProperty.Create;
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create;
    LinkProperty := TFileLinkProperty.Create;
    OwnerProperty := TFileOwnerProperty.Create;
    TypeProperty := TFileTypeProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;


end.


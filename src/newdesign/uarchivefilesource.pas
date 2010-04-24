unit uArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLocalFileSource,
  uFileSource,
  uFile,
  uFileProperty;

type

  IArchiveFileSource = interface(ILocalFileSource)
    ['{13A8637C-FFDF-46B0-B5B4-E7C6851C157A}']

    function GetArchiveFileSource: IFileSource;

    {en
       Full path to the archive on the ArchiveFileSource.
    }
    property ArchiveFileName: String read GetCurrentAddress;
    {en
       File source that has the archive on it.
       It should be direct-access file source (usually filesystem).
    }
    property ArchiveFileSource: IFileSource read GetArchiveFileSource;
  end;

  TArchiveFileSource = class(TLocalFileSource, IArchiveFileSource)

  private
    FArchiveFileSource: IFileSource; //en> File source that has the archive.

  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;

    function GetArchiveFileSource: IFileSource;

  public
    constructor Create(anArchiveFileSource: IFileSource;
                       anArchiveFileName: String); virtual reintroduce overload;

    class function CreateFile(const APath: String): TFile; override;

    property ArchiveFileName: String read GetCurrentAddress;
  end;

implementation

constructor TArchiveFileSource.Create(anArchiveFileSource: IFileSource;
                                      anArchiveFileName: String);
begin
  FCurrentAddress := anArchiveFileName;
  FArchiveFileSource := anArchiveFileSource;
  inherited Create;
end;

class function TArchiveFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    SizeProperty := TFileSizeProperty.Create;
    CompressedSizeProperty := TFileCompressedSizeProperty.Create;
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
  end;
end;

function TArchiveFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize, fpCompressedSize, fpAttributes, fpModificationTime];
end;

function TArchiveFileSource.GetArchiveFileSource: IFileSource;
begin
  Result := FArchiveFileSource;
end;

end.

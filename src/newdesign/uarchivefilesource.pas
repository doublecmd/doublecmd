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

    {en
       Full path to the archive on the ParentFileSource.
    }
    property ArchiveFileName: String read GetCurrentAddress;

  end;

  TArchiveFileSource = class(TLocalFileSource, IArchiveFileSource)

  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    {en
      Creates an archive file source.

      @param(anArchiveFileSource
             File source that stores the archive.
             Usually it will be direct-access file source, like filesystem.)
      @param(anArchiveFileName
             Full path to the archive on the ArchiveFileSource.)
    }
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
  inherited Create;
  ParentFileSource := anArchiveFileSource;
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

end.

unit uArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uLocalFileSource,
  uFile,
  uFileProperty;

type

  IArchiveFileSource = interface(ILocalFileSource)
    ['{13A8637C-FFDF-46B0-B5B4-E7C6851C157A}']

    property ArchiveFileName: String read GetCurrentAddress;
  end;

  TArchiveFileSource = class(TLocalFileSource, IArchiveFileSource)

  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    constructor Create(anArchiveFileName: String); virtual reintroduce overload;
    constructor Create(anArchiveFileName: String; aPath: String); virtual reintroduce overload;

    class function CreateFile(const APath: String): TFile; override;

    property ArchiveFileName: String read GetCurrentAddress;
  end;

implementation

constructor TArchiveFileSource.Create(anArchiveFileName: String);
begin
  Create(anArchiveFileName, PathDelim);
end;

constructor TArchiveFileSource.Create(anArchiveFileName: String; aPath: String);
begin
  FCurrentAddress := anArchiveFileName;
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

end.


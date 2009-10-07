unit uWcxArchiveFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uArchiveFile, uWcxArchiveFileSource, uFileProperty, uWCXmodule;

type

  TWcxArchiveFile = class(TArchiveFile)
  public
    constructor Create(WcxHeader: TWCXHeader); overload;

    class function GetSupportedProperties: TFilePropertiesTypes; override;
  end;

implementation

constructor TWcxArchiveFile.Create(WcxHeader: TWCXHeader);
begin
  inherited Create;
{
    FileCRC,
    CompressionMethod,
    Comment,
}
  Size := WcxHeader.UnpSize;
  CompressedSize := WcxHeader.PackSize;
  Attributes := {TNtfsFileAttributesProperty or Unix?} WcxHeader.FileAttr;
  try
    ModificationTime := FileDateToDateTime(WcxHeader.FileTime);
  except
    ModificationTime := 0;
  end;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := ExtractFileName(WcxHeader.FileName);
end;

class function TWcxArchiveFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedProperties;
end;

end.


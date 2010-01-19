unit uWcxArchiveFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uArchiveFile, uFileProperty, uWCXmodule;

type

  TWcxArchiveFile = class(TArchiveFile)
  public
    constructor Create(const APath: String; WcxHeader: TWCXHeader); overload;

    class function GetSupportedProperties: TFilePropertiesTypes; override;
  end;

  { TWcxArchiveFiles }

  TWcxArchiveFiles = class(TFiles)
  public
    function CreateObjectOfSameType(const APath: String): TFiles; override;
    function CreateFileObject(const APath: String): TFile; override;
    function Clone: TWcxArchiveFiles; override;
  end;

implementation

constructor TWcxArchiveFile.Create(const APath: String; WcxHeader: TWCXHeader);
begin
  inherited Create(APath);
{
    FileCRC,
    CompressionMethod,
    Comment,
}
  Size := WcxHeader.UnpSize;
  CompressedSize := WcxHeader.PackSize;
  Attributes := {TNtfsFileAttributesProperty or Unix?} WcxHeader.FileAttr;
  try
    ModificationTime := WcxFileTimeToDateTime(WcxHeader);
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

{ TWcxArchiveFiles }

function TWcxArchiveFiles.CreateObjectOfSameType(const APath: String): TFiles;
begin
  Result:= TWcxArchiveFiles.Create(APath);
end;

function TWcxArchiveFiles.CreateFileObject(const APath: String): TFile;
begin
  Result:= TWcxArchiveFile.Create(APath);
end;

function TWcxArchiveFiles.Clone: TWcxArchiveFiles;
begin
  Result:= TWcxArchiveFiles.Create(Path);
  CloneTo(Result);
end;

end.


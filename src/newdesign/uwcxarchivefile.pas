unit uWcxArchiveFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uArchiveFile, uFileProperty, uWCXmodule;

type

  TWcxArchiveFile = class(TArchiveFile)
  public
    constructor Create(WcxHeader: TWCXHeader); overload;

    class function GetSupportedProperties: TFilePropertiesTypes; override;
  end;

  { TWcxArchiveFiles }

  TWcxArchiveFiles = class(TFiles)
  public
    function CreateObjectOfSameType: TFiles; override;
    function CreateFileObject: TFile; override;
    function Clone: TWcxArchiveFiles; override;
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

function TWcxArchiveFiles.CreateObjectOfSameType: TFiles;
begin
  Result:= TWcxArchiveFiles.Create;
end;

function TWcxArchiveFiles.CreateFileObject: TFile;
begin
  Result:= TWcxArchiveFile.Create;
end;

function TWcxArchiveFiles.Clone: TWcxArchiveFiles;
begin
  Result:= TWcxArchiveFiles.Create;
  CloneTo(Result);
end;

end.


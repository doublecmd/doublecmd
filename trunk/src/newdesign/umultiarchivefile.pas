unit uMultiArchiveFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile, uArchiveFile, uFileProperty, uMultiArc;

type

  { TMultiArchiveFile }

  TMultiArchiveFile = class(TArchiveFile)
  public
    constructor Create(const APath: String; ArchiveItem: TArchiveItem); overload;

    class function GetSupportedProperties: TFilePropertiesTypes; override;
  end;

  { TMultiArchiveFiles }

  TMultiArchiveFiles = class(TFiles)
  public
    function CreateObjectOfSameType(const APath: String): TFiles; override;
    function CreateFileObject(const APath: String): TFile; override;
    function Clone: TMultiArchiveFiles; override;
  end;

implementation

uses
  DateUtils;

constructor TMultiArchiveFile.Create(const APath: String; ArchiveItem: TArchiveItem);
begin
  inherited Create(APath);
{
    Comment,
}
  Size := ArchiveItem.UnpSize;
  CompressedSize := ArchiveItem.PackSize;
  Attributes := {TNtfsFileAttributesProperty or Unix?} ArchiveItem.Attributes;
  try
    with ArchiveItem do
    ModificationTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
  except
    ModificationTime := 0;
  end;

  // Set name after assigning Attributes property, because it is used to get extension.
  Name := ExtractFileName(ArchiveItem.FileName);
  if ArchiveItem.FileExt <> EmptyStr then
    Name:= Name + '.' + ArchiveItem.FileExt;
end;

class function TMultiArchiveFile.GetSupportedProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedProperties;
end;

{ TMultiArchiveFiles }

function TMultiArchiveFiles.CreateObjectOfSameType(const APath: String): TFiles;
begin
  Result:= TMultiArchiveFiles.Create(APath);
end;

function TMultiArchiveFiles.CreateFileObject(const APath: String): TFile;
begin
  Result:= TMultiArchiveFile.Create(APath);
end;

function TMultiArchiveFiles.Clone: TMultiArchiveFiles;
begin
  Result:= TMultiArchiveFiles.Create(Path);
  CloneTo(Result);
end;

end.


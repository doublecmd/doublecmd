unit uInfoToolTip;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  uFile, uFileSource;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

implementation

uses
  uLng, uFileSourceProperty, uFileProperty
{$IF DEFINED(MSWINDOWS)}
  , uShlObjAdditional
{$ENDIF}
  ;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;
begin
  Result:= EmptyStr;
  {$IF DEFINED(MSWINDOWS)}
  if fspDirectAccess in aFileSource.Properties then
    Result:= SHGetInfoTip(aFile.Path, aFile.Name)
  else
  {$ENDIF}
    begin
      if fpModificationTime in aFile.SupportedProperties then
        Result:= rsColDate + #32 +  (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).AsString;
      if fpSize in aFile.SupportedProperties then
        Result:= LineEnding + rsColSize + #32 +  (aFile.Properties[fpSize] as TFileSizeProperty).AsString;
      if fpCompressedSize in aFile.SupportedProperties then
        Result:= LineEnding + rsColSize + #32 +  (aFile.Properties[fpCompressedSize] as TFileCompressedSizeProperty).AsString;
    end;
end;

end.


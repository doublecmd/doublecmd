unit uInfoToolTip;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  uFile, uFileSource;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

implementation

uses
  StrUtils, uFileProperty
{$IF DEFINED(MSWINDOWS)}
  , uShlObjAdditional, uFileSourceProperty
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
        with (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty) do
        Result:= GetDescription + #58#32 +  AsString;
      if fpSize in aFile.SupportedProperties then
        with (aFile.Properties[fpSize] as TFileSizeProperty) do
        Result:= IfThen(Result=EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 + AsString;
      if fpCompressedSize in aFile.SupportedProperties then
        with (aFile.Properties[fpCompressedSize] as TFileCompressedSizeProperty) do
        Result:= IfThen(Result=EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 + AsString;
    end;
end;

end.

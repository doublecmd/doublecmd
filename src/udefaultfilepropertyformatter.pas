unit uDefaultFilePropertyFormatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileProperty;

type

  TDefaultFilePropertyFormatter = class(TInterfacedObject, IFilePropertyFormatter)

  public
    function FormatFileName(FileProperty: TFileNameProperty): String;
    function FormatFileSize(FileProperty: TFileSizeProperty): String;
    function FormatDateTime(FileProperty: TFileDateTimeProperty): String;
    function FormatModificationDateTime(FileProperty: TFileModificationDateTimeProperty): String;
    function FormatNtfsAttributes(FileProperty: TNtfsFileAttributesProperty): String;
    function FormatUnixAttributes(FileProperty: TUnixFileAttributesProperty): String;

  end;

  TMaxDetailsFilePropertyFormatter = class(TInterfacedObject, IFilePropertyFormatter)

  public
    function FormatFileName(FileProperty: TFileNameProperty): String;
    function FormatFileSize(FileProperty: TFileSizeProperty): String;
    function FormatDateTime(FileProperty: TFileDateTimeProperty): String;
    function FormatModificationDateTime(FileProperty: TFileModificationDateTimeProperty): String;
    function FormatNtfsAttributes(FileProperty: TNtfsFileAttributesProperty): String;
    function FormatUnixAttributes(FileProperty: TUnixFileAttributesProperty): String;

  end;

var
  DefaultFilePropertyFormatter: IFilePropertyFormatter = nil;
  MaxDetailsFilePropertyFormatter: IFilePropertyFormatter = nil;

implementation

uses
  uGlobs, uDCUtils, DCBasicTypes, DCFileAttributes, DCDateTimeUtils;

function TDefaultFilePropertyFormatter.FormatFileName(
           FileProperty: TFileNameProperty): String;
begin
  Result := FileProperty.Value;
end;

function TDefaultFilePropertyFormatter.FormatFileSize(
           FileProperty: TFileSizeProperty): String;
begin
  Result := cnvFormatFileSize(FileProperty.Value);
end;

function TDefaultFilePropertyFormatter.FormatDateTime(
            FileProperty: TFileDateTimeProperty): String;
begin
  Result := SysUtils.FormatDateTime(gDateTimeFormat, FileProperty.Value);
end;

function TDefaultFilePropertyFormatter.FormatModificationDateTime(
           FileProperty: TFileModificationDateTimeProperty): String;
begin
  Result := FormatDateTime(FileProperty);
end;

function TDefaultFilePropertyFormatter.FormatNtfsAttributes(FileProperty: TNtfsFileAttributesProperty): String;
{
  Format as decimal:
begin
  Result := IntToStr(FileProperty.Value);
end;
}
begin
  Result:= DCFileAttributes.FormatNtfsAttributes(FileProperty.Value);
end;

function TDefaultFilePropertyFormatter.FormatUnixAttributes(FileProperty: TUnixFileAttributesProperty): String;
begin
  Result:= DCFileAttributes.FormatUnixAttributes(FileProperty.Value);
end;

// ----------------------------------------------------------------------------

function TMaxDetailsFilePropertyFormatter.FormatFileName(
           FileProperty: TFileNameProperty): String;
begin
  Result := FileProperty.Value;
end;

function TMaxDetailsFilePropertyFormatter.FormatFileSize(
           FileProperty: TFileSizeProperty): String;
var
  d: Double;
begin
  d := FileProperty.Value;
  Result := Format('%.0n', [d]);
end;

function TMaxDetailsFilePropertyFormatter.FormatDateTime(
            FileProperty: TFileDateTimeProperty): String;
var
  Bias: LongInt = 0;
  Sign: String;
begin
  Bias := -GetTimeZoneBias;

  if Bias >= 0 then
    Sign := '+'
  else
    Sign := '-';

  Result := SysUtils.FormatDateTime('ddd, dd mmmm yyyy hh:nn:ss', FileProperty.Value)
          + ' UT' + Sign
          + Format('%.2D%.2D', [Bias div 60, Bias mod 60]);
end;

function TMaxDetailsFilePropertyFormatter.FormatModificationDateTime(
           FileProperty: TFileModificationDateTimeProperty): String;
begin
  Result := FormatDateTime(FileProperty);
end;

function TMaxDetailsFilePropertyFormatter.FormatNtfsAttributes(FileProperty: TNtfsFileAttributesProperty): String;
begin
  Result := DefaultFilePropertyFormatter.FormatNtfsAttributes(FileProperty);
end;

function TMaxDetailsFilePropertyFormatter.FormatUnixAttributes(FileProperty: TUnixFileAttributesProperty): String;
begin
  Result := DefaultFilePropertyFormatter.FormatUnixAttributes(FileProperty);
end;

initialization

  DefaultFilePropertyFormatter := TDefaultFilePropertyFormatter.Create as IFilePropertyFormatter;
  MaxDetailsFilePropertyFormatter := TMaxDetailsFilePropertyFormatter.Create as IFilePropertyFormatter;

finalization

  DefaultFilePropertyFormatter := nil; // frees the interface
  MaxDetailsFilePropertyFormatter := nil;

end.


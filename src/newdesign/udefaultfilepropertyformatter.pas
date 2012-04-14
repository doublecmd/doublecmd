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
var
  iAttr: TFileAttrs;
begin
  iAttr := FileProperty.Value;

  Result:= '--------';

  if (iAttr and FILE_ATTRIBUTE_DIRECTORY    ) <> 0 then Result[1] := 'd';
  if (iAttr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then Result[1] := 'l';
  if (iAttr and FILE_ATTRIBUTE_READONLY     ) <> 0 then Result[2] := 'r';
  if (iAttr and FILE_ATTRIBUTE_ARCHIVE      ) <> 0 then Result[3] := 'a';
  if (iAttr and FILE_ATTRIBUTE_HIDDEN       ) <> 0 then Result[4] := 'h';
  if (iAttr and FILE_ATTRIBUTE_SYSTEM       ) <> 0 then Result[5] := 's';

  // These two are exclusive on NTFS.
  if (iAttr and FILE_ATTRIBUTE_COMPRESSED   ) <> 0 then Result[6] := 'c';
  if (iAttr and FILE_ATTRIBUTE_ENCRYPTED    ) <> 0 then Result[6] := 'e';

  if (iAttr and FILE_ATTRIBUTE_TEMPORARY    ) <> 0 then Result[7] := 't';
  if (iAttr and FILE_ATTRIBUTE_SPARSE_FILE  ) <> 0 then Result[8] := 'p';
end;

function TDefaultFilePropertyFormatter.FormatUnixAttributes(FileProperty: TUnixFileAttributesProperty): String;
var
  iAttr: TFileAttrs;
begin
  iAttr := FileProperty.Value;

  Result:= '----------';

  if ((iAttr and S_IFMT) = S_IFDIR)  then Result[1]  := 'd';
  if ((iAttr and S_IFMT) = S_IFLNK)  then Result[1]  := 'l';
  if ((iAttr and S_IFMT) = S_IFSOCK) then Result[1]  := 's';
  if ((iAttr and S_IFMT) = S_IFIFO)  then Result[1]  := 'f';
  if ((iAttr and S_IFMT) = S_IFBLK)  then Result[1]  := 'b';
  if ((iAttr and S_IFMT) = S_IFCHR)  then Result[1]  := 'c';

  if ((iAttr and S_IRUSR) = S_IRUSR) then Result[2]  := 'r';
  if ((iAttr and S_IWUSR) = S_IWUSR) then Result[3]  := 'w';
  if ((iAttr and S_IXUSR) = S_IXUSR) then Result[4]  := 'x';
  if ((iAttr and S_IRGRP) = S_IRGRP) then Result[5]  := 'r';
  if ((iAttr and S_IWGRP) = S_IWGRP) then Result[6]  := 'w';
  if ((iAttr and S_IXGRP) = S_IXGRP) then Result[7]  := 'x';
  if ((iAttr and S_IROTH) = S_IROTH) then Result[8]  := 'r';
  if ((iAttr and S_IWOTH) = S_IWOTH) then Result[9]  := 'w';
  if ((iAttr and S_IXOTH) = S_IXOTH) then Result[10] := 'x';

  if ((iAttr and S_ISUID) = S_ISUID) then Result[4]  := 's';
  if ((iAttr and S_ISGID) = S_ISGID) then Result[7]  := 's';
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


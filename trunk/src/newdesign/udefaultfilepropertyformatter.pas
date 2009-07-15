unit uDefaultFilePropertyFormatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileProperty;

type

  TDefaultFilePropertyFormatter = class(TInterfacedObject, IFilePropertyFormatter)

  public
    function FormatFileSize(FileProperty: TFileSizeProperty): String;
    function FormatDateTime(FileProperty: TFileDateTimeProperty): String;
    function FormatModificationDateTime(FileProperty: TFileModificationDateTimeProperty): String;
    function FormatAttributes(FileProperty: TFileAttributesProperty): String;

  end;

var
  DefaultFilePropertyFormatter: IFilePropertyFormatter = nil;

implementation

uses
  uGlobs, uDCUtils, LCLProc, uOSUtils
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF}
{$IFDEF UNIX}
  , BaseUnix, Unix
{$ENDIF}
  ;

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

function TDefaultFilePropertyFormatter.FormatAttributes(FileProperty: TFileAttributesProperty): String;
{
  Format as decimal:
begin
  Result := IntToStr(FileProperty.Value);
end;
}
var
  iAttr: Cardinal;
begin
  iAttr := FileProperty.Value;

{$IFDEF MSWINDOWS}
  Result:= '--------';

  if FPS_ISDIR(iAttr) then Result[1]:='d';
  if FPS_ISLNK(iAttr) then Result[1]:='l';

  if (iAttr and FILE_ATTRIBUTE_READONLY   ) <> 0 then Result[2] := 'r';
  if (iAttr and FILE_ATTRIBUTE_ARCHIVE    ) <> 0 then Result[3] := 'a';
  if (iAttr and FILE_ATTRIBUTE_HIDDEN     ) <> 0 then Result[4] := 'h';
  if (iAttr and FILE_ATTRIBUTE_SYSTEM     ) <> 0 then Result[5] := 's';

  // These two are exclusive on NTFS.
  if (iAttr and FILE_ATTRIBUTE_COMPRESSED ) <> 0 then Result[6] := 'c';
  if (iAttr and FILE_ATTRIBUTE_ENCRYPTED  ) <> 0 then Result[6] := 'e';

  if (iAttr and FILE_ATTRIBUTE_TEMPORARY  ) <> 0 then Result[7] := 't';
  if (iAttr and FILE_ATTRIBUTE_SPARSE_FILE) <> 0 then Result[8] := 'p';
{$ELSE}
  Result:= '----------';

  if FPS_ISDIR(iAttr) then Result[1]:='d';
  if FPS_ISLNK(iAttr) then Result[1]:='l';
  if FPS_ISSOCK(iAttr) then Result[1]:='s';
  if FPS_ISFIFO(iAttr) then Result[1]:='f';
  if FPS_ISBLK(iAttr) then Result[1]:='b';
  if FPS_ISCHR(iAttr) then Result[1]:='c';

  if ((iAttr AND S_IRUSR) = S_IRUSR) then Result[2]  := 'r';
  if ((iAttr AND S_IWUSR) = S_IWUSR) then Result[3]  := 'w';
  if ((iAttr AND S_IXUSR) = S_IXUSR) then Result[4]  := 'x';
  if ((iAttr AND S_IRGRP) = S_IRGRP) then Result[5]  := 'r';
  if ((iAttr AND S_IWGRP) = S_IWGRP) then Result[6]  := 'w';
  if ((iAttr AND S_IXGRP) = S_IXGRP) then Result[7]  := 'x';
  if ((iAttr AND S_IROTH) = S_IROTH) then Result[8]  := 'r';
  if ((iAttr AND S_IWOTH) = S_IWOTH) then Result[9]  := 'w';
  if ((iAttr AND S_IXOTH) = S_IXOTH) then Result[10] := 'x';

  if ((iAttr AND S_ISUID) = S_ISUID) then Result[4]  := 's';
  if ((iAttr AND S_ISGID) = S_ISGID) then Result[7]  := 's';
{$ENDIF}
end;

initialization

  DefaultFilePropertyFormatter := TDefaultFilePropertyFormatter.Create as IFilePropertyFormatter;

finalization

  DefaultFilePropertyFormatter := nil; // frees the interface

end.


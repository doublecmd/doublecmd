unit rpm_wdx_intf;

{$mode delphi}
{$include calling.inc}

interface
procedure ContentGetDetectString(DetectString:pchar; maxlen:integer); dcpcall;
function ContentGetSupportedField(FieldIndex:integer; FieldName:pchar;
  Units:pchar;maxlen:integer):integer; dcpcall;
function ContentGetValue(FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte;
  maxlen,flags:integer):integer; dcpcall;

implementation

uses SysUtils, WdxPlugin, rpm_io, rpm_def;

const
  IDX_PACKAGE    = 0;
  IDX_VERSION    = 1;
  IDX_RELEASE    = 2;
  IDX_DISTRIBUTION = 3;
  IDX_VENDER      = 4;
  IDX_LICENSE     = 5;
  IDX_PACKAGER    = 6;
  IDX_GROUP       = 7;
  IDX_OS          = 8;
  IDX_ARCH        = 9;
  IDX_SOURCE_RPM  = 10;
  IDX_SUMMARY     = 11;
  IDX_DESCRIPTION = 12;

  FIELDS_COUNT = 13;

//  IDX_BUILDTIME   ,
//  IDX_ARCHIVE_SIZE


var
  CurrentPackageFile: String;
  FileInfoCache : RPM_InfoRec;    //cache

procedure ContentGetDetectString(DetectString:pchar; maxlen:integer);
begin
  StrPCopy(DetectString, 'EXT="RPM"');
end;

function ContentGetSupportedField(FieldIndex:integer;  FieldName:pchar;
  Units:pchar;maxlen:integer):integer;
var
  Field: String;
begin
  StrPCopy(Units, '');

//  if FieldIndex =IDX_ARCHIVE_SIZE then
//  begin
//    StrPCopy(FieldName, FieldList.Strings[FieldIndex]);
//    StrPCopy(Units, 'bytes|kbytes|Mbytes|Gbytes'+#0);
//    Result := FT_NUMERIC_64;
//    exit;
//  end
//  else
  if FieldIndex >= FIELDS_COUNT then
  begin
      Result := FT_NOMOREFIELDS;
      exit;
  end;

  Result := FT_STRING;
  case FieldIndex of
      IDX_PACKAGE:     Field := 'Package';
      IDX_VERSION:     Field := 'Version';
      IDX_RELEASE:     Field := 'Release';
      IDX_DISTRIBUTION:Field := 'Distribution';
      IDX_VENDER:      Field := 'Vender';
      IDX_LICENSE:     Field := 'License';
      IDX_PACKAGER:    Field := 'Packager';
      IDX_GROUP:       Field := 'Group';
      IDX_OS:          Field := 'OS';
      IDX_ARCH:        Field := 'Arch';
      IDX_SOURCE_RPM:  Field := 'Source-RPM';
      IDX_SUMMARY:     Field := 'Summary';
      IDX_DESCRIPTION: Field := 'Description';
//      IDX_BUILD_TIME:  Field := 'Build-Time';

//      IDX_ARCHIVE_SIZE:
//              begin
//                Field := 'Archive-Size';
//                StrPCopy(FieldName, Field);
//                StrPCopy(Units, 'bytes|kbytes|Mbytes|Gbytes'+#0);
//                Result := FT_NUMERIC_64;
//              exit;
  end;

  StrPCopy(FieldName, Field);
end;

function ContentGetValue(FileName:pchar; FieldIndex,UnitIndex:integer; FieldValue:pbyte;
  maxlen,flags:integer):integer;
    function ReadRPMInfo(filename: String): integer;
    var
      fh: integer;
      fh_file: file;
      r_lead: RPM_Lead;
      signature, r_header: RPM_Header;
      //r_info: RPM_InfoRec;
    begin
      Result := -1;
      fh := FileOpen(filename, fmOpenRead or fmShareDenyNone);
      if fh=-1 then exit;

      AssignFile(fh_file, filename);
      try
        FileMode := 0;
        Reset(fh_file, 1);
        if IOResult <> 0 then exit;

        RPM_ReadLead(fh_file, r_lead);
        if r_lead.magic <> RPM_MAGIC then exit;

        if not RPM_ReadSignature(fh_file, r_lead.signature_type, signature) then exit;

        if not RPM_ReadHeader(fh_file, false, r_header, FileInfoCache)  then exit;

        Result := 0;
      finally
        CloseFile(fh_file);

        FileClose(fh); //oppsition to FileOpen
      end;
    end;

  function EnsureLength(S: string; nMaxlen: integer): string;
  begin
      Result := S;
      if length(Result)>=nMaxlen then
      begin
          Result := Copy(Result, 1, nMaxlen-4);
          Result := Result + '...';
      end;
  end;
var
  Value : String;
begin
  Result := FT_FILEERROR;
  if not FileExists(FileName) then exit;

  if CurrentPackageFile<>FileName then
  begin
     if ReadRPMInfo(FileName) <0 then exit;
     CurrentPackageFile := FileName;
  end
{$IFDEF GDEBUG}
  else
      SendDebug('Cached info reused for '+FileName);
{$ENDIF};


  if (FieldIndex>=FIELDS_COUNT) then
  begin
     Result := FT_NOSUCHFIELD;
     exit;
  end;

  Result := FT_STRING;
  case FieldIndex of
      IDX_PACKAGE:       Value := FileInfoCache.name;
      IDX_VERSION:    Value := FileInfoCache.version;
      IDX_RELEASE:    Value := FileInfoCache.release;
      IDX_DISTRIBUTION: Value := FileInfoCache.distribution;
      IDX_VENDER:     Value := FileInfoCache.version;
      IDX_LICENSE:    Value := FileInfoCache.license;
      IDX_PACKAGER:   Value := FileInfoCache.packager;
      IDX_GROUP:      Value := FileInfoCache.group;
      IDX_OS:         Value := FileInfoCache.os;
      IDX_ARCH:       Value := FileInfoCache.arch;
      IDX_SOURCE_RPM: Value := FileInfoCache.sourcerpm;
      IDX_SUMMARY:    Value := FileInfoCache.summary;
      IDX_DESCRIPTION: Value := FileINfoCache.description;
//      IDX_BUILD_TIME:
//            //???
//      IDX_ARCHIVE_SIZE:
//          Result := FT_NUMERIC_64;
//          size := FileInfoCache.archive_size;
//          case UnitIndex of
//            0:  //bytes
//              size := size * 1024;
//          // 1: //kbytes
//          //   pass
//            2:  //mbytes
//              size := size div 1024;
//            3:  //gbytes
//              size := size div (1024 * 1024);
//          end;
//          exit;

  else
      Result := FT_FIELDEMPTY;
      exit;
  end;

  StrPCopy(PChar(FieldValue), EnsureLength(Value, maxlen));
end;

initialization
  CurrentPackageFile := '';

end.

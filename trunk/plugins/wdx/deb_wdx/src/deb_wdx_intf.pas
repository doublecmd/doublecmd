//***************************************************************
// This file is part of DEBWDX, a content plugin for
// Total Commander handling Debian Linux package.
// 
// Copyright (C) 2005 Ralgh Young (yang.guilong@gmail.com)
//***************************************************************

// This program is free software; you can redistribute it and/or modify it 
// under the GPL. 
//
// Only version 2.0 package is supported, and I would not to add support for
// old format since I haven't old packages. But if you have some package of old format,
// and you're interested in, you're welcomed to modify the source by yourself.
// FYI: refer to dpkg-deb/extract.c in dpkg-deb's source,
//      and search '0.93' in function extracthalf, you're find something useful.

{$A-,I-}  //no alignment, no I/O error autochecking
unit deb_wdx_intf;

{$mode delphi}{$H+}
{$include calling.inc}

interface

uses
  Classes, WdxPlugin;

procedure ContentGetDetectString(DetectString:pchar; maxlen:integer); dcpcall;
function ContentGetSupportedField(FieldIndex:integer;FieldName:pchar;
  Units:pchar;maxlen:integer):integer; dcpcall;
function ContentGetValue(FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte;
  maxlen,flags:integer):integer; dcpcall;
  
implementation

uses
  SysUtils{$IFDEF GDEBUG}, DbugIntf{$ENDIF}, debunpak;

var
  IDX_PACKAGE,
  IDX_VERSION,
  IDX_SECTION,
  IDX_PRIORITY,
  IDX_ARCH,
  IDX_DEPENDS,
  IDX_RECOMMENDS,
  IDX_SUGGESTS,
  IDX_CONFLICTS,
  IDX_INSTALLED_SIZE,
  IDX_MAINTAINER,
  IDX_SOURCE,
  IDX_SUMMARY,
  IDX_DESCRIPTION : integer;

  CurrentPackageFile: String;
  FieldList : TStrings;
  FileInfo : TStrings;
  
procedure ContentGetDetectString(DetectString:pchar; maxlen:integer);
begin
  StrPCopy(DetectString, 'EXT="DEB"');
end;

function ContentGetSupportedField(FieldIndex:integer; FieldName:pchar;
  Units:pchar;maxlen:integer):integer;
begin
  StrPCopy(Units, '');

  if FieldIndex =IDX_INSTALLED_SIZE then
  begin
    StrPCopy(FieldName, FieldList.Strings[FieldIndex]);
    StrPCopy(Units, 'bytes|kbytes|Mbytes|Gbytes'+#0);
    Result := FT_NUMERIC_64;
  end
  else if FieldIndex < FieldList.Count then
  begin
     StrPCopy(FieldName, FieldList.Strings[FieldIndex]);
     Result := FT_STRING;
  end
  else
    Result := FT_NOMOREFIELDS;
end;

{$WRITEABLECONST ON}
function ContentGetValue(FileName:pchar; FieldIndex,UnitIndex:integer; FieldValue:pbyte;
  maxlen,flags:integer):integer;
  function EnsureLength(S: string; nMaxlen: integer): string;
  begin
      Result := S;
      if length(Result)>=nMaxlen then
      begin
          Result := Copy(Result, 1, nMaxlen-4);
          Result := Result + '...';
      end;
  end;
const
  DescTmpFile: String = '';
var
  Field, Value : String;
  i, where_start_desc : integer;
  size : int64;
begin
  Result := FT_FILEERROR;
  if not FileExists(FileName) then exit;

  if CurrentPackageFile<>FileName then
  begin
     if FileExists(DescTmpFile) then
     begin
        DeleteFile(DescTmpFile);
        RemoveDir(ExtractFileDir(DescTmpFile));
     end;

     if not Deb_ExtractCtrlInfoFile(FileName, DescTmpFile) then exit;

     if not FileExists(DescTmpFile) then  exit;

     FileInfo.Clear;
     FileInfo.LoadFromFile(DescTmpFile);
     CurrentPackageFile := FileName;
  end
{$IFDEF GDEBUG}
  else
      SendDebug('Cached info reused for '+FileName);
{$ENDIF};


  if (FieldIndex>=FieldList.Count) then 
  begin
      Result := FT_NOSUCHFIELD;
      exit;
  end;

  if FieldIndex<>IDX_DESCRIPTION then
  begin
      if FieldIndex=IDX_SUMMARY then //for 'Summary', return the value of Description
          Field := 'Description'
      else
          Field := FieldList.Strings[FieldIndex];
  
      Value := '';
      Value := FileInfo.Values[Field];
  
      if Value='' then
      begin
         Result := FT_FIELDEMPTY;
         exit;
      end;

      if FieldIndex=IDX_INSTALLED_SIZE then
        begin
          size := StrToInt64Def(Value, -1);
          case UnitIndex of
            0:  //bytes
              size := size * 1024;
          // 1: //kbytes
          //   pass
            2:  //mbytes
              size := size div 1024;
            3:  //gbytes
              size := size div (1024 * 1024);
          end;
          Move(size, FieldValue^, sizeof(size));
          Result := FT_NUMERIC_64;
        end
        else   //other fields, just string
        begin
           StrPCopy(PChar(FieldValue), EnsureLength(Value, maxlen));
           Result := FT_STRING;
        end;
  end
  else //IDX_DESCRIPTION,
  begin
      Value := '';
      where_start_desc := -1;
      for i:=0 to FileInfo.Count-1 do
      begin
      	if FileInfo.Names[i]='Description' then
      	begin
      	    where_start_desc := i;
      	    break;
      	end;
      end;
      
      if where_start_desc>=0 then
      begin
          for i:=where_start_desc+1 to FileInfo.Count-1 do
          begin
      	     Value := Value + FileInfo.Strings[i];
          end;
          StrPCopy(PChar(FieldValue), EnsureLength(Value, maxlen));
          //Result := FT_FULLTEXT;
          Result := FT_STRING;
      end;
  end

	 
end;

initialization
  FileInfo := TStringList.Create;
  FileInfo.NameValueSeparator := ':';
  
  CurrentPackageFile := '';
  
  FieldList := TStringList.Create;
  IDX_PACKAGE 	:= FieldList.Add('Package');
  IDX_VERSION 	:= FieldList.Add('Version');
  IDX_SECTION 	:= FieldList.Add('Section');
  IDX_PRIORITY 	:= FieldList.Add('Priority');
  IDX_ARCH	:= FieldList.Add('Architecture');
  IDX_DEPENDS   := FieldList.Add('Depends');
  IDX_RECOMMENDS:= FieldList.Add('Recommends');
  IDX_SUGGESTS  := FieldList.Add('Suggests');
  IDX_CONFLICTS := FieldList.Add('Conflicts');
  IDX_INSTALLED_SIZE := FieldList.Add('Installed-Size');
  IDX_MAINTAINER    := FieldList.Add('Maintainer');
  IDX_SOURCE    := FieldList.Add('Source');
  IDX_SUMMARY   := FieldList.Add('Summary');
  IDX_DESCRIPTION    := FieldList.Add('Description');

  
finalization
  FileInfo.Free;
  FieldList.Free;

end.

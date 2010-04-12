{
   Double commander
   -------------------------------------------------------------------------
   xpi_wdx is destined to obtainin the information from xpi-files
   in which extensions and themes for Gecko-based applications are distributed.

   Copyright (C) 2010 Koblov Alexander (Alexx2000@mail.ru)

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit xpi_io;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xpi_def;

function ParseInsatallManifest(FileName: PAnsiChar; out InstallManifest: TInstallManifest): Boolean;

implementation

uses
  Unzip, ZipUtils, DOM, XMLRead;

function ExtractInstallManifest(FileName: PAnsiChar; out MemoryStream: TMemoryStream): Boolean;
var
  xpiFile: unzFile = nil;
  rdfInfo: unz_file_info;
  iError: LongInt;
  FileBuffer: Pointer = nil;
begin
  Result:= False;
  xpiFile:= unzOpen(FileName);
  if Assigned(xpiFile) then
  try
    iError:= unzLocateFile(xpiFile, 'install.rdf', 0);
    if iError <> UNZ_OK then Exit;
    iError:= unzGetCurrentFileInfo(xpiFile, @rdfInfo, nil, 0, nil, 0, nil, 0);
    if iError <> UNZ_OK then Exit;
    FileBuffer:= AllocMem(rdfInfo.uncompressed_size);
    if FileBuffer = nil then Exit;
    iError := unzOpenCurrentFile(xpiFile);
    if iError <> UNZ_OK then Exit;
    iError := unzReadCurrentFile(xpiFile, FileBuffer, rdfInfo.uncompressed_size);
    if iError < 0 then Exit;
    if MemoryStream.Write(FileBuffer^, iError) = iError then
       Result:= True;
    iError := unzCloseCurrentFile(xpiFile);
  finally
    iError := unzClose(xpiFile);
    if Assigned(FileBuffer) then
      FreeMem(FileBuffer);
  end;
end;

function AddonType(const sType: AnsiString): AnsiString;
var
  iType: Integer;
begin
  iType:= StrToIntDef(sType, 0);
  case iType of
    2:   Result:= 'Extension';
    4:   Result:= 'Theme';
    8:   Result:= 'Locale';
    16:  Result:= 'Plugin';
    32:  Result:= 'Multiple Item Package';
  else
    Result:= 'Unknown';
  end;
end;

function AddString(const TargetStr, SourceStr: AnsiString): AnsiString;
begin
  if Length(TargetStr) = 0 then
    Result:= SourceStr
  else
    Result:= TargetStr + ', ' + SourceStr;
end;

procedure AddApplication(const InstallManifest: TInstallManifest; Node: TDOMNode);
var
  I: LongWord;
  Application: AnsiString;
  ID,
  MinVersion,
  MaxVersion: AnsiString;
begin
  Node:= Node.FindNode('Description');
  if Assigned(Node) then
    begin
      for I:= 0 to Node.ChildNodes.Count - 1 do
      with Node.ChildNodes.Item[I] do
      begin
        if nodeName = 'em:id' then
          ID:= FirstChild.NodeValue
        else if nodeName = 'em:minVersion' then
          MinVersion:= FirstChild.NodeValue
        else if nodeName = 'em:maxVersion' then
          MaxVersion:= FirstChild.NodeValue
      end;
      if InstallManifest.TargetApplication.IndexOfName(ID) < 0 then
        begin
          InstallManifest.TargetApplication.Add(ID + '=' + MinVersion + '-' + MaxVersion);
          Application:= ApplicationList.Values[ID];
          if Length(Application) <> 0 then
            begin
              InstallManifest.Compatibility:= AddString(InstallManifest.Compatibility, Application);
            end;
        end;
    end;
end;

function ParseInsatallManifest(FileName: PAnsiChar; out InstallManifest: TInstallManifest): Boolean;
var
  I: LongWord;
  MemoryStream: TMemoryStream = nil;
  Doc: TXMLDocument = nil;
  mainNode: TDOMNode = nil;
  childNode: TDOMNode = nil;
begin
  Result:= False;
  MemoryStream:= TMemoryStream.Create;
  try
    if ExtractInstallManifest(FileName, MemoryStream) then;
    begin
      MemoryStream.Position:= 0;
      Doc := TXMLDocument.Create;
      ReadXMLFile(Doc, MemoryStream);
       if Assigned(Doc.documentElement) then
         mainNode:= Doc.documentElement.FindNode('Description');
       {
       if not Assigned(mainNode) then
         mainNode:= Doc.documentElement.FindNode('RDF:Description');
       }
       if Assigned(mainNode) then
       begin
         InstallManifest.Clear;
         for I:= 0 to mainNode.ChildNodes.Count - 1 do
         begin
           childNode := mainNode.ChildNodes.Item[I];
           if Assigned(childNode) then
           with childNode do
           begin
             if nodeName = 'em:id' then
               InstallManifest.ID:= FirstChild.NodeValue
             else if nodeName = 'em:name' then
               InstallManifest.Name:= FirstChild.NodeValue
             else if nodeName = 'em:version' then
               InstallManifest.Version:= FirstChild.NodeValue
             else if nodeName = 'em:type' then
               InstallManifest.AddonType:= AddonType(FirstChild.NodeValue)
             else if nodeName = 'em:description' then
               InstallManifest.Description:= FirstChild.NodeValue
             else if nodeName = 'em:creator' then
               InstallManifest.Creator:= FirstChild.NodeValue
             else if nodeName = 'em:contributor' then
               InstallManifest.Contributor:= AddString(InstallManifest.Contributor, FirstChild.NodeValue)
             else if nodeName = 'em:developer' then
               InstallManifest.Developer:= AddString(InstallManifest.Developer, FirstChild.NodeValue)
             else if nodeName = 'em:translator' then
               InstallManifest.Translator:= AddString(InstallManifest.Translator, FirstChild.NodeValue)
             else if nodeName = 'em:homepageURL' then
               InstallManifest.HomePageURL:= FirstChild.NodeValue
             else if nodeName = 'em:updateURL' then
               InstallManifest.UpdateURL:= FirstChild.NodeValue
             else if nodeName = 'em:targetPlatform' then
               InstallManifest.TargetPlatform:= AddString(InstallManifest.TargetPlatform, FirstChild.NodeValue)
             else if nodeName = 'em:targetApplication' then
               AddApplication(InstallManifest, childNode)
           end;
         end;
         Result:= True;
       end;
      FreeAndNil(Doc);
    end;
  finally
    FreeAndNil(MemoryStream);
  end;
end;

end.


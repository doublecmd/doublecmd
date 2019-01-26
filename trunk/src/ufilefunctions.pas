{
   Double Commander
   -------------------------------------------------------------------------
   Filepanel columns implementation unit

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFileFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, uFile, uFileProperty, uFileSource;

type
  TFileFunction = (fsfName = 0,
                   fsfExtension = 1,
                   fsfSize = 2,
                   fsfAttr = 3,
                   fsfPath = 4,
                   fsfGroup = 5,
                   fsfOwner = 6,
                   fsfModificationTime = 7,
                   fsfCreationTime = 8,
                   fsfLastAccessTime = 9,
                   fsfChangeTime = 10,
                   fsfLinkTo = 11,
                   fsfNameNoExtension = 12,
                   fsfType = 13,
                   fsfComment = 14,
                   fsfCompressedSize = 15,
                   fsfInvalid = 16,
                   fsfVariant = Ord(fpVariant),
                   fsfMaximum = Ord(fpMaximum));

  TFileFunctions = array of TFileFunction;

const
  fsfVariantAll = [fsfVariant..fsfMaximum];

  const TFileFunctionStrings: array [Low(TFileFunction)..fsfInvalid] of string
            = ('GETFILENAME',
               'GETFILEEXT',
               'GETFILESIZE',
               'GETFILEATTR',
               'GETFILEPATH',
               'GETFILEGROUP',
               'GETFILEOWNER',
               'GETFILETIME',
               'GETFILECREATIONTIME',
               'GETFILELASTACCESSTIME',
               'GETFILECHANGETIME',
               'GETFILELINKTO',
               'GETFILENAMENOEXT',
               'GETFILETYPE',
               'GETFILECOMMENT',
               'GETFILECOMPRESSEDSIZE',
               ''                 // fsfInvalid
               );

  function FormatFileFunction(FuncS: string; AFile: TFile; const AFileSource: IFileSource; RetrieveProperties: Boolean = False): string;
  function FormatFileFunctions(FuncS: String; AFile: TFile; const AFileSource: IFileSource): String;
  function GetVariantFileProperty(const FuncS: String; AFile: TFile; const AFileSource: IFileSource): Variant;
  function GetFileFunctionByName(FuncS: string): TFileFunction;
  function GetFilePropertyType(FileFunction: TFileFunction): TFilePropertiesTypes; inline;

  procedure FillContentFieldMenu(MenuItem: TMenuItem; OnMenuItemClick: TNotifyEvent; const FileSystem: String = '');

  procedure FillFileFuncList;

const
  sFuncTypeDC     = 'DC';
  sFuncTypePlugin = 'PLUGIN';

var
  FileFunctionsStr: TStringList;

implementation

uses
  StrUtils, WdxPlugin, uWdxModule, uGlobs, uLng, uDefaultFilePropertyFormatter,
  uFileSourceProperty, uWfxPluginFileSource, uWfxModule, uColumns, DCFileAttributes,
  DCStrUtils, DCBasicTypes, uDCUtils, uTypes;

const
  ATTR_OCTAL = 'OCTAL';
  //***Note: Number of elements in "FILE_SIZE" should normally fit with the number of element in "TFileSizeFormat" we have in "uTypes" unit.:
  FILE_SIZE: array[0..11] of String = ('FLOAT', 'BYTE', 'KILO', 'MEGA', 'GIGA', 'TERA', 'PERSFLOAT', 'PERSBYTE', 'PERSKILO', 'PERSMEGA', 'PERSGIGA', 'PERSTERA');

// Which file properties must be supported for each file function to work.
const TFileFunctionToProperty: array [Low(TFileFunction)..fsfInvalid] of TFilePropertiesTypes
          = ([fpName],
             [fpName],
             [fpSize],
             [fpAttributes],
             [] { path },
             [fpOwner],
             [fpOwner],
             [fpModificationTime],
             [fpCreationTime],
             [fpLastAccessTime],
             [fpChangeTime],
             [fpLink],
             [fpName],
             [fpType],
             [fpComment],
             [fpCompressedSize],
             [] { invalid });

//Return type (Script or DC or Plugin etc)
function GetModType(str: String): String;
begin
  if pos('(', Str) > 0 then
    Result := Copy(Str, 1, pos('(', Str) - 1)
  else
    Result := EmptyStr;
end;

//Return name in (). (SriptName or PluginName etc)
function GetModName(str: String): String;
var
  s: String;
begin
  s := str;
  if pos('(', S) > 0 then
    Delete(s, 1, pos('(', S))
  else
    Exit(EmptyStr);

  if pos(')', s) > 0 then
    Result := Copy(s, 1, pos(')', s) - 1);
end;

//Return function name (DCFunction,PluginFunction etc)
function GetModFunctionName(str: String): String;
var
  s: String;
begin
  s := str;
  if pos('.', S) > 0 then
    Delete(s, 1, pos('.', S))
  else
    Exit(EmptyStr);

  if pos('{', S) > 0 then
    Result := Copy(s, 1, pos('{', S) - 1);
end;

// Return function parameters
function GetModFunctionParams(str: String): String;
var
  I: Integer;
  S: String;
begin
  S := str;
  I := pos('{', S);
  if I < 0 then
    Exit(EmptyStr);
  Delete(S, 1, I);
  I := pos('}', S);
  if I < 0 then
    Exit(EmptyStr);
  Result := Copy(S, 1, I - 1);
end;

function FormatFileFunction(FuncS: string; AFile: TFile;
  const AFileSource: IFileSource; RetrieveProperties: Boolean): string;
var
  AIndex: Integer;
  FileFunction: TFileFunction;
  AType, AFunc, AParam: String;
  AFileProperty: TFileVariantProperty;
  FilePropertiesNeeded: TFilePropertiesTypes;
begin
  Result := EmptyStr;
  //---------------------
  AType  := upcase(GetModType(FuncS));
  AFunc  := upcase(GetModFunctionName(FuncS));
  AParam := upcase(GetModFunctionParams(FuncS));
  //---------------------
  //Internal doublecmd function
  //------------------------------------------------------
  if AType = sFuncTypeDC then
  begin
    AIndex:= FileFunctionsStr.IndexOfName(AFunc);
    if AIndex < 0 then Exit;
    FileFunction:= TFileFunction(AIndex);
    // Retrieve additional properties if needed
    if RetrieveProperties then
    begin
      FilePropertiesNeeded:= TFileFunctionToProperty[FileFunction];
      if aFileSource.CanRetrieveProperties(AFile, FilePropertiesNeeded) then
        aFileSource.RetrieveProperties(AFile, FilePropertiesNeeded, []);
    end;
    case FileFunction of
      fsfName:
        begin
          // Show square brackets around directories
          if gDirBrackets and (AFile.IsDirectory or
            AFile.IsLinkToDirectory) then
            Result := '[' + AFile.Name + ']'
          else
            Result := AFile.Name;
        end;

      fsfExtension:
        begin
          Result := AFile.Extension;
        end;

      fsfSize:
        begin
          if (AFile.IsDirectory or AFile.IsLinkToDirectory) and
            ((not (fpSize in AFile.SupportedProperties)) or (AFile.Size = 0))
          then begin
            if AFile.IsLinkToDirectory then
              Result := rsAbbrevDisplayLink
            else
              Result := rsAbbrevDisplayDir;
          end
          else if fpSize in AFile.SupportedProperties then
          begin
            if Length(AParam) = 0 then
              Result := AFile.Properties[fpSize].Format(DefaultFilePropertyFormatter)
            else
              for AIndex:= 0 to High(FILE_SIZE) do
              begin
                if AParam = FILE_SIZE[AIndex] then
                begin
                  Result := cnvFormatFileSize(AFile.Size, TFileSizeFormat(AIndex), gFileSizeDigits);
                  Break;
                end;
              end;
          end;
        end;

      fsfAttr:
        if fpAttributes in AFile.SupportedProperties then
        begin
          if AFile.Properties[fpAttributes] is TUnixFileAttributesProperty and (AParam = ATTR_OCTAL) then
            Result := FormatUnixModeOctal(AFile.Attributes)
          else
            Result := AFile.Properties[fpAttributes].Format(DefaultFilePropertyFormatter);
        end;

      fsfPath:
        Result := AFile.Path;

      fsfGroup:
        if fpOwner in AFile.SupportedProperties then
          Result := AFile.OwnerProperty.GroupStr;

      fsfOwner:
        if fpOwner in AFile.SupportedProperties then
          Result := AFile.OwnerProperty.OwnerStr;

      fsfModificationTime:
        if fpModificationTime in AFile.SupportedProperties then
          Result := AFile.Properties[fpModificationTime].Format(
            DefaultFilePropertyFormatter);

      fsfCreationTime:
        if fpCreationTime in AFile.SupportedProperties then
          Result := AFile.Properties[fpCreationTime].Format(
            DefaultFilePropertyFormatter);

      fsfLastAccessTime:
        if fpLastAccessTime in AFile.SupportedProperties then
          Result := AFile.Properties[fpLastAccessTime].Format(
            DefaultFilePropertyFormatter);

      fsfChangeTime:
        if fpChangeTime in AFile.SupportedProperties then
          Result := AFile.Properties[fpChangeTime].Format(
            DefaultFilePropertyFormatter);

      fsfLinkTo:
        if fpLink in AFile.SupportedProperties then
          Result := AFile.LinkProperty.LinkTo;

      fsfNameNoExtension:
        begin
          // Show square brackets around directories
          if gDirBrackets and (AFile.IsDirectory or
            AFile.IsLinkToDirectory) then
            Result := '[' + AFile.NameNoExt + ']'
          else
            Result := AFile.NameNoExt;
        end;

      fsfType:
        if fpType in AFile.SupportedProperties then
          Result := AFile.TypeProperty.Format(DefaultFilePropertyFormatter);

      fsfComment:
        if fpComment in AFile.SupportedProperties then
          Result := AFile.CommentProperty.Format(DefaultFilePropertyFormatter);

      fsfCompressedSize:
        begin
          if (AFile.IsDirectory or AFile.IsLinkToDirectory) and
            ((not (fpCompressedSize in AFile.SupportedProperties)) or (AFile.CompressedSize = 0))
          then begin
            if AFile.IsLinkToDirectory then
              Result := rsAbbrevDisplayLink
            else
              Result := rsAbbrevDisplayDir;
          end
          else if fpCompressedSize in AFile.SupportedProperties then
            Result := AFile.Properties[fpCompressedSize].Format(DefaultFilePropertyFormatter);
        end;
    end;
  end
  //------------------------------------------------------
  //Plugin function
  //------------------------------------------------------
  else if AType = sFuncTypePlugin then
  begin
    // Retrieve additional properties if needed
    if RetrieveProperties then
      Result:= GetVariantFileProperty(FuncS, AFile, AFileSource)
    else begin
      for AIndex:= 0 to High(AFile.VariantProperties) do
      begin
        AFileProperty:= TFileVariantProperty(AFile.VariantProperties[AIndex]);
        if Assigned(AFileProperty) and SameText(FuncS, AFileProperty.Name) then
        begin
          Result:= AFileProperty.Format(DefaultFilePropertyFormatter);
          Break;
        end;
      end;
    end;
  end;
  //------------------------------------------------------
end;

function FormatFileFunctions(FuncS: String; AFile: TFile; const AFileSource: IFileSource): String;
var
  P: Integer;
begin
  Result:= EmptyStr;

  while True do
  begin
    P := Pos('[', FuncS);
    if P = 0 then
      Break
    else if P > 1 then
      Result:= Result + Copy(FuncS, 1, P - 1);
    Delete(FuncS, 1, P);

    P := Pos(']', FuncS);
    if P = 0 then
      Break
    else if P > 1 then
      Result:= Result + FormatFileFunction(Copy(FuncS, 1, P - 1), AFile, AFileSource, True);
    Delete(FuncS, 1, P);
  end;

  if Length(FuncS) <> 0 then
    Result:= Result + FuncS;
end;

function GetFileFunctionByName(FuncS: String): TFileFunction;
var
  AIndex: Integer;
  AType, AFunc: String;
begin
  AType := UpCase(GetModType(FuncS));
  AFunc := UpCase(GetModFunctionName(FuncS));

  // Only internal DC functions.
  if AType = sFuncTypeDC then
  begin
    AIndex := FileFunctionsStr.IndexOfName(AFunc);
    if AIndex >= 0 then Exit(TFileFunction(AIndex));
  end
  else if AType = sFuncTypePlugin then Exit(fsfVariant);
  Result := fsfInvalid;
end;

function GetVariantFileProperty(const FuncS: String; AFile: TFile;
  const AFileSource: IFileSource): Variant;
var
  AType, AName, AFunc, AParam: String;
begin
  Result := Unassigned;
  //---------------------
  AType  := upcase(GetModType(FuncS));
  AName  := upcase(GetModName(FuncS));
  AFunc  := upcase(GetModFunctionName(FuncS));
  AParam := upcase(GetModFunctionParams(FuncS));
  //------------------------------------------------------
  //Plugin function
  //------------------------------------------------------
  if AType = sFuncTypePlugin then
  begin
    if AFileSource.IsClass(TWfxPluginFileSource) then
    begin
      if IWfxPluginFileSource(AFileSource).WfxModule.ContentPlugin and
         IWfxPluginFileSource(AFileSource).WfxModule.FileParamVSDetectStr(AFile) then
      begin
        Result := IWfxPluginFileSource(AFileSource).WfxModule.CallContentGetValueV(
          AFile.FullPath, AFunc, AParam, 0);
      end;
    end
    else if fspDirectAccess in AFileSource.Properties then
    begin
      if not gWdxPlugins.IsLoaded(AName) then
        if not gWdxPlugins.LoadModule(AName) then
          Exit;
      if gWdxPlugins.GetWdxModule(AName).FileParamVSDetectStr(AFile) then
      begin
        Result := gWdxPlugins.GetWdxModule(AName).CallContentGetValueV(
          AFile.FullPath, AFunc, AParam, 0);
      end;
    end;
  end;
  //------------------------------------------------------
end;

function GetFilePropertyType(FileFunction: TFileFunction): TFilePropertiesTypes;
begin
  if FileFunction >= fsfVariant then
    Result:= [TFilePropertyType(FileFunction)]
  else begin
    Result:= TFileFunctionToProperty[FileFunction];
  end;
end;

procedure AddModule(MenuItem: TMenuItem; OnMenuItemClick: TNotifyEvent; Module: TWDXModule);
var
  J, K: Integer;
  MI, MI2: TMenuItem;
  WdxField: TWdxField;
begin
  MI:= TMenuItem.Create(MenuItem);
  MI.Caption:= Module.Name;
  MenuItem.Add(MI);
  // Load fields list
  for J:= 0 to Module.FieldList.Count - 1 do
  begin
    WdxField:= TWdxField(Module.FieldList.Objects[J]);
    if not (WdxField.FType in [ft_fulltext, ft_fulltextw]) then
    begin
      MI:= TMenuItem.Create(MenuItem);
      MI.Tag:= 1;
      MI.Caption:= WdxField.LName;
      MI.Hint:= Module.FieldList[J];
      MenuItem.Items[MenuItem.Count - 1].Add(MI);
      if WdxField.FType <> ft_multiplechoice then
      begin
        for K:= 0 to High(WdxField.FUnits) do
        begin
          MI2:=TMenuItem.Create(MenuItem);
          MI2.Tag:= 2;
          MI2.Caption:= WdxField.LUnits[K];
          MI2.Hint:= WdxField.FUnits[K];
          MI2.OnClick:= OnMenuItemClick;
          MI.Add(MI2);
        end;
      end;
      if MI.Count = 0 then MI.OnClick:= OnMenuItemClick;
    end;
  end;
end;

procedure FillContentFieldMenu(MenuItem: TMenuItem; OnMenuItemClick: TNotifyEvent; const FileSystem: String);
var
  I, J: Integer;
  MI, MI2: TMenuItem;
  Module: TWDXModule;
  FileSize: TDynamicStringArray;
begin
  MenuItem.Clear;

  // DC commands
  MI:= TMenuItem.Create(MenuItem);
  MI.Caption:= 'DC';
  MenuItem.Add(MI);
  for I:= 0 to FileFunctionsStr.Count - 1 do
  begin
    MI:= TMenuItem.Create(MenuItem);
    MI.Tag:= 0;
    MI.Hint:= FileFunctionsStr.Names[I];
    MI.Caption:= FileFunctionsStr.ValueFromIndex[I] + '  (' + MI.Hint + ')';
    MenuItem.Items[0].Add(MI);
    // Special case for attributes
    if TFileFunctionStrings[fsfAttr] = FileFunctionsStr.Names[I] then
    begin
      // String attributes
      MI2:= TMenuItem.Create(MenuItem);
      MI2.Tag:= 3;
      MI2.Hint:= '';
      MI2.Caption:= rsMnuContentDefault;
      MI2.OnClick:= OnMenuItemClick;
      MI.Add(MI2);
      // Octal attributes
      MI2:= TMenuItem.Create(MenuItem);
      MI2.Tag:= 3;
      MI2.Hint:= ATTR_OCTAL;
      MI2.Caption:= rsMnuContentOctal;
      MI2.OnClick:= OnMenuItemClick;
      MI.Add(MI2);
    end;
    // Special case for size
    if TFileFunctionStrings[fsfSize] = FileFunctionsStr.Names[I] then
    begin
      // Default format
      MI2:= TMenuItem.Create(MenuItem);
      MI2.Tag:= 3;
      MI2.Hint:= '';
      MI2.Caption:= rsMnuContentDefault;
      MI2.OnClick:= OnMenuItemClick;
      MI.Add(MI2);
      FileSize:= SplitString(rsOptFileSizeFloat + ';' + rsLegacyOperationByteSuffixLetter + ';' + rsLegacyDisplaySizeSingleLetterKilo + ';' + rsLegacyDisplaySizeSingleLetterMega + ';' + rsLegacyDisplaySizeSingleLetterGiga + ';' + rsLegacyDisplaySizeSingleLetterTera + ';' + rsOptPersonalizedFileSizeFormat, ';');
      for J:= 0 to High(FILE_SIZE) do
      begin
        MI2:= TMenuItem.Create(MenuItem);
        MI2.Tag:= 3;
        MI2.Hint:= FILE_SIZE[J];
        MI2.Caption:= FileSize[J];
        MI2.OnClick:= OnMenuItemClick;
        MI.Add(MI2);
      end;
    end;
    if MI.Count = 0 then MI.OnClick:= OnMenuItemClick;
  end;
  // Plugins
  if (FileSystem = EmptyStr) or SameText(FileSystem, FS_GENERAL) then
  begin
    MI:= TMenuItem.Create(MenuItem);
    MI.Caption:= rsOptionsEditorPlugins;
    MenuItem.Add(MI);
    for I:= 0 to gWdxPlugins.Count - 1 do
    begin
      Module:= gWdxPlugins.GetWdxModule(I);
      if not (Module.IsLoaded or Module.LoadModule) then
       Continue;
      AddModule(MI, OnMenuItemClick, Module);
    end;
  end
  else begin
    I:= gWFXPlugins.IndexOfName(FileSystem);
    if (I >= 0) then begin
      Module:= gWFXPlugins.LoadModule(gWFXPlugins.FileName[I]);
      if Assigned(Module) and TWfxModule(Module).ContentPlugin then
        AddModule(MenuItem, OnMenuItemClick, Module);
    end;
  end;
end;

procedure FillFileFuncList;
begin
  with FileFunctionsStr do
  begin
    Add(TFileFunctionStrings[fsfName] + '=' + rsFuncName);
    Add(TFileFunctionStrings[fsfExtension] + '=' + rsFuncExt);
    Add(TFileFunctionStrings[fsfSize] + '=' + rsFuncSize);
    Add(TFileFunctionStrings[fsfAttr] + '=' + rsFuncAttr);
    Add(TFileFunctionStrings[fsfPath] + '=' + rsFuncPath);
    Add(TFileFunctionStrings[fsfGroup] + '=' + rsFuncGroup);
    Add(TFileFunctionStrings[fsfOwner] + '=' + rsFuncOwner);
    Add(TFileFunctionStrings[fsfModificationTime] + '=' + rsFuncMTime);
    Add(TFileFunctionStrings[fsfCreationTime] + '=' + rsFuncCTime);
    Add(TFileFunctionStrings[fsfLastAccessTime] + '=' + rsFuncATime);
    Add(TFileFunctionStrings[fsfChangeTime] + '=' + rsFuncHTime);
    Add(TFileFunctionStrings[fsfLinkTo] + '=' + rsFuncLinkTo);
    Add(TFileFunctionStrings[fsfNameNoExtension] + '=' + rsFuncNameNoExt);
    Add(TFileFunctionStrings[fsfType] + '=' + rsFuncType);
    Add(TFileFunctionStrings[fsfComment] + '=' + rsFuncComment);
    Add(TFileFunctionStrings[fsfCompressedSize] + '=' + rsFuncCompressedSize);
  end;
end;

initialization
  FileFunctionsStr := TStringList.Create;

finalization
  FreeAndNil(FileFunctionsStr);

end.


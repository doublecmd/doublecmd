{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains TFileInfoToolTip class and functions.

   Copyright (C) 2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit uInfoToolTip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, DCXmlConfig,
  uFile, uFileSource;

type
  { THintItem }

  THintItem = class
    Name: String;
    Mask: String;
    Hint: String;
    function Clone: THintItem;
  end;

  { THintItemList }

  THintItemList = specialize TFPGObjectList<THintItem>;

  { TFileInfoToolTip }

  TFileInfoToolTip = class(TPersistent)
  protected
    FHintItemList: THintItemList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): String;

    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure LoadFromFile(const FileName: String);
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToFile(const FileName: String);
    function ComputeSignature(Seed: dword = $00000000): dword;
    procedure Sort;

    property  HintItemList: THintItemList read FHintItemList;
  end;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): String;

implementation

uses
  crc, LCLProc, StrUtils, uMasks, uDebug, uGlobs, uFileProperty, uFileFunctions,
  uSearchTemplate, uFileSourceProperty
{$IF DEFINED(MSWINDOWS)}
  , uShlObjAdditional
{$ENDIF}
  ,DCClassesUtf8;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): String;

  function GetDefaultToolTip(const Hint: String): String;
  begin
    Result:= Hint;
    if fpModificationTime in aFile.SupportedProperties then
      with (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty) do
      Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 +  AsString;
    if fpSize in aFile.SupportedProperties then
      with (aFile.Properties[fpSize] as TFileSizeProperty) do
      Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 + AsString;
    if fpCompressedSize in aFile.SupportedProperties then
      with (aFile.Properties[fpCompressedSize] as TFileCompressedSizeProperty) do
      Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 + AsString;
  end;

begin
  Result:= EmptyStr;

  if fspDirectAccess in aFileSource.Properties then
  begin
    case gShowToolTipMode of
      tttmCombineDcSystem, tttmDcSystemCombine, tttmDcIfPossThenSystem, tttmDcOnly: Result := StringReplace(gFileInfoToolTip.GetFileInfoToolTip(aFileSource, aFile), '\n', LineEnding, [rfReplaceAll]);
      tttmSystemOnly: Result := EmptyStr;
    end;

    {$IF DEFINED(MSWINDOWS)}
    case gShowToolTipMode of
      tttmCombineDcSystem: Result := IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + SHGetInfoTip(aFile.Path, aFile.Name);
      tttmDcSystemCombine: Result := SHGetInfoTip(aFile.Path, aFile.Name) + IfThen(Result = EmptyStr, EmptyStr, LineEnding + Result);
      tttmDcIfPossThenSystem: if Result = EmptyStr then Result := SHGetInfoTip(aFile.Path, aFile.Name);
      tttmDcOnly: ;
      tttmSystemOnly: Result := SHGetInfoTip(aFile.Path, aFile.Name);
    end;
    {$ELSE}
    case gShowToolTipMode of
      tttmCombineDcSystem: Result := IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + GetDefaultToolTip(EmptyStr);
      tttmDcSystemCombine: Result := GetDefaultToolTip(EmptyStr) + IfThen(Result = EmptyStr, EmptyStr, LineEnding + Result);
      tttmDcIfPossThenSystem: if Result = EmptyStr then Result := GetDefaultToolTip(EmptyStr);
      tttmDcOnly: ;
      tttmSystemOnly: Result := GetDefaultToolTip(Result);
    end;
    {$ENDIF}
  end
  else
    begin
      Result:= GetDefaultToolTip(Result);
    end;
end;

{ THintItem }

function THintItem.Clone: THintItem;
begin
  Result:= THintItem.Create;
  Result.Name:= Name;
  Result.Mask:= Mask;
  Result.Hint:= Hint;
end;

{ TFileInfoToolTip }

constructor TFileInfoToolTip.Create;
begin
  FHintItemList:= THintItemList.Create(True);
end;

destructor TFileInfoToolTip.Destroy;
begin
  FreeThenNil(FHintItemList);
  inherited Destroy;
end;

procedure TFileInfoToolTip.Clear;
begin
  begin
    while FHintItemList.Count > 0 do
      begin
        //FHintItemList[0].Free;
        FHintItemList.Delete(0);
      end;
  end;
end;

procedure TFileInfoToolTip.Assign(Source: TPersistent);
var
  I: LongInt;
  From: TFileInfoToolTip;
begin
  Clear;
  From:= Source as TFileInfoToolTip;
  for I:= 0 to From.FHintItemList.Count - 1 do
    FHintItemList.Add(From.FHintItemList[I].Clone);
end;

function TFileInfoToolTip.GetFileInfoToolTip(aFileSource: IFileSource;
  const aFile: TFile): String;
var
  I, J: Integer;
  HintItem: THintItem;
begin
 Result:= EmptyStr;

 for I:= 0 to FHintItemList.Count - 1 do
   begin
     HintItem:= FHintItemList[I];

     // Get hint by search template
     if IsMaskSearchTemplate(HintItem.Mask) then
       for J:= 0 to gSearchTemplateList.Count - 1 do
         with gSearchTemplateList do
         begin
           if (Templates[J].TemplateName = PChar(HintItem.Mask)+1) and
              Templates[J].CheckFile(AFile) then
             begin
               Result:= FormatFileFunctions(HintItem.Hint, aFile, aFileSource);
               Exit;
             end;
         end;

     // Get hint by file mask
     if MatchesMaskList(AFile.Name, HintItem.Mask) then
       begin
         Result:= FormatFileFunctions(HintItem.Hint, aFile, aFileSource);
         Exit;
       end;
   end;
end;

procedure TFileInfoToolTip.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sMask,
  sName,
  sHint: String;
  MaskItem: THintItem;
begin
  Clear;

  ANode := ANode.FindNode('CustomFields');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('CustomField') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', sName) and
           AConfig.TryGetValue(ANode, 'Mask', sMask) and
           AConfig.TryGetValue(ANode, 'Hint', sHint) then
        begin
          MaskItem:= THintItem.Create;
          MaskItem.Name := sName;
          MaskItem.Mask := sMask;
          MaskItem.Hint := sHint;
          FHintItemList.Add(MaskItem);
        end
        else
        begin
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
        end;
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

{ TFileInfoToolTip.LoadFromFile }
procedure TFileInfoToolTip.LoadFromFile(const FileName: String);
var
  TooltipConfig: TXmlConfig = nil;
  Root, Node: TXmlNode;
begin
  TooltipConfig := TXmlConfig.Create(FileName);
  try
    if TooltipConfig.Load then
    begin
      Root := TooltipConfig.RootNode;
      Node := Root.FindNode('ToolTips');
      if Assigned(Node) then
        Load(TooltipConfig, Node);
    end;
  finally
    TooltipConfig.Free;
  end;
end;

procedure TFileInfoToolTip.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I : Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'CustomFields', True);
  AConfig.ClearNode(ANode);

  for I:=0 to FHintItemList.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'CustomField');
      AConfig.AddValue(SubNode, 'Name', FHintItemList[I].Name);
      AConfig.AddValue(SubNode, 'Mask', FHintItemList[I].Mask);
      AConfig.AddValue(SubNode, 'Hint', FHintItemList[I].Hint);
    end;
end;

{ TFileInfoToolTip.SaveToFile }
procedure TFileInfoToolTip.SaveToFile(const FileName: String);
var
  TooltipConfig: TXmlConfig = nil;
  Root, Node: TXmlNode;
begin
  TooltipConfig := TXmlConfig.Create(FileName);
  try
    Root := TooltipConfig.RootNode;
    Node := TooltipConfig.FindNode(Root, 'ToolTips', True);
    Save(TooltipConfig, Node);
    TooltipConfig.Save;
  finally
    TooltipConfig.Free;
  end;
end;

{ TFileInfoToolTip.ComputeSignature }
function TFileInfoToolTip.ComputeSignature(Seed: dword): dword;
  procedure UpdateSignature(sInfo: string);
  begin
    if length(sInfo) > 0 then
      Result := crc32(Result, @sInfo[1], length(sInfo));
  end;
var
  Index: integer;
begin
  Result := Seed;
  for Index := 0 to pred(FHintItemList.Count) do
  begin
    UpdateSignature(FHintItemList[Index].Name);
    UpdateSignature(FHintItemList[Index].Mask);
    UpdateSignature(FHintItemList[Index].Hint);
  end;
end;

{ MyHintCompare }
function MyHintCompare(const Item1, Item2: THintItem): integer;
begin
  Result := CompareStr(Item1.Name, Item2.Name);
end;

{ TFileInfoToolTip.Sort }
procedure TFileInfoToolTip.Sort;
begin
  Self.HintItemList.Sort(@MyHintCompare);
end;

end.


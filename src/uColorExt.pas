{
   Double Commander
   -------------------------------------------------------------------------
   Load colors of files in file panels

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2022 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   
}

unit uColorExt;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, uFile, uMasks, uSearchTemplate, DCXmlConfig, DCJsonConfig,
  fpJson;

type

  { TMaskItem }

  TMaskItem = class
  private
    FExt: String;
    FModeStr: String;
    FMaskList: TMaskList;
    FAttrList: TMaskList;
    FTemplate: TSearchTemplate;
    FColor: array[0..1] of TColor;
  private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
    procedure SetExt(const AValue: String);
    procedure SetModeStr(const AValue: String);
  public
    sName: String;

    destructor Destroy; override;
    procedure Assign(ASource: TMaskItem);
    property sExt: String read FExt write SetExt;
    property cColor: TColor read GetColor write SetColor;
    property sModeStr: String read FModeStr write SetModeStr;
  end;

  { TColorExt }

  TColorExt = class
  private
    FStyle: Integer;
    FMaskItems: TList;

    function GetCount: Integer;
    function GetItems(const Index: Integer): TMaskItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AItem: TMaskItem);

    function GetColorBy(const AFile: TFile): TColor;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;

    procedure Load(AConfig: TJSONObject); overload;
    procedure Save(AConfig: TJSONObject); overload;

    procedure UpdateStyle;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMaskItem read GetItems; default;
  end;

implementation

uses
  SysUtils, uDebug, uGlobs, uFileProperty, uColors;

{ TMaskItem }

procedure TMaskItem.SetExt(const AValue: String);
var
  ATemplate: TSearchTemplate;
begin
  FExt:= AValue;
  FreeAndNil(FMaskList);
  // Plain mask
  if not IsMaskSearchTemplate(FExt) then
  begin
    FreeAndNil(FTemplate);
    if (Length(FExt) > 0) then
      FMaskList:= TMaskList.Create(FExt);
  end
  // Search template
  else begin
    ATemplate:= gSearchTemplateList.TemplateByName[PAnsiChar(FExt) + 1];
    if (ATemplate = nil) then
      FreeAndNil(FTemplate)
    else begin
      if (FTemplate = nil) then begin
        FTemplate:= TSearchTemplate.Create;
      end;
      FTemplate.SearchRecord:= ATemplate.SearchRecord;
    end;
  end;
end;

function TMaskItem.GetColor: TColor;
begin
  Result:= FColor[TColorThemes.StyleIndex];
end;

procedure TMaskItem.SetColor(AValue: TColor);
begin
  FColor[TColorThemes.StyleIndex]:= AValue;
end;

procedure TMaskItem.SetModeStr(const AValue: String);
begin
  if FModeStr <> AValue then
  begin
    FModeStr:= AValue;
    FreeAndNil(FAttrList);
    if (Length(FModeStr) > 0) then
      FAttrList:= TMaskList.Create(FModeStr);
  end;
end;

destructor TMaskItem.Destroy;
begin
  FAttrList.Free;
  FTemplate.Free;
  FMaskList.Free;
  inherited Destroy;
end;

procedure TMaskItem.Assign(ASource: TMaskItem);
begin
  Assert(Assigned(ASource));
  sExt := ASource.sExt;
  sModeStr := ASource.sModeStr;
  FColor[0] := ASource.FColor[0];
  FColor[1] := ASource.FColor[1];
  sName := ASource.sName;
end;

function TColorExt.GetCount: Integer;
begin
  Result := FMaskItems.Count;
end;

function TColorExt.GetItems(const Index: Integer): TMaskItem;
begin
  Result := TMaskItem(FMaskItems[Index]);
end;

constructor TColorExt.Create;
begin
  FMaskItems:= TList.Create;
  FStyle:= TColorThemes.StyleIndex;
end;

destructor TColorExt.Destroy;
begin
  Clear;
  FreeAndNil(FMaskItems);
  inherited Destroy;
end;

procedure TColorExt.Clear;
begin
  while FMaskItems.Count > 0 do
  begin
    TMaskItem(FMaskItems[0]).Free;
    FMaskItems.Delete(0);
  end;
end;

procedure TColorExt.Add(AItem: TMaskItem);
begin
  FMaskItems.Add(AItem);
end;

function TColorExt.GetColorBy(const AFile: TFile): TColor;
var
  Attr: String;
  Index: Integer;
  MaskItem: TMaskItem;
begin
  Result:= clDefault;

  if not (fpAttributes in AFile.SupportedProperties) then
    Attr:= EmptyStr
  else begin
    Attr:= AFile.Properties[fpAttributes].AsString;
  end;

  for Index:= 0 to FMaskItems.Count - 1 do
  begin
    MaskItem:= TMaskItem(FMaskItems[Index]);

    // Get color by search template
    if IsMaskSearchTemplate(MaskItem.FExt) then
    begin
      if Assigned(MaskItem.FTemplate) and MaskItem.FTemplate.CheckFile(AFile) then
      begin
        Result:= MaskItem.FColor[FStyle];
        Exit;
      end;
      Continue;
    end;

    // Get color by extension and attribute.
    // If attributes field is empty then don't match directories.
    if ((MaskItem.FMaskList = nil) or (((MaskItem.FAttrList <> nil) or
           not (AFile.IsDirectory or AFile.IsLinkToDirectory)) and
          MaskItem.FMaskList.Matches(AFile.Name)))
       and
       ((MaskItem.FAttrList = nil) or (Length(Attr) = 0) or
         MaskItem.FAttrList.Matches(Attr)) then
      begin
        Result:= MaskItem.FColor[FStyle];
        Exit;
      end;
  end;
end;

procedure TColorExt.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  iColor: Integer;
  MaskItem: TMaskItem;
  sAttr, sName, sExtMask: String;
begin
  Clear;

  ANode := ANode.FindNode('FileFilters');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('Filter') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', sName) and
           AConfig.TryGetValue(ANode, 'FileMasks', sExtMask) and
           AConfig.TryGetValue(ANode, 'Color', iColor) and
           AConfig.TryGetValue(ANode, 'Attributes', sAttr) then
        begin
          MaskItem := TMaskItem.Create;
          MaskItem.sName                   := sName;
          MaskItem.FColor[FStyle]          := iColor;
          MaskItem.FColor[Abs(FStyle - 1)] := iColor;
          MaskItem.sExt                    := sExtMask;
          MaskItem.sModeStr                := sAttr;
          FMaskItems.Add(MaskItem);
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

procedure TColorExt.Save(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  ANode:= AConfig.FindNode(ANode, 'FileFilters', False);
  if Assigned(ANode) then AConfig.DeleteNode(ANode);
end;

procedure TColorExt.Load(AConfig: TJSONObject);
var
  I: Integer;
  AList: TJSONArray;
  AItem: TJSONObject;
  MaskItem: TMaskItem;
  AColors: TJSONArray;
  sAttr, sName, sExtMask: TJSONString;
begin
  if not AConfig.Find('FileColors', AList) then
    Exit;

  for I:= 0 to AList.Count - 1 do
  begin
    AItem:= AList.Objects[I];

    if AItem.Find('Name', sName) and
       AItem.Find('Masks', sExtMask) and
       AItem.Find('Colors', AColors) and
       AItem.Find('Attributes', sAttr) then
    begin
      MaskItem := TMaskItem.Create;
      MaskItem.sName     := sName.AsString;
      MaskItem.FColor[0] := AColors.Integers[0];
      MaskItem.FColor[1] := AColors.Integers[1];
      MaskItem.sExt      := sExtMask.AsString;
      MaskItem.sModeStr  := sAttr.AsString;
      FMaskItems.Add(MaskItem);
    end;
  end;
end;

procedure TColorExt.Save(AConfig: TJSONObject);
var
  I: Integer;
  AList: TJSONArray;
  AItem: TJSONObject;
  AColors: TJSONArray;
  MaskItem: TMaskItem;
begin
  if not Assigned(FMaskItems) then
    Exit;

  if AConfig.Find('FileColors', AList) then
    AList.Clear
  else begin
    AList:= TJSONArray.Create;
    AConfig.Add('FileColors', AList);
  end;

  for I:= 0 to FMaskItems.Count - 1 do
  begin
    MaskItem := TMaskItem(FMaskItems[I]);
    AItem:= TJSONObject.Create;
    AItem.Add('Name', MaskItem.sName);
    AItem.Add('Masks', MaskItem.sExt);
    AColors:= TJSONArray.Create;
    AColors.Add(MaskItem.FColor[0]);
    AColors.Add(MaskItem.FColor[1]);
    AItem.Add('Colors', AColors);
    AItem.Add('Attributes', MaskItem.sModeStr);

    AList.Add(AItem);
  end;
end;

procedure TColorExt.UpdateStyle;
begin
  FStyle:= TColorThemes.StyleIndex;
end;

end.

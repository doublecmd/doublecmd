{
   Double Commander
   -------------------------------------------------------------------------
   Load colors of files in file panels

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2020 Alexander Koblov (alexx2000@mail.ru)
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
  Classes, Graphics, uFile, uMasks, uSearchTemplate, DCXmlConfig;

type

  { TMaskItem }

  TMaskItem = class
  private
    FExt: String;
    FModeStr: String;
    FMaskList: TMaskList;
    FAttrList: TMaskList;
    FTemplate: TSearchTemplate;
    procedure SetExt(const AValue: String);
    procedure SetModeStr(const AValue: String);
  public
    sName: String;

    cColor: TColor;

    constructor Create;
    destructor Destroy; override;

    procedure Assign(ASource: TMaskItem);
    property sExt: String read FExt write SetExt;
    property sModeStr: String read FModeStr write SetModeStr;
  end;

  { TColorExt }

  TColorExt = class
  private
    FMaskItems: TList;

    function GetCount: Integer;
    function GetItems(const Index: Integer): TMaskItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(AItem: TMaskItem);

    function GetColorByExt(const sExt: String): TColor;
    function GetColorByAttr(const sModeStr: String): TColor;
    function GetColorBy(const AFile: TFile): TColor;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode);

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMaskItem read GetItems; default;
  end;

implementation

uses
  SysUtils, uDebug, uGlobs, uFileProperty;

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

constructor TMaskItem.Create;
begin
  FMaskList:= TMaskList.Create(FExt);
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
  cColor := ASource.cColor;
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

function TColorExt.GetColorByExt(const sExt: String): TColor;
var
  I: Integer;
begin
  Result:= clDefault;
  for I:=0 to FMaskItems.Count-1 do
  begin
    if MatchesMaskList(sExt, TMaskItem(FMaskItems[I]).sExt,';') then
    begin
      Result:= TMaskItem(FMaskItems[I]).cColor;
      Exit;
    end;
  end;
end;

function TColorExt.GetColorByAttr(const sModeStr: String): TColor;
var
  I: Integer;
begin
  Result:= clDefault;
  for I:=0 to FMaskItems.Count-1 do
  begin
    if MatchesMaskList(sModeStr,TMAskItem(FMaskItems[I]).sModeStr,';') then
    begin
      Result:=TMAskItem(FMaskItems[I]).cColor;
      Exit;
    end;
  end;
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
        Result:= MaskItem.cColor;
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
        Result:= MaskItem.cColor;
        Exit;
      end;
  end;
end;

procedure TColorExt.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sExtMask,
  sAttr,
  sName: String;
  iColor: Integer;
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
          FMaskItems.Add(TMaskItem.Create);
          TMaskItem(FMaskItems.Last).sName    := sName;
          TMaskItem(FMaskItems.Last).cColor   := iColor;
          TMaskItem(FMaskItems.Last).sExt     := sExtMask;
          TMaskItem(FMaskItems.Last).sModeStr := sAttr;
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
var
  I : Integer;
  SubNode: TXmlNode;
begin
  if not Assigned(FMaskItems) then
    Exit;

  ANode := AConfig.FindNode(ANode, 'FileFilters', True);
  AConfig.ClearNode(ANode);

  for I:=0 to FMaskItems.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'Filter');
      AConfig.AddValue(SubNode, 'Name', TMaskItem(FMaskItems[I]).sName);
      AConfig.AddValue(SubNode, 'FileMasks', TMaskItem(FMaskItems[I]).sExt);
      AConfig.AddValue(SubNode, 'Color', TMaskItem(FMaskItems[I]).cColor);
      AConfig.AddValue(SubNode, 'Attributes', TMaskItem(FMaskItems[I]).sModeStr);
    end;
end;

end.

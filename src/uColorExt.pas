{
   Double Commander
   -------------------------------------------------------------------------
   Load colors of files in file panels

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)
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

interface

uses
  Classes, Graphics, uFile, uXmlConfig;

type

  { TMaskItem }

  TMaskItem = class
    sExt: String;
    sModeStr: String;
    cColor: TColor;
    sName: String;
  end;

  { TColorExt }

  TColorExt = class
  private
    fOldCount: Integer;
  protected
    lslist: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetColorByExt(const sExt: String): TColor;
    function GetColorByAttr(const sModeStr: String): TColor;
    function GetColorBy(const AFile: TFile): TColor;
    procedure LoadIni;
    procedure SaveIni;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode);
    property  MaskItemList: TList read lslist;
  end;

implementation

uses
  SysUtils, LCLProc, uGlobs, uMasks, uFileProperty;

constructor TColorExt.Create;
begin
  inherited;
  lslist:= TList.Create;
end;

destructor TColorExt.Destroy;
begin
  if assigned(lsList) then
    begin
      Clear;
      FreeAndNil(lsList);
    end;
end;

procedure TColorExt.Clear;
begin
  while lslist.Count > 0 do
    begin
      TMaskItem(lslist[0]).Free;
      lslist.Delete(0);
    end;
end;

function TColorExt.GetColorByExt(const sExt: String): TColor;
var I:integer;
begin
 Result:= -1;
// Result:= gForeColor; //$0000ff00;
 for I:=0 to lslist.Count-1 do
   begin
     if MatchesMaskList(sExt, TMaskItem(lslist[I]).sExt,';') then
       begin
         Result:= TMaskItem(lslist[I]).cColor;
         Exit;
       end;
   end;
end;

function TColorExt.GetColorByAttr(const sModeStr: String): TColor;
var I:Integer;
begin
  Result:= -1;
// Result:= gForeColor; //$0000ff00;
 for I:=0 to lslist.Count-1 do
   begin
     if MatchesMaskList(sModeStr,TMAskItem(lslist[I]).sModeStr,';') then
       begin
         Result:=TMAskItem(lslist[I]).cColor;
         exit;
       end;
   end;
end;

function TColorExt.GetColorBy(const AFile: TFile): TColor;
var
  I, J: Integer;
  MaskItem: TMaskItem;
begin
 Result:= -1;//gForeColor; //$0000ff00;
 for I:= 0 to lslist.Count-1 do
   begin
     MaskItem:= TMaskItem(lslist[I]);
     // get color by search template
     if MaskItem.sExt[1] = '>' then
       for J:= 0 to gSearchTemplateList.Count - 1 do
         with gSearchTemplateList do
         begin
           if (Templates[J].TemplateName = PChar(MaskItem.sExt)+1) and
              Templates[J].CheckFile(AFile) then
             begin
               Result:= MaskItem.cColor;
               Exit;
             end;
         end;

     // get color by extension and attribute
     if ((MaskItem.sExt     = '') or
          MatchesMaskList(AFile.Name, MaskItem.sExt , ';')) and
        ((MaskItem.sModeStr = '') or
          not (fpAttributes in AFile.SupportedProperties) or
          MatchesMaskList(AFile.Properties[fpAttributes].AsString, MaskItem.sModeStr, ';')) then
       begin
         Result:= MaskItem.cColor;
         Exit;
       end;
   end;

end;

(* Load colors of files from doublecmd.ini *)

{  format of colors storage as in Total Commander:
   doublecmd.ini
     [Colors]
     ColorFilter1=*.o;*.ppu;*.rst;*.bak;*.dcu
     ColorFilter1Color=16711680
     ColorFilter2=*.pas
     ColorFilter2Color=16711000
   etc...

Added Attributes:
 ColorFilter1Attributes=-r*xr*xr*x     //all read/executable file
 ColorFilter2Attributes=-*x*   //all executable
 ColorFilter3Attributes=d*     //all directories
 ColorFilter4Attributes=l*     //all links

 Be careful with * expression. Functions return just first found value.
 
 This is right demo of [Colors] section:
 ColorFilter3=*
 ColorFilter3Color=55758
 ColorFilter3Attributes=-rwxrwxr*x
 ColorFilter3Name=SomeName3
 ColorFilter4=*
 ColorFilter4Color=32768
 ColorFilter4Attributes=-*x*
 ColorFilter4Name=SomeName4

 This IS WRONG because ColorFilter3Attributes=-*x* will be
 found and ColorFilter3Color=32768 will be returned first:
 ColorFilter3=*
 ColorFilter3Color=32768
 ColorFilter3Attributes=-*x*
 ColorFilter3Name=SomeName3
 ColorFilter4=*
 ColorFilter4Color=55758
 ColorFilter4Attributes=-rwxrwxr*x
 ColorFilter4Name=SomeName4
 

!!! The "?" and other regular expressions DOES NOT SUPPORTED

}

procedure TColorExt.LoadIni;
var
  sExtMask,
  sAttr,
  sName: String;
  iColor,
  I : Integer;
begin
  I := 1;

  Clear;

  while gIni.ReadString('Colors', 'ColorFilter' + IntToStr(I), '') <> '' do
    begin
      sExtMask := gIni.ReadString('Colors', 'ColorFilter' + IntToStr(I), '');
      iColor := gIni.ReadInteger('Colors', 'ColorFilter' + IntToStr(I) + 'Color', clWindowText);
      sName:=gIni.ReadString('Colors', 'ColorFilter' + IntToStr(I)+'Name', '');
      sAttr := gIni.ReadString('Colors', 'ColorFilter' + IntToStr(I) + 'Attributes', '');

      lsList.Add(TMaskItem.Create);
      TMaskItem(lsList[lsList.Count-1]).sName:=sName;
      TMaskItem(lsList[lsList.Count-1]).cColor:=iColor;
      TMaskItem(lsList[lsList.Count-1]).sExt:=sExtMask;
      TMaskItem(lsList[lsList.Count-1]).sModeStr:=sAttr;

      fOldCount := I;
      Inc(I);
    end; // while gIni.ReadString();
end;

procedure TColorExt.SaveIni;
var
  I : Integer;
begin

  if (not assigned(lslist))  then exit;

  for I:=0 to lslist.Count - 1 do
    begin
      gIni.WriteString('Colors', 'ColorFilter' + IntToStr(I+1), TMaskItem(lsList[I]).sExt);
      gIni.WriteInteger('Colors', 'ColorFilter' + IntToStr(I+1) + 'Color', TMaskItem(lsList[I]).cColor);
      gIni.WriteString('Colors', 'ColorFilter' + IntToStr(I+1)+'Name', TMaskItem(lsList[I]).sName);
      gIni.WriteString('Colors', 'ColorFilter' + IntToStr(I+1) + 'Attributes', TMaskItem(lsList[I]).sModeStr);
    end;

  // delete old not used filters
  for I := lslist.Count + 1 to fOldCount do
    begin
      gIni.DeleteKey('Colors', 'ColorFilter' + IntToStr(I));
      gIni.DeleteKey('Colors', 'ColorFilter' + IntToStr(I) + 'Color');
      gIni.DeleteKey('Colors', 'ColorFilter' + IntToStr(I)+'Name');
      gIni.DeleteKey('Colors', 'ColorFilter' + IntToStr(I) + 'Attributes');
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
          lsList.Add(TMaskItem.Create);
          TMaskItem(lsList.Last).sName    := sName;
          TMaskItem(lsList.Last).cColor   := iColor;
          TMaskItem(lsList.Last).sExt     := sExtMask;
          TMaskItem(lsList.Last).sModeStr := sAttr;
        end
        else
        begin
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
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
  if not Assigned(lslist) then
    Exit;

  ANode := AConfig.FindNode(ANode, 'FileFilters', True);
  AConfig.ClearNode(ANode);

  for I:=0 to lslist.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'Filter');
      AConfig.AddValue(SubNode, 'Name', TMaskItem(lsList[I]).sName);
      AConfig.AddValue(SubNode, 'FileMasks', TMaskItem(lsList[I]).sExt);
      AConfig.AddValue(SubNode, 'Color', TMaskItem(lsList[I]).cColor);
      AConfig.AddValue(SubNode, 'Attributes', TMaskItem(lsList[I]).sModeStr);
    end;
end;

end.

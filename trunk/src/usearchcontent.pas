{
   Double Commander
   -------------------------------------------------------------------------
   Content plugin search control

   Copyright (C) 2014-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uSearchContent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, uFindFiles;

type

  { TPluginPanel }

  TPluginPanel = class(TPanel)
  private
   FPlugin,
   FField,
   FOperator,
   FValue,
   FUnit: TComboBox;
  private
    function GetCompare: TPluginOperator;
    function GetField: String;
    function GetFieldType: Integer;
    function GetPlugin: String;
    function GetUnitName: String;
    function GetValue: Variant;
    procedure PluginChange(Sender: TObject);
    procedure FieldChange(Sender: TObject);
    procedure SetCompare(AValue: TPluginOperator);
    procedure SetField(AValue: String);
    procedure SetPlugin(AValue: String);
    procedure SetUnitName(AValue: String);
    procedure SetValue(AValue: Variant);
    procedure SetComboBox(ComboBox: TComboBox; const Value, Error: String);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Plugin: String read GetPlugin write SetPlugin;
    property Field: String read GetField write SetField;
    property UnitName: String read GetUnitName write SetUnitName;
    property FieldType: Integer read GetFieldType;
    property Compare: TPluginOperator read GetCompare write SetCompare;
    property Value: Variant read GetValue write SetValue;
  end;

implementation

uses
  uLng, Variants, StrUtils, WdxPlugin, uGlobs, uWDXModule, Graphics, uShowMsg;

{ TPluginPanel }

function TPluginPanel.GetCompare: TPluginOperator;
begin
  Result:= TPluginOperator(PtrInt(FOperator.Items.Objects[FOperator.ItemIndex]));
end;

function TPluginPanel.GetField: String;
begin
  Result:= FField.Text;
end;

function TPluginPanel.GetFieldType: Integer;
begin
  Result:= PtrInt(FField.Items.Objects[FField.ItemIndex]);
end;

function TPluginPanel.GetPlugin: String;
begin
  Result:= FPlugin.Text;
end;

function TPluginPanel.GetUnitName: String;
begin
  Result:= FUnit.Text;
end;

function TPluginPanel.GetValue: Variant;
begin
  Result:= StrToVar(FValue.Text, PtrInt(FField.Items.Objects[FField.ItemIndex]));
end;

procedure TPluginPanel.PluginChange(Sender: TObject);
var
  I: Integer;
  Module: TWDXModule;
begin
  if FPlugin.ItemIndex < 0 then Exit;

  FField.Clear;
  Module:= gWdxPlugins.GetWdxModule(FPlugin.Text);
  if Assigned(Module) then
  for I:= 0 to  Module.FieldList.Count - 1 do
  begin
    FField.Items.AddObject(Module.FieldList[I], TObject(PtrInt(TWdxField(Module.FieldList.Objects[I]).FType)));
  end;
  if FField.Items.Count > 0 then
  begin
    FField.ItemIndex:= 0;
    FieldChange(FField);
  end;
end;

procedure TPluginPanel.FieldChange(Sender: TObject);
var
  I, J: Integer;
  sUnits: String;
  Module: TWDXModule;
begin
  FUnit.Items.Clear;
  FValue.Items.Clear;
  FOperator.Items.Clear;
  Module:= gWdxPlugins.GetWdxModule(FPlugin.Text);
  J:= Module.GetFieldIndex(FField.Text);
  if J < 0 then Exit;
  
  I:= TWdxField(Module.FieldList.Objects[J]).FType;
  if (I <> FT_MULTIPLECHOICE) then
  begin
    sUnits:= TWdxField(Module.FieldList.Objects[J]).FUnits;
    while sUnits <> EmptyStr do FUnit.Items.Add(Copy2SymbDel(sUnits, '|'));
  end;
  FUnit.Enabled := (I <> FT_MULTIPLECHOICE) AND (FUnit.Items.Count > 0);
  if FUnit.Enabled then FUnit.ItemIndex:= 0;

  case I of
  FT_NUMERIC_32,
  FT_NUMERIC_64,
  FT_NUMERIC_FLOATING,
  FT_DATE,
  FT_TIME,
  FT_DATETIME:
    begin
      FValue.Style:= csDropDown;
      FOperator.Items.AddObject('=', TObject(PtrInt(poEqualCaseSensitive)));
      FOperator.Items.AddObject('!=', TObject(PtrInt(poNotEqualCaseSensitive)));
      FOperator.Items.AddObject('>', TObject(PtrInt(poMore)));
      FOperator.Items.AddObject('<', TObject(PtrInt(poLess)));
      FOperator.Items.AddObject('>=', TObject(PtrInt(poMoreEqual)));
      FOperator.Items.AddObject('<=', TObject(PtrInt(poLessEqual)));
    end;
  FT_BOOLEAN:
    begin
      FValue.Items.Add(rsSimpleWordTrue);
      FValue.Items.Add(rsSimpleWordFalse);
      FValue.ItemIndex:= 0;
      FValue.Style:= csDropDownList;
      FOperator.Items.AddObject('=', TObject(PtrInt(poEqualCaseSensitive)));
    end;
  FT_MULTIPLECHOICE:
    begin
      begin
        FValue.Style:= csDropDownList;
        FOperator.Items.AddObject('=', TObject(PtrInt(poEqualCaseSensitive)));
        FOperator.Items.AddObject('!=', TObject(PtrInt(poNotEqualCaseSensitive)));
        sUnits:= TWdxField(Module.FieldList.Objects[J]).FUnits;
        while sUnits <> EmptyStr do
        begin
          FValue.Items.Add(Copy2SymbDel(sUnits, '|'));
        end;
        if FValue.Items.Count > 0 then FValue.ItemIndex:= 0;
      end;
    end;
  FT_STRING,
  FT_STRINGW:
    begin
      FValue.Style:= csDropDown;
      FOperator.Items.AddObject(rsPluginSearchEqualNotCase, TObject(PtrInt(poEqualCaseInsensitive)));
      FOperator.Items.AddObject(rsPluginSearchNotEqualNotCase, TObject(PtrInt(poNotEqualCaseInsensitive)));
      FOperator.Items.AddObject(rsPluginSearchEqualCaseSensitive, TObject(PtrInt(poEqualCaseSensitive)));
      FOperator.Items.AddObject(rsPluginSearchNotEquaCaseSensitive, TObject(PtrInt(poNotEqualCaseSensitive)));
      FOperator.Items.AddObject(rsPluginSearchContainsNotCase, TObject(PtrInt(poContainsCaseInsensitive)));
      FOperator.Items.AddObject(rsPluginSearchNotContainsNotCase, TObject(PtrInt(poNotContainsCaseInsensitive)));
      FOperator.Items.AddObject(rsPluginSearchContainsCaseSenstive, TObject(PtrInt(poContainsCaseSensitive)));
      FOperator.Items.AddObject(rsPluginSearchNotContainsCaseSenstive, TObject(PtrInt(poNotContainsCaseSensitive)));
      FOperator.Items.AddObject(rsPluginSearchRegExpr, TObject(PtrInt(poRegExpr)));
      FOperator.Items.AddObject(rsPluginSearchNotRegExpr, TObject(PtrInt(poNotRegExpr)));
    end;
  FT_FULLTEXT,
  FT_FULLTEXTW:
    begin
      FValue.Style:= csDropDown;
      FOperator.Items.AddObject(rsPluginSearchContainsNotCase, TObject(PtrInt(poContainsCaseInsensitive)));
      FOperator.Items.AddObject(rsPluginSearchNotContainsNotCase, TObject(PtrInt(poNotContainsCaseInsensitive)));
      FOperator.Items.AddObject(rsPluginSearchContainsCaseSenstive, TObject(PtrInt(poContainsCaseSensitive)));
      FOperator.Items.AddObject(rsPluginSearchNotContainsCaseSenstive, TObject(PtrInt(poNotContainsCaseSensitive)));
    end;
  end;
  if FOperator.Items.Count > 0 then FOperator.ItemIndex:= 0;
end;

procedure TPluginPanel.SetCompare(AValue: TPluginOperator);
var
  Index: Integer;
begin
  Index:= FOperator.Items.IndexOfObject(TObject(PtrInt(AValue)));
  if Index >= 0 then FOperator.ItemIndex:= Index;
end;

procedure TPluginPanel.SetField(AValue: String);
begin
  SetComboBox(FField, AValue, Format(rsPluginSearchFieldNotFound, [AValue]));
end;

procedure TPluginPanel.SetPlugin(AValue: String);
begin
  SetComboBox(FPlugin, AValue, Format(rsPluginSearchPluginNotFound, [AValue]));
end;

procedure TPluginPanel.SetUnitName(AValue: String);
begin
  if FUnit.Enabled then
    SetComboBox(FUnit, AValue, Format(rsPluginSearchUnitNotFoundForField, [AValue, Self.Field]));
end;

procedure TPluginPanel.SetValue(AValue: Variant);
begin
  if not VarIsBool(AValue) then
    FValue.Text := AValue
  else
    if AValue then
      FValue.Text := rsSimpleWordTrue
    else
      FValue.Text := rsSimpleWordFalse;
end;

procedure TPluginPanel.SetComboBox(ComboBox: TComboBox; const Value,
  Error: String);
var
  Index: Integer;
begin
  Index:= ComboBox.Items.IndexOf(Value);
  if Index < 0 then
    msgError(Error)
  else begin
    ComboBox.ItemIndex:= Index;
    if Assigned(ComboBox.OnChange) then ComboBox.OnChange(ComboBox);
  end;
end;

constructor TPluginPanel.Create(TheOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(TheOwner);
  AutoSize:= True;
  BevelOuter:= bvNone;
  ChildSizing.ControlsPerLine:= 5;
  ChildSizing.Layout:= cclLeftToRightThenTopToBottom;
  ChildSizing.EnlargeHorizontal:= crsScaleChilds;

  FPlugin:= TComboBox.Create(Self);
  FPlugin.Parent:= Self;
  FPlugin.Style:= csDropDownList;
  FPlugin.OnChange:= @PluginChange;

  FField:= TComboBox.Create(Self);
  FField.Parent:= Self;
  FField.Style:= csDropDownList;
  FField.OnChange:= @FieldChange;

  FOperator:= TComboBox.Create(Self);
  FOperator.Parent:= Self;
  FOperator.Style:= csDropDownList;

  FValue:= TComboBox.Create(Self);
  FValue.Parent:= Self;

  FUnit:= TComboBox.Create(Self);
  FUnit.Style:= csDropDownList;
  FUnit.Parent:= Self;

  for I:= 0 to gWDXPlugins.Count - 1do
  begin
    if gWdxPlugins.GetWdxModule(I).IsLoaded or gWdxPlugins.GetWdxModule(I).LoadModule then
    begin
      FPlugin.Items.Add(gWdxPlugins.GetWdxModule(I).Name);
    end;
  end;
  if FPlugin.Items.Count > 0 then
  begin
    FPlugin.ItemIndex:= 0;
    PluginChange(FPlugin);
  end;
end;

destructor TPluginPanel.Destroy;
begin
  FPlugin.Free;
  FField.Free;
  FOperator.Free;
  FValue.Free;
  FUnit.Free;
  inherited Destroy;
end;

end.


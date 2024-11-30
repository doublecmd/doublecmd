{
   Double Commander
   -------------------------------------------------------------------------
   Content plugin search control

   Copyright (C) 2014-2023 Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, LCLType, uFindFiles,
  EditBtn, DateTimePicker;

type

  { TPluginPanel }

  TPluginPanel = class(TPanel)
  private
   FComboPlugin,
   FComboField, // <---The text of this combo is filled from localized string. The "objects" pointed from the its list are "TWdxField" type.
   FComboOperator,
   FComboValue,
   FComboUnit: TComboBox;
   FEditValue: TCalcEdit;
   FDateTimeValue: TDateTimePicker;
   FValuePanel: TPanel;
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
    procedure ValueKeyPress(Sender: TObject; var Key: Char);
    procedure ValueUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ComboValueKeyPress(Sender: TObject; var Key: Char);
    procedure ComboValueUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSizes(ColumnWidth: Integer);
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
  DateUtils, uLng, Variants, WdxPlugin, uGlobs, uWDXModule, Graphics, uShowMsg;

{ TPluginPanel }

function TPluginPanel.GetCompare: TPluginOperator;
begin
  Result:= TPluginOperator(PtrInt(FComboOperator.Items.Objects[FComboOperator.ItemIndex]));
end;

function TPluginPanel.GetField: String;
begin
  Result := TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]).FName;
end;

function TPluginPanel.GetFieldType: Integer;
begin
  Result := TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]).FType
end;

function TPluginPanel.GetPlugin: String;
begin
  Result:= FComboPlugin.Text;
end;

function TPluginPanel.GetUnitName: String;
begin
  if (FComboField.ItemIndex < 0) or (FComboUnit.ItemIndex < 0) then
    Result:= FComboUnit.Text
  else begin
    Result:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]).FUnits[FComboUnit.ItemIndex];
  end;
end;

function TPluginPanel.GetValue: Variant;
var
  WdxField: TWdxField;
begin
  WdxField:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]);
  if (WdxField.FType <> ft_multiplechoice) then
    case WdxField.FType of
    FT_NUMERIC_32:
      result:= FEditValue.AsInteger;
    FT_NUMERIC_64:
      result:= round(FEditValue.AsFloat);
    FT_NUMERIC_FLOATING:
      result:= FEditValue.AsFloat;
    FT_DATE:
      result:= FDateTimeValue.Date;
    FT_TIME:
      result:= FDateTimeValue.Time;
    FT_DATETIME:
      result:= FDateTimeValue.DateTime;
    FT_BOOLEAN,
    FT_STRING,
    FT_STRINGW,
    FT_FULLTEXT,
    FT_FULLTEXTW:
      Result:= StrToVar(FComboValue.Text, WdxField.FType)
    end
  else begin
    Result:= StrToVar(WdxField.FUnits[FComboValue.ItemIndex], WdxField.FType)
  end;
end;

// When a plugin is selected from the plugin combo, we populate the others in cascade.
// In the field combo, each element references a corresding "TWdxField" object.
// Since this combo is for the user we populate text of it from localized strings.
procedure TPluginPanel.PluginChange(Sender: TObject);
var
  I: Integer;
  Module: TWDXModule;
begin
  if FComboPlugin.ItemIndex < 0 then Exit;

  FComboField.Clear;
  Module:= gWdxPlugins.GetWdxModule(FComboPlugin.Text);
  if Assigned(Module) then
    for I:= 0 to  Module.FieldList.Count - 1 do
      FComboField.Items.AddObject(TWdxField(Module.FieldList.Objects[I]).LName, TObject(Module.FieldList.Objects[I]));

  if FComboField.Items.Count > 0 then
  begin
    FComboField.ItemIndex:= 0;
    FieldChange(FComboField);
  end;
end;

procedure TPluginPanel.FieldChange(Sender: TObject);
var
  WdxField: TWdxField;
begin
  FComboUnit.Clear;
  FComboValue.Items.Clear;
  FComboOperator.Items.Clear;
  FComboValue.Text:= EmptyStr;
  FComboValue.Visible:= True;
  FDateTimeValue.DateTime:= RecodeMilliSecond(Now, 0);
  FDateTimeValue.Visible:= False;
  FEditValue.Text:= EmptyStr;
  FEditValue.Visible:= False;
  if (FComboField.ItemIndex < 0) then Exit;

  WdxField:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]);

  if (WdxField.FType <> FT_MULTIPLECHOICE) then
  begin
    FComboUnit.Items.AddStrings(WdxField.LUnits);
  end;
  FComboUnit.Visible := (WdxField.FType <> FT_MULTIPLECHOICE) and (FComboUnit.Items.Count > 0);
  if FComboUnit.Visible then FComboUnit.ItemIndex:= 0;

  case WdxField.FType of
  FT_NUMERIC_32,
  FT_NUMERIC_64,
  FT_NUMERIC_FLOATING,
  FT_DATE,
  FT_TIME,
  FT_DATETIME:
    begin
      FComboValue.Visible:= False;
      if (WdxField.FType > FT_NUMERIC_FLOATING) then
      begin
        FDateTimeValue.Visible:= True;
        if (WdxField.FType = FT_DATETIME) then
          FDateTimeValue.Kind:= dtkDateTime
        else if (WdxField.FType = FT_TIME) then
          FDateTimeValue.Kind:= dtkTime
        else
          FDateTimeValue.Kind:= dtkDate
      end
      else
      begin
        FEditValue.Visible:= True;;
      end;
      FComboOperator.Items.AddObject('=', TObject(PtrInt(poEqualCaseSensitive)));
      FComboOperator.Items.AddObject('!=', TObject(PtrInt(poNotEqualCaseSensitive)));
      FComboOperator.Items.AddObject('>', TObject(PtrInt(poMore)));
      FComboOperator.Items.AddObject('<', TObject(PtrInt(poLess)));
      FComboOperator.Items.AddObject('>=', TObject(PtrInt(poMoreEqual)));
      FComboOperator.Items.AddObject('<=', TObject(PtrInt(poLessEqual)));
    end;
  FT_BOOLEAN:
    begin
      FComboValue.Items.Add(rsSimpleWordTrue);
      FComboValue.Items.Add(rsSimpleWordFalse);
      FComboValue.ItemIndex:= 0;
      FComboValue.Style:= csDropDownList;
      FComboOperator.Items.AddObject('=', TObject(PtrInt(poEqualCaseSensitive)));
    end;
  FT_MULTIPLECHOICE:
    begin
      begin
        FComboValue.Style:= csDropDownList;
        FComboOperator.Items.AddObject('=', TObject(PtrInt(poEqualCaseSensitive)));
        FComboOperator.Items.AddObject('!=', TObject(PtrInt(poNotEqualCaseSensitive)));
        FComboValue.Items.AddStrings(WdxField.LUnits);
        if FComboValue.Items.Count > 0 then FComboValue.ItemIndex:= 0;
      end;
    end;
  FT_STRING,
  FT_STRINGW:
    begin
      FComboValue.Style:= csDropDown;
      FComboValue.Items := glsSearchHistory;
      FComboOperator.Items.AddObject(rsPluginSearchEqualNotCase, TObject(PtrInt(poEqualCaseInsensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchNotEqualNotCase, TObject(PtrInt(poNotEqualCaseInsensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchEqualCaseSensitive, TObject(PtrInt(poEqualCaseSensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchNotEquaCaseSensitive, TObject(PtrInt(poNotEqualCaseSensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchContainsNotCase, TObject(PtrInt(poContainsCaseInsensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchNotContainsNotCase, TObject(PtrInt(poNotContainsCaseInsensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchContainsCaseSenstive, TObject(PtrInt(poContainsCaseSensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchNotContainsCaseSenstive, TObject(PtrInt(poNotContainsCaseSensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchRegExpr, TObject(PtrInt(poRegExpr)));
      FComboOperator.Items.AddObject(rsPluginSearchNotRegExpr, TObject(PtrInt(poNotRegExpr)));
    end;
  FT_FULLTEXT,
  FT_FULLTEXTW:
    begin
      FComboValue.Style:= csDropDown;
      FComboValue.Items := glsSearchHistory;
      FComboOperator.Items.AddObject(rsPluginSearchContainsNotCase, TObject(PtrInt(poContainsCaseInsensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchNotContainsNotCase, TObject(PtrInt(poNotContainsCaseInsensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchContainsCaseSenstive, TObject(PtrInt(poContainsCaseSensitive)));
      FComboOperator.Items.AddObject(rsPluginSearchNotContainsCaseSenstive, TObject(PtrInt(poNotContainsCaseSensitive)));
    end;
  end;
  if FComboOperator.Items.Count > 0 then FComboOperator.ItemIndex:= 0;
end;

procedure TPluginPanel.SetCompare(AValue: TPluginOperator);
var
  Index: Integer;
begin
  Index:= FComboOperator.Items.IndexOfObject(TObject(PtrInt(AValue)));
  if Index >= 0 then FComboOperator.ItemIndex:= Index;
end;

// The "AValue" parameter received here is not localized.
// We can't search it in combo box directly so we go by index.
procedure TPluginPanel.SetField(AValue: String);
var
  Module: TWDXModule;
begin
  Module := gWdxPlugins.GetWdxModule(FComboPlugin.Text);
  if Module = nil then exit;

  FComboField.ItemIndex := Module.GetFieldIndex(AValue);
  if FComboField.ItemIndex <> -1 then
  begin
    if Assigned(FComboField.OnChange) then FComboField.OnChange(FComboField);
  end
  else begin
    msgError(rsPluginSearchFieldNotFound);
  end;
end;

procedure TPluginPanel.SetPlugin(AValue: String);
begin
  SetComboBox(FComboPlugin, AValue, Format(rsPluginSearchPluginNotFound, [AValue]));
end;

procedure TPluginPanel.SetUnitName(AValue: String);
var
  Index: Integer;
  WdxField: TWdxField;
begin
  if FComboUnit.Visible then
  begin
    WdxField:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]);
    Index := WdxField.GetUnitIndex(AValue);
    if Index >= 0 then AValue:= WdxField.LUnits[Index];
    SetComboBox(FComboUnit, AValue, Format(rsPluginSearchUnitNotFoundForField, [AValue, Self.Field]));
  end;
end;

procedure TPluginPanel.SetValue(AValue: Variant);
var
  Index: Integer;
  WdxField: TWdxField;
begin
  if VarIsBool(AValue) then
  begin
    if AValue then
      FComboValue.Text := rsSimpleWordTrue
    else
      FComboValue.Text := rsSimpleWordFalse;
  end
  else begin
    WdxField:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]);
    if (WdxField.FType <> FT_MULTIPLECHOICE) then
    begin
      if (WdxField.FType < FT_DATE) then
        FEditValue.Text:= AValue
      else if (WdxField.FType in [FT_DATE, FT_TIME, FT_DATETIME]) then
        FDateTimeValue.DateTime:= AValue
      else
        FComboValue.Text := AValue;
    end
    else begin
      Index:= WdxField.GetUnitIndex(AValue);
      if Index < 0 then
        FComboValue.Text := AValue
      else
        FComboValue.Text := WdxField.LUnits[Index];
    end;
  end;
end;

procedure TPluginPanel.SetComboBox(ComboBox: TComboBox; const Value, Error: String);
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

procedure TPluginPanel.ValueKeyPress(Sender: TObject; var Key: Char);
var
  WdxField: TWdxField;
begin
  WdxField:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]);
  case WdxField.FType of
    FT_NUMERIC_32,
    FT_NUMERIC_64:
      begin
        if not (Key in ['0'..'9', Chr(VK_BACK)]) then
          Key:= #0;
      end;
  FT_NUMERIC_FLOATING:
    begin
      if not (Key in ['0'..'9', Chr(VK_BACK), DefaultFormatSettings.DecimalSeparator]) then
        Key:= #0;
    end;
  end;
end;

procedure TPluginPanel.ValueUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  WdxField: TWdxField;
begin
  WdxField:= TWdxField(FComboField.Items.Objects[FComboField.ItemIndex]);
  case WdxField.FType of
    FT_NUMERIC_32,
    FT_NUMERIC_64,
    FT_NUMERIC_FLOATING:
    begin
      if (Length(UTF8Key) > 1) then UTF8Key:= #0;
    end;
  end;
end;

procedure TPluginPanel.ComboValueKeyPress(Sender: TObject; var Key: Char);
var
  WdxField: TWdxField;
begin
  if (FComboField.ItemIndex < 0) then Exit;
  ValueKeyPress(Sender, Key);
end;

procedure TPluginPanel.ComboValueUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  WdxField: TWdxField;
begin
  if (FComboField.ItemIndex < 0) then Exit;
  ValueUTF8KeyPress(Sender, UTF8Key);
end;

procedure TPluginPanel.UpdateSizes(ColumnWidth: Integer);
begin
  FComboPlugin.Constraints.MinWidth:= ColumnWidth;
  FComboPlugin.Constraints.MaxWidth:= ColumnWidth;
  FComboField.Constraints.MinWidth:= ColumnWidth;
  FComboField.Constraints.MaxWidth:= ColumnWidth;
  FComboOperator.Constraints.MinWidth:= ColumnWidth;
  FComboOperator.Constraints.MaxWidth:= ColumnWidth;
  FValuePanel.Constraints.MinWidth:= ColumnWidth * 2;
  FValuePanel.Constraints.MaxWidth:= ColumnWidth * 2;
end;

constructor TPluginPanel.Create(TheOwner: TComponent);
var
  I, ColumnWidth: Integer;
begin
  inherited Create(TheOwner);
  AutoSize:= True;
  BevelOuter:= bvNone;

  FComboPlugin:= TComboBox.Create(Self);
  FComboPlugin.Parent:= Self;
  FComboPlugin.Style:= csDropDownList;
  FComboPlugin.OnChange:= @PluginChange;

  FComboField:= TComboBox.Create(Self);
  FComboField.Parent:= Self;
  FComboField.Style:= csDropDownList;
  FComboField.OnChange:= @FieldChange;

  FComboOperator:= TComboBox.Create(Self);
  FComboOperator.Parent:= Self;
  FComboOperator.Style:= csDropDownList;

  FValuePanel:= TPanel.Create(Self);
  FValuePanel.Parent:= Self;
  FValuePanel.AutoSize:= True;
  FValuePanel.BevelOuter:= bvNone;
  FValuePanel.ChildSizing.ControlsPerLine:= 2;
  FValuePanel.ChildSizing.Layout:= cclLeftToRightThenTopToBottom;
  FValuePanel.ChildSizing.EnlargeHorizontal:= crsScaleChilds;
  FValuePanel.ChildSizing.EnlargeVertical:= crsScaleChilds;

  FComboValue:= TComboBox.Create(Self);
  FComboValue.OnKeyPress:= @ComboValueKeyPress;
  FComboValue.OnUTF8KeyPress:= @ComboValueUTF8KeyPress;
  FComboValue.Parent:= FValuePanel;

  FEditValue:= TCalcEdit.Create(Self);
  FEditValue.OnKeyPress:= @ValueKeyPress;
  FEditValue.OnUTF8KeyPress:= @ValueUTF8KeyPress;
  FEditValue.Parent:= FValuePanel;

  FDateTimeValue:= TDateTimePicker.Create(Self);
  FDateTimeValue.Parent:= FValuePanel;
  FDateTimeValue.BorderSpacing.CellAlignVertical:=ccaCenter;
  FDateTimeValue.BorderSpacing.CellAlignHorizontal:=ccaCenter;

  FComboUnit:= TComboBox.Create(Self);
  FComboUnit.Style:= csDropDownList;
  FComboUnit.Parent:= FValuePanel;

  FComboPlugin.AnchorVerticalCenterTo(FValuePanel);
  FComboField.AnchorVerticalCenterTo(FValuePanel);
  FComboField.AnchorToNeighbour(akLeft, 0, FComboPlugin);
  FComboOperator.AnchorVerticalCenterTo(FValuePanel);
  FComboOperator.AnchorToNeighbour(akLeft, 0, FComboField);
  FValuePanel.AnchorToNeighbour(akLeft, 0, FComboOperator);
  ColumnWidth:= TControl(TheOwner).ClientWidth div 5;
  UpdateSizes(ColumnWidth);

  for I:= 0 to gWDXPlugins.Count - 1do
  begin
    if gWdxPlugins.GetWdxModule(I).IsLoaded or gWdxPlugins.GetWdxModule(I).LoadModule then
    begin
      FComboPlugin.Items.Add(gWdxPlugins.GetWdxModule(I).Name);
    end;
  end;
  if FComboPlugin.Items.Count > 0 then
  begin
    FComboPlugin.ItemIndex:= 0;
    PluginChange(FComboPlugin);
  end;
end;

destructor TPluginPanel.Destroy;
begin
  FComboPlugin.Free;
  FComboField.Free;
  FComboOperator.Free;
  FValuePanel.Free;
  FComboValue.Free;
  FEditValue.Free;
  FDateTimeValue.Free;
  FComboUnit.Free;
  inherited Destroy;
end;

end.


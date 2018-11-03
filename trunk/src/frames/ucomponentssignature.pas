{
   Double Commander
   -------------------------------------------------------------------------
   Compute signature of a form, frame, etc. based on current options set

   Copyright (C) 2016-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit uComponentsSignature;

{$mode objfpc}{$H+}

interface

uses
  Classes, EditBtn;

function ComputeSignatureBasedOnComponent(aComponent: TComponent; seed: dword): dword;
function ComputeSignatureSingleComponent(aComponent: TComponent; seed: dword): dword;
function ComputeSignatureString(seed: dword; sParamString: string): dword;
function ComputeSignatureBoolean(seed: dword; bParamBoolean: boolean): dword;
function ComputeSignaturePtrInt(seed: dword; iPtrInt: PtrInt): dword;

implementation

uses
  Graphics, ComCtrls, ColorBox, ExtCtrls, Spin, StdCtrls, Math, Dialogs,
  SysUtils, crc;
const
  SAMPLEBYTES: array[0..1] of byte = ($23, $35);

{ ComputeSignatureSingleComponent }
function ComputeSignatureSingleComponent(aComponent: TComponent; seed: dword): dword;
var
  SampleValue: dword;
  iSampleValue, iIndex: integer;
  ColorSampleValue: TColor;
begin
  Result := seed;

  case aComponent.ClassName of
    'TCheckBox':
      Result := crc32(Result, @SAMPLEBYTES[ifthen(TCheckBox(aComponent).Checked, 1, 0)], 1);

    'TRadioGroup':
    begin
      SampleValue := TRadioGroup(aComponent).ItemIndex;
      Result := crc32(Result, @SampleValue, sizeof(SampleValue));
    end;

    'TRadioButton':
    begin
      Result := crc32(Result, @SAMPLEBYTES[ifthen(TRadioButton(aComponent).Checked, 1, 0)], 1);
    end;

    'TEdit':
      if length(TEdit(aComponent).Text) > 0 then
        Result := crc32(Result, @TEdit(aComponent).Text[1], length(TEdit(aComponent).Text));

    'TLabeledEdit':
    begin
      if length(TLabeledEdit(aComponent).Text) > 0 then
        Result := crc32(Result, @TLabeledEdit(aComponent).Text[1], length(TLabeledEdit(aComponent).Text));
    end;

    'TFileNameEdit':
      if length(TFileNameEdit(aComponent).FileName) > 0 then
        Result := crc32(Result, @TFileNameEdit(aComponent).FileName[1], length(TFileNameEdit(aComponent).FileName));

    'TDirectoryEdit':
      if length(TDirectoryEdit(aComponent).Text) > 0 then
        Result := crc32(Result, @TDirectoryEdit(aComponent).Text[1], length(TDirectoryEdit(aComponent).Text));

    'TComboBox', 'TComboBoxAutoWidth':
    begin
      if TComboBox(aComponent).ItemIndex <> -1 then
      begin
        SampleValue := TComboBox(aComponent).ItemIndex;
        Result := crc32(Result, @SampleValue, sizeof(SampleValue));
      end;

      if TComboBox(aComponent).Style <> csDropDownList then
      begin
        if length(TComboBox(aComponent).Text) > 0 then
          Result := crc32(Result, @TComboBox(aComponent).Text[1], length(TComboBox(aComponent).Text));
      end;
    end;

    'TSpinEdit':
    begin
      SampleValue := TSpinEdit(aComponent).Value;
      Result := crc32(Result, @SampleValue, sizeof(SampleValue));
    end;

    'TColorBox':
    begin
      ColorSampleValue := TColorBox(aComponent).Selected;
      Result := crc32(Result, @ColorSampleValue, 4);
    end;

    'TTrackBar':
    begin
      iSampleValue := TTrackBar(aComponent).Position;
      Result := crc32(Result, @iSampleValue, 4);
    end;

    'TListBox':
    begin
      if not TListBox(aComponent).MultiSelect then
      begin
        iSampleValue := TListBox(aComponent).ItemIndex;
        Result := crc32(Result, @iSampleValue, sizeof(iSampleValue));
      end;
    end;

    'TMemo':
    begin
      SampleValue := TMemo(aComponent).Lines.Count;
      Result := crc32(Result, @SampleValue, sizeof(SampleValue));
      for iIndex:=0 to pred(TMemo(aComponent).Lines.Count) do
        begin
          if length(TMemo(aComponent).Lines.Strings[iIndex]) > 0 then
            Result := crc32(Result, @TMemo(aComponent).Lines.Strings[iIndex][1], length(TMemo(aComponent).Lines.Strings[iIndex]));
        end;
    end;

  end;
end;

{ ComputeSignatureBasedOnComponent }
function ComputeSignatureBasedOnComponent(aComponent: TComponent; seed: dword): dword;
var
  iComponent: integer;
begin
  Result := ComputeSignatureSingleComponent(aComponent, seed);
  case aComponent.ClassName of
    'TRadioGroup': begin end; // Nothing. Because if we go inside, we'll analyse *always* ALL unchecked "TRadioButton" after load but they're not when it's time to save them.
    else
      begin
        for iComponent := 0 to pred(aComponent.ComponentCount) do
          Result := ComputeSignatureBasedOnComponent(aComponent.Components[iComponent], Result)
      end;
  end;
end;

{ ComputeSignatureString }
function ComputeSignatureString(seed: dword; sParamString: string): dword;
begin
  result := seed;
  if length(sParamString) > 0 then result := crc32(result, @sParamString[1], length(sParamString));
end;

{ ComputeSignatureBoolean }
function ComputeSignatureBoolean(seed: dword; bParamBoolean: boolean): dword;
const
  SAMPLEBYTES: array[0..1] of byte = ($23, $35);
begin
  result := crc32(seed, @SAMPLEBYTES[ifthen(bParamBoolean, 1, 0)], 1);
end;

{ ComputeSignaturePtrInt }
function ComputeSignaturePtrInt(seed: dword; iPtrInt: PtrInt): dword;
begin
  result := crc32(seed, @iPtrInt, sizeof(PtrInt));
end;

end.








{
   Double Commander
   -------------------------------------------------------------------------
   Compute signature of a form, frame, etc. based on current options set

   Copyright (C) 2016  Alexander Koblov (alexx2000@mail.ru)

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

unit uComponentsSignature;

{$mode objfpc}{$H+}

interface

uses
  Classes, EditBtn;
function ComputeSignatureBasedOnComponent(aComponent: TComponent; seed: dword): dword;
function ComputeSignatureSingleComponent(aComponent: TComponent; seed: dword): dword;

implementation

uses
  //uDebug,
  Graphics, ComCtrls, ColorBox, ExtCtrls, Spin, StdCtrls, Math, Dialogs,
  SysUtils, crc;
const
  SAMPLEBYTES: array[0..1] of byte = ($23, $35);

{ ComputeSignatureSingleComponent }
function ComputeSignatureSingleComponent(aComponent: TComponent; seed: dword): dword;
var
  SampleValue: dword;
  iSampleValue: integer;
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

    //'TRadioButton': 2016-01-20:DB-With LAZ 1.4.4 and FPC 2.6.4 and in Windows 7 64-bits, it looks like after the .LOAD, the RadioButton are all "false"... That's why we'll check the "itemIndex" of the TRadioGroup instead.

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

    'TComboBox':
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
  end;
end;

{ ComputeSignatureBasedOnComponent }
function ComputeSignatureBasedOnComponent(aComponent: TComponent; seed: dword): dword;
var
  iComponent: integer;
begin
  Result := seed;

  for iComponent := 0 to pred(aComponent.ComponentCount) do
  begin
    if aComponent.Components[iComponent].ComponentCount > 0 then
      Result := ComputeSignatureBasedOnComponent(aComponent.Components[iComponent], Result)
    else
      Result := ComputeSignatureSingleComponent(aComponent.Components[iComponent], Result);
  end;
end;


end.








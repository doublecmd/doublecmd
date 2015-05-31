{
   Double Commander
   -------------------------------------------------------------------------
   Compute signature of a form, frame, etc. based on current options set

   Copyright (C) 2015  Alexander Koblov (alexx2000@mail.ru)

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

implementation

uses
  Graphics, ComCtrls, ColorBox, ExtCtrls, Spin, StdCtrls, Math, Dialogs,
  SysUtils, crc;

const
  SAMPLEBYTES: array[0..1] of byte = ($23, $35);

function ComputeSignatureBasedOnComponent(aComponent: TComponent; seed: dword): dword;
var
  iComponent: integer;
  SampleValue: dword;
  iSampleValue: integer;
  ColorSampleValue: TColor;
begin
  Result := seed;

  for iComponent := 0 to pred(aComponent.ComponentCount) do
  begin
    if aComponent.Components[iComponent].ComponentCount > 0 then
      Result := ComputeSignatureBasedOnComponent(aComponent.Components[iComponent], Result);

    case aComponent.Components[iComponent].ClassName of
      'TCheckBox':
        Result := crc32(Result, @SAMPLEBYTES[ifthen(TCheckBox(aComponent.Components[iComponent]).Checked, 1, 0)], 1);

      'TRadioButton':
        Result := crc32(Result, @SAMPLEBYTES[ifthen(TRadioButton(aComponent.Components[iComponent]).Checked, 1, 0)], 1);

      'TEdit':
        if length(TEdit(aComponent.Components[iComponent]).Text) > 0 then
          Result := crc32(Result, @TEdit(aComponent.Components[iComponent]).Text[1], length(TEdit(aComponent.Components[iComponent]).Text));

      'TLabeledEdit':
      begin
        if length(TLabeledEdit(aComponent.Components[iComponent]).Text) > 0 then
          Result := crc32(Result, @TLabeledEdit(aComponent.Components[iComponent]).Text[1], length(TLabeledEdit(aComponent.Components[iComponent]).Text));
      end;

      'TFileNameEdit':
        if length(TFileNameEdit(aComponent.Components[iComponent]).FileName) > 0 then
          Result := crc32(Result, @TFileNameEdit(aComponent.Components[iComponent]).FileName[1], length(TFileNameEdit(aComponent.Components[iComponent]).FileName));

      'TComboBox':
      begin
        SampleValue := TComboBox(aComponent.Components[iComponent]).ItemIndex;
        Result := crc32(Result, @SampleValue, sizeof(SampleValue));

        if length(TComboBox(aComponent.Components[iComponent]).Text) > 0 then
          Result := crc32(Result, @TComboBox(aComponent.Components[iComponent]).Text[1], length(TComboBox(aComponent.Components[iComponent]).Text));
      end;

      'TSpinEdit':
      begin
        SampleValue := TSpinEdit(aComponent.Components[iComponent]).Value;
        Result := crc32(Result, @SampleValue, sizeof(SampleValue));
      end;

      'TColorBox':
      begin
        ColorSampleValue := TColorBox(aComponent.Components[iComponent]).Selected;
        Result := crc32(Result, @ColorSampleValue, 4);
      end;

      'TTrackBar':
      begin
        iSampleValue := TTrackBar(aComponent.Components[iComponent]).Position;
        Result := crc32(Result, @iSampleValue, 4);
      end;
    end;
  end;
end;


end.






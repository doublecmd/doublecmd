{
   Double Commander
   -------------------------------------------------------------------------
   Brief view options page

   Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

unit fOptionsBriefView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls, ExtCtrls, Spin;

type

  { TfrmOptionsBriefView }

  TfrmOptionsBriefView = class(TOptionsEditor)
    gbShowFileExt: TGroupBox;
    gbColumns: TGroupBox;
    rbUseFixedWidth: TRadioButton;
    rbUseFixedCount: TRadioButton;
    rbUseAutoSize: TRadioButton;
    rbDirectly: TRadioButton;
    rbAligned: TRadioButton;
    speUseFixedWidth: TSpinEdit;
    speUseFixedCount: TSpinEdit;
    lblStub: TLabel;
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

{ TfrmOptionsBriefView }

procedure TfrmOptionsBriefView.Load;
begin
  rbAligned.Checked := gBriefViewFileExtAligned;
  speUseFixedWidth.Value := gBriefViewFixedWidth;
  speUseFixedCount.Value := gBriefViewFixedCount;
  rbUseAutoSize.Checked := gBriefViewMode = bvmAutoSize;
  rbUseFixedWidth.Checked := gBriefViewMode = bvmFixedWidth;
  rbUseFixedCount.Checked := gBriefViewMode = bvmFixedCount;
end;

function TfrmOptionsBriefView.Save: TOptionsEditorSaveFlags;
begin
  gBriefViewFileExtAligned := rbAligned.Checked;
  gBriefViewFixedWidth := speUseFixedWidth.Value;
  gBriefViewFixedCount := speUseFixedCount.Value;
  if rbUseAutoSize.Checked then gBriefViewMode := bvmAutoSize;
  if rbUseFixedWidth.Checked then gBriefViewMode := bvmFixedWidth;
  if rbUseFixedCount.Checked then gBriefViewMode := bvmFixedCount;
  Result := [];
end;

class function TfrmOptionsBriefView.GetIconIndex: Integer;
begin
  Result := 35;
end;

class function TfrmOptionsBriefView.GetTitle: String;
begin
  Result := rsOptionsEditorBriefView;
end;

end.


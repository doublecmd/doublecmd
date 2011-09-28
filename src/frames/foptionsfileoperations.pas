{
   Double Commander
   -------------------------------------------------------------------------
   File operations options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsFileOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Spin,
  fOptionsFrame;

type

  { TfrmOptionsFileOperations }

  TfrmOptionsFileOperations = class(TOptionsEditor)
    cbDeleteToTrash: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbPartialNameSearch: TCheckBox;
    cbProcessComments: TCheckBox;
    cbRenameSelOnlyName: TCheckBox;
    cbSaveThumbnails: TCheckBox;
    cbShowCopyTabSelectPanel: TCheckBox;
    cbSkipFileOpError: TCheckBox;
    edtCopyBufferSize: TEdit;
    gbCopyBufferSize: TGroupBox;
    gbFileSearch: TGroupBox;
    gbGeneralOptions: TGroupBox;
    lblCopyBufferSize: TLabel;
    lblWipePassNumber: TLabel;
    rbUseMmapInSearch: TRadioButton;
    rbUseStreamInSearch: TRadioButton;
    seWipePassNumber: TSpinEdit;
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

{ TfrmOptionsFileOperations }

class function TfrmOptionsFileOperations.GetIconIndex: Integer;
begin
  Result := 8;
end;

class function TfrmOptionsFileOperations.GetTitle: String;
begin
  Result := rsOptionsEditorFileOperations;
end;

procedure TfrmOptionsFileOperations.Load;
begin
  edtCopyBufferSize.Text           := IntToStr(gCopyBlockSize div 1024);
  cbSkipFileOpError.Checked        := gSkipFileOpError;
  cbDropReadOnlyFlag.Checked       := gDropReadOnlyFlag;
  rbUseMmapInSearch.Checked        := gUseMmapInSearch;
  cbPartialNameSearch.Checked      := gPartialNameSearch;
  seWipePassNumber.Value           := gWipePassNumber;
  cbProcessComments.Checked        := gProcessComments;
  cbShowCopyTabSelectPanel.Checked := gShowCopyTabSelectPanel;
  cbDeleteToTrash.Checked          := gUseTrash;
  cbSaveThumbnails.Checked          := gSaveThumb;
  cbRenameSelOnlyName.Checked := gRenameSelOnlyName;
end;

function TfrmOptionsFileOperations.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gCopyBlockSize          := StrToIntDef(edtCopyBufferSize.Text, gCopyBlockSize) * 1024;
  gSkipFileOpError        := cbSkipFileOpError.Checked;
  gDropReadOnlyFlag       := cbDropReadOnlyFlag.Checked;
  gUseMmapInSearch        := rbUseMmapInSearch.Checked;
  gPartialNameSearch      := cbPartialNameSearch.Checked;
  gWipePassNumber         := seWipePassNumber.Value;
  gProcessComments        := cbProcessComments.Checked;
  gShowCopyTabSelectPanel := cbShowCopyTabSelectPanel.Checked;
  gUseTrash               := cbDeleteToTrash.Checked;
  gSaveThumb              := cbSaveThumbnails.Checked;
  gRenameSelOnlyName      := cbRenameSelOnlyName.Checked;
end;

end.


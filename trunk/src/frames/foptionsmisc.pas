{
   Double Commander
   -------------------------------------------------------------------------
   Miscellaneous options page

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

unit fOptionsMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, fOptionsFrame;

type

  { TfrmOptionsMisc }

  TfrmOptionsMisc = class(TOptionsEditor)
    cbDirBrackets: TCheckBox;
    cbGridHorzLine: TCheckBox;
    cbGridVertLine: TCheckBox;
    cbShowWarningMessages: TCheckBox;
    cbSortCaseSensitive: TCheckBox;
    cbSortMethod: TComboBox;
    cbSpaceMovesDown: TCheckBox;
    gbExtended: TGroupBox;
    gbShowGrid: TGroupBox;
    gbSorting: TGroupBox;
    lblSortMethod: TLabel;
  protected
    procedure Init; override;
  public
    class function GetTitle: String; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  uGlobs, uDCUtils, uLng;

{ TfrmOptionsMisc }

procedure TfrmOptionsMisc.Init;
begin
  ParseLineToList(rsOptSortMethod, cbSortMethod.Items);
end;

class function TfrmOptionsMisc.GetTitle: String;
begin
  Result := rsOptionsEditorMiscellaneous;
end;

procedure TfrmOptionsMisc.Load;
begin
  cbGridVertLine.Checked:= gGridVertLine;
  cbGridHorzLine.Checked:= gGridHorzLine;
  cbShowWarningMessages.Checked:= gShowWarningMessages;
  cbSpaceMovesDown.Checked:= gSpaceMovesDown;
  cbDirBrackets.Checked:= gDirBrackets;
  cbSortCaseSensitive.Checked:= gSortCaseSensitive;
  if gSortNatural then cbSortMethod.ItemIndex:= 1;
end;

function TfrmOptionsMisc.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gGridVertLine:= cbGridVertLine.Checked;
  gGridHorzLine:= cbGridHorzLine.Checked;
  gShowWarningMessages:= cbShowWarningMessages.Checked;
  gSpaceMovesDown:= cbSpaceMovesDown.Checked;
  gDirBrackets:= cbDirBrackets.Checked;
  gSortCaseSensitive:= cbSortCaseSensitive.Checked;
  gSortNatural:= (cbSortMethod.ItemIndex = 1);
end;

initialization
  RegisterOptionsEditor(optedMisc, TfrmOptionsMisc);

end.


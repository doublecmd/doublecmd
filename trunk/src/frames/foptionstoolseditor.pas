{
   Double Commander
   -------------------------------------------------------------------------
   Tools options page for the editor tool

   Copyright (C) 2006-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsToolsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Dialogs,
  Buttons, fOptionsFrame, fOptionsToolBase;

type

  { TfrmOptionsEditor }

  TfrmOptionsEditor = class(TfrmOptionsToolBase)
    chkShowSpecialChars: TCheckBox;
    gbInternalEditor: TGroupBox;
    chkScrollPastEndLine: TCheckBox;
    chkTrimTrailingSpaces: TCheckBox;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  SynEdit, uGlobs, uLng;

{ TfrmOptionsEditor }

procedure TfrmOptionsEditor.Init;
begin
  ExternalTool := etEditor;
  inherited Init;
end;

procedure TfrmOptionsEditor.Load;
begin
  inherited Load;
  chkScrollPastEndLine.Checked:= eoScrollPastEoL in gEditorSynEditOptions;
  chkShowSpecialChars.Checked:= eoShowSpecialChars in gEditorSynEditOptions;
  chkTrimTrailingSpaces.Checked:= eoTrimTrailingSpaces in gEditorSynEditOptions;
end;

function TfrmOptionsEditor.Save: TOptionsEditorSaveFlags;

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption);
  begin
    if AValue then
      gEditorSynEditOptions := gEditorSynEditOptions + [AnOption]
    else
      gEditorSynEditOptions := gEditorSynEditOptions - [AnOption];
  end;

begin
  Result:= inherited Save;
  UpdateOptionFromBool(chkScrollPastEndLine.Checked, eoScrollPastEoL);
  UpdateOptionFromBool(chkShowSpecialChars.Checked, eoShowSpecialChars);
  UpdateOptionFromBool(chkTrimTrailingSpaces.Checked, eoTrimTrailingSpaces);
end;

constructor TfrmOptionsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name := 'frmOptionsEditor';
end;

class function TfrmOptionsEditor.GetIconIndex: Integer;
begin
  Result := 10;
end;

class function TfrmOptionsEditor.GetTitle: String;
begin
  Result := rsToolEditor;
end;

end.

{
   Double Commander
   -------------------------------------------------------------------------
   Terminal options page

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

unit fOptionsTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls;

type

  { TfrmOptionsTerminal }

  TfrmOptionsTerminal = class(TOptionsEditor)
    edtRunInTerm: TEdit;
    edtRunTerm: TEdit;
    lblRunInTerm: TLabel;
    lblRunTerm: TLabel;
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

{ TfrmOptionsTerminal }

procedure TfrmOptionsTerminal.Load;
begin
  edtRunInTerm.Text := gRunInTerm;
  edtRunTerm.Text   := gRunTerm;
end;

function TfrmOptionsTerminal.Save: TOptionsEditorSaveFlags;
begin
  gRunInTerm := edtRunInTerm.Text;
  gRunTerm   := edtRunTerm.Text;
  Result := [];
end;

class function TfrmOptionsTerminal.GetIconIndex: Integer;
begin
  Result := 24;
end;

class function TfrmOptionsTerminal.GetTitle: String;
begin
  Result := rsOptionsEditorTerminal;
end;

end.


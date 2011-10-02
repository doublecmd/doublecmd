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
    cbShowWarningMessages: TCheckBox;
    gbExtended: TGroupBox;
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
  uGlobs, uDCUtils, uLng;

{ TfrmOptionsMisc }

class function TfrmOptionsMisc.GetIconIndex: Integer;
begin
  Result := 14;
end;

class function TfrmOptionsMisc.GetTitle: String;
begin
  Result := rsOptionsEditorMiscellaneous;
end;

procedure TfrmOptionsMisc.Load;
begin
  cbShowWarningMessages.Checked:= gShowWarningMessages;
end;

function TfrmOptionsMisc.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gShowWarningMessages:= cbShowWarningMessages.Checked;
end;

end.


{
   Double Commander
   -------------------------------------------------------------------------
   Tools options page

   Copyright (C) 2006-2023  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Spin, ExtCtrls, ColorBox, Dialogs, Types,
  fOptionsFrame, fOptionsToolBase;

type

  { TfrmOptionsViewer }

  TfrmOptionsViewer = class(TfrmOptionsToolBase)
    gbInternalViewer: TGroupBox;
    lblNumberColumnsViewer: TLabel;
    seNumberColumnsViewer: TSpinEdit;
  protected
    procedure Init; override;
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

{ TfrmOptionsViewer }

class function TfrmOptionsViewer.GetIconIndex: Integer;
begin
  Result := 22;
end;

class function TfrmOptionsViewer.GetTitle: String;
begin
  Result := rsToolViewer;
end;

procedure TfrmOptionsViewer.Init;
begin
  ExternalTool := etViewer;

  inherited Init;
end;

procedure TfrmOptionsViewer.Load;
begin
  inherited Load;
  seNumberColumnsViewer.Value := gColCount;
end;

function TfrmOptionsViewer.Save: TOptionsEditorSaveFlags;
begin
  Result := inherited Save;
  gColCount := seNumberColumnsViewer.Value;
end;

end.


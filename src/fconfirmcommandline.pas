{
   Double Commander
   -------------------------------------------------------------------------
   Window to confirm the execution of command line and its parameters

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

unit fConfirmCommandLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type
  { TTfrmConfirmCommandLine }
  TTfrmConfirmCommandLine = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    lbleCommandLine: TLabeledEdit;
    lbleStartPath: TLabeledEdit;
    lbleParameters: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TfrmConfirmCommandLine: TTfrmConfirmCommandLine;

function ConfirmCommandLine(var sCommandLine: string; var sParameter:string; var sStartPath:string):boolean;

implementation

{$R *.lfm}

uses
  uLng,uGlobs;

function ConfirmCommandLine(var sCommandLine: string; var sParameter:string; var sStartPath:string):boolean;
begin
  with TTfrmConfirmCommandLine.Create(Application) do
  try
    Caption:= rsConfirmExecution;
    lbleCommandLine.Text:=sCommandLine;
    lbleParameters.Text:=sParameter;
    lbleStartPath.Text:=sStartPath;

    Result:= (ShowModal = mrOK);
    if Result then
    begin
      sCommandLine:=lbleCommandLine.Text;
      sParameter:=lbleParameters.Text;
      sStartPath:=lbleStartPath.Text;
    end;
  finally
    Free;
  end;
end;

{ TTfrmConfirmCommandLine }
procedure TTfrmConfirmCommandLine.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);
end;



end.


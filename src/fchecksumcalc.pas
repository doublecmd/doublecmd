{
   Double Commander
   -------------------------------------------------------------------------
   Calculate check sum dialog

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit fCheckSumCalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfrmCheckSumCalc }

  TfrmCheckSumCalc = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbSeparateFile: TCheckBox;
    edtSaveTo: TEdit;
    lblSaveTo: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

function ShowCalcCheckSum(const sFileName: UTF8String; out CheckSumOpt: Cardinal): Boolean;

implementation

function ShowCalcCheckSum(const sFileName: UTF8String; out CheckSumOpt: Cardinal): Boolean;
begin
  with TfrmCheckSumCalc.Create(Application) do
  try
    edtSaveTo.Text:= sFileName;
    Result:= (ShowModal = mrOK);
    if Result then
      begin
        CheckSumOpt:= Integer(cbSeparateFile.Checked);
      end;
  finally
    Free;
  end;
end;

initialization
  {$I fchecksumcalc.lrs}

end.


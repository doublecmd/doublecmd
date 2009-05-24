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
  StdCtrls, Buttons, uHash;

type

  { TfrmCheckSumCalc }

  TfrmCheckSumCalc = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbSeparateFile: TCheckBox;
    edtSaveTo: TEdit;
    lblSaveTo: TLabel;
    rbHashMD5: TRadioButton;
    rbHashSHA1: TRadioButton;
    procedure cbSeparateFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbHashMD5Change(Sender: TObject);
    procedure rbHashSHA1Change(Sender: TObject);
  private
    FFileName: UTF8String;
    FAlgorithm: THashAlgorithm;
  public
    { public declarations }
  end; 

function ShowCalcCheckSum(var sFileName: UTF8String; out SeparateFile: Boolean;
                          out HashAlgorithm: THashAlgorithm): Boolean;

implementation

uses
  uGlobs;

function ShowCalcCheckSum(var sFileName: UTF8String; out SeparateFile: Boolean;
                          out HashAlgorithm: THashAlgorithm): Boolean;
begin
  with TfrmCheckSumCalc.Create(Application) do
  try
    FFileName:= sFileName;
    edtSaveTo.Text:= FFileName;
    // set up default hash algorithm if need
    if not (rbHashMD5.Checked or rbHashSHA1.Checked) then
      rbHashMD5.Checked:= True;
    Result:= (ShowModal = mrOK);
    if Result then
      begin
        sFileName:= edtSaveTo.Text;
        SeparateFile:= cbSeparateFile.Checked;
        HashAlgorithm:= FAlgorithm;
      end;
  finally
    Free;
  end;
end;

{ TfrmCheckSumCalc }

procedure TfrmCheckSumCalc.cbSeparateFileChange(Sender: TObject);
begin
  if cbSeparateFile.Checked then
    edtSaveTo.Text:= ExtractFilePath(edtSaveTo.Text) + '*.' + HashFileExt[FAlgorithm]
  else
    edtSaveTo.Text:= ExtractFilePath(edtSaveTo.Text) + ExtractFileName(FFileName) + '.' + HashFileExt[FAlgorithm];
end;

procedure TfrmCheckSumCalc.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);
end;

procedure TfrmCheckSumCalc.rbHashMD5Change(Sender: TObject);
begin
  FAlgorithm:= HASH_MD5;
  edtSaveTo.Text:= ChangeFileExt(edtSaveTo.Text, '.md5');
end;

procedure TfrmCheckSumCalc.rbHashSHA1Change(Sender: TObject);
begin
  FAlgorithm:= HASH_SHA1;
  edtSaveTo.Text:= ChangeFileExt(edtSaveTo.Text, '.sha');
end;

initialization
  {$I fchecksumcalc.lrs}

end.


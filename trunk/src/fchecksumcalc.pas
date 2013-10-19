{
   Double Commander
   -------------------------------------------------------------------------
   Calculate check sum dialog

   Copyright (C) 2009-2013  Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, uHash;

type

  { TfrmCheckSumCalc }

  TfrmCheckSumCalc = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbSeparateFile: TCheckBox;
    cmbHashAlgorithm: TComboBox;
    edtSaveTo: TEdit;
    lblSaveTo: TLabel;
    procedure cbSeparateFileChange(Sender: TObject);
    procedure cmbHashAlgorithmChange(Sender: TObject);
    procedure edtSaveToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFileName: UTF8String;
    FAlgorithm: THashAlgorithm;
  public
    { public declarations }
  end; 

function ShowCalcCheckSum(var sFileName: UTF8String; out SeparateFile: Boolean;
                          out HashAlgorithm: THashAlgorithm): Boolean;

function ShowCalcVerifyCheckSum(out Hash: String;
                                out HashAlgorithm: THashAlgorithm): Boolean;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

function ShowCalcCheckSum(var sFileName: UTF8String; out SeparateFile: Boolean;
                          out HashAlgorithm: THashAlgorithm): Boolean;
begin
  with TfrmCheckSumCalc.Create(Application) do
  try
    FFileName:= sFileName;

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

function ShowCalcVerifyCheckSum(out Hash: String;
                                out HashAlgorithm: THashAlgorithm): Boolean;
begin
  with TfrmCheckSumCalc.Create(Application) do
  try
    OnShow:= nil;
    SessionProperties:= EmptyStr;
    Caption:= rsCheckSumVerifyTitle;
    cbSeparateFile.Visible:= False;
    cmbHashAlgorithm.OnChange:= nil;
    edtSaveTo.OnChange:= @edtSaveToChange;
    lblSaveTo.Caption:= rsCheckSumVerifyText;
    cmbHashAlgorithm.Anchors:= [akTop, akLeft, akRight];
    cmbHashAlgorithm.AnchorSide[akRight].Side:= asrRight;
    cmbHashAlgorithm.AnchorSide[akRight].Control:= edtSaveTo;

    Result:= (ShowModal = mrOK);
    if Result then
    begin
      Hash:= Trim(edtSaveTo.Text);
      Result:= Length(Hash) > 0;
      HashAlgorithm:= THashAlgorithm(cmbHashAlgorithm.ItemIndex);
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

procedure TfrmCheckSumCalc.cmbHashAlgorithmChange(Sender: TObject);
begin
  FAlgorithm:= THashAlgorithm(cmbHashAlgorithm.ItemIndex);
  edtSaveTo.Text:= ChangeFileExt(edtSaveTo.Text, '.' + HashFileExt[FAlgorithm]);
end;

procedure TfrmCheckSumCalc.edtSaveToChange(Sender: TObject);
begin
  case Length(Trim(edtSaveTo.Text)) of
     8: cmbHashAlgorithm.ItemIndex:= Integer(HASH_SFV);
    32: cmbHashAlgorithm.ItemIndex:= Integer(HASH_MD5);
    40: cmbHashAlgorithm.ItemIndex:= Integer(HASH_SHA1);
    64: cmbHashAlgorithm.ItemIndex:= Integer(HASH_SHA256);
    96: cmbHashAlgorithm.ItemIndex:= Integer(HASH_SHA384);
   128: cmbHashAlgorithm.ItemIndex:= Integer(HASH_SHA512);
  end;
end;

procedure TfrmCheckSumCalc.FormCreate(Sender: TObject);
var
  I: THashAlgorithm;
begin
  for I:= Low(HashFileExt) to High(HashFileExt) do
  begin
    cmbHashAlgorithm.Items.Add(UpperCase(HashFileExt[I]));
  end;
  cmbHashAlgorithm.ItemIndex:= 0;
  InitPropStorage(Self);
end;

procedure TfrmCheckSumCalc.FormShow(Sender: TObject);
begin
  edtSaveTo.Text:= FFileName + ExtensionSeparator;
  cmbHashAlgorithmChange(cmbHashAlgorithm);
end;

end.


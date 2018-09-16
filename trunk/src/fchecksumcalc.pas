{
   Double Commander
   -------------------------------------------------------------------------
   Calculate checksum dialog

   Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fCheckSumCalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, fButtonForm, uHash,
  uOperationsManager;

type

  { TfrmCheckSumCalc }

  TfrmCheckSumCalc = class(TfrmButtonForm)
    cbSeparateFile: TCheckBox;
    cbOpenAfterJobIsComplete: TCheckBox;
    edtSaveTo: TEdit;
    lblSaveTo: TLabel;
    lbHashAlgorithm: TListBox;
    procedure cbSeparateFileChange(Sender: TObject);
    procedure edtSaveToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbHashAlgorithmSelectionChange(Sender: TObject; User: boolean);
  private
    FFileName: String;
    FAlgorithm: THashAlgorithm;
  public
    { public declarations }
  end; 

function ShowCalcCheckSum(var sFileName: String; out SeparateFile: Boolean;
                          out HashAlgorithm: THashAlgorithm; out OpenFileAfterJobCompleted: Boolean;
                          out QueueId: TOperationsManagerQueueIdentifier): Boolean;

function ShowCalcVerifyCheckSum(out Hash: String; out HashAlgorithm: THashAlgorithm;
                                out QueueId: TOperationsManagerQueueIdentifier): Boolean;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

function ShowCalcCheckSum(var sFileName: String; out SeparateFile: Boolean;
  out HashAlgorithm: THashAlgorithm; out OpenFileAfterJobCompleted: Boolean;
  out QueueId: TOperationsManagerQueueIdentifier): Boolean;
begin
  with TfrmCheckSumCalc.Create(Application) do
  try
    FFileName:= sFileName;

    Result:= (ShowModal = mrOK);
    if Result then
      begin
        sFileName:= edtSaveTo.Text;
        SeparateFile:= cbSeparateFile.Checked;
        OpenFileAfterJobCompleted:=(cbOpenAfterJobIsComplete.Checked AND cbOpenAfterJobIsComplete.Enabled);
        HashAlgorithm:= FAlgorithm;
        QueueId:= QueueIdentifier
      end;
  finally
    Free;
  end;
end;

function ShowCalcVerifyCheckSum(out Hash: String; out HashAlgorithm: THashAlgorithm;
  out QueueId: TOperationsManagerQueueIdentifier): Boolean;
begin
  with TfrmCheckSumCalc.Create(Application) do
  try
    OnShow:= nil;
    edtSaveTo.Text:= EmptyStr;
    SessionProperties:= EmptyStr;
    Caption:= rsCheckSumVerifyTitle;
    cbSeparateFile.Visible:= False;
    cbOpenAfterJobIsComplete.Visible:= False;
    lbHashAlgorithm.OnSelectionChange:= nil;
    edtSaveTo.OnChange:= @edtSaveToChange;
    lblSaveTo.Caption:= rsCheckSumVerifyText;

    Result:= (ShowModal = mrOK);
    if Result then
    begin
      Hash:= Trim(edtSaveTo.Text);
      Result:= Length(Hash) > 0;
      QueueId:= QueueIdentifier;
      HashAlgorithm:= THashAlgorithm(lbHashAlgorithm.ItemIndex);
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

  cbOpenAfterJobIsComplete.Enabled:=not cbSeparateFile.Checked;
end;

procedure TfrmCheckSumCalc.edtSaveToChange(Sender: TObject);
begin
  case Length(Trim(edtSaveTo.Text)) of
     8: lbHashAlgorithm.ItemIndex:= Integer(HASH_SFV);
    32: lbHashAlgorithm.ItemIndex:= Integer(HASH_MD5);
    40: lbHashAlgorithm.ItemIndex:= Integer(HASH_SHA1);
    64: lbHashAlgorithm.ItemIndex:= Integer(HASH_SHA256);
    96: lbHashAlgorithm.ItemIndex:= Integer(HASH_SHA384);
   128: lbHashAlgorithm.ItemIndex:= Integer(HASH_SHA512);
  end;
end;

procedure TfrmCheckSumCalc.FormCreate(Sender: TObject);
var
  I: THashAlgorithm;
begin
  for I:= Low(HashName) to High(HashName) do
  begin
    lbHashAlgorithm.Items.Add(UpperCase(HashName[I]));
  end;
  InitPropStorage(Self); // Must be *after* lbHashAlgorithm.Items has been loaded so index is restored correctly.
  if (lbHashAlgorithm.ItemIndex=-1) AND (lbHashAlgorithm.Count>0) then lbHashAlgorithm.ItemIndex:= 0;
end;

procedure TfrmCheckSumCalc.FormShow(Sender: TObject);
begin
  edtSaveTo.Text:= FFileName + ExtensionSeparator;
  lbHashAlgorithmSelectionChange(lbHashAlgorithm,FALSE);
end;

procedure TfrmCheckSumCalc.lbHashAlgorithmSelectionChange(Sender: TObject;
  User: boolean);
begin
  FAlgorithm:= THashAlgorithm(lbHashAlgorithm.ItemIndex);
  edtSaveTo.Text:= ChangeFileExt(edtSaveTo.Text, '.' + HashFileExt[FAlgorithm]);
end;

end.


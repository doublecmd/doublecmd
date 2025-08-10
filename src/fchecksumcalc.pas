{
   Double Commander
   -------------------------------------------------------------------------
   Calculate checksum dialog

   Copyright (C) 2009-2025 Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ExtCtrls, fButtonForm,
  uHash, uOperationsManager, uClassesEx;

type

  { TfrmCheckSumCalc }

  TfrmCheckSumCalc = class(TfrmButtonForm)
    cbSeparateFile: TCheckBox;
    cbOpenAfterJobIsComplete: TCheckBox;
    cbSeparateFolder: TCheckBox;
    cbHashAlgorithm: TCheckBox;
    cmbHashAlgorithm: TComboBox;
    edtSaveTo: TEdit;
    lblFileFormat: TLabel;
    lblSaveTo: TLabel;
    rgHashAlgorithm: TRadioGroup;
    rbWindows: TRadioButton;
    rbUnix: TRadioButton;
    procedure cbHashAlgorithmChange(Sender: TObject);
    procedure cbSeparateFileChange(Sender: TObject);
    procedure cbSeparateFolderChange(Sender: TObject);
    procedure cmbHashAlgorithmChange(Sender: TObject);
    procedure edtSaveToChange(Sender: TObject);
    procedure rbFileFormatChange(Sender: TObject);
    procedure rgHashAlgorithmSelectionChanged(Sender: TObject);
  private
    FCreate: Boolean;
    FFileName: String;
    FAlgorithm: THashAlgorithm;
    FIniPropStorage: TIniPropStorageEx;
  private
    procedure RestoreProperties;
    procedure Change(Sender, Alien: TCheckBox);
    procedure SetHashType(AHashType: THashAlgorithm);
  public
    constructor Create(TheOwner: TComponent; ACreateHash: Boolean); reintroduce;
  end; 

function ShowCalcCheckSum(TheOwner: TComponent; var sFileName: String;
                          out SeparateFile: Boolean; out SeparateFolder: Boolean;
                          out HashAlgorithm: THashAlgorithm; out OpenFileAfterJobCompleted: Boolean;
                          out TextLineBreakStyle: TTextLineBreakStyle;
                          out QueueId: TOperationsManagerQueueIdentifier): Boolean;

function ShowCalcVerifyCheckSum(TheOwner: TComponent; out Hash: String; out HashAlgorithm: THashAlgorithm;
                                out QueueId: TOperationsManagerQueueIdentifier): Boolean;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

const
  HASH_TYPE = 'Hash_Type';
  FILE_FORMAT = 'Hash_FileFormat';

function ShowCalcCheckSum(TheOwner: TComponent; var sFileName: String; out
  SeparateFile: Boolean; out SeparateFolder: Boolean; out
  HashAlgorithm: THashAlgorithm; out OpenFileAfterJobCompleted: Boolean; out
  TextLineBreakStyle: TTextLineBreakStyle; out
  QueueId: TOperationsManagerQueueIdentifier): Boolean;
const
  TextLineBreak: array[Boolean] of TTextLineBreakStyle = (tlbsLF, tlbsCRLF);
begin
  with TfrmCheckSumCalc.Create(TheOwner, True) do
  try
    FFileName:= sFileName;
    cmbHashAlgorithm.ItemIndex:= 0;
    edtSaveTo.Text:= FFileName + ExtensionSeparator;
    cmbHashAlgorithm.OnChange:= @cmbHashAlgorithmChange;
    RestoreProperties;

    Result:= (ShowModal = mrOK);

    if Result then
      begin
        sFileName:= edtSaveTo.Text;
        SeparateFile:= cbSeparateFile.Checked;
        SeparateFolder:= cbSeparateFolder.Checked;
        TextLineBreakStyle:= TextLineBreak[rbWindows.Checked];
        FIniPropStorage.StoredValue[HASH_TYPE]:= HashName[FAlgorithm];
        FIniPropStorage.StoredValue[FILE_FORMAT]:= IntToStr(lblFileFormat.Tag);
        OpenFileAfterJobCompleted:=(cbOpenAfterJobIsComplete.Checked AND cbOpenAfterJobIsComplete.Enabled);
        HashAlgorithm:= FAlgorithm;
        QueueId:= QueueIdentifier
      end;
  finally
    Free;
  end;
end;

function ShowCalcVerifyCheckSum(TheOwner: TComponent; out Hash: String; out
  HashAlgorithm: THashAlgorithm; out QueueId: TOperationsManagerQueueIdentifier): Boolean;
begin
  with TfrmCheckSumCalc.Create(TheOwner, False) do
  try
    rbUnix.Visible:= False;
    rbWindows.Visible:= False;
    edtSaveTo.Text:= EmptyStr;
    SessionProperties:= EmptyStr;
    Caption:= rsCheckSumVerifyTitle;
    lblFileFormat.Visible:= False;
    cbSeparateFile.Visible:= False;
    cbSeparateFolder.Visible:= False;
    edtSaveTo.OnChange:= @edtSaveToChange;
    cbOpenAfterJobIsComplete.Visible:= False;
    lblSaveTo.Caption:= rsCheckSumVerifyText;

    Result:= (ShowModal = mrOK);

    if Result then
    begin
      Hash:= TrimHash(edtSaveTo.Text);
      Result:= Length(Hash) > 0;
      QueueId:= QueueIdentifier;
      if cbHashAlgorithm.Checked then
        HashAlgorithm:= THashAlgorithm(UIntPtr(cmbHashAlgorithm.Items.Objects[cmbHashAlgorithm.ItemIndex]))
      else begin
        HashAlgorithm:= THashAlgorithm(UIntPtr(rgHashAlgorithm.Items.Objects[rgHashAlgorithm.ItemIndex]));
      end;
    end;
  finally
    Free;
  end;
end;

{ TfrmCheckSumCalc }

procedure TfrmCheckSumCalc.cbSeparateFileChange(Sender: TObject);
begin
  Change(cbSeparateFile, cbSeparateFolder);
end;

procedure TfrmCheckSumCalc.cbSeparateFolderChange(Sender: TObject);
begin
  Change(cbSeparateFolder, cbSeparateFile);
end;

procedure TfrmCheckSumCalc.edtSaveToChange(Sender: TObject);
var
  AHashType: THashAlgorithm;
begin
  case Length(TrimHash(edtSaveTo.Text)) of
     8: AHashType:= HASH_SFV;
    32: AHashType:= HASH_MD5;
    40: AHashType:= HASH_SHA1;
    56: AHashType:= HASH_SHA224;
    64: AHashType:= HASH_SHA256;
    96: AHashType:= HASH_SHA384;
   128: AHashType:= HASH_SHA512;
   else AHashType:= HASH_BEST;
  end;
  if AHashType <> HASH_BEST then
  begin
    SetHashType(AHashType);
  end;
end;

procedure TfrmCheckSumCalc.rbFileFormatChange(Sender: TObject);
begin
  lblFileFormat.Tag:= TComponent(Sender).Tag;
end;

procedure TfrmCheckSumCalc.rgHashAlgorithmSelectionChanged(Sender: TObject);
begin
  if rgHashAlgorithm.ItemIndex < 0 then Exit;
  rgHashAlgorithm.Tag:= rgHashAlgorithm.ItemIndex;
  FAlgorithm:= THashAlgorithm(UIntPtr(rgHashAlgorithm.Items.Objects[rgHashAlgorithm.ItemIndex]));
  if FCreate then
  begin
    edtSaveTo.Text:= ChangeFileExt(edtSaveTo.Text, '.' + HashFileExt[FAlgorithm]);
  end;
  cbHashAlgorithm.Checked:= False;
end;

procedure TfrmCheckSumCalc.cbHashAlgorithmChange(Sender: TObject);
begin
  cmbHashAlgorithm.Enabled:= cbHashAlgorithm.Checked;

  if cbHashAlgorithm.Checked then
  begin
    rgHashAlgorithm.ItemIndex:= -1;
    if FCreate then cmbHashAlgorithmChange(cmbHashAlgorithm);
  end
  else begin
    if rgHashAlgorithm.ItemIndex < 0 then rgHashAlgorithm.ItemIndex:= rgHashAlgorithm.Tag;
  end;
end;

procedure TfrmCheckSumCalc.cmbHashAlgorithmChange(Sender: TObject);
begin
  if cmbHashAlgorithm.ItemIndex < 0 then Exit;
  FAlgorithm:= THashAlgorithm(UIntPtr(cmbHashAlgorithm.Items.Objects[cmbHashAlgorithm.ItemIndex]));
  edtSaveTo.Text:= ChangeFileExt(edtSaveTo.Text, '.' + HashFileExt[FAlgorithm]);
end;

procedure TfrmCheckSumCalc.RestoreProperties;
var
  AHashName: String;
  AFileFormat: Integer;
  AHashType: THashAlgorithm;
begin
  AFileFormat:= StrToIntDef(FIniPropStorage.StoredValue[FILE_FORMAT], -1);
  if AFileFormat = rbUnix.Tag then
    rbUnix.Checked:= True
  else if AFileFormat = rbWindows.Tag then
    rbWindows.Checked:= True
  else begin
    if (DefaultTextLineBreakStyle = tlbsCRLF) then
      rbWindows.Checked:= True
    else begin
      rbUnix.Checked:= True;
    end;
  end;
  AHashName:= FIniPropStorage.StoredValue[HASH_TYPE];
  if (Length(AHashName) > 0) then
  begin
    for AHashType:= Low(THashAlgorithm) to Pred(High(THashAlgorithm)) do
    begin
      if (HashName[AHashType] = AHashName) then
      begin
        SetHashType(AHashType);
        Exit;
      end;
    end;
  end;
  SetHashType(HASH_BLAKE3);
end;

procedure TfrmCheckSumCalc.Change(Sender, Alien: TCheckBox);
begin
  if Sender.Checked then
  begin
    Alien.Checked:= False;
    edtSaveTo.Text:= ExtractFilePath(edtSaveTo.Text) + '*.' + HashFileExt[FAlgorithm]
  end
  else begin
    edtSaveTo.Text:= ExtractFilePath(edtSaveTo.Text) + ExtractFileName(FFileName) + '.' + HashFileExt[FAlgorithm];
  end;

  cbOpenAfterJobIsComplete.Enabled:= not (Sender.Checked or Alien.Checked);
end;

procedure TfrmCheckSumCalc.SetHashType(AHashType: THashAlgorithm);
var
  Index: Integer;
begin
  Index:= rgHashAlgorithm.Items.IndexOfObject(TObject(UIntPtr(AHashType)));
  if (Index >= 0) then
    rgHashAlgorithm.ItemIndex:= Index
  else begin
    Index:= cmbHashAlgorithm.Items.IndexOfObject(TObject(UIntPtr(AHashType)));
    if (Index >= 0) then
      cmbHashAlgorithm.ItemIndex:= Index
    else begin
      rgHashAlgorithm.ItemIndex:= rgHashAlgorithm.Items.IndexOfObject(TObject(UIntPtr(HASH_BLAKE3)))
    end;
  end;
  cbHashAlgorithm.Checked:= cmbHashAlgorithm.Items.IndexOfObject(TObject(UIntPtr(AHashType))) >= 0;
end;

constructor TfrmCheckSumCalc.Create(TheOwner: TComponent; ACreateHash: Boolean);
var
  AHashType: THashAlgorithm;
begin
  inherited Create(TheOwner);

  FCreate:= ACreateHash;

  // Add 12 hash algorithms to RadioGroup
  for AHashType in HashFirst do
  begin
    rgHashAlgorithm.Items.AddObject(UpperCase(HashName[AHashType]), TObject(UIntPtr(AHashType)));
  end;
  // Add all the rest hash algorithms to ComboBox
  for AHashType:= Low(THashAlgorithm) to Pred(High(THashAlgorithm)) do
  begin
    if (FCreate) and (AHashType = HASH_HAVAL) then Continue;

    if rgHashAlgorithm.Items.IndexOfObject(TObject(UIntPtr(AHashType))) < 0 then
    begin
      cmbHashAlgorithm.Items.AddObject(UpperCase(HashName[AHashType]), TObject(UIntPtr(AHashType)));
    end;
  end;

  if FCreate then
  begin
    FIniPropStorage:= InitPropStorage(Self);
    with FIniPropStorage do
    begin
      IniSection:= Self.ClassName;
      StoredValues.Add.DisplayName:= HASH_TYPE;
      StoredValues.Add.DisplayName:= FILE_FORMAT;
    end;
  end
  else begin
    rgHashAlgorithm.ItemIndex:= 0;
    cmbHashAlgorithm.ItemIndex:= 0;
  end;
end;

end.


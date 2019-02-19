{
   Double Commander
   -------------------------------------------------------------------------
   Verify checksum dialog

   Copyright (C) 2009-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit fCheckSumVerify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Buttons, SynEdit, uOSForms,
  Graphics, uFileSourceCalcChecksumOperation, DCBasicTypes, Controls;

type

  { TfrmCheckSumVerify }

  TfrmCheckSumVerify = class(TAloneForm)
    btnClose: TBitBtn;
    seCheckSumVerify: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure seCheckSumVerifySpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
  private
    procedure AddHeader(const aText: String; aCount: Integer; aColor: TColor);
    procedure ProcessResult(const aResult: TDynamicStringArray;
                            const aText: String; aColor: TColor);
  public
    { public declarations }
  end; 

procedure ShowVerifyCheckSum(const VerifyResult: TVerifyChecksumResult);

implementation

{$R *.lfm}

uses
  uLng, uGlobs, uClassesEx;

procedure ShowVerifyCheckSum(const VerifyResult: TVerifyChecksumResult);
var
  aTotalCount: Integer;
begin
  with TfrmCheckSumVerify.Create(Application) do
  begin
    seCheckSumVerify.Lines.BeginUpdate;
    try
      seCheckSumVerify.Lines.AddObject(rsCheckSumVerifyGeneral, TObject(PtrInt(clWindowText)));
      aTotalCount:= Length(VerifyResult.Success) + Length(VerifyResult.ReadError) +
                    Length(VerifyResult.Broken) + Length(VerifyResult.Missing);

      // Add header information
      AddHeader(rsCheckSumVerifyTotal, aTotalCount, clWindowText);
      AddHeader(rsCheckSumVerifySuccess, Length(VerifyResult.Success), gLogSuccessColor);
      AddHeader(rsCheckSumVerifyMissing, Length(VerifyResult.Missing), gLogErrorColor);
      AddHeader(rsCheckSumVerifyBroken, Length(VerifyResult.Broken), gLogErrorColor);
      AddHeader(rsCheckSumVerifyReadError, Length(VerifyResult.ReadError), gLogErrorColor);

      // Add broken files
      ProcessResult(VerifyResult.Broken, rsCheckSumVerifyBroken, gLogErrorColor);
      // Add read error files
      ProcessResult(VerifyResult.ReadError, rsCheckSumVerifyReadError, gLogErrorColor);
      // Add missing files
      ProcessResult(VerifyResult.Missing, rsCheckSumVerifyMissing, gLogErrorColor);
      // Add good files
      ProcessResult(VerifyResult.Success, rsCheckSumVerifySuccess, gLogSuccessColor);
    finally
      seCheckSumVerify.Lines.EndUpdate;
    end;

    Show;
  end;
end;

{ TfrmCheckSumVerify }

procedure TfrmCheckSumVerify.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmCheckSumVerify.FormCreate(Sender: TObject);
begin
  seCheckSumVerify.FixDefaultKeystrokes;
end;

procedure TfrmCheckSumVerify.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

procedure TfrmCheckSumVerify.seCheckSumVerifySpecialLineColors(Sender: TObject; Line: integer;
                                                               var Special: boolean; var FG, BG: TColor);
begin
  Special:= True;
  FG:= TColor(PtrInt(seCheckSumVerify.Lines.Objects[Line - 1]));
end;

procedure TfrmCheckSumVerify.AddHeader(const aText: String; aCount: Integer; aColor: TColor);
begin
  if aCount = 0 then aColor:= clWindowText;
  seCheckSumVerify.Lines.AddObject(#32 + aText + #32 + IntToStr(aCount), TObject(PtrInt(aColor)));
end;

procedure TfrmCheckSumVerify.ProcessResult(const aResult: TDynamicStringArray;
                                           const aText: String; aColor: TColor);
var
  I: Integer;
begin
  if Length(aResult) > 0 then
  begin
    seCheckSumVerify.Lines.Add(EmptyStr);
    seCheckSumVerify.Lines.AddObject(aText, TObject(PtrInt(aColor)));
    for I:= Low(aResult) to High(aResult) do
    begin
      seCheckSumVerify.Lines.AddObject(#32 + aResult[I], TObject(PtrInt(aColor)));
    end;
  end;
end;

end.


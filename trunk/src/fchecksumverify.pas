{
   Double Commander
   -------------------------------------------------------------------------
   Verify checksum dialog

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

unit fCheckSumVerify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Buttons, SynEdit, uOSForms,
  Graphics, uFileSourceCalcChecksumOperation, DCBasicTypes;

type

  { TfrmCheckSumVerify }

  TfrmCheckSumVerify = class(TAloneForm)
    btnClose: TBitBtn;
    seCheckSumVerify: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  uLng;

procedure ShowVerifyCheckSum(const VerifyResult: TVerifyChecksumResult);
var
  aTotalCount: Integer;
begin
  with TfrmCheckSumVerify.Create(Application) do
  begin
    seCheckSumVerify.Lines.BeginUpdate;
    try
      seCheckSumVerify.Lines.Add(rsCheckSumVerifyGeneral);
      aTotalCount:= Length(VerifyResult.Success) + Length(VerifyResult.ReadError) +
                    Length(VerifyResult.Broken) + Length(VerifyResult.Missing);

      // Add header information
      AddHeader(rsCheckSumVerifyTotal, aTotalCount, clBlack);
      AddHeader(rsCheckSumVerifySuccess, Length(VerifyResult.Success), clGreen);
      AddHeader(rsCheckSumVerifyMissing, Length(VerifyResult.Missing), clRed);
      AddHeader(rsCheckSumVerifyBroken, Length(VerifyResult.Broken), clRed);
      AddHeader(rsCheckSumVerifyReadError, Length(VerifyResult.ReadError), clRed);

      // Add broken files
      ProcessResult(VerifyResult.Broken, rsCheckSumVerifyBroken, clRed);
      // Add read error files
      ProcessResult(VerifyResult.ReadError, rsCheckSumVerifyReadError, clRed);
      // Add missing files
      ProcessResult(VerifyResult.Missing, rsCheckSumVerifyMissing, clRed);
      // Add good files
      ProcessResult(VerifyResult.Success, rsCheckSumVerifySuccess, clGreen);
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

procedure TfrmCheckSumVerify.seCheckSumVerifySpecialLineColors(Sender: TObject; Line: integer;
                                                               var Special: boolean; var FG, BG: TColor);
begin
  Special:= True;
  FG:= TColor(PtrInt(seCheckSumVerify.Lines.Objects[Line - 1]));
end;

procedure TfrmCheckSumVerify.AddHeader(const aText: String; aCount: Integer; aColor: TColor);
begin
  if aCount = 0 then
    seCheckSumVerify.Lines.Add(#32 + aText + #32 + IntToStr(aCount))
  else
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


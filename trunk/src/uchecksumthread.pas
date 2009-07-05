{
    Double Commander
    -------------------------------------------------------------------------
    Implementing of calculate/verify checksum thread.

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

unit uCheckSumThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, uFileOpThread, uFileList, uTypes, SysUtils, LCLProc, uClassesEx, uHash;

type
  TCheckSumOp = (checksum_calc, checksum_verify);

  { TCheckSumThread }

  TCheckSumThread = class(TFileOpThread)
  private
    FCopied: Int64;
    FOneFile: Boolean;
    FCheckSumOp: TCheckSumOp;
    FAlgorithm: THashAlgorithm;
    FCheckSumFile: TStringListEx;
    FResult: TStringList;
    procedure ShowVerifyCheckSumResult;
    function CheckSumCalc(FileRecItem: PFileRecItem): String;
    function GetHashAlgByFileName(const sFileName: UTF8String): THashAlgorithm;
  protected
    procedure MainExecute; override;
    procedure CheckSumFile(pr:PFileRecItem);
    function GetCaptionLng: String; override;
  public
    constructor Create(aFileList: TFileList); override;
    destructor Destroy; override;
    property CheckSumOp: TCheckSumOp write FCheckSumOp;
    property Algorithm: THashAlgorithm read FAlgorithm write FAlgorithm;
    property OneFile: Boolean write FOneFile;
    property Result: TStringList read FResult;
  end;

implementation

uses
  FileUtil, StrUtils, uLng, uGlobs, fCheckSumVerify, uOSUtils;

{ TCheckSumThread }

constructor TCheckSumThread.Create(aFileList: TFileList);
begin
  inherited Create(aFileList);
  FResult:= TStringList.Create;
  FCheckSumFile:= TStringListEx.Create;
  FSymLinkAll:= True;
  FAlgorithm:= HASH_MD5;
end;

destructor TCheckSumThread.Destroy;
begin
  if Assigned(FResult) then
    FreeAndNil(FResult);
  if Assigned(FCheckSumFile) then
    FreeAndNil(FCheckSumFile);
  inherited Destroy;
end;

procedure TCheckSumThread.MainExecute;
var
  pr: PFileRecItem;
  xIndex: Integer;
begin
  FCopied:= 0;

  for xIndex:= NewFileList.Count - 1 downto 0 do
  begin
    if Terminated then Exit;
    if Paused then Suspend;
    pr:= NewFileList.GetItem(xIndex);
    if FPS_ISDIR(pr^.iMode) then Continue;

    EstimateTime(FCopied);

    // process file
    CheckSumFile(pr);

    inc(FCopied,pr^.iSize);
    if FFilesSize <> 0 then
      FFileOpDlg.iProgress2Pos:= (FCopied * 100) div FFilesSize;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;

  if Terminated then Exit;
  // make result
  case FCheckSumOp of
    checksum_calc:
      if FOneFile then
        FCheckSumFile.SaveToFile(sDstMask);
    checksum_verify:
      begin
        Synchronize(@FFileOpDlg.Hide);
        Synchronize(@ShowVerifyCheckSumResult);
      end;
  end;
end;

procedure TCheckSumThread.CheckSumFile(pr: PFileRecItem);
var
  fri: TFileRecItem;
  I: Integer;
  sCheckSum1,
  sCheckSum2: String;
  bResult: Boolean;
begin
  case FCheckSumOp of
    checksum_calc: // calculate check sum
      begin
        if not FOneFile then
          FCheckSumFile.Clear;
        //------------------------------------
        FFileOpDlg.sFileNameFrom:= pr^.sName;
        Synchronize(@FFileOpDlg.UpdateDlg);
        //------------------------------------
        sCheckSum1:= CheckSumCalc(pr);
        FCheckSumFile.Add(sCheckSum1 + ' *' + ExtractFileName(pr^.sName));
        if not FOneFile then
          FCheckSumFile.SaveToFile(pr^.sName + '.' + HashFileExt[FAlgorithm]);
      end;
    checksum_verify: // verify check sum
      begin
        FCheckSumFile.Clear;
        FCheckSumFile.NameValueSeparator:= #32;
        FCheckSumFile.LoadFromFile(pr^.sName);
        FAlgorithm:= GetHashAlgByFileName(pr^.sName);
        for I:= 0 to FCheckSumFile.Count - 1 do
          begin
            bResult:= False;
            FillByte(fri, SizeOf(fri), 0);
            fri.sName:= sDstPath + (PChar(FCheckSumFile.ValueFromIndex[I])+1);
            fri.iSize:= mbFileSize(fri.sName);
            //------------------------------------
            FFileOpDlg.sFileNameFrom:= fri.sName;
            Synchronize(@FFileOpDlg.UpdateDlg);
            //------------------------------------
            sCheckSum1:= CheckSumCalc(@fri);
            sCheckSum2:= FCheckSumFile.Names[I];
            bResult:= (StrComp(PChar(sCheckSum1), PChar(sCheckSum2)) = 0);
            FResult.Add(PChar(FCheckSumFile.ValueFromIndex[I])+1 + ': ' + IfThen(bResult, 'True', 'False'));
          end;
      end;
  end;
end;

procedure TCheckSumThread.ShowVerifyCheckSumResult;
begin
  ShowVerifyCheckSum(FResult);
end;

function TCheckSumThread.CheckSumCalc(FileRecItem: PFileRecItem): String;
var
  hFile: THandle;
  Buf: PChar;
  Context: THashContext;
  Count: Cardinal;
  Digest: THashDigest;
  iProcessed: Cardinal;
  iCopyBlockSize: Cardinal;
begin
  Result:= EmptyStr;
  iCopyBlockSize:= gCopyBlockSize;

  FFileOpDlg.iProgress1Pos:= 0;
  FFileOpDlg.iProgress1Max:= 100;
  Synchronize(@FFileOpDlg.UpdateDlg);
  iProcessed:= 0;

  HashInit(Context, FAlgorithm);
  try
    hFile:= mbFileOpen(FileRecItem^.sName, fmOpenRead or fmShareDenyNone);
    if hFile <> feInvalidHandle then
      begin
        GetMem(Buf, iCopyBlockSize);
        repeat
          if Terminated then Exit;
          if Paused then Suspend;
          Count:= FileRead(hFile, Buf^, iCopyBlockSize);
          if Count > 0 then
            begin
              HashUpdate(Context, Buf^, Count);
              iProcessed := iProcessed + Count;
              FFileOpDlg.iProgress1Pos:= (iProcessed * 100) div FileRecItem^.iSize;
              if FCheckSumOp = checksum_calc then
                begin
                  FFileOpDlg.iProgress2Pos:= ((FCopied + iProcessed) * 100) div FFilesSize;
                  EstimateTime(FCopied + iProcessed);
                end;
              Synchronize(@FFileOpDlg.UpdateDlg);
            end;
        until Count < iCopyBlockSize;
        FreeMem(Buf, iCopyBlockSize);
        FileClose(hFile);
      end;
  finally
    HashFinal(Context, Digest);
    Result:= HashPrint(Digest);
  end;
end;

function TCheckSumThread.GetHashAlgByFileName(const sFileName: UTF8String): THashAlgorithm;
var
  sExt: UTF8String;
begin
  sExt:= ExtractFileExt(sFileName);
  if mbCompareText(sExt, '.md5') = 0 then
    Result:= HASH_MD5
  else if mbCompareText(sExt, '.sha') = 0 then
    Result:= HASH_SHA1;
end;

function TCheckSumThread.GetCaptionLng: String;
begin
  case FCheckSumOp of
    checksum_calc:
      Result:= rsDlgCheckSumCalc;
    checksum_verify:
      Result:= rsDlgCheckSumVerify;
  end;
end;

end.

{
    Double Commander
    -------------------------------------------------------------------------
    Implementing of calculation/test checksum thread.

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
  Classes, uFileOpThread, uFileList, uTypes, SysUtils, LCLProc, uClassesEx;

type
  TCheckSumOp = (checksum_calc, checksum_verify);

  { TCheckSumThread }

  TCheckSumThread = class(TFileOpThread)
  private
    FCopied: Int64;
    FOneFile: Boolean;
    FCheckSumOp: TCheckSumOp;
    FCheckSumFile: TStringListEx;
    FResult: TStringList;
    procedure ShowVerifyCheckSumResult;
    function CheckSumCalc(FileRecItem: PFileRecItem): String;
  protected
    constructor Create(aFileList: TFileList); override;
    destructor Destroy; override;
    procedure MainExecute; override;
    procedure CheckSumFile(pr:PFileRecItem);
    function GetCaptionLng: String; override;
  public
    property CheckSumOp: TCheckSumOp write FCheckSumOp;
    property OneFile: Boolean write FOneFile;
    property Result: TStringList read FResult;
  end;

implementation

uses
  FileUtil, md5, StrUtils, uLng, uGlobs, uLog, fCheckSumVerify, uOSUtils;

constructor TCheckSumThread.Create(aFileList: TFileList);
begin
  inherited Create(aFileList);
  FResult:= TStringList.Create;
  FCheckSumFile:= TStringListEx.Create;
  FSymLinkAll:= True;
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

    EstimateTime(FCopied);

    // process file
    CheckSumFile(pr);

    inc(FCopied,pr^.iSize);
    if FFilesSize <> 0 then
      FFileOpDlg.iProgress2Pos:= (FCopied * 100) div FFilesSize;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;

  // make result
  case FCheckSumOp of
    checksum_calc:
      if FOneFile then
        FCheckSumFile.SaveToFile(sDstPath + ExtractFileName(ExcludeTrailingBackslash(sDstPath)) + '.md5');
    checksum_verify:
      Synchronize(@ShowVerifyCheckSumResult);
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
        FFileOpDlg.sFileName:= pr^.sName;
        Synchronize(@FFileOpDlg.UpdateDlg);
        //------------------------------------
        sCheckSum1:= CheckSumCalc(pr);
        FCheckSumFile.Add(sCheckSum1 + ' *' + ExtractFileName(pr^.sName));
        if not FOneFile then
          FCheckSumFile.SaveToFile(ExtractFileNameWithoutExt(pr^.sName) + '.md5');
      end;
    checksum_verify: // verify check sum
      begin
        FCheckSumFile.Clear;
        FCheckSumFile.NameValueSeparator:= '*';
        FCheckSumFile.LoadFromFile(pr^.sName);
        for I:= 0 to FCheckSumFile.Count - 1 do
          begin
            bResult:= False;
            FillByte(fri, SizeOf(fri), 0);
            fri.sName:= sDstPath + FCheckSumFile.ValueFromIndex[I];
            fri.iSize:= mbFileSize(fri.sName);
            //------------------------------------
            FFileOpDlg.sFileName:= fri.sName;
            Synchronize(@FFileOpDlg.UpdateDlg);
            //------------------------------------
            sCheckSum1:= CheckSumCalc(@fri);
            sCheckSum2:= Trim(FCheckSumFile.Names[I]);
            bResult:= MD5Match(MD5String(sCheckSum1), MD5String(sCheckSum2));
            FResult.Add(FCheckSumFile.ValueFromIndex[I] + ': ' + IfThen(bResult, 'True', 'False'));
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
  Context: TMDContext;
  Count: Cardinal;
  Digest: TMDDigest;
  iProcessed: Int64;
  iCopyBlockSize: Integer;
begin
  Result:= EmptyStr;
  iCopyBlockSize:= gCopyBlockSize;

  FFileOpDlg.iProgress1Pos:= 0;
  FFileOpDlg.iProgress1Max:= 100;
  Synchronize(@FFileOpDlg.UpdateDlg);
  iProcessed:= 0;

  MD5Init(Context);
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
              MD5Update(Context, Buf^, Count);
              Inc(iProcessed, Count);
              FFileOpDlg.iProgress1Pos:= (iProcessed * 100) div FileRecItem^.iSize;
              Synchronize(@FFileOpDlg.UpdateDlg);
            end;
        until Count < iCopyBlockSize;
        FreeMem(Buf, iCopyBlockSize);
        FileClose(hFile);
      end;
  finally
    MD5Final(Context, Digest);
    Result:= MD5Print(Digest);
  end;
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

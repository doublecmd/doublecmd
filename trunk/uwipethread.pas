{
   Double Commander
   -------------------------------------------------------------------------
   This module implements a secure erase of disk media as per the
   Department of Defense clearing and sanitizing standard: DOD 5220.22-M

   The standard states that hard disk media is erased by
   overwriting with a character, then the character's complement,
   and then a random character. Note that the standard specicically
   states that this method is not suitable for TOP SECRET information.
   TOP SECRET data sanatizing is only achievable by a Type 1 or 2
   degauss of the disk, or by disintegrating, incinerating,
   pulverizing, shreding, or melting the disk.

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

   Based on:
   
     WP - wipes files in a secure way.
     version 3.2 - By Uri Fridman. urifrid@yahoo.com
     www.geocities.com/urifrid

   Contributors:

     Radek Cervinka  <radek.cervinka@centrum.cz>

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

unit uWipeThread;

{$mode objfpc}{$H+}

interface

uses
  uFileOpThread, uFileList, uTypes, SysUtils, LCLProc;

type

  { TWipeThread }

  TWipeThread = class(TFileOpThread)
  private
    everythingOK: boolean;
    errors, 
    files, 
    directories: Integer;
    buffer: array [0..4095] of Byte;
    procedure Fill(chr: Integer);
    procedure SecureDelete(pass: Integer; FileName: String);
    procedure WipeDir(dir: string);
    procedure WipeFile(filename: String);
  protected
    constructor Create(aFileList: TFileList);override;
    procedure MainExecute; override;
    procedure Wipe(fr: PFileRecItem);
    function GetCaptionLng: String;override;
  end;

implementation
uses
  uLng, uGlobs, uLog, uFindEx, uFileStreamEx, uOSUtils;

constructor TWipeThread.Create(aFileList: TFileList);
begin
  inherited Create(aFileList);
  FSymLinkAll := True;
end;

//fill buffer with characters
//0 = with 0, 1 = with 1 and 2 = random
procedure TWipeThread.Fill(chr:integer);
var i: integer;
begin
   if chr=0 then
   begin
    for i := Low(buffer) to High(buffer) do
      buffer[i] := 0;
      exit;
   end;

   if chr=1 then
   begin
    for i := Low(buffer) to High(buffer) do
      buffer[i] := 1;
      exit;
   end;

   if chr=2 then
   begin
    for i := Low(buffer) to High(buffer) do
      buffer[i] := Random(256);
      exit;
   end;
end;

procedure TWipeThread.SecureDelete(pass: Integer; FileName: String);
var
  max, n: LongInt;
  i: Integer;
  fs: TFileStreamEx;
  rena: String;                   // renames file to delete
begin
  try
    if mbRenameFile(filename,ExtractFilePath(filename)+'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.aaaaaaa') then
    begin
     rena:= ExtractFilePath(filename)+'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.aaaaaaa';
     filename:=rena;
    end;
  except
    DebugLn('wp: error renaming file: '+filename);
    everythingOK:=False;
    errors:=errors+1;
    Exit;
  end;

  fs := TFilestreamEx.Create(FileName, fmOpenReadWrite or fmShareExclusive);
  try
    for i := 1 to pass do
    begin
      write('.');
      //with zeros
      fill(0);
      max := fs.Size;
      fs.Position := 0;
      while max > 0 do
      begin
        if max > SizeOf(buffer) then
          n := SizeOf(buffer)
        else
          n := max;
        fs.Write(Buffer, n);
        max := max - n;
      end;
      FileFlush(fs.Handle);

      //with ones
      fill(1);
      max := fs.Size;
      fs.Position := 0;
      while max > 0 do
      begin
        if max > SizeOf(buffer) then
          n := SizeOf(buffer)
        else
          n := max;
        fs.Write(Buffer, n);
        max := max - n;
      end;
      FileFlush(fs.Handle);

      //with random data
      fill(2);
      max := fs.Size;
      fs.Position := 0;
      while max > 0 do
      begin
        if max > SizeOf(buffer) then
          n := SizeOf(buffer)
        else
          n := max;
        fs.Write(Buffer, n);
        max := max - n;
      end;
      FileFlush(fs.Handle);
    end;
    FileTruncate(fs.Handle, 0);    
    fs.Free;
  except
    on E: Exception do
    begin
      DebugLn('wp: error wiping: '+filename+': '+E.Message);
      fs.Free;
      everythingOK:=False;
      errors:=errors+1;
      Exit;
    end;
  end;
  try
    mbDeleteFile(FileName);
  except
    on E: Exception do
    begin
      DebugLn('wp: error deleting: '+filename+': '+E.Message);
      fs.Free;
      everythingOK:=False;
      errors:=errors+1;
      Exit;
    end;
  end;
  files:= files+1;
  DebugLn('OK');
  everythingOK:= True;
end;  

procedure TWipeThread.WipeDir(dir: string);
var
  Search: TSearchRec;
  ok: Integer;
begin
  ok:= FindFirstEx(dir + PathDelim + '*', faAnyFile, Search);
  while ok = 0 do  begin
    if ((Search.Name <> '.' ) and (Search.Name <> '..')) then
      begin
        if fpS_ISDIR(Search.Attr) then
          begin
            //remove read-only attr
            try          
              FileCopyAttr(Search.Name, Search.Name, True);
            except
              DebugLn('wp: FAILED when trying to remove read-only attr on '+Search.Name);
            end;
            DebugLn('entering '+dir + PathDelim + Search.Name);
            WipeDir(dir + PathDelim + Search.Name);
          end
        else
          begin
          //remove read-only attr
            try
              if not FileCopyAttr(Search.Name, Search.Name, True) then
                DebugLn('wp: FAILED when trying to remove read-only attr on '+Search.Name);
            except
              DebugLn('wp: FAILED when trying to remove read-only attr on '+Search.Name);
            end;
            // do something with the file
            DebugLn('wiping '+dir + PathDelim + Search.Name);
            SecureDelete(1, dir + PathDelim + Search.Name);
          end;
      end;
    ok:= FindNextEx(Search);
  end;
  FindClose(Search);
  try
    if everythingOK then
      begin
        DebugLn('wiping '+dir);
            
        if not mbRemoveDir(dir) then
          begin
            DebugLn('wp: error wiping directory '+dir);
            // write log -------------------------------------------------------------------
            if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
              logWrite(Self, Format(rsMsgLogError+rsMsgLogRmDir, [dir]), lmtError);
            //------------------------------------------------------------------------------
          end
        else
          begin
            directories:= directories + 1;
            DebugLn('OK');
            // write log -------------------------------------------------------------------
            if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
              logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogRmDir, [dir]), lmtSuccess)        
            //------------------------------------------------------------------------------
          end;
      end;
  except
    on EInOutError do DebugLn('Couldn''t remove '+ dir);
  end;
end;

procedure TWipeThread.WipeFile(filename: String);
var
  Found: Integer;
  SRec: TSearchRec;
  sPath: String;
begin
  sPath:= ExtractFilePath(filename);
  { Use FindFirst so we can specify wild cards in the filename }
  Found:= FindFirstEx(filename,faReadOnly or faSysFile or faArchive or faSysFile,SRec);
  if Found <> 0 then
    begin
      DebugLn('wp: file not found: ',filename);
      errors:= errors+1;
      exit;
    end;
    while Found = 0 do
    begin
      //remove read-only attr
      try
        if not FileCopyAttr(sPath + SRec.Name, sPath + SRec.Name, True) then
          DebugLn('wp: FAILED when trying to remove read-only attr on '+ sPath + SRec.Name);
      except
        DebugLn('wp: can''t wipe '+ sPath + SRec.Name + ', file might be in use.');
        DebugLn('wipe stopped.');
        errors:= errors+1;
        everythingOK:= False;
        exit;        
      end;

      DebugLn('wiping ' + sPath + SRec.Name);
      SecureDelete(1, sPath + SRec.Name);
      if not everythingOK then
         DebugLn('wp: couldn''t wipe ' + sPath + SRec.Name);

      Found:= FindNextEx(SRec);   { Find the next file }
    end;
    FindClose(SRec);
end;
            
procedure TWipeThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCoped:Int64;
begin
  iCoped:=0;
  FFileOpDlg.iProgress1Max:=1;
  FFileOpDlg.iProgress1Pos:=1; // in delete use only 1 progress

  Synchronize(@FFileOpDlg.UpdateDlg);

  for xIndex:=NewFileList.Count-1 downto 0 do // deleting
  begin
    pr:=NewFileList.GetItem(xIndex);
    FFileOpDlg.sFileName:=pr^.sName;
    Synchronize(@FFileOpDlg.UpdateDlg);
    inc(iCoped,pr^.iSize);
    EstimateTime(iCoped);
    Wipe(pr);
    FFileOpDlg.iProgress2Pos:=iCoped;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
end;

procedure TWipeThread.Wipe(fr: PFileRecItem);
begin
  try
    if FPS_ISDIR(fr^.iMode) then // directory
      WipeDir(fr^.sName)
    else // files
      WipeFile(fr^.sName);
  except
    DebugLn('Can not wipe ', fr^.sName);
  end;
end;

function TWipeThread.GetCaptionLng:String;
begin
  Result:= rsDlgDel;
end;

end.

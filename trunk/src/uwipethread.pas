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

   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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
  uFileOpThread, uFileList, uTypes, SysUtils, LCLProc, fFileOpDlg;

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
    procedure MainExecute; override;
    procedure Wipe(fr: PFileRecItem);
    function GetCaptionLng: String; override;
    function GetFileOpDlgLook: TFileOpDlgLook; override;
  public
    constructor Create(aFileList: TFileList); override;
  end;

implementation
uses
  uLng, uGlobs, uLog, uFindEx, uClassesEx, uOSUtils;

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
  n, i: Integer;
  max,
  iPos,
  iMax: Int64;
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
      //---------------Progress--------------
      if Paused then Suspend;
      FFileOpDlg.iProgress1Max:= 100;
      FFileOpDlg.iProgress1Pos:= 0;
      Synchronize(@FFileOpDlg.UpdateDlg);
      iMax:= fs.Size * 3;
      iPos:= 0;
      //-------------------------------------

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
        //---------------Progress--------------
        if Paused then Suspend;
        Inc(iPos, n);
        if iMax <> 0 then
          FFileOpDlg.iProgress1Pos:= (iPos * 100) div iMax;
        Synchronize(@FFileOpDlg.UpdateDlg);
        //-------------------------------------
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
        //---------------Progress--------------
        if Paused then Suspend;
        Inc(iPos, n);
        if iMax <> 0 then
          FFileOpDlg.iProgress1Pos:= (iPos * 100) div iMax;
        Synchronize(@FFileOpDlg.UpdateDlg);
        //-------------------------------------
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
        //---------------Progress--------------
        if Paused then Suspend;
        Inc(iPos, n);
        if iMax <> 0 then
          FFileOpDlg.iProgress1Pos:= (iPos * 100) div iMax;
        Synchronize(@FFileOpDlg.UpdateDlg);
        //-------------------------------------
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
  sPath: String;
begin
  sPath:= IncludeTrailingPathDelimiter(dir);
  ok:= FindFirstEx(sPath + '*', faAnyFile, Search);
  while ok = 0 do  begin
    if ((Search.Name <> '.' ) and (Search.Name <> '..')) then
      begin
        if fpS_ISDIR(Search.Attr) then
          begin
            //remove read-only attr
            try          
              FileCopyAttr(sPath + Search.Name, sPath + Search.Name, True);
            except
              DebugLn('wp: FAILED when trying to remove read-only attr on '+ sPath + Search.Name);
            end;
            DebugLn('entering '+ sPath + Search.Name);
            WipeDir(sPath + Search.Name);
          end
        else
          begin
          //remove read-only attr
            try
              if not FileCopyAttr(sPath + Search.Name, sPath + Search.Name, True) then
                DebugLn('wp: FAILED when trying to remove read-only attr on '+ sPath + Search.Name);
            except
              DebugLn('wp: FAILED when trying to remove read-only attr on '+ sPath + Search.Name);
            end;
            // do something with the file
            DebugLn('wiping '+ sPath + Search.Name);
            SecureDelete(gWipePassNumber, sPath + Search.Name);
          end;
      end;
    ok:= FindNextEx(Search);
  end;
  FindCloseEx(Search);
  try
    if everythingOK then
      begin
        DebugLn('wiping ' + dir);
            
        if not mbRemoveDir(dir) then
          begin
            DebugLn('wp: error wiping directory ' + dir);
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
      DebugLn('wp: file not found: ', filename);
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
      SecureDelete(gWipePassNumber, sPath + SRec.Name);
      if not everythingOK then
         DebugLn('wp: couldn''t wipe ' + sPath + SRec.Name);

      Found:= FindNextEx(SRec);   { Find the next file }
    end;
    FindCloseEx(SRec);
end;
            
procedure TWipeThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCopied:Int64;
begin
  iCopied:= 0;
  FFileOpDlg.iProgress1Max:= 100;
  FFileOpDlg.iProgress1Pos:= 0;

  Synchronize(@FFileOpDlg.UpdateDlg);

  for xIndex:=NewFileList.Count-1 downto 0 do // deleting
  begin
    if Terminated then Exit;
    pr:=NewFileList.GetItem(xIndex);
    FFileOpDlg.sFileNameFrom:= pr^.sName;
    Synchronize(@FFileOpDlg.UpdateDlg);
    inc(iCopied,pr^.iSize);
    EstimateTime(iCopied);
    Wipe(pr);
    if FFilesSize <> 0 then
      FFileOpDlg.iProgress2Pos:= (iCopied * 100) div FFilesSize;
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

    // process comments if need
    if gProcessComments and Assigned(FDescr) then
      FDescr.DeleteDescription(fr^.sName);
  except
    DebugLn('Can not wipe ', fr^.sName);
  end;
end;

function TWipeThread.GetCaptionLng:String;
begin
  Result:= rsDlgDel;
end;

function TWipeThread.GetFileOpDlgLook: TFileOpDlgLook;
begin
  Result:= [fodl_from_lbl, fodl_first_pb, fodl_second_pb];
end;

end.

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

unit uFileSystemWipeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceWipeOperation,
  uFileSystemFileSource,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uFileSystemFile,
  uDescr, uGlobs, uLog;

type

  { TFileSystemWipeOperation }

  TFileSystemWipeOperation = class(TFileSourceWipeOperation)
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
  private
    FFullFilesTreeToDelete: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceWipeOperationStatistics; // local copy of statistics
    FDescription: TDescription;

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;
    FDeleteReadOnly: TFileSourceOperationOptionGeneral;

  protected
    procedure Wipe(aFile: TFileSystemFile);
    function ShowError(sMessage: String): TFileSourceOperationUIResponse;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(var aTargetFileSource: TFileSource;
                       var theFilesToWipe: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uOSUtils, uLng, uFindEx, uClassesEx,
  uFileSystemUtil, FileUtil, LCLProc;

constructor TFileSystemWipeOperation.Create(var aTargetFileSource: TFileSource;
                                              var theFilesToWipe: TFiles);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FDeleteReadOnly := fsoogNone;
  FFullFilesTreeToDelete := nil;

  if gProcessComments then
    FDescription := TDescription.Create(True)
  else
    FDescription := nil;

  inherited Create(aTargetFileSource, theFilesToWipe);
end;

destructor TFileSystemWipeOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FDescription) then
  begin
    FDescription.SaveDescription;
    FreeAndNil(FDescription);
  end;

  if Assigned(FFullFilesTreeToDelete) then
    FreeAndNil(FFullFilesTreeToDelete);
end;

procedure TFileSystemWipeOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(FilesToWipe as TFileSystemFiles,
               FFullFilesTreeToDelete,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)

  FDescription.Clear;
end;

procedure TFileSystemWipeOperation.MainExecute;
var
  aFile: TFileSystemFile;
  CurrentFileIndex: Integer;
  OldDoneBytes: Int64; // for if there was an error
begin
  for CurrentFileIndex := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[CurrentFileIndex] as TFileSystemFile;

    FStatistics.CurrentFile := aFile.Path + aFile.Name;
    UpdateStatistics(FStatistics);

    // If there will be an error in Wipe the DoneBytes value
    // will be inconsistent, so remember it here.
    OldDoneBytes := FStatistics.DoneBytes;

    Wipe(aFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      // Correct statistics if file not correctly processed.
      if DoneBytes < (OldDoneBytes + aFile.Size) then
        DoneBytes := OldDoneBytes + aFile.Size;

      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

procedure TFileSystemWipeOperation.Finalize;
begin
end;

//fill buffer with characters
//0 = with 0, 1 = with 1 and 2 = random
procedure TFileSystemWipeOperation.Fill(chr: Integer);
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

procedure TFileSystemWipeOperation.SecureDelete(pass: Integer; FileName: String);
var
  i, j, n: Integer;
  max: Int64;
  fs: TFileStreamEx;
  rena: String; // renames file to delete
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
      CheckOperationState; // check pause and stop
      FStatistics.CurrentFileTotalBytes:= fs.Size * 3;
      FStatistics.CurrentFileDoneBytes:= 0;
      UpdateStatistics(FStatistics);
      //-------------------------------------
      for j:= 0 to 2 do
      begin
        fill(j);
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
          with FStatistics do
          begin
            Inc(CurrentFileDoneBytes, n);
            Inc(DoneBytes, n div (3 * pass));
            UpdateStatistics(FStatistics);
            CheckOperationState; // check pause and stop
          end;
          //-------------------------------------
        end;
        FileFlush(fs.Handle);
        CheckOperationState; // check pause and stop
      end;
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

procedure TFileSystemWipeOperation.WipeDir(dir: string);
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
              mbFileSetReadOnly(sPath + Search.Name, False);
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
              if not mbFileSetReadOnly(sPath + Search.Name, False) then
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
            LogMessage(Format(rsMsgLogError+rsMsgLogRmDir, [dir]), [log_dir_op, log_delete], lmtError);
            //------------------------------------------------------------------------------
          end
        else
          begin
            directories:= directories + 1;
            DebugLn('OK');
            // write log -------------------------------------------------------------------
            LogMessage(Format(rsMsgLogSuccess+rsMsgLogRmDir, [dir]), [log_dir_op, log_delete], lmtSuccess);
            //------------------------------------------------------------------------------
          end;
      end;
  except
    on EInOutError do DebugLn('Couldn''t remove '+ dir);
  end;
end;

procedure TFileSystemWipeOperation.WipeFile(filename: String);
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

procedure TFileSystemWipeOperation.Wipe(aFile: TFileSystemFile);
var
  FileName: String;
begin
  try
    FileName:= aFile.Path + aFile.Name;
    if aFile.IsDirectory then // directory
      WipeDir(FileName)
    else // files
      WipeFile(FileName);

    // process comments if need
    if gProcessComments and Assigned(FDescription) then
      FDescription.DeleteDescription(FileName);
  except
    DebugLn('Can not wipe ', FileName);
  end;
end;

function TFileSystemWipeOperation.ShowError(sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError then
  begin
    if Assigned(Thread) then
      logWrite(Thread, sMessage, lmtError, True)
    else
      logWrite(sMessage, lmtError, True);

    Result := fsourSkip;
  end
  else
  begin
    Result := AskQuestion(sMessage, '', [fsourSkip, fsourCancel], fsourSkip, fsourCancel);
    if Result = fsourCancel then
      RaiseAbortOperation;
  end;
end;

procedure TFileSystemWipeOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:
      if not (log_errors in gLogOptions) then Exit;
    lmtInfo:
      if not (log_info in gLogOptions) then Exit;
    lmtSuccess:
      if not (log_success in gLogOptions) then Exit;
  end;

  if logOptions <= gLogOptions then
  begin
    logWrite(Thread, sMessage, logMsgType);
  end;
end;

end.


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

   Copyright (C) 2008-2018 Alexander Koblov (alexx2000@mail.ru)

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
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFileSystemWipeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ISAAC,
  uFileSourceWipeOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uDescr, uGlobs, uLog;

type

  { TFileSystemWipeOperation }

  TFileSystemWipeOperation = class(TFileSourceWipeOperation)
  private
    FRandom: isaac_ctx;
    FBuffer: array [0..2, 0..4095] of Byte;
  private
    procedure Fill(Step: Integer);
    function WipeDir(const FileName: String): Boolean;
    function WipeLink(const FileName: String): Boolean;
    function WipeFile(const FileName: String): Boolean;
    function Rename(const FileName: String; out NewName: String): Boolean;
  private
    FDescription: TDescription;
    FFullFilesTreeToDelete: TFiles; // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceWipeOperationStatistics; // local copy of statistics

    // Options
    FSkipErrors: Boolean;
    FWipePassNumber: Integer;
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FDeleteReadOnly: TFileSourceOperationOptionGeneral;

  protected
    procedure Wipe(aFile: TFile);
    function HandleError(const Message: String): Boolean;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToWipe: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  uDebug, uLng, DCClassesUtf8, uFileSystemUtil, uRandom, uAdministrator, DCOSUtils;

constructor TFileSystemWipeOperation.Create(aTargetFileSource: IFileSource;
                                            var theFilesToWipe: TFiles);
begin
  FSkipErrors := False;
  FSymLinkOption := fsooslNone;
  FDeleteReadOnly := fsoogNone;
  FFullFilesTreeToDelete := nil;
  FWipePassNumber:= gWipePassNumber;

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

  FreeAndNil(FFullFilesTreeToDelete);
end;

procedure TFileSystemWipeOperation.Initialize;
begin
  Fill(0); Fill(1);
  isaac_init(FRandom, Int32(GetTickCount64));

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(FilesToWipe, True, False,
               FFullFilesTreeToDelete,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)

  if gProcessComments then
    FDescription.Clear;
end;

procedure TFileSystemWipeOperation.MainExecute;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
  OldDoneBytes: Int64; // for if there was an error
begin
  for CurrentFileIndex := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[CurrentFileIndex];

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

//fill buffer with characters
//0 = with 0, 1 = with 1 and 2 = random
procedure TFileSystemWipeOperation.Fill(Step: Integer);
var
  Index: Integer;
  Count: Integer;
begin
  Count:= SizeOf(FBuffer[Step]);
  case Step of
    0:
      begin
        Count:= Count div SizeOf(DWord);
        FillDWord(FBuffer[Step, 0], Count, $00000000);
      end;
    1:
      begin
        Count:= Count div SizeOf(DWord);
        FillDWord(FBuffer[Step, 0], Count, $FFFFFFFF);
      end;
    2:
      begin
        Index:= 0;
        while Index < Count do
        begin
          Move(FRandom.randrsl[0], FBuffer[Step, Index], SizeOf(FRandom.randrsl));
          Inc(Index, SizeOf(FRandom.randrsl));
          isaac_generate(FRandom);
        end;
      end;
  end;
end;

function TFileSystemWipeOperation.Rename(const FileName: String; out NewName: String): Boolean;
var
  bRetry: Boolean;
begin
  repeat
    bRetry := False;
    NewName:= GetTempName(ExtractFilePath(FileName)) + '.tmp';
    Result := RenameFileUAC(FileName, NewName);
    if not Result then
      bRetry := HandleError(Format(rsMsgErrRename, [FileName, NewName]));
  until not bRetry;
end;

function TFileSystemWipeOperation.WipeDir(const FileName: String): Boolean;
var
  bRetry: Boolean;
  sTempFileName: String;
begin
  Result:= Rename(FileName, sTempFileName);
  if Result then
  begin
    repeat
      bRetry := False;
      Result:= RemoveDirectoryUAC(sTempFileName);
      if not Result then
        bRetry := HandleError(Format(rsMsgCannotDeleteDirectory, [sTempFileName]));
    until not bRetry;
  end;
end;

function TFileSystemWipeOperation.WipeLink(const FileName: String): Boolean;
var
  bRetry: Boolean;
  sTempFileName: String;
begin
  Result:= Rename(FileName, sTempFileName);
  if Result then
  repeat
    bRetry := False;
    Result := DeleteFileUAC(sTempFileName);
    if not Result then begin
      bRetry := HandleError(Format(rsMsgNotDelete, [sTempFileName]) + LineEnding + mbSysErrorMessage);
    end;
  until not bRetry;
end;

function TFileSystemWipeOperation.WipeFile(const FileName: String): Boolean;
var
  i, j: Integer;
  bRetry: Boolean;
  sTempFileName: String;
  TotalBytesToWrite: Int64;
  TargetFileStream: TFileStreamUAC;
  BytesToWrite, BytesWrittenTry, BytesWritten: Int64;
begin
  // Check file access
  repeat
    bRetry := False;
    Result:= mbFileAccess(FileName, fmOpenWrite);
    if not Result then begin
      bRetry := HandleError(rsMsgErrEOpen + ' ' + FileName);
      if not bRetry then Exit(False);
    end;
  until not bRetry;

  if not Rename(FileName, sTempFileName) then
    Exit(False);

  // Try to open file
  repeat
    bRetry := False;
    try
      TargetFileStream := TFilestreamUAC.Create(sTempFileName, fmOpenReadWrite or fmShareExclusive);
    except
      on E: Exception do
      begin
        bRetry := HandleError(rsMsgErrEOpen + ' ' + sTempFileName + LineEnding + E.Message);
        if not bRetry then Exit(False);
      end;
    end;
  until not bRetry;

  try
    for i := 1 to FWipePassNumber do
    begin
      CheckOperationState; // check pause and stop
      FStatistics.CurrentFileTotalBytes:= TargetFileStream.Size * 3;
      FStatistics.CurrentFileDoneBytes:= 0;
      UpdateStatistics(FStatistics);

      for j:= 0 to 2 do
      begin
        TargetFileStream.Position := 0;
        TotalBytesToWrite := TargetFileStream.Size;

        while TotalBytesToWrite > 0 do
        begin
          BytesWritten := 0;

          if (j = 2) then Fill(j);

          if TotalBytesToWrite > SizeOf(FBuffer[j]) then
            BytesToWrite := SizeOf(FBuffer[j])
          else begin
            BytesToWrite := TotalBytesToWrite;
          end;

          repeat
            bRetry := False;
            try
              BytesWrittenTry := TargetFileStream.Write(FBuffer[j, BytesWritten], BytesToWrite);
              BytesWritten := BytesWritten + BytesWrittenTry;
              if BytesWrittenTry = 0 then
              begin
                raise EWriteError.Create(mbSysErrorMessage(GetLastOSError));
              end
              else if BytesWritten < BytesToWrite then
              begin
                bRetry := True; // repeat and try to write the rest
                Dec(BytesToWrite, BytesWrittenTry);
              end;
            except
              on E: Exception do
              begin
                bRetry:= HandleError(rsMsgErrEWrite + ' ' + sTempFileName + LineEnding + E.Message);
                if not bRetry then Exit(False);
              end;
            end;
          until not bRetry;
          Dec(TotalBytesToWrite, BytesWritten);

          with FStatistics do
          begin
            Inc(CurrentFileDoneBytes, BytesWritten);
            Inc(DoneBytes, BytesWritten div (3 * Int64(FWipePassNumber)));
            UpdateStatistics(FStatistics);
            CheckOperationState; // check pause and stop
          end;
        end;
        // Flush data to disk
        repeat
          bRetry := False;
          Result := FileFlush(TargetFileStream.Handle);
          if not Result then begin
            bRetry := HandleError(rsMsgErrEWrite + ' ' + sTempFileName + LineEnding + mbSysErrorMessage);
            if not bRetry then Exit;
          end;
        until not bRetry;
        CheckOperationState; // check pause and stop
      end;
    end;
    // Truncate file size to zero
    repeat
      bRetry := False;
      Result := FileTruncate(TargetFileStream.Handle, 0);
      if not Result then begin
        bRetry := HandleError(rsMsgErrEWrite + ' ' + sTempFileName + LineEnding + mbSysErrorMessage);
        if not bRetry then Exit;
      end;
    until not bRetry;
  finally
    FreeAndNil(TargetFileStream);
  end;

  if Result then
  repeat
    bRetry := False;
    Result := DeleteFileUAC(sTempFileName);
    if not Result then begin
      bRetry := HandleError(Format(rsMsgNotDelete, [sTempFileName]) + LineEnding + mbSysErrorMessage);
    end;
  until not bRetry;
end;

procedure TFileSystemWipeOperation.Wipe(aFile: TFile);
var
  FileName: String;
  WipeResult: Boolean;
begin
  FileName := aFile.FullPath;

  if FileIsReadOnly(aFile.Attributes) then
  begin
    case FDeleteReadOnly of
      fsoogNone:
        case AskQuestion(Format(rsMsgFileReadOnly, [FileName]), '',
                         [fsourYes, fsourSkip, fsourAbort, fsourAll, fsourSkipAll],
                         fsourYes, fsourAbort) of
          fsourAll:
            FDeleteReadOnly := fsoogYes;
          fsourSkip:
            Exit;
          fsourSkipAll:
            begin
              FDeleteReadOnly := fsoogNo;
              Exit;
            end;
          fsourAbort:
            RaiseAbortOperation;
        end;

       fsoogNo:
         Exit;
    end;
  end;

  if FileIsReadOnly(aFile.Attributes) then
    FileSetReadOnlyUAC(FileName, False);

  if aFile.IsDirectory then // directory
    WipeResult := WipeDir(FileName)
  else if aFile.IsLink then // symbolic link
    WipeResult := WipeLink(FileName)
  else begin // normal file
    WipeResult := WipeFile(FileName);
  end;

  if aFile.IsDirectory then
  begin
    if not WipeResult then
      LogMessage(Format(rsMsgLogError + rsMsgLogWipeDir, [FileName]), [log_dir_op, log_delete], lmtError)
    else
      LogMessage(Format(rsMsgLogSuccess + rsMsgLogWipeDir, [FileName]), [log_dir_op, log_delete], lmtSuccess);
  end
  else begin
    if not WipeResult then
      LogMessage(Format(rsMsgLogError + rsMsgLogWipe, [FileName]), [log_delete], lmtError)
    else
      LogMessage(Format(rsMsgLogSuccess + rsMsgLogWipe, [FileName]), [log_delete], lmtSuccess);
  end;

  // Process comments if need
  if WipeResult and gProcessComments then
    FDescription.DeleteDescription(FileName);
end;

function TFileSystemWipeOperation.HandleError(const Message: String): Boolean;
begin
  Result := False;
  if gSkipFileOpError then
  begin
    logWrite(Thread, Message, lmtError, True);
  end
  else if not FSkipErrors then
  begin
    case AskQuestion(Message, '',
                     [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                     fsourRetry, fsourSkip) of
      fsourRetry:
        Result := True;
      fsourAbort:
        RaiseAbortOperation;
      fsourSkip: ; // Do nothing
      fsourSkipAll:
        FSkipErrors := True;
    end;
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


{
   Double Commander
   -------------------------------------------------------------------------
   Simple TAR archive writer

   Copyright (C) 2011  Koblov Alexander (Alexx2000@mail.ru)

   This unit is based on libtar.pp from the Free Component Library (FCL)

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

unit uTarWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uGlobs, uWcxModule, WcxPlugin, DCClassesUtf8,
  uFile,
  uFileSource,
  uFileSourceOperationUI,
  uFileSourceOperation,
  uFileSourceCopyOperation;

const
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;
  USTAR      = 'ustar'#32#32;
  LONGLINK   = '././@LongLink';
  LONGLEN    = RECORDSIZE * 64;
  LONGMAX    = RECORDSIZE * 128;


type
  TDataWriteProcedure = procedure(Buffer: Pointer; BytesToWrite: Int64) of object;
  TUpdateStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;

  { TTarHeader }

  TTarHeader = packed record
    Name:     array [0..NAMSIZ - 1] of AnsiChar;
    Mode:     array [0..7] of AnsiChar;
    UID:      array [0..7] of AnsiChar;
    GID:      array [0..7] of AnsiChar;
    Size:     array [0..11] of AnsiChar;
    MTime:    array [0..11] of AnsiChar;
    ChkSum:   array [0..7] of AnsiChar;
    TypeFlag: AnsiChar;
    LinkName: array [0..NAMSIZ - 1] of AnsiChar;
    Magic:    array [0..7] of AnsiChar;
    UName:    array [0..TUNMLEN - 1] of AnsiChar;
    GName:    array [0..TGNMLEN - 1] of AnsiChar;
    DevMajor: array [0..7] of AnsiChar;
    DevMinor: array [0..7] of AnsiChar;
    Prefix:   array [0..154] of AnsiChar;
  end;

  { TTarHeaderEx }

  TTarHeaderEx = packed record
    case Boolean of
    True:  (HR: TTarHeader);
    False: (HA: array [0..RECORDSIZE - 1] of AnsiChar);
  end;

  { TTarWriter }

  TTarWriter = class
  private
    FSourceStream,
    FTargetStream: TFileStreamEx;
    FWcxModule: TWcxModule;
    FTarHeader: TTarHeaderEx;
    FBasePath,
    FTargetPath,
    FArchiveFileName: UTF8String;
    FBufferIn,
    FBufferOut: Pointer;
    FBufferSize: LongWord;
    FMemPack: TArcHandle;
    FLongName: array[0..Pred(LONGMAX)] of AnsiChar;
    procedure WriteFakeHeader(const ItemName: UTF8String; IsFileName: Boolean; Offset: LongInt);
    function MakeLongName(const FileName, LinkName: UTF8String;
                          NameLen, LinkLen: LongInt): LongInt;
    function ReadData(BytesToRead: Int64): Int64;
    procedure WriteData(Buffer: Pointer; BytesToWrite: Int64);
    procedure CompressData(BufferIn: Pointer; BytesToCompress: Int64);
  protected
    AskQuestion: TAskQuestionFunction;
    AbortOperation: TAbortOperationFunction;
    CheckOperationState: TCheckOperationStateFunction;
    UpdateStatistics: TUpdateStatisticsFunction;
    DataWrite: TDataWriteProcedure;
    procedure ShowError(sMessage: String);
    procedure AddFile(const FileName: UTF8String);
    function WriteFile(const FileName: UTF8String; var Statistics: TFileSourceCopyOperationStatistics): Boolean;
  public
    constructor Create(ArchiveFileName: UTF8String;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction
                       );
    constructor Create(ArchiveFileName: UTF8String;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       WcxModule: TWcxModule
                       );
    destructor Destroy; override;

    function ProcessTree(var Files: TFiles; var Statistics: TFileSourceCopyOperationStatistics): Boolean;

  end;

implementation

uses
  {$IF DEFINED(MSWINDOWS)}
  Windows, DCFileAttributes, uMyWindows,
  {$ELSEIF DEFINED(UNIX)}
  BaseUnix, FileUtil, uUsersGroups,
  {$ENDIF}
  uLng, DCStrUtils, DCOSUtils, uOSUtils;

{$IF DEFINED(MSWINDOWS)}
const
  FILE_UNIX_MODE   = S_IRUSR or S_IWUSR or S_IRGRP or S_IROTH;
  FOLDER_UNIX_MODE = S_IRUSR or S_IWUSR or S_IXUSR or S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH;
{$ENDIF}

// Makes a string of octal digits
// The string will always be "Len" characters long
procedure Octal64(N : Int64; P : PAnsiChar; Len : Integer);
var
  I : Integer;
begin
  for I := Len - 1 downto 0 do
  begin
    (P + I)^ := AnsiChar (ORD ('0') + ORD (N and $07));
    N := N shr 3;
  end;
  for I := 0 to Len - 1 do
  begin
    if (P + I)^ in ['0'..'7'] then Break;
    (P + I)^ := '0';
  end;
end;

procedure OctalN(N : Int64; P : PAnsiChar; Len : Integer);
begin
  Octal64(N, P, Len-1);
  (P + Len - 1)^ := #0;
end;

procedure CheckSum(var TarHeader: TTarHeaderEx);
var
  I: Integer;
  ChkSum : Cardinal = 0;
begin
  with TarHeader do
  begin
    StrMove(HR.ChkSum, CHKBLANKS, 8);
    for I := 0 to SizeOf(TTarHeader) - 1 do
      Inc(ChkSum, Ord(HA[I]));
    Octal64(ChkSum, HR.ChkSum, 6);
    HR.ChkSum[6] := #0;
    HR.ChkSum[7] := #32;
  end;
end;

{$IF DEFINED(MSWINDOWS)}
function GetFileInfo(const FileName: UTF8String; out FileInfo: TWin32FindDataW): Boolean;
var
  Handle: System.THandle;
begin
  Result:= False;
  Handle := FindFirstFileW(PWideChar(UTF8Decode(FileName)), FileInfo);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      FileInfo.dwFileAttributes:= ExtractFileAttributes(FileInfo);
      Windows.FindClose(Handle);
      Result:= True;
    end;
end;
{$ELSEIF DEFINED(UNIX)}
function GetFileInfo(const FileName: UTF8String; out FileInfo: BaseUnix.Stat): Boolean;
begin
  Result:= fpLStat(PAnsiChar(UTF8ToSys(FileName)), FileInfo) >= 0;
end;
{$ENDIF}

{ TTarWriter }

procedure TTarWriter.ShowError(sMessage: String);
begin
  AskQuestion(sMessage, '', [fsourAbort], fsourAbort, fsourAbort);
  AbortOperation;
end;

constructor TTarWriter.Create(ArchiveFileName: UTF8String;
                              AskQuestionFunction: TAskQuestionFunction;
                              AbortOperationFunction: TAbortOperationFunction;
                              CheckOperationStateFunction: TCheckOperationStateFunction;
                              UpdateStatisticsFunction: TUpdateStatisticsFunction);
begin
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  DataWrite:= @WriteData;

  FArchiveFileName:= ArchiveFileName;
  FTargetPath:= ExtractFilePath(ArchiveFileName);
  // Allocate buffers
  FBufferSize := gCopyBlockSize;
  GetMem(FBufferIn, FBufferSize);
  FBufferOut:= nil;

  FWcxModule:= nil;
  FMemPack:= 0;
end;

constructor TTarWriter.Create(ArchiveFileName: UTF8String;
                              AskQuestionFunction: TAskQuestionFunction;
                              AbortOperationFunction: TAbortOperationFunction;
                              CheckOperationStateFunction: TCheckOperationStateFunction;
                              UpdateStatisticsFunction: TUpdateStatisticsFunction;
                              WcxModule: TWcxModule);
begin
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  DataWrite:= @CompressData;

  FArchiveFileName:= ArchiveFileName;
  FTargetPath:= ExtractFilePath(ArchiveFileName);
  // Allocate buffers
  FBufferSize := gCopyBlockSize;
  GetMem(FBufferIn, FBufferSize);
  GetMem(FBufferOut, FBufferSize);

  FWcxModule:= WcxModule;
  // Starts packing into memory
  FMemPack:= FWcxModule.WcxStartMemPack(MEM_OPTIONS_WANTHEADERS, ExtractFileName(ArchiveFileName));
end;

destructor TTarWriter.Destroy;
begin
  inherited Destroy;

  if Assigned(FWcxModule) then
  begin
    // Ends packing into memory
    if (FMemPack <> 0) then
      FWcxModule.DoneMemPack(FMemPack);
  end;
  if Assigned(FBufferIn) then
  begin
    FreeMem(FBufferIn);
    FBufferIn := nil;
  end;
  if Assigned(FBufferOut) then
  begin
    FreeMem(FBufferOut);
    FBufferOut := nil;
  end;
end;

procedure TTarWriter.AddFile(const FileName: UTF8String);
{$IF DEFINED(MSWINDOWS)}
var
  FileInfo: TWin32FindDataW;
  LinkName,
  FileNameIn: UTF8String;
  FileMode: Cardinal;
  FileTime,
  FileSize: Int64;
  NameLen,
  LinkLen: LongInt;
begin
  if GetFileInfo(FileName, FileInfo) then
  with FTarHeader do
  begin
    FillByte(HR, SizeOf(FTarHeader), 0);
    // File name
    FileNameIn:= ExtractDirLevel(FBasePath, FileName);
    FileNameIn:= StringReplace (FileNameIn, '\', '/', [rfReplaceAll]);
    if FPS_ISDIR(FileInfo.dwFileAttributes) then
      FileNameIn:= FileNameIn + '/';
    StrLCopy (HR.Name, PAnsiChar(FileNameIn), NAMSIZ);
    // File mode
    if FPS_ISDIR(FileInfo.dwFileAttributes) then
      FileMode:= FOLDER_UNIX_MODE
    else
      FileMode:= FILE_UNIX_MODE;
    OctalN(FileMode, HR.Mode, 8);
    // File size
    FileSize:= (FileInfo.nFileSizeHigh shl 32) or FileInfo.nFileSizeLow;
    if FPS_ISLNK(FileInfo.dwFileAttributes) then
      OctalN(0, HR.Size, 12)
    else
      OctalN(FileSize, HR.Size, 12);
    // Modification time
    FileTime:= Round((Int64(FileInfo.ftLastWriteTime) - 116444736000000000) / 10000000);
    OctalN(FileTime, HR.MTime, 12);
    // File type
    if FPS_ISLNK(FileInfo.dwFileAttributes) then
      HR.TypeFlag := '2'
    else if FPS_ISDIR(FileInfo.dwFileAttributes) then
      HR.TypeFlag := '5'
    else
      HR.TypeFlag := '0';
    // Link name
    if FPS_ISLNK(FileInfo.dwFileAttributes) then
    begin
      LinkName:= ReadSymLink(FileName);
      StrLCopy(HR.LinkName, PAnsiChar(LinkName), NAMSIZ);
    end;
    // Magic
    StrLCopy (HR.Magic, PAnsiChar(USTAR), 8);
    // Header checksum
    CheckSum(FTarHeader);
    // Get file name and link name length
    NameLen:= Length(FileNameIn);
    LinkLen:= Length(LinkName);
    // Write data
    if not ((NameLen > NAMSIZ) or (LinkLen > NAMSIZ)) then
      DataWrite(@HA, RECORDSIZE)
    else
      begin
        NameLen:= MakeLongName(FileNameIn, LinkName, NameLen, LinkLen);
        DataWrite(@FLongName, NameLen);
      end;
  end;
end;
{$ELSEIF DEFINED(UNIX)}
var
  FileInfo: BaseUnix.Stat;
  LinkName,
  FileNameIn: UTF8String;
  NameLen,
  LinkLen: LongInt;
begin
  if GetFileInfo(FileName, FileInfo) then
  with FTarHeader do
  begin
    FillByte(HR, SizeOf(FTarHeader), 0);
    // File name
    FileNameIn:= ExtractDirLevel(FBasePath, FileName);
    if fpS_ISDIR(FileInfo.st_mode) then
      FileNameIn:= FileNameIn + PathDelim;
    StrLCopy (HR.Name, PAnsiChar(FileNameIn), NAMSIZ);
    // File mode
    OctalN(FileInfo.st_mode and $FFF, HR.Mode, 8);
    // UID
    OctalN(FileInfo.st_uid, HR.UID, 8);
    // GID
    OctalN(FileInfo.st_gid, HR.GID, 8);
    // File size
    if fpS_ISLNK(FileInfo.st_mode) then
      OctalN(0, HR.Size, 12)
    else
      OctalN(FileInfo.st_size, HR.Size, 12);
    // Modification time
    OctalN(FileInfo.st_mtime, HR.MTime, 12);
    // File type
    if fpS_ISLNK(FileInfo.st_mode) then
      HR.TypeFlag:= '2'
    else if fpS_ISCHR(FileInfo.st_mode) then
      HR.TypeFlag:= '3'
    else if fpS_ISBLK(FileInfo.st_mode) then
      HR.TypeFlag:= '4'
    else if fpS_ISDIR(FileInfo.st_mode) then
      HR.TypeFlag:= '5'
    else if fpS_ISFIFO(FileInfo.st_mode) then
      HR.TypeFlag:= '6'
    else
      HR.TypeFlag:= '0';
    // Link name
    if fpS_ISLNK(FileInfo.st_mode) then
    begin
      LinkName:= ReadSymLink(FileName);
      StrLCopy(HR.LinkName, PAnsiChar(LinkName), NAMSIZ);
    end;
    // Magic
    StrLCopy (HR.Magic, PAnsiChar(USTAR), 8);
    // User
    StrPLCopy(HR.UName, UIDToStr(FileInfo.st_uid), TUNMLEN);
    // Group
    StrPLCopy(HR.GName, GIDToStr(FileInfo.st_gid), TGNMLEN);
    // Header checksum
    CheckSum(FTarHeader);
    // Get file name and link name length
    NameLen:= Length(FileNameIn);
    LinkLen:= Length(LinkName);
    // Write data
    if not ((NameLen > NAMSIZ) or (LinkLen > NAMSIZ)) then
      DataWrite(@HA, RECORDSIZE)
    else
      begin
        NameLen:= MakeLongName(FileNameIn, LinkName, NameLen, LinkLen);
        DataWrite(@FLongName, NameLen);
      end;
  end;
end;
{$ENDIF}

procedure TTarWriter.WriteFakeHeader(const ItemName: UTF8String;
  IsFileName: Boolean; Offset: LongInt);
var
  TarHeader: TTarHeaderEx;
begin
  with TarHeader do
  begin
    FillByte(TarHeader, SizeOf(TTarHeaderEx), 0);
    StrPLCopy (HR.Name, LONGLINK, NAMSIZ);
    if IsFileName then
      HR.TypeFlag:= 'L'
    else
      HR.TypeFlag:= 'K';
    // File mode
    OctalN(0, HR.Mode, 8);
    // UID
    OctalN(0, HR.UID, 8);
    // GID
    OctalN(0, HR.GID, 8);
    // Name size
    OctalN(Length(ItemName) + 1, HR.Size, 12);
    // Modification time
    OctalN(0, HR.MTime, 12);
    // Magic
    StrLCopy (HR.Magic, PAnsiChar(USTAR), 8);
    // User
    StrPLCopy(HR.UName, 'root', TUNMLEN);
    // Group
    StrPLCopy(HR.GName, 'root', TGNMLEN);
    // Header checksum
    CheckSum(TarHeader);
    // Copy file record
    Move(HA, PByte(PAnsiChar(@FLongName) + Offset)^, RECORDSIZE);
    // Copy file name
    StrMove(PAnsiChar(@FLongName) + Offset + RECORDSIZE, PAnsiChar(ItemName), Length(ItemName));
  end;
end;

function TTarWriter.MakeLongName(const FileName, LinkName: UTF8String;
                                 NameLen, LinkLen: LongInt): LongInt;
begin
  with FTarHeader do
  begin
    Result:= 0;

    // Strip string length to maximum length
    if (NameLen + RECORDSIZE) > LONGLEN then
      NameLen:= LONGLEN - RECORDSIZE * 2;
    if (LinkLen + RECORDSIZE) > LONGLEN then
      LinkLen:= LONGLEN - RECORDSIZE * 2;

    // Clear output buffer
    FillChar(FLongName, NameLen + LinkLen + RECORDSIZE * 4, #0);

    // Write Header for long link name
    if LinkLen > NAMSIZ then
    begin
      WriteFakeHeader(LinkName, False, Result);
      // Align link name by RECORDSIZE (512)
      if (LinkLen mod RECORDSIZE) = 0 then
        Result:= Result + RECORDSIZE + Linklen
      else
        Result:= Result + RECORDSIZE * 2 + (LinkLen div RECORDSIZE) * RECORDSIZE;
    end;

    // Write Header for long file name
    if NameLen > NAMSIZ then
    begin
      WriteFakeHeader(FileName, True, Result);
      // Align file name by RECORDSIZE (512)
      if (NameLen mod RECORDSIZE) = 0 then
        Result:= Result + RECORDSIZE + NameLen
      else
        Result:= Result + RECORDSIZE * 2 + (NameLen div RECORDSIZE) * RECORDSIZE;
    end;
    // Copy file record
    Move(HA, PByte(PAnsiChar(@FLongName) + Result)^, RECORDSIZE);
    Result:= Result + RECORDSIZE;
  end;
end;

function TTarWriter.ReadData(BytesToRead: Int64): Int64;
var
  bRetryRead: Boolean;
  BytesRead: Int64;
begin
  repeat
    try
      bRetryRead := False;

      FillByte(FBufferIn^, FBufferSize, 0);
      BytesRead:= FSourceStream.Read(FBufferIn^, BytesToRead);

      if (BytesRead = 0) then
        Raise EReadError.Create(mbSysErrorMessage(GetLastOSError));

    except
      on E: EReadError do
        begin
          case AskQuestion(rsMsgErrERead + ' ' + FSourceStream.FileName + ':',
                           E.Message,
                           [fsourRetry, fsourSkip, fsourAbort],
                           fsourRetry, fsourSkip) of
            fsourRetry:
              bRetryRead := True;
            fsourAbort:
              AbortOperation;
            fsourSkip:
              Exit;
          end; // case
        end;
    end;
  until not bRetryRead;

  Result:= BytesRead;
end;

procedure TTarWriter.WriteData(Buffer: Pointer; BytesToWrite: Int64);
var
  iTotalDiskSize, iFreeDiskSize: Int64;
  bRetryWrite: Boolean;
  BytesWrittenTry, BytesWritten: Int64;
begin
  BytesWritten := 0;
  repeat
    try
      bRetryWrite := False;
      BytesWrittenTry := FTargetStream.Write((Buffer + BytesWritten)^, BytesToWrite - BytesWritten);
      BytesWritten := BytesWritten + BytesWrittenTry;
      if BytesWrittenTry = 0 then
      begin
        Raise EWriteError.Create(mbSysErrorMessage(GetLastOSError));
      end
      else if BytesWritten < BytesToWrite then
      begin
        bRetryWrite := True;   // repeat and try to write the rest
      end;
    except
      on E: EWriteError do
        begin
          { Check disk free space }
          GetDiskFreeSpace(FTargetPath, iFreeDiskSize, iTotalDiskSize);
          if BytesToWrite > iFreeDiskSize then
            begin
              case AskQuestion(rsMsgNoFreeSpaceRetry, '',
                               [fsourYes, fsourNo],
                               fsourYes, fsourNo) of
                fsourYes:
                  bRetryWrite := True;
                fsourNo:
                  AbortOperation;
              end; // case
            end
          else
            begin
              case AskQuestion(rsMsgErrEWrite + ' ' + FArchiveFileName + ':',
                               E.Message,
                               [fsourRetry, fsourSkip, fsourAbort],
                               fsourRetry, fsourSkip) of
                fsourRetry:
                  bRetryWrite := True;
                fsourAbort:
                  AbortOperation;
                fsourSkip:
                  Exit;
              end; // case
            end;

        end; // on do
    end; // except
  until not bRetryWrite;
end;

procedure TTarWriter.CompressData(BufferIn: Pointer; BytesToCompress: Int64);
var
  InLen:   LongInt;
  Written: LongInt = 0;
  Taken:   LongInt = 0;
  SeekBy:  LongInt = 0;
  OffSet:  LongInt = 0;
  Result:  LongInt;
begin
  InLen:= BytesToCompress;
  // Do while not all data accepted
  repeat
    // Recalculate offset
    if (Taken <> 0) then
    begin
      OffSet:= OffSet + Taken;
      InLen:= InLen - Taken;
    end;
    // Compress input buffer
    {$PUSH}{$WARNINGS OFF}
    Result:= FWcxModule.PackToMem(FMemPack, PByte(PtrUInt(BufferIn) + OffSet), InLen, @Taken, FBufferOut, FBufferSize, @Written, @SeekBy);
    {$POP}

    if not (Result in [MEMPACK_OK, MEMPACK_DONE]) then
    begin
      ShowError(Format(rsMsgLogError + rsMsgLogPack,
                       [FArchiveFileName + ' - ' + GetErrorMsg(Result)]));
    end;
    // Seek if needed
    if (SeekBy <> 0) then
      FTargetStream.Seek(SeekBy, soCurrent);
    // Write compressed data
    if Written > 0 then
      WriteData(FBufferOut, Written);
  until ((Taken = InLen) and (BytesToCompress <> 0)) or (Result = MEMPACK_DONE);
end;

function TTarWriter.WriteFile(const FileName: UTF8String; var Statistics: TFileSourceCopyOperationStatistics): Boolean;
var
  BytesRead, BytesToRead, BytesToWrite: Int64;
  TotalBytesToRead: Int64 = 0;
begin
  Result := False;

  BytesToRead := FBufferSize;
  try
    FSourceStream:= nil;
    try
      FSourceStream := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyWrite);

      TotalBytesToRead := FSourceStream.Size;

      while TotalBytesToRead > 0 do
      begin
        // Without the following line the reading is very slow
        // if it tries to read past end of file.
        if TotalBytesToRead < BytesToRead then
          BytesToRead := TotalBytesToRead;

        BytesRead:= ReadData(BytesToRead);

        TotalBytesToRead := TotalBytesToRead - BytesRead;

        BytesToWrite:= BytesRead;
        if (BytesRead mod RECORDSIZE) <> 0 then
        begin
          // Align by TAR RECORDSIZE
          BytesToWrite:= (BytesRead div RECORDSIZE) * RECORDSIZE + RECORDSIZE;
        end;

        // Write data
        DataWrite(FBufferIn, BytesToWrite);

        with Statistics do
        begin
          CurrentFileDoneBytes := CurrentFileDoneBytes + BytesRead;
          DoneBytes := DoneBytes + BytesRead;

          UpdateStatistics(Statistics);
        end;

        CheckOperationState; // check pause and stop
      end; // while

    finally
      FreeAndNil(FSourceStream);
    end;

    Result:= True;

  except
    on EFOpenError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEOpen + ': ' + FileName);
      end;
    on EWriteError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEWrite + ': ' + FArchiveFileName);
      end;
  end;
end;

function TTarWriter.ProcessTree(var Files: TFiles;
                                 var Statistics: TFileSourceCopyOperationStatistics): Boolean;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
  iTotalDiskSize, iFreeDiskSize: Int64;
begin
  try
    Result:= False;
    // Set base path
    FBasePath:= Files.Path;
    // Update progress
    with Statistics do
    begin
      TotalBytes:= TotalBytes * 2;
      UpdateStatistics(Statistics);
    end;
    // Check disk free space
    //if FCheckFreeSpace = True then
    begin
      GetDiskFreeSpace(FTargetPath, iFreeDiskSize, iTotalDiskSize);
      if Statistics.TotalBytes > iFreeDiskSize then
      begin
        AskQuestion('', rsMsgNoFreeSpaceCont, [fsourAbort], fsourAbort, fsourAbort);
        AbortOperation;
      end;
    end;

    // Create destination file
    FTargetStream := TFileStreamEx.Create(FArchiveFileName, fmCreate);
    try
      for CurrentFileIndex := 0 to Files.Count - 1 do
      begin
        aFile := Files[CurrentFileIndex];

        if aFile.IsDirectory or aFile.IsLinkToDirectory then
          AddFile(aFile.FullPath)
        else if aFile.IsLink and not aFile.IsLinkToDirectory then
          begin
            // Add file record
            AddFile(aFile.FullPath);
            // Update progress
            with Statistics do
            begin
              CurrentFileFrom := aFile.FullPath;
              CurrentFileTotalBytes := aFile.Size;
              CurrentFileDoneBytes := CurrentFileTotalBytes;
              DoneBytes := DoneBytes + CurrentFileTotalBytes;
            end;
            UpdateStatistics(Statistics);
          end
        else
          begin
            // Update progress
            with Statistics do
            begin
              CurrentFileFrom := aFile.FullPath;
              CurrentFileTotalBytes := aFile.Size;
              CurrentFileDoneBytes := 0;
            end;
            UpdateStatistics(Statistics);

            // Add file record
            AddFile(aFile.FullPath);
            // TAR current file
            if not WriteFile(aFile.FullPath, Statistics) then Break;
          end;

        CheckOperationState;
      end;
      // Finish TAR archive with two null records
      FillByte(FBufferIn^, RECORDSIZE * 2, 0);
      DataWrite(FBufferIn, RECORDSIZE * 2);
      // Finish compression if needed
      if (FMemPack <> 0) then CompressData(FBufferIn, 0);
    finally
      if Assigned(FTargetStream) then
        begin
          FreeAndNil(FTargetStream);
          if (Statistics.DoneBytes <> Statistics.TotalBytes div 2) then
            // There was some error, because not all files has been archived.
            // Delete the not completed target file.
            mbDeleteFile(FArchiveFileName)
          else
            Result:= True;
        end;
    end;
  except
    on EFCreateError do
      begin
        ShowError(rsMsgLogError + rsMsgErrECreate + ': ' + FArchiveFileName);
      end;
  end;
end;

end.


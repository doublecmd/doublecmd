{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   some file routines

   contributors:

   Mattias Gaertner (from Lazarus code)
  
   Copyright (C) 2007-2010  Koblov Alexander (Alexx2000@mail.ru)
}

unit uFileProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes;

{en
   Create a chain of directories
   @param(DirectoryName The full path to directory)
   @returns(@true if DirectoryName already existed or was created succesfully.
   If it failed to create any of the parts, @false is returned.)
}
function mbForceDirectory(DirectoryName: string): boolean;
{en
   Copies a file.
   @param(sSrc String expression that specifies the name of the file to be copied)
   @param(sDst String expression that specifies the target file name)
   @returns(The function returns @true if successful, @false otherwise)
}
function CopyFile(const sSrc, sDst: String; bAppend: Boolean = False): Boolean;
{en
   Remove the contents of directory recursively
   @param(sFolderName String expression that specifies the name of the folder to be removed)
}
procedure DelTree(const sFolderName: String);
{en
   Read string from a text file into variable and goto next line
   @param(hFile Handle of file)
   @param(S Stores the result string)
}
function FileReadLn(hFile: THandle; out S: String): Boolean;
{en
   Write string to a text file and append newline
   @param(hFile Handle of file)
   @param(S String for writing)
}
procedure FileWriteLn(hFile: Integer; S: String);

function GetNextCopyName(FileName: UTF8String): UTF8String;

implementation

uses
  LCLProc, Dialogs, SysUtils, uLng, uGlobs, uClassesEx, uDCUtils,
  uOSUtils, uFileSystemFileSource, uFile, uFileSystemDeleteOperation,
  uFileSourceOperationOptions;

const
  cBlockSize=16384; // size of block if copyfile
// if pb is assigned > use, else work without pb :-)


function CopyFile(const sSrc, sDst: String; bAppend: Boolean): Boolean;
var
  src: TFileStreamEx = nil;
  dst: TFileStreamEx = nil;
  iDstBeg:Integer; // in the append mode we store original size
  Buffer: PChar = nil;
begin
  Result:=False;
  if not mbFileExists(sSrc) then Exit;
  
  GetMem(Buffer,cBlockSize+1);
  try
    try
      src:=TFileStreamEx.Create(sSrc,fmOpenRead or fmShareDenyNone);
      if not Assigned(src) then
        Exit;

      if bAppend then
      begin
        dst:=TFileStreamEx.Create(sDst,fmOpenReadWrite);
        dst.Seek(0,soFromEnd); // seek to end
      end
      else
        dst:=TFileStreamEx.Create(sDst,fmCreate);
      if not Assigned(dst) then
        Exit;

      iDstBeg:=dst.Size;
      // we dont't use CopyFrom, because it's alocate and free buffer every time is called
      while (dst.Size+cBlockSize)<= (src.Size+iDstBeg) do
      begin
        Src.ReadBuffer(Buffer^, cBlockSize);
        dst.WriteBuffer(Buffer^, cBlockSize);
      end;

      if (iDstBeg+src.Size)>dst.Size then
      begin
//        dst.CopyFrom(src,src.Size-dst.size);
        src.ReadBuffer(Buffer^, src.Size+iDstBeg-dst.size);
        dst.WriteBuffer(Buffer^, src.Size+iDstBeg-dst.size);
      end;
      Result := mbFileCopyAttr(sSrc, sDst, gDropReadOnlyFlag); // chmod, chgrp

    except
      on EStreamError do
        MessageDlg('Error', Format(rsMsgErrCannotCopyFile, [sSrc, sDst]), mtError, [mbOK], 0);
    end;

  finally
    if assigned(src) then
      FreeAndNil(src);
    if assigned(dst) then
      FreeAndNil(dst);
    if assigned(Buffer) then
      FreeMem(Buffer);
  end;
end;

procedure DelTree(const sFolderName: String);
var
  DeleteOperation: TFileSystemDeleteOperation = nil;
  aFiles: TFiles = nil;
begin
  aFiles := TFiles.Create(sFolderName);
  try
    aFiles.Add(TFileSystemFileSource.CreateFileFromFile(sFolderName));

    DeleteOperation := TFileSystemDeleteOperation.Create(
      TFileSystemFileSource.GetFileSource, aFiles);
    DeleteOperation.DeleteReadOnly := fsoogYes;
    DeleteOperation.SymLinkOption := fsooslDontFollow;
    DeleteOperation.SkipErrors := True;
    DeleteOperation.Execute;

  finally
    FreeThenNil(aFiles);
    FreeThenNil(DeleteOperation);
  end;
end;

function FileReadLn(hFile: THandle; out S: String): Boolean;
const
  cBufSize = 4096;
var
   Buf: array[1..cBufSize] of Char;
   iNumRead,
   iCounter,
   iBufPos: Integer;
   bEOLFound: Boolean;
   iFilePos,
   iFileSize: Int64;
begin
  S:='';
  Result:= False;
  // get current position
  iFilePos:= FileSeek(hFile, 0, soFromCurrent);
  // get file size
  iFileSize:= FileSeek(hFile, 0, soFromEnd);
  // restore position
  FileSeek(hFile, iFilePos, soFromBeginning);
  bEOLFound:= False;

  while (iFilePos < iFileSize) and not bEOLFound do
    begin
      iNumRead:= FileRead(hFile, Buf, SizeOf(Buf));

      for iCounter:= 1 to iNumRead do
          begin
            if Buf[iCounter] in [#13, #10] then
              begin
                bEOLFound:=True;
                iBufPos:=iCounter+1;
                if ((iBufPos) <= iNumRead) and (Buf[iBufPos] in [#13, #10]) then
                  Inc(iBufPos);
                Buf[iCounter]:= #0;
                S:= StrPas(@Buf);
                FileSeek(hFile, iFilePos+iBufPos-1, soFromBeginning);
                Exit(True);
              end;
          end; // for

      if (not bEOLFound) then
         begin
           if (iNumRead < cBufSize) then
             Buf[iNumRead+1]:= #0;
           S:= StrPas(@Buf);
         end;
      Inc(iFilePos, iNumRead);
    end; // while
end;

procedure FileWriteLn(hFile: Integer; S: String);
begin
  S:= S + LineEnding;
  FileWrite(hFile, PChar(S)[0], Length(S));
end;

function mbForceDirectory(DirectoryName: string): boolean;
var
  i: integer;
  sDir: string;
begin
  if DirectoryName = '' then Exit;
  DirectoryName := IncludeTrailingPathDelimiter(DirectoryName);
  i:= 1;
  if Pos('\\', DirectoryName) = 1 then // if network path
    begin
      i := CharPos(PathDelim, DirectoryName, 3); // index of the end of computer name
      i := CharPos(PathDelim, DirectoryName, i + 1); // index of the end of first remote directory
    end;

  // Move past path delimiter at the beginning.
  if (i = 1) and (DirectoryName[i] = PathDelim) then
    i := i + 1;

  while i<=length(DirectoryName) do
  begin
    if DirectoryName[i]=PathDelim then
    begin
      sDir:=copy(DirectoryName,1,i-1);

      if not mbDirectoryExists(sDir) then
      begin
        Result:=mbCreateDir(sDir);
        if not Result then exit;
      end;
    end;
    Inc(i);
  end;
  Result := True;
end;

function GetNextCopyName(FileName: UTF8String): UTF8String;
var
  CopyNumber: Int64 = 1;
  sFilePath,
  sFileName: UTF8String;
begin
  sFilePath:= ExtractFilePath(FileName);
  sFileName:= ExtractFileName(FileName);
  repeat
    Result := sFilePath + Format(rsCopyNameTemplate, [CopyNumber, sFileName]);
    Inc(CopyNumber);
  until not mbFileSystemEntryExists(Result);
end;

end.

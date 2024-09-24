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
   Write string to a text file and append newline
   @param(hFile Handle of file)
   @param(S String for writing)
}
procedure FileWriteLn(hFile: THandle; S: String);

function GetNextCopyName(FileName: String; IsDirectory: Boolean): String;

function mbFileIsText(const FileName: String): Boolean;

function mbReadFileToString(const FileName: String): String;

implementation

uses
  LCLProc, Dialogs, SysUtils, uLng, uGlobs, DCClassesUtf8, DCStrUtils,
  DCOSUtils, uFileSystemFileSource, uFile, uFileSystemDeleteOperation,
  uFileSourceOperationOptions, uAdministrator;

const
  cBlockSize=16384; // size of block if copyfile
// if pb is assigned > use, else work without pb :-)


function CopyFile(const sSrc, sDst: String; bAppend: Boolean): Boolean;
var
  src: TFileStreamEx = nil;
  dst: TFileStreamEx = nil;
  iDstBeg:Integer; // in the append mode we store original size
  Buffer: PChar = nil;
  CopyPropertiesOptions: TCopyAttributesOptions;
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

      CopyPropertiesOptions := CopyAttributesOptionCopyAll;
      if gDropReadOnlyFlag then
        Include(CopyPropertiesOptions, caoRemoveReadOnlyAttr);
      Result := mbFileCopyAttr(sSrc, sDst, CopyPropertiesOptions) = []; // chmod, chgrp

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
    FreeAndNil(aFiles);
    FreeAndNil(DeleteOperation);
  end;
end;

procedure FileWriteLn(hFile: THandle; S: String);
begin
  S:= S + LineEnding;
  FileWrite(hFile, PAnsiChar(S)^, Length(S));
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

function GetNextCopyName(FileName: String; IsDirectory: Boolean): String;
var
  CopyNumber: Int64 = 1;
  sFilePath, sFileName, SuffixStr: String;
begin
  SuffixStr:= '';
  sFilePath:= ExtractFilePath(FileName);
  sFileName:= ExtractFileName(FileName);
  repeat
    case gTypeOfDuplicatedRename of
      drLegacyWithCopy:
        begin
{$IFDEF UNIX}
          if (Length(sFileName) > 0) and (sFileName[1] = ExtensionSeparator) then
            Result := sFilePath + ExtensionSeparator + Format(rsCopyNameTemplate, [CopyNumber, Copy(sFileName, 2, MaxInt)])
          else
{$ENDIF}
          Result := sFilePath + Format(rsCopyNameTemplate, [CopyNumber, sFileName]);
        end;
      drLikeWindows7, drLikeTC:
        begin
          if IsDirectory then
            Result := FileName + SuffixStr
          else
            Result := sFilePath + RemoveFileExt(sFileName) + SuffixStr + ExtractFileExt(sFileName);
        end;
    end;

    Inc(CopyNumber);
    case gTypeOfDuplicatedRename of
      drLikeWindows7: SuffixStr:= ' (' + IntToStr(CopyNumber) + ')';
      drLikeTC: SuffixStr:= '(' + IntToStr(CopyNumber) + ')';
    end;

    until not mbFileSystemEntryExists(Result);
end;

function mbFileIsText(const FileName: String): Boolean;
const
  BUF_LEN = 4096;
var
  Len: Integer;
  H, L: Integer;
  Wide: Boolean;
  Buffer: String;
  Handle: THandle;
  P, F: PAnsiChar;
begin
  Handle:= FileOpenUAC(FileName, fmOpenRead or fmShareDenyNone);
  if (Handle = feInvalidHandle) then Exit(False);
  try
    Wide:= False;
    SetLength(Buffer{%H-}, BUF_LEN);
    Len:= FileRead(Handle, Buffer[1], BUF_LEN);
    if Len > 0 then
    begin
      P:= PAnsiChar(Buffer);
      F:= P + Len;

      // UTF-8 BOM
      if (P[0] = #$EF) and (P[1] = #$BB) and (P[2] = #$BF) then
      begin
        Inc(P, 3);
      end
      // UTF-16LE BOM
      else if (P[0] = #$FF) and (P[1] = #$FE) then
      begin
        H:= 1;
        L:= 0;
        Inc(P, 2);
        Wide:= True;
      end
      // UTF-16BE BOM
      else if (P[0] = #$FE) and (P[1] = #$FF) then
      begin
        H:= 0;
        L:= 1;
        Inc(P, 2);
        Wide:= True;
      end;

      if not Wide then
      begin
        while P < F do
        begin
          case P^ of
            #0..#8, #11, #14..#25, #27..#31: Exit(False);
          end;
          Inc(P);
        end;
      end
      else begin
        while P < F do
        begin
          if P[H] = #0 then
          begin
            case P[L] of
              #0..#8, #11, #14..#25, #27..#31: Exit(False);
            end;
          end;
          Inc(P, 2);
        end;
      end;
    end;
  finally
    FileClose(Handle);
  end;
  Result:= True;
end;

function mbReadFileToString(const FileName: String): String;
var
  ASize: Int64;
  Handle: THandle;
begin
  Result:= EmptyStr;
  Handle:= mbFileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if Handle <> feInvalidHandle then
  begin
    ASize:= FileGetSize(Handle);
    SetLength(Result, ASize);
    if Length(Result) > 0 then
    begin
      if FileRead(Handle, Result[1], ASize) <> ASize then
      begin
        SetLength(Result, 0);
      end;
    end;
    FileClose(Handle);
  end;
end;

end.

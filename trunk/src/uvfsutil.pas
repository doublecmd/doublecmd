{
  Double Commander
  --------------------------------------------------------------
  Some utils for VFS system (and maybe classic fs) .
  Part of Commander, realised under GNU GPL 2.  (C)opyright 2003

Authors:
  Radek Cervinka, radek.cervinka@centrum.cz
  Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

contributors:



}

unit uVFSutil;

interface

uses
  Classes, uTypes, uFileList;

procedure FillAndCount(var fl:TFileList; out FilesSize : Int64);
procedure AddUpLevel(sUpPath : String; var ls:TFileList);

{en
  Checks if a filename matches any filename in the filelist or
  if it could be in any directory of the file list or any of their subdirectories.
}
function MatchesFileList(const FileList: TFileList; FileName: String): Boolean;

{en
  Checks if a filename with full path belongs in the specified path.
  @param(sPath
         Fully specified path to directory to check.)
  @param(sFilePath
         Fully specified path to file name.)
  @param(AllowSubDirs
         If True, allows the file path to point to a file in some subdirectory of sPath.
         If False, only allows the file path to point directly to a file in sPath.
}
function IsFileInPath(const sPath : String; sFilePath : String; AllowSubDirs: Boolean) : Boolean;

{en
  Changes all the file names in FileList making them relative to 'sNewRootPath'.
  It is done by removing 'sNewRootPath' prefix from file names and setting
  current directory to sNewRootPath.
  @param(sNewRootPath
         Path that specifies new 'root' directory for all filenames.)
  @param(FileList
         Contains list of files to change.)
}
procedure ChangeFileListRoot(sNewRootPath: String; var FileList: TFileList);

function ExtractDirLevel(const sPrefix, sPath: String): String;

implementation

uses
  SysUtils, uFileOp, uOSUtils, uFindEx, LCLProc {$IFDEF UNIX}, BaseUnix{$ENDIF}
  {$IF DEFINED(MSWINDOWS)} , Windows {$ENDIF};

{ TFileList }

(* Get all files in subfolders in Real File System *)

procedure FillAndCount(var fl:TFileList; out FilesSize : Int64);
var
  i:Integer;
  ptr:PFileRecItem;
  sRealName : String;
  NewFileList: TFileList;

procedure FillAndCountRec(const srcPath, dstPath:String);
var
  sr:TSearchRec;
  fr:TFileRecItem;

begin
  if FindFirstEx(srcPath+'*',faAnyFile,sr)<>0 then
  begin
    FindCloseEx(sr);
    Exit;
  end;
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;
    fr.sName:=ExtractDirLevel(fl.CurrentDirectory, srcPath+sr.Name);
    fr.sPath:=dstPath;
    fr.sNameNoExt:=sr.Name; // we use to save dstname
    fr.iMode := sr.Attr;
    fr.bSelected:=False;
    fr.iSize := sr.Size;

    NewFileList.AddItem(@fr);

    if FPS_ISDIR(fr.iMode) then
      begin
        FillAndCountRec(srcPath+sr.Name+DirectorySeparator, dstPath+sr.Name+DirectorySeparator);
      end
    else
      inc(FilesSize, fr.iSize);
  until FindNextEx(sr)<>0;
  FindCloseEx(sr);
end;



begin
  NewFileList:=TFileList.Create;
  NewFileList.CurrentDirectory := fl.CurrentDirectory;
  FilesSize := 0;
  for i:=0 to fl.Count-1 do
  begin
    ptr:=fl.GetItem(i);

    if FPS_ISDIR(ptr^.iMode) and (not ptr^.bLinkIsDir) then
    begin
      sRealName := ptr^.sName;
      ptr^.sName := ExtractDirLevel(fl.CurrentDirectory, ptr^.sName);
      NewFileList.AddItem(ptr); // add DIR to List
      FillAndCountRec(sRealName + DirectorySeparator, ptr^.sNameNoExt + DirectorySeparator);  // rekursive browse child dir
    end
    else
    begin
      ptr^.sName := ExtractDirLevel(fl.CurrentDirectory, ptr^.sName);
      NewFileList.AddItem(ptr);
      inc(FilesSize, ptr^.iSize);
    end;
  end;
  fl.Free;
  fl := NewFileList;
end;

procedure ChangeFileListRoot(sNewRootPath: String; var FileList: TFileList);
var
  i: Integer;
  pfri: PFileRecItem;
begin
  for i:=0 to FileList.Count-1 do
  begin
    pfri := FileList.GetItem(i);

    pfri^.sName := ExtractDirLevel(sNewRootPath, pfri^.sName);
    pfri^.sPath := ExtractDirLevel(sNewRootPath, pfri^.sPath);
  end;

  FileList.CurrentDirectory := ExtractDirLevel(sNewRootPath, FileList.CurrentDirectory);
end;

procedure AddUpLevel(sUpPath : String; var ls:TFileList); // add virtually ..
var
  fi:TFileRecItem;
begin
  fi.sName:='..';
  fi.iSize:=0;
  fi.sExt:='';
  fi.sNameNoExt:=fi.sName;
  fi.bSelected:=False;
  fi.bExecutable:=False;
  fi.bSysFile := False;
{$IF DEFINED(MSWINDOWS)}
  fi.iMode := FILE_ATTRIBUTE_DIRECTORY;
{$ELSE}
  fi.iMode :=__S_IFDIR or S_IRUSR or S_IWUSR or S_IXUSR  // 'drwxr-xr-x';
                       or S_IRGRP or S_IXGRP
                       or S_IROTH or S_IXOTH;
{$ENDIF}
  fi.sModeStr:=AttrToStr(fi.iMode);
  fi.iDirSize:=0;
  fi.sPath := sUpPath;
  ls.AddItem(@fi);
end;

function MatchesFileList(const FileList: TFileList; FileName: String): Boolean;
var
  i: Integer;
  pfri : pFileRecItem;
begin
  Result := False;
  for i:=0 to FileList.Count-1 do
  begin
    pfri:=FileList.GetItem(i);

    if FPS_ISDIR(pfri^.iMode) then
    begin
      // Check if 'FileName' is in this directory or any of its subdirectories.
      if IsFileInPath(pfri^.sName, FileName, True) then
        Result := True;
    end
    else
    begin
      // Item in the list is a file, only compare names.
      if pfri^.sName = FileName then
        Result := True;
    end;
  end;
end;

function IsFileInPath(const sPath : String; sFilePath : String; AllowSubDirs: Boolean) : Boolean;
var
  PathLength, FilePathLength: Integer;
  DelimiterPos: Integer;
begin
  Result := False;

  PathLength := Length(sPath);
  FilePathLength := Length(sFilePath);

  if (FilePathLength >= PathLength) and
     (CompareStr(Copy(sFilePath, 1, PathLength), sPath) = 0) then
  begin
    if AllowSubDirs then
      Result := True
    else
    begin
      // Additionally check if the remaining path is a relative path.

      // Look for a path delimiter in the middle of the filepath.
      sFilePath := Copy(sFilePath, 1 + PathLength, FilePathLength - PathLength);
      DelimiterPos := Pos(DirectorySeparator, sFilePath);

      // If no delimiter was found or it was found at then end (for example,
      // directories may end with it), then the filename is in 'sPath'.
      if (DelimiterPos = 0) or (DelimiterPos = FilePathLength - PathLength) then
        Result := True;
    end;
  end;
end;

function ExtractDirLevel(const sPrefix, sPath: String): String;
var
  PrefixLength: Integer;
begin
  if IsFileInPath(sPrefix, sPath, True) then
  begin
    PrefixLength := Length(sPrefix);
    Result := Copy(sPath, 1 + PrefixLength, Length(sPath) - PrefixLength)
  end
  else
    Result := sPath;
end;

end.

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



implementation

uses
  SysUtils, uFileOp, uOSUtils, uFindEx, LCLProc, uDCUtils
  {$IFDEF UNIX}, BaseUnix{$ENDIF}
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


end.

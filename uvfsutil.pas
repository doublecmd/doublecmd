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
function LowDirLevel(sPath : String) : String;
function IncludeFileInList(sPath : String; var sFileName : String) : Boolean;
function ExtractDirLevel(const sPrefix, sPath: String): String;
function ls2FileInfo(sls:string):TFileRecItem;
// convert line in ls -la (or vfs) format to our structure
function ModeStr2Mode(const sMode:String):Integer;

implementation

uses
  SysUtils, uFileOp, uOSUtils, uFindEx, LCLProc {$IFNDEF WIN32}, BaseUnix{$ENDIF};

{ TFileList }

(* Get all files in subfolders in Real File System *)

procedure FillAndCount(var fl:TFileList; out FilesSize : Int64);
var
  i:Integer;
  ptr:PFileRecItem;
  sRealName : String;
  sr : TSearchRec;
  NewFileList: TFileList;

procedure FillAndCountRec(const srcPath, dstPath:String);
var
  sr:TSearchRec;
  fr:TFileRecItem;

begin
  if FindFirstEx(srcPath+'*',faAnyFile,sr)<>0 then
  begin
    FindClose(sr);
    Exit;
  end;
  repeat
    if (sr.Name='.') or (sr.Name='..') then Continue;
    fr.sName:=ExtractDirLevel(fl.CurrentDirectory, srcPath+sr.Name);
    fr.sPath:=dstPath;
    fr.sNameNoExt:=sr.Name; // we use to save dstname
    {$IFDEF UNIX}
    fr.iMode := sr.Mode;
    {$ELSE}
    fr.iMode := sr.Attr;
    {$ENDIF}
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
  FindClose(sr);
end;



begin
  NewFileList:=TFileList.Create;
  NewFileList.CurrentDirectory := fl.CurrentDirectory;
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
  fi.sModeStr:='drwxr-xr-x';
  fi.iMode:=ModeStr2Mode(fi.sModeStr); //!!!!
  fi.iDirSize:=0;
  fi.sPath := sUpPath;
  ls.AddItem(@fi);
end;

function LowDirLevel(sPath : String) : String;
var
Index, I, Count : Integer;
tmp : String;
begin
  Count := 0;
  sPath := ExcludeTrailingPathDelimiter(sPath);
  tmp := sPath;
  while True do
    begin
      Index := Pos(PathDelim, tmp);
      if Index > 0 then
        begin
          Delete(tmp, Index, 1);
          Inc(Count);
          I := Index;
        end
      else
        Break;
    end;
  Result := Copy(sPath, 1, I + Count - 1);

end;


function IncludeFileInList(sPath : String; var sFileName : String) : Boolean;
var
  Index : Integer;
begin
//DebugLN('Folder = ' + SPath);
Result := False;
Index := Pos(SPath, sFileName);
if Index > 0 then
  begin
    Delete(sFileName, 1, Index + Length(SPath) - 1);
    if Pos(DirectorySeparator, sFileName) = 0 then
      Result := True;
  end;
end;

function ExtractDirLevel(const sPrefix, sPath: String): String;
begin
  Result := sPath;
  //DebugLN('Prefix = ' + sPrefix);
  //DebugLN('sPath = ' + sPath);

  IncludeFileInList(sPrefix, Result);

  //DebugLN('Result := ' + Result);
end;

function ModeStr2Mode(const sMode:String):Integer;
begin
// stupid conversion
  Result:=0;
{$IFDEF WINDOWS}

if sMode[1] = 'd' then Result := Result or $10;
if sMode[2] = 'r' then Result := Result or $01;
if sMode[3] = 'a' then Result := Result or $20;
if sMode[4] = 'h' then Result := Result or $02;
if sMode[5] = 's' then Result := Result or $04;
if sMode[6] = 'v' then Result := Result or $08;

{$ELSE}
//  if sMode[1]='-' then Result:=Result+10;
  if sMode[1]='d' then Result:=Result or __S_IFDIR;
  if sMode[1]='l' then Result:=Result or __S_IFLNK;
  if sMode[1]='s' then Result:=Result or __S_IFSOCK;
  if sMode[1]='f' then Result:=Result or __S_IFIFO;
  if sMode[1]='b' then Result:=Result or __S_IFBLK;
  if sMode[1]='c' then Result:=Result or __S_IFCHR;


  if sMode[2]='r' then Result:=Result or S_IRUSR;
  if sMode[3]='w' then Result:=Result or S_IWUSR;
  if sMode[4]='x' then Result:=Result or S_IXUSR;
  if sMode[5]='r' then Result:=Result or S_IRGRP;
  if sMode[6]='w' then Result:=Result or S_IWGRP;
  if sMode[7]='x' then Result:=Result or S_IXGRP;
  if sMode[8]='r' then Result:=Result or S_IROTH;
  if sMode[9]='w' then Result:=Result or S_IWOTH;
  if sMode[10]='x' then Result:=Result or S_IXOTH;

  if sMode[4]='s' then Result:=Result or S_ISUID;
  if sMode[7]='s' then Result:=Result or S_ISGID;
{$ENDIF}
end;

function ls2FileInfo(sls:string):TFileRecItem;
var
  i:Integer;
  iPoz:Integer;
begin
  if sls='' then
    Raise Exception.Create('ls2FileInfo: sls is empty');
  Result.sModeStr:=Copy(sls,1,10); // Atrrs
  Result.iMode:=ModeStr2Mode(Result.sModeStr);
  iPoz:=11;
  i:=iPoz;
  while sls[i]<=' ' do
    inc(i);

//  Result.iNr:=StrToInt(sls[i]); //???
  iPoz:=i+1;
  i:=iPoz+1;
  while sls[i]>' ' do
    inc(i);

  Result.sOwner:=Trim(copy(sls,iPoz,i-iPoz)); // Owner
  while sls[i]<=' ' do
    inc(i);

  iPoz:=i;
  while sls[i]>' ' do
    inc(i);
  Result.sGroup:=Trim(copy(sls,iPoz,i-iPoz));  //Group

  while sls[i]<=' ' do
    inc(i);

  iPoz:=i;
  while sls[i]>' ' do
    inc(i);
  Result.iSize:=StrToInt(copy(sls,iPoz,i-iPoz));  // FileSize
  while sls[i]<=' ' do
    inc(i);

  iPoz:=i;
  i:=length(sls);
  while (sls[i]>' ') and (i>iPoz) do
    dec(i);

  Result.sTime:=Copy(sls,iPoz,i-iPoz); // date
//  Result.fTimeI:=StrToDateTime(Result.fTime);
//  Inc(iPoz,12);
  iPoz:=i;
  Result.sName:=Trim(Copy(sls,iPoz,length(sls)-iPoz+1)); // FileName
  Result.sExt:='';
  Result.sNameNoExt:=Result.sName;


  Result.bSelected:=False;
  Result.bExecutable:=False;
  Result.bIsLink:=False;
  Result.bLinkIsDir:=False;
  Result.iDirSize:=0;
end;



end.

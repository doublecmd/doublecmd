unit uFileSystemUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc,
  uFileSystemFileSource,
  uFileSystemFile,
  uFile;

  procedure SplitFileMask(const DestMask: String; out DestNameMask: String; out DestExtMask: String);
  function ApplyMask(const TargetString: String; Mask: String): String;
  function GetAbsoluteTargetFileName(aFile: TFile; SourcePath: String; TargetPath: String;
                                     NameMask: String; ExtMask: String): String;
  procedure FillAndCount(Files: TFileSystemFiles; out NewFiles: TFileSystemFiles;
                         out FilesCount: Int64; out FilesSize: Int64); overload;

implementation

uses
  uOSUtils, uDCUtils, FileUtil, uFindEx;

procedure SplitFileMask(const DestMask: String; out DestNameMask: String; out DestExtMask: String);
begin
  DivFileName(DestMask, DestNameMask, DestExtMask);
  if DestNameMask='' then
    DestNameMask:='*';
  if DestExtMask='' then
    DestExtMask:='.*';
end;

function ApplyMask(const TargetString: String; Mask: String): String;
var
  i:Integer;
begin
  Result:='';
  for i:=1 to length(Mask) do
  begin
    if Mask[i]= '?' then
      Result:=Result + TargetString[i]
    else
    if Mask[i]= '*' then
      Result:=Result + Copy(TargetString, i, Length(TargetString) - i + 1)
    else
      Result:=Result + Mask[i];
  end;
end;

function GetAbsoluteTargetFileName(aFile: TFile; SourcePath: String; TargetPath: String;
                                   NameMask: String; ExtMask: String): String;
var
  sDstExt: String;
  sDstName: String;
  NewName: String;
begin
  // Only change name for files.
  if aFile.IsDirectory or aFile.IsLink then
  begin
    NewName := aFile.Name;
  end
  else
  begin
    DivFileName(aFile.Name, sDstName, sDstExt);
    sDstName := ApplyMask(sDstName, NameMask);
    sDstExt  := ApplyMask(sDstExt, ExtMask);

    NewName := sDstName;
    if sDstExt <> '.' then
      NewName := NewName + sDstExt;
  end;

  Result := TargetPath + ExtractDirLevel(SourcePath, aFile.Path) + NewName;
end;

procedure FillAndCount(Files: TFileSystemFiles; out NewFiles: TFileSystemFiles;
                       out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: String);
  var
    sr: TSearchRec;
    aFile: TFileSystemFile;
  begin
    if FindFirstEx(srcPath + '*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Name='.') or (sr.Name='..') then Continue;
        aFile := TFileSystemFile.Create(sr);
        aFile.Path := srcPath;

        // For process symlinks, read only files etc.
  //      CheckFile(aFile);

        NewFiles.Add(aFile);
        if aFile.IsLink then
        begin
        end
        else if aFile.IsDirectory then
        begin
          //inc(FDirCount);
          FillAndCountRec(srcPath + sr.Name + DirectorySeparator); // go down to directory
        end
        else
        begin
          inc(FilesSize, aFile.Size);
          inc(FilesCount);
        end;
      until FindNextEx(sr) <> 0;
    end;

    FindCloseEx(sr);
  end;

var
  i: Integer;
  aFile: TFileSystemFile;
begin
  NewFiles := TFileSystemFiles.Create;
  FilesCount:= 0;
  FilesSize:= 0;
  //FDirCount:= 0;
  for i := 0 to Files.Count - 1 do
  begin
    aFile := Files[i];

    // For process symlinks, read only files etc.
    //CheckFile(aFile);

    if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
    begin
      //inc(FDirCount);
      NewFiles.Add(aFile); // add DIR to List
      FillAndCountRec(aFile.Path + aFile.Name + DirectorySeparator);  // recursive browse child dir
    end
    else
    begin
      NewFiles.Add(aFile);
      inc(FilesCount);
      inc(FilesSize, aFile.Size); // in first level we know file size -> use it
    end;
  end;
end;

end.


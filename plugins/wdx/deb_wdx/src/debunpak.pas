unit debunpak;

{$mode delphi}{$H+}

interface

uses
  SysUtils;

// Extract 'control' from control.tar.gz
function Deb_ExtractCtrlInfoFile(const DebFile: String; var DescFile: String): Boolean;

implementation

uses
  dpkg_deb, gzio, libtar;

var
  DebPkg: TDebianPackage;
  TempDir: array[0..MAX_PATH] of AnsiChar;

function ExtractGzip(const FileName, OutName: String): Boolean;
var
  AFile: gzFile;
  Handle: THandle;
  ALength: Integer;
  Buffer: array[Word] of Byte;
begin
  AFile:= gzopen(FileName, 'r');
  Result:= Assigned(AFile);
  if Result then
  begin
    Handle:= FileCreate(OutName);
    Result:= (Handle <> feInvalidHandle);
    if Result then
    begin
      while True do
      begin
        ALength:= gzread(AFile, @Buffer[0], SizeOf(Buffer));
        if ALength < 0 then
        begin
          Result:= False;
          Break;
        end;
        if ALength = 0 then Break;
        if (FileWrite(Handle, Buffer[0], ALength) <> ALength) then
        begin
          Result:= False;
          Break;
        end;
      end;
      FileClose(Handle);
      if not Result then DeleteFile(OutName);
    end;
    gzclose(AFile);
  end;
end;

//member: 1: control.tar.gz, 2: data.tar.gz
//return: full path of extracted file (${TEMP}\debXXXX\foo.tar.gz,
function UnpackDebFile(DebFile: string; memberidx: integer): string;
var
  Index: Integer;
  FileDestination: String;
  TempFile, FileExt: String;
begin
  Result := '';
{$IFDEF GDEBUG}
  WriteLn('UnpackDebFile: memberidx='+IntToStr(memberidx));
{$ENDIF}
  if (memberidx <> MEMBER_CONTROL) and (memberidx <> MEMBER_DATA) then Exit; //error

  try
    // a debian package must have control.tar.* and data.tar.*
    if DebPkg.ReadFromFile(DebFile) < 2 then Exit;

    // Check file type
    FileExt:= TrimRight(DebPkg.FMemberList[memberidx].ar_name);
    Index:= Pos(ExtensionSeparator, FileExt);
    if Index = 0 then Exit;
    FileExt:= Copy(FileExt, Index, MaxInt);
    if (FileExt <> '.tar.gz') then Exit;

    TempFile:= GetTempFileName(TempDir, 'deb') + FileExt;

  {$IFDEF GDEBUG}
    WriteLn('TempFile=' + TempFile);
  {$ENDIF}

    //extract 'control.tar.gz'
    if not DebPkg.ExtractMemberToFile(memberidx, TempFile) then Exit;
    FileDestination := StrPas(TempDir) + ChangeFileExt(ExtractFileName(TempFile), ''); // ${TEMP}\foo.tar
    ExtractGzip(TempFile, FileDestination);
  except
    // Skip
  end;

  DeleteFile(TempFile);

  if not FileExists(FileDestination) then Exit;

  Result := FileDestination;

{$IFDEF GDEBUG}
  WriteLn('UnpackDebFile');
{$ENDIF}
end;

//function ExtractDebInfoFile(debfile: string; var descfile: string): boolean;
//debfile:   full path of .deb file to extract member from
//descfile: [out] full path of extracted control file (${TEMP}\debXXXX\control)
//        you should remove this file (and the temp folder ${TEMP}\debXXXX after use.
//return: succeed or not
function Deb_ExtractCtrlInfoFile(const DebFile: string; var DescFile: string): boolean;
var
  TA: TTarArchive;
  DirRec: TTarDirRec;
  TarFileName: String;
begin
  Result := False;
{$IFDEF GDEBUG}
  WriteLn('ExtractDebInfoFile');
{$ENDIF}
  TarFileName := UnpackDebFile(DebFile, MEMBER_CONTROL);

{$IFDEF GDEBUG}
  WriteLn('tarfilename=' + tarfilename);
{$ENDIF}
  if not FileExists(TarFileName) then Exit;

  DescFile := StrPas(TempDir) + 'control.txt';
{$IFDEF GDEBUG}
  WriteLn('descfile=' + descfile);
{$ENDIF}
  TA := TTarArchive.Create(TarFileName);

  while TA.FindNext(DirRec) do
  begin
{$IFDEF GDEBUG}
    WriteLn('DirRec.Name=' + DirRec.Name);
{$ENDIF}
    if (DirRec.Name = './control') or (DirRec.Name = '.\control') or (DirRec.Name = 'control') then
    begin
      TA.ReadFile(DescFile);
      Break;
    end;
  end;
  TA.Free;

  DeleteFile(TarFileName); //foo.tar
  Result := true;
{$IFDEF GDEBUG}
  WriteLn('ExtractDebInfoFile');
{$ENDIF}
(*
   filelist := '';
  TA := TarArchive.Create('data.tar.gz');
  WHILE TA.FindNext(DirRec) DO BEGIN
      filelist := filelist + DirRec.Name + #13#10;
  end;
  TA.Free;
*)

end;

initialization
  Randomize;
  TempDir:= GetTempDir;
  DebPkg := TDebianPackage.Create;

finalization
  DebPkg.Free;

end.


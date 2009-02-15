unit debunpak;

{$mode delphi}{$H+}

interface
uses
  SysUtils
{$IFDEF GDEBUG}
  , dbugintf
{$ENDIF};

//extract `control' in control.tar.gz
function Deb_ExtractCtrlInfoFile(debfile: string; var descfile: string): boolean;
//list out files int data.tar.gz
function Deb_ListFileInDataMember(debfile: string; var FileList: string): integer;

implementation

uses
  dpkg_deb, minigzip, tarfile, {$IFDEF USE_LIBTAR}libtar{$ELSE}untar, classes{$ENDIF};

var
  DebPkg: TDebianPackage; //dpkg_deb
//  TarArch: TTarArchive; //libtar
  TempDir: array[0..MAX_PATH] of char;

//member: 1: control.tar.gz, 2: data.tar.gz
//return: full path of extracted file (${TEMP}\debXXXX\foo.tar.gz,
function UnpackDebFile(DebFile: string; memberidx: integer): string;
var
  TempFile: array[0..MAX_PATH] of char;
  FileSource,
  FileDestination: String;
begin
{$IFDEF GDEBUG}
  SendMethodEnter('UnpackDebFile: memberidx='+IntToStr(memberidx));
{$ENDIF}
  Result := '';
  if (memberidx<>MEMBER_CONTROL) and (memberidx<>MEMBER_DATA) then exit; //error

  repeat
    GetTempFileName(TempDir, 'deb', random(1000), TempFile);
    StrLCopy(TempFile, PChar(ChangeFileExt(StrPas(TempFile), '.tar.gz')), MAX_PATH);
  until not FileExists(StrPas(TempFile));
  
{$IFDEF GDEBUG}
  SendDebug('TempFile=' + TempFile);
{$ENDIF}

  DebPkg := TDebianPackage.Create;
  if DebPkg.ReadFromFile(DebFile) < 2 then  exit;
      //a debian package must have control.tar.gz and data.tar.gz

  //extract 'control.tar.gz'
  if not DebPkg.ExtractMemberToFile(memberidx, StrPas(TempFile)) then  exit;

  FileSource := StrPas(TempFile); //  X:\some\where\foo.tar.gz
  FileDestination := StrPas(TempDir) +
    ChangeFileExt(ExtractFileName(FileSource), ''); // ${TempDir}\foo.tar

  file_uncompress(FileSource);

  if not FileExists(FileDestination) then
  begin
    DeleteFile(TempFile); //foo.tar.gz
    Exit;
  end;
  Result := FileDestination;

  DeleteFile(PChar(FileSource));
{$IFDEF GDEBUG}
  SendMethodExit('UnpackDebFile');
{$ENDIF}
end;

//function Deb_ExtractDataFileList(debfile: string; var FileList: string): integer;
function Deb_ListFileInDataMember;
var
   tarfilename: string;
   fname : string;
   fsize : integer;
   TarFile: TTarFile;
begin
  tarfilename := UnpackDebFile(debfile, MEMBER_DATA);
{$IFDEF GDEBUG}
  SendDebug('tarfilename=' + tarfilename);
{$ENDIF}
  FileList:='';
  Result := 0;
  if not FileExists(tarfilename) then   exit;

  TarFile := TTarFile.Create(tarfilename);
  try
        while not TarFile.Eof do
        begin
            fname := TarFile.GetNextFilename;
            fsize := TarFile.GetNextSize;
            if fsize>0 then
            begin
                TarFile.GetNextDate;
                TarFile.SkipFile;
            end;
            if FileList='' then
                FileList := fname
            else
                FileList := FileList + #13#10 + fname;
            Inc(Result);
        end;
  finally
        TarFile.Free;
        DeleteFile(PChar(tarfilename));
  end;

end;

//function ExtractDebInfoFile(debfile: string; var descfile: string): boolean;
//debfile:   full path of .deb file to extract member from
//descfile: [out] full path of extracted control file (${TEMP}\debXXXX\control)
//        you should remove this file (and the temp folder ${TEMP}\debXXXX after use.
//return: succeed or not
function Deb_ExtractCtrlInfoFile;
var
  tarfilename: string;
{$IFDEF USE_LIBTAR}
  TA: TTarArchive;
  DirRec: TTarDirRec;
{$ELSE USE_TARFILE}
  Untar1: TUntar;
  strlst: TStringList;
{$ENDIF}
begin
  Result := false;
{$IFDEF GDEBUG}
  SendMethodEnter('ExtractDebInfoFile');
{$ENDIF}
  tarfilename := UnpackDebFile(debfile, MEMBER_CONTROL);

{$IFDEF GDEBUG}
  SendDebug('tarfilename=' + tarfilename);
{$ENDIF}
  if not FileExists(tarfilename) then   exit;

{$IFDEF USE_LIBTAR}  //libtar seems to be bad
    descfile := StrPas(TempDir) + 'control.txt';
  {$IFDEF GDEBUG}
    SendDebug('descfile=' + descfile);
  {$ENDIF}
    TA := TTarArchive.Create(tarfilename);
    TA.Reset;
    while TA.FindNext(DirRec) do
    begin
  {$IFDEF GDEBUG}
      SendDebug('DirRec.Name=' + DirRec.Name);
  {$ENDIF}
      if (DirRec.Name = './control') or (DirRec.Name = '.\control') or (DirRec.Name = 'control') then
      begin
        TA.ReadFile(descfile);
        break;
      end;
    end;
    TA.Free;
{$ELSE}
    Untar1 := TUntar.Create(nil);
    Untar1.FileSource := tarfilename;
    //Untar1.UnpackPath := TempDir + 'deb4wii';
    Untar1.UnpackPath := TempDir + ChangeFileExt(ExtractFileName(Untar1.FileSource), ''); // ${TempDir}\foo
  {$IFDEF GDEBUG}
    SendDebug('UnpackPath=' + Untar1.UnpackPath);
  {$ENDIF}
    strlst := TStringList.Create;
    strlst.Add('control');
    strlst.Add('./control');
    strlst.Add('.\control');
    Untar1.OverwriteMode := omReplace; //shit!
    Untar1.UntarSelected(strlst);
    //Untar1.Untar; //untar all
    descfile := IncludeTrailingBackSlash(Untar1.UnpackPath) + 'control';
    strlst.Free;
    Untar1.Free;
{$ENDIF}

  DeleteFile(PChar(tarfilename)); //foo.tar
  Result := true;
{$IFDEF GDEBUG}
  SendMethodExit('ExtractDebInfoFile');
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
  DebPkg := TDebianPackage.Create;
  TempDir:= GetTempDir;
  Randomize;

finalization
  DebPkg.Free;
//  TarArc.Free;

end.


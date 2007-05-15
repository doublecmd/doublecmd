{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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

unit uOSUtils;
 
interface

uses
    SysUtils, Classes, uFileList, LCLProc
    {$IFDEF WIN32}
    , Windows, ShellApi, MMSystem, uNTFSLinks
    {$ELSE}
    , BaseUnix, Libc, Unix, UnixType, fFileProperties
    {$ENDIF};
    
type
TDriveType = (dtUnknown, dtNoDrive, dtFloppy, dtFixed, dtNetwork, dtCDROM,
    dtRAM);
    
TDrive = record
Name,
Path,
DriveLabel :String;
DriveType : TDriveType;
DriveIcon : Integer;
end;
PDrive = ^TDrive;

{$IFDEF WIN32}
const
  WM_DEVICECHANGE = $0219;
  faSymLink   = $00000400;

type
  _DEV_BROADCAST_HDR = record // Device broadcast header
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;
  DEV_BROADCAST_HDR = _DEV_BROADCAST_HDR;
  PDEV_BROADCAST_HDR = ^DEV_BROADCAST_HDR;

// The following messages are for WM_DEVICECHANGE. The immediate list
// is for the wParam. ALL THESE MESSAGES PASS A POINTER TO A STRUCT
// STARTING WITH A DWORD SIZE AND HAVING NO POINTER IN THE STRUCT.

const
  DBT_DEVICEARRIVAL        = $8000; // system detected a new device
  DBT_DEVICEREMOVECOMPLETE = $8004; // device is gone

  DBT_DEVTYP_VOLUME = $00000002; // logical volume

  DBTF_MEDIA = $0001; // media comings and goings
  DBTF_NET   = $0002; // network volume

type
  _DEV_BROADCAST_VOLUME = record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: WORD;
  end;
  DEV_BROADCAST_VOLUME  = _DEV_BROADCAST_VOLUME;
  PDEV_BROADCAST_VOLUME = ^DEV_BROADCAST_VOLUME;
{$ENDIF}

function FPS_ISDIR(iAttr:Cardinal) : Boolean;
function FPS_ISLNK(iAttr:Cardinal) : Boolean;
function ExecCmdFork(const sCmd:String):Integer;
function GetDiskFreeSpace(Path : String; var FreeSize, TotalSize : Int64) : Boolean;
function CreateHardLink(Path, LinkName: string) : Boolean;
function CreateSymLink(Path, LinkName: string) : Boolean;
function ReadSymLink(LinkName : String) : String;
function GetHomeDir : String;
function GetLastDir(Path : String) : String;

function IsAvailable(Path : String) : Boolean;
function GetAllDrives : TList;

procedure ShowFilePropertiesDialog(FileList:TFileList; const aPath:String);

implementation

   
(*Is Directory*)

function  FPS_ISDIR(iAttr:Cardinal) : Boolean;
{$IFDEF WIN32}
begin
  Result := Boolean(iAttr and faDirectory);
end;
{$ELSE}
begin
  Result := BaseUnix.FPS_ISDIR(iAttr);
end;
{$ENDIF}

(*Is Link*)

function FPS_ISLNK(iAttr:Cardinal) : Boolean;
{$IFDEF WIN32}
begin
  Result := Boolean(iAttr and faSymLink);
end;
{$ELSE}
begin
  Result := BaseUnix.FPS_ISLNK(iAttr);
end;
{$ENDIF}

(* Execute external commands *)

function ExecCmdFork(const sCmd:String):Integer;
{$IFDEF UNIX}
var
  pid    : longint;
Begin
  pid := fpFork;

  if pid = 0 then
   begin
     {The child does the actual exec, and then exits}
      Shell(sCmd);
     { If the shell fails, we return an exitvalue of 127, to let it be known}
      fpExit(127);
   end
  else
   if pid = -1 then         {Fork failed}
    begin
      raise Exception.Create('Fork failed:'+sCmd);
    end;
  Result:=0;
end;
{$ELSE}
begin
ShellExecute(0, 'open',PChar(sCmd), nil, PChar(ExtractFilePath(sCmd)), SW_SHOW);
end;
{$ENDIF}

(* Get Disk Free Space *)

function GetDiskFreeSpace(Path : String; var FreeSize, TotalSize : Int64) : Boolean;
{$IFDEF UNIX}
var
  sbfs:Tstatfs;
begin
    statfs(PChar(Path),sbfs);
//    writeln('Statfs:',sbfs.bavail,' ',sbfs.bsize,' ',sbfs.blocks,' ', sbfs.bfree);
    FreeSize := (Int64(sbfs.bavail)*sbfs.bsize);
    TotalSize := (Int64(sbfs.blocks)*sbfs.bsize);
end;
{$ELSE}
begin
  Result:= GetDiskFreeSpaceEx(PChar(Path), FreeSize, TotalSize, nil);
end;
{$ENDIF}



function CreateHardLink(Path, LinkName: string) : Boolean;
{$IFDEF WIN32}
begin
  Result := True;
  try
    uNTFSLinks.CreateHardlink(Path, LinkName);
  except
    Result := False;
  end;
end;
{$ELSE}
begin
  Result := (fplink(PChar(@Path[1]),PChar(@LinkName[1]))=0);
end;
{$ENDIF}

function CreateSymLink(Path, LinkName: string) : Boolean;
{$IFDEF WIN32}
begin
  Result := True;
  try
    uNTFSLinks.CreateSymlink(Path, LinkName);
  except
    Result := False;
  end;
end;
{$ELSE}
begin
  Result := (fpsymlink(PChar(@Path[1]),PChar(@LinkName[1]))=0);
end;
{$ENDIF}

(* Get symlink target *)

function ReadSymLink(LinkName : String) : String;
{$IFDEF WIN32}
var
  Target: WideString;
  LinkType: TReparsePointType;
begin
  try
    if uNTFSLinks.FGetSymlinkInfo(LinkName, Target, LinkType) then
      Result := Target
    else
      Result := '';
  except
    Result := '';
  end;
end;
{$ELSE}
begin
  Result := fpReadlink(LinkName);
end;
{$ENDIF}

(* Return home directory*)

function GetHomeDir : String;
{$IFDEF WIN32}
var
size : Integer;
begin
  size := GetEnvironmentVariable('USERPROFILE', nil, 0);
  if size > 0 then
    begin
      SetLength(Result, size);
      GetEnvironmentVariable('USERPROFILE', PChar(Result), size);
    end;
  Delete(Result, size, 1);
  Result := Result + DirectorySeparator;
end;
{$ELSE}
begin
  Result := GetEnvironmentVariable('HOME')+DirectorySeparator;
end;
{$ENDIF}


function GetLastDir(Path : String) : String;
begin
Result := ExtractFileName(ExcludeTrailingPathDelimiter(Path));
if Result = '' then
  Result := ExtractFileDrive(Path);
end;


{$IFDEF WIN32}
(* Drive ready *)

const drive_root: AnsiString = ':\';

function DriveReady(const Drv: Char): Boolean;
var
  NotUsed: DWORD;
begin
  Result := GetVolumeInformation(PChar(Drv + drive_root), nil, 0, nil,
    NotUsed, NotUsed, nil, 0);
end;

(* Disk label *)

function GetLabelDisk(const Drv: Char; const VolReal: Boolean): string;

  function DisplayName(const Drv: Char): string;
  var
    SFI: TSHFileInfo;
  begin
    FillChar(SFI, SizeOf(SFI), 0);
    SHGetFileInfo(PChar(Drv + drive_root), 0, SFI, SizeOf(SFI), SHGFI_DISPLAYNAME);
    Result := SFI.szDisplayName;


    if Pos('(', Result) <> 0 then
      SetLength(Result, Pos('(', Result) - 2);
  end;

var
  WinVer: Byte;
  DriveType, NotUsed: DWORD;
  Buf: array [0..MAX_PATH - 1] of Char;
begin
  Result := '';
  WinVer := LOBYTE(LOWORD(GetVersion));
  DriveType := GetDriveType(PChar(Drv + drive_root));

  if (WinVer <= 4) and (DriveType <> DRIVE_REMOVABLE) or VolReal then
  begin // Win9x, Me, NT <= 4.0
    Buf[0] := #0;
    GetVolumeInformation(PChar(Drv + drive_root), Buf, DWORD(SizeOf(Buf)), nil,
      NotUsed, NotUsed, nil, 0);
    Result := Buf;

    if VolReal and (WinVer >= 5) and (Result <> '') and
       (DriveType <> DRIVE_REMOVABLE) then // Win2k, XP and higher
      Result := DisplayName(Drv)
    else if (Result = '') and (not VolReal) then
      Result := '<none>';
  end else
    Result := DisplayName(Drv);
end;

(* Wait for change disk label *)

procedure WaitLabelChange(const Drv: Char; const Str: string);
var
  st1, st2: string;
begin
  if GetLabelDisk(Drv, True) = '' then
    Exit;
  st1 := TrimLeft(Str);
  st2 := st1;
  while st1 = st2 do
    st2 := GetLabelDisk(Drv, FALSE);
end;

(* Close CD/DVD *)

procedure CloseCD(const Drive: string);
var
  OpenParms: MCI_OPEN_PARMS;
begin
  FillChar(OpenParms, SizeOf(OpenParms), 0);
  OpenParms.lpstrDeviceType := 'CDAudio';
  OpenParms.lpstrElementName := PChar(Drive + ':');
  mciSendCommand(0, MCI_OPEN, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, Longint(@OpenParms));
  mciSendCommand(OpenParms.wDeviceID, MCI_SET, MCI_SET_DOOR_CLOSED, 0);
  mciSendCommand(OpenParms.wDeviceID, MCI_CLOSE, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, Longint(@OpenParms));
end;

(* Drive icon *)

function DriveIconSysIdx(const Path: string): Integer;
var
  SFI: TSHFileInfo;
begin
  SFI.iIcon := 0;
  SHGetFileInfo(PChar(Path), 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX);
  Result := SFI.iIcon + $1000;
end;
{$ENDIF}


function IsAvailable(Path: String): Boolean;
{$IFDEF WIN32}
var
  Drv: Char;
    DriveLabel: string;
begin

  Drv := ExtractFileDrive(Path)[1];

  { Close CD/DVD }
  if (GetDriveType(PChar(Drv + drive_root)) = DRIVE_CDROM) and
     (not DriveReady(Drv)) then
    begin
       DriveLabel:= GetLabelDisk(Drv, False);
       CloseCD(Drv);
       if DriveReady(Drv) then
         WaitLabelChange(Drv, DriveLabel);
    end;
  Result :=  DriveReady(Drv);
end;
{$ELSE}
var
  mtab: PIOFile;
  pme: PMountEntry;
begin
  Result:= False;
  mtab:= setmntent(_PATH_MOUNTED,'r');
  if not Assigned(mtab) then exit;
  pme:= getmntent(mtab);
  while (pme <> nil) do
  begin
    if pme.mnt_dir = Path then
    begin
      Result:= True;
      Break;
    end;
    pme:= getmntent(mtab);
  end;
  endmntent(mtab);
end;
{$ENDIF}

(*Return a list of drives in system*)

function GetAllDrives : TList;
var
  Drive : PDrive;

{$IFDEF WIN32}
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
Result := TList.Create;
{ fill list }
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
  New(Drive);
    with Drive^ do
    begin
     if not (DriveNum in DriveBits) then Continue;
     Name := Char(DriveNum + Ord('a')) + ':\';
     Path := Name;
     DriveType := TDriveType(GetDriveType(PChar(Name)));
     if DriveType <> dtFloppy then
     DriveLabel := GetLabelDisk(Name[1], True);
     DriveIcon := DriveIconSysIdx(Path);
    end;
  Result.Add(Drive);
  end;

end;

{$ELSE}
  fstab: PIOFile;
  pme: PMountEntry;
begin
  Result := TList.Create;
  fstab:= setmntent(_PATH_FSTAB,'r');
  if not Assigned(fstab) then exit;
  pme:= getmntent(fstab);
  while (pme <> nil) do
  begin
    if (pme.mnt_dir <> '/') and (pme.mnt_dir <> 'none') and
       (pme.mnt_dir <> 'swap') and (pme.mnt_dir <> '/proc') and 
       (pme.mnt_dir <> '/dev/pts') then
       begin
         New(Drive);
         with Drive^ do
         begin
           Name := ExtractFileName(pme.mnt_dir);
           Path := pme.mnt_dir;
           // TODO drive icons on Linux
         end;
         Result.Add(Drive);
       end;
    pme:= getmntent(fstab);
  end;
  endmntent(fstab);
end;
{$ENDIF}

(* Show file properties dialog *)
procedure ShowFilePropertiesDialog(FileList:TFileList; const aPath:String);
{$IFDEF UNIX}
begin
  ShowFileProperties(FileList, aPath);
end;
{$ELSE}
var
  SExInfo: TSHELLEXECUTEINFO;
  Error: LongInt;
  iCurrent : Integer;
  FName : String;

  (* Find first selected file *)
  function FindNextSelected:Boolean;
  var
    i:Integer;
  begin
    for i:=iCurrent to FileList.Count-1 do
    begin
      if FileList.GetItem(i)^.bSelected then
      begin
        iCurrent:=i;
        Result:=True;
        Exit;
      end;
    end;
    Result:=False;
  end;

begin
  iCurrent := 0;
  if FindNextSelected then
    begin
      FName := aPath + FileList.GetItem(iCurrent)^.sName;
      //DebugLN(FName);
      ZeroMemory(Addr(SExInfo),SizeOf(SExInfo));
      SExInfo.cbSize := SizeOf(SExInfo);
      SExInfo.lpFile := PChar(FName);
      SExInfo.lpVerb := 'properties';
      SExInfo.fMask := SEE_MASK_INVOKEIDLIST;
      ShellExecuteExA(Addr(SExInfo));
    end;
end;
{$ENDIF}


end.

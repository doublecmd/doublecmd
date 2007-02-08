unit uOSUtils;
{
This unit contains platform depended functions.

Licence  : GNU GPL v 2.0
Author   : Koblov Alexander (Alexx2000@mail.ru)

contributors:
}

interface

uses
    SysUtils, Classes {$IFDEF WIN32}, Windows, ShellApi, MMSystem{$ELSE}, BaseUnix{$ENDIF};
    
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
function CreateHardLink(Path, LinkName: string) : Boolean;
function CreateSymLink(Path, LinkName: string) : Boolean;
function GetHomeDir : String;
function GetLastDir(Path : String) : String;

function GetAllDrives : TList;

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

function FPS_ISLNK(iAttr:Cardinal) : Boolean;
{$IFDEF WIN32}
begin
Result := False;
end;
{$ELSE}
begin
Result := BaseUnix.FPS_ISLNK(iAttr);
end;
{$ENDIF}

function CreateHardLink(Path, LinkName: string) : Boolean;
{$IFDEF WIN32}
begin
// on NTFS
end;
{$ELSE}
begin
Result := (fplink(PChar(@Path[1]),PChar(@LinkName[1]))=0);
end;
{$ENDIF}

function CreateSymLink(Path, LinkName: string) : Boolean;
{$IFDEF WIN32}
begin
// on NTFS
end;
{$ELSE}
begin
Result := (fpsymlink(PChar(@Path[1]),PChar(@LinkName[1]))=0);
end;
{$ENDIF}

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

function DriveReady(const Drv: Char): Boolean;
var
  NotUsed: DWORD;
begin
  Result := GetVolumeInformation(PChar(Drv + ':\'), nil, 0, nil,
    NotUsed, NotUsed, nil, 0);
end;

(* Disk label *)

function GetLabelDisk(const Drv: Char; const VolReal: Boolean): string;

  function DisplayName(const Drv: Char): string;
  var
    SFI: TSHFileInfo;
  begin
    FillChar(SFI, SizeOf(SFI), 0);
    SHGetFileInfo(PChar(Drv + ':\'), 0, SFI, SizeOf(SFI), SHGFI_DISPLAYNAME);
    Result := SFI.szDisplayName;

    // В Win9x, Me - нет метки диска -> #32 + (x:)
    // В WinNT 5.x - нет метки диска -> Название устройства + #32 + (x:)
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
  DriveType := GetDriveType(PChar(Drv + ':\'));

  if (WinVer <= 4) and (DriveType <> DRIVE_REMOVABLE) or VolReal then
  begin // Win9x, Me, NT <= 4.0
    Buf[0] := #0;
    GetVolumeInformation(PChar(Drv + ':\'), Buf, DWORD(SizeOf(Buf)), nil,
      NotUsed, NotUsed, nil, 0);
    Result := Buf;

    if VolReal and (WinVer >= 5) and (Result <> '') and
       (DriveType <> DRIVE_REMOVABLE) then // Win2k, XP и выше
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
begin
Result := TList.Create;
//need to make
end;
{$ENDIF}


end.

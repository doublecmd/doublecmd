unit uDarwinFileUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, Cocoa_Extra
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  ;

type

  { TDarwinFileUtil }

  TDarwinFileUtil = class
    class function cloneFile( const fromPath: String; const toPath: String; const size: Int64 ): Boolean;
  end;

implementation

type
  copyfile_state_t_o = record
  end;
  copyfile_state_t = ^copyfile_state_t_o;
  copyfile_flags_t = UInt32;

  function copyfile( const fromPath: pchar; const toPath: pchar; state: copyfile_state_t; flags: copyfile_flags_t ): Integer;
    cdecl; external name 'copyfile';

const
  COPYFILE_ACL   = 1 shl 0;
  COPYFILE_STAT	 = 1 shl 1;
  COPYFILE_XATTR = 1 shl 2;
  COPYFILE_DATA	 = 1 shl 3;

  COPYFILE_SECURITY = COPYFILE_STAT or COPYFILE_ACL;
  COPYFILE_METADATA = COPYFILE_SECURITY or COPYFILE_XATTR;
  COPYFILE_ALL	    = COPYFILE_METADATA or COPYFILE_DATA;

  COPYFILE_UNLINK      = 1 shl 21;
  COPYFILE_CLONE       = 1 shl 24;
  COPYFILE_CLONE_FORCE = 1 shl 25;

{$IFDEF DARWIN}
function IsAPFS(const Path: String): Boolean;
var
  statfsBuf: statfs;
  dir: String;
begin
  // We check the folder, not the file
  if DirectoryExists(Path) then
    dir := Path
  else
    dir := ExtractFilePath(Path);
  if dir = '' then
    dir := '/';
  Result := False;
  if fpStatFS(PChar(dir), @statfsBuf) = 0 then
    Result := string(statfsBuf.f_fstypename) = 'apfs';
end;
{$ENDIF}

{ TDarwinFileUtil }

// the copyfile() api has two advantages:
// 1. dramatically improve file copy speed on APFS
// 2. supports copying macOS specific attributes
// therefore, we should try copyfile() as much as possible on macOS
class function TDarwinFileUtil.cloneFile( const fromPath: String; const toPath: String; const size: Int64 ): Boolean;
const
  NO_CALLBACK_MAXSIZE = 20*1024*1024;   // 20MB
var
  flags: copyfile_flags_t;
  ret: Integer;
begin
  Result:= False;
  flags:= COPYFILE_ALL;

  {$IFDEF DARWIN}
  // We check that both folders (source and target) are on APFS
  if not IsAPFS(fromPath) or not IsAPFS(toPath) then
    Exit(False);
  {$ENDIF}

  // call copyfile() when:
  // 1. macOS < 10.13 and filesize <= MAX_SIZE (copy only)
  // 2. macOS >= 10.13 and filesize > MAX_SIZE (clone only, fail fast)
  // 3. macOS >= 10.13 and filesize <= MAX_SIZE (try clone, then copy)
  if NSAppKitVersionNumber < NSAppKitVersionNumber10_13 then begin
    if size > NO_CALLBACK_MAXSIZE then
      Exit;
  end else begin
    if size > NO_CALLBACK_MAXSIZE then
      flags:= flags or COPYFILE_CLONE_FORCE or COPYFILE_UNLINK
    else
      flags:= flags or COPYFILE_CLONE;
  end;

  ret:= copyfile( pchar(fromPath), pchar(toPath), nil, flags );
  Result:= (ret=0);
end;

end.

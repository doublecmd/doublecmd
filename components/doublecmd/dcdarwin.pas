unit DCDarwin;

{$mode delphi}
{$packrecords c}
{$pointermath on}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, DCBasicTypes, CocoaAll, BaseUnix;

const
  CLOSE_RANGE_CLOEXEC = (1 << 2);

function CloseRange(first: cuint; last: cuint; flags: cint): cint; cdecl;
function mbFileCopyXattr(const Source, Target: String): Boolean;

// MacOS File Utils
function MacosFileSetCreationTime( const path:String; const birthtime:TFileTimeEx ): Boolean;

type

  { TDarwinFileUtil }

  TDarwinFileUtil = class
    class function cloneFile( const fromPath: String; const toPath: String; const size: Int64 ): Boolean;
  end;

implementation

uses
  DCUnix;

type
  proc_fdinfo = record
    proc_fd: Int32;
    proc_fdtype: UInt32;
  end;
  Pproc_fdinfo = ^proc_fdinfo;

const
  PROC_PIDLISTFDS = 1;
  PROC_PIDLISTFD_SIZE = SizeOf(proc_fdinfo);

const
  NSAppKitVersionNumber10_13 = 1561;

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

type
  copyfile_state_t_o = record
  end;
  copyfile_state_t = ^copyfile_state_t_o;
  copyfile_flags_t = UInt32;

function copyfile( const fromPath: pchar; const toPath: pchar; state: copyfile_state_t; flags: copyfile_flags_t ): Integer;
  cdecl; external name 'copyfile';

function proc_pidinfo(pid: cint; flavor: cint; arg: cuint64; buffer: pointer; buffersize: cint): cint; cdecl; external 'proc';

function CloseRange(first: cuint; last: cuint; flags: cint): cint; cdecl;
var
  I: cint;
  Handle: cint;
  ProcessId: TPid;
  bufferSize: cint;
  pidInfo: Pproc_fdinfo;
begin
  Result:= -1;
  ProcessId:= FpGetpid;
  bufferSize:= proc_pidinfo(ProcessId, PROC_PIDLISTFDS, 0, nil, 0);
  pidInfo:= GetMem(bufferSize);
  if Assigned(pidInfo) then
  begin
    bufferSize:= proc_pidinfo(ProcessId, PROC_PIDLISTFDS, 0, pidInfo, bufferSize);
    for I:= 0 to (bufferSize div PROC_PIDLISTFD_SIZE) - 1 do
    begin
      Handle:= pidInfo[I].proc_fd;
      if (Handle >= first) and (Handle <= last) then
      begin
        if (flags and CLOSE_RANGE_CLOEXEC <> 0) then
          FileCloseOnExec(Handle)
        else begin
          FileClose(Handle);
        end;
      end;
    end;
    Result:= 0;
    FreeMem(pidInfo);
  end;
end;

function mbFileCopyXattr(const Source, Target: String): Boolean;
var
  ret: Integer;
begin
  Writeln( '>>3> mbFileCopyXattr' );
  ret:= copyfile( pchar(Source), pchar(Target), nil, COPYFILE_XATTR );
  fpseterrno( ret );
  Result:= (ret=0);
end;

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

function MacosFileSetCreationTime( const path:String; const birthtime:TFileTimeEx ): Boolean;
var
  seconds: Double;
  attrs: NSMutableDictionary;
  nsPath: NSString;
begin
  Result:= true;
  if birthtime = TFileTimeExNull then exit;
  seconds:= birthtime.sec.ToDouble + birthtime.nanosec.ToDouble / (1000.0*1000.0*1000.0);
  attrs:= NSMutableDictionary.dictionaryWithCapacity( 1 );
  attrs.setValue_forKey( NSDate.dateWithTimeIntervalSince1970(seconds), NSFileCreationDate );
  nsPath:= StringToNSString( path );
  Result:= NSFileManager.defaultManager.setAttributes_ofItemAtPath_error( attrs, nsPath, nil );
end;

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


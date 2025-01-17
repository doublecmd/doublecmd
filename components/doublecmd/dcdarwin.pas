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

// MacOS File Utils
function MacosFileSetCreationTime( const path:String; const birthtime:TFileTimeEx ): Boolean;

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

end.


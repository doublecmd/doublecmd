unit inotify;

interface

uses
  SysCall, UnixType;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  uint32_t = cuint32;
  {en Structure describing an inotify event. }
  inotify_event = record
    wd:     longint;  //en< Watch descriptor.
    mask:   uint32_t; //en< Watch mask.
    cookie: uint32_t; //en< Cookie to synchronize two events.
    len:    uint32_t; //en< Length (including NULs) of name.
    name:   char;     //en< Name.
  end;
  {en Pointer to structure describing an inotify event. }
  pinotify_event = ^inotify_event;

const
  { Supported events suitable for MASK parameter of INOTIFY_ADD_WATCH. }
  IN_ACCESS    = $00000001;     {en< File was accessed. }
  IN_MODIFY    = $00000002;     {en< File was modified. }
  IN_ATTRIB    = $00000004;     {en< Metadata changed. }
  IN_CLOSE_WRITE = $00000008;   {en< Writtable file was closed. }
  IN_CLOSE_NOWRITE = $00000010; {en< Unwrittable file closed. }
  IN_CLOSE     = IN_CLOSE_WRITE or IN_CLOSE_NOWRITE;     {en< Close. }
  IN_OPEN      = $00000020;     {en< File was opened.   }
  IN_MOVED_FROM = $00000040;    {en< File was moved from X. }
  IN_MOVED_TO  = $00000080;     {en< File was moved to Y. }
  IN_MOVE      = IN_MOVED_FROM or IN_MOVED_TO;     {en< Moves. }
  IN_CREATE    = $00000100;     {en< Subfile was created. }
  IN_DELETE    = $00000200;     {en< Subfile was deleted. }
  IN_DELETE_SELF = $00000400;   {en< Self was deleted. }
  IN_MOVE_SELF = $00000800;     {en< Self was moved. }
  { Events sent by the kernel. }
  IN_UNMOUNT   = $00002000;     {en< Backing fs was unmounted. }
  IN_Q_OVERFLOW = $00004000;    {en< Event queued overflowed. }
  IN_IGNORED   = $00008000;     {en< File was ignored. }
  { Helper events. }
//  IN_CLOSE     = (IN_CLOSE_WRITE or IN_CLOSE_NOWRITE); {en< Close.}
//  IN_MOVE      = (IN_MOVED_FROM or IN_MOVED_TO);       {en< Moves.}
  { Special flags. }
  IN_ONLYDIR   = $01000000;     {en< Only watch the path if it is a directory. }
  IN_DONT_FOLLOW = $02000000;   {en< Do not follow a sym link. }
  IN_MASK_ADD  = $20000000;     {en< Add to the mask of an already existing watch. }
  IN_ISDIR     = $40000000;     {en< Event occurred against dir. }
  IN_ONESHOT   = $80000000;     {en< Only send event once. }
  {en All events which a program can wait on. }
  IN_ALL_EVENTS =
    ((((((((((IN_ACCESS or IN_MODIFY) or IN_ATTRIB) or IN_CLOSE_WRITE) or
    IN_CLOSE_NOWRITE) or IN_OPEN) or IN_MOVED_FROM) or IN_MOVED_TO) or IN_CREATE) or
    IN_DELETE) or IN_DELETE_SELF) or IN_MOVE_SELF;

{en
   Create and initialize inotify instance.
 }
function inotify_init: LongInt;
{en
   Add watch of object NAME to inotify instance FD.  Notify about events specified by MASK.
}
function inotify_add_watch(__fd: LongInt; __name: PChar; __mask: uint32_t): LongInt;
{en
   Remove the watch specified by WD from the inotify instance FD.
}
function inotify_rm_watch(__fd: LongInt; __wd: uint32_t): LongInt;

implementation

uses
  SysUtils, BaseUnix, StrUtils;

function CheckKernelVersion: Boolean;
const
  Numbers = ['0'..'9'];
var
  KernelName: TUtsName;
  sRelease: String;
  I, iVersion,
  iRelease, iPatch: Integer;
begin
  fpUname(KernelName);
  sRelease:= KernelName.Release;
  iVersion:= StrToIntDef(Copy2SymbDel(sRelease, '.'), 0);
  iRelease:= StrToIntDef(Copy2SymbDel(sRelease, '.'), 0);
  for I:= 1 to Length(sRelease) do
    if not (sRelease[I] in Numbers) then Break;
  iPatch:= StrToIntDef(LeftStr(sRelease, I-1), 0);
  Result:= (iVersion >= 2) and (iRelease >= 6) and (iPatch >= 13);
end;

function inotify_init: LongInt;
begin
  inotify_init := -1;
  if CheckKernelVersion then
    inotify_init := do_syscall(syscall_nr_inotify_init);
end;

function inotify_add_watch(__fd: LongInt; __name: PChar; __mask: uint32_t): LongInt;
begin
  inotify_add_watch := -1;
  if CheckKernelVersion then
    inotify_add_watch := do_syscall(syscall_nr_inotify_add_watch, TSysParam(__fd), TSysParam(__name), TSysParam(__mask));
end;

function inotify_rm_watch(__fd: LongInt; __wd: uint32_t): LongInt;
begin
  inotify_rm_watch := -1;
  if CheckKernelVersion then
    inotify_rm_watch := do_syscall(syscall_nr_inotify_rm_watch, TSysParam(__fd), TSysParam(__wd));
end;

end.


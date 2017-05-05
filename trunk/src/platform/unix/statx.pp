unit statx;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  SysUtils, CTypes;

const
  STATX_BTIME = $00000800; //* Want/got stx_btime */

type

  statx_timestamp = record
    tv_sec: cint64;
    tv_nsec: cuint32;
    __reserved: cint32;
  end;

  PStatX = ^TStatX;
  TStatX = record
    //* 0x00 */
    stx_mask: cuint32;            //* What results were written [uncond] */
    stx_blksize: cuint32;         //* Preferred general I/O size [uncond] */
    stx_attributes: cuint64;      //* Flags conveying information about the file [uncond] */
    //* 0x10 */
    stx_nlink: cuint32;           //* Number of hard links */
    stx_uid: cuint32;             //* User ID of owner */
    stx_gid: cuint32;             //* Group ID of owner */
    stx_mode: cuint16;            //* File mode */
    __spare0: cuint16;
    //* 0x20 */
    stx_ino: cuint64;             //* Inode number */
    stx_size: cuint64;            //* File size */
    stx_blocks: cuint64;          //* Number of 512-byte blocks allocated */
    stx_attributes_mask: cuint64; //* Mask to show what's supported in stx_attributes */
    //* 0x40 */
    stx_atime: statx_timestamp;  //* Last access time */
    stx_btime: statx_timestamp;  //* File creation time */
    stx_ctime: statx_timestamp;  //* Last attribute change time */
    stx_mtime: statx_timestamp;  //* Last data modification time */
    //* 0x80 */
    stx_rdev_major: cuint32;     //* Device ID of special file [if bdev/cdev] */
    stx_rdev_minor: cuint32;
    stx_dev_major: cuint32;      //* ID of device containing file [uncond] */
    stx_dev_minor: cuint32;
    //* 0x90 */
    __spare2: array[0..13] of cuint64; //* Spare space for future expansion */
    //* 0x100 */
  end;

function fpstatx(dfd: cint; const path: string; flags: cuint;
                 mask: cuint; buffer: pstatx): cint;

var
  HasStatX: Boolean = False;

implementation

uses
  SysCall, Dos, DCConvertEncoding;

const
{$IF DEFINED(CPUI386)}
  syscall_nr_statx = 383;
{$ELSEIF DEFINED(CPUX86_64)}
  syscall_nr_statx = 332;
{$ELSEIF DEFINED(CPUARM) or DEFINED(CPUARM64)}
  syscall_nr_statx = 397;
{$ELSE}
  syscall_nr_statx = 0;
{$ENDIF}

function fpstatx(dfd: cint; const path: string; flags: cuint;
                 mask: cuint; buffer: pstatx): cint;
var
  pathname: String;
  filename: PAnsiChar;
begin
  if (Length(path) = 0) then
    filename:= nil
  else begin
    pathname:= CeUtf8ToSys(path);
    filename:= PAnsiChar(pathname);
  end;
{$PUSH}{$WARNINGS OFF}{$HINTS OFF}
  Result := do_syscall(syscall_nr_statx, TSysParam(dfd), TSysParam(filename),
                       TSysParam(flags), TSysParam(mask), TSysParam(buffer));
{$POP}
end;

initialization
  // Linux kernel >= 4.11
  HasStatX:= (syscall_nr_statx > 0) and (DosVersion >= 2820);

end.

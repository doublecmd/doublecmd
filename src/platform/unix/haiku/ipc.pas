unit ipc;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  BaseUnix, CTypes;

const
  IPC_CREAT  = &01000;
  IPC_EXCL   = &02000;
  IPC_NOWAIT = &04000;

  IPC_RMID   = 0;
  IPC_SET    = 1;
  IPC_STAT   = 2;

type
  key_t = cint32;
  TKey  = key_t;

function ftok(path: pansichar; id: cint): key_t; cdecl; external clib;

const
  SEM_GETPID = 3;
  SEM_GETVAL = 4;
  SEM_SETVAL = 8;
  SEM_UNDO   = 10;

type
  sembuf = record
    sem_num: cushort;
    sem_op: cshort;
    sem_flg: cshort;
  end;
  Tsembuf = sembuf;
  Psembuf = ^sembuf;

  semun = record
  case cint of
    0 : ( val : cint );
    1 : ( buf : Pointer );
  end;
  Tsemun = semun;
  Psemun = ^semun;

function semctl(semID: cint; semNum: cint; command: cint; var arg: tsemun): cint; cdecl; external clib;
function semget(key: key_t; numSems: cint; semFlags: cint): cint; cdecl; external clib;
function semop(semID: cint; semOps: Psembuf; numSemOps: csize_t): cint; cdecl; external clib;

implementation

end.


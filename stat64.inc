
  { Number 512-byte blocks allocated.  }
  { future possible st_blocks high bits  }
  { will be high 32 bits of ctime someday  }
{$packrecords C}
     stat64 = record
          st_dev : word;
          __pad0 : array[0..9] of byte;
          __st_ino : dword;
          st_mode : dword;
          st_nlink : dword;
          st_uid : dword;
          st_gid : dword;
          st_rdev : word;
          __pad3 : array[0..9] of byte;
          st_size : int64;
          st_blksize : dword;
          st_blocks : dword;
          __pad4 : dword;
          st_atime : dword;
          __pad5 : dword;
          st_mtime : dword;
          __pad6 : dword;
          st_ctime : dword;
          __pad7 : dword;
          st_ino : qword;
       end;


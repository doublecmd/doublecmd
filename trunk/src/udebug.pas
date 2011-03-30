{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains functions used for debugging.

    Copyright (C) 2011  Przemysław Nagay (cobines@gmail.com)

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

unit uDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

// Thread-safe calling DebugLn.
// Still not fully safe because may conflict with DebugLn called by LCL,
// so maybe redirect LCL to a file and use DCDebug as thread-safe write to console.
// Or write directly with Writeln(StdOut, ...)
procedure DCDebug(Args: array of const);
procedure DCDebug(const S: String; Args: array of const);// similar to Format(s,Args)
procedure DCDebug(const s: String);
procedure DCDebug(const s1,s2: String);
procedure DCDebug(const s1,s2,s3: String);
procedure DCDebug(const s1,s2,s3,s4: String);
procedure DCDebug(const s1,s2,s3,s4,s5: String);

implementation

uses
  LCLProc, syncobjs;

var
  DebugLnLock: TCriticalSection;

procedure DCDebug(Args: array of const);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(Args);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const S: String; Args: array of const);// similar to Format(s,Args)
begin
  DebugLnLock.Acquire;
  try
    DebugLn(S, Args);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const s: String);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(s);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const s1,s2: String);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(s1, s2);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const s1,s2,s3: String);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(s1, s2, s3);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const s1,s2,s3,s4: String);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(s1, s2, s3, s4);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const s1,s2,s3,s4,s5: String);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(s1, s2, s3, s4, s5);
  finally
    DebugLnLock.Release;
  end;
end;

procedure DCDebug(const s1,s2,s3,s4,s5,s6: String);
begin
  DebugLnLock.Acquire;
  try
    DebugLn(s1, s2, s3, s4, s5, s6);
  finally
    DebugLnLock.Release;
  end;
end;

initialization
  DebugLnLock := TCriticalSection.Create;

finalization
  DebugLnLock.Free;

end.


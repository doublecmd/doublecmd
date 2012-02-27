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
  Classes, SysUtils, LCLVersion;

// For Lazarus < 0.9.31.
// Thread-safe DebugLn via DCDebug functions.
// Still not fully safe because may conflict with DebugLn called by LCL.

procedure DCDebug(Args: array of const);
procedure DCDebug(const S: String; Args: array of const);// similar to Format(s,Args)
procedure DCDebug(const s: String);
procedure DCDebug(const s1,s2: String);
procedure DCDebug(const s1,s2,s3: String);
procedure DCDebug(const s1,s2,s3,s4: String);
procedure DCDebug(const s1,s2,s3,s4,s5: String);

// For Lazarus >= 0.9.31.
// DebugLn and DbgOut are thread-safe due to TDCLogger but since TLazLogger
// itself is designed for single-thread then DebugLnEnter, DebugLnExit cannot
// be used from multiple threads.

implementation

uses
  LCLProc, syncobjs
  {$IF lcl_fullversion >= 093100}
  , LazLogger
  {$ENDIF}
  ;

{$IF lcl_fullversion >= 093100}
type
  {en
     Logger with thread-safe DebugLn and DbgOut.
  }
  TDCLogger = class(TLazLogger)
  private
    DebugLnLock: TCriticalSection;
  protected
    procedure DoDbgOut(const s: string); override;
    procedure DoDebugLn(const s: string); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  DCLogger: TDCLogger;

{ TDCLogger }

procedure TDCLogger.DoDbgOut(const s: string);
begin
  DebugLnLock.Acquire;
  try
    inherited DoDbgOut(s);
  finally
    DebugLnLock.Release;
  end;
end;

procedure TDCLogger.DoDebugLn(const s: string);
begin
  DebugLnLock.Acquire;
  try
    inherited DoDebugLn(s);
  finally
    DebugLnLock.Release;
  end;
end;

constructor TDCLogger.Create;
begin
  DebugLnLock := TCriticalSection.Create;
  inherited Create;
end;

destructor TDCLogger.Destroy;
begin
  inherited Destroy;
  DebugLnLock.Free;
end;

procedure DCDebug(Args: array of const);
begin
  DebugLn(Args);
end;

procedure DCDebug(const S: String; Args: array of const);// similar to Format(s,Args)
begin
  DebugLn(S, Args);
end;

procedure DCDebug(const s: String);
begin
  DebugLn(s);
end;

procedure DCDebug(const s1,s2: String);
begin
  DebugLn(s1, s2);
end;

procedure DCDebug(const s1,s2,s3: String);
begin
  DebugLn(s1, s2, s3);
end;

procedure DCDebug(const s1,s2,s3,s4: String);
begin
  DebugLn(s1, s2, s3, s4);
end;

procedure DCDebug(const s1,s2,s3,s4,s5: String);
begin
  DebugLn(s1, s2, s3, s4, s5);
end;

procedure DCDebug(const s1,s2,s3,s4,s5,s6: String);
begin
  DebugLn(s1, s2, s3, s4, s5, s6);
end;

{$ELSE}

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
{$ENDIF}

initialization
  {$IF lcl_fullversion >= 093100}
  DCLogger := TDCLogger.Create;
  LazLogger.SetDebugLogger(DCLogger);
  {$ELSE}
  DebugLnLock := TCriticalSection.Create;
  {$ENDIF}

finalization
  {$IF lcl_fullversion >= 093100}
  {$ELSE}
  DebugLnLock.Free;
  {$ENDIF}

end.


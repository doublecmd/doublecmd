{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains functions used for debugging.

    Copyright (C) 2011  Przemysław Nagay (cobines@gmail.com)
    Copyright (C) 2019  Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;

// DebugLn and DbgOut are thread-safe due to TDCLogger but since TLazLogger
// itself is designed for single-thread then DebugLnEnter, DebugLnExit cannot
// be used from multiple threads.

procedure DCDebug(Args: array of const);
procedure DCDebug(const S: String; Args: array of const);// similar to Format(s,Args)
procedure DCDebug(const s: String);
procedure DCDebug(const s1,s2: String);
procedure DCDebug(const s1,s2,s3: String);
procedure DCDebug(const s1,s2,s3,s4: String);
procedure DCDebug(const s1,s2,s3,s4,s5: String);

implementation

uses
  LCLProc, SyncObjs, LazLogger, LazLoggerBase, LazClasses;

type
  {en
     Logger with thread-safe DebugLn and DbgOut.
  }
  TDCLogger = class(TLazLoggerFile)
  private
    DebugLnLock: TCriticalSection;
  protected
    procedure DoDbgOut({$if lcl_fullversion < 2010000}const{$endif} s: string); override;
    procedure DoDebugLn({$if lcl_fullversion < 2010000}const{$endif} s: string); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function CreateDCLogger: TRefCountedObject;
begin
  Result := TDCLogger.Create;
  TDCLogger(Result).Assign(GetExistingDebugLogger);
end;

{ TDCLogger }

procedure TDCLogger.DoDbgOut({$if lcl_fullversion < 2010000}const{$endif} s: string);
begin
  DebugLnLock.Acquire;
  try
    inherited DoDbgOut(s);
  finally
    DebugLnLock.Release;
  end;
end;

procedure TDCLogger.DoDebugLn({$if lcl_fullversion < 2010000}const{$endif} s: string);
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

initialization
  LazDebugLoggerCreator := @CreateDCLogger;
  RecreateDebugLogger;

end.

